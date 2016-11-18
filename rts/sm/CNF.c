/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2014
 *
 * GC support for immutable non-GCed structures, also known as Compact
 * Normal Forms (CNF for short). This provides the RTS support for
 * the 'compact' package and the Data.Compact module.
 *
 * ---------------------------------------------------------------------------*/

#define _GNU_SOURCE

#include "PosixSource.h"
#include <string.h>
#include "Rts.h"
#include "RtsUtils.h"

#include "Capability.h"
#include "GC.h"
#include "Storage.h"
#include "CNF.h"
#include "Hash.h"
#include "HeapAlloc.h"
#include "BlockAlloc.h"
#include "Trace.h"
#include "sm/ShouldCompact.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

/*
  Note [Compact Normal Forms]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~

  A compact normal form (CNF) is a region of memory containing one or more
  Haskell data structures.  The goals are:

  * The CNF lives or dies as a single unit as far as the GC is concerned.  The
    GC does not traverse the data inside the CNF.

  * A CNF can be "serialized" (stored on disk or transmitted over a network).
    To "deserialize", all we need to do is adjust the addresses of the pointers
    within the CNF ("fixup"),  Deserializing can only be done in the context of
    the same Haskell binary that produced the CNF.

  Structure
  ~~~~~~~~~

  * In Data.Compact.Internal we have
    data Compact a = Compact Compact# a

  * The Compact# primitive object is operated on by the primitives.

  * A single CNF looks like this:

  .---------,       .-------------------------------.        ,-------------
  | Compact |    ,--+-> StgCompactNFDataBlock       |   ,--->| StgCompac...
  +---------+    `--+--- self                       |   |    |   self
  |    .----+-.  ,--+--- owner                      |   |    |   wner
  +---------+ |  |  |    next ----------------------+---'    |   next -------->
  |    .    | |  |  |-------------------------------+        +-------------
  `----+----' `--+--+-> StgCompactNFData (Compact#) |        | more data...
       |            |    totalW                     |        |
       |            |    autoblockW                 |        |
       |            |    nursery                    |        |
       |            |    last                       |        |
       |            |-------------------------------|        |
       `------------+--> data ...                   |        |
                    |                               |        |
                    |                               |        |
                    `-------------------------------'        `-------------

  * Each block in a CNF starts with a StgCompactNFDataBlock header

  * The blocks in a CNF are chained through the next field

  * Multiple CNFs are chained together using the bdescr->link and bdescr->u.prev
    fields of the bdescr.

  * The first block of a CNF (only) contains the StgCompactNFData (aka
    Compact#), right after the StgCompactNFDataBlock header.

  * The data inside a CNF block is ordinary closures

  Invariants
  ~~~~~~~~~~

  (1) A CNF is self-contained.  The data within it does not have any external
      pointers.  EXCEPT: pointers to static constructors that are guaranteed to
      never refer (directly or indirectly) to CAFs are allowed, because the
      garbage collector does not have to track or follow these.

  (2) A CNF contains only immutable data, no THUNKS or explicitly mutable
      objects.  This helps maintain invariant (1).

  Details
  ~~~~~~~

  Blocks are appended to the chain automatically as needed, or manually with a
  compactResize() call, which also adjust the size of automatically appended
  blocks.
 
  Objects can be appended to the block currently marked to the nursery, or any
  of the later blocks if the nursery block is too full to fit the entire
  object. For each block in the chain (which can be multiple block allocator
  blocks), we use the bdescr of its beginning to store how full it is.
  After an object is appended, it is scavenged for any outgoing pointers,
  and all pointed to objects are appended, recursively, in a manner similar
  to copying GC (further discussion in the note [Appending to a Compact])
 
  We also flag each bdescr in each block allocator block of a compact
  (including those there were obtained as second or later from a single
  allocGroup(n) call) with the BF_COMPACT. This allows the GC to quickly
  realize that a given pointer is in a compact region, and trigger the
  CNF path.
 
  These two facts combined mean that in any compact block where some object
  begins bdescrs must be valid. For this simplicity this is achieved by
  restricting the maximum size of a compact block to 252 block allocator
  blocks (so that the total with the bdescr is one megablock).
 
  Compacts as a whole live in special list in each generation, where the
  list is held through the bd->link field of the bdescr of the StgCompactNFData
  closure (as for large objects). They live in a different list than large
  objects because the operation to free them is different (all blocks in
  a compact must be freed individually), and stats/sanity behavior are
  slightly different. This is also the reason that compact allocates memory
  using a special function instead of just calling allocate().
 
  Compacts are also suitable for network or disk serialization, and to
  that extent they support a pointer fixup operation, which adjusts pointers
  from a previous layout of the chain in memory to the new allocation.
  This works by constructing a temporary binary search table (in the C heap)
  of the old block addresses (which are known from the block header), and
  then searching for each pointer in the table, and adjusting it.
  It relies on ABI compatibility and static linking (or no ASLR) because it
  does not attempt to reconstruct info tables, and uses info tables to detect
  pointers. In practice this means only the exact same binary should be
  used.
*/

typedef enum {
    ALLOCATE_APPEND,
    ALLOCATE_NEW,
    ALLOCATE_IMPORT_NEW,
    ALLOCATE_IMPORT_APPEND,
} AllocateOp;

static StgCompactNFDataBlock *
compactAllocateBlockInternal(Capability            *cap,
                             StgWord                aligned_size,
                             StgCompactNFDataBlock *first,
                             AllocateOp             operation)
{
    StgCompactNFDataBlock *self;
    bdescr *block, *head;
    uint32_t n_blocks;
    generation *g;

    n_blocks = aligned_size / BLOCK_SIZE;

    // Attempting to allocate an object larger than maxHeapSize
    // should definitely be disallowed.  (bug #1791)
    if ((RtsFlags.GcFlags.maxHeapSize > 0 &&
         n_blocks >= RtsFlags.GcFlags.maxHeapSize) ||
        n_blocks >= HS_INT32_MAX)   // avoid overflow when
                                    // calling allocGroup() below
    {
        heapOverflow();
        // heapOverflow() doesn't exit (see #2592), but we aren't
        // in a position to do a clean shutdown here: we
        // either have to allocate the memory or exit now.
        // Allocating the memory would be bad, because the user
        // has requested that we not exceed maxHeapSize, so we
        // just exit.
        stg_exit(EXIT_HEAPOVERFLOW);
    }

    // It is imperative that first is the first block in the compact
    // (or NULL if the compact does not exist yet)
    // because the evacuate code does not update the generation of
    // blocks other than the first (so we would get the statistics
    // wrong and crash in Sanity)
    if (first != NULL) {
        block = Bdescr((P_)first);
        g = block->gen;
    } else {
        g = g0;
    }

    ACQUIRE_SM_LOCK;
    block = allocGroup(n_blocks);
    switch (operation) {
    case ALLOCATE_NEW:
        ASSERT (first == NULL);
        ASSERT (g == g0);
        dbl_link_onto(block, &g0->compact_objects);
        g->n_compact_blocks += block->blocks;
        g->n_new_large_words += aligned_size / sizeof(StgWord);
        break;

    case ALLOCATE_IMPORT_NEW:
        dbl_link_onto(block, &g0->compact_blocks_in_import);
        /* fallthrough */
    case ALLOCATE_IMPORT_APPEND:
        ASSERT (first == NULL);
        ASSERT (g == g0);
        g->n_compact_blocks_in_import += block->blocks;
        g->n_new_large_words += aligned_size / sizeof(StgWord);
        break;

    case ALLOCATE_APPEND:
        g->n_compact_blocks += block->blocks;
        if (g == g0)
            g->n_new_large_words += aligned_size / sizeof(StgWord);
        break;

    default:
#ifdef DEBUG
        ASSERT(!"code should not be reached");
#else
        RTS_UNREACHABLE;
#endif
    }
    RELEASE_SM_LOCK;

    cap->total_allocated += aligned_size / sizeof(StgWord);

    self = (StgCompactNFDataBlock*) block->start;
    self->self = self;
    self->next = NULL;

    head = block;
    initBdescr(head, g, g);
    head->flags = BF_COMPACT;
    for (block = head + 1, n_blocks --; n_blocks > 0; block++, n_blocks--) {
        block->link = head;
        block->blocks = 0;
        block->flags = BF_COMPACT;
    }

    return self;
}

static inline StgCompactNFDataBlock *
compactGetFirstBlock(StgCompactNFData *str)
{
    return (StgCompactNFDataBlock*) ((W_)str - sizeof(StgCompactNFDataBlock));
}

static inline StgCompactNFData *
firstBlockGetCompact(StgCompactNFDataBlock *block)
{
    return (StgCompactNFData*) ((W_)block + sizeof(StgCompactNFDataBlock));
}

void
compactFree(StgCompactNFData *str)
{
    StgCompactNFDataBlock *block, *next;
    bdescr *bd;

    block = compactGetFirstBlock(str);

    for ( ; block; block = next) {
        next = block->next;
        bd = Bdescr((StgPtr)block);
        ASSERT((bd->flags & BF_EVACUATED) == 0);
        freeGroup(bd);
    }
}

void
compactMarkKnown(StgCompactNFData *str)
{
    bdescr *bd;
    StgCompactNFDataBlock *block;

    block = compactGetFirstBlock(str);
    for ( ; block; block = block->next) {
        bd = Bdescr((StgPtr)block);
        bd->flags |= BF_KNOWN;
    }
}

StgWord
countCompactBlocks(bdescr *outer)
{
    StgCompactNFDataBlock *block;
    W_ count;

    count = 0;
    while (outer) {
        bdescr *inner;

        block = (StgCompactNFDataBlock*)(outer->start);
        do {
            inner = Bdescr((P_)block);
            ASSERT (inner->flags & BF_COMPACT);

            count += inner->blocks;
            block = block->next;
        } while(block);

        outer = outer->link;
    }

    return count;
}

StgCompactNFData *
compactNew (Capability *cap, StgWord size)
{
    StgWord aligned_size;
    StgCompactNFDataBlock *block;
    StgCompactNFData *self;
    bdescr *bd;

    aligned_size = BLOCK_ROUND_UP(size + sizeof(StgCompactNFData)
                                  + sizeof(StgCompactNFDataBlock));
    if (aligned_size >= BLOCK_SIZE * BLOCKS_PER_MBLOCK)
        aligned_size = BLOCK_SIZE * BLOCKS_PER_MBLOCK;

    block = compactAllocateBlockInternal(cap, aligned_size, NULL,
                                         ALLOCATE_NEW);

    self = firstBlockGetCompact(block);
    SET_HDR((StgClosure*)self, &stg_COMPACT_NFDATA_CLEAN_info, CCS_SYSTEM);
    self->autoBlockW = aligned_size / sizeof(StgWord);
    self->nursery = block;
    self->last = block;
    self->hash = NULL;

    block->owner = self;

    bd = Bdescr((P_)block);
    bd->free = (StgPtr)((W_)self + sizeof(StgCompactNFData));

    self->totalW = bd->blocks * BLOCK_SIZE_W;

    debugTrace(DEBUG_compact, "compactNew: size %" FMT_Word, size);

    return self;
}

static StgCompactNFDataBlock *
compactAppendBlock (Capability       *cap,
                    StgCompactNFData *str,
                    StgWord           aligned_size)
{
    StgCompactNFDataBlock *block;
    bdescr *bd;

    block = compactAllocateBlockInternal(cap, aligned_size,
                                         compactGetFirstBlock(str),
                                         ALLOCATE_APPEND);
    block->owner = str;
    block->next = NULL;

    ASSERT (str->last->next == NULL);
    str->last->next = block;
    str->last = block;
    if (str->nursery == NULL)
        str->nursery = block;

    bd = Bdescr((P_)block);
    bd->free = (StgPtr)((W_)block + sizeof(StgCompactNFDataBlock));
    ASSERT (bd->free == (StgPtr)block + sizeofW(StgCompactNFDataBlock));

    str->totalW += bd->blocks * BLOCK_SIZE_W;

    return block;
}

void
compactResize (Capability *cap, StgCompactNFData *str, StgWord new_size)
{
    StgWord aligned_size;

    aligned_size = BLOCK_ROUND_UP(new_size + sizeof(StgCompactNFDataBlock));
    if (aligned_size >= BLOCK_SIZE * BLOCKS_PER_MBLOCK)
        aligned_size = BLOCK_SIZE * BLOCKS_PER_MBLOCK;

    str->autoBlockW = aligned_size / sizeof(StgWord);

    compactAppendBlock(cap, str, aligned_size);
}

static rtsBool
allocate_in_compact (StgCompactNFDataBlock *block, StgWord sizeW, StgPtr *at)
{
    bdescr *bd;
    StgPtr top;
    StgPtr free;

    bd = Bdescr((StgPtr)block);
    top = bd->start + BLOCK_SIZE_W * bd->blocks;
    if (bd->free + sizeW > top)
        return rtsFalse;

    free = bd->free;
    bd->free += sizeW;
    *at = free;

    return rtsTrue;
}

static rtsBool
block_is_full (StgCompactNFDataBlock *block)
{
    bdescr *bd;
    StgPtr top;
    StgWord sizeW;

    bd = Bdescr((StgPtr)block);
    top = bd->start + BLOCK_SIZE_W * bd->blocks;

    // We consider a block full if we could not fit
    // an entire closure with 7 payload items
    // (this leaves a slop of 64 bytes at most, but
    // it avoids leaving a block almost empty to fit
    // a large byte array, while at the same time
    // it avoids trying to allocate a large closure
    // in a chain of almost empty blocks)
    sizeW = sizeofW(StgHeader) + 7;
    return (bd->free + sizeW > top);
}

static rtsBool
allocate_loop (Capability       *cap,
               StgCompactNFData *str,
               StgWord           sizeW,
               StgPtr           *at)
{
    StgCompactNFDataBlock *block;
    StgWord next_size;

    // try the nursery first
 retry:
    if (str->nursery != NULL) {
        if (allocate_in_compact(str->nursery, sizeW, at))
            return rtsTrue;

        if (block_is_full (str->nursery)) {
            str->nursery = str->nursery->next;
            goto retry;
        }

        // try subsequent blocks
        block = str->nursery->next;
        while (block != NULL) {
            if (allocate_in_compact(block, sizeW, at))
                return rtsTrue;

            block = block->next;
        }
    }

    next_size = stg_max(str->autoBlockW * sizeof(StgWord),
                    BLOCK_ROUND_UP(sizeW * sizeof(StgWord)));
    if (next_size >= BLOCKS_PER_MBLOCK * BLOCK_SIZE)
        next_size = BLOCKS_PER_MBLOCK * BLOCK_SIZE;
    if (next_size < sizeW * sizeof(StgWord) + sizeof(StgCompactNFDataBlock))
        return rtsFalse;

    block = compactAppendBlock(cap, str, next_size);
    ASSERT (str->nursery != NULL);
    return allocate_in_compact(block, sizeW, at);
}

void *
allocateForCompact (Capability *cap,
                    StgCompactNFData *str,
                    StgWord sizeW,
                    StgClosure *p)
{
    StgPtr to;
    if (!allocate_loop(cap, str, sizeW, &to)) {
        barf("Failed to copy object in compact, object too large\n");
    }
    if (str->hash) {
        insertHashTable(str->hash, (StgWord)p, (const void*)to);
        if (str->header.info == &stg_COMPACT_NFDATA_CLEAN_info) {
            str->header.info = &stg_COMPACT_NFDATA_DIRTY_info;
            recordClosureMutated(cap, (StgClosure*)str);
        }
    }
    return to;
}


StgWord
compactContains (StgCompactNFData *str, StgPtr what)
{
    bdescr *bd;

    // This check is the reason why this needs to be
    // implemented in C instead of (possibly faster) Cmm
    if (!HEAP_ALLOCED (what))
        return 0;

    // Note that we don't care about tags, they are eaten
    // away by the Bdescr operation anyway
    bd = Bdescr((P_)what);
    return (bd->flags & BF_COMPACT) != 0 &&
        (str == NULL || objectGetCompact((StgClosure*)what) == str);
}

StgCompactNFDataBlock *
compactAllocateBlock(Capability            *cap,
                     StgWord                size,
                     StgCompactNFDataBlock *previous)
{
    StgWord aligned_size;
    StgCompactNFDataBlock *block;
    bdescr *bd;

    aligned_size = BLOCK_ROUND_UP(size);

    // We do not link the new object into the generation ever
    // - we cannot let the GC know about this object until we're done
    // importing it and we have fixed up all info tables and stuff
    //
    // but we do update n_compact_blocks, otherwise memInventory()
    // in Sanity will think we have a memory leak, because it compares
    // the blocks he knows about with the blocks obtained by the
    // block allocator
    // (if by chance a memory leak does happen due to a bug somewhere
    // else, memInventory will also report that all compact blocks
    // associated with this compact are leaked - but they are not really,
    // we have a pointer to them and we're not losing track of it, it's
    // just we can't use the GC until we're done with the import)
    //
    // (That btw means that the high level import code must be careful
    // not to lose the pointer, so don't use the primops directly
    // unless you know what you're doing!)

    // Other trickery: we pass NULL as first, which means our blocks
    // are always in generation 0
    // This is correct because the GC has never seen the blocks so
    // it had no chance of promoting them

    block = compactAllocateBlockInternal(cap, aligned_size, NULL,
                                         previous != NULL ? ALLOCATE_IMPORT_APPEND : ALLOCATE_IMPORT_NEW);
    if (previous != NULL)
        previous->next = block;

    bd = Bdescr((P_)block);
    bd->free = (P_)((W_)bd->start + size);

    return block;
}

//
// shouldCompact(c,p): returns:
//    SHOULDCOMPACT_IN_CNF if the object is in c
//    SHOULDCOMPACT_STATIC if the object is static
//    SHOULDCOMPACT_NOTIN_CNF if the object is dynamic and not in c
//
StgWord shouldCompact (StgCompactNFData *str, StgClosure *p)
{
    bdescr *bd;

    if (!HEAP_ALLOCED(p))
        return SHOULDCOMPACT_STATIC;  // we have to copy static closures too

    bd = Bdescr((P_)p);
    if (bd->flags & BF_PINNED) {
        return SHOULDCOMPACT_PINNED;
    }
    if ((bd->flags & BF_COMPACT) && objectGetCompact(p) == str) {
        return SHOULDCOMPACT_IN_CNF;
    } else {
        return SHOULDCOMPACT_NOTIN_CNF;
    }
}

/* -----------------------------------------------------------------------------
   Sanity-checking a compact
   -------------------------------------------------------------------------- */

#ifdef DEBUG
STATIC_INLINE rtsBool
object_in_compact (StgCompactNFData *str, StgClosure *p)
{
    bdescr *bd;

    // Only certain static closures are allowed to be referenced from
    // a compact, but let's be generous here and assume that all
    // static closures are OK.
    if (!HEAP_ALLOCED(p))
        return rtsTrue;

    bd = Bdescr((P_)p);
    return (bd->flags & BF_COMPACT) != 0 &&
        objectGetCompact(p) == str;
}

static rtsBool
verify_mut_arr_ptrs (StgCompactNFData *str,
                     StgMutArrPtrs    *a)
{
    StgPtr p, q;

    p = (StgPtr)&a->payload[0];
    q = (StgPtr)&a->payload[a->ptrs];
    for (; p < q; p++) {
        if (!object_in_compact(str, UNTAG_CLOSURE(*(StgClosure**)p)))
            return rtsFalse;
    }

    return rtsTrue;
}

static rtsBool
verify_consistency_block (StgCompactNFData *str, StgCompactNFDataBlock *block)
{
    bdescr *bd;
    StgPtr p;
    const StgInfoTable *info;
    StgClosure *q;

    p = (P_)firstBlockGetCompact(block);
    bd = Bdescr((P_)block);
    while (p < bd->free) {
        q = (StgClosure*)p;

        if (!LOOKS_LIKE_CLOSURE_PTR(q))
            return rtsFalse;

        info = get_itbl(q);
        switch (info->type) {
        case CONSTR_1_0:
            if (!object_in_compact(str, UNTAG_CLOSURE(q->payload[0])))
                return rtsFalse;
        case CONSTR_0_1:
            p += sizeofW(StgClosure) + 1;
            break;

        case CONSTR_2_0:
            if (!object_in_compact(str, UNTAG_CLOSURE(q->payload[1])))
                return rtsFalse;
        case CONSTR_1_1:
            if (!object_in_compact(str, UNTAG_CLOSURE(q->payload[0])))
                return rtsFalse;
        case CONSTR_0_2:
            p += sizeofW(StgClosure) + 2;
            break;

        case CONSTR:
        case PRIM:
        case CONSTR_NOCAF:
        {
            uint32_t i;

            for (i = 0; i < info->layout.payload.ptrs; i++)
                if (!object_in_compact(str, UNTAG_CLOSURE(q->payload[i])))
                    return rtsFalse;

            p += sizeofW(StgClosure) + info->layout.payload.ptrs +
                info->layout.payload.nptrs;
            break;
        }

        case ARR_WORDS:
            p += arr_words_sizeW((StgArrBytes*)p);
            break;

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            if (!verify_mut_arr_ptrs(str, (StgMutArrPtrs*)p))
                return rtsFalse;
            p += mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
            break;

        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
        {
            uint32_t i;
            StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs*)p;

            for (i = 0; i < arr->ptrs; i++)
                if (!object_in_compact(str, UNTAG_CLOSURE(arr->payload[i])))
                    return rtsFalse;

            p += sizeofW(StgSmallMutArrPtrs) + arr->ptrs;
            break;
        }

        case COMPACT_NFDATA:
            p += sizeofW(StgCompactNFData);
            break;

        default:
            return rtsFalse;
        }
    }

    return rtsTrue;
}

static rtsBool
verify_consistency_loop (StgCompactNFData *str)
{
    StgCompactNFDataBlock *block;

    block = compactGetFirstBlock(str);
    do {
        if (!verify_consistency_block(str, block))
            return rtsFalse;
        block = block->next;
    } while (block && block->owner);

    return rtsTrue;
}

void verifyCompact (StgCompactNFData *str USED_IF_DEBUG)
{
    IF_DEBUG(sanity, ASSERT(verify_consistency_loop(str)));
}
#endif // DEBUG

/* -----------------------------------------------------------------------------
   Fixing up pointers
   -------------------------------------------------------------------------- */

STATIC_INLINE rtsBool
any_needs_fixup(StgCompactNFDataBlock *block)
{
    // ->next pointers are always valid, even if some blocks were
    // not allocated where we want them, because compactAllocateAt()
    // will take care to adjust them

    do {
        if (block->self != block)
            return rtsTrue;
        block = block->next;
    } while (block && block->owner);

    return rtsFalse;
}

#ifdef DEBUG
static void
spew_failing_pointer(StgWord *fixup_table, uint32_t count, StgWord address)
{
    uint32_t i;
    StgWord key, value;
    StgCompactNFDataBlock *block;
    bdescr *bd;
    StgWord size;

    debugBelch("Failed to adjust 0x%" FMT_HexWord ". Block dump follows...\n",
               address);

    for (i  = 0; i < count; i++) {
        key = fixup_table [2 * i];
        value = fixup_table [2 * i + 1];

        block = (StgCompactNFDataBlock*)value;
        bd = Bdescr((P_)block);
        size = (W_)bd->free - (W_)bd->start;

        debugBelch("%" FMT_Word32 ": was 0x%" FMT_HexWord "-0x%" FMT_HexWord
                   ", now 0x%" FMT_HexWord "-0x%" FMT_HexWord "\n", i, key,
                   key+size, value, value+size);
    }
}
#endif

STATIC_INLINE StgCompactNFDataBlock *
find_pointer(StgWord *fixup_table, uint32_t count, StgClosure *q)
{
    StgWord address = (W_)q;
    uint32_t a, b, c;
    StgWord key, value;
    bdescr *bd;

    a = 0;
    b = count;
    while (a < b-1) {
        c = (a+b)/2;

        key = fixup_table[c * 2];
        value = fixup_table[c * 2 + 1];

        if (key > address)
            b = c;
        else
            a = c;
    }

    // three cases here: 0, 1 or 2 blocks to check
    for ( ; a < b; a++) {
        key = fixup_table[a * 2];
        value = fixup_table[a * 2 + 1];

        if (key > address)
            goto fail;

        bd = Bdescr((P_)value);

        if (key + bd->blocks * BLOCK_SIZE <= address)
            goto fail;

        return (StgCompactNFDataBlock*)value;
    }

 fail:
    // We should never get here

#ifdef DEBUG
    spew_failing_pointer(fixup_table, count, address);
#endif
    return NULL;
}

static rtsBool
fixup_one_pointer(StgWord *fixup_table, uint32_t count, StgClosure **p)
{
    StgWord tag;
    StgClosure *q;
    StgCompactNFDataBlock *block;


    q = *p;
    tag = GET_CLOSURE_TAG(q);
    q = UNTAG_CLOSURE(q);

    if (!HEAP_ALLOCED(q))
        return rtsTrue;

    block = find_pointer(fixup_table, count, q);
    if (block == NULL)
        return rtsFalse;
    if (block == block->self)
        return rtsTrue;

    q = (StgClosure*)((W_)q - (W_)block->self + (W_)block);
    *p = TAG_CLOSURE(tag, q);

    return rtsTrue;
}

static rtsBool
fixup_mut_arr_ptrs (StgWord          *fixup_table,
                    uint32_t               count,
                    StgMutArrPtrs    *a)
{
    StgPtr p, q;

    p = (StgPtr)&a->payload[0];
    q = (StgPtr)&a->payload[a->ptrs];
    for (; p < q; p++) {
        if (!fixup_one_pointer(fixup_table, count, (StgClosure**)p))
            return rtsFalse;
    }

    return rtsTrue;
}

static rtsBool
fixup_block(StgCompactNFDataBlock *block, StgWord *fixup_table, uint32_t count)
{
    const StgInfoTable *info;
    bdescr *bd;
    StgPtr p;

    bd = Bdescr((P_)block);
    p = bd->start + sizeofW(StgCompactNFDataBlock);
    while (p < bd->free) {
        ASSERT (LOOKS_LIKE_CLOSURE_PTR(p));
        info = get_itbl((StgClosure*)p);

        switch (info->type) {
        case CONSTR_1_0:
            if (!fixup_one_pointer(fixup_table, count,
                                   &((StgClosure*)p)->payload[0]))
                return rtsFalse;
        case CONSTR_0_1:
            p += sizeofW(StgClosure) + 1;
            break;

        case CONSTR_2_0:
            if (!fixup_one_pointer(fixup_table, count,
                                   &((StgClosure*)p)->payload[1]))
                return rtsFalse;
        case CONSTR_1_1:
            if (!fixup_one_pointer(fixup_table, count,
                                   &((StgClosure*)p)->payload[0]))
                return rtsFalse;
        case CONSTR_0_2:
            p += sizeofW(StgClosure) + 2;
            break;

        case CONSTR:
        case PRIM:
        case CONSTR_NOCAF:
        {
            StgPtr end;

            end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
                if (!fixup_one_pointer(fixup_table, count, (StgClosure **)p))
                    return rtsFalse;
            }
            p += info->layout.payload.nptrs;
            break;
        }

        case ARR_WORDS:
            p += arr_words_sizeW((StgArrBytes*)p);
            break;

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            fixup_mut_arr_ptrs(fixup_table, count, (StgMutArrPtrs*)p);
            p += mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
            break;

        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
        {
            uint32_t i;
            StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs*)p;

            for (i = 0; i < arr->ptrs; i++) {
                if (!fixup_one_pointer(fixup_table, count,
                                       &arr->payload[i]))
                    return rtsFalse;
            }

            p += sizeofW(StgSmallMutArrPtrs) + arr->ptrs;
            break;
        }

        case COMPACT_NFDATA:
            if (p == (bd->start + sizeofW(StgCompactNFDataBlock))) {
                // Ignore the COMPACT_NFDATA header
                // (it will be fixed up later)
                p += sizeofW(StgCompactNFData);
                break;
            }

            // fall through

        default:
            debugBelch("Invalid non-NFData closure (type %d) in Compact\n",
                       info->type);
            return rtsFalse;
        }
    }

    return rtsTrue;
}

static int
cmp_fixup_table_item (const void *e1, const void *e2)
{
    const StgWord *w1 = e1;
    const StgWord *w2 = e2;

    return *w1 - *w2;
}

static StgWord *
build_fixup_table (StgCompactNFDataBlock *block, uint32_t *pcount)
{
    uint32_t count;
    StgCompactNFDataBlock *tmp;
    StgWord *table;

    count = 0;
    tmp = block;
    do {
        count++;
        tmp = tmp->next;
    } while(tmp && tmp->owner);

    table = stgMallocBytes(sizeof(StgWord) * 2 * count, "build_fixup_table");

    count = 0;
    do {
        table[count * 2] = (W_)block->self;
        table[count * 2 + 1] = (W_)block;
        count++;
        block = block->next;
    } while(block && block->owner);

    qsort(table, count, sizeof(StgWord) * 2, cmp_fixup_table_item);

    *pcount = count;
    return table;
}

static rtsBool
fixup_loop(StgCompactNFDataBlock *block, StgClosure **proot)
{
    StgWord *table;
    rtsBool ok;
    uint32_t count;

    table = build_fixup_table (block, &count);

    do {
        if (!fixup_block(block, table, count)) {
            ok = rtsFalse;
            goto out;
        }

        block = block->next;
    } while(block && block->owner);

    ok = fixup_one_pointer(table, count, proot);

 out:
    stgFree(table);
    return ok;
}

static void
fixup_early(StgCompactNFData *str, StgCompactNFDataBlock *block)
{
    StgCompactNFDataBlock *last;

    do {
        last = block;
        block = block->next;
    } while(block);

    str->last = last;
}

static void
fixup_late(StgCompactNFData *str, StgCompactNFDataBlock *block)
{
    StgCompactNFDataBlock *nursery;
    bdescr *bd;
    StgWord totalW;

    nursery = block;
    totalW = 0;
    do {
        block->self = block;

        bd = Bdescr((P_)block);
        totalW += bd->blocks * BLOCK_SIZE_W;

        if (block->owner != NULL) {
            if (bd->free != bd->start)
                nursery = block;
            block->owner = str;
        }

        block = block->next;
    } while(block);

    str->nursery = nursery;
    str->totalW = totalW;
}

static StgClosure *
maybe_fixup_internal_pointers (StgCompactNFDataBlock *block,
                               StgClosure            *root)
{
    rtsBool ok;
    StgClosure **proot;

    // Check for fast path
    if (!any_needs_fixup(block))
        return root;

    debugBelch("Compact imported at the wrong address, will fix up"
               " internal pointers\n");

    // I am PROOT!
    proot = &root;

    ok = fixup_loop(block, proot);
    if (!ok)
        *proot = NULL;

    return *proot;
}

StgPtr
compactFixupPointers(StgCompactNFData *str,
                     StgClosure       *root)
{
    StgCompactNFDataBlock *block;
    bdescr *bd;
    StgWord total_blocks;

    block = compactGetFirstBlock(str);

    fixup_early(str, block);

    root = maybe_fixup_internal_pointers(block, root);

    // Do the late fixup even if we did not fixup all
    // internal pointers, we need that for GC and Sanity
    fixup_late(str, block);

    // Now we're ready to let the GC, Sanity, the profiler
    // etc. know about this object
    bd = Bdescr((P_)block);

    total_blocks = str->totalW / BLOCK_SIZE_W;

    ACQUIRE_SM_LOCK;
    ASSERT (bd->gen == g0);
    ASSERT (g0->n_compact_blocks_in_import >= total_blocks);
    g0->n_compact_blocks_in_import -= total_blocks;
    g0->n_compact_blocks += total_blocks;
    dbl_link_remove(bd, &g0->compact_blocks_in_import);
    dbl_link_onto(bd, &g0->compact_objects);
    RELEASE_SM_LOCK;

#if DEBUG
    if (root)
        verify_consistency_loop(str);
#endif

    return (StgPtr)root;
}
