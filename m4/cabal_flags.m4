# CABAL_FLAGS(VARIABLE, FLAGS)
# ----------------------------
# Sets VARIABLE (and substitutes it) to the Cabal flags of FLAGS, a list of
# flag:SETTING: +flag when the shell variable SETTING is YES, -flag otherwise.
AC_DEFUN([CABAL_FLAGS],[
    $1=
    for cabal_flag in m4_normalize([$2]); do
        eval "cabal_flag_setting=\$${cabal_flag@%:@*:}"
        if test "$cabal_flag_setting" = YES; then
            $1="$$1 +${cabal_flag%%:*}"
        else
            $1="$$1 -${cabal_flag%%:*}"
        fi
    done
    $1=${$1@%:@ }
    AC_SUBST([$1])
])
