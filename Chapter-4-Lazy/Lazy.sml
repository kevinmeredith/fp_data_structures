datatype 'alpha susp = $ of 'alpha

fun sum n = if n = 0 then 1
            else n + sum(n-1)