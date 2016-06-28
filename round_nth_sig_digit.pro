FUNCTION ROUND_NTH_SIG_DIGIT,nums,sig_digit, $
                               DOUBLE=double

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(sig_digit) THEN BEGIN
     sig_digit = 1
  ENDIF

  dPlace       = MAX(FLOOR(ALOG10(ABS(nums))))-sig_digit+1

  IF KEYWORD_SET(double) THEN BEGIN
     divisor   = 10.D
  ENDIF ELSE BEGIN
     divisor   = 10.
  ENDELSE

  divisor      = divisor^dPlace

  roundedNums  = ROUND(nums/divisor)*divisor

  RETURN,roundedNums

END