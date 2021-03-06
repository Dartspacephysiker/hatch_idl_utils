FUNCTION ROUND_NTH_SIG_DIGIT,nums,sig_digit, $
                             FLOOR=floor, $
                             CEILING=ceiling, $
                             DOUBLE=double

  COMPILE_OPT IDL2,STRICTARRSUBS

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

  CASE 1 OF
     KEYWORD_SET(floor): BEGIN
        func   = 'FLOOR'
     END
     KEYWORD_SET(ceiling): BEGIN
        func   = 'CEIL'
     END
     ELSE: BEGIN
        func   = 'ROUND'
     END
  ENDCASE

  roundedNums  = (KEYWORD_SET(double) ? $
                  DOUBLE( (CALL_FUNCTION(func,nums/divisor,/L64)) )*divisor : $
                  (CALL_FUNCTION(func,nums/divisor,/L64))*divisor           )

  RETURN,roundedNums

END