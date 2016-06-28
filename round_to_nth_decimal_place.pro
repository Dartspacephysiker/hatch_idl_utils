FUNCTION ROUND_TO_NTH_DECIMAL_PLACE,nums,decimal_place, $
                               DOUBLE=double

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(decimal_place) THEN BEGIN
     decimal_place = 0
  ENDIF

  IF KEYWORD_SET(double) THEN BEGIN
     divisor   = 10.D
  ENDIF ELSE BEGIN
     divisor   = 10.
  ENDELSE

  divisor      = divisor^decimal_place

  roundedNums  = ROUND(nums/divisor)*divisor

  RETURN,roundedNums

END