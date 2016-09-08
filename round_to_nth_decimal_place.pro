FUNCTION ROUND_TO_NTH_DECIMAL_PLACE,nums,decimal_place, $
                                    FLOOR=floor, $
                                    CEILING=ceiling, $
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

  roundedNums  = (CALL_FUNCTION(func,nums/divisor))*divisor

  RETURN,roundedNums

END