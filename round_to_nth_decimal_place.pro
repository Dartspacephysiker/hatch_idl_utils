FUNCTION ROUND_TO_NTH_DECIMAL_PLACE,nums,decimal_place, $
                                    FLOOR=floor, $
                                    CEILING=ceiling, $
                                    DOUBLE=double, $
                                    LEAVE_AS_INTEGER=leave_as_integer

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~KEYWORD_SET(decimal_place) THEN BEGIN
     decimal_place = 0
  ENDIF

  IF KEYWORD_SET(double) THEN BEGIN
     divisor   = 10.D
  ENDIF ELSE BEGIN
     divisor   = 10.
  ENDELSE

  divisor      = divisor^decimal_place

  IF KEYWORD_SET(leave_as_integer) THEN BEGIN
     multiplier = 1LL
  ENDIF ELSE BEGIN
     multiplier = divisor
  ENDELSE

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
                  DOUBLE( (CALL_FUNCTION(func,nums/divisor,/L64)) ) : $
                  (CALL_FUNCTION(func,nums/divisor,/L64))           )*multiplier

  IF KEYWORD_SET(leave_as_integer) THEN roundedNums = FIX(roundedNums,TYPE=14)

  RETURN,roundedNums

END