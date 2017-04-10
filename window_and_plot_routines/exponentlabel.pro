FUNCTION EXPONENTLABEL, axis, index, number

  e_notation = 1

                                ; A special case.
  IF number EQ 0 THEN RETURN, '0' 

                                ; Assuming multiples of 10 with format.
  ex            = String(number, Format='(e8.0)') 
  pt            = StrPos(ex, '.')

  first         = STRMID(ex, 0, pt)
  sign          = STRMID(ex, pt+2, 1)
  thisExponent  = STRMID(ex, pt+3)

                                ; Shave off leading zero in exponent
  WHILE STRMID(thisExponent, 0, 1) EQ '0' DO thisExponent = STRMID(thisExponent, 1)

                                ; Fix for sign and missing zero problem.
  IF (LONG(thisExponent) EQ 0) THEN BEGIN
     sign          = ''
     thisExponent  = '0'
  ENDIF

  ;; Make the exponent a superscript.
  CASE 1 OF
     KEYWORD_SET(e_notation): BEGIN

        IF sign EQ '-' THEN BEGIN
           RETURN, (STRMATCH(first,'  1',/FOLD_CASE) ? '1' : first + '') + 'E' + sign + thisExponent + '' 
        ENDIF ELSE BEGIN             
           RETURN, (STRMATCH(first,'  1',/FOLD_CASE) ? '1' : first + '') + 'E' + thisExponent + ''
        ENDELSE

     END
     ELSE: BEGIN

        IF sign EQ '-' THEN BEGIN
           RETURN, (STRMATCH(first,'  1',/FOLD_CASE) ? '' : first + 'x') + '10!U' + sign + thisExponent + '!N' 
        ENDIF ELSE BEGIN             
           RETURN, (STRMATCH(first,'  1',/FOLD_CASE) ? '' : first + 'x') + '10!U' + thisExponent + '!N'
        ENDELSE

     END
  ENDCASE



  
END
