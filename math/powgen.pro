;2017/04/19
FUNCTION POWGEN,minVal,maxVal,step, $
                NATURAL=natural

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(natural) THEN BEGIN
     vals = minVal*step^(LINDGEN(ALOG(maxVal/minVal)/ALOG(step)+1))
  ENDIF ELSE BEGIN
     vals = minVal*step^(LINDGEN(ALOG10(maxVal/minVal)/ALOG10(step)+1))
  ENDELSE

  RETURN,vals

END
