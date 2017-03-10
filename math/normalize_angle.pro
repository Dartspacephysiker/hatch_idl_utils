;2017/03/10
FUNCTION NORMALIZE_ANGLE,values,minVal,maxVal, $
                         DEGREE=degree

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(minVal) EQ 0 THEN BEGIN
     minVal   = 0.D
  ENDIF

  IF N_ELEMENTS(maxVal) EQ 0 THEN BEGIN
     maxVal   = KEYWORD_SET(degree) ? 360.D  : 2.D*!PI
  ENDIF

  width       = maxVal - minVal ;
  offsetValue = values - minVal ;  value relative to 0

  RETURN, ( offsetValue - ( FLOOR( offsetValue / width ) * width ) ) + minVal

END
