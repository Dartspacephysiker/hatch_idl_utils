;2018/05/09
FUNCTION CARDINAL_TO_ORDINAL_STRING,cardinal

  COMPILE_OPT IDL2,STRICTARRSUBS

  PRINT,"UNFINISHED"
  STOP

  nHere = N_ELEMENTS(cardinal)

  IF nHere EQ 0 THEN BEGIN
     PRINT,"None!"
     RETURN,''
  ENDIF

  ord = MAKE_ARRAY(nHere,/STRING,VALUE='')

  IF SIZE(cardinal,/TYPE) NE 7 THEN $
     IF ((WHERE(SIZE(cardinal,/TYPE) EQ [2,3,14]))[0] EQ -1 THEN BEGIN
     PRINT,"Wrong data types, fool."
     RETURN,ord
  ENDIF

  FOR jj=0,N_ELEMENTS(cardinal)-1 DO BEGIN

     

  ENDFOR

END
