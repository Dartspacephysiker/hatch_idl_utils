;2018/05/09
FUNCTION CARDINAL_TO_ORDINAL_STRING,cardinal, $
                                    TOUPCASE=toUpcase

  COMPILE_OPT IDL2,STRICTARRSUBS

  nHere = N_ELEMENTS(cardinal)

  IF nHere EQ 0 THEN BEGIN
     PRINT,"None!"
     RETURN,''
  ENDIF

  suffs = ['th','st','nd','rd','th','th','th','th','th','th']
  suff111213 = 'th'

  IF KEYWORD_SET(toUpcase) THEN BEGIN
     suffs = '!U' + suffs + '!N'
     suff111213 = '!U' + suff111213 + '!N'

  ENDIF

  lastDigs = MAKE_ARRAY(nHere,VALUE=0,/LONG)
  ord = MAKE_ARRAY(nHere,/STRING,VALUE='')

  IF SIZE(cardinal,/TYPE) EQ 7 THEN BEGIN
     ords = cardinal
  ENDIF ELSE BEGIN
     IF (WHERE(SIZE(cardinal,/TYPE) EQ [2,3,14]))[0] EQ -1 THEN BEGIN
        PRINT,"Wrong data types, fool."
        RETURN,ord
     ENDIF

     ords = STRING(FORMAT='(I0)',cardinal)
  ENDELSE

  strLens = STRLEN(ords)
  FOR k=0,nHere-1 DO BEGIN
     lastDigs[k] = LONG(STRMID(ords[k],strLens[k]-1,1))
  ENDFOR

  inds11 = WHERE(LONG(ords) EQ 11,n11,/NULL)
  inds12 = WHERE(LONG(ords) EQ 12,n12,/NULL)
  inds13 = WHERE(LONG(ords) EQ 13,n13,/NULL)

  IF (n11+n12+n13) GT 0 THEN BEGIN
     inds111213 = [TEMPORARY(inds11), $
                   TEMPORARY(inds12), $
                   TEMPORARY(inds13)]

     ords[inds111213] += suff111213
     
     ;; Now check the not-11s,12s,13s
     notThose = CGSETDIFFERENCE(LINDGEN(nHere),inds111213,COUNT=nNotThose)

     IF nNotThose GT 0 THEN BEGIN
        ords[notThose] += suffs[lastDigs[notThose]]
     ENDIF

  ENDIF ELSE BEGIN

     ords = ords + suffs[lastDigs]

  ENDELSE

  RETURN,ords

END
