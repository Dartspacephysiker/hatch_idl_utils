;2016/12/30
;Convert an array with positive and negative values to a mixture of log and linear values. All values below 1 are linear, and all
;values above are log-a-tized.
FUNCTION LINLOG_ARR,data, $
                    REPLACE_DATA=replace, $
                    USER_INDS=user_inds, $
                    OUTINDSTRUCT=out_indStruct

  COMPILE_OPT IDL2

  nData = N_ELEMENTS(data)

  IF (nData EQ 0) THEN BEGIN
     PRINT,"Dumb"
     RETURN,!NULL
  ENDIF

  CASE 1 OF
     KEYWORD_SET(user_inds): BEGIN
        inds = user_inds
     END
     ELSE: BEGIN
        inds = LINDGEN(N_ELEMENTS(data))
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(replace): BEGIN

     END
     ELSE: BEGIN
        oldData = data
     END
  ENDCASE

  tmp_lInds   = CGSETINTERSECTION(inds,WHERE(ABS(data) GE 1),COUNT=nLogMe)
  tmp_linInds = CGSETINTERSECTION(inds,WHERE(ABS(data) LT 1),COUNT=nLin  )

  IF nLogMe GT 0 THEN BEGIN
     lInds = GET_POS_NEG_INDICES(data, $
                                     USER_INDS=tmp_lInds)
  ENDIF
  
  IF lInds.pos[0] NE -1 THEN BEGIN
     data[lInds.pos] = ALOG10(data[lInds.pos])
  ENDIF

  IF lInds.neg[0] NE -1 THEN BEGIN
     data[lInds.neg] = ALOG10(ABS(data[lInds.neg]))
  ENDIF

  IF ARG_PRESENT(out_indStruct) THEN BEGIN
     out_indStruct = {pos:lInds.pos, $
                      neg:lInds.neg, $
                      lin:tmp_linInds}
  ENDIF

  IF KEYWORD_SET(no_struct) THEN BEGIN

     IF ~KEYWORD_SET(replace) THEN BEGIN
        retData = TEMPORARY(data)
        data    = TEMPORARY(oldData)
        RETURN,retData
     ENDIF ELSE BEGIN
        RETURN,data
     ENDELSE
  ENDIF ELSE BEGIN
     datStr     = {pos:0B, $
                   neg:0B, $
                   lin:0B}

     IF lInds.pos[0] NE -1 THEN BEGIN
        STR_ELEMENT,datStr,'pos',data[lInds.pos],/ADD_REPLACE
     ENDIF

     IF lInds.neg[0] NE -1 THEN BEGIN
        STR_ELEMENT,datStr,'neg',data[lInds.neg],/ADD_REPLACE
     ENDIF

     IF nLin GT 0 THEN BEGIN
        STR_ELEMENT,datStr,'lin',data[tmp_linInds],/ADD_REPLACE
     ENDIF

     data    = TEMPORARY(oldData)

     RETURN,datStr

  ENDELSE


END
