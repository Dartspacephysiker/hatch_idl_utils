;2016/07/09
; BACK: Return to standard SDT data structure with tags x (time) and y (data).
FUNCTION CONV_TO_FA_FIELDS_STRUCT,dat, $
                                  BACK=back

  COMPILE_OPT idl2

  IF SIZE(dat,/TYPE) NE 8 THEN BEGIN
     PRINT,'Must provide structure for this routine...'
     RETURN,-1
  ENDIF

  IF KEYWORD_SET(back) THEN BEGIN
     IF (WHERE(STRMATCH(TAG_NAMES(dat),'time',/FOLD_CASE) EQ 1))[0] EQ -1 THEN BEGIN
        PRINT,"Bad struct provided; no tag named 'TIME'."
        RETURN,-1
     ENDIF

     new    = {x:dat.time}

     compInds = WHERE(STRMATCH(TAG_NAMES(dat),'comp*',/FOLD_CASE),nDim)

     IF nDim EQ 0 THEN BEGIN
        PRINT,"No tags with pref 'comp' in this structure!"
        RETURN,0
     ENDIF

     newDat = MAKE_ARRAY(N_ELEMENTS(dat.comp1),nDim)
     
     FOR i=0,nDim-1 DO BEGIN
        newDat[*,i] = dat.(compInds[i])
     ENDFOR

     new   = CREATE_STRUCT(new,'y',newDat)

     RETURN,new
  ENDIF

  ;; nDim      = NDIMEN(dat.y)
  dims      = SIZE(dat.y,/DIM)
  CASE N_ELEMENTS(dims) OF
     1: nDim = 1
     2: nDim = dims[1]
     ELSE: BEGIN
        PRINT,"huh?"
        STOP
     END
  ENDCASE

  compStrs  = 'comp' + STRCOMPRESS(INDGEN(nDim)+1,/REMOVE_ALL)

  new       = {time:dat.x,ncomp:nDim}

  FOR i=0,nDim-1 DO BEGIN
     new    = CREATE_STRUCT(new,compStrs[i],dat.y[*,i])
  ENDFOR

  RETURN,new

END