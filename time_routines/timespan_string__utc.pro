;2017/03/17
FUNCTION TIMESPAN_STRING__UTC,t1,t2, $
                              MSEC=msec, $
                              INCLUDE_DATE=include_date

  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE N_ELEMENTS(t1) OF
     0: BEGIN
        RETURN,'ERROR!'
     END
     1: BEGIN

        t1In = t1

        IF N_ELEMENTS(t2) EQ 0 THEN BEGIN
           RETURN,'ERROR!'
        ENDIF ELSE BEGIN
           t2In = t2
        ENDELSE

     END
     ELSE: BEGIN

        t1In  = t1[0]
        t2In  = t1[1]

     END
  ENDCASE

  CASE SIZE(t1In,/TYPE) OF
     5: BEGIN
        t1Str = TIME_TO_STR(t1In,MSEC=msec)
     END
     7: BEGIN
        t1Str = t1In
     END
  ENDCASE

  CASE SIZE(t2In,/TYPE) OF
     0: BEGIN
        RETURN,'ERROR!'
     END
     5: BEGIN
        t2Str = TIME_TO_STR(t2In,MSEC=msec)
     END
     7: BEGIN
        t2Str = t2In
     END
  ENDCASE


  t1Str = t1Str.Replace(':','_')
  t2Str = t2Str.Replace(':','_')

  ;; IF KEYWORD_SET(msec) THEN BEGIN
     t1Str = t1Str.Replace('.','__')
     t2Str = t2Str.Replace('.','__')
  ;; ENDIF
     
  t1Str = STRSPLIT(t1Str,'/',/EXTRACT)
  t2Str = STRSPLIT(t2Str,'/',/EXTRACT)

  IF ~KEYWORD_SET(include_date) THEN BEGIN
     IF N_ELEMENTS(t1Str) GT 1 THEN t1Str = t1Str[1]
     IF N_ELEMENTS(t2Str) GT 1 THEN t2Str = t2Str[1]
  ENDIF

  RETURN,t1Str+'-'+t2Str

END
