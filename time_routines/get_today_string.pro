;2015/10/23
FUNCTION GET_TODAY_STRING,DO_YYYYMMDD_FMT=do_YYYYMMDD_fmt

  IF KEYWORD_SET(do_YYYYMMDD_fmt) THEN BEGIN
     ;; tempString = TIME_TO_STR(SYSTIME(/SECONDS))
     ;; day        = STRMID(tempString, 8, 2)
     ;; year       = STRMID(tempString, 0, 4)
     ;; mon        = STRMID(tempString, 5, 2)
     ;; hoyDia     = STRING(FORMAT='(A4,A2,A2)',year,mon,day)
     SPAWN,"date +'%Y%m%d'",hoyDia

  ENDIF ELSE BEGIN
     hoyDia     = STRCOMPRESS(STRMID(SYSTIME(), 4, 3),/REMOVE_ALL) + "_" + $
                  STRCOMPRESS(STRMID(SYSTIME(), 8,2),/REMOVE_ALL) + "_" +  $
                  STRCOMPRESS(STRMID(SYSTIME(), 22, 2),/REMOVE_ALL)
  ENDELSE

  RETURN,hoyDia

END