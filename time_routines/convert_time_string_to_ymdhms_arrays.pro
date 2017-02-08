;;02/08/17
PRO CONVERT_TIME_STRING_TO_YMDHMS_ARRAYS,timeStr, $
   OUT_YEARARR=yearArr, $
   OUT_DOYARR=DOYArr, $
   OUT_MONTHARR=monthArr, $
   OUT_DAYARR=dayArr, $
   OUT_HOURARR=hourArr, $
   OUT_MINARR=minArr, $
   OUT_SECARR=secArr

  COMPILE_OPT IDL2

  nStr = N_ELEMENTS(timeStr)
  IF nStr EQ 0 THEN BEGIN
     PRINT,"No input provided!"
     RETURN
  ENDIF

  IF SIZE(timeStr,/TYPE) NE 7 THEN BEGIN
     PRINT,"Must input string!"
     RETURN
  ENDIF

  IF (WHERE(STRLEN(timeStr[0:((nStr-1) < 1000000)]) LT 19))[0] NE -1 THEN BEGIN
     PRINT,"This is obviously a deception. Strings aren't even 19 chars long!"
     STOP
  ENDIF ELSE BEGIN
     maxMinDigs = MIN(STRLEN(timeStr[0:((nStr-1) < 1000000)]))-17
  ENDELSE  

  yearArr   = FIX(STRMID(timeStr, 0,4))
  monthArr  = FIX(STRMID(timeStr, 5,2))
  dayArr    = FIX(STRMID(timeStr, 8,2))
  hourArr   = FIX(STRMID(timeStr,11,2))
  minArr    = FIX(STRMID(timeStr,14,2))
  
  IF maxMinDigs GT 2 THEN BEGIN
     secArr = FLOAT(STRMID(timeStr,17,maxMinDigs))
  ENDIF ELSE BEGIN
     secArr = FIX(STRMID(timeStr,17,2))
  ENDELSE

  ;;Now DOY

  ;; Days before start of each month.
  yDays  = [0,31,59,90,120,151,181,212,243,273,304,334,366]
  ;; yCumul = TOTAL(yDays,/CUMULATIVE)
  ;; DOYArr = yCumul[monthArr-1]+dayArr
  DOYArr = yDays[(monthArr-1)]+dayArr

  leapMe = WHERE( ( ( ( ( yearArr MOD 4 ) EQ 0 ) AND ( ( yearArr MOD 100 ) NE 0 ) ) OR       $
                    ( ( yearArr MOD 400 ) EQ 0 )                                       ) AND $
                  monthArr GT 2, $
                  nLeap)

  IF nLeap GT 0 THEN BEGIN
     
     DOYArr[leapMe] += 1

  ENDIF


END

