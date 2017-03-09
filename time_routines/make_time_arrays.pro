PRO MAKE_TIME_ARRAYS, $
   IN_STARTYEAR=startYear, $
   IN_STOPYEAR=stopYear, $
   IN_HOURARR=hourArr, $
   OUT_YEARARR=finalYearArr, $
   OUT_DOYARR=finalDOYArr, $
   OUT_MONTHARR=finalMonthArr, $
   OUT_DAYARR=finalDayArr, $
   OUT_HOURARR=finalHourArr, $
   OUT_JULDAY=julDay
                     
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; IF ~KEYWORD_SET(nYears) THEN BEGIN
  ;;    PRINT,"Must provide start year!"
  ;;    STOP
  ;;    ;; RETURN
  ;; ENDIF

  IF N_ELEMENTS(startYear) EQ 0 THEN BEGIN
     startYear    = 1970
     PRINT,'Default startYear : ' + STRCOMPRESS(startYear,/REMOVE_ALL)
  ENDIF
     
  IF N_ELEMENTS(startYear) EQ 0 THEN BEGIN
     stopYear    = 1971
     PRINT,'Default stopYear : ' + STRCOMPRESS(stopYear,/REMOVE_ALL)
  ENDIF

  nYears          = FIX(stopYear)-FIX(startYear)+1 ;inclusive

  years           = LINDGEN(nYears)+startYear

  finalYearArr    = !NULL
  finalDOYArr     = !NULL
  finalMonthArr   = !NULL
  finalDayArr     = !NULL
  finalHourArr    = !NULL

  hourFrac        = TEMPORARY(hourArr/24.D)
  nHour           = N_ELEMENTS(hourFrac)

  FOR i=0,nYears-1 DO BEGIN
     nDays        = 365 + ( (years[i] MOD 4) EQ 0 )

     
     tempDOY      = MAKE_ARRAY(nHour,nDays,/FLOAT)
     tempHourArr  = MAKE_ARRAY(nHour,nDays,/FLOAT)

     tempYArr     = REPLICATE(years[i],nDays*nHour)

     FOR jo=0,nHour-1 DO BEGIN
        ;; tempDOY   = [tempDOY,tempDOY+hourFrac[jo]]
        tempDOY[jo,*] = FINDGEN(nDays)+1.+hourFrac[jo]
        tempHourArr[jo,*] = MAKE_ARRAY(nDays,VALUE=hourArr[jo],/FLOAT)
     ENDFOR
     tempDOY     = REFORM(tempDOY,nDays*nHour)
     tempHourArr = REFORM(tempHourArr,nDays*nHour)

     ;; tempYArr     = [tempYArr,tempYArr, $
     ;;              tempYArr,tempYArr]
     
     CALDAT,JULDAY(1, tempDOY, years[i],tempHourArr),tempMonth,tempDay,tempYear,tempHour

     finalYearArr    = [finalYearArr,tempYArr]
     finalDOYArr     = [finalDOYArr,tempDOY]
     finalMonthArr   = [finalMonthArr,tempMonth]
     finalDayArr     = [finalDayArr,tempDay]
     finalHourArr    = [finalHourArr,tempHour]
  ENDFOR

  julDay             = JULDAY(finalMonthArr,finalDayArr,finalYearArr,finalHourArr,0,0)


END