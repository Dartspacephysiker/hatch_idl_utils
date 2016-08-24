;2016/08/24 In response to a need while working with Dst data
FUNCTION TIMEGEN_EASIER,t1_julDay,t2_julDay, $
                        NMONTHS_PER_CALC=monthInterval, $
                        NDAYS_PER_CALC=dayInterval, $
                        NYEARS_PER_CALC=yearInterval, $
                        NHOURS_PER_CALC=hourInterval, $
                        OUT_MAXSEP=maxSep


  COMPILE_OPT idl2

  nTypes   = 0
  IF KEYWORD_SET(monthInterval) THEN nTypes++
  IF KEYWORD_SET(dayInterval  ) THEN nTypes++
  IF KEYWORD_SET(yearInterval ) THEN nTypes++
  IF KEYWORD_SET(hourInterval ) THEN nTypes++

  IF nTypes EQ 0 THEN BEGIN
     PRINT,"No interval provided! Assuming you just want to live you life."
     PRINT,"Setting monthInterval to 3 ..."
     monthInterval        = 3
  ENDIF

  CASE nTypes OF
     1: BEGIN
        IF KEYWORD_SET(monthInterval) THEN BEGIN
           monthInterval  = ROUND(monthInterval) ;TIMEGEN would do it to you anyway
           PRINT,FORMAT='("Month interval",T20,":",T23,F0.1)',monthInterval
           itvlUnits      = "Months"
           stepSize       = monthInterval
           maxSep         = 1
        ENDIF
        IF KEYWORD_SET(dayInterval  ) THEN BEGIN
           PRINT,FORMAT='("Day interval",T20,":",T23,F0.1)',dayInterval
           itvlUnits      = "Days"
           stepSize       = dayInterval
           maxSep         = .041666668D
        ENDIF
        IF KEYWORD_SET(yearInterval ) THEN BEGIN
           yearInterval   = ROUND(yearInterval) ;TIMEGEN would do it to you anyway
           PRINT,FORMAT='("Year interval",T20,":",T23,F0.1)',YearInterval
           itvlUnits      = "Years"
           stepSize       = yearInterval
           maxSep         = 30.
        ENDIF
        IF KEYWORD_SET(hourInterval ) THEN BEGIN
           PRINT,FORMAT='("Hour interval",T20,":",T23,F0.1)',HourInterval
           itvlUnits      = "Hours"
           stepSize       = hourInterval
           maxSep         = minDiff
        ENDIF
     END
     ELSE: BEGIN
        PRINT,"Can't set more than one interval type! Out ..."
        RETURN,-1
     END
  ENDCASE

  ;;Get time arrays in Julian days, since that's what TIMEGEN is all about
  timeArr    = TIMEGEN(UNITS=itvlUnits,STEP_SIZE=stepSize, $
                       START=t1_julDay, $
                       FINAL=t2_julDay)
                    
  RETURN,timeArr

END