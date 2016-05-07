;2016/04/29 Gots to know DAT
FUNCTION SECONDS_SINCE_YEAR_BEGINNING__UTC,times

  COMPILE_OPT idl2

  ;;What do we have here, anyway?
  CASE SIZE(times,/TYPE) OF
     0: BEGIN
        PRINT,"What are you thinking? You've provided ... well, nothing!"
        PRINT,"Laterz ..."
        RETURN,-1
     END
     7: BEGIN
        tempTimes = DOUBLE(STR_TO_TIME(times))
     END
     ELSE: BEGIN
        ;;Make sure this isn't gobbledygook
        test      = TIME_TO_STR(times[0])
        IF test EQ -1 THEN BEGIN
           PRINT,"Not sure what's wrong with times you provided, but they're bogus. Check it out."
           STOP
        ENDIF
        tempTimes = DOUBLE(times)
     END
  ENDCASE

  ;;Set up years since, say, 1900
  years       = INDGEN(90)+1970
  yearStr     = STRING(FORMAT='(I0,"-01-01/00:00:00")',years)
  yearUTC     = DOUBLE(STR_TO_TIME(yearStr))

  bestDiff    = VALUE_LOCATE(yearUTC,tempTimes,/L64)

  seconds     = tempTimes-yearUTC[bestDiff]

  ;;Better error check
  test        = WHERE(seconds LT 0)
  IF test[0] NE -1 THEN BEGIN
     PRINT,"SECONDS_SINCE_YEAR_BEGINNING__UTC: Something bad happened when stripping the year away..."
     RETURN,-1
  ENDIF

  RETURN,seconds

END