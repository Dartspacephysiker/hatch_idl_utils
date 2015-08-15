;2015/08/15
;Sick and tired of it, man. Just write dat fonctionne
FUNCTION CONV_UTC_TO_JULDAY,utcArray,tStamps_out

  conv2julDay=2440587.5

  julDayArray = (utcArray/86400.0D) + conv2julDay

  CALDAT, julDayArray, month, day, year, hour, minute, second

  tStamps_out = TIMESTAMP(YEAR = year, MONTH = month, $
                                 DAY = day, HOUR = hour, MINUTE = minute, SECOND = second)
  RETURN,julDayArray

END
