;2015/08/12
;Sick and tired of it, man. Just write dat fonctionne
FUNCTION CONV_JULDAY_TO_UTC,julday_array,i_gt_1970,i_lt_1970

  ;;Get all the times as UTC times, for crying out loud
  ;;Can't go before 1970, of course!
  conv2utc=2440587.5
  i_1970 = WHERE(julday_array GT conv2utc,COMPLEMENT=bad_i,/NULL)

  utc=(julday_array(i_1970)-conv2utc)*86400.0

  PRINT,'TOTAL ELEMENTS: ' + STRCOMPRESS(N_ELEMENTS(julday_array),/REMOVE_ALL)
  PRINT,'ELEMENTS BEFORE 1970: ' + STRCOMPRESS(N_ELEMENTS(bad_i),/REMOVE_ALL)

  i_gt_1970 = i_1970
  i_lt_1970 = bad_i

  RETURN,utc

END
