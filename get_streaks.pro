;2015/10/14
;This routine returns, for integer input, all streak beginning and ending indices.
;For example, if input=[0,1,2,4,5,8], start_i[0]=0, stop_i[0]=2, start_i[1]=3, stop_i[1]=4, and single_i=5
PRO GET_STREAKS,input,START_I=start_i,STOP_I=stop_i,SINGLE_I=single_i,LUN=lun,NO_PRINT_SUMMARY=no_print_summary

  IF N_ELEMENTS(lun) EQ 0 THEN lun = -1
  IF N_ELEMENTS(no_print_summary) EQ 0 THEN no_print_summary = 0

  CHECK_SORTED,input,is_sorted
  IF ~is_sorted THEN BEGIN
     PRINTF,lun,"Input to GET_STREAKS is not sorted!"
     RETURN
  ENDIF

  diff = input - shift(input,1)

  start_i = WHERE(diff NE 1)
  WHERECHECK,start_i  ;;Make sure this is OK

  stop_i = [start_i[1:-1]-1,N_ELEMENTS(input)-1]
  single_ii = WHERE(start_i EQ stop_i,COMPLEMENT=keep_ii)
  WHERECHECK,single_ii,keep_ii

  start_i   = start_i[keep_ii]
  stop_i    = stop_i[keep_ii]
  single_i  = start_i[single_ii]

  IF ~no_print_summary THEN BEGIN
     PRINTF,lun,""
     PRINTF,lun,FORMAT='("**GET_STREAKS**")'
     PRINTF,lun,FORMAT='("N Streaks                     :",T35,I0)',N_ELEMENTS(start_i)
     PRINTF,lun,FORMAT='("N single values               :",T35,I0)',N_ELEMENTS(single_i)
  ENDIF

END