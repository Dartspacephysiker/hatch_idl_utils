;2015/10/14
;This routine returns, for integer input, all streak beginning and ending indices.
;For example, if input=[0,1,2,4,5,8], start_i[0]=0, stop_i[0]=2, start_i[1]=3, stop_i[1]=4, and single_i=5
;2016/02/19 Added OUT_STREAKLENS keyword
PRO GET_STREAKS,input, $
                START_I=start_i, $
                STOP_I=stop_i, $
                SINGLE_I=single_i, $
                MIN_STREAK_TO_KEEP=min_streak, $
                LOWSTREAK_START_I=lowStreak_start_i, $
                LOWSTREAK_STOP_I=lowStreak_stop_i, $
                OUT_STREAKLENS=streakLens, $
                LUN=lun, $
                NO_PRINT_SUMMARY=no_print_summary

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
  single_ii = WHERE(start_i EQ stop_i,COMPLEMENT=keep_ii,/NULL)
  WHERECHECK,single_ii,keep_ii

  start_i   = start_i[keep_ii]
  stop_i    = stop_i[keep_ii]
  single_i  = start_i[single_ii]

  IF KEYWORD_SET(min_streak) THEN BEGIN
     PRINTF,lun,'Minimum streak for keeping: ' + STRCOMPRESS(min_streak,/REMOVE_ALL)
     minStreak_ii = WHERE((stop_i-start_i) GE min_streak,COMPLEMENT=lowStreak_ii,/NULL,NCOMPLEMENT=nLow)
     IF N_ELEMENTS(minStreak_ii) GT 0 THEN BEGIN
        
        IF nLow GT 0 THEN BEGIN
           PRINT,'Losing ' + STRCOMPRESS(nLow,/REMOVE_ALL) + 'due to min streak requirement...'
           lowStreak_start_i = start_i[lowStreak_ii]
           lowStreak_stop_i  = stop_i[lowStreak_ii]
        ENDIF

        start_i              = start_i[minStreak_ii]
        stop_i               = stop_i[minStreak_ii]
     ENDIF
  ENDIF

  IF ~no_print_summary THEN BEGIN
     PRINTF,lun,""
     PRINTF,lun,FORMAT='("**GET_STREAKS**")'
     PRINTF,lun,FORMAT='("N Streaks                     :",T35,I0)',N_ELEMENTS(start_i)
     PRINTF,lun,FORMAT='("N single values               :",T35,I0)',N_ELEMENTS(single_i)
  ENDIF

  streakLens                 = stop_i-start_i

END