;2016/07/04
;This routine returns, for integer input, all single-index streak beginning and ending indices.
;For example, if input=[0,0,0,1,1,8], start_i[0]=0, stop_i[0]=2, start_i[1]=3, stop_i[1]=4, and single_i=5
PRO GET_SINGLE_INDEX_STREAKS,input, $
                             START_I=start_i, $
                             STOP_I=stop_i, $
                             SINGLE_I=single_i, $
                             MIN_STREAK_TO_KEEP=min_streak, $
                             LOWSTREAK_START_I=lowStreak_start_i, $
                             LOWSTREAK_STOP_I=lowStreak_stop_i, $
                             OUT_STREAKLENS=streakLens, $
                             LUN=lun, $
                             N_STREAKS=nStreaks, $
                             N_SINGLE=nSingle, $
                             KEEP_SINGLES=keep_singles, $
                             NO_PRINT_SUMMARY=no_print_summary

  type                              = SIZE(input,/TYPE)
  IF (type NE 1) AND $
     (type NE 2) AND $
     (type NE 3) AND $
     (type NE 12) AND $
     (type NE 13) AND $
     (type NE 14) AND $
     (type NE 15) THEN BEGIN  
     PRINT,"Must provide integer-type data!"
     PRINT,"Laterz ..."
     STOP
  ENDIF

  ;;Assume we have no streaks
  nStreaks                          = 0

  IF N_ELEMENTS(lun) EQ 0 THEN lun  = -1
  IF N_ELEMENTS(no_print_summary) EQ 0 THEN BEGIN
     no_print_summary               = 0
  ENDIF

  ;; CHECK_SORTED,input,is_sorted
  ;; IF ~is_sorted THEN BEGIN
  ;;    PRINTF,lun,"Input to GET_STREAKS is not sorted!"
  ;;    RETURN
  ;; ENDIF

  diff                              = input - shift(input,1)

  start_i                           = WHERE(diff NE 0)
  WHERECHECK,start_i  ;;Make sure this is OK

  stop_i                            = [start_i[1:-1]-1,N_ELEMENTS(input)-1]

  single_ii                         = WHERE(start_i EQ stop_i,nSingle, $
                                            COMPLEMENT=keep_ii, $
                                            NCOMPLEMENT=nStreaks, $
                                            /NULL)
  WHERECHECK,single_ii,keep_ii
  IF ~KEYWORD_SET(keep_singles) THEN BEGIN
     start_i                        = start_i[keep_ii]
     stop_i                         = stop_i[keep_ii]
  ENDIF
  single_i                          = start_i[single_ii]


  IF KEYWORD_SET(min_streak) THEN BEGIN
     nStreaks                       = 0
     PRINTF,lun,'Minimum streak for keeping: ' + STRCOMPRESS(min_streak,/REMOVE_ALL)
     minStreak_ii                   = WHERE((stop_i-start_i) GE min_streak,COMPLEMENT=lowStreak_ii,/NULL,NCOMPLEMENT=nLow)
     IF N_ELEMENTS(minStreak_ii) GT 0 THEN BEGIN
        
        IF nLow GT 0 THEN BEGIN
           PRINT,'Losing ' + STRCOMPRESS(nLow,/REMOVE_ALL) + 'due to min streak requirement...'
           lowStreak_start_i        = start_i[lowStreak_ii]
           lowStreak_stop_i         = stop_i[lowStreak_ii]
        ENDIF

        start_i                     = start_i[minStreak_ii]
        stop_i                      = stop_i[minStreak_ii]
        nStreaks++
     ENDIF
  ENDIF

  IF ~no_print_summary THEN BEGIN
     PRINTF,lun,""
     PRINTF,lun,FORMAT='("**GET_SINGLE_INDEX_STREAKS**")'
     PRINTF,lun,FORMAT='("N Streaks                     :",T35,I0)',nStreaks
     IF ~KEYWORD_SET(keep_singles) THEN BEGIN
        PRINTF,lun,FORMAT='("N single values               :",T35,I0)',nSingle
     ENDIF
  ENDIF

  streakLens                        = stop_i-start_i

END