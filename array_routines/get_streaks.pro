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
                N_STREAKS=n_streaks, $
                NO_PRINT_SUMMARY=no_print_summary, $
                QUIET=quiet

  COMPILE_OPT idl2

  IF ~IS_INT_TYPE(input) THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,"GET_STREAKS: Input not int type!"
     RETURN
  ENDIF

  ;;Assume we have no streaks
  n_streaks                         = 0

  IF N_ELEMENTS(lun) EQ 0 THEN lun  = -1
  IF N_ELEMENTS(no_print_summary) EQ 0 THEN BEGIN
     no_print_summary               = 0
  ENDIF

  CHECK_SORTED,input,is_sorted,QUIET=quiet
  IF ~is_sorted THEN BEGIN
     PRINTF,lun,"Input to GET_STREAKS is not sorted!"
     RETURN
  ENDIF

  diff                              = input - shift(input,1)

  start_i                           = WHERE(diff NE 1,nStart)
  WHERECHECK,start_i  ;;Make sure this is OK

  CASE nStart OF
     0: BEGIN
        RETURN
     END
     1: BEGIN
        stop_i = N_ELEMENTS(input)-1
        single_i = -1
     END
     ELSE: BEGIN
        stop_i                            = [start_i[1:-1]-1,N_ELEMENTS(input)-1]
        single_ii                         = WHERE(start_i EQ stop_i,nSingle, $
                                                  COMPLEMENT=keep_ii,NCOMPLEMENT=nKeep)
        ;; WHERECHECK,single_ii,keep_ii

        
        IF nKeep GT 0 THEN BEGIN
           start_i                        = start_i[keep_ii]
           stop_i                         = stop_i[keep_ii]
        ENDIF

        IF nSingle GT 0 THEN BEGIN
           single_i                       = start_i[single_ii]
        ENDIF ELSE BEGIN
           single_i                       = -1
        ENDELSE

     END
  ENDCASE

  IF KEYWORD_SET(min_streak) THEN BEGIN
     PRINTF,lun,'Minimum streak for keeping: ' + STRCOMPRESS(min_streak,/REMOVE_ALL)
     minStreak_ii                   = WHERE((stop_i-start_i) GE min_streak,n_streaks, $
                                            COMPLEMENT=lowStreak_ii,/NULL,NCOMPLEMENT=nLow)
     IF N_ELEMENTS(minStreak_ii) GT 0 THEN BEGIN
        
        IF nLow GT 0 THEN BEGIN
           PRINT,'Losing ' + STRCOMPRESS(nLow,/REMOVE_ALL) + ' due to min streak requirement...'
           lowStreak_start_i        = start_i[lowStreak_ii]
           lowStreak_stop_i         = stop_i[lowStreak_ii]
        ENDIF

        start_i                     = start_i[minStreak_ii]
        stop_i                      = stop_i[minStreak_ii]
     ENDIF
  ENDIF ELSE BEGIN
     n_streaks = N_ELEMENTS(start_i)
  ENDELSE

  streakLens                        = stop_i-start_i

  n_streaks                         = N_ELEMENTS(WHERE(streakLens GT 0,/NULL))
  IF ~no_print_summary THEN BEGIN
     nSingle_i                      = single_i[0] EQ -1 ? 0 : N_ELEMENTS(single_i)
     PRINTF,lun,""
     PRINTF,lun,FORMAT='("**GET_STREAKS**")'
     PRINTF,lun,FORMAT='("N Streaks                     :",T35,I0)',n_streaks
     PRINTF,lun,FORMAT='("N single values               :",T35,I0)',nSingle_i
  ENDIF


END