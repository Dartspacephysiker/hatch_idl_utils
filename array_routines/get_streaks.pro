;2015/10/14
;This routine returns, for integer input, all streak beginning and ending indices.
;For example, if input=[0,1,2,4,5,8], start_i[0]=0, stop_i[0]=2, start_i[1]=3, stop_i[1]=4, and single_i=5
;2016/02/19 Added OUT_STREAKLENS keyword
PRO GET_STREAKS,input, $
                START_I=start_i, $
                STOP_I=stop_i, $
                SINGLE_I=single_i, $
                ALLOWABLE_GAP=allowable_gap, $
                MIN_STREAK_TO_KEEP=min_streak, $
                LOWSTREAK_START_I=lowStreak_start_i, $
                LOWSTREAK_STOP_I=lowStreak_stop_i, $
                OUT_STREAKLENS=streakLens, $
                OUT_GAPLENS=gapLens, $
                LUN=lun, $
                N_STREAKS=n_streaks, $
                NO_PRINT_SUMMARY=no_print_summary, $
                QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

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

  ;; IF KEYWORD_SET(allowable_gap) THEN BEGIN
  ;;    start_i                        = WHERE(ABS(diff) GT allowable_gap,nStart)
  ;; ENDIF ELSE BEGIN
     start_i                        = WHERE(diff NE 1,nStart)
  ;; ENDELSE
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

        
        IF nSingle GT 0 THEN BEGIN
           single_i                       = start_i[single_ii]
        ENDIF ELSE BEGIN
           single_i                       = -1
        ENDELSE

        IF nKeep GT 0 THEN BEGIN
           start_i                        = start_i[keep_ii]
           stop_i                         = stop_i[keep_ii]
        ENDIF

     END
  ENDCASE

  IF (nKeep GT 0) AND KEYWORD_SET(allowable_gap) THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINTF,lun,'Allowable gap between start and stop: ' + STRCOMPRESS(allowable_gap,/REMOVE_ALL)
     gapLens = start_i[1:-1]-stop_i[0:-2]

     stitchTheseGaps_ii = WHERE(gapLens LE allowable_gap,nGapsToStitch)
     IF nGapsToStitch GT 0 THEN BEGIN

        streakInd = 1
        nStreaks  = N_ELEMENTS(start_i)

        newStart_i = start_i[0]
        newStop_i  = stop_i[0]
        newNStreaks = 1
        WHILE streakInd LT nStreaks DO BEGIN
           
           ;; tmpStop_i     = stop_i[streakInd-1]
           ;;Check the start_i ahead of where we are, see if it is within allowable_gap of the last stop_i
           keepStreaking = (start_i[streakInd] - stop_i[streakInd-1]) LE allowable_gap
           IF keepStreaking THEN BEGIN
              ;;If the start_i ahead of current position is within allowable gap, just adjust newStop_i
              newStop_i[-1] = stop_i[streakInd]
           ENDIF ELSE BEGIN
              ;;In this case we need to add a new start_i and a new stop_i since the gap was too large
              newStart_i    = [newStart_i,start_i[streakInd]]
              newStop_i     = [newStop_i,stop_i[streakInd]]
              ;; tmpStart_i    = start_i[streakInd]
              newNStreaks++
           ENDELSE

           streakInd++
              
        ENDWHILE

        STOP

        start_i = TEMPORARY(newStart_i)
        stop_i  = TEMPORARY(newStop_i)

     ENDIF ELSE BEGIN

     ENDELSE

  ENDIF

  IF KEYWORD_SET(min_streak) THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINTF,lun,'Minimum streak for keeping: ' + STRCOMPRESS(min_streak,/REMOVE_ALL)
     minStreak_ii                   = WHERE((stop_i-start_i) GE min_streak,n_streaks, $
                                            COMPLEMENT=lowStreak_ii,/NULL,NCOMPLEMENT=nLow)
     IF N_ELEMENTS(minStreak_ii) GT 0 THEN BEGIN
        
        IF nLow GT 0 THEN BEGIN
           IF ~KEYWORD_SET(quiet) THEN PRINT,'Losing ' + STRCOMPRESS(nLow,/REMOVE_ALL) + ' due to min streak requirement...'
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
  IF ~(no_print_summary OR KEYWORD_SET(quiet)) THEN BEGIN
     nSingle_i                      = single_i[0] EQ -1 ? 0 : N_ELEMENTS(single_i)
     PRINTF,lun,""
     PRINTF,lun,FORMAT='("**GET_STREAKS**")'
     PRINTF,lun,FORMAT='("N Streaks                     :",T35,I0)',n_streaks
     PRINTF,lun,FORMAT='("N single values               :",T35,I0)',nSingle_i
  ENDIF


END
