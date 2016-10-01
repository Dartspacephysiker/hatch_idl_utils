;;09/30/16
;;This routine will give you streaks of indices for a given resolution. 
;;   For a FA_FIELDS_BUFS-style treatment of double-type time series, please see GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE.
;;
PRO GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE,nums,decimal_place, $
                                          NPTS=n, $
                                          MIN_T_STREAKLEN=min_streakLen_t, $
                                          GAP_TIME=gap_time, $
                                          START_I=start_i, $
                                          STOP_I=stop_i, $
                                          STREAKLENS=streakLens, $
                                          T_STREAKLENS=streakLens_t, $
                                          FLOOR=floor, $
                                          CEILING=ceiling, $
                                          PRINT_START_STOP_TIMES=print_start_stop_times

  COMPILE_OPT IDL2

  IF SIZE(nums,/TYPE) NE 5 THEN BEGIN
     PRINT,'Nums not of type double! Out ...'
     RETURN
  ENDIF

  IF N_ELEMENTS(decimal_place) EQ 0 THEN BEGIN
     PRINT,"Decimal place must be specified to search for streaks. Out!"
     RETURN
  ENDIF

  IF N_ELEMENTS(n) EQ 0 THEN n = 1

  IF n EQ 0 THEN BEGIN
     PRINT,"N must be greater than zero!"
     RETURN
  ENDIF

  IF N_ELEMENTS(nums) LE n THEN BEGIN
     PRINT,'Insufficient data to search for streak length = ' + $
           STRCOMPRESS(n,/REMOVE_ALL) + '! Out ...'
     RETURN
  ENDIF

  IF ~ARRAY_EQUAL(SORT(nums),INDGEN(N_ELEMENTS(nums))) THEN BEGIN
     PRINT,"Array is unsorted! Exiting ..."
     RETURN
  ENDIF

  rounded = ROUND_TO_NTH_DECIMAL_PLACE(nums,decimal_place, $
                                       FLOOR=floor, $
                                       CEILING=ceiling, $
                                       /DOUBLE, $
                                       /LEAVE_AS_INTEGER)


  diffTot     = rounded - SHIFT(rounded,1)

  ;; uniq_i     = UNIQ(diffTot,SORT(diffTot))
  
  ;; ;;Get rid of negative diffs
  ;; uniq_ii    = WHERE(diffTot[uniq_i] GT 0)
  ;; uniq_i     = uniq_i[uniq_ii]

  ;;Anything less than gap_time gets subsumed
  IF KEYWORD_SET(gap_time) THEN BEGIN
     gapMod   = FIX(ROUND(gap_time*(10.D)^(-1.D*decimal_place)),TYPE=14)

     ;; uniq_iii = WHERE(diffTot[uniq_i] GE gapMod,nUniqiii)

     ;; IF nUniqiii EQ 0 THEN BEGIN
     ;;    PRINT,"Gap is too big! I'm just going to return the whole time series to you, you know."
     ;;    RETURN
     ;; ENDIF

     ;; uniq_i = uniq_i[uniq_ii]

     ;; ;;Delta also need not be 
     ;; delta    = gap_time/2.
  ENDIF ELSE BEGIN
     gapMod   = 1
  ENDELSE

  ;; IF N_ELEMENTS(delta) GT 0 THEN BEGIN
  ;;    deltMod   = FIX(ROUND(delta*(10.D)^(-1.D*decimal_place)),TYPE=14)

  ;;    ;;Check for double-sided winners
  ;;    IF N_ELEMENTS(uniq_i) GT 3 THEN BEGIN

  ;;       rm_iii = !NULL
  ;;       solid      = !NULL
  ;;       nRemove    = 0
  ;;       FOR k=N_ELEMENTS(uniq_i)-2,1,-1 DO BEGIN ;;work backwards


  ;;          checkMe = ((diffTot[uniq_i[k]] - diffTot[uniq_i[k-1]]) LE deltMod) AND $
  ;;                    ((diffTot[uniq_i[k+1]] - diffTot[uniq_i[k]]) LE deltMod)

  ;;          IF N_ELEMENTS(rm_iii) GT 0 THEN BEGIN
  ;;             checkMe = checkMe AND ( (WHERE(rm_iii EQ k))[0] EQ -1)
  ;;          ENDIF

  ;;          IF checkMe THEN BEGIN

  ;;             solid = [solid,k]

  ;;             IF nRemove GT 0 THEN BEGIN
  ;;                IF (WHERE(solid EQ (k-1)))[0] EQ -1 THEN BEGIN
  ;;                   rm_iii = [rm_iii,k-1]
  ;;                   nRemove++
  ;;                ENDIF
  
  ;;                IF (WHERE(solid EQ (k+1)))[0] EQ -1 THEN BEGIN
  ;;                   rm_iii = [rm_iii,k+1]
  ;;                   nRemove++
  ;;                ENDIF

  ;;             ENDIF ELSE BEGIN
  ;;                rm_iii = [rm_iii,k-1,k+1]
  ;;                nRemove    = 2
  ;;             ENDELSE

  ;;          ENDIF

  ;;       ENDFOR

  ;;       IF N_ELEMENTS(rm_iii) NE 0 THEN BEGIN
  ;;          rm_iii = rm_iii[UNIQ(rm_iii,SORT(rm_iii))]
  ;;          REMOVE,rm_iii,uniq_i
  ;;       ENDIF

  ;;    ENDIF

  ;;    uniqMod_iii = !NULL
  ;;    FOR k=N_ELEMENTS(uniq_i)-1,0,-1 DO BEGIN
  ;;       check = WHERE(ABS(diffTot[uniq_i[k]]-diffTot[uniq_i]) LE deltMod,nCheck)

  ;;       IF nCheck GT 0 THEN BEGIN
  ;;          uniqMod_iii = [uniqMod_iii,check[-1]] ;just keep largest unique elem
  ;;       ENDIF
  ;;    ENDFOR

  ;;    uniq_i = uniq_i[uniqmod_iii[UNIQ(uniqmod_iii,SORT(uniqmod_iii))]]
  ;; ENDIF

  ;; nUniq       = N_ELEMENTS(uniq_i)

  ;; start_i    = !NULL
  ;; stop_i     = !NULL
  ;; streakLens  = !NULL
  ;; nTotStreaks = 0
  ;; FOR k=0,nUniq-1 DO BEGIN
  
  streaksHere = 0

  ;; CASE 1 OF
  ;;    KEYWORD_SET(gap_time): BEGIN
  ;;       tmp_ii = WHERE(ABS(diffTot-diffTot[uniq_i[k]]) LE deltMod,nTmp)
  ;;    END
  ;;    ELSE: BEGIN
  ;;       tmp_ii = WHERE(diffTot EQ diffTot[uniq_i[k]],nTmp)
  ;;    END
  ;; ENDCASE

  ;; IF nTmp LT n THEN CONTINUE

  ;; diff     = tmp_ii - SHIFT(tmp_ii,1)
  
  start_i  = WHERE(diffTot GT gapMod,nStart) ;;  start_i  = WHERE(diffTot LE gapMod AND (diffTot GE 0),nStart)

  IF diffTot[0] LT 0 THEN start_i = [0,start_i]

  CASE nStart OF
     1: BEGIN
        stop_i     = N_ELEMENTS(diffTot)-1
        single_ii = -1

        streaksHere = 1
     END
     ELSE: BEGIN
        stop_i    = [start_i[1:-1]-1,N_ELEMENTS(diffTot)-1]
        single_ii = WHERE(start_i EQ stop_i,nSingle, $
                         COMPLEMENT=keep_ii,NCOMPLEMENT=nKeep)

        IF nKeep GT 0 THEN BEGIN
           start_i    = start_i[keep_ii]
           stop_i     = stop_i[keep_ii]

           streaksHere  = 1
        ENDIF
     END
  ENDCASE

  IF ~streaksHere THEN BEGIN
     start_i     = 0
     stop_i      = 0
     streakLens  = 0
     RETURN
  ENDIF

  tmpStrk        = stop_i-start_i

  goodStreak_ii  = WHERE(tmpStrk GE n,nGoodStreaks)

  IF nGoodStreaks EQ 0 THEN BEGIN
     doQuit      = 1
  ENDIF

  IF KEYWORD_SET(min_streakLen_t) AND ~KEYWORD_SET(doQuit) THEN BEGIN
     streakLens_t   = nums[stop_i] - nums[start_i]
     goodTStreak_ii = WHERE(streakLens_t GE min_streakLen_t,nGoodStreaks)

     IF nGoodStreaks EQ 0 THEN BEGIN
        doQuit   = 1
     ENDIF ELSE BEGIN

        goodStreak_ii = CGSETINTERSECTION(goodStreak_ii,goodTStreak_ii,COUNT=nGoodStreaks)

        IF nGoodStreaks EQ 0 THEN BEGIN
           doQuit   = 1
        ENDIF
     ENDELSE

  ENDIF

  IF KEYWORD_SET(doQuit) THEN BEGIN
     start_i     = 0
     stop_i      = 0
     streakLens  = 0
     RETURN
  ENDIF

  streakLens     = tmpStrk[goodStreak_ii]
  streakLens_t   = streakLens_t[goodStreak_ii]
  start_i        = start_i[goodStreak_ii]
  stop_i         = stop_i[goodStreak_ii]

  sort           = SORT(start_i)
  streakLens     = streakLens[sort]
  streakLens_t   = streakLens_t[sort]
  start_i        = start_i[sort]
  stop_i         = stop_i[sort]


  IF KEYWORD_SET(print_start_stop_times) THEN BEGIN
     FOR k=0,n_elements(start_i)-1 DO PRINT,TIME_TO_STR(nums[start_i[k]]),"   ", $
                                            TIME_TO_STR(nums[stop_i[k]]), $
                                            nums[stop_i[k]]-nums[start_i[k]]
  ENDIF
END
