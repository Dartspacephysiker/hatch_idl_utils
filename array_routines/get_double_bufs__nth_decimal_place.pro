;;09/30/16
;;This routine is comparable to FA_FIELDS_BUFS in that it treats each 
;;  uniq value of diffTot as a "sample rate," within the resolution
;;  specified by delta.
;;It will NOT give you streaks of indices for a given resolution. 
;;   For that trick, please see GET_DOUBLE_STREAKS.
;;
PRO GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,nums,decimal_place, $
                                       N=nMin, $
                                       DELTA=delta, $
                                       START_I=start_i, $
                                       STOP_I=stop_i, $
                                       STREAKLENS=streakLens, $
                                       OUT_RATES=rates, $
                                       FLOOR=floor, $
                                       CEILING=ceiling, $
                                       BADSTART_I=badStart_i, $
                                       BADSTOP_I=badStop_i, $
                                       BADSTREAKLENS=badStreakLens, $
                                       OUT_BADRATES=badRates
                                          ;; NAN=nan

  COMPILE_OPT IDL2

  IF SIZE(nums,/TYPE) NE 5 THEN BEGIN
     PRINT,'Nums not of type double! Out ...'
     RETURN
  ENDIF

  IF N_ELEMENTS(decimal_place) EQ 0 THEN BEGIN
     PRINT,"Decimal place must be specified to search for streaks. Out!"
     RETURN
  ENDIF

  IF KEYWORD_SET(NaN) THEN BEGIN
     ;; good = WHERE(FINITE(nums),nGood)
     ;; IF nGood EQ 0 THEN BEGIN
     ;;    MESSAGE,"No good data--they're all NaNs! Out ..."
     ;; ENDIF
     PRINT,"NaN stuff not currently supported ..."
  ENDIF ELSE BEGIN
     good = LINDGEN(N_ELEMENTS(nums))
  ENDELSE

  IF N_ELEMENTS(nMin) EQ 0 THEN nMin = 1

  IF nMin EQ 0 THEN BEGIN
     PRINT,"N must be greater than zero!"
     RETURN
  ENDIF

  IF N_ELEMENTS(nums[good]) LE nMin THEN BEGIN
     PRINT,'Insufficient data to search for streak length = ' + $
           STRCOMPRESS(n,/REMOVE_ALL) + '! Out ...'
     RETURN
  ENDIF

  IF ~ARRAY_EQUAL(SORT(nums[good]),INDGEN(N_ELEMENTS(good))) THEN BEGIN
     PRINT,"Array is unsorted! Exiting ..."
     RETURN
  ENDIF

  rounded = ROUND_TO_NTH_DECIMAL_PLACE(nums[good],decimal_place, $
                                       FLOOR=floor, $
                                       CEILING=ceiling, $
                                       /DOUBLE, $
                                       /LEAVE_AS_INTEGER)


  diffTot     = rounded - SHIFT(rounded,1)

  uniq_ii     = UNIQ(diffTot,SORT(diffTot))
  
  uniq_iii    = WHERE(diffTot[uniq_ii] GT 0)
  uniq_ii     = uniq_ii[uniq_iii]

  ;;For later
  IF ARG_PRESENT(rates) THEN BEGIN
     rateTot     = ROUND_TO_NTH_DECIMAL_PLACE(nums[good],decimal_place, $
                                              FLOOR=floor, $
                                              CEILING=ceiling, $
                                              /DOUBLE)
     rateTot     = rateTot - SHIFT(rateTot,1)
  ENDIF

  IF N_ELEMENTS(delta) GT 0 THEN BEGIN
     deltMod   = FIX(ROUND(delta*(10.D)^(-1.D*decimal_place)),TYPE=14)

     ;;Check for double-sided winners
     IF N_ELEMENTS(uniq_ii) GT 3 THEN BEGIN

        rm_iii = !NULL
        solid      = !NULL
        nRemove    = 0
        FOR k=N_ELEMENTS(uniq_ii)-2,1,-1 DO BEGIN ;;work backwards


           checkMe = ((diffTot[uniq_ii[k]] - diffTot[uniq_ii[k-1]]) LE deltMod) AND $
                     ((diffTot[uniq_ii[k+1]] - diffTot[uniq_ii[k]]) LE deltMod)

           IF N_ELEMENTS(rm_iii) GT 0 THEN BEGIN
              checkMe = checkMe AND ( (WHERE(rm_iii EQ k))[0] EQ -1)
           ENDIF

           IF checkMe THEN BEGIN

              solid = [solid,k]

              IF nRemove GT 0 THEN BEGIN
                 IF (WHERE(solid EQ (k-1)))[0] EQ -1 THEN BEGIN
                    rm_iii = [rm_iii,k-1]
                    nRemove++
                 ENDIF
                 
                 IF (WHERE(solid EQ (k+1)))[0] EQ -1 THEN BEGIN
                    rm_iii = [rm_iii,k+1]
                    nRemove++
                 ENDIF

              ENDIF ELSE BEGIN
                 rm_iii = [rm_iii,k-1,k+1]
                 nRemove    = 2
              ENDELSE

           ENDIF

        ENDFOR

        IF N_ELEMENTS(rm_iii) NE 0 THEN BEGIN
           rm_iii = rm_iii[UNIQ(rm_iii,SORT(rm_iii))]
           REMOVE,rm_iii,uniq_ii
        ENDIF

     ENDIF

     uniqMod_iii = !NULL
     FOR k=N_ELEMENTS(uniq_ii)-1,0,-1 DO BEGIN
        check = WHERE(ABS(diffTot[uniq_ii[k]]-diffTot[uniq_ii]) LE deltMod,nCheck)

        IF nCheck GT 0 THEN BEGIN
           uniqMod_iii = [uniqMod_iii,check[-1]] ;just keep largest unique elem
        ENDIF
     ENDFOR

     uniq_ii = uniq_ii[uniqmod_iii[UNIQ(uniqmod_iii,SORT(uniqmod_iii))]]
  ENDIF

  nUniq       = N_ELEMENTS(uniq_ii)

  ;; uniq_ii = DOUBLE_UNIQ(nums,decimal_place, $
                        
  ;;                       N=n, $
  ;;                       DELTA=delta, $
  ;;                       FLOOR=floor, $
  ;;                       CEILING=ceiling, $
  ;;                       QUIET=quiet, $
  ;;                       OUT_DELTMOD=deltMod, $
  ;;                       OUT_DIFFTOT=diffTot, $
  ;;                       NUNIQ=nUniq)

  start_i     = !NULL
  stop_i      = !NULL
  start_ii    = !NULL
  stop_ii     = !NULL
  streakLens  = !NULL
  nTotStreaks = 0
  rates       = !NULL

  badStart_i     = !NULL
  badStop_i      = !NULL
  badStart_ii    = !NULL
  badStop_ii     = !NULL
  badStreakLens  = !NULL
  nTotBadStreaks = 0
  badRates       = !NULL
  FOR k=0,nUniq-1 DO BEGIN
     
     streaksHere = 0

     CASE 1 OF
        KEYWORD_SET(delta): BEGIN
           tmp_ii = WHERE(ABS(diffTot-diffTot[uniq_ii[k]]) LE deltMod,nTmp)
        END
        ELSE: BEGIN
           tmp_ii = WHERE(diffTot EQ diffTot[uniq_ii[k]],nTmp)
        END
     ENDCASE

     ;; IF nTmp LT nMin THEN CONTINUE

     diff     = tmp_ii - SHIFT(tmp_ii,1)
     
     start_iii  = WHERE(diff NE 1,nStart)

     CASE nStart OF
        1: BEGIN
           stop_iii   = nTmp-1
           single_iii = -1

           streaksHere = 1
        END
        ELSE: BEGIN
           stop_iii   = [start_iii[1:-1]-1,nTmp-1]
           single_iv  = WHERE(start_iii EQ stop_iii,nSingle, $
                              COMPLEMENT=keep_iv,NCOMPLEMENT=nKeep)

           IF nKeep GT 0 THEN BEGIN
              start_iii    = start_iii[keep_iv]
              stop_iii     = stop_iii[keep_iv]

              streaksHere  = 1
           ENDIF
        END
     ENDCASE

     IF ~streaksHere THEN CONTINUE

     tmpStrk  = stop_iii-start_iii

     goodStreak_iv = WHERE(tmpStrk GE nMin,nGoodStreaks, $
                           COMPLEMENT=badStreak_iv,NCOMPLEMENT=nBadStreaks)

     IF nBadStreaks GT 0 THEN BEGIN
        badStreakLens   = [badStreakLens,tmpStrk[badStreak_iv]]
        badStart_ii     = [badStart_ii,tmp_ii[start_iii[badStreak_iv]]]
        badStop_ii      = [badStop_ii,tmp_ii[stop_iii[badStreak_iv]]]
        nTotBadStreaks += nBadStreaks

        IF ARG_PRESENT(badRates) THEN BEGIN
           badRates     = [badRates,rateTot[tmp_ii[start_iii[badStreak_iv]]]]
        ENDIF

     ENDIF

     IF nGoodStreaks EQ 0 THEN CONTINUE

     nTotStreaks += nGoodStreaks

     streakLens  = [streakLens,tmpStrk[goodStreak_iv]]
     start_ii    = [start_ii,tmp_ii[start_iii[goodStreak_iv]]]
     stop_ii     = [stop_ii,tmp_ii[stop_iii[goodStreak_iv]]]

     IF ARG_PRESENT(rates) THEN BEGIN
        rates    = [rates,rateTot[tmp_ii[start_iii[goodStreak_iv]]]]
     ENDIF

  ENDFOR

  ;;Take care of bad streaks, if they exist
  IF nTotBadStreaks GT 0 THEN BEGIN
     badStart_i     = good[badStart_ii]
     badStop_i      = good[badStop_ii]

     sort           = SORT(badStart_i)
     badStreakLens  = badStreakLens[sort]
     badStart_i     = badStart_i[sort]
     badStop_i      = badStop_i[sort]

     IF ARG_PRESENT(badRates) THEN BEGIN
        badRates    = badRates[sort]
     ENDIF

  ENDIF

  IF nTotStreaks EQ 0 THEN RETURN

  start_i     = good[start_ii]
  stop_i      = good[stop_ii]

  sort        = SORT(start_i)
  streakLens  = streakLens[sort]
  start_i     = start_i[sort]
  stop_i      = stop_i[sort]
  IF ARG_PRESENT(rates) THEN BEGIN
     rates    = rates[sort]
  ENDIF
END
