;;01/25/17
PRO ESA__LOOK_FOR_GAPS_AND_REMOVE__SINGLE_ORBIT,times, $
   ORBIT=curOrb, $
   ILATS=ilat, $
   KEEP_I=keep_i, $
   JUNK_I=junk_i, $
   BEFSTART_I=befStart_i, $
   JUNKTIMES=junkTimes, $
   BEFSTARTTIMES=befStartTimes, $
   MISSINGINTERVALS=missingIntervals

  COMPILE_OPT IDL2,STRICTARRSUBS

  nInds  = N_ELEMENTS(times)
  tmp_i  = LINDGEN(nInds)
  keep_i = tmp_i
  IF nInds EQ 0 THEN BEGIN
     PRINT,"no inds provided! Returning ..."
     RETURN
  ENDIF
  
  IF N_ELEMENTS(curOrb) EQ 0 OR N_ELEMENTS(ilat) EQ 0 THEN BEGIN
     GET_FA_ORBIT,times,/TIME_ARRAY

     GET_DATA,'ORBIT',DATA=orbit
     curOrb = (TEMPORARY(orbit)).y[0]

     GET_DATA,'ILAT',DATA=ilat
     ilat      = ilat.y
  ENDIF
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;First we worry about junking the first ten seconds in each hemisphere
  ;; times      = NEWELL__eSpec.x[tmp_i]

  missingIntervals  = !NULL
  befStart_ii       = !NULL
  junk_ii           = !NULL
  this              = !NULL
  this              = LOAD_JE_AND_JE_TIMES_FOR_ORB(curOrb,TIME_RANGES_OUT=tRanges,/USE_DUPELESS_FILES)
  ;;Make sure we junk the first ten seconds, if we haven't already, when entering auroral zone in either hemisphere
  nLoops            = this NE -1
  IF nLoops GT 0 THEN BEGIN

     intStartTs     = tRanges[*,0]
     CHECK_SORTED,intStartTs,sorted_intervals,/QUIET
     IF ~sorted_intervals THEN STOP
     
     extreme_ii   = !NULL        
     interval_ii  = VALUE_CLOSEST2(times,intStartTs, $
                                   EXTREME_I=extreme_ii, $
                                   ONLY_HAVE_EXTREME=onlyExtreme)

     IF N_ELEMENTS(extreme_ii) GT 0 THEN BEGIN
        lt0_iii = WHERE(extreme_ii EQ -1,nLT0)
        gtN_iii = WHERE(extreme_ii EQ nInds,nGTN)
        
        IF nLT0 GT 0 THEN BEGIN
           interval_ii[lt0_iii] = 0
        ENDIF

        IF nGTN GT 0 THEN BEGIN
           interval_ii[gtN_iii] = nInds-1
        ENDIF

     ENDIF
     
     goodIntMatch = WHERE(ABS(times[interval_ii]-intStartTs) LT 5, $
                          nGoodIntMatch, $
                          COMPLEMENT=badIntMatch, $
                          NCOMPLEMENT=nBadIntMatch)

     IF nGoodIntMatch NE N_ELEMENTS(intStartTs) THEN BEGIN

        ;;Let's see if we just don't have times for this interval
        intStopTs = tRanges[*,1] 
        intStop_ii = VALUE_CLOSEST2(times,intStopTs) 
        goodStopMatch = WHERE(ABS(times[intStop_ii]-intStopTs) LT 5, $
                              nGoodStopMatch, $
                              COMPLEMENT=badStopMatch, $
                              NCOMPLEMENT=nBadStopMatch)

        missingInterval = WHERE(badIntMatch EQ badStopMatch,nMissing)
        IF nMissing GT 0 THEN BEGIN

           IF (nMissing + nGoodIntMatch) EQ N_ELEMENTS(intStartTs) THEN BEGIN
              ;; PRINT,FORMAT='("K, so you''re missing these intervals: ",10(I0,:,", "))',badIntMatch[missingInterval]
              FOR m=0,nMissing-1 DO BEGIN
                 missingIntervals = [[missingIntervals],[curOrb,badIntMatch[missingInterval[m]]]]
              ENDFOR

           ENDIF ELSE BEGIN

              PRINT,"Un lío! The number of times that NEWELL__eSpec matches the ESA interval start times is " + $
                    "fewer than the number of ESA interval start times for this orbit!" 
              STOP

           ENDELSE

        ENDIF

     ENDIF

     north_iii   = WHERE(ilat[tmp_i[interval_ii]] GT 0.0,nNorth3)
     south_iii   = WHERE(ilat[tmp_i[interval_ii]] LT 0.0,nSouth3)

     IF nNorth3 GT 0 THEN BEGIN
        tmpBefStart_ii  = WHERE(ABS(times - (intStartTs[north_iii[0]])) LE 10,nBefStart)
        IF nBefStart GT 0 THEN BEGIN
           ;; befStart_i   = [befStart_i,tmp_i[tmpBefStart_ii]]
           befStart_ii  = [befStart_ii,tmpBefStart_ii]
        ENDIF
     ENDIF

     IF nSouth3 GT 0 THEN BEGIN
        tmpBefStart_ii  = WHERE(ABS(times - (intStartTs[south_iii[0]])) LE 10,nBefStart)
        IF nBefStart GT 0 THEN BEGIN
           ;; befStart_i   = [befStart_i,tmp_i[tmpBefStart_ii]]
           befStart_ii  = [befStart_ii,tmpBefStart_ii]
        ENDIF
     ENDIF

  ENDIF ELSE BEGIN
     ;; PRINT,"No junkstart inds here!"
  ENDELSE

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now we worry about transitions between sampling rates
  delta   = 0.08
  dPlace  = -2
  nMin    = 4
  GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,times-times[0],-2, $
                                     N=nMin, $
                                     DELTA=delta, $
                                     START_I=start_ii, $
                                     STOP_I=stop_ii, $
                                     STREAKLENS=streakLens, $
                                     OUT_RATES=rates, $
                                     FLOOR=floor, $
                                     CEILING=ceiling, $
                                     BADSTART_I=badStart_ii, $
                                     BADSTOP_I=badStop_ii, $
                                     BADSTREAKLENS=badStreakLens, $
                                     OUT_BADRATES=badRates


  ;;Junk any streaks smaller than nMin
  nBadStreaks   = N_ELEMENTS(badStart_ii)
  FOR k=0,nBadStreaks-1 DO BEGIN
     ;; IF badStart_ii[k] EQ badStop_ii[k] THEN BEGIN
     ;;    tmpJunk_ii = badStart_ii
     tmpJunk_ii = [(badStart_ii[k]):badStop_ii[k]]
     junk_ii    = [junk_ii,tmpJunk_ii]
  ENDFOR

  sampDT    = !NULL
  nStreaks  = N_ELEMENTS(start_ii)
  IF nStreaks GT 0 THEN BEGIN

     FOR k=0,nStreaks-1 DO BEGIN
        ;; PRINT,times[(start_ii[k]+1):stop_ii[k]]-times[(start_ii[k]):(stop_ii[k]-1)]
        sampDT = [sampDT,MEAN(times[(start_ii[k]+1):stop_ii[k]]-times[(start_ii[k]):(stop_ii[k]-1)])]
     ENDFOR

     IF (WHERE(sampDT LE 0))[0] NE -1 THEN BEGIN
        PRINT,"A negative sample dt, huh? Tell me about it."
        STOP
     END

     samp251    = ABS(sampDT-2.51) LT 0.2
     samp0628   = ABS(sampDT-0.624) LT 0.1
     samp0312   = ABS(sampDT-0.312) LT 0.1
     w251       = WHERE(samp251,n251)
     w0628      = WHERE(samp0628,n0628)
     w0312      = WHERE(samp0312,n0312)
     safe       = WHERE(TEMPORARY(samp251) OR TEMPORARY(samp0628) OR TEMPORARY(samp0312), $
                        nSafeStreaks,COMPLEMENT=wUnsafe,NCOMPLEMENT=nUnsafe)

     IF nSafeStreaks NE nStreaks THEN BEGIN
        PRINT,FORMAT='(A0,": ","Unsafe sampDTs!! (",' + STRCOMPRESS(nUnsafe) + '(F0.2,:,", "),A0)',orbSTring,sampDT[wUnsafe],")"

        ;;Junk all the places where we transition from 2.51-s sample period to something else
        RM_INTERVAL,times,wUnsafe,wUnsafe_ii,nUnsafe,start_ii,stop_ii,sampDT,junk_ii
     ENDIF

     ;;Junk all the places where we transition from 2.51-s sample period to something else
     RM_INTERVAL,times,w251,w251_ii,n251,start_ii,stop_ii,sampDT,junk_ii

     ;;Junk all the places where we transition from 0.628-s sample period to something else
     RM_INTERVAL,times,w0628,w0628_ii,n0628,start_ii,stop_ii,sampDT,junk_ii

     ;;Junk all the places where we transition from 0.628-s sample period to something else
     RM_INTERVAL,times,w0312,w0312_ii,n0312,start_ii,stop_ii,sampDT,junk_ii

  ENDIF
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now store 'em up!  
  IF N_ELEMENTS(befStart_ii) GT 0 THEN BEGIN
     ;; befStart_i    = [befStart_i,tmp_i[befStart_ii]]
     ;; befStartTimes = [befStartTimes,times[befStart_ii]]

     befStart_i    = tmp_i[befStart_ii]
     befStartTimes = times[befStart_ii]

     keep_i        = CGSETDIFFERENCE(keep_i,befStart_i,COUNT=nKeep)
     IF ~KEYWORD_SET(quiet) THEN PRINT,"N lost to befStart: ",nInds-nKeep
  ENDIF

  IF N_ELEMENTS(junk_ii) GT 0 THEN BEGIN

     CHECK_SORTED,junk_ii,junk_sorted,/QUIET

     ;;If they're not sorted, they may not be unique either. We simply must know
     IF ~junk_sorted THEN BEGIN

        nUniqJunk = N_ELEMENTS(UNIQ(junk_ii,SORT(junk_ii)))
        nJunk     = N_ELEMENTS(junk_ii)
        IF nUniqJunk NE nJunk THEN BEGIN
           ;; PRINT,"Say what??? Ridding us of " + STRCOMPRESS(nJunk-nUniqJunk) + " duplicate junkers"
           nDupes = nJunk-nUniqJunk
           junk_ii = junk_ii[UNIQ(junk_ii,SORT(junk_ii))]
        ENDIF ELSE BEGIN
           junk_ii = junk_ii[SORT(junk_ii)]
        ENDELSE

     ENDIF
     
     ;; junk_i        = [junk_i,tmp_i[junk_ii]]
     ;; junkTimes     = [junkTimes,times[junk_ii]]

     junk_i        = tmp_i[junk_ii]
     junkTimes     = times[junk_ii]

     keep_i        = CGSETDIFFERENCE(keep_i,junk_i,COUNT=nKeep)
     IF ~KEYWORD_SET(quiet) THEN PRINT,"N lost to junker: ",nInds-nKeep

  ENDIF

END
