;;09/29/16
;;Smoothing à la Strangeway et al. [2005] (Appendix A)
FUNCTION STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS,data, $
   INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
   INTERP_TO_COMMON_TS=interp_to_common_TS, $
   ONESEC_TS=tS_1s, $
   NO_SEPARATE_DC_AC=no_separate_DC_AC, $
   USE_DOUBLE_STREAKER=use_double_streaker, $
   EXIT_ON_PROBLEMS=exit_on_problems

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(interp_4Hz_to_1s) OR KEYWORD_SET(interp_to_common_TS) THEN BEGIN

     tSeriesName = (KEYWORD_SET(interp_4Hz_to_1s) ? '1-s' : '' ) +  ' time series'
     IF N_ELEMENTS(tS_1s) EQ 0 THEN BEGIN
        MESSAGE,'Must provide ' + tSeriesName + ' if you want me to interp for you'
        ;; RETURN,-1
     ENDIF

     IF SIZE(tS_1s,/TYPE) NE 5 THEN BEGIN
        MESSAGE,'Provided ' + tSeriesName + " isn't of type double! Returning ..."
        ;; RETURN,-1
     ENDIF

     junk1 = MIN(ABS(data.x[0]-tS_1s),strt_i)
     IF strt_i EQ (N_ELEMENTS(tmp_tS)-1) THEN BEGIN
        MESSAGE,tSeriesName + ' ends before desired decimated data begin! Returning ...'
        ;; RETURN,-1
     ENDIF

     junk2 = MIN(ABS(data.x[-1]-tS_1s),stop_i)
     IF stop_i EQ 0 THEN BEGIN
        MESSAGE,tSeriesName + ' ends before desired decimated data begin! Returning ...'
        ;; RETURN,-1
     ENDIF

     ;;Make sure gap is actually 1 second
     IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN
        IF ( (WHERE( ABS( tS_1s[1:-1] - tS_1s[0:-2] ) LT 0.99))[0] NE -1 ) AND $
           ( (WHERE( ABS( tS_1s[1:-1] - tS_1s[0:-2] ) GT 1.01))[0] NE -1 ) THEN BEGIN
           MESSAGE,'This supposedly 1-s resolved time series is bogus! Try again, liar.'
           ;; RETURN,-1
        ENDIF
     ENDIF
  ENDIF


  IF SIZE(data,/TYPE) NE 8 THEN BEGIN
     MESSAGE,"SMOOTH_TSERIES: Incorrect data type"
     ;; RETURN,-1
  ENDIF

  IF N_ELEMENTS(SIZE(data.y,/DIMENSIONS)) GT 1 THEN BEGIN
     MESSAGE,"Only set up to handle 1D Array! Returning ..."
     ;; RETURN,-1
  ENDIF

  ;;Never hurt anyone to be monotonic
  data.x = data.x[SORT(data.x)]
  data.y = data.y[SORT(data.x)]

  finite_y         = FINITE(data.y)

  IF (WHERE(finite_y))[0] EQ -1 THEN BEGIN
     MESSAGE,"No finite data here! No smoothing to be done ..."
     ;; RETURN,-1
  ENDIF

  IF KEYWORD_SET(use_double_streaker) THEN BEGIN
     GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,data.x,-2, $
                                        N=20, $
                                        DELTA=0.01, $
                                        START_I=strt_i, $
                                        STOP_I=stop_i, $
                                        STREAKLENS=streakLens, $
                                        FLOOR=floor, $
                                        CEILING=ceiling

     two = 2.D
     four = 4.D
     
  ENDIF ELSE BEGIN
     PRINT,"Strangeway-decimating and smoothing ..."

     FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i

     IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN BEGIN
        PRINT,"Can't Strangeway-decimate or Strangeway-smooth these data! They can't be chunked into buffers ..."
        IF KEYWORD_SET(exit_on_problems) THEN RETURN,{x: data.x, y:MAKE_ARRAY(N_ELEMENTS(data.x),/FLOAT,VALUE=!VALUES.F_NAN)}
     ENDIF
  ENDELSE


  sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
  sPeriods = data.x[strt_i+1]-data.x[strt_i]
  nBufs    = N_ELEMENTS(strt_i)

  tmpDat = data
  finalSampRates = !NULL
  CASE 1 OF
     KEYWORD_SET(use_double_streaker): BEGIN

        sampRateInQuestion = KEYWORD_SET(interp_4Hz_to_1s) ? 4 : 1

        FOR k=0,nBufs-1 DO BEGIN

           curSampRate    = sRates[k]
           tmpI           = [strt_i[k]:stop_i[k]]
           tmp            = {x:tmpDat.x[tmpI], $
                             y:tmpDat.y[tmpI]}
           nCurrent       = N_ELEMENTS(tmp.x)

           PRINT,k,', ',curSampRate,', ',nCurrent

           earlyBreak     = 0
           WHILE curSampRate GT sampRateInQuestion DO BEGIN
              
              
              nCurrent       = N_ELEMENTS(tmp.x)
              CASE 1 OF
                 (curSampRate LT 3): BEGIN

                    ;;Don't jump over every third data point here
                    ;; tmp      = {x:tmp.x[0:nCurrent-1:curSampRate], $
                    ;;             y:tmp.y[0:nCurrent-1:curSampRate]}

                    ;; curSampRate /= curSampRate
                    ;; shouldStop

                 END
                 ( (curSampRate GE 3) AND (curSampRate LT 6) ): BEGIN

                    ;;3-point smooth
                    tmp.y          = SMOOTH(tmp.y,3,/NAN,MISSING=!VALUES.F_NaN)

                    tmp   = {x:tmp.x[0:nCurrent-1:2], $
                             y:tmp.y[0:nCurrent-1:2]}

                    curSampRate /= 2.D

                 END
                 ( (curSampRate GE 6) AND (curSampRate LT 12) ): BEGIN

                    ;;5-point smooth
                    tmp.y          = SMOOTH(tmp.y,5,/NAN,MISSING=!VALUES.F_NaN)
              
                    ;;Jump over every other data point here
                    tmp   = {x:tmp.x[0:nCurrent-1:3], $
                             y:tmp.y[0:nCurrent-1:3]}

                    curSampRate /= 3.D

                 END
                 ELSE: BEGIN

                    ;;5-point smooth
                    tmp.y = SMOOTH(tmp.y,5,/NAN,MISSING=!VALUES.F_NaN)

                    tmp   = {x:tmp.x[0:nCurrent-1:3], $
                             y:tmp.y[0:nCurrent-1:3]}

                    curSampRate /= 3.D

                 END
              ENDCASE

              IF KEYWORD_SET(interp_4Hz_to_1s) AND ( curSampRate LE 4 ) THEN BREAK

              ;; IF KEYWORD_SET(earlyBreak) THEN BREAK

           ENDWHILE
           finalSampRates = [finalSampRates,curSampRate]

           IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN

              ;;It has NOT been smoothed and decimated to one second. 
              IF N_ELEMENTS(final) EQ 0 THEN BEGIN

                 final = TEMPORARY(tmp)

              ENDIF ELSE BEGIN

                 final = {x:[final.x,tmp.x], $
                          y:[final.y,tmp.y]}

              ENDELSE
           ENDIF ELSE BEGIN

              ;;OK, it's been smoothed and decimated to one second. 
              ;;Now let's get Alfvénic and DC components
              DCdat = SMOOTH(tmp.y,5,/NAN,MISSING=!VALUES.F_NaN)
              
              ACdat = tmp.y-DCdat

              IF N_ELEMENTS(final) EQ 0 THEN BEGIN

                 final = {x:tmp.x, $
                          DC:DCDat, $
                          AC:ACDat}

              ENDIF ELSE BEGIN

                 final = {x:[final.x,tmp.x], $
                          DC:[final.DC,DCDat], $
                          AC:[final.AC,ACDat]}

              ENDELSE
           ENDELSE

        ENDFOR

     END
     ELSE: BEGIN

        IF (WHERE( ABS(FIX(sRates) - sRates) GT 0.1))[0] NE -1 AND $
           ~KEYWORD_SET(use_double_streaker) THEN BEGIN
           MESSAGE,"The sampling frequencies aren't integer! Sorry ... (Oh, and is there any chance you're using spin-fitted variables? That'll booger you up in no time: their sample rate is 0.4, or T=2.5 s)"
           
           ;; RETURN,-1
        ENDIF

        sRates = FIX(sRates)

        PRINT,'Buf, CurSampRate, nPoints'
        FOR k=0,nBufs-1 DO BEGIN

           ;; curSampPeriod  = sPeriods[k]
           curSampRate    = sRates[k]
           tmpI           = [strt_i[k]:stop_i[k]]
           tmp            = {x:tmpDat.x[tmpI], $
                             y:tmpDat.y[tmpI]}

           ;; WHILE curSampPeriod GT 1 DO BEGIN
           earlyBreak     = 0
           WHILE curSampRate GT 1 DO BEGIN
              
              nCurrent       = N_ELEMENTS(tmp.x)

              PRINT,k,', ',curSampRate,', ',nCurrent


              CASE 1 OF
                 KEYWORD_SET(interp_4Hz_to_1s): BEGIN

                    CASE 1 OF
                       (curSampRate LT 4): BEGIN

                          ;; PRINT,"What should you actually do here? What you ARE doing is using the current sample rate as the stride"
                          ;; STOP
                          ;; tmp   = {x:tmp.x[0:nCurrent-1:curSampRate], $
                          ;;          y:tmp.y[0:nCurrent-1:curSampRate]}

                          ;; curSampRate /= curSampRate

                       END
                       ( curSampRate EQ 4 ): BEGIN

                          ;;Don't jump over every fourth data point here
                          ;; tmp   = {x:tmp.x[0:nCurrent-1], $
                          ;;          y:tmp.y[0:nCurrent-1]}

                          earlyBreak = 1

                       END
                       ( curSampRate EQ 8 ): BEGIN

                          ;;3-point smooth
                          tmp.y = SMOOTH(tmp.y,3,/NAN,MISSING=!VALUES.F_NaN)

                          ;;Jump over every other data point here
                          tmp   = {x:tmp.x[0:nCurrent-1:2], $
                                   y:tmp.y[0:nCurrent-1:2]}

                          curSampRate /= 2

                          ;; IF KEYWORD_SET(interp_4Hz_to_1s) THEN earlyBreak = 1

                       END
                       ( (curSampRate MOD 4) EQ 0): BEGIN

                          ;;7-point smooth
                          tmp.y = SMOOTH(tmp.y,7,/NAN,MISSING=!VALUES.F_NaN)

                          tmp   = {x:tmp.x[0:nCurrent-1:4], $
                                   y:tmp.y[0:nCurrent-1:4]}

                          curSampRate /= 4

                       END
                       ELSE: BEGIN
                       END
                    ENDCASE

                 END
                 ELSE: BEGIN
                    
                    CASE 1 OF
                       (curSampRate LE 1): BEGIN
                          earlyBreak = 1
                       END
                       (curSampRate LE 4): BEGIN

                          ;; PRINT,"What should you actually do here? What you ARE doing is using the current sample rate as the stride"
                          ;; STOP

                          ;;(2N-1)-point smooth
                          tmp.y = SMOOTH(tmp.y,FIX(curSampRate*2-1) < (nCurrent - 1),/NAN,MISSING=!VALUES.F_NaN)

                          tmp   = {x:tmp.x[0:nCurrent-1:curSampRate], $
                                   y:tmp.y[0:nCurrent-1:curSampRate]}

                          curSampRate /= curSampRate

                       END
                       ( (curSampRate MOD 4) EQ 0): BEGIN

                          ;;7-point smooth
                          tmp.y = SMOOTH(tmp.y,7 < (nCurrent - 1),/NAN,MISSING=!VALUES.F_NaN)

                          tmp   = {x:tmp.x[0:nCurrent-1:4], $
                                   y:tmp.y[0:nCurrent-1:4]}

                          curSampRate /= 4

                       END
                       ELSE: BEGIN
                          MESSAGE,'What???? Samprate is ' + STRCOMPRESS(curSampRate,/REMOVE_ALL)
                          ;; earlyBreak = 1
                          STOP
                       END
                    ENDCASE

                 END
              ENDCASE

              IF KEYWORD_SET(interp_4Hz_to_1s) AND ( curSampRate LE 4 ) THEN BREAK

              IF KEYWORD_SET(earlyBreak) THEN BREAK

           ENDWHILE
           finalSampRates = [finalSampRates,curSampRate]

           IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN

              IF N_ELEMENTS(final) EQ 0 THEN BEGIN

                 final = TEMPORARY(tmp)

              ENDIF ELSE BEGIN

                 final = {x:[final.x,tmp.x], $
                          y:[final.y,tmp.y]}

              ENDELSE

           ENDIF ELSE BEGIN

              nCurrent = N_ELEMENTS(tmp.x)
              ;;OK, it's been smoothed and decimated to one second. 
              ;;Now let's get Alfvénic and DC components
              DCdat = SMOOTH(tmp.y,7 < (nCurrent - 1),/NAN,MISSING=!VALUES.F_NaN)
              
              ACdat = tmp.y-DCdat

              IF N_ELEMENTS(final) EQ 0 THEN BEGIN

                 final = {x:tmp.x, $
                          DC:TEMPORARY(DCDat), $
                          AC:TEMPORARY(ACDat)}

              ENDIF ELSE BEGIN

                 final = {x:[final.x,tmp.x], $
                          DC:[final.DC,TEMPORARY(DCDat)], $
                          AC:[final.AC,TEMPORARY(ACDat)]}

              ENDELSE

           ENDELSE

        ENDFOR

     END
  ENDCASE

  ;;Interp data with 4 Hz resolution 
  IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN

     IF (WHERE(finalSampRates GT 4))[0] NE -1 THEN STOP
     
     CASE 1 OF
        KEYWORD_SET(use_double_streaker): BEGIN
           delt_t = 2.9
           finalSm = 5
        END
        ELSE: BEGIN
           delt_t = 0.9
           finalSm = 7
        END
     ENDCASE

     CASE 1 OF
        KEYWORD_SET(no_separate_DC_AC): BEGIN
           ;; FA_FIELDS_COMBINE,{time:tS_1s,comp1:tS_1s}, $
           ;;                   {time:final.x,comp1:final.y}, $
           ;;                   RESULT=finalDat, $
           ;;                   /SPLINE, $
           ;;                   DELT_T=delt_t

           final.y  = SMOOTH(final.y,finalSm,/NAN,MISSING=!VALUES.F_NaN)

           finalDat = DATA_CUT({x:final.x,y:final.y},tS_1s, $
                               ;; COUNT=count, $
                               GAP_THRESH=gap_thresh, $
                               INTERP_GAP=interp_gap, $
                               GAP_DIST=gap_dist, $
                               ;; MISSING=missing, $
                               IGNORE_NAN=ignore_nan)
           final = {x:tS_1s, $
                    y:finalDat}

        END
        ELSE: BEGIN
           ;; FA_FIELDS_COMBINE,{time:tS_1s,comp1:tS_1s}, $
           ;;                   {time:final.x,comp1:final.DC}, $
           ;;                   RESULT=DCinterp, $
           ;;                   /SPLINE, $
           ;;                   DELT_T=delt_t
           ;; FA_FIELDS_COMBINE,{time:tS_1s,comp1:tS_1s}, $
           ;;                   {time:final.x,comp1:final.AC}, $
           ;;                   RESULT=ACinterp, $
           ;;                   /SPLINE, $
           ;;                   DELT_T=delt_t
           ;; DCinterp = DATA_CUT({x:final.x,y:final.DC},tS_1s, $
           ;;                     ;; COUNT=count, $
           ;;                     GAP_THRESH=gap_thresh, $
           ;;                     INTERP_GAP=interp_gap, $
           ;;                     GAP_DIST=gap_dist, $
           ;;                     ;; MISSING=missing, $
           ;;                     IGNORE_NAN=ignore_nan)
           ;; ACinterp = DATA_CUT({x:final.x,y:final.AC},tS_1s, $
           ;;                     ;; COUNT=count, $
           ;;                     GAP_THRESH=gap_thresh, $
           ;;                     INTERP_GAP=interp_gap, $
           ;;                     GAP_DIST=gap_dist, $
           ;;                     ;; MISSING=missing, $
           ;;                     IGNORE_NAN=ignore_nan)

           final.y = SMOOTH(final.y,7,/NAN,MISSING=!VALUES.F_NaN)

           yInterp  = DATA_CUT({x:final.x,y:final.y},tS_1s, $
                               ;; COUNT=count, $
                               GAP_THRESH=gap_thresh, $
                               INTERP_GAP=interp_gap, $
                               GAP_DIST=gap_dist, $
                               ;; MISSING=missing, $
                               IGNORE_NAN=ignore_nan)

           ;;some-point smooth
           DCdat    = SMOOTH(yInterp,finalSm,/NAN,MISSING=!VALUES.F_NaN)
           ACdat    = yInterp-DCdat

           ;; ACinterp = DATA_CUT({x:final.x,y:final.AC},tS_1s, $
           ;;                     ;; COUNT=count, $
           ;;                     GAP_THRESH=gap_thresh, $
           ;;                     INTERP_GAP=interp_gap, $
           ;;                     GAP_DIST=gap_dist, $
           ;;                     ;; MISSING=missing, $
           ;;                     IGNORE_NAN=ignore_nan)


           final = {x:tS_1s, $
                    DC:DCdat, $
                    AC:ACdat}

        END
     ENDCASE

  ENDIF ELSE BEGIN

     CASE 1 OF
        KEYWORD_SET(no_separate_DC_AC): BEGIN
           final = {x:final.x, $
                    y:final.DC+final.AC}
        END
        ELSE: BEGIN

        END
     ENDCASE

  ENDELSE

  RETURN,final

END
