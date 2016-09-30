;;09/29/16
;;Smoothing à la Strangeway et al. [2005] (Appendix A)
FUNCTION STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS,data, $
   INTERP_4HZ_RES_TO_1S_TIMESERIES=interp_4Hz_to_1s, $
   ONESEC_TS=tS_1s

  COMPILE_OPT IDL2

  IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN

     IF N_ELEMENTS(tS_1s) EQ 0 THEN BEGIN
        MESSAGE,"Must provide one-sec time series if you want me to interp for you"
        ;; RETURN,-1
     ENDIF

     IF SIZE(tS_1s,/TYPE) NE 5 THEN BEGIN
        MESSAGE,"Provided 1-s time series isn't of type double! Returning ..."
        ;; RETURN,-1
     ENDIF

     junk1 = MIN(ABS(data.x[0]-tS_1s),strt_i)
     IF strt_i EQ (N_ELEMENTS(tmp_tS)-1) THEN BEGIN
        MESSAGE,'1-s time series ends before desired decimated data begin! Returning ...'
        ;; RETURN,-1
     ENDIF

     junk2 = MIN(ABS(data.x[-1]-tS_1s),stop_i)
     IF stop_i EQ 0 THEN BEGIN
        MESSAGE,'1-s time series ends before desired decimated data begin! Returning ...'
        ;; RETURN,-1
     ENDIF

     ;;Make sure gap is actually 1 second
     IF (WHERE( ABS( tS_1s[1:-1] - tS_1s[0:-2] ) GT 0.01))[0] NE -1 THEN BEGIN
        MESSAGE,'This supposedly 1-s resolved time series is bogus! Try again, liar.'
        ;; RETURN,-1
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

  FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i

  IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN BEGIN
     MESSAGE,"Can't Strangeway-decimate or Strangeway-smooth these data! They can't be chunked into buffers ..."
     ;; RETURN,-1
  ENDIF

  PRINT,"Strangeway-decimating and smoothing ..."

  sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
  sPeriods = data.x[strt_i+1]-data.x[strt_i]
  nBufs    = N_ELEMENTS(strt_i)

  IF (WHERE( ABS(FIX(sRates) - sRates) GT 0.1))[0] NE -1 THEN BEGIN
     MESSAGE,"The sampling frequencies aren't integer! Sorry ..."
     ;; RETURN,-1
  ENDIF

  sRates = FIX(sRates)

  tmpDat = data
  FOR k=0, nBufs-1 DO BEGIN

     ;; curSampPeriod  = sPeriods[k]
     curSampRate    = sRates[k]
     tmpI           = [strt_i[k]:stop_i[k]]
     tmp            = {x:tmpDat.x[tmpI], $
                       y:tmpDat.y[tmpI]}

     ;; WHILE curSampPeriod GT 1 DO BEGIN
     earlyBreak     = 0
     WHILE curSampRate GT 1 DO BEGIN
        
        
        ;;7-point smooth
        tmp.y = SMOOTH(tmp.y,7,/NAN,MISSING=!VALUES.F_NaN)
        
        nCurrent       = N_ELEMENTS(tmp.x)
        CASE 1 OF
           (curSampRate LT 4): BEGIN
              tmp   = {x:tmp.x[0:nCurrent-1:curSampRate], $
                       y:tmp.y[0:nCurrent-1:curSampRate]}

              curSampRate /= curSampRate

           END
           ( curSampRate EQ 4 ): BEGIN

              ;;Don't jump over every fourth data point here
              tmp   = {x:tmp.x[0:nCurrent-1], $
                       y:tmp.y[0:nCurrent-1]}

              ;; IF KEYWORD_SET(interp_4Hz_to_1s) THEN earlyBreak = 1

           END
           ( curSampRate EQ 8 ): BEGIN

              ;;Jump over every other data point here
              tmp   = {x:tmp.x[0:nCurrent-1:2], $
                       y:tmp.y[0:nCurrent-1:2]}

              curSampRate /= 2

              ;; IF KEYWORD_SET(interp_4Hz_to_1s) THEN earlyBreak = 1

           END
           ( (curSampRate MOD 4) EQ 0): BEGIN
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

        IF KEYWORD_SET(interp_4Hz_to_1s) AND ( curSampRate EQ 4 ) THEN BREAK

        ;; IF KEYWORD_SET(earlyBreak) THEN BREAK

     ENDWHILE

     ;;OK, it's been smoothed and decimated to one second. 
     ;;Now let's get Alfvénic and DC components
     DCdat = SMOOTH(tmp.y,7,/NAN,MISSING=!VALUES.F_NaN)
     
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

  ENDFOR

  ;;Interp data with 4 Hz resolution 

  IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN

     IF curSampRate NE 4 THEN STOP
     
     FA_FIELDS_COMBINE,{time:tS_1s,comp1:tS_1s}, $
                       {time:final.x,comp1:final.DC}, $
                       RESULT=DCinterp, $
                       /SPLINE, $
                       DELT_T=0.9
     FA_FIELDS_COMBINE,{time:tS_1s,comp1:tS_1s}, $
                       {time:final.x,comp1:final.AC}, $
                       RESULT=ACinterp, $
                       /SPLINE, $
                       DELT_T=0.9

     final = {x:tS_1s, $
              DC:DCinterp, $
              AC:ACinterp}

  ENDIF

  ;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;;Below I was attempting to go manually. Why not just use FA_FIELDS_COMBINE?
  ;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;; IF KEYWORD_SET(interp_4Hz_to_1s) THEN BEGIN

  ;;    IF curSampRate NE 4 THEN STOP
     
  ;;    tmp_tS = tS_1s

  ;;    ;;Make sure life is sensible at the beginning
  ;;    junk1 = MIN(ABS(final.x[0]-tmp_tS),strt_i)
  ;;    IF ( junk1 GT 4 ) THEN BEGIN

  ;;       PRINT,"Trimming decimated data; they start before 1-s time series ..."

  ;;       junk11 = MIN(ABS(tmp_tS[strt_i]-final.x),strt_i)

  ;;       IF ( junk11 GT 4 ) THEN STOP

  ;;       strt_i += ( strt_i EQ -1 ) ;Make sure indices are sensible

  ;;       final  = {x:final.x[start_i:-1], $
  ;;                 DC:final.DC[start_i:-1], $
  ;;                 AC:final.AC[start_i:-1]}

  ;;    ENDIF ELSE BEGIN

  ;;       tmp_tS = tmp_tS[strt_i:-1]

  ;;    ENDELSE

  ;;    ;;Now make sure things end sensibly
  ;;    junk2 = MIN(ABS(tmp.x[-1]-tmp_tS),stop_i)
  ;;    IF ( junk2 GT 4 ) THEN BEGIN

  ;;       PRINT,"Trimming input data; they go outside the bounds ..."

  ;;       junk22 = MIN(ABS(tmp_tS[-1]-final.x),stop_i)

  ;;       IF ( junk22 GT 4 ) THEN STOP

  ;;       stop_i -= ( stop_i EQ N_ELEMENTS(final.x) ) ;Make sure indices are sensible

  ;;       final  = {x:final.x[0:stop_i], $
  ;;                 DC:final.DC[0:stop_i], $
  ;;                 AC:final.AC[0:stop_i]}

  ;;    ENDIF ELSE BEGIN

  ;;       tmp_tS = tmp_tS[0:stop_i]

  ;;    ENDELSE

  ;;    ;;Now look for gaps
  ;;    ;;At this point we've made sure that there is overlap between both time series

  ;;    gap_i = WHERE(final.x[1:-1]-final.x[0:-2] GT 4,nGaps)

  ;;    IF nGaps GT 0 THEN BEGIN

  ;;       PRINT,'Gaps! Junking them ...'

  ;;       separate_start = [0,gap_i+1]
  ;;       separate_stop  = [gap_i,N_ELEMENTS(final.x)-1]

        
  ;;       finDC = !NULL
  ;;       finAC = !NULL
  ;;       finT  = !NULL
  ;;       FOR k=0,nGaps-2 DO BEGIN
  ;;          tmpI  = [separate_start[k]:separate_stop[k]]
  ;;          tmpt1 = final.x[separate_start[k]]
  ;;          tmpt2 = final.x[separate_stop[k]]

  ;;          ;; gapt1 = final.x[separate_stop[k]]
  ;;          ;; gapt2 = final.x[separate_start[k+1]]

  ;;          junk2 = MIN(ABS(tmpt1-tmp_tS),tmpstrt_i)
  ;;          junk2 = MIN(ABS(tmpt2-tmp_tS),tmpstop_i)

  ;;          tmptmp_tS = tmp_tS[tmpstrt_i:tmpstop_i]

  ;;          PRINT,"Junked " + STRCOMPRESS(k+1,/REMOVE_ALL) + " gaps "

  ;;          finDC = [finDC,INTERPOL(final.DC[tmpI],final.x[tmpI],tmptmp_tS,/NAN)]
  ;;          finAC = [finAC,INTERPOL(final.AC[tmpI],final.x[tmpI],tmptmp_tS,/NAN)]
           
  ;;          finT  = [finT,tmptmp_tS]
  ;;       ENDFOR

  ;;       final    = {x:finT, $
  ;;                   DC:finDC, $
  ;;                   AC:finAC}

  ;;       ;;Just close up the gaps in tmp_tS
  ;;       ;; FOR k=0,nGaps-2 DO BEGIN
  ;;       ;;    gapt1 = final.x[separate_stop[k]]
  ;;       ;;    gapt2 = final.x[separate_start[k+1]]

  ;;       ;;    junk2 = MIN(ABS(gapt1-tmp_tS),strtJunk_i)
  ;;       ;;    junk2 = MIN(ABS(gapt2-tmp_tS),stopJunk_i)

  ;;       ;;    tmp_tS = [tmp_tS[0:strtJunk_i],tmp_tS[stopJunk_i:-1]]

  ;;       ;;    PRINT,"Junked " + STRCOMPRESS(k+1,/REMOVE_ALL) + " gaps "
  ;;       ;; ENDFOR

  ;;    ENDIF ELSE BEGIN

  ;;       ;; separate_start = [0]
  ;;       ;; separate_stop  = [N_ELEMENTS(final.x)-1]

  ;;       PRINT,"No gaps!"
  ;;       finDC      = INTERPOL(final.DC,final.x,tmp_tS,/NAN)
  ;;       finAC      = INTERPOL(final.AC,final.x,tmp_tS,/NAN)

  ;;       final      = {x:tmp_tS, $
  ;;                     DC:finDC, $
  ;;                     AC:finAC}

  ;;    ENDELSE
        


  ;; ENDIF




  RETURN,final

END
