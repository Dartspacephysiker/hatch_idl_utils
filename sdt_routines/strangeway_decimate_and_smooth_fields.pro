;;09/29/16
;;Smoothing à la Strangeway et al. [2005] (Appendix A)
FUNCTION STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS,data

  COMPILE_OPT IDL2

  IF SIZE(data,/TYPE) NE 8 THEN BEGIN
     PRINT,"SMOOTH_TSERIES: Incorrect data type"
     RETURN,-1
  ENDIF

  finite_y         = FINITE(data.y)

  IF (WHERE(finite_y))[0] EQ -1 THEN BEGIN
     PRINT,"No finite data here! No smoothing to be done ..."
     RETURN,-1
  ENDIF

  FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i

  IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN BEGIN
     PRINT,"Can't Strangeway-decimate or Strangeway-smooth these data! They can't be chunked into buffers ..."
     RETURN,-1
  ENDIF

  PRINT,"Strangeway-decimating and smoothing ..."

  sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
  sPeriods = data.x[strt_i+1]-data.x[strt_i]
  nBufs    = N_ELEMENTS(strt_i)

  IF (WHERE( ABS(FIX(sRates) - sRates) GT 0.1))[0] NE -1 THEN BEGIN
     PRINT,"The sampling frequencies aren't integer! Sorry ..."
     RETURN,-1
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
     WHILE curSampRate GT 1 DO BEGIN
        
        
        ;;7-point smooth
        tmp.y = SMOOTH(tmp.y,7,/NAN,MISSING=!VALUES.F_NaN)
        
        nCurrent       = N_ELEMENTS(tmp.x)
        CASE 1 OF
           curSampRate LT 4: BEGIN
              tmp   = {x:tmp.x[0:nCurrent-1:curSampRate], $
                       y:tmp.y[0:nCurrent-1:curSampRate]}

              curSampRate /= curSampRate

           END
           ( (curSampRate MOD 4) EQ 0): BEGIN
              tmp   = {x:tmp.x[0:nCurrent-1:4], $
                       y:tmp.y[0:nCurrent-1:4]}

              curSampRate /= 4
           END
           ELSE: BEGIN
              PRINT,'What???? Samprate is ' + STRCOMPRESS(curSampRate,/REMOVE_ALL)
              earlyBreak = 1
           END
        ENDCASE


        IF KEYWORD_SET(earlyBreak) THEN BREAK

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

  RETURN,final

END
