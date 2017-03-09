;;09/28/16
PRO SMOOTH_TSERIES,data, $
                   smoothSeconds, $
                   ZERO_UNSMOOTHABLES=zero_unsmoothables, $
                   NRECURSE=nRecurse

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~KEYWORD_SET(nRecurse) THEN nRecurse = 1



  IF SIZE(data,/TYPE) NE 8 THEN BEGIN
     PRINT,"SMOOTH_TSERIES: Incorrect data type"
     RETURN
  ENDIF

  smoothed = data.y
  IF KEYWORD_SET(zero_unsmoothables) THEN BEGIN
     smoothed[*] = 0.
  ENDIF

  finite_y         = FINITE(data.y)

  IF (WHERE(finite_y))[0] EQ -1 THEN BEGIN
     PRINT,"No finite data here! No smoothing to be done ..."
     RETURN
  ENDIF

  FA_FIELDS_BUFS,{time:data.x},BUF_STARTS=strt_i,BUF_ENDS=stop_i
  IF (strt_i[0] EQ 0) AND (stop_i[0] EQ 0) THEN BEGIN

     PRINT,'Old-fashioned smoothing ...'

     sRates = 1./(data.x[1:-1]-data.x[0:-2])

     ;;Old-fashioned way

     ;; smoothed         = data.y
     tmpDat = data
     WHILE nRecurse GT 0 DO BEGIN

        FOR k=0,N_ELEMENTS(tmpDat.y)-1 DO BEGIN

           tmpI          = WHERE(ABS(tmpDat.x-tmpDat.x[k]) LE smoothSeconds AND $
                                 finite_y)

           IF tmpI[0] EQ -1 THEN CONTINUE

           smoothed[k]   = MEAN(tmpDat.y[tmpI],/NAN) ;Make sure to check for NaNs

        ENDFOR

        tmpDat.y          = smoothed

        nRecurse--
     ENDWHILE

  ENDIF ELSE BEGIN

     PRINT,'Chunk smoothing ...'

     sRates = 1./(data.x[strt_i+1]-data.x[strt_i])
     nBufs  = N_ELEMENTS(strt_i)

     tmpDat = data

     WHILE nRecurse GT 0 DO BEGIN

        FOR k=0, nBufs-1 DO BEGIN

           tmpI           = [strt_i[k]:stop_i[k]]
           tmp            = {x:tmpDat.x[tmpI], $
                             y:tmpDat.y[tmpI]}
           
           smooth_int     = CEIL(sRates[k]*smoothSeconds)
           smoothed[tmpI] = SMOOTH(tmp.y,smooth_int,/NAN,MISSING=!VALUES.F_NaN)
           
           ;; data.y[tmpI]   = smoothed

           ;; PRINT,'Smooth int: ' + STRCOMPRESS(smooth_int,/REMOVE_ALL)
        ENDFOR

        tmpDat.y          = smoothed

        nRecurse--
     ENDWHILE

  ENDELSE

  data.y               = smoothed

END
