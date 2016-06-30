PRO GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
                                  EEB_OR_EES=eeb_or_ees, $
                                  SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                  SDT_NAME=name, $
                                  ANGLE=angle, $
                                  ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                  OUT_ONEDAT=out_oneDat, $
                                  QUIET=quiet

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(name) THEN BEGIN
     name                                        = 'diff_eFlux_oneCount'
  ENDIF

  routine                                        = 'get_fa_'+eeb_or_ees+'_ts'
  dat                                            = CALL_FUNCTION(routine,t1,t2,CALIB=calib)
  
  IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
     dat                                         = AVERAGE_SUM3D(dat,spectra_average_interval)
  ENDIF
  
  shiftVals                                      = !NULL
  FOR i=0,N_ELEMENTS(dat)-1 DO BEGIN
     tempDat                                     = dat[i]

     ;;Make sure units are OK
     CALL_PROCEDURE,tempDat.units_procedure,tempDat,'COUNTS'

     ;;Set count to one
     tempDat.data[*,*]                           = 1

     SPEC2D,tempDat,UNITS='eflux',MSEC=msec,ANGLE=angle, $
            OUT_DAT=tempCount,/NO_PLOT

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        REDUCE_EFLUX_TO_MIN_ANGLE,tempCount     
        shiftVals                                = [shiftVals,0]
     ENDIF ELSE BEGIN

        shiftMe                                  = WHERE(tempCount.angles LT 0)
        shiftVal                                 = MAX(shiftMe)
        shiftVals                                = [shiftVals,shiftVal]
        IF shiftVal NE -1 THEN BEGIN

           tempCount.angles[shiftMe]             = tempCount.angles[shiftMe]+360.

           tempCount.angles                      = SHIFT(tempCount.angles,(-1)*shiftVal-1)
           tempCount.x                           = SHIFT(tempCount.x,0,(-1)*shiftVal-1)
           tempCount.y                           = SHIFT(tempCount.y,0,(-1)*shiftVal-1)
        ENDIF

     ENDELSE

     ADD_EFLUX_TO_EFLUX_STRUCT,dEF_oneCount,tempCount, $
                               ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle

  ENDFOR
  
  IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

     dEF_oneCount                                =  {x:        TRANSPOSE(dEF_oneCount.x), $
                                                     y:        TRANSPOSE(dEF_oneCount.y), $
                                                     angles:   dEF_oneCount.angles, $
                                                     time:     dEF_oneCount.time, $
                                                     shiftVals:shiftVals}
     
  ENDIF ELSE BEGIN
     dEF_oneCount                                =  {x:        TRANSPOSE(dEF_oneCount.x), $
                                                     y:        TRANSPOSE(dEF_oneCount.y), $
                                                     angles:   TRANSPOSE(dEF_oneCount.angles), $
                                                     time:     dEF_oneCount.time, $
                                                     shiftVals:shiftVals}
  ENDELSE

  STORE_DATA,name,DATA=dEF_oneCount













  ;; IF ~KEYWORD_SET(name) THEN BEGIN
  ;;    name                                     = 'diffEflux_oneCount'
  ;; ENDIF

  ;; func                                        = STRING(FORMAT='("GET_FA_",A0)',STRUPCASE(eeb_or_ees))
  ;; IF N_ELEMENTS(t2) GT 0 THEN BEGIN
  ;;    func += '_TS'
  ;;    oneDat                                   = CALL_FUNCTION(func,t1,t2)
  ;; ENDIF ELSE BEGIN
  ;;    oneDat                                   = CALL_FUNCTION(func,t1)
  ;; ENDELSE

  ;; ;;Make sure--is it safe?
  ;; IF ~oneDat[0].valid THEN BEGIN
  ;;    PRINT,'Invalid data from SDT!'
  ;;    oneDat                                   = !NULL
  ;;    oneCurve                                 = !NULL
  ;; ENDIF

  ;; IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
  ;;    oneDat                                   = AVERAGE_SUM3D(oneDat,spectra_average_interval)
  ;; ENDIF

  ;; nDat                                        = N_ELEMENTS(oneDat)

  ;; tempDatArr                                  = !NULL
  ;; oneCurve                                    = !NULL
  ;; tempTime                                    = !NULL
  ;; tempEnergy                                  = !NULL
  ;; FOR i=0,nDat-1 DO BEGIN
  ;;    tempDat                                  = oneDat[i]

  ;;    ;;Make sure units are OK
  ;;    CALL_PROCEDURE,tempDat.units_procedure,tempDat,'COUNTS'

  ;;    ;;Set count to one
  ;;    tempDat.data[*,*]                        = 1

  ;;    ;;Get the one-count curve
  ;;    ;; tempCurve                             = {x:tempDat.time, $
  ;;    ;;                           y:J_2D_FS_EN(tempDat,ANGLE=angle,ENERGY=energy_electrons), $
  ;;    ;;                           ytitle:'One-count curve'}
  ;;    tempCurve                                = J_2D_FS_EN(tempDat,ANGLE=angle,ENERGY=energy_electrons)

  ;;    ;;store 'em
  ;;    tempTime                                 = [tempTime,tempDat.time]
  ;;    tempDatArr                               = [tempDatArr,tempDat]
  ;;    oneCurve                                 = [[oneCurve],[tempCurve]]
  ;;    tempEnergy                               = [[tempEnergy],[REFORM(tempDat.energy[*,0])]]
  ;; ENDFOR

  ;; oneCurve                                    = {x:tempTime, $
  ;;                              y:TRANSPOSE(oneCurve), $
  ;;                              v:TRANSPOSE(tempEnergy), $
  ;;                              ytitle:'One-count curve'}


  ;; STORE_DATA,name,DATA=oneCurve
  ;; IF ~KEYWORD_SET(quiet) THEN PRINT,'Stored one-count nFlux curve as "' + name + '"'

  ;; IF ARG_PRESENT(out_oneDat) THEN out_oneDat  = tempDat

END