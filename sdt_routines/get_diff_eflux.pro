PRO GET_DIFF_EFLUX,T1=t1,T2=t2, $
                   EEB_OR_EES=eeb_or_ees, $
                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                   NAME__DIFF_EFLUX=name__diff_eFlux, $
                   ANGLE=angle, $
                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle ;, $
                   ;; QUIET=quiet

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(name__diff_eFlux) THEN BEGIN
     name__diff_eFlux                      = 'diff_eFlux'
  ENDIF

  routine                                  = 'get_fa_'+eeb_or_ees+'_ts'
  dat                                      = CALL_FUNCTION(routine,t1,t2,CALIB=calib)
  
  IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
     dat                                   = AVERAGE_SUM3D(dat,spectra_average_interval)
  ENDIF
  
  shiftVals                                = !NULL
  FOR i=0,N_ELEMENTS(dat)-1 DO BEGIN
     tempDat                               = dat[i]

     SPEC2D,tempDat,UNITS='eflux',MSEC=msec,ANGLE=angle, $
            OUT_DAT=tempdiff_eFlux,/NO_PLOT

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        REDUCE_EFLUX_TO_MIN_ANGLE,tempdiff_eFlux     
        shiftVals                          = [shiftVals,0]
     ENDIF ELSE BEGIN

        shiftMe                            = WHERE(tempdiff_eFlux.angles LT 0)
        shiftVal                           = MAX(shiftMe)
        shiftVals                          = [shiftVals,shiftVal]
        IF shiftVal NE -1 THEN BEGIN

           tempdiff_eFlux.angles[shiftMe]  = tempdiff_eFlux.angles[shiftMe]+360.

           tempdiff_eFlux.angles           = SHIFT(tempdiff_eFlux.angles,(-1)*shiftVal-1)
           tempdiff_eFlux.x                = SHIFT(tempdiff_eFlux.x,0,(-1)*shiftVal-1)
           tempdiff_eFlux.y                = SHIFT(tempdiff_eFlux.y,0,(-1)*shiftVal-1)
        ENDIF

     ENDELSE

     ADD_EFLUX_TO_EFLUX_STRUCT,diff_eFlux,tempdiff_eFlux, $
                               ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle

  ENDFOR
  
  IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

     diff_eFlux                            =  {x:        TRANSPOSE(diff_eFlux.x), $
                                               y:        TRANSPOSE(diff_eFlux.y), $
                                               angles:   diff_eflux.angles, $
                                               time:     diff_eFlux.time, $
                                               shiftVals:shiftVals}
     
  ENDIF ELSE BEGIN
     diff_eFlux                            =  {x:        TRANSPOSE(diff_eFlux.x), $
                                               y:        TRANSPOSE(diff_eFlux.y), $
                                               angles:   TRANSPOSE(diff_eflux.angles), $
                                               time:     diff_eFlux.time, $
                                               shiftVals:shiftVals}
  ENDELSE

  STORE_DATA,name__diff_eFlux,DATA=diff_eFlux


END