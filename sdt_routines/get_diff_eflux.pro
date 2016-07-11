PRO GET_DIFF_EFLUX,T1=t1,T2=t2, $
                   EEB_OR_EES=eeb_or_ees, $
                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                   NAME__DIFF_EFLUX=name__diff_eFlux, $
                   ANGLE=angle, $
                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                   TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct
  ;; QUIET=quiet

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(name__diff_eFlux) THEN BEGIN
     name__diff_eFlux                         = 'diff_eFlux'
  ENDIF

  routine                                     = 'get_fa_'+eeb_or_ees+'_ts'
  dat                                         = CALL_FUNCTION(routine,t1,t2,CALIB=calib)
  
  IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
     dat                                      = AVERAGE_SUM3D(dat,spectra_average_interval)
  ENDIF
  
  shiftVals                                   = !NULL
  FOR i=0,N_ELEMENTS(dat)-1 DO BEGIN
     tempDat                                  = dat[i]

     SPEC2D,tempDat,UNITS='eflux',MSEC=msec,ANGLE=angle, $
            OUT_DAT=tempdiff_eFlux,/NO_PLOT

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        REDUCE_EFLUX_TO_MIN_ANGLE,tempdiff_eFlux     
        shiftVals                             = [shiftVals,0]
     ENDIF ELSE BEGIN

        IF ~KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
           shiftMe                            = WHERE(tempdiff_eFlux.angles LT 0)
           shiftVal                           = MAX(shiftMe)
           shiftVals                          = [shiftVals,shiftVal]
           IF shiftVal NE -1 THEN BEGIN

              tempdiff_eFlux.angles[shiftMe]  = tempdiff_eFlux.angles[shiftMe]+360.

              tempdiff_eFlux.angles           = SHIFT(tempdiff_eFlux.angles,(-1)*shiftVal-1)
              tempdiff_eFlux.x                = SHIFT(tempdiff_eFlux.x,0,(-1)*shiftVal-1)
              tempdiff_eFlux.y                = SHIFT(tempdiff_eFlux.y,0,(-1)*shiftVal-1)
           ENDIF

        ENDIF ELSE BEGIN
           shiftMe                            = WHERE(tempdiff_eFlux.theta LT 0)
           shiftVal                           = MAX(shiftMe)
           shiftVals                          = [shiftVals,shiftVal]
           IF shiftVal NE -1 THEN BEGIN

              tempdiff_eFlux.theta[shiftMe]   = tempdiff_eFlux.theta[shiftMe]+360.

              tempdiff_eFlux.theta            = SHIFT(tempdiff_eFlux.theta,(-1)*shiftVal-1)
              tempdiff_eFlux.energy           = SHIFT(tempdiff_eFlux.energy,0,(-1)*shiftVal-1)
              tempdiff_eFlux.data             = SHIFT(tempdiff_eFlux.data,0,(-1)*shiftVal-1)
           ENDIF

        ENDELSE
     ENDELSE

     ADD_EFLUX_TO_EFLUX_STRUCT,diff_eFlux,tempdiff_eFlux, $
                               ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle

  ENDFOR
  
  IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

     diff_eFlux                               =  {x:        TRANSPOSE(diff_eFlux.x), $
                                                  y:        TRANSPOSE(diff_eFlux.y), $
                                                  angles:   diff_eflux.angles, $
                                                  time:     diff_eFlux.time, $
                                                  shiftVals:shiftVals}
     
  ENDIF ELSE BEGIN
     IF ~KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN

        diff_eFlux                            =  {x:        TRANSPOSE(diff_eFlux.x), $
                                                  y:        TRANSPOSE(diff_eFlux.y), $
                                                  angles:   TRANSPOSE(diff_eflux.angles), $
                                                  time:     diff_eFlux.time, $
                                                  shiftVals:shiftVals}
     ENDIF ELSE BEGIN

        diff_eFlux                            = {data_name:diff_eFlux.data_name, $
                                                 valid:diff_eFlux.valid, $
                                                 project_name:diff_eFlux.project_name, $
                                                 units_name:diff_eFlux.units_name, $
                                                 units_procedure:diff_eFlux.units_procedure, $
                                                 time:diff_eFlux.time, $
                                                 end_time:diff_eFlux.end_time, $
                                                 integ_t:diff_eFlux.integ_t, $
                                                 nbins:diff_eFlux.nbins, $
                                                 nenergy:diff_eFlux.nenergy, $
                                                 data:TRANSPOSE(diff_eFlux.data), $
                                                 ddata:TRANSPOSE(diff_eFlux.ddata), $
                                                 energy:TRANSPOSE(diff_eFlux.energy), $
                                                 denergy: TRANSPOSE(diff_eFlux.denergy), $
                                                 theta:TRANSPOSE(diff_eFlux.theta), $
                                                 dtheta: TRANSPOSE(diff_eFlux.dtheta), $
                                                 geom: TRANSPOSE(diff_eFlux.geom), $
                                                 eff: TRANSPOSE(diff_eFlux.eff), $
                                                 mass:diff_eFlux.mass, $
                                                 geomfactor:diff_eFlux.geomfactor, $
                                                 header_bytes: diff_eFlux.header_bytes, $
                                                 st_index:diff_eFlux.st_index, $
                                                 en_index:diff_eFlux.en_index, $
                                                 npts:diff_eFlux.npts, $
                                                 index:diff_eFlux.index, $
                                                 shiftVals:shiftVals}
     ENDELSE

  ENDELSE

  STORE_DATA,name__diff_eFlux,DATA=diff_eFlux


END