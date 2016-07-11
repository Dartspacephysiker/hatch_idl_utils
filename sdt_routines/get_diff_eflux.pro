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
     name__diff_eFluxSDT                      = 'diff_eFluxSDT'
  ENDIF

  routine                                     = 'get_fa_'+eeb_or_ees+'_ts'
  dat                                         = CALL_FUNCTION(routine,t1,t2,CALIB=calib)
  
  IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
     dat                                      = AVERAGE_SUM3D(dat,spectra_average_interval)
  ENDIF
  
  shiftVals                                   = !NULL
  FOR i=0,N_ELEMENTS(dat)-1 DO BEGIN
     tempDat                                  = dat[i]

     IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
        SPEC2D,tempDat,UNITS='eflux',MSEC=msec,ANGLE=angle, $
               OUT_DAT=tempDiff_eFlux, $
               OUT_FASTSTR=tempDiff_eFluxSDT, $
               /NO_PLOT
     ENDIF ELSE BEGIN
        SPEC2D,tempDat,UNITS='eflux',MSEC=msec,ANGLE=angle, $
               OUT_DAT=tempDiff_eFlux, $
               /NO_PLOT
     ENDELSE

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        REDUCE_EFLUX_TO_MIN_ANGLE,tempDiff_eFlux     
        shiftVals                             = [shiftVals,0]
     ENDIF ELSE BEGIN

           shiftMe                            = WHERE(tempDiff_eFlux.angles LT 0)
           shiftVal                           = MAX(shiftMe)
           shiftVals                          = [shiftVals,shiftVal]
           IF shiftVal NE -1 THEN BEGIN

              tempDiff_eFlux.angles[shiftMe]  = tempDiff_eFlux.angles[shiftMe]+360.

              tempDiff_eFlux.angles           = SHIFT(tempDiff_eFlux.angles,(-1)*shiftVal-1)
              tempDiff_eFlux.x                = SHIFT(tempDiff_eFlux.x,0,(-1)*shiftVal-1)
              tempDiff_eFlux.y                = SHIFT(tempDiff_eFlux.y,0,(-1)*shiftVal-1)
           ENDIF

        IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
           shiftMe                            = WHERE(tempDiff_eFluxSDT.theta LT 0)
           shiftVal                           = MAX(shiftMe)
           shiftVals                          = [shiftVals,shiftVal]
           IF shiftVal NE -1 THEN BEGIN

              tempDiff_eFluxSDT.theta[shiftMe]   = tempDiff_eFluxSDT.theta[shiftMe]+360.

              tempDiff_eFluxSDT.theta            = SHIFT(tempDiff_eFluxSDT.theta,(-1)*shiftVal-1)
              tempDiff_eFluxSDT.energy           = SHIFT(tempDiff_eFluxSDT.energy,0,(-1)*shiftVal-1)
              tempDiff_eFluxSDT.data             = SHIFT(tempDiff_eFluxSDT.data,0,(-1)*shiftVal-1)
           ENDIF

           ADD_EFLUX_TO_EFLUX_STRUCT,diff_eFluxSDT,tempDiff_eFluxSDT, $
                                     ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                     TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct


        ENDIF

     ENDELSE

     ADD_EFLUX_TO_EFLUX_STRUCT,diff_eFlux,tempDiff_eFlux, $
                               ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle; , $
                               ;; TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct

  ENDFOR
  
  IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

     diff_eFlux                               =  {x:        TRANSPOSE(diff_eFlux.x), $
                                                  y:        TRANSPOSE(diff_eFlux.y), $
                                                  angles:   diff_eFlux.angles, $
                                                  time:     diff_eFlux.time, $
                                                  shiftVals:shiftVals}
     
  ENDIF ELSE BEGIN

        diff_eFlux                            =  {x:        TRANSPOSE(diff_eFlux.x), $
                                                  y:        TRANSPOSE(diff_eFlux.y), $
                                                  angles:   TRANSPOSE(diff_eFlux.angles), $
                                                  time:     diff_eFlux.time, $
                                                  shiftVals:shiftVals}

     IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
        diff_eFluxSDT                         = {data_name:diff_eFluxSDT.data_name, $
                                                 valid:diff_eFluxSDT.valid, $
                                                 project_name:diff_eFluxSDT.project_name, $
                                                 units_name:diff_eFluxSDT.units_name, $
                                                 units_procedure:diff_eFluxSDT.units_procedure, $
                                                 time:diff_eFluxSDT.time, $
                                                 end_time:diff_eFluxSDT.end_time, $
                                                 integ_t:diff_eFluxSDT.integ_t, $
                                                 nbins:diff_eFluxSDT.nbins, $
                                                 nenergy:diff_eFluxSDT.nenergy, $
                                                 data:TRANSPOSE(diff_eFluxSDT.data), $
                                                 ddata:TRANSPOSE(diff_eFluxSDT.ddata), $
                                                 energy:TRANSPOSE(diff_eFluxSDT.energy), $
                                                 denergy: TRANSPOSE(diff_eFluxSDT.denergy), $
                                                 theta:TRANSPOSE(diff_eFluxSDT.theta), $
                                                 dtheta: TRANSPOSE(diff_eFluxSDT.dtheta), $
                                                 geom: TRANSPOSE(diff_eFluxSDT.geom), $
                                                 eff: TRANSPOSE(diff_eFluxSDT.eff), $
                                                 mass:diff_eFluxSDT.mass, $
                                                 geomfactor:diff_eFluxSDT.geomfactor, $
                                                 header_bytes: diff_eFluxSDT.header_bytes, $
                                                 st_index:diff_eFluxSDT.st_index, $
                                                 en_index:diff_eFluxSDT.en_index, $
                                                 npts:diff_eFluxSDT.npts, $
                                                 index:diff_eFluxSDT.index, $
                                                 shiftVals:shiftVals}
        ENDIF

  ENDELSE

  STORE_DATA,name__diff_eFlux,DATA=diff_eFlux

     IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
        STORE_DATA,name__diff_eFluxSDT,DATA=diff_eFluxSDT
     ENDIF
END