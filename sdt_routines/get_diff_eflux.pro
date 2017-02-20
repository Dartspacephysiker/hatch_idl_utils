PRO GET_DIFF_EFLUX,T1=t1,T2=t2, $
                   EEB_OR_EES=eeb_or_ees, $
                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                   NAME__DIFF_EFLUX=name__diff_eFlux, $
                   CALC_GEOM_FACTORS=calc_geom_factors, $
                   UNITS=units, $          
                   ;; ANGLE=angle, $
                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                   FIT_EACH_ANGLE=fit_each_angle, $
                   TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                   OUT_DIFF_EFLUX=diff_eflux, $
                   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                   LOAD_DAT_FROM_FILE=loadFile, $
                   LOAD_DIR=loadDir
                   
  ;; QUIET=quiet

  COMPILE_OPT idl2

  got_restored  = 0
  IF KEYWORD_SET(loadFile) THEN BEGIN

     IF N_ELEMENTS(loadDir) EQ 0 THEN BEGIN
        loadDir = ''
     ENDIF

     IF FILE_TEST(loadDir+loadFile) THEN BEGIN
        PRINT,'Restoring ' + loadFile + '...'
        RESTORE,loadDir+loadFile
        got_restored = (N_ELEMENTS(dat_eFlux) GT 0) OR (N_ELEMENTS(diff_eFlux) GT 0)
     ENDIF ELSE BEGIN
        PRINT,"Couldn't find " + loadFile + "!!!"
        PRINT,'Attempting to get and save for you ...'
        couldntfindLoad = 1
     ENDELSE

     ;; RETURN
  ENDIF

  IF ~KEYWORD_SET(name__diff_eFlux) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(fit_each_angle): BEGIN
           name__diff_eFlux                   = 'diff_eFlux__fit_each_angle'
        END
        ELSE: BEGIN
           name__diff_eFlux                   = 'diff_eFlux'
        END
     ENDCASE
  ENDIF
  name__diff_eFluxSDT                         = 'diff_eFluxSDT'

  routine                                     = 'get_fa_'+eeb_or_ees+'_ts'

  IF KEYWORD_SET(calc_geom_factors) THEN PRINT, "Calculating geometry factors for " + $
     routine + ' ...'
  IF ~got_restored THEN BEGIN
     dat_eFlux                                   = CALL_FUNCTION(routine,t1,t2, $
                                                                 CALIB=calc_geom_factors)
     IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
        dat_eFlux                                = AVERAGE_SUM3D(dat_eFlux,spectra_average_interval)
     ENDIF
     
     shiftVals                                   = !NULL
     FOR i=0,N_ELEMENTS(dat_eFlux)-1 DO BEGIN
        tempDat                                  = dat_eFlux[i]

        CASE 1 OF
           KEYWORD_SET(fit_each_angle): BEGIN
              tempdiff_eflux = PREP_EFLUX_DATA(tempDat, $
                                               CALIB=calc_geom_factors, $
                                               UNITS=units, $          
                                               RETRACE=1, $ ;;Look up 'retrace' in Carlson et al. [2001] if you want to know why
                                               VEL=vel, $
                                               ANGLE=an, $
                                               ARANGE=ar, $
                                               BINS=bins, $
                                               NO_SORT=no_sort)
           END
           KEYWORD_SET(try_synthetic_SDT_struct): BEGIN
              SPEC2D,tempDat,UNITS='eflux',ANGLE=angle, $
                     OUT_DAT=tempDiff_eFlux, $ ;RETRACE=1, $
                     OUT_FASTSTR=tempDiff_eFluxSDT, $
                     /NO_PLOT
           END
           ELSE: BEGIN
              SPEC2D,tempDat,UNITS='eflux',ANGLE=angle, $
                     OUT_DAT=tempDiff_eFlux, $
                     /NO_PLOT
           END
        ENDCASE

        IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
           REDUCE_EFLUX_TO_MIN_ANGLE,tempDiff_eFlux     
           shiftVals                             = [shiftVals,0]
        ENDIF                   ;ELSE BEGIN

        IF ~KEYWORD_SET(fit_each_angle) AND ~KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
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
        ENDIF

        ADD_EFLUX_TO_EFLUX_STRUCT,diff_eFlux,tempDiff_eFlux, $
                                  ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                  FIT_EACH_ANGLE=fit_each_angle


     ENDFOR
     
     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

        diff_eFlux                               =  {x:        TRANSPOSE(diff_eFlux.x), $
                                                     y:        TRANSPOSE(diff_eFlux.y), $
                                                     angles:   diff_eFlux.angles, $
                                                     time:     diff_eFlux.time, $
                                                     shiftVals:shiftVals}
        
     ENDIF ELSE BEGIN

        IF ~KEYWORD_SET(fit_each_angle) THEN BEGIN
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
        ENDIF


     ENDELSE

  ENDIF
  
  IF KEYWORD_SET(save_diff_eFlux_to_file) OR KEYWORD_SET(couldntfindLoad) THEN BEGIN
     IF KEYWORD_SET(couldntfindload) THEN save_diff_eFlux_to_file = loadFile
     PRINT,"Saving diff_eFlux to file: " + save_diff_eFlux_to_file
     SAVE,diff_eFlux,FILENAME=loadDir + save_diff_eFlux_to_file
  ENDIF

  IF KEYWORD_SET(old_mode) THEN BEGIN
     STORE_DATA,name__diff_eFlux,DATA=diff_eFlux

     IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
        STORE_DATA,name__diff_eFluxSDT,DATA=diff_eFluxSDT
     ENDIF
  ENDIF;;  ELSE BEGIN
  ;;    out_diff_eflux = TEMPORARY(diff_eFlux)
  ;; ENDELSE

END