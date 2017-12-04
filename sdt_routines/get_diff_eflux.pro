PRO GET_DIFF_EFLUX,T1=t1,T2=t2, $
                   EEB_OR_EES=eeb_or_ees, $
                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                   MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                   SC_POT=sc_pot, $
                   NAME__DIFF_EFLUX=name__diff_eFlux, $
                   CALC_GEOM_FACTORS=calc_geom_factors, $
                   CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
                   MCFADDEN_GAP_TIME=gap_time, $
                   ;; UNITS=units, $          
                   ;; ANGLE=angle, $
                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                   FIT_EACH_ANGLE=fit_each_angle, $
                   TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                   OUT_DIFF_EFLUX=diff_eflux, $
                   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                   DIFF_EFLUX_FILE=diff_eFlux_file, $
                   LOAD_DAT_FROM_FILE=loadFile, $
                   LOAD_DIR=loadDir, $
                   NO_DATA=no_data
                   
  ;; QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  got_restored  = 0
  IF KEYWORD_SET(loadFile) AND KEYWORD_SET(diff_eFlux_file) THEN BEGIN

     IF N_ELEMENTS(loadDir) EQ 0 THEN BEGIN
        loadDir = ''
     ENDIF

     IF FILE_TEST(loadDir+diff_eFlux_file) OR FILE_TEST(diff_eFlux_file) THEN BEGIN
        PRINT,'Restoring ' + diff_eFlux_file + '...'
        realFile = FILE_TEST(loadDir+diff_eFlux_file) ? loadDir+diff_eFlux_file : diff_eFlux_file
        RESTORE,realFile
        ;; got_restored = (N_ELEMENTS(dat_eFlux) GT 0) OR (N_ELEMENTS(diff_eFlux) GT 0)
        got_restored = (SIZE(dat_eFlux,/TYPE) EQ 8) OR (SIZE(diff_eFlux,/TYPE) EQ 8)
        IF ~got_restored AND N_ELEMENTS(diff_eFlux) GT 0 THEN BEGIN
           IF diff_eFlux EQ 0 THEN diff_eFlux = !NULL
        ENDIF
     ENDIF ELSE BEGIN
        PRINT,"Couldn't find " + diff_eFlux_file + "!!!"
        PRINT,'Attempting to get and save for you ...'
        couldntfindLoad = 1
     ENDELSE

     ;; RETURN
  ENDIF

  IF ~KEYWORD_SET(name__diff_eFlux) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(fit_each_angle): BEGIN
           name__diff_eFlux  = 'diff_eFlux__fit_each_angle'
        END
        ELSE: BEGIN
           name__diff_eFlux  = 'diff_eFlux'
        END
     ENDCASE
  ENDIF
  name__diff_eFluxSDT        = 'diff_eFluxSDT'

  routine                    = 'get_fa_'+eeb_or_ees+'_ts'

  IF KEYWORD_SET(calc_geom_factors) THEN PRINT, "Calculating geometry factors for " + $
     routine + ' ...'
  IF ~got_restored THEN BEGIN
     t1Tmp                   = t1
     t2Tmp                   = t2

     dat_eFlux               = CALL_FUNCTION(routine,t1Tmp,t2Tmp, $
                                             CALIB=calc_geom_factors)

     IF KEYWORD_SET(clean_the_McFadden_way) THEN BEGIN


        killed = 0
        ;; First needs to be valid
        IF ~dat_eFlux[0].valid THEN BEGIN
           no_data = 1
           RETURN
        ENDIF ELSE no_data = 0

        ;; Just following MAKE_ESA_CDF in SDT IDL routines
        IF NOT KEYWORD_SET(gap_time) then gap_time = 4. ; for burst data gap_time should be 0.1
        ;; if routine eq 'fa_ees_c' then nskip=2 else nskip=3
        nskip = 3               ;Assume that 3 need to be skipped. I have no idea why fa_ees_c has any bearing

        last_delta_time=dat_eFlux[0].end_time-dat_eFlux[0].time
        last_time = (dat_eFlux[0].time+dat_eFlux[0].end_time)/2.

        ind = 0
        max = N_ELEMENTS(dat_eFlux)
        invalidInds = !NULL
        ;; Skip to first good one
        WHILE ind LT max DO BEGIN

           IF (dat_eFlux[ind].valid) THEN BEGIN

              ;; Test to see if a transition between fast and slow survey occurs, 
              ;; ie delta_time changes, and skip some data if it does.
              IF (abs((dat_eFlux[ind].end_time-dat_eFlux[ind].time) - last_delta_time) GT 1. ) THEN BEGIN
                 
                 ;; if fast to slow, skip two or three arrays
                 IF (dat_eFlux[ind].end_time-dat_eFlux[ind].time) GT last_delta_time THEN BEGIN
                    ;; FOR i=1,nskip DO BEGIN
                    ;;    dat = call_function(routine,t,/calib,/ad)
                    ;; ENDFOR
                    CASE 1 OF
                       ((ind+nskip) GE max) AND (ind EQ 0): BEGIN
                          dat_eFlux = !NULL
                          no_data   = 1
                          killed   += max
                       END
                       (ind+nskip) GE max: BEGIN
                          indsBef   = [0:(ind-1)]
                          ;; dat_eFlux = dat_eFlux
                          killed   += (ind+nskip-max)
                          indsAft   = !NULL
                          ind       = max
                       END
                       ELSE: BEGIN
                          indsBef  = [0:(ind-1)]
                          indsAft  = [(ind+nskip-1):max]      
                          killed  += 3
                       END
                    ENDCASE
                    IF no_data THEN RETURN
                    inds      = [indsBef,indsAft]
                    dat_eFlux = dat_eFlux[inds]

                 ENDIF ELSE BEGIN
                    WHILE dat_eFlux[ind].time LT last_time+7.5 DO BEGIN
                       ind++
                       killed++
                    ENDWHILE
                 ENDELSE
              ENDIF
              
              ;; Test for data gaps and add NAN if gaps are present.
              IF abs((dat_eFlux[ind].time+dat_eFlux[ind].end_time)/2.-last_time) GE gap_time THEN BEGIN
                 IF ind GE 2 THEN dbadtime = dat_eFlux[ind-1].time - dat_eFlux[ind-2].time ELSE dbadtime = gap_time/2.
                 dat_eFlux[ind].time = (last_time) + dbadtime
                 ;; dat_eFlux[ind].delta_time = !values.f_nan
                 dat_eFlux[ind].data[*,*] = !values.f_nan
                 dat_eFlux[ind].energy[*] = !values.f_nan
                 ;; dat_eFlux[ind].angle[*,*] = !values.f_nan
                 dat_eFlux[ind].theta[*,*] = !values.f_nan
                 dat_eFlux[ind].nenergy = !values.f_nan
                 ;; dat_eFlux[ind].n_angle = !values.f_nan
                 ind++
                 killed++
                 IF ((dat_eFlux[ind].time+dat_eFlux[ind].end_time)/2. GT dat_eFlux[ind-1].time + gap_time) AND $
                    (ind LT max) $
                 THEN BEGIN
                    dat_eFlux[ind].time = (dat_eFlux[ind].time+dat_eFlux[ind].end_time)/2. - dbadtime
                    ;; dat_eFlux[ind].delta_time = !values.f_nan
                    dat_eFlux[ind].data[*,*] = !values.f_nan
;			dat_eFlux[ind].energy(*,*) = !values.f_nan
                    dat_eFlux[ind].energy[*] = !values.f_nan
                    dat_eFlux[ind].theta[*,*] = !values.f_nan
                    dat_eFlux[ind].nenergy = !values.f_nan
                    ;; dat_eFlux[ind].n_angle = !values.f_nan
                    ind++
                    killed++
                 ENDIF
              ENDIF

           ENDIF;;  ELSE BEGIN
           ;;    PRINT,'Invalid packet, dat_eFlux[ind].valid ne 1, at: ',TIME_TO_STR(dat_eFlux[ind].time)
           ;;    invalidInds = [invalidInds,ind]
           ;; ENDELSE

           last_time       = (dat_eFlux[ind].time+dat_eFlux[ind].end_time)/2.
           last_delta_time = dat_eFlux[ind].end_time-dat_eFlux[ind].time
           ind++

        ENDWHILE

        invalid = ~dat_eFlux.valid OR (dat_eFlux.nenergy EQ !values.f_nan)
        invalidInds = WHERE(invalid,nInvalid)

        PRINT,FORMAT='(I0,A0)',nInvalid," invalid structs here"
        PRINT,FORMAT='(I0,A0)',killed," were killed ..."

     ENDIF

     tmpT                    = (dat_eFlux[*].time+dat_eFlux[*].end_time)/2.D
     nHere                   = N_ELEMENTS(tmpT)
     keep                    = WHERE(tmpT GE t1 AND tmpT LE t2 AND ~invalid,nKeep)
     
     IF nKeep NE nHere THEN BEGIN

        IF nKeep EQ 0 THEN STOP
        
        PRINT,FORMAT='("Dropping ",I0,A0)',nHere-nKeep," stinker" + ((nHere-nKeep) GT 1 ? 's' : '')
        dat_eFlux            = dat_eFlux[keep]

     ENDIF

     ;;Turn it into the tangly mess of arrays
     DAT_EFLUX_TO_DIFF_EFLUX,dat_eFlux,diff_eFlux, $
                             ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                             FIT_EACH_ANGLE=fit_each_angle, $
                             SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                             TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                             CALC_GEOM_FACTORS=calc_geom_factors
     
     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

        diff_eFlux           =  {x:        TRANSPOSE(diff_eFlux.x), $
                                 y:        TRANSPOSE(diff_eFlux.y), $
                                 angles:   diff_eFlux.angles, $
                                 time:     diff_eFlux.time, $
                                 shiftVals:shiftVals}
        
     ENDIF ELSE BEGIN

        IF ~(KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0)) THEN BEGIN
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

     ;;Add potential, if we got it
     IF SIZE(sc_pot,/TYPE) EQ 8 THEN BEGIN
        ADD_SC_POT_TO_DIFF_EFLUX,diff_eFlux,sc_pot
     ENDIF

  ENDIF
  
  IF KEYWORD_SET(save_diff_eFlux_to_file) OR KEYWORD_SET(couldntfindLoad) THEN BEGIN
     IF (N_ELEMENTS(diff_eFlux_file) GT 0) AND ~got_restored THEN BEGIN
        save_diff_eFlux_to_file = diff_eFlux_file
        PRINT,"Saving diff_eFlux to file: " + save_diff_eFlux_to_file
        SAVE,diff_eFlux,FILENAME=loadDir + save_diff_eFlux_to_file
     ENDIF ELSE BEGIN
        ;; PRINT,"Sorry, no save for you"
     ENDELSE
  ENDIF

  IF KEYWORD_SET(manual_angle_correction) THEN BEGIN

     ;; STOP

     ;; COMMON cc,counter

     ;; cLimit = 1

     ;; IF counter GE cLimit THEN STOP
     ;; IF N_ELEMENTS(counter) EQ 0 THEN counter = 1 ELSE counter += 1


     MANUALLY_CORRECT_DIFF_EFLUX_ANGLE,diff_eFlux,manual_angle_correction

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
