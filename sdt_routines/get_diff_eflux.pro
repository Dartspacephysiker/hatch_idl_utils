PRO GET_DIFF_EFLUX,T1=t1,T2=t2, $
                   EEB_OR_EES=eeb_or_ees, $
                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                   MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                   SC_POT=sc_pot, $
                   NAME__DIFF_EFLUX=name__diff_eFlux, $
                   CALC_GEOM_FACTORS=calc_geom_factors, $
                   CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
                   ;; UNITS=units, $          
                   ;; ANGLE=angle, $
                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                   FIT_EACH_ANGLE=fit_each_angle, $
                   TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                   OUT_DIFF_EFLUX=diff_eflux, $
                   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                   DIFF_EFLUX_FILE=diff_eFlux_file, $
                   LOAD_DAT_FROM_FILE=loadFile, $
                   LOAD_DIR=loadDir
                   
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


        ind = 0
        max = N_ELEMENTS(dat_eFlux)
        ;; Skip to first good one
        WHILE ind LT max DO BEGIN
           IF dat_eFlux[ind].valid THEN BREAK ELSE ind++
        ENDWHILE

        ;;YOU ARE HERE

        last_time = (dat_eFlux[ind].time+dat_eFlux[ind].end_time)/2.
        last_delta_time=dat_eFlux[ind].end_time-dat_eFlux[ind].time


        WHILE (dat_eFlux[ind].valid) AND (ind lt max) do begin
           IF (dat_eFlux[ind].valid) THEN BEGIN

              ;; Test to see if a transition between fast and slow survey occurs, 
              ;; ie delta_time changes, and skip some data if it does.
              if (abs((dat_eFlux[ind].end_time-dat_eFlux[ind].time) - last_delta_time) gt 1. ) then begin
                 if routine eq 'fa_ees_c' then nskip=2 else nskip=3
                                ; if fast to slow, skip two or three arrays
                 if (dat_eFlux[ind].end_time-dat_eFlux[ind].time) gt last_delta_time then begin
                    for i=1,nskip do begin
                       dat = call_function(routine,t,/calib,/ad)
                    endfor
                 endif else begin
                    while dat_eFlux[ind].time lt last_time+7.5 do begin
                       dat = call_function(routine,t,/calib,/ad)
                    endwhile
                 endelse
              endif
              
              ;; Test for data gaps and add NAN if gaps are present.
              if abs((dat_eFlux[ind].time+dat_eFlux[ind].end_time)/2.-last_time) ge gap_time then begin
                 if n ge 2 then dbadtime = cdfdat[n-1].time - cdfdat[n-2].time else dbadtime = gap_time/2.
                 cdfdat[n].time = (last_time) + dbadtime
                 cdfdat[n].delta_time = !values.f_nan
                 cdfdat[n].data[*,*] = !values.f_nan
                 cdfdat[n].energy[*] = !values.f_nan
                 cdfdat[n].angle[*,*] = !values.f_nan
                 cdfdat[n].n_energy = !values.f_nan
                 cdfdat[n].n_angle = !values.f_nan
                 n=n+1
;;            if ((dat_eFlux[ind].time+dat_eFlux[ind].end_time)/2. gt cdfdat(n-1).time + gap_time) and (n lt max) then begin
;;               cdfdat[n].time = (dat_eFlux[ind].time+dat_eFlux[ind].end_time)/2. - dbadtime
;;               cdfdat[n].delta_time = !values.f_nan
;;               ;; cdfdat[n].data(*,*) = !values.f_nan
;; ;			cdfdat[n].energy(*,*) = !values.f_nan
;;               cdfdat[n].energy(*) = !values.f_nan
;;               ;; cdfdat[n].angle(*,*) = !values.f_nan
;;               cdfdat[n].n_energy = !values.f_nan
;;               cdfdat[n].n_angle = !values.f_nan
;;               n=n+1
;;            endif
              endif

           endif else begin
              print,'Invalid packet, dat_eFlux[ind].valid ne 1, at: ',time_to_str(dat_eFlux[ind].time)
           endelse

           dat = call_function(routine,t,/calib,/ad)
           IF dat_eFlux[ind].valid THEN IF dat_eFlux[ind].time GT tmax THEN dat_eFlux[ind].valid=0

        ENDWHILE

     ENDIF

     tmpT                    = (dat_eFlux[*].time+dat_eFlux[*].end_time)/2.D
     nHere                   = N_ELEMENTS(tmpT)
     keep                    = WHERE(tmpT GE t1 AND tmpT LE t2,nKeep)
     
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