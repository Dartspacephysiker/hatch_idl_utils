PRO GET_DIFF_EFLUX__CLEANED_V0,T1=t1,T2=t2, $
                               EEB_OR_EES=eeb_or_ees, $
                               SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                               ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                               MANUAL_ANGLE_CORRECTION=manual_angle_correction, $
                               SC_POT=sc_pot, $
                               NAME__DIFF_EFLUX=name__diff_eFlux, $
                               CALC_GEOM_FACTORS=calc_geom_factors, $
                               CLEAN_THE_MCFADDEN_WAY=clean_the_McFadden_way, $
                               MCFADDEN_GAP_TIME=gap_time, $
                               ARRAY_OF_STRUCTS_INSTEAD=array_of_structs_instead, $
                               ;; UNITS=units, $
                               ;; ANGLE=angle, $
                               ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                               FIT_EACH_ANGLE=fit_each_angle, $
                               TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                               OUT_DIFF_EFLUX=diff_eflux, $
                               SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                               OVERWRITE_EXISTING=overwrite_existing, $
                               DIFF_EFLUX_FILE=diff_eFlux_file, $
                               LOAD_DAT_FROM_FILE=loadFile, $
                               LOAD_DIR=loadDir, $
                               NO_DATA=no_data

  ;; QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  DIFFEFLUXVERSION = '20190116'

  got_restored  = 0

  IF KEYWORD_SET(loadFile) OR KEYWORD_SET(save_diff_eFlux_to_file) THEN BEGIN
     IF N_ELEMENTS(loadDir) EQ 0 THEN BEGIN
        ;; loadDir = ''
        diffEFluxDir = ''
     ENDIF ELSE BEGIN
        diffEFluxDir = loadDir + '/diff_eFlux/'
     ENDELSE
  ENDIF

  IF KEYWORD_SET(loadFile) AND KEYWORD_SET(diff_eFlux_file) THEN BEGIN

     IF (FILE_TEST(diffEFluxDir+diff_eFlux_file) OR FILE_TEST(diff_eFlux_file)) AND ~KEYWORD_SET(overwrite_existing) THEN BEGIN
        isCorrectVersion = 0    ;Start with assumption that it ISN'T

        PRINT,'Restoring ' + diff_eFlux_file + '...'
        realFile = FILE_TEST(diffEFluxDir+diff_eFlux_file) ? diffEFluxDir+diff_eFlux_file : diff_eFlux_file
        RESTORE,realFile

        IF N_ELEMENTS(SAVEDIFFEFLUXVERSION) GT 0 THEN BEGIN
           isCorrectVersion = DIFFEFLUXVERSION EQ SAVEDIFFEFLUXVERSION
        ENDIF ELSE got_restored = 0

        ;; got_restored = (N_ELEMENTS(dat_eFlux) GT 0) OR (N_ELEMENTS(diff_eFlux) GT 0)
        got_restored = (SIZE(dat_eFlux,/TYPE) EQ 8) OR (SIZE(diff_eFlux,/TYPE) EQ 8) AND isCorrectVersion

        IF got_restored THEN BEGIN

           ;; But if we missed most of the data, redo it!
           siste = N_ELEMENTS(diff_eFlux) GT 1 ? diff_eflux[-1].time : diff_eflux.time[-1]
           IF t2-siste GT 8. THEN BEGIN
              PRINT,"Apparently junk diff_eFlux! Remaking ..."
              got_restored = 0
              diff_eFlux = !NULL
           ENDIF

        ENDIF

        IF ~got_restored AND N_ELEMENTS(diff_eFlux) GT 0 THEN BEGIN
           ;; IF diff_eFlux EQ 0 THEN diff_eFlux = !NULL
           diff_eFlux = !NULL
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

     t1Tmp            = t1
     t2Tmp            = t2

     darr    = [1.d,1.d,1.d]

     units = "eFlux"
     nenergy = 96
     nbins   = 64
     blankArr = MAKE_ARRAY(nenergy,nbins,/FLOAT,VALUE=0.)
     tmplt = { $
             data_name       :    '', $
             valid           :       0B, $
             project_name    :    'FAST', $
             units_name      :    'eFlux', $
             units_procedure :    'convert_esa_units2', $
             time            :       0.D, $
             end_time        :       0.D, $
             integ_t         :       0.D, $
             nbins           :       nbins, $
             nenergy         :       nenergy, $
             data            : blankArr, $
             ddata           : blankArr, $
             energy          : blankArr, $
             theta           : blankArr, $
             geom            : blankArr, $
             denergy         : blankArr, $
             ;; dtheta          : MAKE_ARRAY(nbins,/FLOAT,VALUE=0.), $
             dtheta          : blankArr, $
             eff             : MAKE_ARRAY(nenergy,/FLOAT,VALUE=0.), $
             mass            : 0., $
             geomfactor      : 0., $
             header_bytes    : MAKE_ARRAY(44,/FLOAT,VALUE=0.), $
             st_index        : 0L, $
             en_index        : 0L, $
             npts            : 0L, $
             index           : 0L, $
             fa_pos                : darr, $
             fa_vel                : darr, $
             alt                   : 1.d, $
             ilat                  : 1.d, $
             mlt                   : 1.d, $
             orbit                 : 3l, $
             b_model               : darr, $
             b_foot                : darr, $
             foot_lat              : 1.d, $
             foot_lng              : 1.d $
             }

     IF SIZE(sc_pot,/TYPE) EQ 8 THEN BEGIN
        STR_ELEMENT,tmplt,"sc_pot",!VALUES.F_NaN,/ADD_REPLACE
        STR_ELEMENT,tmplt,"charge",!VALUES.F_NaN,/ADD_REPLACE
     ENDIF


     routine = 'get_fa_'+eeb_or_ees+'_c'
     if keyword_set(t1) then begin
        t=t1
        dat = CALL_FUNCTION(routine,t,/calib)
     endif else begin
        t = 1000                ; get first sample
        dat = CALL_FUNCTION(routine,t,/calib,/start)
     endelse

     n = 0
     max = 10000                ; this could be improved (increased from 2000 - KRB)

     cdfdat = REPLICATE(tmplt,max)

     last_time = (dat.time+dat.end_time)/2.
     last_delta_time=dat.end_time-dat.time
     if not keyword_set(gap_time) then gap_time = 4. ; for burst data gap_time should be 0.1

     if not keyword_set(units) then units = 'Eflux'

     ;;	Collect the data - Main Loop

     nNanned = 0
     ;; if routine eq 'fa_ees_c' then nskip=2 else nskip=3
     ;; if fast to slow, skip two or three arrays
     nskip = 3

     if keyword_set(t2) then tmax=t2 else tmax=1.e30

     while (dat.valid ne 0) and (n lt max) do begin
        if (dat.valid eq 1) then begin

           ;; Test to see if a transition between fast and slow survey occurs,
           ;; ie delta_time changes, and skip some data if it does.
           if (abs((dat.end_time-dat.time) - last_delta_time) gt 1. ) then begin
              if (dat.end_time-dat.time) gt last_delta_time then begin
                 for i=1,nskip do begin
                    dat = call_function(routine,t,/calib,/ad)
                 endfor
              endif else begin
                 while dat.time lt last_time+7.5 do begin
                    dat = call_function(routine,t,/calib,/ad)
                 endwhile
              endelse
           endif

           ;; Test for data gaps and add NAN if gaps are present.
           if abs((dat.time+dat.end_time)/2.-last_time) ge gap_time then begin
              if n ge 2 then dbadtime = cdfdat[n-1].time - cdfdat[n-2].time else dbadtime = gap_time/2.

              ;;;;;;;;;;;;;;;;;;;;
              ;; NYE
              if n ge 2 then dbadtime = cdfdat[n-1].time - cdfdat[n-2].time else dbadtime = gap_time/2.
              cdfdat[n].time = (last_time) + dbadtime
              cdfdat[n].end_time = !values.f_nan
              cdfdat[n].data[*,*] = !values.f_nan
              cdfdat[n].energy[*] = !values.f_nan
              cdfdat[n].theta[*,*] = !values.f_nan
              cdfdat[n].nenergy = !values.f_nan
              cdfdat[n].nbins = !values.f_nan

              n=n+1
              nNanned++

              if ((dat.time+dat.end_time)/2. gt cdfdat[n-1].time + gap_time) and (n lt max) then begin

                 ;;;;;;;;;;;;;;;;;;;;
                 ;; NYE

                 ;; cdfdat[n].time = (dat.time+dat.end_time)/2. - dbadtime
                 ;; Changed line below on 2018/10/24 cause I'm not sure if two above are right
                 cdfdat[n].time = dat.time - dbadtime
                 cdfdat[n].end_time = !values.f_nan

                 cdfdat[n].data[*,*] = !values.f_nan
                 cdfdat[n].energy[*] = !values.f_nan
                 cdfdat[n].theta[*,*] = !values.f_nan
                 cdfdat[n].nenergy = !values.f_nan
                 cdfdat[n].nbins = !values.f_nan
                 n=n+1
                 nNanned++

              endif
           ENDIF

           ;;;;;;;;;;;;;;;;;;;;
           ;; NYE
           dat                   = CONV_UNITS(dat,'counts')
           STR_ELEMENT,dat,'ddata',SQRT(dat.data),/ADD_REPLACE
           dat = conv_units(dat,units)

           if n lt max then begin

              ;; cdfdat[n].time             = (dat.time+dat.end_time)/2.
              cdfdat[n].time             = dat.time
              cdfdat[n].data_name        = dat.data_name
              cdfdat[n].valid            = dat.valid
              cdfdat[n].project_name     = dat.project_name
              cdfdat[n].units_name       = dat.units_name
              cdfdat[n].units_procedure  = dat.units_procedure
              cdfdat[n].end_time         = dat.end_time
              cdfdat[n].integ_t          = dat.integ_t
              cdfdat[n].nbins            = dat.nbins
              cdfdat[n].nenergy          = dat.nenergy
              cdfdat[n].data[0:dat.nenergy-1,0:dat.nbins-1]    = dat.data
              cdfdat[n].ddata[0:dat.nenergy-1,0:dat.nbins-1]   = dat.ddata
              cdfdat[n].energy[0:dat.nenergy-1,0:dat.nbins-1]  = dat.energy
              cdfdat[n].theta[0:dat.nenergy-1,0:dat.nbins-1]   = dat.theta
              cdfdat[n].geom[0:dat.nenergy-1,0:dat.nbins-1]    = dat.geom
              cdfdat[n].denergy[0:dat.nenergy-1,0:dat.nbins-1] = dat.denergy
              cdfdat[n].dtheta[0:dat.nenergy-1,0:dat.nbins-1]  = dat.dtheta
              cdfdat[n].eff[0:dat.nenergy-1]                   = dat.eff
              cdfdat[n].mass             = dat.mass
              cdfdat[n].geomfactor       = dat.geomfactor
              cdfdat[n].header_bytes     = dat.header_bytes
              cdfdat[n].index            = dat.index
              cdfdat[n].st_index         = dat.index
              cdfdat[n].en_index         = dat.index+1
              cdfdat[n].npts             = N_ELEMENTS(dat.nenergy*dat.nbins)

              last_time = cdfdat[n].time
              last_delta_time = cdfdat[n].end_time-cdfdat[n].time
              n=n+1
           endif

        endif else begin
           print,'Invalid packet, dat.valid ne 1, at: ',time_to_str(dat.time)
        endelse

        dat = call_function(routine,t,/calib,/ad)
        if dat.valid ne 0 then if dat.time gt tmax then dat.valid=0

     endwhile

     if n ge max then print, 'warning: reached maximum allocation -- data set returned may not be complete'

     cdfdat=cdfdat[0:n-1]
     time = cdfdat.time

     cdfdat = cdfdat[SORT(time)]
     time   = time[SORT(time)]

     print,FORMAT='(A0,I0)','n NaNned: ',nNanned


     IF N_ELEMENTS(WHERE(cdfdat.nenergy GT 48,/NULL)) GT 0 THEN BEGIN
        ;; 20181024 new prob??

        GET_FA_ORBIT,cdfdat[0].time,/TIME_ARRAY,/NO_STORE,STRUC=struc
        
        PRINT,FORMAT='("Orb ",I0,": ",A0,A0)', $
              struc.orbit, $
              "More than 48", $
              " energy bins! Writing to log ..."

        logFile = '/thelonious_data1/FAST_electrons_2018/weird_energy_log.txt'
        OPENW,lun,logFile, $
              /GET_LUN,/APPEND
        PRINTF,lun,FORMAT='("Orb ",I0,": ",A0,A0)', $
               struc.orbit, $
               "More than 48", $
               " energy bins!"
        CLOSE,lun
        FREE_LUN,lun

     ENDIF

     GET_FA_ORBIT,time,/TIME_ARRAY, $
                  ORBIT_FILE=orbit_file,/ALL
     GET_DATA,'fa_pos',data=fapos
     GET_DATA,'fa_vel',data=favel
     GET_DATA,'B_model',data=bmodel
     GET_DATA,'BFOOT',data=bfoot
     for i=0,2 do begin
        cdfdat[*].fa_pos[i]=fapos.y[*,i]
        cdfdat[*].fa_vel[i]=favel.y[*,i]
        cdfdat[*].b_model[i]=bmodel.y[*,i]
        cdfdat[*].b_foot[i]=bfoot.y[*,i]
     endfor

     get_data,'ALT',data=tmp
     cdfdat[*].alt=tmp.y[*]
     get_data,'ILAT',data=tmp
     cdfdat[*].ilat=tmp.y[*]
     get_data,'MLT',data=tmp
     cdfdat[*].mlt=tmp.y[*]
     get_data,'ORBIT',data=tmp
     cdfdat[*].orbit=tmp.y[*]
     orbit_num=STRCOMPRESS(STRING(tmp.y[0]),/REMOVE_ALL)
     GET_data,'FLAT',data=tmp
     cdfdat[*].foot_lat=tmp.y[*]
     get_data,'FLNG',data=tmp
     cdfdat[*].foot_lng=tmp.y[*]

     dat_eFlux = TEMPORARY(cdfdat)


     IF N_ELEMENTS(WHERE(dat_eFlux.valid,/NULL)) EQ 0 THEN BEGIN
        PRINT,"No valid data here ..."
        RETURN
     ENDIF

     ;;Turn it into the tangly mess of arrays
     DAT_EFLUX_TO_DIFF_EFLUX,dat_eFlux,diff_eFlux, $
                             ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                             FIT_EACH_ANGLE=fit_each_angle, $
                             ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                             SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                             TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                             /DONT_ALTER_DIMS, $
                             CALC_GEOM_FACTORS=calc_geom_factors, $
                             ARRAY_OF_STRUCTS_INSTEAD=array_of_structs_instead

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

        diff_eFlux           =  {x:        TRANSPOSE(diff_eFlux.x), $
                                 y:        TRANSPOSE(diff_eFlux.y), $
                                 angles:   diff_eFlux.angles, $
                                 time:     diff_eFlux.time, $
                                 shiftVals:shiftVals}

     ENDIF ELSE BEGIN

        IF ~(KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0)) THEN BEGIN
           diff_eFlux = {x:        TRANSPOSE(diff_eFlux.x), $
                         y:        TRANSPOSE(diff_eFlux.y), $
                         angles:   TRANSPOSE(diff_eFlux.angles), $
                         time:     diff_eFlux.time, $
                         shiftVals:shiftVals}

           IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
              diff_eFluxSDT = {data_name:diff_eFluxSDT.data_name, $
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
        ADD_SC_POT_TO_DIFF_EFLUX,diff_eFlux,sc_pot, $
                                 ARRAY_OF_STRUCTS_INSTEAD=array_of_structs_instead
     ENDIF

  ENDIF

  IF KEYWORD_SET(save_diff_eFlux_to_file) OR KEYWORD_SET(couldntfindLoad) THEN BEGIN
     gotMeString = SIZE(save_diff_eFlux_to_file,/TYPE) EQ 7
     IF ((N_ELEMENTS(diff_eFlux_file) GT 0) OR gotMeString) AND ~got_restored THEN BEGIN
        save_diff_eFlux_to_file = gotMeString ? save_diff_eFlux_to_file : diff_eFlux_file
        SAVEDIFFEFLUXVERSION = DIFFEFLUXVERSION
        PRINT,"Saving diff_eFlux to file: " + save_diff_eFlux_to_file
        SAVE,diff_eFlux,SAVEDIFFEFLUXVERSION,FILENAME=diffEFluxDir + save_diff_eFlux_to_file
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
