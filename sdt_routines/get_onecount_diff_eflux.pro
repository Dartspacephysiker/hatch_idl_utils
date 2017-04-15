PRO GET_ONECOUNT_DIFF_EFLUX,t1,t2, $
                            ;; LOAD_DAT_FROM_FILE=loadFile, $
                            EEB_OR_EES=eeb_or_ees, $
                            SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                            SC_POT=sc_pot, $
                            IN_PROTOSTRUCT=in_protoStruct, $
                            SDT_NAME=name, $
                            ANGLE=angle, $
                            ;; ESPECUNITS=eSpecUnits, $
                            ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                            FIT_EACH_ANGLE=fit_each_angle, $
                            TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct, $
                            OUT_ONEDAT=out_oneDat, $
                            DEF_ONECOUNT=dEF_oneCount, $
                            SAVE_DEF_ONECOUNT_TO_FILE=save_dEF_oneCount_to_file, $
                            DIFF_EFLUX_FILE=diff_eFlux_file, $
                            LOAD_DAT_FROM_FILE=loadFile, $
                            LOAD_DIR=loadDir, $
                            QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(diff_eFlux_file) THEN BEGIN
     oneCount_file = diff_eFlux_file.Replace('diff_eflux','diff_1Count')
  ENDIF

  got_restored  = 0
  IF KEYWORD_SET(loadFile) AND KEYWORD_SET(diff_eFlux_file) THEN BEGIN

     IF N_ELEMENTS(loadDir) EQ 0 THEN BEGIN
        loadDir = ''
     ENDIF

     IF FILE_TEST(loadDir+oneCount_file) THEN BEGIN
        PRINT,'Restoring ' + oneCount_file + '...'
        RESTORE,loadDir+oneCount_file
        got_restored = (N_ELEMENTS(dat_eFlux) GT 0) OR (N_ELEMENTS(dEF_oneCount) GT 0)
     ENDIF ELSE BEGIN
        PRINT,"Couldn't find " + oneCount_file + "!!!"
        PRINT,'Attempting to get and save for you ...'
        couldntfindLoad = 1
     ENDELSE

     ;; RETURN
  ENDIF

  IF ~KEYWORD_SET(name) THEN BEGIN
     CASE 1 OF
        (KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0)): BEGIN
           name                   = 'diff_eFlux_oneCount__fit_each_angle'
        END
        ELSE: BEGIN
           name                   = 'diff_eFlux_oneCount'
        END
     ENDCASE
  ENDIF

  routine                                        = 'get_fa_'+eeb_or_ees+'_ts'

  IF ~got_restored THEN BEGIN

     IF KEYWORD_SET(in_protoStruct) THEN BEGIN
        dat = MAKE_ARRAY_OF_SDT_STRUCTS_FROM_PREPPED_EFLUX( $
              in_protoStruct, $
              HAS_SC_POT=TAG_EXIST(in_protoStruct,'sc_pot'))
     ENDIF ELSE BEGIN
        dat                                            = CALL_FUNCTION(routine,t1,t2,CALIB=calib)
        
        IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
           dat                                         = AVERAGE_SUM3D(dat,spectra_average_interval)
        ENDIF
     ENDELSE
     
     shiftVals                                      = !NULL
     FOR i=0,N_ELEMENTS(dat)-1 DO BEGIN
        tempDat                                     = dat[i]

        ;;Make sure units are OK
        CALL_PROCEDURE,tempDat.units_procedure,tempDat,'COUNTS'

        ;;Set count to one
        tempDat.data[*,*]                           = 1

        CASE 1 OF
           (KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0)): BEGIN
              tempCount = PREP_EFLUX_DATA( $
                          tempDat, $
                          UNITS=eSpecUnits, $          
                          RETRACE=retrace, $
                          VEL=vel, $
                          ANGLE=an, $
                          ARANGE=ar, $
                          BINS=bins, $
                          NO_SORT=no_sort);; , $
                          ;; SC_POTVAL=TAG_EXIST(tempDat,'sc_pot') ? tempDat.sc_pot : !NULL)
           END
           KEYWORD_SET(try_synthetic_SDT_struct): BEGIN
              SPEC2D,tempDat,UNITS=eSpecUnits,ANGLE=angle, $
                     OUT_DAT=tempCount, $
                     OUT_FASTSTR=tempCountSDT, $
                     /NO_PLOT
           END
           ELSE: BEGIN
              SPEC2D,tempDat,UNITS=eSpecUnits,ANGLE=angle, $
                     OUT_DAT=tempCount, $
                     /NO_PLOT
           END
        ENDCASE

        IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
           REDUCE_EFLUX_TO_MIN_ANGLE,tempCount     
           shiftVals                                = [shiftVals,0]
        ENDIF                   ;ELSE BEGIN

        IF ~(KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0)) AND $
           ~KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
           shiftMe                                  = WHERE(tempCount.angles LT 0)
           shiftVal                                 = MAX(shiftMe)
           shiftVals                                = [shiftVals,shiftVal]
           IF shiftVal NE -1 THEN BEGIN

              tempCount.angles[shiftMe]             = tempCount.angles[shiftMe]+360.

              tempCount.angles                      = SHIFT(tempCount.angles,(-1)*shiftVal-1)
              tempCount.x                           = SHIFT(tempCount.x,0,(-1)*shiftVal-1)
              tempCount.y                           = SHIFT(tempCount.y,0,(-1)*shiftVal-1)
           ENDIF
           ;; ENDIF ELSE BEGIN
           IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
              shiftMe                            = WHERE(tempCountSDT.theta LT 0)
              shiftVal                           = MAX(shiftMe)
              shiftVals                          = [shiftVals,shiftVal]
              IF shiftVal NE -1 THEN BEGIN
                 
                 tempCountSDT.theta[shiftMe]     = tempCountSDT.theta[shiftMe]+360.
                 
                 tempCountSDT.theta              = SHIFT(tempCountSDT.theta,(-1)*shiftVal-1)
                 tempCountSDT.energy             = SHIFT(tempCountSDT.energy,0,(-1)*shiftVal-1)
                 tempCountSDT.data               = SHIFT(tempCountSDT.data,0,(-1)*shiftVal-1)
              ENDIF
              ;; ENDELSE

              ADD_EFLUX_TO_EFLUX_STRUCT,dEF_oneCountSDT,tempCountSDT, $
                                        ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                        TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct
           ENDIF
        ENDIF

        ADD_EFLUX_TO_EFLUX_STRUCT,dEF_oneCount,tempCount, $
                                  ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                  FIT_EACH_ANGLE=fit_each_angle


     ENDFOR
     
     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

        dEF_oneCount                                =  {x:        TRANSPOSE(dEF_oneCount.x), $
                                                        y:        TRANSPOSE(dEF_oneCount.y), $
                                                        angles:   dEF_oneCount.angles, $
                                                        time:     dEF_oneCount.time, $
                                                        shiftVals:shiftVals}
        
     ENDIF ELSE BEGIN

        IF ~(KEYWORD_SET(fit_each_angle) OR (N_ELEMENTS(fit_each_angle) EQ 0)) THEN BEGIN
           dEF_oneCount                                =  {x:        TRANSPOSE(dEF_oneCount.x), $
                                                           y:        TRANSPOSE(dEF_oneCount.y), $
                                                           angles:   TRANSPOSE(dEF_oneCount.angles), $
                                                           time:     dEF_oneCount.time, $
                                                           shiftVals:shiftVals}

           IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
              dEF_oneCountSDT = {data_name:dEF_oneCountSDT.data_name, $
                                 valid:dEF_oneCountSDT.valid, $
                                 project_name:dEF_oneCountSDT.project_name, $
                                 units_name:dEF_oneCountSDT.units_name, $
                                 units_procedure:dEF_oneCountSDT.units_procedure, $
                                 time:dEF_oneCountSDT.time, $
                                 end_time:dEF_oneCountSDT.end_time, $
                                 integ_t:dEF_oneCountSDT.integ_t, $
                                 nbins:dEF_oneCountSDT.nbins, $
                                 nenergy:dEF_oneCountSDT.nenergy, $
                                 data:TRANSPOSE(dEF_oneCountSDT.data), $
                                 ddata:TRANSPOSE(dEF_oneCountSDT.ddata), $
                                 energy:TRANSPOSE(dEF_oneCountSDT.energy), $
                                 denergy: TRANSPOSE(dEF_oneCountSDT.denergy), $
                                 theta:TRANSPOSE(dEF_oneCountSDT.theta), $
                                 dtheta: TRANSPOSE(dEF_oneCountSDT.dtheta), $
                                 geom: TRANSPOSE(dEF_oneCountSDT.geom), $
                                 eff: TRANSPOSE(dEF_oneCountSDT.eff), $
                                 mass:dEF_oneCountSDT.mass, $
                                 geomfactor:dEF_oneCountSDT.geomfactor, $
                                 header_bytes: dEF_oneCountSDT.header_bytes, $
                                 st_index:dEF_oneCountSDT.st_index, $
                                 en_index:dEF_oneCountSDT.en_index, $
                                 npts:dEF_oneCountSDT.npts, $
                                 index:dEF_oneCountSDT.index, $
                                 shiftVals:shiftVals}
              ;; ENDELSE
           ENDIF
        ENDIF
     ENDELSE

     IF SIZE(sc_pot,/TYPE) EQ 8 THEN BEGIN
        ADD_SC_POT_TO_DIFF_EFLUX,dEF_oneCount,sc_pot
     ENDIF

  ENDIF
  
  IF KEYWORD_SET(save_dEF_oneCount_to_file) OR KEYWORD_SET(couldntfindLoad) THEN BEGIN
     IF (KEYWORD_SET(couldntfindload) OR (N_ELEMENTS(oneCount_file) GT 0)) THEN BEGIN
        save_dEF_oneCount_to_file = oneCount_file
        PRINT,"Saving dEF_oneCount to file: " + save_dEF_oneCount_to_file
        SAVE,dEF_oneCount,FILENAME=loadDir + save_dEF_oneCount_to_file
     ENDIF ELSE BEGIN
        PRINT,"Sorry, no save for you"
     ENDELSE
  ENDIF

  IF KEYWORD_SET(old_mode) THEN BEGIN
     STORE_DATA,name,DATA=dEF_oneCount
  ENDIF

  IF KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
     STORE_DATA,name+"SDT",DATA=dEF_oneCountSDT
  ENDIF

END

