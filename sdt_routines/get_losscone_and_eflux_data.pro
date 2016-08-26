;2016/05/15
PRO GET_LOSSCONE_AND_EFLUX_DATA,T1=t1,T2=t2, $
                                LOAD_DAT_FROM_FILE=loadFile, $
                                LOAD_DIR=loadDir, $
                                EEB_OR_EES=eeb_or_ees, $
                                DIFF_EFLUX=diff_eFlux, $
                                SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                OUT_ORB=orb, $
                                OUT_ANGLERANGE=e_angle, $
                                ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                FIT_EACH_ANGLE=fit_each_angle, $
                                CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
                                ANGLESTR=angleStr, $
                                ESPECUNITS=eSpecUnits, $
                                ELECTRON_ENERGY_LIMS=energy_electrons, $
                                SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file

  COMPILE_OPT idl2

  IF N_ELEMENTS(t2) EQ 0 THEN BEGIN
     t2                = t1
  ENDIF

  IF KEYWORD_SET(loadFile) THEN BEGIN

     IF N_ELEMENTS(loadDir) EQ 0 THEN BEGIN
        loadDir = ''
     ENDIF

     IF FILE_TEST(loadDir+loadFile) THEN BEGIN
        PRINT,'Restoring ' + loadFile + '...'
        RESTORE,loadDir+loadFile

     ENDIF ELSE BEGIN
        PRINT,"Couldn't find " + loadFile + "!!!"
        STOP
     ENDELSE

     ;; RETURN
  ENDIF

  CASE SIZE(t1,/TYPE) OF
     0: BEGIN
        PRINT,'t1 empty! Returning ...'
        RETURN
     END
     7: BEGIN
        t1             = STR_TO_TIME(t1)
     END
     ELSE: BEGIN
     END
  ENDCASE
  CASE SIZE(t2,/TYPE) OF
     0: BEGIN
        PRINT,'t2 empty! Returning ...'
        RETURN
     END
     7: BEGIN
        t2             = STR_TO_TIME(t2)
     END
     ELSE: BEGIN
     END
  ENDCASE
  
  IF t1 EQ t2 THEN BEGIN
     timeRangeStr      = TIME_TO_STR(t1,/MSEC,/CONV_TO_UNDERSCORE)
  ENDIF ELSE BEGIN
     timeRangeStr      = TIME_TO_STR(t1,/MSEC,/CONV_TO_UNDERSCORE) + '-' + TIME_TO_STR(t2,/MSEC,/CONV_TO_UNDERSCORE)
  ENDELSE

  IF N_ELEMENTS(eSpecUnits) EQ 0 THEN BEGIN
     ;; eSpecUnits     = 'DF'
     eSpecUnits        = 'eflux'
  ENDIF

  ;;get_orbit data
  GET_FA_ORBIT,t1,t2            ;,/all
  
  GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI,t1,t2,lc_angleRange, $
                                          i_angle,i_angle_up, $
                                          north_south, $
                                          ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                          CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
                                          OUT_E_ANGLE=e_angle, $
                                          ANGLESTR=angleStr, $
                                          /JUST_ONE
  
  GET_DATA,'ORBIT',DATA=orbit
  orb                  = orbit.y[0]

  ;; IF ARG_PRESENT(diff_eFlux) OR KEYWORD_SET(save_these) THEN BEGIN
  IF N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN
     GET_DIFF_EFLUX,T1=t1,T2=t2, $
                    EEB_OR_EES=eeb_or_ees, $
                    ANGLE=e_angle, $
                    NAME__DIFF_EFLUX=name__diff_eFlux, $
                    FIT_EACH_ANGLE=fit_each_angle, $
                    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval 


     GET_DATA,name__diff_eFlux,DATA=diff_eFlux
  ENDIF

END