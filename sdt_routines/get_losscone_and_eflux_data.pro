;2016/05/15
PRO GET_LOSSCONE_AND_EFLUX_DATA,T1=t1,T2=t2, $
                                LOAD_DAT_FROM_FILE=loadFile, $
                                LOAD_DIR=loadDir, $
                                EEB_OR_EES=eeb_or_ees, $
                                DIFF_EFLUX=diff_eFlux, $
                                UPGOING=upgoing, $
                                SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                DEF__INCLUDE_SC_POT=dEF__include_sc_pot, $                                 
                                SC_POT=sc_pot, $
                                OUT_ORB=orb, $
                                OUT_ANGLERANGE=e_angle, $
                                ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                FIT_EACH_ANGLE=fit_each_angle, $
                                CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
                                ANGLESTR=angleStr, $
                                ESPECUNITS=eSpecUnits, $
                                ;; ELECTRON_ENERGY_LIMS=energy_electrons, $
                                SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                                _EXTRA=e

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(t2) EQ 0 THEN BEGIN
     t2                = t1
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
  GET_FA_ORBIT,[t1,t2],/TIME_ARRAY,/NO_STORE,STRUC=struc
  
  GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI,t1,t2,lc_angleRange, $
                                          i_angle,i_angle_up, $
                                          north_south, $
                                          CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
                                          UPGOING=upgoing, $
                                          OUT_E_ANGLE=e_angle, $
                                          ANGLESTR=angleStr, $
                                          /JUST_ONE
  
  ;; GET_DATA,'ORBIT',DATA=orbit
  orb = struc.orbit[0]

  ;; IF ARG_PRESENT(diff_eFlux) OR KEYWORD_SET(save_these) THEN BEGIN
  IF N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN

     IF (KEYWORD_SET(dEF__include_sc_pot) OR N_ELEMENTS(dEF__include_sc_pot) EQ 0) AND $
     N_ELEMENTS(sc_pot) EQ 0 THEN BEGIN
        ;; GET_SC_POTENTIAL,T1=diff_eFlux.time[0],T2=diff_eFlux.time[-1], $
        GET_SC_POTENTIAL,T1=t1,T2=t2, $
                         DATA=sc_pot, $
                         FROM_FA_POTENTIAL=pot__from_fa_potential, $
                         ALL=pot__all, $
                         /REPAIR, $
                         CHASTON_STYLE=pot__Chaston_style, $
                         FILENAME=pot__fName, $
                         FROM_FILE=pot__from_file, $
                         ORBIT=struc.orbit[0], $
                         SAVE_FILE=pot__save_file
        
     ENDIF

     GET_DIFF_EFLUX,T1=t1,T2=t2, $
                    EEB_OR_EES=eeb_or_ees, $
                    SC_POT=sc_pot, $
                    ;; ANGLE=e_angle, $
                    NAME__DIFF_EFLUX=name__diff_eFlux, $
                    /CALC_GEOM_FACTORS, $
                    UNITS=eSpecUnits, $          
                    FIT_EACH_ANGLE=fit_each_angle, $
                    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                    OUT_DIFF_EFLUX=diff_eflux, $
                    SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file, $
                    DIFF_EFLUX_FILE=loadFile, $
                    LOAD_DAT_FROM_FILE=loadFile, $
                    LOAD_DIR=loadDir


     IF KEYWORD_SET(old_mode) THEN BEGIN
        GET_DATA,name__diff_eFlux,DATA=diff_eFlux
     ENDIF

     IF SIZE(diff_eFlux,/TYPE) NE 8 THEN BEGIN
        PRINT,"Couldn't get diff_eFlux!"
        RETURN
     ENDIF

  ENDIF

END