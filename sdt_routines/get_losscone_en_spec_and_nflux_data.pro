;2016/05/15
PRO GET_LOSSCONE_EN_SPEC_AND_NFLUX_DATA,T1=t1,T2=t2, $
                                        EEB_OR_EES=eeb_or_ees, $
                                        EN_SPEC=eSpec, $
                                        JE_EN=je_en, $
                                        DIFF_EFLUX=diff_eFlux, $
                                        SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                        OUT_ORB=orb, $
                                        OUT_ANGLERANGE=e_angle, $
                                        ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                        CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
                                        ANGLESTR=angleStr, $
                                        GET_MASS_AND_DT=get_mass_and_dt, $
                                        OUT_MASS=out_mass, $
                                        OUT_DT=out_dt, $
                                        ESPECUNITS=eSpecUnits, $
                                        ELECTRON_ENERGY_LIMS=energy_electrons, $
                                        SAVE_ESPEC_AND_NFLUX=save_these,$
                                        SAVEFILENAME=saveFN


  COMPILE_OPT idl2

  ;; @startup

  IF ~KEYWORD_SET(eeb_or_ees) THEN BEGIN
     eeb_or_ees        = 'ees'
  ENDIF

  IF ~KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons  = [3e1,3.6e4]
  ENDIF

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
  GET_FA_ORBIT,t1,t2 ;,/all
  
  GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI,t1,t2,lc_angleRange, $
                                          i_angle,i_angle_up, $
                                          north_south, $
                                          ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                          /JUST_ONE
  
  ;;Handle angle ranges
  IF N_ELEMENTS(custom_e_angleRange) EQ 0 THEN BEGIN
     PRINT,FORMAT='("Using loss-cone angle range: [",F0.2,",",F0.2,"]")', $
           lc_angleRange[0], $
           lc_angleRange[1]
     e_angle           = lc_angleRange

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        angleStr       = STRING(FORMAT='("--loss-cone_e_angle_",F0.1,"-",F0.1)', $
                          lc_angleRange[0], $
                          lc_angleRange[1])
     ENDIF ELSE BEGIN
        angleStr       = STRING(FORMAT='("--only_field-aligned_e_angle_",F0.1,"-",F0.1)', $
                          lc_angleRange[0], $
                          lc_angleRange[1])
     ENDELSE
  ENDIF ELSE BEGIN
     PRINT,FORMAT='("Using provided angle range: [",F0.2,",",F0.2,"]")', $
           custom_e_angleRange[0], $
           custom_e_angleRange[1]
     e_angle           = custom_e_angleRange
     angleStr          = STRING(FORMAT='("--e_angle_",F0.1,"-",F0.1)', $
                       custom_e_angleRange[0], $
                       custom_e_angleRange[1])
  ENDELSE

  GET_DATA,'ORBIT',DATA=orbit
  orb                  = orbit.y[0]

  ;;Make the spectrogram data struct
  IF ARG_PRESENT(eSpec) OR KEYWORD_SET(save_these) THEN BEGIN
     
     IF KEYWORD_SET(get_mass_and_dt) THEN BEGIN
        GET_EN_SPEC_FOR_FIT,'fa_' + eeb_or_ees,UNITS=eSpecUnits,NAME='el',T1=t1,T2=t2,$
                            ANGLE=e_angle, $
                            OUT_DT=out_dt, $
                            OUT_MASS=out_mass
                                ;,RETRACE=1
     ENDIF ELSE BEGIN
        GET_EN_SPEC,'fa_' + eeb_or_ees,UNITS=eSpecUnits,NAME='el',T1=t1,T2=t2,$
                    ANGLE=e_angle ;,RETRACE=1
     ENDELSE
     ;;GET the spectrogram data struct
     GET_DATA,'el',DATA=eSpec
  ENDIF
  
  ;;Make the nFlux data struct
  IF ARG_PRESENT(je_en) OR KEYWORD_SET(save_these) THEN BEGIN
     GET_2DT_TS,'j_2d_fs_en','fa_' + eeb_or_ees,T1=t1,T2=t2,NAME='Je_en', $
                ENERGY=energy_electrons, $
                ANGLE=e_angle, $
                AVG_INTERVAL=spectra_average_interval
     ;;GET the spectrogram data struct
     GET_DATA,'Je_en',DATA=je_en
  ENDIF

  IF ARG_PRESENT(diff_eflux) OR KEYWORD_SET(save_these) THEN BEGIN
     GET_DIFF_EFLUX,T1=t1,T2=t2, $
                    EEB_OR_EES=eeb_or_ees, $
                    ANGLE=e_angle, $
                    NAME__DIFF_EFLUX=name__diff_eFlux, $
                    SPECTRA_AVERAGE_INTERVAL=spectra_average_interval
     GET_DATA,name__diff_eFlux,DATA=diff_eFlux
  ENDIF

  IF KEYWORD_SET(save_these) THEN BEGIN
     
     CASE STRUPCASE(eeb_or_ees) OF
        'EEB': BEGIN
           dat         = GET_FA_EEB_TS (t1, t2)
        END
        'EES': BEGIN
           dat         = GET_FA_EES_TS (t1, t2)
        END
     ENDCASE
     
     dj_2d             = !NULL
     ;; dn_2d          = !NULL
     FOR k=0,N_ELEMENTS(dat)-1 DO BEGIN
        tempDat        = dat[k]
        ;; fu_spec2d,'n_2d_fs',tempDat,OUT_PARTIAL=temp_dn_2d,ANGLE=e_angle,/NOPLOT ;,/integ_f,/integ_r ; plot partial density, partial integral densities
        fu_spec2d,'j_2d_fs',tempDat,OUT_PARTIAL=temp_dj_2d,ANGLE=e_angle,/NOPLOT ;,/integ_f,/integ_r ; plot partial density, partial integral densities
        ;; dn_2d       = [dn_2d,temp_dn_2d]
        dj_2d          = [[dj_2d],[temp_dj_2d]]
     ENDFOR
     PRINT,'done!'
     IF ~KEYWORD_SET(saveFN) THEN BEGIN
        IF N_ELEMENTS(saveDir) EQ 0 THEN BEGIN
           saveDir     = './'
        ENDIF
        saveFN         = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--eSpec_' + eSpecUnits + '__and_nFlux--' + eeb_or_ees + '--orb_' + STRCOMPRESS(orb,/REMOVE_ALL) $
                 + '__' + timeRangeStr + '.sav'

        PRINT,'Saving data for energy spectrum and density to ' + saveFN + '...'
        SAVE,je_en, $
             out_mass,out_dt,e_angle,dat, $
             dn_2d,dj_2d,eSpec,diff_eflux,orb,lcw,FILENAME=saveDir + saveFN
     ENDIF

  ENDIF


END