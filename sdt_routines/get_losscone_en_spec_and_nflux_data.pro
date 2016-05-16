;2016/05/15
PRO GET_LOSSCONE_EN_SPEC_AND_NFLUX_DATA,T1=t1,T2=t2, $
                                        EN_SPEC=eSpec, $
                                        JE_EN=je_en, $
                                        OUT_ORB=orb, $
                                        ESPECUNITS=eSpecUnits, $
                                        ELECTRON_ENERGY_LIMS=energy_electrons, $
                                        SAVE_ESPEC_AND_NFLUX=save_these,$
                                        SAVEFILENAME=saveFN


  COMPILE_OPT idl2

  @startup

  IF ~KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons = [3e1,3e4]
  ENDIF

  IF N_ELEMENTS(t2) EQ 0 THEN BEGIN
     t2               = t1
  ENDIF

  CASE SIZE(t1,/TYPE) OF
     0: BEGIN
        PRINT,'t1 empty! Returning ...'
        RETURN
     END
     7: BEGIN
        t1 = STR_TO_TIME(t1)
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
        t2 = STR_TO_TIME(t2)
     END
     ELSE: BEGIN
     END
  ENDCASE
  
  IF t1 EQ t2 THEN BEGIN
     timeRangeStr     = TIME_TO_STR(t1,/MSEC,/CONV_TO_UNDERSCORE)
  ENDIF ELSE BEGIN
     timeRangeStr     = TIME_TO_STR(t1,/MSEC,/CONV_TO_UNDERSCORE) + '-' + TIME_TO_STR(t2,/MSEC,/CONV_TO_UNDERSCORE)
  ENDELSE

  IF N_ELEMENTS(eSpecUnits) EQ 0 THEN BEGIN
     eSpecUnits = 'DF'
  ENDIF

  ;;get_orbit data
  GET_FA_ORBIT,t1,t2 ;,/all
  
  ;;define loss cone angle
  GET_DATA,'ALT',data=alt
  loss_cone_alt = alt.y[0]*1000.0
  lcw = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI

  GET_DATA,'ILAT',data=ilat
  north_south = ABS(ilat.y[0])/ilat.y[0]
  
  GET_DATA,'ORBIT',data=orbit
  orb        = orbit.y[0]

  ;;Loss cone stuff
  IF north_south EQ -1 THEN BEGIN
     e_angle = [180.-lcw,180+lcw] ; for Southern Hemis.

     ;;i_angle=[270.0,90.0]	
     ;;elimnate ram from data
     i_angle = [180.0,360.0]
     i_angle_up = [270.0,360.0]
     
  ENDIF ELSE BEGIN
     e_angle = [360.-lcw,lcw]     ;	for Northern Hemis.
     ;;i_angle=[90.,270.0]
     ;;eliminate ram from data
     i_angle = [0.0,180.0]
     i_angle_up = [90.0,180.0]
     
  ENDELSE

  ;;Make the spectrogram data struct
  IF ARG_PRESENT(eSpec) OR KEYWORD_SET(save_these) THEN BEGIN
     GET_EN_SPEC,'fa_ees',UNITS=eSpecUnits,NAME='el',RETRACE=1,T1=t1,T2=t2,ANGLE=e_angle
     ;;GET the spectrogram data struct
     GET_DATA,'el',DATA=eSpec
  ENDIF
  
  ;;Make the nFlux data struct
  IF ARG_PRESENT(je_en) OR KEYWORD_SET(save_these) THEN BEGIN
     GET_2DT_TS,'j_2d_fs_en','fa_ees',T1=t1,T2=t2,NAME='Je_en',ENERGY=energy_electrons,ANGLE=e_angle
     ;;GET the spectrogram data struct
     GET_DATA,'Je_en',DATA=je_en
  ENDIF

  IF KEYWORD_SET(save_these) THEN BEGIN
     
     fu_spec2d,'n_2d_fs',dat,OUT_PARTIAL=dn_2d,ANGLE=e_angle ;,/integ_f,/integ_r ; plot partial density, partial integral densities
     fu_spec2d,'j_2d_fs',dat,OUT_PARTIAL=dj_2d,ANGLE=e_angle ;,/integ_f,/integ_r ; plot partial density, partial integral densities

     IF ~KEYWORD_SET(saveFN) THEN BEGIN
        IF N_ELEMENTS(saveDir) EQ 0 THEN saveDir = './'
        saveFN = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--eSpec_' + eSpecUnits + '__and_nFlux--' + 'orb_' + STRCOMPRESS(orb,/REMOVE_ALL) $
                 + '__' + timeRangeStr + '.sav'

        PRINT,'Saving data for energy spectrum and density to ' + saveFN + '...'
        SAVE,je_en,dn_2d,dj_2d,eSpec,orb,lcw,FILENAME=saveDir + saveFN
     ENDIF

  ENDIF


END