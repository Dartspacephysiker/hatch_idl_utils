;; 2018/05/07
PRO GET_FA_IESA_ION_BEAMS,time1,time2, $
                          ORBIT=orbit, $
                          NEWELL_2009_INTERP=Newell_2009_interp, $
                          ION_ANGLERANGE=ion_angleRange, $
                          ION_ENERGYRANGE=ion_energyRange, $
                          SPECTROGRAM_UNITS=spectrogram_units, $
                          EEB_OR_EES=eeb_or_ees, $
                          SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                          ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                          SC_POT=sc_pot, $
                          OUT_IONEVENTS=ionEvents, $
                          OUT_SC_POTAVG=sc_potAvg, $
                          BATCH_MODE=batch_mode
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; GET_DATA,'dB_fac_v',DATA=data
  ;; IF SIZE(data,/TYPE) NE 8 THEN $
  ;;    UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi,/QUIET

  IF N_ELEMENTS(orbit) EQ 0 THEN GET_FA_SDT_ORBIT,orbit

  orbString           = STRING(FORMAT='(I0)',orbit)

  saveIonName  = 'fa_ion_beams'
  saveIonName += '-' + orbString + (KEYWORD_SET(bonusPref) ? bonusPref : '' )

  IF N_ELEMENTS(Newell_2009_interp) GT 0 THEN BEGIN
     IF Newell_2009_interp EQ 0 THEN BEGIN
        saveIonName += '-not_Newell_interpreted'
     ENDIF
  ENDIF

  t1  = time1
  t2  = time2

  t1S = STRMID(TIME_TO_STR(time1,/MSEC),11,11)
  t2S = STRMID(TIME_TO_STR(time2,/MSEC),11,11)

  t1S = t1S.REPLACE(':', '_')
  t1S = t1S.REPLACE('.', '__')
  
  t2S = t2S.REPLACE(':', '_')
  t2S = t2S.REPLACE('.', '__')
  
  specAvgSuff = ''
  CASE 1 OF
     KEYWORD_SET(enforce_diff_eFlux_sRate): BEGIN
        specAvgSuff = (STRING(FORMAT='("-sRate",F0.2)',enforce_diff_eFlux_sRate)).Replace('.','_')
     END
     KEYWORD_SET(spectra_average_interval): BEGIN
        specAvgSuff = STRING(FORMAT='("-avgItvl",I0)',spectra_average_interval)
     END
  ENDCASE

  saveIonName += '-' + t1S + '_-_' + t2S + specAvgSuff


  IF STRUPCASE(eeb_or_ees) EQ 'EEB' THEN ieb_or_ies = 'ieb' ELSE ieb_or_ies = 'ies'


  defSpecUnits = 'eflux' 
  specUnits = KEYWORD_SET(spectrogram_units) ? spectrogram_units : defSpecUnits

  CASE STRUPCASE(specUnits) OF
     'FLUX': BEGIN
        specUnits = 'flux'
        ionSpecLogLims  = [1.,7.]
        specLogUnitsString = 'Log #!C/cm!U2!N-s-sr-eV'
        eSpecLogLims = [2.,8.]
     END
     'EFLUX': BEGIN
        specUnits = 'eflux'
        ionSpecLogLims  = [5.,9.]
        eSpecLogLims = [6.,9.]
        specLogUnitsString = 'Log eV!C/cm!U2!N-s-sr-eV'
     END
  ENDCASE

  IF N_ELEMENTS(sc_pot) EQ 0 THEN BEGIN
     GET_SC_POTENTIAL,T1=t1eeb,T2=t2eeb, $
                      DATA=sc_pot, $
                      FROM_FA_POTENTIAL=pot__from_fa_potential, $
                      ALL=pot__all, $
                      /REPAIR, $
                      CHASTON_STYLE=pot__Chaston_style, $
                      FILENAME=pot__fName, $
                      FROM_FILE=pot__from_file, $
                      ORBIT=orbit, $
                      SAVE_FILE=pot__save_file
  ENDIF

  spin_period   = 4.946         ; seconds

  ;;get_sample_rate
  sc_potRed         = STRANGEWAY_DECIMATE_AND_SMOOTH_FIELDS( $
                      sc_pot, $
                      /NO_SEPARATE_DC_AC, $
                      /DO_UNIQ_CHECK)

  sc_pot_dt         = ABS(sc_potRed.x-SHIFT(sc_potRed.x,-1))
  sc_pot_dt[0]      = sc_pot_dt[1]
  nSC_POT           = N_ELEMENTS(sc_potRed.x)
  sc_pot_dt[nSC_POT-1]  = sc_pot_dt[nSC_POT-2]

  ;;get maxima within a 1 spin window
  j_range       = WHERE(sc_potRed.x LT sc_potRed.x[N_ELEMENTS(sc_potRed.x)-1]-spin_period)
  index_max     = MAX(j_range)

  pot = MAKE_ARRAY(N_ELEMENTS(sc_potRed.x),/DOUBLE)
  FOR j=0L,index_max DO BEGIN
     spin_range = j+FINDGEN(CEIL(spin_period/SC_POT_dt[j]))
     pot[j]     = MAX(ABS(sc_potRed.y[spin_range]),ind)
     sign       = sc_potRed.y[spin_range[ind]]/ABS(sc_potRed.y[spin_range[ind]])
     pot[j]     = sign*pot[j]
  ENDFOR

  pot[index_max+1:nSC_POT-1] = pot[j_range[index_max]]
  sc_potAvg     = {x:sc_potRed.x,y:pot,type:"Chaston style"}

  IF sc_pot.isNeg THEN sc_potAvg.y *= -1.

  var_name='Iesa_Angle'
  ion_ER = KEYWORD_SET(ion_energyRange) ? ion_energyRange : [4.,30000.]
  GET_PA_SPEC,'fa_' + ieb_or_ies + '_c', $
              T1=t1, $
              T2=t2, $
              UNITS=specUnits, $
              NAME=var_name, $
              ENERGY=ion_ER, $
              /RETRACE, $
              /CALIB
  GET_DATA,var_name, DATA=data
  STORE_DATA,'IesaPASpec',DATA=data

  ;; var_name='Iesa_Energy'
  ;; GET_EN_SPEC,'fa_' + ieb_or_ies + '_c', $
  ;;             T1=t1, $
  ;;             T2=t2, $
  ;;             NAME=var_name, $
  ;;             UNITS=specUnits, $
  ;;             ANGLE=ion_angleRange, $
  ;;             /CALIB, $
  ;;             RETRACE=1
  ;; GET_DATA,var_name,DATA=data
  ;; IesaLCSpec = data


  ionBeam_aRange = ion_angleRange
  IF ion_angleRange[0] GE 90. AND ion_angleRange[0] LE 180 THEN BEGIN
     ;; NH
     ionBeam_aRange[0] = 180.-(180.-ion_angleRange[0])/2.
     ionBeam_aRange[1] = 180.+(180.-ion_angleRange[0])/2.
     ion_halfRange     = [90,270]
  ENDIF 
  IF ion_angleRange[0] GE 270. AND ion_angleRange[0] LE 360. THEN BEGIN
     ;; SH
     ionBeam_aRange[0] = 360.-(360.-ion_angleRange[0])/2.
     ionBeam_aRange[1] = (360.-ion_angleRange[0])/2.
     ion_halfRange     = [270,90]
  ENDIF
  iAngle       = KEYWORD_SET(ion_angleRange     ) ? ion_angleRange      : [135.,225.]
  iAngleChari  = iAngle

  var_name = "IesaTightEnSpec"
  GET_EN_SPEC,'fa_' + ieb_or_ies + '_c', $
              T1=t1, $
              T2=t2, $
              NAME=var_name, $
              UNITS=specUnits, $
              ANGLE=ionBeam_aRange, $
              /CALIB, $
              RETRACE=1
  var_name = "IesaHalfRangeEnSpec"
  GET_EN_SPEC,'fa_' + ieb_or_ies + '_c', $
              T1=t1, $
              T2=t2, $
              NAME=var_name, $
              UNITS=specUnits, $
              ANGLE=ion_halfRange, $
              /CALIB, $
              RETRACE=1

  var_name='IesaPASpec'
  GET_DATA,var_name, DATA=paspec
  var_name='IesaTightEnSpec'
  GET_DATA,var_name, DATA=enspec
  var_name='IesaHalfRangeEnSpec'
  GET_DATA,var_name,DATA=IesaHRSpec

  compSpec = IesaHRSpec

  t1eeb = 0.D 
  t2eeb = 0.D
  bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t1eeb,/ST)
  bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t2eeb,/EN)
  t1eeb = time1 > t1eeb
  t2eeb = time2 < t2eeb

  GET_2DT_TS_POT,'j_2d_fs','fa_' + ieb_or_ies, $
                 NAME='JiTight', $
                 T1=t1eeb,T2=t2eeb, $
                 ENERGY=[0.,ion_ER[1]], $
                 ANGLE=ionBeam_aRange, $
                 SC_POT={x:sc_potAvg.x,y:sc_potAvg.y*(-1.)}, $ ;This routine expects pot values to be the negative of their actual value
                 /CALIB
  GET_2DT_TS_POT,'je_2d_fs','fa_' + ieb_or_ies, $
                 NAME='JeiTight', $
                 T1=t1eeb,T2=t2eeb, $
                 ENERGY=[0.,ion_ER[1]], $
                 ANGLE=ionBeam_aRange, $
                 SC_POT={x:sc_potAvg.x,y:sc_potAvg.y*(-1.)}, $ ;This routine expects pot values to be the negative of their actual value
                 /CALIB

  GET_DATA,'JiTight',DATA=jiTight
  GET_DATA,'JeiTight',DATA=jeiTight
  chari   = jeiTight.y/jiTight.y*6.242*1.0e11

  this           = VALUE_CLOSEST2(enspec.x,jeiTight.x,/CONSTRAINED) 
  enspec         = {x:enspec.x[this],y:enspec.y[this,*],v:enspec.v[this,*]}
  this           = VALUE_CLOSEST2(compSpec.x,jeiTight.x,/CONSTRAINED) 
  compSpec     = {x:compSpec.x[this],y:compSpec.y[this,*],v:compSpec.v[this,*]}

  that           = VALUE_CLOSEST2(sc_potAvg.x,jeiTight.x,/CONSTRAINED) 
  sc_potAvgIn    = sc_potAvg.y[that]

  GET_FA_ORBIT,enspec.x,/TIME_ARRAY,/NO_STORE,STRUC=ionEphem
  IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,enspec,jeiTight,jiTight, $
                                          ionEphem.mlt,ionEphem.ilat,ionEphem.alt,ionEphem.orbit, $
                                          ionEvents, $
                                          BATCH_MODE=batch_mode, $
                                          SC_POT=sc_potAvgIn, $
                                          /IS_ION, $
                                          ION_HALFRANGE_SPEC=compSpec, $
                                          /QUIET

  ion_erArr = TRANSPOSE([[REPLICATE(ion_er[0],N_ELEMENTS(chari))], $
                         [REPLICATE(ion_er[1],N_ELEMENTS(chari))]])
  ion_erArr[0,*] = ion_erArr[0,*] > (sc_potAvgIn*(-1.))
  ionEvents = {pa     : paspec, $
               en     : enspec, $
               ji     : jiTight, $
               jei    : jeiTight, $
               chari  : chari, $
               newell : ionEvents, $
               arange : ionBeam_aRange, $
               erange : ion_erArr}

  ;; SAVE,ionEvents,FILENAME='/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/orb1694_iondata.sav'

END
