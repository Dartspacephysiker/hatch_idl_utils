;; 2018/05/07
PRO GET_FA_IESA_ION_BEAMS,time1,time2, $
                          USEDIFFEFLUX=useDiffEflux, $
                          IONDIFFEFLUX=ion_dEF, $
                          MCFADDEN_STYLE_DIFF_EFLUX=McFadden_diff_eFlux, $
                          ORBIT=orbit, $
                          THRESH_BEAM_EFLUX=thresh_beam_eFlux, $
                          ;; NEWELL_2009_INTERP=Newell_2009_interp, $
                          ION_ANGLERANGE=ion_angleRange, $
                          ION_ENERGYRANGE=ion_energyRange, $
                          SPECTROGRAM_UNITS=spectrogram_units, $
                          EEB_OR_EES=eeb_or_ees, $
                          SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                          ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
                          SC_POT=sc_pot, $
                          OUT_IONEVENTS=ionEvents, $
                          OUT_SC_POTAVG=sc_potAvg, $
                          BATCH_MODE=batch_mode, $
                          USEPEAKENERGY=usePeakEnergy, $
                          EPHEMSTRUCT=ephemStruct, $
                          MAKE_TPLOT=make_tplot, $
                          SAVE_PS=save_ps

  makeZeroThreshEFlux = KEYWORD_SET(thresh_beam_eFlux) ? thresh_beam_eFlux : 5e4
  makeZeroVal = 0.001
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; GET_DATA,'dB_fac_v',DATA=data
  ;; IF SIZE(data,/TYPE) NE 8 THEN $
  ;;    UCLA_MAG_DESPIN,TW_MAT=tw_mat,ORBIT=orbit,SPIN_AXIS=spin_axis,DELTA_PHI=delta_phi,/QUIET

  min_if_nan_scpots = 10.

  IF N_ELEMENTS(orbit) EQ 0 THEN GET_FA_SDT_ORBIT,orbit

  orbString           = STRING(FORMAT='(I0)',orbit)

  saveIonName  = 'fa_ion_beams'
  saveIonName += '-' + orbString + (KEYWORD_SET(bonusPref) ? bonusPref : '' )

  paSpecName = 'IesaPASpec'
  tightEnSpecName = 'IesaTightEnSpec'
  halfEnSpecName = 'IesaHalfRangeEnSpec'

  ;; IF N_ELEMENTS(Newell_2009_interp) GT 0 THEN BEGIN
  ;;    IF Newell_2009_interp EQ 0 THEN BEGIN
  ;;       saveIonName += '-not_Newell_interpreted'
  ;;    ENDIF
  ;; ENDIF

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
     ELSE: BEGIN
        specAvgSuff = ''
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
     spin_range = j+FINDGEN(CEIL(spin_period/SC_POT_dt[j])) & spin_range = ROUND(spin_range) < index_max
     pot[j]     = MAX(ABS(sc_potRed.y[spin_range]),ind)
     sign       = sc_potRed.y[spin_range[ind]]/ABS(sc_potRed.y[spin_range[ind]])
     pot[j]     = sign*pot[j]
  ENDFOR

  pot[index_max+1:nSC_POT-1] = pot[j_range[index_max]]
  sc_potAvg     = {x:sc_potRed.x,y:pot,type:"Chaston style"}

  IF sc_pot.isNeg THEN BEGIN
     sc_potAvg.y *= -1.
     sc_pot.isNeg = 0B
  ENDIF

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
  STORE_DATA,paSpecName,DATA=data

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
  ion_halfRange = ion_angleRange*0.
  IF NDIMEN(ionBeam_aRange) EQ 2 THEN BEGIN

     nHere = N_ELEMENTS(ionBeam_aRange[0,*])
     FOR dee=0,nHere-1 DO BEGIN

        IF ion_angleRange[0,dee] GE 90. AND ion_angleRange[0,dee] LE 180 THEN BEGIN
           ;; NH
           ionBeam_aRange[0,dee] = 180.-(180.-ion_angleRange[0,dee])/2.
           ionBeam_aRange[1,dee] = 180.+(180.-ion_angleRange[0,dee])/2.
           ion_halfRange[*,dee]  = [90,270]
        ENDIF 
        IF ion_angleRange[0] GE 270. AND ion_angleRange[0] LE 360. THEN BEGIN
           ;; SH
           ionBeam_aRange[0,dee] = 360.-(360.-ion_angleRange[0,dee])/2.
           ionBeam_aRange[1,dee] = (360.-ion_angleRange[0,dee])/2.
           ion_halfRange[*,dee] = [270,90]
        ENDIF

     ENDFOR

  ENDIF ELSE BEGIN

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

  ENDELSE
  ;; iAngle       = KEYWORD_SET(ion_angleRange     ) ? ion_angleRange      : [135.,225.]
  ;; iAngleChari  = iAngle

  var_name=paSpecName
  GET_DATA,var_name, DATA=paspec

  t1eeb = 0.D 
  t2eeb = 0.D
  bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t1eeb,/ST)
  bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t2eeb,/EN)
  t1eeb = time1 > t1eeb
  t2eeb = time2 < t2eeb

  IF KEYWORD_SET(useDiffEflux) THEN BEGIN

     deFlux__array_of_structs  = KEYWORD_SET(McFadden_diff_eFlux)

     varName = tightEnSpecName
     enspec = GET_EN_SPEC__FROM_DIFF_EFLUX( $
              ion_dEF, $
              T1=t1, $
              T2=t2, $
              /RETRACE, $
              ANGLE=ionBeam_aRange, $
              UNITS=specUnits, $
              NAME=varName, $
              OUT_AVGFACTORARR=avgFactorArr, $
              OUT_NORMARR=normArr, $
              BAD_TIME=bad_time1, $
              OUT_TIME=out_time, $
              IS_MCFADDEN_DIFF_EFLUX=deFlux__array_of_structs, $
              QUIET=quiet)

     ;; Try zis
     badFruks = WHERE(enspec.y LT makeZeroThreshEFlux)
     IF badFruks[0] NE -1 THEN BEGIN
        enspec.y[badFruks] = makeZeroVal
     ENDIF

     STORE_DATA,varName,DATA=enspec

     varName = halfEnSpecName
     IesaHRSpec = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                  ion_dEF, $
                  T1=t1, $
                  T2=t2, $
                  /RETRACE, $
                  ANGLE=ion_halfRange, $
                  UNITS=specUnits, $
                  NAME=varName, $
                  OUT_AVGFACTORARR=avgFactorArr, $
                  OUT_NORMARR=normArr, $
                  BAD_TIME=bad_time2, $
                  OUT_TIME=out_time, $
                  IS_MCFADDEN_DIFF_EFLUX=deFlux__array_of_structs, $
                  /QUIET)
     STORE_DATA,varName,DATA=IesaHRSpec


     IF KEYWORD_SET(usePeakEnergy) THEN BEGIN

        ;; this         = WHERE((bad_time1 EQ 1) OR (bad_time1 EQ 0))
        this = VALUE_CLOSEST2(enspec.x, $
                              ion_dEF.time $
                              +(ion_dEF.end_time-ion_dEF.time)/2., $
                              /CONSTRAINED)

        GET_PEAK_ENERGY_FROM_DIFF_EFLUX_AND_ESPEC, $
           ion_dEF, {x:enspec.x[this], $
                     y:enspec.y[this,*], $
                     v:enspec.v[this,*], $
                     yerr:enspec.yerr[this,*], $
                     verr:enspec.verr[this,*]}, $
           PEAKE_BOUNDS_INDSHIFT=peakE_bounds_indShift, $
           OUT_PEAK_INDARR=peak_indArr, $
           OUT_PEAK_ENERGYARR=peak_energyArr, $
           OUT_PEAK_DEARR=peak_dEArr, $
           OUT_PEAK_EBOUNDSARR=peak_EBoundsArr, $
           OUT_PEAKE_INDSHIFT=peakE_indShift

        enBounds = peak_eBoundsArr

     ENDIF ELSE BEGIN

        enBounds = [0.,ion_ER[1]]

     ENDELSE

     enBeam   = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(ion_dEF, $
                                                   ENERGY=enBounds, $
                                                   SC_POT=sc_pot, $
                                                   MIN_IF_NAN_SCPOTS=min_if_nan_scpots, $
                                                   EEB_OR_EES=eeb_or_ees)

     MOMENT_SUITE_2D,ion_dEF, $
                     ;; ENERGY=[0.,ion_ER[1]], $
                     ENERGY=enBeam, $
                     ARANGE__MOMENTS=ionBeam_aRange, $
                     SC_POT=sc_pot, $
                     EEB_OR_EES=ieb_or_ies, $
                     /ERROR_ESTIMATES, $
                     ;; MAP_TO_100KM=map_to_100km, $ 
                     ORBIT=orbit, $
                     /NEW_MOMENT_ROUTINE, $
                     /QUIET, $
                     OUTTIME=time, $
                     OUT_N=n, $
                     OUT_J_=j, $
                     OUT_JE=je, $
                     OUT_T=T, $
                     OUT_CHARE=charE, $
                     OUT_CURRENT=cur, $
                     OUT_JJE_COVAR=jje_coVar, $
                     OUT_ERRORS=errors, $
                     OUT_ERR_N=nErr, $
                     OUT_ERR_J_=jErr, $
                     OUT_ERR_JE=jeErr, $
                     OUT_ERR_T=TErr, $
                     OUT_ERR_CURRENT=curErr, $
                     OUT_ERR_CHARE=charEErr, $
                     INOUT_MAPRATIO=mapRatio, $
                     OUT_STRUCT=ionMomStructBeam, $
                     BATCH_MODE=batch_mode, $
                     MCFADDEN_STYLE_DIFF_EFLUX=deFlux__array_of_structs

     enBounds = [0.,ion_ER[1]]
     enAll    = MAKE_ENERGY_ARRAYS__FOR_DIFF_EFLUX(ion_dEF, $
                                                   ENERGY=enBounds, $
                                                   SC_POT=sc_pot, $
                                                   MIN_IF_NAN_SCPOTS=min_if_nan_scpots, $
                                                   EEB_OR_EES=eeb_or_ees)

     MOMENT_SUITE_2D,ion_dEF, $
                     ;; ENERGY=[0.,ion_ER[1]], $
                     ENERGY=enAll, $
                     ARANGE__MOMENTS=ionBeam_aRange, $
                     SC_POT=sc_pot, $
                     EEB_OR_EES=ieb_or_ies, $
                     ;; /ERROR_ESTIMATES, $ ; Don't need these, vil jeg tro
                     ;; MAP_TO_100KM=map_to_100km, $ 
                     ORBIT=orbit, $
                     /NEW_MOMENT_ROUTINE, $
                     /QUIET, $
                     OUTTIME=time, $
                     OUT_N=n, $
                     OUT_J_=j, $
                     OUT_JE=je, $
                     OUT_T=T, $
                     OUT_CHARE=charE, $
                     OUT_CURRENT=cur, $
                     OUT_JJE_COVAR=jje_coVar, $
                     OUT_ERRORS=errors, $
                     OUT_ERR_N=nErr, $
                     OUT_ERR_J_=jErr, $
                     OUT_ERR_JE=jeErr, $
                     OUT_ERR_T=TErr, $
                     OUT_ERR_CURRENT=curErr, $
                     OUT_ERR_CHARE=charEErr, $
                     INOUT_MAPRATIO=mapRatio, $
                     OUT_STRUCT=ionMomStruct, $
                     BATCH_MODE=batch_mode, $
                     MCFADDEN_STYLE_DIFF_EFLUX=deFlux__array_of_structs


     flipMe = WHERE(FINITE(ionMomStructBeam.j),nFlip,/NULL)

     IF nFlip GT 0 THEN BEGIN

        ionMomStruct.j[flipMe] = -1. * ionMomStruct.j[flipMe]
        ionMomStruct.je[flipMe] = -1. * ionMomStruct.je[flipMe]
        ionMomStruct.cur[flipMe] = -1. * ionMomStruct.cur[flipMe] ;Should this be flipped?

        ionMomStructBeam.j[flipMe] = -1. * ionMomStructBeam.j[flipMe]
        ionMomStructBeam.je[flipMe] = -1. * ionMomStructBeam.je[flipMe]
        ionMomStructBeam.cur[flipMe] = -1. * ionMomStructBeam.cur[flipMe] ;Should this be flipped?

     ENDIF

     ;; Use moments that are based on energies between S/C pot and top of
     ;; detector.
     ;;
     ;; The ones that use 'enBeam' are those corresponding to the accelerated
     ;; population, which instead have a lower-energy bound that is AT the identified peak

     jiTight = {x: ionMomStruct.time, $
                y: ionMomStruct.j}

     jeiTight = {x: ionMomStruct.time, $
                 y: ionMomStruct.je}


  ENDIF ELSE BEGIN

     var_name = tightEnSpecName
     GET_EN_SPEC,'fa_' + ieb_or_ies + '_c', $
                 T1=t1, $
                 T2=t2, $
                 NAME=var_name, $
                 UNITS=specUnits, $
                 ANGLE=ionBeam_aRange, $
                 /CALIB, $
                 RETRACE=1
     var_name = halfEnSpecName
     GET_EN_SPEC,'fa_' + ieb_or_ies + '_c', $
                 T1=t1, $
                 T2=t2, $
                 NAME=var_name, $
                 UNITS=specUnits, $
                 ANGLE=ion_halfRange, $
                 /CALIB, $
                 RETRACE=1

     var_name=tightEnSpecName
     GET_DATA,var_name, DATA=enspec
     var_name=halfEnSpecName
     GET_DATA,var_name,DATA=IesaHRSpec

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

  ENDELSE

  chari   = jeiTight.y/jiTight.y*6.242*1.0e11
  compSpec = IesaHRSpec

  ;; this           = VALUE_CLOSEST2(enspec.x,jeiTight.x,/CONSTRAINED)
  this         = WHERE((bad_time1 EQ 1) OR (bad_time1 EQ 0))
  enspec       = {x:enspec.x[this], $
                  y:enspec.y[this,*], $
                  v:enspec.v[this,*], $
                  yerr:enspec.yerr[this,*], $
                  verr:enspec.verr[this,*]}
  ;; this           = VALUE_CLOSEST2(compSpec.x,jeiTight.x,/CONSTRAINED) 
  this         = WHERE((bad_time2 EQ 1) OR (bad_time2 EQ 0))
  compSpec     = {x:compSpec.x[this], $
                  y:compSpec.y[this,*], $
                  v:compSpec.v[this,*], $
                  yerr:compspec.yerr[this,*], $
                  verr:compspec.verr[this,*]}

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

  IF KEYWORD_SET(useDiffEflux) THEN BEGIN
     ionEvents = {pa         : paspec, $
                  en         : enspec, $
                  ;; ji     : jiTight, $
                  ;; jei    : jeiTight, $
                  ;; chari  : chari, $
                  momsAll    : ionMomStruct, $
                  erangeAll  : ion_erArr, $
                  momsBeam   : ionMomStructBeam, $
                  erangeBeam : enBeam, $
                  arange     : ionBeam_aRange, $
                  newell     : ionEvents}
  ENDIF ELSE BEGIN
     ionEvents = {pa     : paspec, $
                  en     : enspec, $
                  ji     : jiTight, $
                  jei    : jeiTight, $
                  chari  : chari, $
                  newell : ionEvents, $
                  arange : ionBeam_aRange, $
                  erange : ion_erArr}

  ENDELSE

  IF KEYWORD_SET(usePeakEnergy) THEN BEGIN

     peakEStruct = {ind   : peak_indArr, $
                    energy : peak_energyArr, $
                    de     : peak_dEArr, $
                    eBounds : peak_EBoundsArr, $
                    indShift : peakE_indShift}
     ionEvents = CREATE_STRUCT(ionEvents,'peakE',peakEStruct)
  ENDIF

  IF KEYWORD_SET(make_tplot) THEN BEGIN

     savePref = "orb_" + STRING(FORMAT='(I0)',orbit)+"-beam_vs_up_ratios"$
                +specAvgSuff
     saveSuff = ".sav"

     IF N_ELEMENTS(ephemStruct) EQ 0 THEN BEGIN
        GET_FA_ORBIT,ion_dEF.time,/TIME_ARRAY,/ALL,STRUC=ephemStruct
     ENDIF

     ;; get_data,'ILAT',data=ILAT
     ;; get_data,'MLT',data=MLT

     ;; if (n_elements(ILAT.y) LE 0) then return

     plot_ascN = 1
     plot_descN = 1
     plot_ascS = 1
     plot_descS = 1

     GET_N_S_ASCENDING_DESCENDING_TIME_LIMITS, $
        {x:ephemStruct.time,y:ephemStruct.ilat}, $
        TLIMN=tLimN, $
        TLIMASCENDN=tLimNAscend, $
        TLIMDESCENDN=tLimNDescend, $
        TLIMS=tLimS, $
        TLIMASCENDS=tLimSAscend, $
        TLIMDESCENDS=tLimSDescend, $
        NN=nN, $
        NASCENDN=nNAscend, $
        NDESCENDN=nNDescend, $
        NS=nS, $
        NORTHI=northI, $
        SOUTHI=southI, $
        NASCENDS=nSAscend, $
        NDESCENDS=nSDescend, $
        SAVETSTRN=saveTStrN, $
        SAVETSTRS=saveTStrS, $
        SAVETSTRASCENDN=saveTStrNAscend, $
        SAVETSTRDESCENDN=saveTStrNDescend, $
        SAVETSTRASCENDS=saveTStrSAscend, $
        SAVETSTRDESCENDS=saveTStrSDescend

     fName = savePref+saveSuff

     IF N_ELEMENTS(saveTStrN) GT 0 THEN BEGIN
        fNameN = savePref+'-'+saveTStrN+saveSuff
        ;; fNameN = savePref+saveSuff
        ;; PRINT,fNameN
     ENDIF
     IF N_ELEMENTS(saveTStrNAscend) GT 0 THEN BEGIN
        fNameNAscend  = savePref+'-NASC-'+saveTStrNAscend+saveSuff
        ;; PRINT,fNameNAscend
     ENDIF
     IF N_ELEMENTS(saveTStrNDescend) GT 0 THEN BEGIN
        fNameNDescend = savePref+'-NDESC-'+saveTStrNDescend+saveSuff
        ;; PRINT,fNameNDescend
     ENDIF
     IF N_ELEMENTS(saveTStrS) GT 0 THEN BEGIN
        fNameS = savePref+'-'+saveTStrS+saveSuff
        ;; PRINT,fNameS
     ENDIF
     IF N_ELEMENTS(saveTStrSAscend) GT 0 THEN BEGIN
        fNameSAscend  = savePref+'-SASC-'+saveTStrSAscend+saveSuff
        ;; PRINT,fNameSAscend
     ENDIF
     IF N_ELEMENTS(saveTStrSDescend) GT 0 THEN BEGIN
        fNameSDescend = savePref+'-SDESC-'+saveTStrSDescend+saveSuff
        ;; PRINT,fNameSDescend
     ENDIF

     ;; setup plots
     varName = tightEnSpecName
     ;; STORE_DATA,varName,DATA=eSpecUp
     OPTIONS,varName,'spec',1
     ZLIM,varName,1e4,1e8,1
     ylim,varName,4,24000,1
     OPTIONS,varName,'ytitle','Beam ions!C!CEnergy (eV)'
     OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
     OPTIONS,varName,'x_no_interp',1
     OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',typiskPanelSize
     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

     varName = halfEnSpecName
     ;; STORE_DATA,varName,DATA=eSpecDown
     OPTIONS,varName,'spec',1
     zlim,varName,1e4,1e8,1
     ylim,varName,4,24000,1
     OPTIONS,varName,'ytitle','Upward ions !C!CEnergy (eV)'
     OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
     OPTIONS,varName,'x_no_interp',1
     OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',typiskPanelSize
     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

     ;; varName = allAngleVarNameN
     ;; OPTIONS,varName,'spec',1
     ;; zlim,varName,1e4,1e8,1
     ;; ylim,varName,3,40000,1
     ;; OPTIONS,varName,'ytitle','ions (all angles)!C!CEnergy (eV)'
     ;; OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
     ;; OPTIONS,varName,'x_no_interp',1
     ;; OPTIONS,varName,'y_no_interp',1
     ;; OPTIONS,varName,'panel_size',typiskPanelSize
     ;; IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

     ;; varName = "ratioN"
     ;; data = upAllRatioSpecN
     ;; STORE_DATA,varName,DATA=data
     ;; OPTIONS,varName,'spec',1
     ;; YLIM,varName,4,24000,1
     ;; ZLIM,varName,0.1,100,1
     ;; OPTIONS,varName,'ytitle',"Ion energy (eV)"
     ;; OPTIONS,varName,'ztitle','Up/all-angle ion eFlux'
     ;; OPTIONS,varName,'x_no_interp',1
     ;; OPTIONS,varName,'y_no_interp',1

     ;; IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

     beamHalfRatioSpec = {x : enspec.x, $
                          y : enspec.y / IesaHRSpec.y, $
                          v : enspec.v}

     beamHalfRatioSpecVarName = "ratioBeamHalf"
     varName = beamHalfRatioSpecVarName
     data = beamHalfRatioSpec
     STORE_DATA,varName,DATA=data
     OPTIONS,varName,'spec',1
     YLIM,varName,4,24000,1
     ZLIM,varName,0.1,1000,1
     OPTIONS,varName,'ytitle',"Ion energy (eV)"
     OPTIONS,varName,'ztitle','Beam/Up Ion Ratio'
     OPTIONS,varName,'x_no_interp',1
     OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',typiskPanelSize

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

     hvit             = 255
     peakEVarName = "peakE"
     potLStyle = 0              ;solid
     potColor  = hvit
     STORE_DATA,peakEVarName,DATA={x:ionMomStructBeam.time, $
                                    y:peak_energyArr}
     OPTIONS,peakEVarName,'LINESTYLE',potLStyle
     OPTIONS,peakEVarName,'colors',potColor
     OPTIONS,peakEVarName,'thick',3.0

     ;; eBoundLowVarName = "eBoundLow"
     ;; potLStyle = 0              ;solid
     ;; potColor  = 40
     ;; STORE_DATA,eBoundLowVarName,DATA={x:eBound.x,y:eBound.yLow}
     ;; OPTIONS,eBoundLowVarName,'LINESTYLE',potLStyle
     ;; OPTIONS,eBoundLowVarName,'colors',potColor
     ;; OPTIONS,eBoundLowVarName,'thick',3.0

     varName = "ion_beamJ"
     STORE_DATA,varName,DATA={x:ionMomStructBeam.time, $
                              y:ionMomStructBeam.j}
     ;; YLIM,varName,6e6,6e9,1
     OPTIONS,varName,'ytitle','Ion beam flux!C!C#/cm!U2!N-s'
     ;; OPTIONS,varName,'ztitle','eV/cm!U2!N-s-sr-eV'
     OPTIONS,varName,'x_no_interp',1
     OPTIONS,varName,'y_no_interp',1
     OPTIONS,varName,'panel_size',1
     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=[varName] ELSE tPlt_vars=[varName,tPlt_vars]

     timeBars = 1
     timeBar_from_ion_beams = 1

     TPLOT_BEAM_VS_HALFRANGE_ION_FLUXES, $
        tPlt_vars, $
        PEAKEVARNAME=peakEVarName, $
        ;; EBOUNDLOWVARNAME=eBoundLowVarName, $
        BEAMHALFRATIOSPECVARNAME=beamHalfRatioSpecVarName, $
        PLOT_ASCENDING_NORTH=plot_ascN, $
        PLOT_ASCENDING_SOUTH=plot_ascS, $
        PLOT_DESCENDING_NORTH=plot_descN, $
        PLOT_DESCENDING_SOUTH=plot_descS, $
        TLIMASCENDN=tLimNAscend, $
        TLIMDESCENDN=tLimNDescend, $
        TLIMS=tLimS, $
        TLIMASCENDS=tLimSAscend, $
        TLIMDESCENDS=tLimSDescend, $
        FNAMENASCEND=fNameNAscend, $
        FNAMENDESCEND=fNameNDescend, $
        FNAMESASCEND=fNameSAscend, $
        FNAMESDESCEND=fNameSDescend, $
        SAVETSTRN=saveTStrN, $
        SAVETSTRS=saveTStrS, $
        SAVETSTRASCENDN=saveTStrNAscend, $
        SAVETSTRDESCENDN=saveTStrNDescend, $
        SAVETSTRASCENDS=saveTStrSAscend, $
        SAVETSTRDESCENDS=saveTStrSDescend, $
        SAVE_PS=save_ps, $
        MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
        SAVEPREF=savePref, $
        TIMEBARS=timeBars, $
        TIMEBAR_FROM_ION_BEAMS=timeBar_from_ion_beams, $
        IONEVENTS=ionEvents

  ENDIF

  ;; SAVE,ionEvents,FILENAME='/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/orb1694_iondata.sav'

END
