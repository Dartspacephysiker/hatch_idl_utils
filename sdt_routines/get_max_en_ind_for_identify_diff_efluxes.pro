;2019/05/08
PRO GET_MAX_EN_IND_FOR_IDENTIFY_DIFF_EFLUXES, $
   eSpec, $
   SC_POT=sc_pot, $
   IND_SC_POT=ind_sc_pot, $
   OUT_MAX_EN_IND=max_en_ind, $
   PEAKFLUX_ARR=peakFlux_arr, $
   PEAK_ENERGY_ARR=peak_energy_arr, $
   GOODNOKFLUX_ARR=goodNokFlux_arr, $
   ION_HR_SPEC=ion_HR_spec, $
   BEAMHALFRATIO=beamHalfRatio

   COMPILE_OPT IDL2,STRICTARRSUBS

  split_interval     = 5000

  events       = !NULL
  nEvents      = N_ELEMENTS(eSpec.x)
  energies     = REFORM(eSpec.v[0,*]) ;;Note, energies are reversed so that low energies are at the highest indices!!
  nEnergies    = N_ELEMENTS(energies)
  max_en_ind   = MAKE_ARRAY(nEvents,/INTEGER,VALUE=-2)
  CASE 1 OF
     KEYWORD_SET(sc_pot): BEGIN

        ;; Ions are attracted to neg potential, so use neg of potential as threshold
        ;; elecs are attracted to pos potential, so use       potential as threshold
        IF KEYWORD_SET(is_ion) THEN BEGIN
           potFac = (-1.)
        ENDIF ELSE BEGIN
           potFac = 1.
        ENDELSE

        FOR i=0,nEvents-1 DO BEGIN
           tempInd       = MAX(WHERE(energies GT (sc_pot[i]*potFac))) ;;Note (as above) that lowest energies are at HIGH indices!

           ;; tempMax      = MAX(WHERE(
           max_en_ind[i] = tempInd
        ENDFOR
     END
     KEYWORD_SET(ind_sc_pot): BEGIN
        max_en_ind       = ind_sc_pot
     END
     ELSE:
  ENDCASE



  peakFlux_arr    = MAKE_ARRAY(nEvents,/DOUBLE,VALUE=-999.)
  peak_energy_arr = MAKE_ARRAY(nEvents,/DOUBLE,VALUE=-999.)
  goodNokFlux_arr = MAKE_ARRAY(nEvents,/BYTE,VALUE=-999.)

  ;; NESTE
  FOR i=0,nEvents-1 DO BEGIN

     ;; IF ABS(eSpec.x[i]-854157804.36D) LT 1 THEN STOP

     IF KEYWORD_SET(give_timesplit_info) AND ( (i MOD split_interval) EQ 0 ) THEN BEGIN
        clock     = TIC("eSpecs_"+STRCOMPRESS(i,/REMOVE_ALL) +  '-' + STRCOMPRESS(i+split_interval-1,/REMOVE_ALL))
        ;; doClock   = 1
     ENDIF

     ;; The FAST-adjusted way
     tempeSpec = {x:eSpec.x[i], $
                  y:REVERSE(REFORM(eSpec.y[i,0:max_en_ind[i]])), $
                  v:REVERSE(REFORM(eSpec.v[i,0:max_en_ind[i]]))}
     IF KEYWORD_SET(is_ion) THEN BEGIN
        tempHRSpec = {x:ion_HR_spec.x[i], $
                      y:REVERSE(REFORM(ion_HR_spec.y[i,0:max_en_ind[i]])), $
                      v:REVERSE(REFORM(ion_HR_spec.v[i,0:max_en_ind[i]]))}
     ENDIF


     energy_e  = tempeSpec.v
     spec_e    = tempeSpec.y

     peakFlux    = MAX(spec_e,peakFlux_ind)
     peak_energy = energy_e[peakFlux_ind]
     goodNokFlux = (spec_e[peakFlux_ind]/ion_HR_spec.y[peakFlux_ind]) GE beamHalfRatio ;Samme som i diff_energy_flux_spectral_type__fast_adj.pro

     peakFlux_arr[i]    = peakFlux
     peak_energy_arr[i] = peak_energy
     goodNokFlux_arr[i] = goodNokFlux

  ENDFOR

END