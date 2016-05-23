PRO IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,eSpec,Jee,Je, $
   mlt,ilat, $
   events, $
   SC_POT=sc_pot, $
   QUIET=quiet, $
   BATCH_MODE=batch_mode, $
   ORBSTR=orbStr, $
   ERRORLOGFILE=errorLogFile
   
  IF KEYWORD_SET(batch_mode) THEN BEGIN
     ON_ERROR, 2
  ENDIF


  events       = !NULL
  nEvents      = N_ELEMENTS(eSpec.x)
  energies     = REFORM(eSpec.v[0,*])
  nEnergies    = N_ELEMENTS(energies)
  max_en_ind   = MAKE_ARRAY(nEvents,/INTEGER,VALUE=-2)
  IF KEYWORD_SET(sc_pot) THEN BEGIN
     FOR i=0,nEvents-1 DO BEGIN
        tempInd      = MAX(WHERE(energies GT sc_pot[i]))
        ;; tempMax      = MAX(WHERE(
       max_en_ind[i] = tempInd
     ENDFOR
  ENDIF
  ;; IF KEYWORD_SET(out_sc_min_energy_ind) THEN BEGIN
  ;;    max_en_ind = out_sc_min_energy_ind
  ;; ENDIF

  FOR i=0,N_ELEMENTS(eSpec.x)-1 DO BEGIN

     ;; The old way
     ;; tempeSpec = {x:eSpec.x[i],y:REVERSE(REFORM(eSpec.y[i,0:-2])),v:REVERSE(REFORM(eSpec.v[i,0:-2]))}
     ;; tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__NEWELL_ET_AL_2009(tempeSpec,Je.y[0],Jee.y[i],MLT[i])

     ;; The FAST-adjusted way
     tempeSpec = {x:eSpec.x[i],y:REVERSE(REFORM(eSpec.y[i,0:max_en_ind[i]])),v:REVERSE(REFORM(eSpec.v[i,0:max_en_ind[i]]))}
     tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__FAST_ADJ(tempeSpec,Je.y[0],Jee.y[i], $
                                                          mlt[i],ilat[i], $
                                                          QUIET=quiet, $
                                                          BATCH_MODE=batch_mode, $
                                                          ORBSTR=orbStr, $
                                                          ERRORLOGFILE=errorLogFile)
     ADD_EVENT_TO_SPECTRAL_STRUCT,events,tempEvent
     ;; events    = [events,tempEvent]
  ENDFOR

END