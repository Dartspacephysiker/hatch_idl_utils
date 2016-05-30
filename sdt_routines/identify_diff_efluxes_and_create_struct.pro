PRO IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,eSpec,Jee,Je, $
   mlt,ilat, $
   events, $
   SC_POT=sc_pot, $
   IND_SC_POT=ind_sc_pot, $
   QUIET=quiet, $
   BATCH_MODE=batch_mode, $
   ORBSTR=orbStr, $
   PRODUCE_FAILCODE_OUTPUT=produce_failCodes, $
   OUT_FAILCODES=failCodes, $
   ERRORLOGFILE=errorLogFile
   
  ;; IF KEYWORD_SET(batch_mode) THEN BEGIN
  ;;    ON_ERROR, 2
  ;; ENDIF


  ;;A little error checking
  IF NDIMEN(eSpec.v) NE 2 OR NDIMEN(eSpec.y) NE 2 THEN BEGIN
     IF KEYWORD_SET(errorLogFile) THEN BEGIN
        WRITE_MESSAGE_TO_LOGFILE,errorLogFile, $
                                 STRING(FORMAT='(A0,T20,A0,T40,A0)',KEYWORD_SET(orbStr) ? orbStr : '???', $
                                        GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                        'IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT: NDIMEN ESPEC.{v and/or y} NE 2!'), $
                                 /APPEND
     ENDIF
     RETURN
  ENDIF

  events       = !NULL
  nEvents      = N_ELEMENTS(eSpec.x)
  energies     = REFORM(eSpec.v[0,*])
  nEnergies    = N_ELEMENTS(energies)
  max_en_ind   = MAKE_ARRAY(nEvents,/INTEGER,VALUE=-2)
  CASE 1 OF
     KEYWORD_SET(sc_pot): BEGIN
        FOR i=0,nEvents-1 DO BEGIN
           tempInd       = MAX(WHERE(energies GT sc_pot[i]))
           ;; tempMax      = MAX(WHERE(
           max_en_ind[i] = tempInd
        ENDFOR
     END
     KEYWORD_SET(ind_sc_pot): BEGIN
        max_en_ind       = ind_sc_pot
     END
     ELSE:
  ENDCASE

  FOR i=0,N_ELEMENTS(eSpec.x)-1 DO BEGIN

     ;; The old way
     ;; tempeSpec = {x:eSpec.x[i],y:REVERSE(REFORM(eSpec.y[i,0:-2])),v:REVERSE(REFORM(eSpec.v[i,0:-2]))}
     ;; tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__NEWELL_ET_AL_2009(tempeSpec,Je.y[0],Jee.y[i],MLT[i])

     ;; The FAST-adjusted way
     tempeSpec = {x:eSpec.x[i],y:REVERSE(REFORM(eSpec.y[i,0:max_en_ind[i]])),v:REVERSE(REFORM(eSpec.v[i,0:max_en_ind[i]]))}
     IF KEYWORD_SET(produce_failCodes) THEN BEGIN
        tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__FAST_ADJ(tempeSpec,Je.y[i],Jee.y[i], $
                                                             mlt[i],ilat[i], $
                                                             PRODUCE_FAILCODE_OUTPUT=produce_failCodes, $
                                                             OUT_FAILCODES=tempFailCodes, $
                                                             QUIET=quiet, $
                                                             BATCH_MODE=batch_mode, $
                                                             ERRORMSG=errorMsg) ;, $

        ADD_ESPEC_FAILCODES_TO_FAILCODE_STRUCT,failCodes,tempFailCodes
     
     ENDIF ELSE BEGIN
        tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__FAST_ADJ(tempeSpec,Je.y[i],Jee.y[i], $
                                                             mlt[i],ilat[i], $
                                                             QUIET=quiet, $
                                                             BATCH_MODE=batch_mode, $
                                                             ERRORMSG=errorMsg) ;, $
        ;; ORBSTR=orbStr, $
        ;; ERRORLOGFILE=errorLogFile)
     ENDELSE

     IF SIZE(tempEvent,/TYPE) NE 8 THEN BEGIN
        WRITE_MESSAGE_TO_LOGFILE,errorLogFile, $
                                 STRING(FORMAT='(A0,T20,A0,T40,A0)',KEYWORD_SET(orbStr) ? orbStr : '???', $
                                        GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                        errorMsg), $
                                 /APPEND
     ENDIF
     ADD_EVENT_TO_SPECTRAL_STRUCT,events,tempEvent
     ;; events    = [events,tempEvent]
  ENDFOR

END