PRO IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,eSpec,Jee,Je, $
   mlt,ilat, $
   events_final, $
   SC_POT=sc_pot, $
   IND_SC_POT=ind_sc_pot, $
   ORBSTR=orbStr, $
   PRODUCE_FAILCODE_OUTPUT=produce_failCodes, $
   OUT_FAILCODES=failCodes, $
   SAVE_CHUNKS_FOR_SPEED=save_chunks_for_speed, $
   CHUNK_SAVE_INTERVAL=chunk_save_interval, $
   CHUNK_SAVEFILE_PREF=chunk_saveFile_pref, $
   CHUNKDIR=chunkDir, $
   GIVE_TIMESPLIT_INFO=give_timesplit_info, $
   QUIET=quiet, $
   BATCH_MODE=batch_mode, $
   ERRORLOGFILE=errorLogFile
   
  COMPILE_OPT idl2

  ;; IF KEYWORD_SET(batch_mode) THEN BEGIN
  ;;    ON_ERROR, 2
  ;; ENDIF

  IF KEYWORD_SET(give_timesplit_info) THEN split_interval     = 5000

  IF SIZE(Jee,/TYPE) EQ 8 THEN jee_vars = Jee.y ELSE jee_vars = jee
  IF SIZE(Je,/TYPE)  EQ 8 THEN je_vars  = Je.y  ELSE je_vars  = je

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

  ;; doClock         = 0     ;used in the loop
  IF KEYWORD_SET(give_timesplit_info) THEN BEGIN
     TIC
  ENDIF

  
  tmpNEvents      = 0
  nTotPredicted   = 0
  maxChain        = split_interval

  IF KEYWORD_SET(save_chunks_for_speed) THEN BEGIN
     chunkNum        = 0
     totNChunksSaved = 0
     failCodes_string = 'failCodes_'
  ENDIF ELSE BEGIN
     failCodes_string = ''
  ENDELSE
  FOR i=0,N_ELEMENTS(eSpec.x)-1 DO BEGIN


     IF KEYWORD_SET(give_timesplit_info) AND ( (i MOD split_interval) EQ 0 ) THEN BEGIN
        clock     = TIC("eSpecs_"+STRCOMPRESS(i,/REMOVE_ALL) +  '-' + STRCOMPRESS(i+split_interval-1,/REMOVE_ALL))
        ;; doClock   = 1
     ENDIF

     ;; The FAST-adjusted way
     tempeSpec = {x:eSpec.x[i],y:REVERSE(REFORM(eSpec.y[i,0:max_en_ind[i]])),v:REVERSE(REFORM(eSpec.v[i,0:max_en_ind[i]]))}
     IF KEYWORD_SET(produce_failCodes) THEN BEGIN
        tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__FAST_ADJ_V2(tempeSpec,je_vars[i],jee_vars[i], $
                                                             mlt[i],ilat[i], $
                                                             PRODUCE_FAILCODE_OUTPUT=produce_failCodes, $
                                                             OUT_FAILCODES=tempFailCodes, $
                                                             QUIET=quiet, $
                                                             BATCH_MODE=batch_mode, $
                                                             ERRORMSG=errorMsg) ;, $

        ADD_ESPEC_FAILCODES_TO_FAILCODE_STRUCT,failCodes,tempFailCodes
     
     ENDIF ELSE BEGIN
        tempEvent = DIFF_ENERGY_FLUX_SPECTRAL_TYPE__FAST_ADJ(tempeSpec,je_vars[i],jee_vars[i], $
                                                             mlt[i],ilat[i], $
                                                             QUIET=quiet, $
                                                             BATCH_MODE=batch_mode, $
                                                             ERRORMSG=errorMsg) ;, $
     ENDELSE
     tmpNEvents++

     IF SIZE(tempEvent,/TYPE) NE 8 THEN BEGIN
        WRITE_MESSAGE_TO_LOGFILE,errorLogFile, $
                                 STRING(FORMAT='(A0,T20,A0,T40,A0)',KEYWORD_SET(orbStr) ? orbStr : '???', $
                                        GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                        errorMsg), $
                                 /APPEND
     ENDIF
     ADD_EVENT_TO_SPECTRAL_STRUCT,events,tempEvent

     IF KEYWORD_SET(give_timesplit_info) AND ( (i MOD split_interval) EQ split_interval-1 ) THEN BEGIN
        TOC,clock
     ENDIF

     IF tmpNEvents GE maxChain THEN BEGIN
        ADD_EVENT_TO_SPECTRAL_STRUCT,events_final,events

        IF KEYWORD_SET(produce_failCodes) THEN BEGIN
           ADD_ESPEC_FAILCODES_TO_FAILCODE_STRUCT,failCodes_final,failCodes
           failCodes     = !NULL
        ENDIF

        events           = !NULL
        ;; previousTot      = nTotPredicted
        nTotPredicted   += tmpNEvents
        tmpNEvents       = 0


        PRINT,'nTotPredicted: ' + STRCOMPRESS(nTotPredicted,/REMOVE_ALL)

        IF KEYWORD_SET(save_chunks_for_speed) AND (nTotPredicted - totNChunksSaved) GE chunk_save_interval THEN BEGIN

           chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I0,"--eSpecs_",A0,I0,"-",I0,".sav")',chunk_saveFile_pref,chunkNum,failCodes_string, $
                                    totNChunksSaved+1,nTotPredicted)

           PRINT,'Saving chunk to ' + chunkTempFName + '...'
           IF KEYWORD_SET(produce_failCodes) THEN BEGIN
              SAVE,events_final,failCodes_final,FILENAME=chunkDir+chunkTempFName
              failCodes_final     = !NULL
           ENDIF ELSE BEGIN
              SAVE,events_final,FILENAME=chunkDir+chunkTempFName
           ENDELSE

           totNChunksSaved += N_ELEMENTS(events_final.x)

           IF totNChunksSaved NE nTotPredicted THEN STOP

           chunkNum++

           events_final   = !NULL
        ENDIF

     ENDIF

  ENDFOR

  IF tmpNEvents GE 1 THEN BEGIN
     ADD_EVENT_TO_SPECTRAL_STRUCT,events_final,events

     events           = !NULL
     ;; previousTot      = nTotPredicted
     nTotPredicted   += tmpNEvents
     tmpNEvents       = 0

     IF KEYWORD_SET(save_chunks_for_speed) THEN BEGIN

        chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I0,"--eSpecs_",A0,I0,"-",I0,".sav")',chunk_saveFile_pref,chunkNum,failCodes_string, $
                                 totNChunksSaved+1,nTotPredicted)
        PRINT,'Saving chunk to ' + chunkTempFName + '...'
        IF KEYWORD_SET(produce_failCodes) THEN BEGIN
           SAVE,events_final,failCodes_final,FILENAME=chunkDir+chunkTempFName
           failCodes_final     = !NULL
        ENDIF ELSE BEGIN
           SAVE,events_final,FILENAME=chunkDir+chunkTempFName
        ENDELSE

        totNChunksSaved += N_ELEMENTS(events_final.x)
        chunkNum++

        events_final   = !NULL

        PRINT,'Total number of eSpecs saved: ' + STRCOMPRESS(totNChunksSaved,/REMOVE_ALL)
     ENDIF
  ENDIF



END