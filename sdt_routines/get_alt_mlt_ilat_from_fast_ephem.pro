;; 2017/12/01 Why NOT_TIMEARR? Because I don't want to mess up other routines that depend on a particular default behavior
;; Sorry it's strange, but it all goes to say---the default needs to be that a time array is provided
;; 
PRO GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,orb,timeArr, $
                                     NOT_TIMEARR=not_timeArr, $
                                     NOT_TARR__DELTA_T=delta_t, $
                                     OUT_TSORTED_I=tSort_i, $
                                     OUT_ALT=alt, $
                                     OUT_MLT=mlt, $
                                     OUT_ILAT=ilat, $
                                     OUT_ORBIT=orbit, $
                                     OUT_MAPRATIO=mapRatio, $
                                     OUT_NEVENTS=nEvents, $
                                     LOGLUN=logLun
  

  COMPILE_OPT idl2,STRICTARRSUBS

  CASE 1 OF 
     KEYWORD_SET(not_timeArr): BEGIN

        CASE 1 OF
           N_ELEMENTS(timeArr) EQ 2: BEGIN
              ;; PRINT,"GET_ALT_MLT_ILAT_FROM_FAST_EPHEM: Beginning-and-end-array mode"
           END
           N_ELEMENTS(orb) EQ 1: BEGIN
              PRINT,"GET_ALT_MLT_ILAT_FROM_FAST_EPHEM: Orb (" + STRCOMPRESS(orb) + ") mode"
              PRINT,"BUT I DON'T KNOW HOW!"
              STOP
           END
           ELSE: BEGIN
              PRINT,"Whaaaaaaaaaaatttt"
              STOP
           END
        ENDCASE

        GET_FA_ORBIT,timeArr[0],timeArr[1], $
                     DELTA_T=delta_t, $
                     /ALL, $
                     /NO_STORE, $
                     STRUC=struc

     END
     ELSE: BEGIN

        IF N_ELEMENTS(timeArr) EQ 0 THEN BEGIN
           PRINT,'Bogus'

           alt      = -999
           mlt      = -999
           ilat     = -999
           mapRatio = -999
           nEvents  = 0
           RETURN
        ENDIF

        ;; IF N_ELEMENTS(fastLoc) EQ 0 OR N_ELEMENTS(fastLoc_times) EQ 0 THEN BEGIN
        ;;    LOAD_FASTLOC_AND_FASTLOC_TIMES,fastLoc,fastLoc_times
        ;; ENDIF

        ;;Make sure time array is monotonic and unique
        ;; CHECK_SORTED,timeArr,tSorted,SORTED_I=tSort_i,/QUIET
        ;; IF ~tSorted THEN BEGIN
        ;;    PRINT,"Input time array not sorted! Providing sorted inds for you so you can pick up the pieces of your life ..."
        ;;    times           = timeArr[tSort_i]
        ;; ENDIF ELSE BEGIN
        ;;    times           = timeArr
        ;; ENDELSE

        uniq_i  = UNIQ(timeArr,SORT(timeArr))
        nUniq   = N_ELEMENTS(uniq_i)
        nEvents = N_ELEMENTS(timeArr)
        IF nUniq NE nEvents THEN BEGIN
           PRINT,"Time array is not unique! Providing uniq inds (even better) so you can stop wasting everyone's time"

           ;; tSort_i         = UNIQ(timeArr,SORT(timeArr))
           tSort_i         = TEMPORARY(uniq_i)
           times           = timeArr[tSort_i]
           nEvents         = TEMPORARY(nUniq)
        ENDIF ELSE BEGIN
           ;; times           = timeArr
           tSort_i         = !NULL
           ;; tSort_i         = LINDGEN(N_ELEMENTS(timeArr))
        ENDELSE

        ;; tmpFL_i         = WHERE(fastLoc.orbit EQ orb,nTmpFLTimes)
        
        ;; IF nTmpFLTimes LT 2 THEN BEGIN
        IF KEYWORD_SET(orb) THEN BEGIN
           PRINT,"Getting ephem data for orbit " + STRCOMPRESS(orb,/REMOVE_ALL) + '...'
        ENDIF ELSE BEGIN
           PRINT,"Getting ephem data for " + STRCOMPRESS(N_ELEMENTS(timeArr),/REMOVE_ALL) + ' times ...'
        ENDELSE

        GET_FA_ORBIT,(N_ELEMENTS(tSort_i) GT 0 ? times : timeArr),/TIME_ARRAY,/ALL,/NO_STORE,STRUC=struc

     END
  ENDCASE

  ;; GET_DATA,'ALT',DATA=alt
  ;; GET_DATA,'MLT',DATA=mlt
  ;; GET_DATA,'ILAT',DATA=ilat
  ;; GET_DATA,'B_model',DATA=bMod
  ;; GET_DATA,'BFOOT',DATA=bFoot
  alt = struc.alt
  mlt = struc.mlt
  ilat = struc.ilat
  orbit = struc.orbit
  ;; bMod = struc.bMod
  ;; bFoot = struc.bFoot

  IF ~KEYWORD_SET(not_timeArr) THEN IF N_ELEMENTS(struc.alt) NE nEvents THEN STOP
  ;; alt       = (TEMPORARY(alt)).y
  ;; mlt       = (TEMPORARY(mlt)).y
  ;; ilat      = (TEMPORARY(ilat)).y

  mag1      = (struc.B_model[*,0]*struc.B_model[*,0]+ $
               struc.B_model[*,1]*struc.B_model[*,1]+ $
               struc.B_model[*,2]*struc.B_model[*,2])^0.5
  mag2      = (struc.bFoot[*,0]*struc.bFoot[*,0]+ $
               struc.bFoot[*,1]*struc.bFoot[*,1]+ $
               struc.bFoot[*,2]*struc.bFoot[*,2])^0.5
  mapRatio  = TEMPORARY(mag2)/TEMPORARY(mag1)

     
  ;; ENDIF ELSE BEGIN
  ;;    ;;Just interp, if we already have it ...
  ;;    tmpFLTimes      = fastLoc_times[tmpFL_i]
  ;;    tmpAlt          = {time:tmpFLTimes,comp1:fastLoc.alt[tmpFL_i]}
  ;;    tmpMLT          = {time:tmpFLTimes,comp1:fastLoc.mlt[tmpFL_i]}
  ;;    tmpILAT         = {time:tmpFLTimes,comp1:fastLoc.ilat[tmpFL_i]}
     
  ;;    junk            = {time:times,comp1:0,ncomp:1}

  ;;    FA_FIELDS_COMBINE,junk,tmpAlt,RESULT=alt,/INTERP,DELT_T=50.,/TALK
  ;;    FA_FIELDS_COMBINE,junk,tmpMLT,RESULT=mlt,/INTERP,DELT_T=50.,/TALK
  ;;    FA_FIELDS_COMBINE,junk,tmpMLT,RESULT=ilat,/INTERP,DELT_T=50.,/TALK

  ;; ENDELSE



END
