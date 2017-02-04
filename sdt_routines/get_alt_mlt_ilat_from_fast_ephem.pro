PRO GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,orb,timeArr, $
                                     OUT_TSORTED_I=tSort_i, $
                                     OUT_ALT=alt, $
                                     OUT_MLT=mlt, $
                                     OUT_ILAT=ilat, $
                                     OUT_MAPRATIO=mapRatio, $
                                     OUT_NEVENTS=nEvents, $
                                     LOGLUN=logLun
  

  COMPILE_OPT idl2

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
     PRINT,"Getting orb data for orbit " + STRCOMPRESS(orb,/REMOVE_ALL) + '...'
     GET_FA_ORBIT,(N_ELEMENTS(tSort_i) GT 0 ? times : timeArr),/TIME_ARRAY,/ALL
     get_data,'ALT',DATA=alt
     get_data,'MLT',DATA=mlt
     get_data,'ILAT',DATA=ilat
     GET_DATA,'B_model',DATA=bMod
     GET_DATA,'BFOOT',DATA=bFoot

     IF N_ELEMENTS(alt.y) NE nEvents THEN STOP
     alt       = (TEMPORARY(alt)).y
     mlt       = (TEMPORARY(mlt)).y
     ilat      = (TEMPORARY(ilat)).y

     mag1      = (bMod.y[*,0]*bMod.y[*,0]+ $
                  bMod.y[*,1]*bMod.y[*,1]+ $
                  bMod.y[*,2]*bMod.y[*,2])^0.5
     mag2      = (bFoot.y[*,0]*bFoot.y[*,0]+ $
                  bFoot.y[*,1]*bFoot.y[*,1]+ $
                  bFoot.y[*,2]*bFoot.y[*,2])^0.5
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
