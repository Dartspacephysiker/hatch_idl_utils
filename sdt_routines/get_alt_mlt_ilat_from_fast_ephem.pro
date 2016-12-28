PRO GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,orb,timeArr, $
                                     OUT_TSORTED_I=tSort_i, $
                                     OUT_ALT=alt, $
                                     OUT_MLT=mlt, $
                                     OUT_ILAT=ilat, $
                                     LOGLUN=logLun
  

  COMPILE_OPT idl2

  ;; IF N_ELEMENTS(fastLoc) EQ 0 OR N_ELEMENTS(fastLoc_times) EQ 0 THEN BEGIN
  ;;    LOAD_FASTLOC_AND_FASTLOC_TIMES,fastLoc,fastLoc_times
  ;; ENDIF

  ;;Make sure time array is monotonic and unique
  CHECK_SORTED,timeArr,tSorted,SORTED_I=tSort_i,/QUIET
  IF ~tSorted THEN BEGIN
     PRINT,"Input time array not sorted! Providing sorted inds for you so you can pick up the pieces of your life ..."
     times           = timeArr[tSort_i]
  ENDIF ELSE BEGIN
     times           = timeArr
  ENDELSE

  uniq_i = UNIQ(times)

  IF N_ELEMENTS(uniq_i) NE N_ELEMENTS(times) THEN BEGIN
     PRINT,"Time array is not unique! Providing uniq inds (even better) so you can stop wasting everyone's time"

     tSort_i         = UNIQ(timeArr,SORT(timeArr))
     times           = timeArr[tSort_i]
  ENDIF

  ;; tmpFL_i         = WHERE(fastLoc.orbit EQ orb,nTmpFLTimes)
  
  ;; IF nTmpFLTimes LT 2 THEN BEGIN
     PRINT,"Getting orb data for orbit " + STRCOMPRESS(orb,/REMOVE_ALL) + '...'
     GET_FA_ORBIT,times,/TIME_ARRAY
     get_data,'ALT',DATA=alt
     get_data,'MLT',DATA=mlt
     get_data,'ILAT',DATA=ilat

     IF N_ELEMENTS(alt.y) NE N_ELEMENTS(times) THEN STOP
     alt             = alt.y
     mlt             = mlt.y
     ilat            = ilat.y
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
