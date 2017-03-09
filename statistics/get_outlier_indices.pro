;;11/26/16
FUNCTION GET_OUTLIER_INDICES,data, $
                             USER_INDS=user_inds, $
                             ONLY_UPPER=only_upper, $
                             ONLY_LOWER=only_lower, $
                             SUSPECTED_OUTLIER_I=susOut_i, $
                             COMPLEMENT_INDICES=comp_i, $
                             LOG_OUTLIERS=log_outliers, $
                             LOG__ABS=log__abs, $
                             LOG__NEG=log__neg, $
                             NULL=null, $
                             FINITE=finite, $
                             DOUBLE=double, $
                             RETURN_STATISTICS=return_stats, $
                             OUT_STATISTICS=outStats, $
                             VERBOSE=verbose
  

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(log_outliers) THEN BEGIN

     IF ~KEYWORD_SET(log__neg) THEN BEGIN
        stat_i = GET_POS_NEG_INDICES(data, $
                                      USER_INDS=user_inds, $
                                      /POS_ONLY, $
                                      ;; INCLUDE_WHERE_EQ_0=inc_0, $
                                      ;; NULL=null, $
                                      FINITE=finite, $
                                      ;; RETURN_STRUCT=struct, $
                                      ;; OUT_STATS_NAME=stats_name, $
                                      VERBOSE=verbose)

        IF stat_i[0] EQ -1 THEN BEGIN
           IF ~KEYWORD_SET(log__abs) THEN BEGIN
              PRINT,"Can't do log inds for you--none are positive!"
              RETURN,(KEYWORD_SET(null) ? !NULL : -1)
           ENDIF

           stat_i = !NULL
        ENDIF
        
     ENDIF

     IF KEYWORD_SET(log__abs) OR KEYWORD_SET(log__neg) THEN BEGIN
        negInds = GET_POS_NEG_INDICES(data, $
                                      USER_INDS=user_inds, $
                                      /NEG_ONLY, $
                                      ;; INCLUDE_WHERE_EQ_0=inc_0, $
                                      ;; NULL=null, $
                                      FINITE=finite, $
                                      ;; RETURN_STRUCT=struct, $
                                      ;; OUT_STATS_NAME=stats_name, $
                                      VERBOSE=verbose)

        IF KEYWORD_SET(log__abs) THEN BEGIN
           stat_i = CGSETUNION(stat_i,negInds)
        ENDIF

        IF negInds[0] NE -1 THEN BEGIN
           transformedNeg = 1B
           data[negInds] = (-1)*data[negInds]
        ENDIF
     ENDIF

     transformedLog = 1
     data = KEYWORD_SET(double) ? DOUBLE(ALOG10(data)) : ALOG10(data)

  ENDIF ELSE BEGIN
     stat_i = KEYWORD_SET(user_inds) ? user_inds : LINDGEN(N_ELEMENTS(data))
  ENDELSE



  stats = GET_BOXPLOT_AND_MOMENT_STATISTICS( $
          data, $
          stat_i, $
          STATS_NAME=stats_name, $
          BPD__OUTLIERS=BPDOutliers, $
          BPD__SUSPECTED_OUTLIERS=BPDSusOutliers)

  ;; CASE 1 OF
  ;;    KEYWORD_SET(only_upper): BEGIN
  IF KEYWORD_SET(only_lower) THEN BEGIN

     upperOutBound = !NULL
     upSusOutBound = !NULL

  ENDIF ELSE BEGIN

     IF N_ELEMENTS(BPDOutliers) GT 0 THEN BEGIN
        upperBad_ii = WHERE(BPDOutliers[1,*] GT stats.mom[4],nUpperBad)
        IF nUpperBad GT 0 THEN BEGIN
           upperOutBound = MIN((BPDOutliers[1,*])[upperBad_ii])
        ENDIF ELSE BEGIN
           upperOutBound = !NULL
        ENDELSE
     ENDIF

     IF N_ELEMENTS(BPDSusOutliers) GT 0 THEN BEGIN
        upSusBad_ii = WHERE(BPDSusOutliers[1,*] GT stats.mom[4],nUpSusBad)
        IF nUpSusBad GT 0 THEN BEGIN
           upSusOutBound = MIN((BPDSusOutliers[1,*])[upSusBad_ii])
        ENDIF ELSE BEGIN
           upSusOutBound = !NULL
        ENDELSE
     ENDIF
     
  ENDELSE

  IF KEYWORD_SET(only_upper) THEN BEGIN

     lowerOutBound = !NULL
     loSusOutBound = !NULL

  ENDIF ELSE BEGIN

     IF N_ELEMENTS(BPDOutliers) GT 0 THEN BEGIN
        lowerBad_ii = WHERE(BPDOutliers[1,*] LT stats.mom[4],nLowerBad)
        IF nLowerBad GT 0 THEN BEGIN
           lowerOutBound = MAX((BPDOutliers[1,*])[lowerBad_ii])
        ENDIF ELSE BEGIN
           lowerOutBound = !NULL
        ENDELSE
     ENDIF

     IF N_ELEMENTS(BPDSusOutliers) GT 0 THEN BEGIN
        loSusBad_ii = WHERE(BPDSusOutliers[1,*] LT stats.mom[4],nLoSusBad)
        IF nLoSusBad GT 0 THEN BEGIN
           loSusOutBound = MAX((BPDSusOutliers[1,*])[loSusBad_ii])
        ENDIF ELSE BEGIN
           loSusOutBound = !NULL
        ENDELSE
     ENDIF
     
  ENDELSE

  
  ;;Now remember, we're still in either log or not log space, so no need to correct

  IF N_ELEMENTS(lowerOutBound) GT 0 THEN BEGIN
     outlier_ii = WHERE(data[stat_i] LE lowerOutBound,/NULL)
  ENDIF 

  IF N_ELEMENTS(upperOutBound) GT 0 THEN BEGIN
     IF N_ELEMENTS(outlier_ii) GT 0 THEN BEGIN
        outlier_ii = CGSETUNION(outlier_ii,WHERE(data[stat_i] GE upperOutBound,/NULL))
     ENDIF ELSE BEGIN
        outlier_ii = WHERE(data[stat_i] GE upperOutBound,/NULL)
     ENDELSE
  ENDIF;;  ELSE BEGIN
  ;;    outlier_ii = !NULL
  ;; ENDELSE

  ;;Now suspected outliers
  IF N_ELEMENTS(loSusOutBound) GT 0 THEN BEGIN
     susOut_ii = WHERE(data[stat_i] LE loSusOutBound)
  ENDIF 

  IF N_ELEMENTS(upSusOutBound) GT 0 THEN BEGIN
     IF N_ELEMENTS(susOut_ii) GT 0 THEN BEGIN
        susOut_ii = CGSETUNION(susOut_ii,WHERE(data[stat_i] GE upSusOutBound,/NULL))
     ENDIF ELSE BEGIN
        susOut_ii = WHERE(data[stat_i] GE upSusOutBound,/NULL)
     ENDELSE
  ENDIF ;; ELSE BEGIN
  ;;    susOut_ii = !NULL
  ;; ENDELSE
  IF ( N_ELEMENTS(outlier_ii) GT 0 ) AND $
     ( N_ELEMENTS(susOut_ii) GT 0 ) $
  THEN BEGIN 

     susOut_ii = CGSETDIFFERENCE(susOut_ii,outlier_ii,NORESULT=-1)

     IF (susOut_ii[0] EQ -1) THEN BEGIN
        susOut_ii = !NULL
     ENDIF

  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Make output arrays
  IF N_ELEMENTS(outlier_ii) GT 0 THEN BEGIN
     outlier_i = stat_i[TEMPORARY(outlier_ii)]
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(null) THEN BEGIN
        outlier_i = !NULL
     ENDIF ELSE BEGIN
        outlier_i = -1
     ENDELSE
  ENDELSE

  IF N_ELEMENTS(susOut_ii) GT 0 THEN BEGIN
     susOut_i = stat_i[TEMPORARY(susOut_ii)]
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(null) THEN BEGIN
        susOut_i = !NULL
     ENDIF ELSE BEGIN
        susOut_i = -1
     ENDELSE
  ENDELSE


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Finish up
  IF KEYWORD_SET(transformedLog) THEN BEGIN
     data = KEYWORD_SET(double) ? (DOUBLE(10.D))^(data) : 10.^(data)
  ENDIF     

  IF KEYWORD_SET(transformedNeg) THEN BEGIN
     data[negInds] = (-1)*data[negInds]
  ENDIF     

  IF KEYWORD_SET(return_stats) THEN BEGIN
     outStats = TEMPORARY(stats)
  ENDIF

  IF ARG_PRESENT(comp_i) THEN BEGIN

     IF N_ELEMENTS(outlier_i) GT 0 THEN BEGIN
        comp_i = CGSETDIFFERENCE(stat_i,outlier_i,NORESULT=-1)
     ENDIF ELSE BEGIN
        comp_i = -1
     ENDELSE

     IF (comp_i[0] EQ -1) AND KEYWORD_SET(null) THEN BEGIN
        comp_i = !NULL
     ENDIF

  ENDIF

  RETURN,outlier_i

END
