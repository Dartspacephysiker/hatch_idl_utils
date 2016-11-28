;;11/22/16
FUNCTION GET_BOXPLOT_AND_MOMENT_STATISTICS, $
   data, $
   inds, $
   STATS_NAME=stats_name, $
   BPD__OUTLIERS=BPDOutliers, $
   BPD__SUSPECTED_OUTLIERS=BPDSusOutliers

  COMPILE_OPT IDL2

  IF N_ELEMENTS(inds) GE 5 THEN BEGIN
     include_BPD = 1 
  ENDIF ELSE BEGIN
     include_BPD = 0
  ENDELSE

  IF N_ELEMENTS(inds) EQ 0 THEN BEGIN
     include_mom = 0
  ENDIF ELSE BEGIN
     include_mom = 1
  ENDELSE

  IF include_BPD THEN BEGIN
     tmpBPD = CREATEBOXPLOTDATA(data[inds], $
                                CI_VALUES=ci_Vals, $
                                MEAN_VALUES=BPDMean, $
                                OUTLIER_VALUES=BPDOutliers, $
                                SUSPECTED_OUTLIER_VALUES=BPDSusOutliers)
  ENDIF ELSE BEGIN
     tmpBPD         = MAKE_ARRAY(1,5,VALUE=0)
     tmpBPD[0,2]    = N_ELEMENTS(inds) GT 1 ? MEDIAN(data[inds]) : ( N_ELEMENTS(inds) GT 0 ? data[inds] : 0 )
     ci_Vals        = MAKE_ARRAY(2,VALUE=0)
     BPDMean        = 0
     BPDOutliers    = 0
  ENDELSE

  ;;Now load 'er up
  IF include_mom THEN BEGIN
     mom    = MOMENT(data[inds])
     mom    = [mom,( ( N_ELEMENTS(data) GT 1 AND N_ELEMENTS(inds) GT 1 ) ? MEDIAN(data[inds]) : data[0])]
  ENDIF ELSE BEGIN
     mom    = MAKE_ARRAY(5,VALUE=0)
  ENDELSE

  tmpExtra =  {ci_values   : N_ELEMENTS(ci_Vals) GT 0 ? ci_Vals : 0, $
               mean_values : BPDMean}

  CASE 1 OF
     (N_ELEMENTS(BPDOutliers) GT 0) AND $
        (N_ELEMENTS(BPDSusOutliers) GT 0): BEGIN
        tmpExtra    = CREATE_STRUCT(tmpExtra, $
                                    "OUTLIER_VALUES",BPDOutliers, $
                                    "SUSPECTED_OUTLIER_VALUES",BPDSusOutliers)
        
     END
     (N_ELEMENTS(BPDOutliers) GT 0): BEGIN
        tmpExtra   = CREATE_STRUCT(tmpExtra, $
                                   "OUTLIER_VALUES",BPDOutliers)

     END
     (N_ELEMENTS(BPDSusOutliers) GT 0): BEGIN
        tmpExtra   = CREATE_STRUCT(tmpExtra, $
                                   "SUSPECTED_OUTLIER_VALUES",BPDSusOutliers)
     END
     ELSE: tmpExtra = 0
  ENDCASE

  tmpBPD          = {data    : tmpBPD, $
                     bad     : ~include_BPD, $
                     extras  : tmpExtra}

  BPMomStats      = {BPD     : tmpBPD, $
                     mom     : mom, $
                     name    : KEYWORD_SET(stats_name) ? stats_name : "Boxplot and moment data"}

  RETURN,BPMomStats

END
