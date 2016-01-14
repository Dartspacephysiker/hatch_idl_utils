;+
; NAME:                      RUNNING AVERAGE
;
;
;
; PURPOSE:                   Calculate a running average of the provided dataset.
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:                   x                  :        Independent variable; doesn't need to be monotonic.
;                           y                  :        Dependent variable
;                           binWidth           :        Width around which to take bin
;
;
; OPTIONAL INPUTS:          BIN_CENTERS        :        Array of values around which to calculate the running average.
;                           BIN_{L,R}_EDGES    :        User-specified bin edges, if desired.
;                                                         If not set, the default is integer spacing between MIN(x) and MAX(x).
;                           X{MIN,MAX}         :        {Min,Max}imum value of x to be included in the running average.
;                                                       (disregarded if edges are not truncated)
;
;
;
; KEYWORD PARAMETERS:      DONT_TRUNCATE_EDGES :        Allow bin edges to extend beyond XMIN or XMAX.
;                          DROP_EDGES          :        Drop the bins that don't cover a full bin width
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:         OUT_{L,R}_EDGES     :       Returned bin edges used for calculating running average.
;                           OUT_NONZERO_I       :       Indices of bin centers with corresponding data.
;                           OUT_ZERO_I          :       Indices of bin centers with no corresponding data.
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:             Without mirror wrapping, some weird stuff can happen. Try the example.
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:                  x=INDGEN(100) & y=SIN(x/!PI) & avgs=RUNNING_AVERAGE(x,y,10)
;                           plot,avgs
;
;
;
; MODIFICATION HISTORY:     2015/12/22 Barn
;                           2015/12/24 Added error bar output
;                           2015/12/26 Adding smoothing.
;                           2016/01/07 Added a default confidence limit in RUNNING_STATS_SETUP, also BIN_{L,R}_OFFSET
;                           2016/01/11 Added PRINT_STATS_FOR_THESE_RANGES keyword to get some storm stats.
;-
FUNCTION RUNNING_MEDIAN,x,y,binWidth, $
                        BIN_CENTERS=bin_centers, $
                        BIN_SPACING=bin_spacing, $
                        BIN_L_OFFSET=bin_l_offset, $
                        BIN_R_OFFSET=bin_r_offset, $
                        BIN_L_EDGES=bin_l_edges, $
                        BIN_R_EDGES=bin_r_edges, $
                        XMIN=xMin, $
                        XMAX=xMax, $
                        SMOOTH_NPOINTS=smooth_nPoints, $
                        DONT_TRUNCATE_EDGES=dont_truncate_edges, $
                        DROP_EDGES=drop_edges, $
                        MAKE_ERROR_BARS=make_error_bars, $
                        ERROR_BAR_NBOOT=nBoot, $
                        ERROR_BAR_CONFLIMIT=confLimit, $
                        OUT_L_EDGES=out_l_edges, $
                        OUT_R_EDGES=out_r_edges, $
                        OUT_NONZERO_i=out_nonzero_i, $
                        OUT_ZERO_I=out_zero_i, $
                        OUT_ERROR_BARS=out_error_bars, $
                        PRINT_STATS_FOR_THESE_RANGES=print_stats_ranges, $
                        LUN=lun

  COMPILE_OPT idl2

  status                     = RUNNING_STATS_SETUP(x,y,binWidth, $
                                                   BIN_CENTERS=bin_centers, $
                                                   BIN_SPACING=bin_spacing, $
                                                   BIN_L_OFFSET=bin_l_offset, $
                                                   BIN_R_OFFSET=bin_r_offset, $
                                                   BIN_L_EDGES=bin_l_edges, $
                                                   BIN_R_EDGES=bin_r_edges, $
                                                   NBINS=nBins, $
                                                   XMIN=xMin, $
                                                   XMAX=xMax, $
                                                   SMOOTH_NPOINTS=smooth_nPoints, $
                                                   DONT_TRUNCATE_EDGES=dont_truncate_edges, $
                                                   DROP_EDGES=drop_edges, $
                                                   MAKE_ERROR_BARS=make_error_bars, $
                                                   ERROR_BAR_NBOOT=nBoot, $
                                                   ERROR_BAR_CONFLIMIT=confLimit, $
                                                   LUN=lun)

  IF status LT 0 THEN RETURN, status

  zero_i                     = !NULL
  nonzero_i                  = !NULL
  medians                    = MAKE_ARRAY(nBins,/DOUBLE)
  error_bars                 = MAKE_ARRAY(2,nBins,/DOUBLE)
  ;; error_bars                 = MAKE_ARRAY(nBins,2,/DOUBLE)

  FOR i=0,N_ELEMENTS(bin_centers)-1 DO BEGIN

     temp_i                  = Where(x GT bin_l_edges[i] AND x LE bin_r_edges[i],/NULL)
     nTemp                   = N_ELEMENTS(temp_i)
     
     CASE nTemp OF
        0: BEGIN
           zero_i            = [zero_i,i]
           medians[i]        = 0.
        END
        1: BEGIN
           nonzero_i         = [nonzero_i,i]
           medians[i]        = y[temp_i]
           IF KEYWORD_SET(make_error_bars) THEN BEGIN
              ;; error_bars[i,*]   = [y[temp_i],y[temp_i]]
              error_bars[*,i]   = [y[temp_i],y[temp_i]]
           ENDIF
        END
        ELSE: BEGIN
           nonzero_i         = [nonzero_i,i]

           IF KEYWORD_SET(make_error_bars) THEN BEGIN
              temp_eb        = BOOTSTRAP_MEDIAN(y[temp_i], $
                                                NBOOT=nBoot, $
                                                CONFLIMIT=confLimit)
              ;; error_bars[i,*]= [temp_eb[0],temp_eb[2]]
              error_bars[*,i]= [temp_eb[0],temp_eb[2]]
              medians[i]     = temp_eb[1]
           ENDIF ELSE BEGIN
              medians[i]     = MEDIAN(y[temp_i],/DOUBLE)
           ENDELSE

        END
     ENDCASE

  ENDFOR

  ;;Any smoothing?
  IF smooth_nPoints GT 1 THEN BEGIN
     PRINTF,lun,FORMAT='("Smoothing running medians with ",I0," points ...")',smooth_nPoints
     medians                 = SMOOTH(medians,smooth_nPoints,/EDGE_TRUNCATE)
  ENDIF

  IF KEYWORD_SET(print_stats_ranges) THEN BEGIN
     PRINT_RUNNING_STATS_RANGES,x,y,print_stats_ranges, $
                                /MEDIAN, $
                                LUN=lun     
  ENDIF

  ;;Take care of optional output
  out_nonzero_i              = nonzero_i
  out_zero_i                 = zero_i
  out_l_edge                 = bin_l_edges
  out_r_edge                 = bin_r_edges
  out_error_bars             = error_bars

  RETURN,medians

END