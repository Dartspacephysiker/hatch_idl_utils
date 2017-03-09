;+
; NAME:                      RUNNING HISTO
;
;
;
; PURPOSE:                   Calculate a running histogram (i.e., bins can overlap) of the provided dataset.
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
;                           binWidth           :        Width around which to take bin
;
;
; OPTIONAL INPUTS:          BIN_CENTERS        :        Array of values around which to calculate the running histo.
;                           BIN_{L,R}_EDGES    :        User-specified bin edges, if desired.
;                                                         If not set, the default is integer spacing between MIN(x) and MAX(x).
;                           X{MIN,MAX}         :        {Min,Max}imum value of x to be included in the running histo.
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
; OPTIONAL OUTPUTS:         OUT_{L,R}_EDGES     :       Returned bin edges used for calculating running histo.
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
; EXAMPLE:                  x=INDGEN(100) & y=SIN(x/!PI) & avgs=RUNNING_HISTO(x,10)
;                           plot,avgs
;
;
;
; MODIFICATION HISTORY:     2016/01/09 Barn, branched from RUNNING_AVERAGE
;
;-
FUNCTION RUNNING_HISTO,x,binWidth, $
                       BIN_CENTERS=bin_centers, $
                       BIN_SPACING=bin_spacing, $
                       BIN_OFFSET=bin_offset, $
                       ;; BIN_L_OFFSET=bin_l_offset, $
                       ;; BIN_R_OFFSET=bin_r_offset, $
                       BIN_L_EDGES=bin_l_edges, $
                       BIN_R_EDGES=bin_r_edges, $
                       XMIN=xMin, $
                       XMAX=xMax, $
                       ;; SMOOTH_NPOINTS=smooth_nPoints, $
                       DONT_TRUNCATE_EDGES=dont_truncate_edges, $
                       DROP_EDGES=drop_edges, $
                       ;; WINDOW_SUM=window_sum, $
                       ;; MAKE_ERROR_BARS=make_error_bars, $
                       ;; ERROR_BAR_NBOOT=nBoot, $
                       ;; ERROR_BAR_CONFLIMIT=confLimit, $
                       OUT_L_EDGES=out_l_edges, $
                       OUT_R_EDGES=out_r_edges, $
                       OUT_NONZERO_i=out_nonzero_i, $
                       OUT_ZERO_I=out_zero_i, $
                       ;; OUT_ERROR_BARS=out_error_bars, $
                       LUN=lun

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Fake RUNNING_STATS_SETUP out by providing x twice
  status                        = RUNNING_STATS_SETUP(x,x,binWidth, $
                                                      BIN_CENTERS=bin_centers, $
                                                      BIN_SPACING=bin_spacing, $
                                                      BIN_OFFSET=bin_offset, $
                                                      ;; BIN_L_OFFSET=bin_l_offset, $
                                                      ;; BIN_R_OFFSET=bin_r_offset, $
                                                      BIN_L_EDGES=bin_l_edges, $
                                                      BIN_R_EDGES=bin_r_edges, $
                                                      NBINS=nBins, $
                                                      XMIN=xMin, $
                                                      XMAX=xMax, $
                                                      ;; SMOOTH_NPOINTS=smooth_nPoints, $
                                                      DONT_TRUNCATE_EDGES=dont_truncate_edges, $
                                                      DROP_EDGES=drop_edges, $
                                                      ;; MAKE_ERROR_BARS=make_error_bars, $
                                                      ;; ERROR_BAR_NBOOT=nBoot, $
                                                      ;; ERROR_BAR_CONFLIMIT=confLimit, $
                                                      LUN=lun)
  
  IF status LT 0 THEN RETURN, status

  ;; IF KEYWORD_SET(make_error_bars) AND KEYWORD_SET(window_sum) THEN BEGIN
  ;;    PRINTF,lun,"make_error_bars and window_sum set! Meaningless!"
  ;;    STOP
  ;; ENDIF

  ;;Calculate some running histos!
  zero_i                        = !NULL
  nonzero_i                     = !NULL
  output                        = MAKE_ARRAY(nBins,/DOUBLE)
  FOR i=0,N_ELEMENTS(bin_centers)-1 DO BEGIN

     temp_i                     = WHERE(x GT bin_l_edges[i] AND x LE bin_r_edges[i],/NULL)
     nTemp                      = N_ELEMENTS(temp_i)
     
     CASE nTemp OF
        0: BEGIN
           zero_i               = [zero_i,i]
           output[i]            = 0.
        END
        ;; 1: BEGIN
        ;;    nonzero_i            = [nonzero_i,i]
        ;;    output[i]            = nTemp
        ;; END
        ELSE: BEGIN
           nonzero_i            = [nonzero_i,i]

           output[i]            = nTemp
        END
     ENDCASE

  ENDFOR

  ;;Take care of optional output
  out_nonzero_i                 = nonzero_i
  out_zero_i                    = zero_i
  out_l_edge                    = bin_l_edges
  out_r_edge                    = bin_r_edges

  RETURN,output

END