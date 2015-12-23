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
;
;-

FUNCTION RUNNING_MEDIAN,x,y,binWidth, $
                         BIN_CENTERS=bin_centers, $
                         BIN_L_EDGES=bin_l_edges, $
                         BIN_R_EDGES=bin_r_edges, $
                         XMIN=xMin, $
                         XMAX=xMax, $
                         DONT_TRUNCATE_EDGES=dont_truncate_edges, $
                         DROP_EDGES=drop_edges, $
                         OUT_L_EDGES=out_l_edges, $
                         OUT_R_EDGES=out_r_edges, $
                         OUT_NONZERO_i=out_nonzero_i, $
                         OUT_ZERO_I=out_zero_i, $
                         LUN=lun

  COMPILE_OPT idl2

  status = RUNNING_STATS_SETUP(x,y,binWidth, $
                               BIN_CENTERS=bin_centers, $
                               BIN_L_EDGES=bin_l_edges, $
                               BIN_R_EDGES=bin_r_edges, $
                               NBINS=nBins, $
                               XMIN=xMin, $
                               XMAX=xMax, $
                               DONT_TRUNCATE_EDGES=dont_truncate_edges, $
                               DROP_EDGES=drop_edges, $
                               LUN=lun)
  
  IF status LT 0 THEN RETURN, status

  zero_i                = !NULL
  nonzero_i             = !NULL
  medians              = MAKE_ARRAY(nBins,/DOUBLE)
  FOR i=0,N_ELEMENTS(bin_centers)-1 DO BEGIN

     temp_i             = WHERE(x GT bin_l_edges[i] AND x LE bin_r_edges[i],/NULL)
     nTemp              = N_ELEMENTS(temp_i)
     
     CASE nTemp OF
        0: BEGIN
           zero_i       = [zero_i,i]
           medians[i]  = 0.
        END
        1: BEGIN
           nonzero_i    = [nonzero_i,i]
           medians[i]  = y[temp_i]
        END
        ELSE: BEGIN
           nonzero_i    = [nonzero_i,i]
           medians[i]  = MEDIAN(y[temp_i],/DOUBLE)
        END
     ENDCASE

  ENDFOR

  ;;Take care of optional output
  out_nonzero_i      = nonzero_i
  out_zero_i         = zero_i
  out_l_edge         = bin_l_edges
  out_r_edge         = bin_r_edges

  RETURN,medians

END