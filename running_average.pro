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
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:         OUT_{L,R}_EDGES     :       Returned bin edges used for calculating running average.
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
FUNCTION RUNNING_AVERAGE,x,y,binWidth, $
                         BIN_CENTERS=bin_centers, $
                         BIN_L_EDGES=bin_l_edges, $
                         BIN_R_EDGES=bin_r_edges, $
                         XMIN=xMin, $
                         XMAX=xMax, $
                         DONT_TRUNCATE_EDGES=dont_truncate_edges, $
                         OUT_L_EDGES=out_l_edges, $
                         OUT_R_EDGES=out_r_edges, $
                         OUT_ZERO_I=out_zero_i
                         LUN=lun

  IF ~KEYWORD_SET(lun) THEN lun = -1 ;stdout

  ;;Error checking
  IF N_ELEMENTS(x) LE 1 THEN BEGIN
     PRINTF,lun,"More than one x value must be provided to calculate running average!"
     PRINTF,lun,"Quitting ..."
     RETURN, -1
  ENDIF
  IF N_ELEMENTS(y) LE 1 THEN BEGIN
     PRINTF,lun,"More than one y value must be provided to calculate running average!"
     PRINTF,lun,"Quitting ..."
     RETURN, -1
  ENDIF
  IF N_ELEMENTS(y) NE N_ELEMENTS(x) THEN BEGIN
     PRINTF,lun,"Arrays must have the same number of elements to calculate running average!"
     PRINTF,lun,"Quitting ..."
     RETURN, -1
  ENDIF
  IF N_ELEMENTS(binWidth) GT 1 THEN BEGIN
     PRINTF,lun,"Unexpected array provided as binWidth for running average!"
     PRINTF,lun,"Quitting ..."
     RETURN, -1
  ENDIF
  IF N_ELEMENTS(binWidth) EQ 0 THEN BEGIN
     PRINTF,lun,"No binWidth provided for running average! Setting to 1 ..."
     binWidth            = 1
  ENDIF
  IF KEYWORD_SET(bin_l_edges) AND KEYWORD_SET(bin_r_edges) THEN BEGIN
     IF N_ELEMENTS(bin_l_edges) GT 1 AND N_ELEMENTS(bin_r_edges) GT 1 AND $
        (N_ELEMENTS(bin_l_edges) NE N_ELEMENTS(bin_r_edges)) THEN BEGIN
        PRINTF,lun,"Bad sitiation (and I do mean sitiation): bogus l_edges or r_edges provided for running average!"
        PRINT,lun,"Quitting ..."
        RETURN, -1
     ENDIF
     IF N_ELEMENTS(bin_l_edges) EQ 1 AND bin_l_edges LT 0 THEN BEGIN
        PRINTF,lun,"Negative bin_l_edge provided! Bogus!"
        PRINTF,lun,"Quitting..."
        RETURN, -1
     ENDIF
     IF N_ELEMENTS(bin_r_edges) EQ 1 AND bin_r_edges LT 0 THEN BEGIN
        PRINTF,lun,"Negative bin_r_edge provided! Bogus!"
        PRINTF,lun,"Quitting..."
        RETURN, -1
     ENDIF
  ENDIF

  ;;Handle bin centers if not provided
  IF ~KEYWORD_SET(bin_centers) THEN BEGIN
     PRINTF,lun,"No bin centers provided for running average; using integer spacing ..."
     xMin               = KEYWORD_SET(xMin) ? xMin : MIN(x)
     xMax               = KEYWORD_SET(xMax) ? xMax : MAX(x)
     bin_centers        = INDGEN(FLOOR(xMax-xMin))+xMin
  ENDIF

  ;;Take care of bin edges, if not provided or if only one value provided
  IF N_ELEMENTS(bin_l_edges) EQ 0 THEN BEGIN
     IF KEYWORD_SET(dont_truncate_edges) THEN BEGIN
        bin_l_edges     = bin_centers - binWidth/2.
     ENDIF ELSE BEGIN
        bin_l_edges     = (bin_centers - binWidth/2.) > xMin
     ENDELSE
  ENDIF ELSE BEGIN
     IF N_ELEMENTS(bin_l_edges) EQ 1 THEN BEGIN
        bin_l_edges     = bin_centers - bin_l_edges
     ENDIF
  ENDELSE

  IF N_ELEMENTS(bin_r_edges) EQ 0 THEN BEGIN
     IF KEYWORD_SET(dont_truncate_edges) THEN BEGIN
        bin_r_edges     = bin_centers + binWidth/2.
     ENDIF ELSE BEGIN
        bin_r_edges     = (bin_centers + binWidth/2.) < xMax
     ENDELSE
  ENDIF ELSE BEGIN
     IF N_ELEMENTS(bin_r_edges) EQ 1 THEN BEGIN
        bin_r_edges     = bin_centers + bin_r_edges
     ENDIF
  ENDELSE

  ;;Check to make sure bin edges are sensible
  nBins                 = N_ELEMENTS(bin_centers)
  FOR i=0,nBins-1 DO BEGIN
     IF bin_r_edges[i] LT bin_l_edges[i] THEN BEGIN
        PRINTF,lun,FORMAT='("bin_r_edge is less than bin_l_edge for i=",I0,": ",G10.2,TR5,G10.2)',i,bin_r_edges[i],bin_l_edges[i]
        PRINTF,lun,"Quitting ..."
        RETURN, -1
     ENDIF
  ENDFOR
  ;;Calculate some running averages!
  zero_i                = !NULL
  averages              = MAKE_ARRAY(nBins,/DOUBLE)
  FOR i=0,N_ELEMENTS(bin_centers)-1 DO BEGIN

     temp_i             = WHERE(x GT bin_l_edges[i] AND x LE bin_r_edges[i],/NULL)
     nTemp              = N_ELEMENTS(temp_i)
     
     CASE nTemp OF
        0: BEGIN
           zero_i       = [zero_i,i]
           averages[i]  = 1e-10
        END
        1: BEGIN
           averages[i]  = y[temp_i]
        END
        ELSE: BEGIN
           averages[i]  = TOTAL(y[temp_i],/DOUBLE)/nTemp
        END
     ENDCASE

     ;; IF nTemp GT 1 THEN BEGIN
     ;;    
     ;; ENDIF ELSE BEGIN
     ;;    IF nTemp EQ 1 THEN BEGIN
           
     ;; ENDELSE

  ENDFOR

  ;;Take care of optional output
  out_zero_i         = zero_i
  out_l_edge         = bin_l_edges
  out_r_edge         = bin_r_edges

  RETURN,averages

END