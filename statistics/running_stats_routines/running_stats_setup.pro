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
; CALLING SEQUENCE:          Called by either RUNNING_AVERAGE or RUNNING_MEDIAN
;
;
;
; INPUTS:                    x                  :        Independent variable; doesn't need to be monotonic.
;                            y                  :        Dependent variable
;                            binWidth           :        Width around which to take bin
;                           
;                           
; OPTIONAL INPUTS:           BIN_CENTERS        :        Array of values around which to calculate the running average.
;                            BIN_{L,R}_EDGES    :        User-specified bin edges, if desired.
;                                                          If not set, the default is integer spacing between MIN(x) and MAX(x).
;                            X{MIN,MAX}         :        {Min,Max}imum value of x to be included in the running average.
;                                                        (disregarded if edges are not truncated)
;                           
;                           
;                           
; KEYWORD PARAMETERS:       DONT_TRUNCATE_EDGES :        Allow bin edges to extend beyond XMIN or XMAX.
;                           DROP_EDGES          :        Drop the bins that don't cover a full bin width
;
;
; OUTPUTS:
;
;
;
; PROCEDURE:                Called by either RUNNING_AVERAGE or RUNNING_MEDIAN
;
;
;
; MODIFICATION HISTORY:     2015/12/23 Barn
;                           2015/12/24 Added error bar output
;                           2015/12/26 Adding smooth keyword
;                           2016/01/07 Added a default confidence limit and BIN_{L,R}_OFFSETS
;                           
;-
FUNCTION RUNNING_STATS_SETUP,x,y,binWidth, $
                             BIN_CENTERS=bin_centers, $
                             BIN_SPACING=bin_spacing, $
                             ;; BIN_L_OFFSET=bin_l_offset, $
                             ;; BIN_R_OFFSET=bin_r_offset, $
                             BIN_OFFSET=bin_offset, $
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
                             LUN=lun

  ;;defaults
  defBinWidth                   = 1
  defSmooth_nPoints             = 1
  defNBoot                      = 10
  ;; defConfLimit                  = 0.68
  defConfLimit                  = 0.68

  IF ~KEYWORD_SET(lun) THEN lun = -1 ;stdout

  ;;Error checking
  IF N_ELEMENTS(x) LE 1 THEN BEGIN
     PRINTF,lun,"More than one x value must be provided to calculate running statistics!"
     PRINTF,lun,"Quitting ..."
     RETURN, -1
  ENDIF
  IF N_ELEMENTS(y) LE 1 THEN BEGIN
     PRINTF,lun,"More than one y value must be provided to calculate running statistics!"
     PRINTF,lun,"Quitting ..."
     RETURN, -1
  ENDIF
  IF N_ELEMENTS(y) NE N_ELEMENTS(x) THEN BEGIN
     PRINTF,lun,"Arrays must have the same number of elements to calculate running statistics!"
     PRINTF,lun,"Quitting ..."
     RETURN, -1
  ENDIF

  CASE N_ELEMENTS(binWidth) OF
     0: BEGIN
        binWidth     = defBinWidth
        PRINTF,lun,"No binWidth provided for running statistics! Set to default: " + STRCOMPRESS(binWidth,/REMOVE_ALL)
     END
     1: BEGIN
        PRINTF,lun,'Bin width: ' + STRCOMPRESS(binWidth,/REMOVE_ALL)
     END
     ELSE: BEGIN
        PRINTF,lun,"Unexpected array provided as binWidth for running statistics!"
        PRINTF,lun,"Quitting ..."
        RETURN, -1
     END
  ENDCASE

  IF N_ELEMENTS(bin_offset) EQ 0 THEN bin_offset = 0.0

  IF N_ELEMENTS(bin_l_edges) GT 0 AND N_ELEMENTS(bin_r_edges) GT 0 THEN BEGIN
     IF N_ELEMENTS(bin_l_edges) GT 1 AND N_ELEMENTS(bin_r_edges) GT 1 AND $
        (N_ELEMENTS(bin_l_edges) NE N_ELEMENTS(bin_r_edges)) THEN BEGIN
        PRINTF,lun,"Bad sitiation (and I do mean sitiation): bogus l_edges or r_edges provided for running statistics!"
        PRINT,lun,"Quitting ..."
        RETURN, -1
     ENDIF
     IF N_ELEMENTS(bin_l_edges) EQ 1 AND bin_l_edges[0] LT 0 THEN BEGIN
        PRINTF,lun,"Negative bin_l_edge provided! Bogus!"
        PRINTF,lun,"Quitting..."
        RETURN, -1
     ENDIF
     IF N_ELEMENTS(bin_r_edges) EQ 1 AND bin_r_edges[0] LT 0 THEN BEGIN
        PRINTF,lun,"Negative bin_r_edge provided! Bogus!"
        PRINTF,lun,"Quitting..."
        RETURN, -1
     ENDIF
  ENDIF

  ;;Handle bin centers if not provided
  IF ~KEYWORD_SET(bin_centers) THEN BEGIN
     xMin               = KEYWORD_SET(xMin) ? xMin : MIN(x)
     xMax               = KEYWORD_SET(xMax) ? xMax : MAX(x)

     IF ~KEYWORD_SET(bin_spacing) THEN BEGIN
        IF KEYWORD_SET(make_error_bars) THEN BEGIN
           bin_spacing  = 5
        ENDIF ELSE BEGIN
           PRINTF,lun,"No bin centers provided for running statistics; using integer spacing ..."
           bin_spacing  = 1
        ENDELSE
     ENDIF ELSE BEGIN
        PRINTF,lun,"No bin centers provided for running statistics; using " + STRCOMPRESS(bin_spacing,/REMOVE_ALL) + "-hr spacing ..."
     ENDELSE
     
     ;; bin_centers        = INDGEN(FLOOR(xMax-xMin))+xMin+bin_spacing/2.

     nBins              = FLOOR( (xMax-xMin+ABS(bin_offset))/DOUBLE(bin_spacing) )
     bin_centers        = INDGEN(nBins)*DOUBLE(bin_spacing)+xMin+bin_spacing/2.
     ;; bin_centers        = INDGEN(nBins)*DOUBLE(bin_spacing) + xMin + binCenterOff
  ENDIF ELSE BEGIN
     nBins              = N_ELEMENTS(bin_centers)
  ENDELSE

  ;;Take care of bin edges, if not provided or if only one value provided
  IF N_ELEMENTS(bin_l_edges) EQ 0 THEN BEGIN
        
     ;; IF KEYWORD_SET(dont_truncate_edges) THEN BEGIN
        bin_l_edges     = bin_centers - 0.5*binWidth + bin_offset
        ;; bin_l_edges     = bin_centers - binWidth/2.
     ;; ENDIF ELSE BEGIN
     ;;    bin_l_edges     = (bin_centers - 0.5*binWidth + bin_offset) > xMin
        ;; bin_l_edges     = (bin_centers - binWidth/2.) > xMin
     ;; ENDELSE
  ENDIF ELSE BEGIN
     ;; IF N_ELEMENTS(bin_l_edges) EQ 1 THEN BEGIN
     ;;    bin_l_edges     = bin_centers - bin_l_edges
     ;; ENDIF
  ENDELSE

  IF N_ELEMENTS(bin_r_edges) EQ 0 THEN BEGIN

     ;; IF KEYWORD_SET(dont_truncate_edges) THEN BEGIN
        bin_r_edges     = bin_centers + 0.5*binWidth + bin_offset
        ;; bin_r_edges     = bin_centers + binWidth/2.
     ;; ENDIF ELSE BEGIN
     ;;    bin_r_edges     = (bin_centers + 0.5*binWidth + bin_offset) < xMax
        ;; bin_r_edges     = (bin_centers + binWidth/2.) < xMax
     ;; ENDELSE
  ENDIF ELSE BEGIN
     ;; IF N_ELEMENTS(bin_r_edges) EQ 1 THEN BEGIN
     ;;    bin_r_edges     = bin_centers + bin_r_edges
     ;; ENDIF
  ENDELSE

  ;;Drop the edges so that only bins getting a full bin width are kept?
  IF KEYWORD_SET(drop_edges) THEN BEGIN
     keep_i             = INDGEN(nBins)

     lMin_i             = WHERE(bin_l_edges LT xMin)
     rMax_i             = WHERE(bin_r_edges GT xMax)

     IF lMin_i[0] NE -1 THEN BEGIN
        lMin            = MAX(lMin_i)+1
     ENDIF ELSE BEGIN
        lMin            = MIN(keep_i)
     ENDELSE

     IF rMax_i[0] NE -1 THEN BEGIN
        rMax            = MIN(rMax_i)-1
     ENDIF ELSE BEGIN
        rMax            = MAX(keep_i)
     ENDELSE
        
     nBins              = N_ELEMENTS([lMin:rMax])
     bin_l_edges        = bin_l_edges[lMin:rMax]
     bin_r_edges        = bin_r_edges[lMin:rMax]
     bin_centers        = bin_centers[lMin:rMax]
  ENDIF

  ;;Check to make sure bin edges are sensible
  ;; nBins                 = N_ELEMENTS(bin_centers)
  FOR i=0,nBins-1 DO BEGIN
     IF bin_r_edges[i] LT bin_l_edges[i] THEN BEGIN
        PRINTF,lun,FORMAT='("bin_r_edge is less than bin_l_edge for i=",I0,": ",G10.2,TR5,G10.2)',i,bin_r_edges[i],bin_l_edges[i]
        PRINTF,lun,"Quitting ..."
        RETURN, -1
     ENDIF
  ENDFOR

  IF N_ELEMENTS(smooth_nPoints) EQ 0 THEN BEGIN
     smooth_nPoints     = defSmooth_nPoints
  ENDIF ELSE BEGIN
  ENDELSE
  IF smooth_nPoints GT 1 THEN BEGIN
     PRINTF,lun,'Smoothing running statistics with ' + STRCOMPRESS(smooth_nPoints,/REMOVE_ALL) + ' points ...'
  ENDIF

  IF KEYWORD_SET(make_error_bars) THEN BEGIN
     PRINTF,lun,'Making error bars as well ...'
     IF ~KEYWORD_SET(nBoot) THEN BEGIN
        nBoot                = defNBoot
        PRINTF,lun, 'No NBoot provided! Set to default: ' + STRCOMPRESS(nBoot,/REMOVE_ALL)
     ENDIF

     IF ~KEYWORD_SET(confLimit) THEN BEGIN
        confLimit            = defConfLimit
        PRINTF,lun,"No confidence limit provided! Set to default: " + STRCOMPRESS(confLimit,/REMOVE_ALL)
     ENDIF
  ENDIF

  RETURN,0

END