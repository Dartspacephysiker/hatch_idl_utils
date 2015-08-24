;+
; NAME: Density_plot
;
;
;
; PURPOSE: 2d histo density plot
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY: 02/20/2015 Ripped off IDL Coyote's density plot
;
;-
PRO density_plot,x,y,XRANGE=xRange,YRANGE=yRange,XTITLE=xTitle,YTITLE=yTitle, $
                 LOG_X=log_X,LOG_Y=log_Y,LOGXRANGE=logXRange,LOGYRANGE=logYRange, $
                 XBINSIZE=xBinSize, YBINSIZE=yBinSize, PRINTBINSIZES=printBinsizes
  
  IF N_ELEMENTS(x) NE N_ELEMENTS(y) THEN BEGIN
     print, "N elements(x) doesn't match N elements(y)! Aborting..."
     RETURN
  ENDIF

  ;; can't set log ranges and regular ranges
  IF (KEYWORD_SET(xRange) AND KEYWORD_SET(logXRange)) OR (KEYWORD_SET(yRange) AND KEYWORD_SET(logyRange)) THEN BEGIN
     print,"Can't set both range and logrange keyword for a given axis! Set one or the other."
     RETURN
  ENDIF

  ;; Make sure no NaNs or infinities!
  good_i = WHERE(FINITE(x) AND FINITE(y))
  IF N_ELEMENTS(good_i) NE N_ELEMENTS(x) THEN print, "Found some NaNs! Junking them..."
  x = x(good_i)
  y = y(good_i)

  ;; do log stuff
  IF KEYWORD_SET(log_X) THEN BEGIN
     check = WHERE(x LE 0)
     IF check[0] NE -1 THEN BEGIN
        print, "x has some negative values. I have to convert them to absolute values..."
        x = abs(x)
     ENDIF
     IF KEYWORD_SET(xRange) THEN BEGIN
        IF xRange[0] LT 0 THEN xRange[0] =  MIN(x(WHERE(x NE 0)))
        IF xRange[1] LT 0 THEN xRange[1] = -xRange[1]
        xRange = ALOG10(xRange)
     ENDIF
     x = ALOG10(x)
  ENDIF
  IF KEYWORD_SET(log_Y) THEN BEGIN
     check = WHERE(y LE 0)
     IF check[0] NE -1 THEN BEGIN
        print, "y has some negative values. I have to convert them to absolute values..."
        y = abs(y)
     ENDIF
     IF KEYWORD_SET(yRange) THEN BEGIN
        IF yRange[0] LT 0 THEN yRange[0] = MIN(y(WHERE(y NE 0)))
        IF yRange[1] LT 0 THEN yRange[1] = -yRange[1]
        yRange = ALOG10(yRange)
     ENDIF
     y = ALOG10(y)
  ENDIF

  ;; log ranges involved?
  IF KEYWORD_SET(logXRange) THEN xRange = logXRange
  IF KEYWORD_SET(logYRange) THEN xRange = logYRange

  ;; Ranges
  IF KEYWORD_SET(xRange) THEN BEGIN
     xlim_i = WHERE((x GE xRange[0]) AND (x LE xRange[1]))
  ENDIF ELSE  BEGIN
     xRange = [Min(x), Max(x)]
     xlim_i = WHERE(x EQ x)
  ENDELSE
  IF KEYWORD_SET(yRange) THEN BEGIN
     ylim_i = WHERE((y GE yRange[0]) AND (y LE yRange[1]))
  ENDIF ELSE  BEGIN
     yRange = [Min(y), Max(y)]
     ylim_i = WHERE(y EQ y)
  ENDELSE
  
  ;; Now filter those data
  lim_i = CGSETINTERSECTION(xlim_i,ylim_i)
  x = x(lim_i)
  y = y(lim_i)

  ;;Bin sizes
  IF NOT KEYWORD_SET(xBinsize) THEN xBinsize = (3.5 * StdDev(x)) / N_Elements(x)^(0.3333)
  IF NOT KEYWORD_SET(yBinsize) THEN yBinsize = (3.5 * StdDev(y)) / N_Elements(y)^(0.3333)
  IF KEYWORD_SET(printBinSizes) THEN BEGIN
     print,"xBinsize : " + strcompress(xBinsize,/remove_all)
     print,"yBinsize : " + strcompress(yBinsize,/remove_all)
  ENDIF

  ;; Axis titles
  IF KEYWORD_SET(log_X) THEN xLogStr = "log " ELSE xLogStr = ""
  IF KEYWORD_SET(log_Y) THEN yLogStr = "log " ELSE yLogStr = ""
  IF NOT KEYWORD_SET(xTitle) THEN xTitle = "Concentration of " + xLogStr + "x"
  IF NOT KEYWORD_SET(yTitle) THEN yTitle = "Concentration of " + yLogStr + "y"
  
  ;; Open a display window.
  cgDisplay
  
  ;; Create the density plot by binning the data into a 2D histogram.
  density = Hist_2D(x, y, Min1=xRange[0], Max1=xRange[1], Bin1=xBinsize, $
                    Min2=yRange[0], Max2=yRange[1], Bin2=yBinsize)   
  
  maxDensity = Ceil(Max(density)/1e2) * 1e2
  scaledDensity = BytScl(density, Min=0, Max=maxDensity)
  
  ;; Load the color table for the display. All zero values will be gray.
  cgLoadCT, 33
  TVLCT, cgColor('gray', /Triple), 0
  TVLCT, r, g, b, /Get
  palette = [ [r], [g], [b] ]
  
  ;; Display the density plot.
  cgImage, scaledDensity, XRange=xRange, YRange=yRange, /Axes, Palette=palette, $
           XTitle=xTitle, YTitle=yTitle, $
           Position=[0.125, 0.125, 0.9, 0.8]
  
  thick = (!D.Name EQ 'PS') ? 6 : 2
  cgContour, density, LEVELS=maxDensity*[0.25, 0.5, 0.75], /OnImage, $
             C_Colors=['Tan','Tan', 'Brown'], C_Annotation=['Low', 'Avg', 'High'], $
             C_Thick=thick, C_CharThick=thick
  
  ;; Display a color bar.
  cgColorbar, Position=[0.125, 0.875, 0.9, 0.925], Title='Density', $
              Range=[0, maxDensity], NColors=254, Bottom=1, OOB_Low='gray', $
              TLocation='Top'
END ;;*****************************************************************