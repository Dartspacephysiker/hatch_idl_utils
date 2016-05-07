;2016/03/19 the layout keyword is lame, so let's do this ourselves
;Example:
;; nPlotColumns = 2 & nPlotRows = 2 & mLeft = 0.1 & mRight = 0.0 & mBottom=0.1 & mTop = 0.0 & hPlotSpace =0.1 & vPlotSpace = 0.1
;; FOR i=1,4 DO PRINT,CALC_PLOT_POSITION(i,nPlotColumns,nPlotRows, $
;;                             WMARGIN_LEFT=mLeft, $
;;                             WMARGIN_RIGHT=mRight, $
;;                             WMARGIN_BOTTOM=mBottom, $
;;                             WMARGIN_TOP=mTop, $
;;                             SPACE_HORIZ_BETWEEN_COLS=hPlotSpace, $
;;                             SPACE_VERT_BETWEEN_ROWS=vPlotSpace)


FUNCTION CALC_PLOT_POSITION,plot_index,nPlotColumns,nPlotRows, $
                            WMARGIN_LEFT=mLeft, $
                            WMARGIN_RIGHT=mRight, $
                            WMARGIN_BOTTOM=mBottom, $
                            WMARGIN_TOP=mTop, $
                            SPACE_HORIZ_BETWEEN_COLS=hPlotSpace, $
                            SPACE_VERT_BETWEEN_ROWS=vPlotSpace

  COMPILE_OPT idl2

  IF plot_index LE 0 THEN BEGIN
     PRINT,"Warning! You're about to get junk from CALC_PLOT_POSITION"
     STOP
  ENDIF

  ;;First see if we got any thangs 
  IF N_ELEMENTS(mLeft) EQ 0 THEN mLeft             = 0.D
  IF N_ELEMENTS(mRight) EQ 0 THEN mRight           = 0.D
  IF N_ELEMENTS(mBottom) EQ 0 THEN mBottom         = 0.D
  IF N_ELEMENTS(mTop) EQ 0 THEN mTop               = 0.D
  
  IF N_ELEMENTS(hPlotSpace) EQ 0 THEN hPlotSpace   = 0.D
  IF N_ELEMENTS(vPlotSpace) EQ 0 THEN vPlotSpace   = 0.D
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;How much space do we have to work with?
  totUsableWidth                      = 1.0D - mLeft - mRight
  totUsableHeight                     = 1.0D - mBottom - mTop

  widthPerPlot                        = totUsableWidth/nPlotColumns
  heightPerPlot                       = totUsableHeight/nPlotRows
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;where is the current plot?
  plotColumn       = ( (plot_index - 1) MOD nPlotColumns )
  plotRow          = (plot_index   - 1)/nPlotColumns

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Hold it, now hit it
  IF nPlotColumns GT 1 THEN BEGIN
     CASE plotColumn OF
        0: BEGIN
           posLeft      = mLeft
           posRight     = mLeft + widthPerPlot - hPlotSpace/2.
        END
        nPlotColumns-1: BEGIN
           posLeft      = 1.0 - mRight - widthPerPlot + hPlotSpace/2.
           posRight     = 1.0 - mRight
        END
        ELSE: BEGIN
           posLeft      = mLeft + widthPerPlot*plotColumn + hPlotSpace/4.
           posRight     = mLeft + widthPerPlot*(plotColumn+1) - hPlotSpace/4.
        END
     ENDCASE
  ENDIF ELSE BEGIN
     posLeft            = mLeft
     posRight           = 1.0 - mRight
  ENDELSE


  IF nPlotRows GT 1 THEN BEGIN
     CASE plotRow OF
        0: BEGIN
           posBottom    = 1.0 - mTop - heightPerPlot + vPlotSpace/2.
           posTop       = 1.0 - mTop
        END
        nPlotRows-1: BEGIN
           posBottom    = mBottom
           posTop       = mBottom + heightPerPlot - vPlotSpace/2.
        END
        ELSE: BEGIN
           posBottom    = mBottom + heightPerPlot*plotRow + vPlotSpace/4.
           posTop       = mBottom + heightPerPlot*(plotRow+1) - vPlotSpace/4.
        END
     ENDCASE
  ENDIF ELSE BEGIN
     posBottom          = mBottom
     posTop             = 1.0-mTop
  ENDELSE

  position = [posLeft,posBottom,posRight,posTop]

  RETURN,position
END