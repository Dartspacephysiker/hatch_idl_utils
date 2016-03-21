;2016/03/21 This routine exists to provide an easy way to 
PRO WINDOW_CUSTOM_LAYOUT, $
   NPLOTROWS=nPlotRows, $
   NPLOTCOLUMNS=nPlotColumns, $
   MARGIN_LEFT=mLeft, $
   MARGIN_RIGHT=mRight, $
   MARGIN_TOP=mTop, $
   MARGIN_BOTTOM=mBottom, $
   SPACE_HORIZ_BETWEEN_COLS=space_horiz_between_cols, $
   SPACE_VERT_BETWEEN_ROWS=space_vert_between_rows, $
   XRANGE=xRange, $
   YRANGE=yRange, $
   XLOG=xLog, $
   YLOG=yLog, $
   XMINOR=xMinor, $
   YMINOR=yMinor, $
   PLOTTITLE=plotTitle, $
   PLOTNAME=plotName, $
   PLOTCOLOR=plotColor, $
   OUTPLOTARR=outPlotArr, $
   OUT_PLOT_I=out_plot_i, $
   WINDOW_TITLE=window_title, $
   WINDOW_DIMENSIONS=dimensions, $
   CURRENT_WINDOW=window, $
   SYM_TRANSPARENCY=sym_transparency, $
   SYM_COLOR=sym_color

  COMPILE_OPT idl2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;window setup
  IF N_ELEMENTS(window) EQ 0 THEN BEGIN
     wTitle        = KEYWORD_SET(window_title) ? window_title : "Custom layout"
     window     = WINDOW(WINDOW_TITLE=wTitle,DIMENSIONS=KEYWORD_SET(dimensions) ? dimensions : [1250,900])
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;plot array/window setup
  nPlots      = nPlotRows*nPlotColumns
  plotArr     = N_ELEMENTS(outPlotArr) EQ 0 ? MAKE_ARRAY(nPlots,/OBJ) : outPlotArr

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;plot details
  xTitle     = !NULL ;KEYWORD_SET(title_maxInd1) ? title_maxInd1 : dataName1
  yTitle     = !NULL ;KEYWORD_SET(title_maxInd2) ? title_maxInd2 : dataName2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;do it
  plot_i               = N_ELEMENTS(out_plot_i) GT 0 ? out_plot_i : 1
  FOR i=0,2 DO BEGIN

     plotColumn       = ( (plot_i - 1) MOD nPlotColumns )
     plotRow          = (plot_i - 1)/nPlotColumns

     ;;for titles
     mLeft               = KEYWORD_SET(mLeft) ? mLeft : 0.06
     mBottom             = KEYWORD_SET(mBottom) ? mBottom : 0.06
     mRight              = KEYWORD_SET(mRight) ? mRight : 0.02
     mTop                = KEYWORD_SET(mTop) ? mTop : (KEYWORD_SET(window_title) ? 0.04 : 0.02)
     hSpaceTwixt         = KEYWORD_SET(space_horiz_between_cols) ? space_horiz_between_cols : 0.04
     vSpaceTwixt         = KEYWORD_SET(space_vert_between_rows) ? space_vert_between_rows : 0.03

     plotPos             = CALC_PLOT_POSITION(plot_i,nPlotColumns,nPlotRows, $
                                              WMARGIN_LEFT=mLeft, $
                                              WMARGIN_RIGHT=mRight, $
                                              WMARGIN_BOTTOM=mBottom, $
                                              WMARGIN_TOP=mTop, $
                                              SPACE_HORIZ_BETWEEN_COLS=hSpaceTwixt, $
                                              SPACE_VERT_BETWEEN_ROWS=vSpaceTwixt)
     
     plotArr[plot_i-1]   = SCATTERPLOT(x,y, $
                                       TITLE=plotTitle, $
                                       NAME=plotName, $
                                       XTITLE=xTitle, $
                                       YTITLE=yTitle, $
                                       XRANGE=xRange, $
                                       YRANGE=yRange, $
                                       XMINOR=xMinor, $
                                       YMINOR=yMinor, $
                                       XLOG=xLog, $
                                       YLOG=yLog, $
                                       XSHOWTEXT=(plotRow EQ nPlotRows-1) ? !NULL : 0, $
                                       YSHOWTEXT=(plotColumn EQ 0) ? !NULL : 0, $
                                       POSITION=plotPos, $
                                       CURRENT=window, $
                                       SYM_COLOR=sym_color, $
                                       SYM_TRANSPARENCY=KEYWORD_SET(sym_transparency) ? sym_transparency : 90) ; $

     IF plotRow EQ 0 AND KEYWORD_SET(column_names) THEN BEGIN
        column_text = TEXT(MEAN([plotPos[0],plotPos[2]]),plotPos[1]-0.55*hSpaceTwixt,column_names[i], $
                           FONT_SIZE=15, $
                           ALIGNMENT=0.5, $
                           TARGET=window)
     ENDIF
     
     plot_i++
     
  ENDFOR

  IF KEYWORD_SET(xTitle) THEN BEGIN
     xTitleText = text(mLeft+0.5*(1.0-mLeft-mRight),0.013, $
                      title_maxInd1, $
                      ;; FONT_NAME='Courier', $
                      ALIGNMENT=0.5, $
                      FONT_SIZE=18, $
                      /NORMAL, $
                      TARGET=window, $
                      CLIP=0)
  ENDIF

  IF KEYWORD_SET(yTitle) THEN BEGIN
     yTitleText = TEXT(0.02,0.5, $
                       title_maxInd2, $
                       ;; FONT_NAME='Courier', $
                       ALIGNMENT=0.5, $
                       FONT_SIZE=18, $
                       ORIENTATION=90, $
                       /NORMAL, $
                       TARGET=window, $
                       CLIP=0)
  ENDIF

  IF KEYWORD_SET(window_title) AND plotRow EQ 0 THEN BEGIN
     winTitleText = TEXT(mLeft+0.5*(1.0-mLeft-mRight),1.0-mTop*0.65, $
                         window_title, $
                         ALIGNMENT=0.5, $
                         FONT_SIZE=18, $
                         /NORMAL, $
                         TARGET=window, $
                         CLIP=0)
  ENDIF

  outPlotArr        = plotArr

END