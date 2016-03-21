;2016/03/21 This routine exists to provide an easy way to, you know, lay out some plots
PRO WINDOW_CUSTOM_SETUP, $
   NPLOTROWS=nPlotRows, $
   NPLOTCOLUMNS=nPlotColumns, $
   MARGIN_LEFT=mLeft, $
   MARGIN_RIGHT=mRight, $
   MARGIN_TOP=mTop, $
   MARGIN_BOTTOM=mBottom, $
   SPACE_HORIZ_BETWEEN_COLS=space_horiz_between_cols, $
   SPACE_VERT_BETWEEN_ROWS=space_vert_between_rows, $
   XTITLE=xTitle, $
   YTITLE=yTitle, $
   WINDOW_TITLE=window_title, $
   WINDOW_DIMENSIONS=dimensions, $
   COLUMN_NAMES=column_names, $
   ROW_NAMES=row_names, $
   SPACE_FOR_COLUMN_NAMES=space_for_column_names, $
   SPACE_FOR_ROW_NAMES=space_for_row_names, $
   CURRENT_WINDOW=cur__window, $
   CURRENT_PLOT_I=cur_plot_i, $
   CURRENT_PLOT_POS=cur_plotPos

  COMPILE_OPT idl2

  COMMON WINDOW_CUSTOM,WC__nPlotRows,WC__nPlotCols,WC__nPlots, $
     WC__mLeft,WC__mRight,WC__mBottom,WC__mTop, $
     WC__space_horiz,WC__space_vert, $
     WC__xTitleObj,WC__yTitleObj, $
     WC__winTitleObj,WC__dimensions, $
     WC__colNameObjs,WC_rowNameObjs, $
     WC__spaceForCol,WC__spaceForRow, $
     WC__window,WC__plot_i,WC__plotPos
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;window setup
  WC__dimensions      = KEYWORD_SET(dimensions) ? dimensions : [1250,900]
  IF N_ELEMENTS(WC__window) EQ 0 THEN BEGIN
     wTitle           = KEYWORD_SET(window_title) ? window_title : "Custom layout"
     WC__window       = WINDOW(WINDOW_TITLE=wTitle,DIMENSIONS=WC__dimensions)
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;plot array/window setup
  WC__nPlotRows       = KEYWORD_SET(nPlotRows) ? nPlotRows : 1
  WC__nPlotCols       = KEYWORD_SET(nPlotColumns) ? nPlotColumns : 1
  WC__nPlots          = WC__nPlotRows*WC__nPlotCols

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;set up margins
  ;;for titles
  WC__spaceForCol     = KEYWORD_SET(space_for_column_names) ? space_for_column_names : (KEYWORD_SET(column_names) ? 0.03 : 0.00)
  WC__spaceForRow     = KEYWORD_SET(space_for_row_names) ? space_for_row_names : (KEYWORD_SET(row_names) ? 0.03 : 0.00)

  WC__mLeft           = KEYWORD_SET(mLeft) ? mLeft : ( (KEYWORD_SET(yTitle) ? 0.06 : 0.00) + WC__spaceForRow )
  WC__mBottom         = KEYWORD_SET(mBottom) ? mBottom : (KEYWORD_SET(xTitle) ? 0.06 : 0.00)
  WC__mRight          = KEYWORD_SET(mRight) ? mRight : 0.02
  WC__mTop            = KEYWORD_SET(mTop) ? mTop : ( (KEYWORD_SET(window_title) ? 0.04 : 0.02) + WC__spaceForCol )

  WC__space_horiz     = KEYWORD_SET(space_horiz_between_cols) ? space_horiz_between_cols : 0.04
  WC__space_vert      = KEYWORD_SET(space_vert_between_rows) ? space_vert_between_rows : 0.03
  
  IF KEYWORD_SET(row_names) THEN BEGIN
     WC__rowNameObjs  = MAKE_ARRAY(WC__nPlotRows,/OBJ)
  ENDIF
  
  IF KEYWORD_SET(column_names) THEN BEGIN
     WC__colNameObjs  = MAKE_ARRAY(WC__nPlotCols,/OBJ)
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;do it
  WC__plot_i          = 1
  FOR i=0,WC__nPlots-1 DO BEGIN

     plotColumn       = ( (WC__plot_i - 1) MOD WC__nPlotCols )
     plotRow          = (WC__plot_i - 1)/WC__nPlotCols

     WC__plotPos      = CALC_PLOT_POSITION(WC__plot_i,WC__nPlotCols,WC__nPlotRows, $
                                           WMARGIN_LEFT=WC__mLeft, $
                                           WMARGIN_RIGHT=WC__mRight, $
                                           WMARGIN_BOTTOM=WC__mBottom, $
                                           WMARGIN_TOP=WC__mTop, $
                                           SPACE_HORIZ_BETWEEN_COLS=WC__space_horiz, $
                                           SPACE_VERT_BETWEEN_ROWS=WC__space_vert)
     
     IF plotRow EQ 0 AND KEYWORD_SET(column_names) THEN BEGIN
        ;; FOR i=0,N_ELEMENTS(column_names)-1 DO BEGIN
           WC__colNameObjs[plotColumn] = TEXT(MEAN([WC__plotPos[0],WC__plotPos[2]]),WC__plotPos[3]+0.55*WC__spaceForCol,column_names[plotColumn], $
                                           FONT_SIZE=14, $
                                           ALIGNMENT=0.5, $
                                           TARGET=WC__window)
        ;; ENDFOR
     ENDIF
     
     IF plotColumn EQ 0 AND KEYWORD_SET(row_names) THEN BEGIN
        ;; FOR i=0,N_ELEMENTS(row_names)-1 DO BEGIN
           WC__rowNameObjs[plotRow] = TEXT(WC__plotPos[0]-0.55*WC__spaceForRow,MEAN([WC__plotPos[1],WC__plotPos[3]]),row_names[plotRow], $
                                           FONT_SIZE=14, $
                                           ORIENTATION=90, $
                                           ALIGNMENT=0.5, $
                                           TARGET=WC__window)
        ;; ENDFOR
     ENDIF
     
     WC__plot_i++
     
  ENDFOR

  IF KEYWORD_SET(xTitle) THEN BEGIN
     WC__xTitleObj    = TEXT(WC__mLeft+0.5*(1.0-WC__mLeft-WC__mRight),0.013, $
                             xTitle, $
                             ;; FONT_NAME='Courier', $
                             ALIGNMENT=0.5, $
                             FONT_SIZE=18, $
                             /NORMAL, $
                             TARGET=WC__window, $
                             CLIP=0)
  ENDIF

  IF KEYWORD_SET(yTitle) THEN BEGIN
     WC__yTitleObj    = TEXT(0.02,0.5, $
                             yTitle, $
                             ;; FONT_NAME='Courier', $
                             ALIGNMENT=0.5, $
                             FONT_SIZE=18, $
                             ORIENTATION=90, $
                             /NORMAL, $
                             TARGET=WC__window, $
                             CLIP=0)
  ENDIF

  ;; IF KEYWORD_SET(window_title) AND plotRow EQ 0 THEN BEGIN
  IF KEYWORD_SET(window_title) THEN BEGIN
     WC__winTitleObj  = TEXT(WC__mLeft+0.5*(1.0-WC__mLeft-WC__mRight),1.0-WC__mTop*0.65, $
                             window_title, $
                             ALIGNMENT=0.5, $
                             FONT_SIZE=18, $
                             /NORMAL, $
                             TARGET=WC__window, $
                             CLIP=0)
  ENDIF
  
  ;;Reset, deliver first stuff
  WC__plot_i          = 1   
  WC__plotPos         = CALC_PLOT_POSITION(WC__plot_i,WC__nPlotCols,WC__nPlotRows, $
                                           WMARGIN_LEFT=WC__mLeft, $
                                           WMARGIN_RIGHT=WC__mRight, $
                                           WMARGIN_BOTTOM=WC__mBottom, $
                                           WMARGIN_TOP=WC__mTop, $
                                           SPACE_HORIZ_BETWEEN_COLS=WC__space_horiz, $
                                           SPACE_VERT_BETWEEN_ROWS=WC__space_vert)
  
  cur_window          = WC__window
  cur_plot_i          = WC__plot_i
  cur_plotPos         = WC__plotPos

END