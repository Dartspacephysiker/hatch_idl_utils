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
   SPACE__XTITLE=space__xTitle, $
   SPACE__YTITLE=space__yTitle, $
   SPACE__WINTITLE=space__winTitle, $
   WINDOW_DIMENSIONS=dimensions, $
   COLUMN_NAMES=column_names, $
   ROW_NAMES=row_names, $
   SPACE_FOR_COLUMN_NAMES=space_for_column_names, $
   SPACE_FOR_ROW_NAMES=space_for_row_names, $
   CURRENT_WINDOW=cur_window, $
   MAKE_NEW=make_new, $
   CURRENT_PLOT_I=cur_plot_i
   ;; CURRENT_PLOT_POS=cur_plotPos

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON WINDOW_CUSTOM,WC__nPlotRows,WC__nPlotCols,WC__nPlots, $
     WC__mLeft,WC__mRight,WC__mBottom,WC__mTop, $
     WC__space_horiz,WC__space_vert, $
     WC__xTitleObj,WC__yTitleObj,WC__winTitleObj, $
     WC__space__xTitle,WC__space__yTitle,WC__space__winTitle, $
     WC__colNameObjs,WC_rowNameObjs, $
     WC__space_colNames,WC__space__rowNames, $
     WC__window,WC__dimensions, $
     WC__plot_i,WC__plotColumn,WC__plotRow, $
     WC__plotPos, $
     WC__window_list,WC__window_i
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;window setup
  WC__dimensions      = KEYWORD_SET(dimensions) ? dimensions : [1250,900]
  ;; IF N_ELEMENTS(WC__window) EQ 0 THEN BEGIN

  IF N_ELEMENTS(WC__window_i) EQ 0 THEN BEGIN
     WC__window_list  = LIST()
     WC__window_i     = 0
     WC__window_list.add,WINDOW(WINDOW_TITLE=wTitle,DIMENSIONS=WC__dimensions)
     WC__window       = WC__window_list[0]
  ENDIF ELSE BEGIN
     ;;garbage cleanup, if necessary
     IF N_ELEMENTS(WC__window_list[WC__window_i]) GT 0 AND ~ISA(WC__window_list[WC__window_i]) THEN BEGIN
        ;;Are there others?
        temp_i        = WC__window_i
        WHILE temp_i GE 0 DO BEGIN
           IF N_ELEMENTS(WC__window_list[temp_i]) GT 0 AND ~ISA(WC__window_list[temp_i]) THEN BEGIN
              PRINT,'WINDOW_CUSTOM: Junking dead window ...'
              WC__window_list.remove,temp_i
              IF WC__window_i GT 0 THEN BEGIN
                 WC__window_i-- 
                 ENDIF ELSE BEGIN
                    ;; make_new = 1
                 ENDELSE
           ENDIF
           temp_i--
        ENDWHILE
     ENDIF

     IF KEYWORD_SET(make_new) OR N_ELEMENTS(WC__window_list) EQ 0 THEN BEGIN
        WC__window_list.add,WINDOW(WINDOW_TITLE=wTitle,DIMENSIONS=WC__dimensions)
        WC__window_i     = N_ELEMENTS(WC__window_list) - 1
        WC__window       = WC__window_list[WC__window_i]
     ENDIF ELSE BEGIN ;;just return the current window

        PRINT,"Already got a window here, and you didn't ask for a nouveau; wassup?"
        PRINT,'Doing it for you...'
        WC__window_list.add,WINDOW(WINDOW_TITLE=wTitle,DIMENSIONS=WC__dimensions)
        WC__window_i     = N_ELEMENTS(WC__window_list) - 1
        WC__window       = WC__window_list[WC__window_i]

     ENDELSE
  ENDELSE 



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;plot array/window setup
  WC__nPlotRows       = KEYWORD_SET(nPlotRows) ? nPlotRows : 1
  WC__nPlotCols       = KEYWORD_SET(nPlotColumns) ? nPlotColumns : 1
  WC__nPlots          = WC__nPlotRows*WC__nPlotCols

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;set up margins
  ;;for titles
  def_xTitle_space    = 0.07
  def_yTitle_space    = 0.04
  def_winTitle_space  = 0.04

  WC__space__xTitle   = KEYWORD_SET(space__xTitle) ? space__xTitle : def_xTitle_space
  WC__space__yTitle   = KEYWORD_SET(space__yTitle) ? space__yTitle : def_yTitle_space
  WC__space__winTitle = KEYWORD_SET(space__winTitle) ? space__winTitle : def_winTitle_space

  WC__space__colNames = KEYWORD_SET(space_for_column_names) ? space_for_column_names : (KEYWORD_SET(column_names) ? 0.04 : 0.00)
  WC__space__rowNames = KEYWORD_SET(space_for_row_names) ? space_for_row_names : (KEYWORD_SET(row_names) ? 0.04 : 0.00)

  WC__mLeft           = KEYWORD_SET(mLeft) ? mLeft : ( (KEYWORD_SET(yTitle) ? WC__space__yTitle : 0.00) + WC__space__rowNames )
  WC__mBottom         = KEYWORD_SET(mBottom) ? mBottom : (KEYWORD_SET(xTitle) ? WC__space__xTitle : 0.00)
  WC__mRight          = KEYWORD_SET(mRight) ? mRight : 0.02
  WC__mTop            = KEYWORD_SET(mTop) ? mTop : ( (KEYWORD_SET(window_title) ? WC__space__winTitle : 0.02) + WC__space__colNames )

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
           WC__colNameObjs[plotColumn] = TEXT(MEAN([WC__plotPos[0],WC__plotPos[2]]),1-WC__space__winTitle-0.5*WC__space__colNames, $
                                              column_names[plotColumn], $
                                              FONT_SIZE=14, $
                                              ALIGNMENT=0.5, $
                                              TARGET=WC__window)
        ENDIF
     
     IF plotColumn EQ 0 AND KEYWORD_SET(row_names) THEN BEGIN
        WC__rowNameObjs[plotRow]       = TEXT(WC__space__yTitle+0.05*WC__space__rowNames,MEAN([WC__plotPos[1],WC__plotPos[3]]), $
                                              row_names[plotRow], $
                                              FONT_SIZE=14, $
                                              ORIENTATION=90, $
                                              ALIGNMENT=0.5, $
                                              TARGET=WC__window)
     ENDIF
     
     WC__plot_i++
     
  ENDFOR

  IF KEYWORD_SET(xTitle) THEN BEGIN
     WC__xTitleObj    = TEXT(WC__mLeft+0.5*(1.0-WC__mLeft-WC__mRight),0.01, $
                             xTitle, $
                             ALIGNMENT=0.5, $
                             FONT_SIZE=18, $
                             /NORMAL, $
                             TARGET=WC__window, $
                             CLIP=0)
  ENDIF

  IF KEYWORD_SET(yTitle) THEN BEGIN
     WC__yTitleObj    = TEXT(0.02,0.5, $
                             yTitle, $
                             ALIGNMENT=0.5, $
                             FONT_SIZE=18, $
                             ORIENTATION=90, $
                             /NORMAL, $
                             TARGET=WC__window, $
                             CLIP=0)
  ENDIF

  IF KEYWORD_SET(window_title) THEN BEGIN
     WC__winTitleObj  = TEXT(WC__mLeft+0.5*(1.0-WC__mLeft-WC__mRight),1.0-WC__space__winTitle*0.8, $
                             window_title, $
                             ALIGNMENT=0.5, $
                             FONT_SIZE=18, $
                             /NORMAL, $
                             TARGET=WC__window, $
                             CLIP=0)
  ENDIF
  
  ;;Reset, deliver first stuff
  WC__plot_i          = 1
  cur_window          = WC__window
  cur_plot_i          = WC__plot_i

END