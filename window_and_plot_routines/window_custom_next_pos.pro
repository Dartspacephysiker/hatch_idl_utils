;2016/03/21 This routine exists to provide an easy way to, you know, lay out some plots
FUNCTION WINDOW_CUSTOM_NEXT_POS,NEXT_ROW=next_row,NEXT_COLUMN=next_column,HOLD_POSITION=hold_position

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON WINDOW_CUSTOM
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;do it
  WC__plotPos            = CALC_PLOT_POSITION(WC__plot_i,WC__nPlotCols,WC__nPlotRows, $
                                              WMARGIN_LEFT=WC__mLeft, $
                                              WMARGIN_RIGHT=WC__mRight, $
                                              WMARGIN_BOTTOM=WC__mBottom, $
                                              WMARGIN_TOP=WC__mTop, $
                                              SPACE_HORIZ_BETWEEN_COLS=WC__space_horiz, $
                                              SPACE_VERT_BETWEEN_ROWS=WC__space_vert)
  

  CASE 1 OF 
     KEYWORD_SET(next_row): BEGIN
        ;; WC__plotRow++
        ;; IF WC__plotRow GE WC__nPlotRows THEN BEGIN
           
        ;; ENDIF
        ;; WC__plot_i       = WC__plotRow*WC__nPlotCols+1
        ;; plotColumn       = ( (WC__plot_i - 1) MOD WC__nPlotCols )
        ;; WC__plot_i = ((WC__plot_i + WC__nPlotCols) MOD WC__nPlots) + 1
        WC__plot_i += WC__nPlotCols
        IF WC__plot_i GT WC__nPlots THEN WC__plot_i += (1 - WC__nPlots)
     END
     ;; KEYWORD_SET(next_column): BEGIN
     ;;    WC__plotColumn++
        
     ;; END
     KEYWORD_SET(hold_position): BEGIN

     END
     ELSE: BEGIN
        WC__plot_i++
        WC__plotColumn   = ( (WC__plot_i - 1) MOD WC__nPlotCols )
        WC__plotRow      = (WC__plot_i - 1)/WC__nPlotCols
     END
  ENDCASE

  RETURN,WC__plotPos

END