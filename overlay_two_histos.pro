PRO OVERLAY_TWO_HISTOS,data_1,data_2,DATA_1_COL=data_1_col,DATA_2_COL=data_2_col,DATA_OUTLINE_COL=data_outline_col, $
                       DATA_1_TITLE=data_1_title,DATA_2_TITLE=data_2_title, $
                       BINSIZE=binSize,XRANGE=xRange,YRANGE=yRange,HISTTITLE=histTitle,XTITLE=xTitle, $
                       OUTFILE=outFile,POS=pos

  ;; Defaults
  defData_1_col='yellow'
  defData_2_col='olive'
  
  ;; defData_outline_col='dark slate gray'
  defData_outline_col=36

  defData_1_title='Data 1'
  defData_2_title='Data 2'
  
  defOutFile='overlaid_histos.png'

  defYRange=[0,1]

  defPos=[0.12,0.12,0.88,0.88]

  defThick=1.2
  defCharSize=2.1

  ;; histTitle='Relative freq. of Alfven activity'
  
  ;; Set defaults
  IF N_ELEMENTS(data_1_col) EQ 0 THEN data_1_col = defData_1_col
  IF N_ELEMENTS(data_2_col) EQ 0 THEN data_2_col = defData_2_col
  IF N_ELEMENTS(data_outline_col) EQ 0 THEN data_outline_col = defData_outline_col

  IF N_ELEMENTS(data_1_title) EQ 0 THEN data_1_title = defData_1_title
  IF N_ELEMENTS(data_2_title) EQ 0 THEN data_2_title = defData_2_title

  IF N_ELEMENTS(yRange) LT 2 THEN yRange=defYRange

  IF ~KEYWORD_SET(pos) THEN pos=defPos
  IF ~KEYWORD_SET(thick) THEN thick=defThick
  IF ~KEYWORD_SET(charSize) THEN charSize=defCharSize

  cgHistoplot, data_2, POLYCOLOR=data_2_col, DATACOLORNAME=data_outline_col, $
               /FREQUENCY, /FILL, $
               XRANGE=xRange, YRANGE=yRange, BINSIZE=binSize, $
               XTITLE=xTitle,TITLE=histTitle,CHARSIZE=charSize,THICK=thick, $
               POSITION=pos
  cgHistoplot, data_1, POLYCOLOR=data_1_col, DATACOLORNAME=data_outline_col, $
               /FREQUENCY, /FILL, /OPLOT, $
               ;; YRANGE=yRange, BINSIZE=binSize, $
               XRANGE=xRange, YRANGE=yRange, BINSIZE=binSize, $
               XTITLE=xTitle,TITLE=histTitle,CHARSIZE=charSize,THICK=thick, $
               POSITION=pos
  firstPlot = cgSnapshot()
  
  cgHistoplot, data_1, POLYCOLOR=data_1_col, DATACOLORNAME=data_outline_col, $
               /FREQUENCY, /FILL, $
               XRANGE=xRange, YRANGE=yRange, BINSIZE=binSize, $
               XTITLE=xTitle,TITLE=histTitle,CHARSIZE=charSize,THICK=thick, $
               POSITION=pos
  cgHistoplot, data_2, POLYCOLOR=data_2_col, DATACOLORNAME=data_outline_col, $
               /FREQUENCY, /FILL, /OPLOT, $
               ;; YRANGE=yRange, BINSIZE=binSize, $
               XRANGE=xRange, YRANGE=yRange, BINSIZE=binSize, $
               XTITLE=xTitle,TITLE=histTitle,CHARSIZE=charSize,THICK=thick, $
               POSITION=pos
  secondPlot = cgSnapshot()
  
  cgBlendimage, firstPlot, secondPlot, ALPHA=0.75
  
  cgLegend, SymColors=[data_1_col, data_2_col], PSyms=[15,15], Symsize=1.5, Location=[0.625, 0.87], $
            Titles=[data_1_title, data_2_title], Length=0, /Box, VSpace=2.75, /Background, BG_Color='light gray'
  
  finalPlot = cgSnapshot(FILENAME=outFile)
  
END
