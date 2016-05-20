PRO PLOT_SPECTRAL_TYPE__NEWELL_ET_AL_2009,events, $
                                          DATA=data, $
                                          OUTNAME=outName, $
                                          TITLE=title

  IF KEYWORD_SET(data) AND N_ELEMENTS(events) EQ 0 THEN events = data

  nSpectra            = N_ELEMENTS(events)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;PLOT PRELIMS
  monoVal             = 1
  strictMVal          = 2
  broadVal            = 3
  strictBVal          = 4
  diffuseVal          = 5

  monoCol             = 'blue'
  strictMCol          = 'green'
  broadCol            = 'red'
  strictBCol          = 'purple'
  diffuseCol          = 'black'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;SORT EVENTS
  ;; time                = MAKE_ARRAY(nSpectra,/DOUBLE ,VALUE=-1)
  mono                = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)
  strictM             = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)
  broad               = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)
  strictB             = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)
  diffuse             = MAKE_ARRAY(nSpectra,/INTEGER,VALUE=-1)

  mono_i              = WHERE(events.mono    EQ 1,nMono)
  strictM_i           = WHERE(events.mono    EQ 2,nStrictM)
  broad_i             = WHERE(events.broad   EQ 1,nBroad)
  strictB_i           = WHERE(events.broad   EQ 2,nStrictB)
  diffuse_i           = WHERE(events.diffuse EQ 1,nDiffuse)

  mono[mono_i]        = monoVal
  strictM[strictM_i]  = strictMVal
  broad[broad_i]      = broadVal
  strictB[strictB_i]  = strictBVal
  diffuse[diffuse_i]  = diffuseVal

  ;;get time from events struct arr
  ;; FOR i=0,nSpectra-1 DO time[i] = events[i].time

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;NOW GET PLOTS READY
  window              = WINDOW(DIMENSIONS=[800,600])

  nPlots              = 5
  plotArr             = MAKE_ARRAY(nPlots,/OBJ)
  plotSym             = '*'
  symSize             = 1.0

  colArr              = [monoCol       ,strictMCol   ,broadCol   ,strictBCol    ,diffuseCol]
  datArr              = [[mono]        ,[strictM]    ,[broad]    ,[strictB]     ,[diffuse] ]
  nameArr             =['Monoenergetic','Strict Mono','Broadband','Strict Broad','Diffuse' ]

  ;; times               = events.time_e-events[0].time_e, $
  times               = UTC_TO_JULDAY(events.time_e)
  xTickUnits          = 'TIME'
  xTitle              = 'Time since ' + TIME_TO_STR(events[0].time_e,/MSEC)
  FOR i=0,nPlots-1 DO BEGIN
     plotArr[i]       = PLOT(times, $
                             datArr[*,i], $
                             NAME=nameArr[i], $
                             TITLE=title, $
                             YRANGE=[0.5,5.5], $
                             XTICKUNITS=xTickUnits, $
                             LINESTYLE='', $
                             SYMBOL=plotSym, $
                             SYM_COLOR=colArr[i], $
                             SYM_SIZE=symSize, $
                             XTITLE=xTitle, $
                             YTITLE='Spectral type', $
                             YTICKV=[1,2,3,4,5], $
                             YTICKNAME=nameArr, $
                             YMINOR=0, $
                             OVERPLOT=i GT 1, $
                             CURRENT=window)
  ENDFOR

  IF N_ELEMENTS(outName) GT 0 THEN BEGIN
     PRINT,'Saving to ' + outName + '...'
     window.save,outName
  ENDIF

END