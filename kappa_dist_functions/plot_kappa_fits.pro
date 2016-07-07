PRO PLOT_KAPPA_FITS,orig,kappaFit,gaussFit,oneCurve, $
                    ;; TITLE=title, $
                    BOUNDS_I=bounds_i, $
                    XRANGE=xRange, $
                    YRANGE=yRange, $
                    XLOG=xLog, $
                    YLOG=yLog, $
                    STRINGS=strings, $
                    SAVE_FITPLOTS=save_fitPlots, $
                    ;; PLOT_SAVENAME=plotSN, $
                    PLOTDIR=plotDir

  COMPILE_OPT idl2

  plotSN               = STRING(FORMAT='(A0,"--nFlux_fit--",A0,"--",A0,A0,"--orb_",I0,"__",A0,A0,".png")', $
                                strings.today, $
                                strings.timeFNStrs[bounds_i], $
                                strings.eeb_or_ees, $
                                strings.avgStr, $
                                strings.orbStr, $
                                strings.orbDate[bounds_i], $
                                strings.angleStr)

  title                = STRING(FORMAT='("Loss-cone e!U-!N # flux, (Orbit ",I0,", ",A0,")")', $
                                strings.orbStr, $
                                strings.orbDate[bounds_i])

  ;;plot things
  nPlots               = KEYWORD_SET(orig)+KEYWORD_SET(kappaFit)+KEYWORD_SET(gaussFit)+KEYWORD_SET(oneCurve)
  window               = WINDOW(DIMENSION=[800,600])
  plotArr              = MAKE_ARRAY(nPlots,/OBJ) 

  colorList            = LIST('RED','BLACK','BLUE','GRAY')

  ;;Silly string stuff
  xTitle               = "Energy (eV)"
  yTitle               = "Differential Energy Flux!C(eV/cm!U2!N-sr-s)"

  lineStyle            = ['','--','-.','-:']

  iPlot                = 0
  ;;Data
  plotArr[iPlot]       = PLOT(orig.x, $ ;x, $
                              orig.y, $
                              TITLE=title, $
                              NAME=orig.name, $
                              XTITLE=xTitle, $
                              YTITLE=yTitle, $
                              XRANGE=xRange, $
                              YRANGE=yRange, $
                              YLOG=1, $
                              XLOG=1, $
                              THICK=2.2, $
                              COLOR=colorList[0], $
                              ;; OVERPLOT=i GT 0, $
                              CURRENT=window) 
  
  iPlot++

  ;;Kappa fit
  IF N_ELEMENTS(kappaFit) GT 0 THEN BEGIN
     plotArr[iPlot]    = PLOT(kappaFit.x, $ ;x, $
                              kappaFit.y, $
                              ;; TITLE=title, $
                              NAME=kappaFit.name, $
                              ;; XTITLE=xTitle, $
                              ;; YTITLE=yTitle, $
                              ;; XRANGE=kappaFit.xRange, $
                              ;; YRANGE=kappaFit.yRange, $
                              ;; YLOG=kappaFit.yLog, $
                              ;; XLOG=kappaFit.xLog, $
                              THICK=2.2, $
                              LINESTYLE=lineStyle[1], $
                              COLOR=colorList[1], $
                              /OVERPLOT, $
                              CURRENT=window) 
     iPlot++
  ENDIF

  ;;GaussFit
  IF N_ELEMENTS(gaussFit) GT 0 THEN BEGIN
     plotArr[iPlot]    = PLOT(gaussFit.X, $
                              gaussFit.y, $
                              ;; TITLE=title, $
                              NAME=gaussFit.name, $
                              ;; XTITLE=xTitle, $
                              ;; YTITLE=yTitle, $
                              ;; XRANGE=gaussFit.xRange, $
                              ;; YRANGE=gaussFit.yRange, $
                              ;; YLOG=gaussFit.yLog, $
                              ;; XLOG=gaussFit.xLog1, $
                              THICK=2.2, $
                              LINESTYLE=lineStyle[2], $
                              COLOR=colorList[2], $
                              /OVERPLOT, $
                              CURRENT=window) 
     iPlot++
  ENDIF

  ;;OneCount curve
  IF N_ELEMENTS(oneCurve) GT 0 THEN BEGIN
     plotArr[iPlot]    = PLOT(oneCurve.x, $
                              oneCurve.y, $
                              NAME=oneCurve.name, $
                              THICK=2.2, $
                              LINESTYLE=lineStyle[3], $
                              COLOR=colorList[3], $
                              /OVERPLOT, $
                              CURRENT=window) 
     iPlot++
  ENDIF

  
  legend               = LEGEND(TARGET=plotArr[*],POSITION=[0.55,0.85],/NORMAL)

  IF KEYWORD_SET(add_fitParams_text) THEN BEGIN
     fitTitle          = ["Bulk energy (eV)","Plasma temp. (eV)","Kappa","Density (cm^-3)"]
     fitInfoStr        = [STRING(FORMAT='(F-15.2)',kappaFit.A[0]), $
                          STRING(FORMAT='(F-15.2)',kappaFit.A[1]), $
                          STRING(FORMAT='(F-7.3)',kappaFit.A[2]), $
                          STRING(FORMAT='(F-8.4)',kappaFit.A[3])]

     fitParamsText     = TEXT(0.2,0.25, $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                              STRING(FORMAT='("Fit success",T20,": ",A0)',(kappaFit.fitStatus EQ 0 ? 'Y' : 'N')), $
                              FONT_SIZE=10, $
                              FONT_NAME='Courier', $
                              /NORMAL)

     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        fitTitle       = ["Bulk energy (eV)","Plasma temp. (eV)","Kappa","Density (cm^-3)"]
        fitInfoStr     = [STRING(FORMAT='(F-15.2)',gaussFit.A[0]), $
                          STRING(FORMAT='(F-15.2)',gaussFit.A[1]), $
                          STRING(FORMAT='(F-7.3)',gaussFit.A[2]), $
                          STRING(FORMAT='(F-8.4)',gaussFit.A[3])]

        fitParamsText  = TEXT(0.52,0.25, $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                              STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                              STRING(FORMAT='("GaussFit success",T20,": ",A0)',(gaussFit.fitStatus EQ 0 ? 'Y' : 'N')), $
                              FONT_SIZE=10, $
                              FONT_NAME='Courier', $
                              /NORMAL, $
                              FONT_COLOR=colorList[2])
     ENDIF

  ENDIF

  IF KEYWORD_SET(save_fitplots) THEN BEGIN
     IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_SDT,/ADD_TODAY,/VERBOSE
     ENDIF
     PRINT,'Saving plot to ' + plotSN + '...'
     window.Save,plotDir+plotSN
     window.Close
  ENDIF

END