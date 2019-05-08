;2019/05/08
;; Ultimately want
;; Sumplots som viser: ion enspec (allangles),
;; (plot1) algorithmConic (ion up, ion down, up/down ratio),
;; (plot2) algorithmBeam (ion enBeam, ionEnHalf, beam/half ratio) and algorithm ident (conicAlgorithmtype1, conicAlgorithmtype2, beamAlgorithm)
;; ion pa

PRO TPLOT_BEAM_VS_HALFRANGE_ION_FLUXES, $
   tPlt_vars, $
   PEAKEVARNAME=peakEVarName, $
   ;; EBOUNDVARNAME=eBoundVarName, $
   ;; EBOUNDLOWVARNAME=eBoundLowVarName, $
   BEAMHALFRATIOSPECVARNAME=beamHalfRatioSpecVarName, $
   ;; UPDOWNRATIOSPECVARNAME=upDownRatioSpecVarName, $
   PLOT_ASCENDING_NORTH=plot_ascN, $
   PLOT_ASCENDING_SOUTH=plot_ascS, $
   PLOT_DESCENDING_NORTH=plot_descN, $
   PLOT_DESCENDING_SOUTH=plot_descS, $
   TLIMASCENDN=tLimNAscend, $
   TLIMDESCENDN=tLimNDescend, $
   TLIMS=tLimS, $
   TLIMASCENDS=tLimSAscend, $
   TLIMDESCENDS=tLimSDescend, $
   FNAMENASCEND=fNameAscendN, $
   FNAMENDESCEND=fNameDescendN, $
   FNAMESASCEND=fNameAscendS, $
   FNAMESDESCEND=fNameDescendS, $
   SAVETSTRN=saveTStrN, $
   SAVETSTRS=saveTStrS, $
   SAVETSTRASCENDN=saveTStrNAscend, $
   SAVETSTRDESCENDN=saveTStrNDescend, $
   SAVETSTRASCENDS=saveTStrSAscend, $
   SAVETSTRDESCENDS=saveTStrSDescend, $
   SAVE_PS=save_ps, $
   MAKE_SPECIAL_JGR_PLOT=make_special_JGR_plot, $
   SAVEPREF=savePref, $
   TIMEBARS=timeBars, $
   TIMEBAR_FROM_ION_BEAMS=timeBar_from_ion_beams, $
   IONEVENTS=ionEvents

  COMPILE_OPT IDL2,STRICTARRSUBS

  ctNum            = 43         ;better. no oceans of green

  red              = (ctNum EQ 43) ? 235 : 250
  darkRed          = 250
  green            = 130
  blue             = 90
  maxwell          = 50
  black            = 10
  poiple           = 40
  violet           = 60
  hvit             = 255

  nPlots = 0
  tLimList = LIST()
  pNameList = LIST()
  saveTStrList = LIST()

  IF KEYWORD_SET(plot_ascN) AND N_ELEMENTS(tLimNAscend) GT 0 THEN BEGIN
     nPlots++
     tLimList.Add,tLimNAscend
     pNameList.Add,fNameAscendN
     saveTStrList.Add,saveTStrNAscend
  ENDIF
  IF KEYWORD_SET(plot_descN) AND N_ELEMENTS(tLimNDescend) GT 0 THEN BEGIN
     nPlots++
     tLimList.Add,tLimNDescend
     pNameList.Add,fNameDescendN
     saveTStrList.Add,saveTStrNDescend
  ENDIF
  IF KEYWORD_SET(plot_ascS) AND N_ELEMENTS(tLimSAscend) GT 0 THEN BEGIN
     nPlots++
     tLimList.Add,tLimSAscend
     pNameList.Add,fNameAscendS
     saveTStrList.Add,saveTStrSAscend
  ENDIF
  IF KEYWORD_SET(plot_descS) AND N_ELEMENTS(tLimSDescend) GT 0 THEN BEGIN
     nPlots++
     tLimList.Add,tLimSDescend
     pNameList.Add,fNameDescendS
     saveTStrList.Add,saveTStrSDescend
  ENDIF

  pIdx = 0

  IF KEYWORD_SET(timeBars) THEN BEGIN

     IF KEYWORD_SET(timeBar_from_ion_beams) THEN BEGIN

        this = WHERE(ionEvents.newell.mono EQ 1 OR ionEvents.newell.mono EQ 2)

        IF this[0] EQ -1 THEN BEGIN
           tBars = !NULL
        ENDIF ELSE BEGIN

           GET_STREAKS,this, $
                       START_I=start_i, $
                       STOP_I=stop_i, $
                       ALLOWABLE_GAP=1, $
                       MIN_STREAK_TO_KEEP=7, $
                       OUT_STREAKLENS=streakLens, $
                       OUT_GAPLENS=gapLens

           tBars = !NULL
           FOR k=0,N_ELEMENTS(start_i)-1 DO tBars = [[tBars], $
                                                     [ionEvents.momsbeam.time[this[start_i[k]]], $
                                                      ionEvents.momsbeam.time[this[stop_i [k]]]]]

        ENDELSE

     ENDIF ELSE BEGIN
        tBars = timeBars
     ENDELSE

  ENDIF

  WHILE pIdx LT nPlots DO BEGIN

     IF ~KEYWORD_SET(save_ps) THEN BEGIN

        wInd = pIdx
        WINDOW,wInd,XSIZE=1200,YSIZE=600

     ENDIF ELSE BEGIN

        oldSize = !P.CHARSIZE
        oldSymSize = !P.SYMSIZE

        !P.CHARSIZE = 3.4
        !P.SYMSIZE  = 2.0

        fName = pNameList[pIdx]
        saveTStr = saveTStrList[pIdx]
        psNavn = fName.Replace(".sav","")

        IF N_ELEMENTS(plotDir) EQ 0 THEN BEGIN
           SET_PLOT_DIR,plotDir,/FOR_SDT,ADD_SUFF='/outflow_summaries'
        ENDIF

        count = 0
        WHILE FILE_TEST(plotDir+psNavn+(KEYWORD_SET(eps) ? '.eps' : '.ps')) DO BEGIN
           count++
           psNavn = STRING(FORMAT='(A0,A0,"-",I02)', $
                            savePref, $
                            saveTStr, $
                            count)
        ENDWHILE

        POPEN,plotDir+psNavn, $
              ;; /LAND, $
              /PORT, $
              ;; ASPECT=0.625, $
              FONT=-1, $
              ENCAPSULATED=eps  ;,XSIZE=8,YSIZE=7
        DEVICE,/PALATINO,FONT_SIZE=3

     ENDELSE

     ctNum            = 43
     LOADCT2,ctNum

     TPLOT,REVERSE(tPlt_vars),VAR=['ALT','ILAT','MLT'], $
           WINDOW=wInd, $
     TRANGE=tLimList[pIdx]

     TPLOT_PANEL,VARIABLE=beamHalfRatioSpecVarName,OPLOTVAR=peakEVarName
     ;; TPLOT_PANEL,VARIABLE=beamHalfRatioSpecVarName,OPLOTVAR=eBoundLowVarName

     IF KEYWORD_SET(timeBars) THEN BEGIN

        CASE NDIMEN(tBars) OF
           0:
           -1:
           1: BEGIN

              use_colours = 0
              colours = [poiple,darkRed,green,blue,poiple]
              use_lineStyles = 1
              lineStyles = [1,2,5,0] ;dotted, dashed, long dashes, solid

              FOR k=0,N_ELEMENTS(tBars)-1 DO BEGIN
                 TIMEBAR,tBars[k], $
                         THICK=3.0, $
                         LINESTYLE=(KEYWORD_SET(use_lineStyles) ? lineStyles[0] : !NULL), $
                         COLOR=(KEYWORD_SET(use_colours) ? colours[0] : !NULL)
              ENDFOR

           END
           2: BEGIN

              nHjar = N_ELEMENTS(tBars[0,*])
              ;; colours = GENERATE_LIST_OF_RANDOM_COLORS(nHjar)
              use_colours = 0
              colours = [poiple,darkRed,green,blue,poiple]
              use_lineStyles = 1
              lineStyles = [1,2,5,0] ;dotted, dashed, long dashes, solid

              FOR k=0,nHjar-1 DO BEGIN
                 ;; TIMEBAR,tBars[0,k],THICK=3.0,COLOR=(colours[k])[0]
                 ;; TIMEBAR,tBars[1,k],THICK=3.0,COLOR=(colours[k])[0]
                 TIMEBAR,tBars[0,k], $
                         LINESTYLE=(KEYWORD_SET(use_lineStyles) ? lineStyles[k] : !NULL), $
                         THICK=3.0, $
                         COLOR=(KEYWORD_SET(use_colours) ? colours[k] : !NULL)
                 TIMEBAR,tBars[1,k], $
                         LINESTYLE=(KEYWORD_SET(use_lineStyles) ? lineStyles[k] : !NULL), $
                         THICK=3.0, $
                         COLOR=(KEYWORD_SET(use_colours) ? colours[k] : !NULL)
              ENDFOR
              
           END
        ENDCASE

     ENDIF


     CASE 1 OF
        KEYWORD_SET(save_png): BEGIN
           CGPS_CLOSE
        END
        KEYWORD_SET(save_ps): BEGIN
           !P.CHARSIZE = TEMPORARY(oldSize)
           !P.SYMSIZE  = TEMPORARY(oldSymSize)

           PCLOSE
        END
        ELSE:
     ENDCASE

     pIdx++

  ENDWHILE

END
