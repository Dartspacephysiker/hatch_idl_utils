;2018/12/31
PRO PLOT_DIFF_EFLUX__2D_DISTS,diff_eFlux, $
                              EEB_OR_EES=eeb_or_ees, $
                              ORBIT=orbit, $
                              STARTIND=startInd, $
                              STOPIND=stopInd, $
                              IND__STRIDE=stride, $
                              RETRACE=retrace,   $
                              ;; NPLOTS=nPlots, $
                              SPEC_AVG_INTVL=spec_avg_intvl, $
                              FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                              JUST_SAVE_ALL=just_save_all, $
                              COMBINE_PLOTS_IN_PDF=combine_plots_in_PDF

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; spec_avg_intvl   = !NULL

  dataOpt  = {eeb_or_ees    : eeb_or_ees, $
              spec_avg_intvl : N_ELEMENTS(spec_avg_intvl) GT 0 ? spec_avg_intvl : 0}
  orbStr   = STRING(FORMAT='(I0)',orbit)
  angleStr = ''
  
  dEF_strings = INIT_KAPPA_STRING_STRUCT(diff_eFlux, $
                                         orbStr, $
                                         angleStr, $
                                         dataOpt)
  
  
  ;; IF N_ELEMENTS(timeFNStr) EQ 0 THEN BEGIN
  ;;    ind = VALUE_CLOSEST2(diff_eFlux.time,curDataStr.time,/CONSTRAINED)
  ;;    timeFNStr = dEF_strings.timeFNStrs[ind]
  ;; ENDIF


  IF N_ELEMENTS(startInd) EQ 0 THEN startInd = 0

  IF N_ELEMENTS(stopInd) EQ 0 THEN BEGIN
     ;; IF N_ELEMENTS(nPlots) EQ 0 THEN nPlots = 1
     stopInd = startInd
  ENDIF
  
  IF N_ELEMENTS(stride) EQ 0 THEN BEGIN
     stride = 1
  ENDIF

  units = 'eflux'

  FOR k=startInd,stopInd,stride DO BEGIN

     curDataStr = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX__MCFADDEN_STYLE( $
                  diff_eFlux, $
                  k, $
                  HAS_SC_POT=has_sc_pot, $
                  UNITS=units)

     IF ~curDataStr.valid THEN BEGIN
        PRINT,T2S(curDataStr.time) + ': Invalid data!'
        CONTINUE
     ENDIF

     PLOT_FA_ESA_STRUCT__2D_DIST,curDataStr,fit2DStruct, $
                                 PLOTNAMEPREF=plotNamePref, $
                                 TIMEFNSTR=dEF_strings.timeFNStrs[k], $
                                 DEF_STRINGS=dEF_strings, $
                                 ;; ONLY_DATA=only_data, $ 
                                 ;; FOR_HORSESHOE_FIT=for_horseshoe_fit, $
                                 RETRACE=retrace, $
                                 PROMPT__CONT_TO_NEXT_FIT=prompt__cont_to_next_fit, $
                                 PROMPT__CONT_UNTIL_FIT_EQ=prompt__cont_until_fit_eq, $
                                 PROMPT__NTOT2DFITS=nTot2DFits, $
                                 FINISH_AND_SAVE_ALL=just_save_all, $
                                 KAPPA_FIT__SHOW__QUIT=show__quit, $
                                 FIT2D__PA_ZRANGE=fit2D__PA_zRange, $
                                 OUT_PLOTDIR=plotDir, $
                                 EPS=combine_plots_in_PDF

  ENDFOR

  IF KEYWORD_SET(combine_plots_in_PDF) THEN BEGIN

     plotTypeStr = '2DDists'
     nye_plotSuff = '_NYE'

     SHELLCMDINIT = 'export PS1=dude; . /home/spencerh/.bash_funcs;'
     nyPDF = STRING(FORMAT='("orb",A0,"_",A0)',dEF_strings.orbStr,plotTypeStr)+'.pdf'

     pdfCount = 0
     WHILE FILE_TEST(plotDir+'/' + nyPDF) DO BEGIN
        pdfCount++
        nyPDF = STRING(FORMAT='("orb",A0,"_",A0,"__",I02,A0)', $
                       dEF_strings.orbStr, $
                       plotTypeStr, $
                       pdfCount, $
                       '.pdf')
     ENDWHILE

     PRINT,"Converting all 1D plots into single pdf ..."
     SPAWN,SHELLCMDINIT + ' cd ' + plotDir + '; ' $
           + 'pwd; convert_and_unite_eps ' $
           + nyPDF $
           + " " + STRING(FORMAT='(A0)',"'*.eps'")

     ;; SPAWN,SHELLCMDINIT + ' cd ' + plotDir + '; ' $
     ;;       + 'mv ' + nyPDF + ' ../../'

     ;; mv the nye_plotSuffs to regular file thing
     SPAWN,SHELLCMDINIT + ' cd ' + plotDir + '; ' $
           + STRING(FORMAT='(A0,A0,A0,A0,A0)', $
                    'for brud in *', $
                    nye_plotSuff, $
                    '.eps; do mv ${brud} ${brud%%', $
                    nye_plotSuff, $
                    '.eps}.eps; done')

  ENDIF

END
