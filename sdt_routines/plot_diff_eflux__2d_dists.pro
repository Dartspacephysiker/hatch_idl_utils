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
                              JUST_SAVE_ALL=just_save_all

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
                                 EPS=eps

  ENDFOR
END
