PRO PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,evts,ioneVts, $
   TPLOT_NAME=tPlot_name, $
   NO_STRICT_TYPES=no_strict_types, $
   CONVERT_TO_NEWELL_INTERP=convert_to_Newell_interp, $
   FAVOR_BROADSTRICT_OVER_MONO=favor_broadStrict_over_mono, $
   SYMSIZE=symSize, $
   YTITLE=yTitle

  IF N_ELEMENTS(convert_to_Newell_interp) EQ 0 THEN convert_to_Newell_interp = 1

  IF KEYWORD_SET(convert_to_Newell_interp) OR KEYWORD_SET(favor_broadStrict_over_mono) THEN BEGIN
     CONVERT_ESPEC_TO_STRICT_NEWELL_INTERPRETATION,evts,events, $
        NB_CONVTOM_=nB_ConvToM_, $
        NB_CONVTOMS=nB_ConvToMS, $
        NBSCONVTOMS=nBSConvToMS, $
        NBTCONVTOMT=nBTConvToMT, $
        NM_CONVTOBS=nM_ConvToBS, $
        FAVOR_BROADSTRICT_OVER_MONO=favor_broadStrict_over_mono, $
        HUGE_STRUCTURE=huge_structure, $
        /HAS_NO_INFO_STRUCT, $
        /VERBOSE
  ENDIF ELSE BEGIN
     events = evts
  ENDELSE

  limits = {no_strict_types:KEYWORD_SET(no_strict_types)}

  IF ~KEYWORD_SET(tPlot_name) THEN tPlot_name = 'spectral_types'

    STORE_DATA,tPlot_name,DATA=events,LIMITS=limits
    OPTIONS,tPlot_name,'tplot_routine','plot_spectral_type__newell_et_al_2009__tplot'
    OPTIONS,tPlot_name,'ytitle',KEYWORD_SET(yTitle) ? yTitle : ' '
    OPTIONS,tPlot_name,'symsize',KEYWORD_SET(symSize) ? symSize : 0.7


    PRINT,'TPLOT NAME FOR IDENTIFIED EFLUXES: ' + tPlot_name
END
