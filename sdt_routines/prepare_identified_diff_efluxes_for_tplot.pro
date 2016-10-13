PRO PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,evts,TPLOT_NAME=tPlot_name, $
   NO_STRICT_TYPES=no_strict_types, $
   CONVERT_TO_NEWELL_INTERP=convert_to_Newell_interp

  IF N_ELEMENTS(convert_to_Newell_interp) EQ 0 THEN convert_to_Newell_interp = 1

  IF KEYWORD_SET(convert_to_Newell_interp) THEN BEGIN
     CONVERT_ESPEC_TO_STRICT_NEWELL_INTERPRETATION,evts,events, $
        NB_CONVTOM_=nB_ConvToM_, $
        NB_CONVTOMS=nB_ConvToMS, $
        NBSCONVTOMS=nBSConvToMS, $
        NBTCONVTOMT=nBTConvToMT, $
        NM_CONVTOBS=nM_ConvToBS, $
        FAVOR_BROADSTRICT_OVER_MONO=favor_broadStrict_over_mono, $
        HUGE_STRUCTURE=huge_structure, $
        /VERBOSE
  ENDIF

  limits = {no_strict_types:KEYWORD_SET(no_strict_types)}

  IF ~KEYWORD_SET(tPlot_name) THEN tPlot_name = 'spectral_types'

    STORE_DATA,tPlot_name,DATA=events,LIMITS=limits
    OPTIONS,tPlot_name,'tplot_routine','plot_spectral_type__newell_et_al_2009__tplot'
    OPTIONS,tPlot_name,'ytitle',' '


    PRINT,'TPLOT NAME FOR IDENTIFIED EFLUXES: ' + tPlot_name
END