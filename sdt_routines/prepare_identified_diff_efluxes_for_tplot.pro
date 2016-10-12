PRO PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,events,TPLOT_NAME=tPlot_name,NO_STRICT_TYPES=no_strict_types

  limits = {no_strict_types:KEYWORD_SET(no_strict_types)}

  IF ~KEYWORD_SET(tPlot_name) THEN tPlot_name = 'spectral_types'

    STORE_DATA,tPlot_name,DATA=events,LIMITS=limits
    OPTIONS,tPlot_name,'tplot_routine','plot_spectral_type__newell_et_al_2009__tplot'

    PRINT,'TPLOT NAME FOR IDENTIFIED EFLUXES: ' + tPlot_name
END