;2016/05/13
;X      = vector of energies in eV for which number flux, j(X), is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution
;;Assuming a field-aligned beam, only fit energies above peak energy

;;get yourself some data from orb 10000
;; @startup
;; timeStr = '99-3-2/18:08:42' & t=str_to_time(timeStr) &  dat = get_fa_ees(t) ; get electron esa survey
;; kappa_flux__fit_above_peak__bulkangle_0,TEMPERATURE=100,SDT_DAT=dat

;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;;From Kivelson & Russell, Table 2.2 "Properties of Typical Plasmas"
;;   ;;"Magnetosphere"
;;   T_def                                             = 1000L    ;eV
;;   n_def                                             = 1L       ;cm^-3
;;   E_b_def                                           = 500L     ;bulk energy, eV

;;   IF N_ELEMENTS(kappa)       EQ 0 THEN kappa        = 5        ;a guess
;;   IF N_ELEMENTS(T)           EQ 0 THEN T            = T_def    ;temperature guess in eV
;;   IF N_ELEMENTS(n_est)       EQ 0 THEN n_est        = n_def    ;n guess in cm^-3
;;   IF N_ELEMENTS(peak_energy) EQ 0 THEN peak_energy  = E_b_def  ;peak energy guess, eV    

;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;;From Orb 10000 at 18:08:46
;; inDir                                             = '/SPENCEdata/software/sdt/batch_jobs/20160420--fit_Maxwellians_kappas_for_inverted_Vs/'
;; restFile                                          = inDir + 'nFlux_and_eSpec--orb_10000__18_08_36-18_09_00.sav'
;; RESTORE,restFile


PRO KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0__EFLUX_UNITS, $ ;X,A,F,pders, $
   T1=t1, $
   T2=t2, $
   ENERGY_ELECTRONS=energy_electrons, $
   LOAD_DAT_FROM_FILE=loadFile, $
   EEB_OR_EES=eeb_or_ees, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   SDT_TIME_INDS=bounds, $
   DO_ALL_TIMES=do_all_times, $
   MIN_PEAK_ENERGY=min_peak_energy, $
   DENSITY_EST=n_est, $
   TEMPERATURE_EST=T, $
   KAPPA_EST=kappa, $
   SDT_DAT=dat, $
   BULK_OFFSET=bulk_offset, $
   ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
   ESTIMATE_FACTORS=estFacs, $
   TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
   N_ENERGIES_BELOW_PEAK=n_below_peak, $
   N_ENERGIES_AFTER_PEAK=n_after_peak, $
   CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
   FIT_TOLERANCE=fit_tol, $
   MAX_ITERATIONS=max_iter, $
   ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
   USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
   ADD_ONECOUNT_CURVE=add_oneCount_curve, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
   ELECTRON_ANGLERANGE=electron_angleRange, $
   GET_MASS_AND_DT=get_mass_and_dt, $
   NO_PLOTS=no_plots, $
   SAVE_FITPLOTS=save_fitplots, $
   PLOT_FULL_FIT=plot_full_fit, $
   PLOTDIR=plotDir, $
   OUTPUT_DENSITY_ESTIMATES=output_density_estimates, $
   OUTPUT_DENSITY__ERANGE=dens_est_eRange, $
   OUTPUT_DENS__ENERGIES=output_dens__energies, $
   OUTPUT_DENS__ANGLES=output_dens__angles, $
   OUT_DENS_STRUCT=out_dens, $
   OUT_PEAK_DENS_STRUCT=out_peak_dens, $
   ONLY_DENS_ESTIMATES=only_dens_estimates, $
   OUT_FITTED_PARAMS=out_fitted_params, $
   OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
   OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
   OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
   ADD_FULL_FITS=add_full_fits, $
   OUT_ERANGE_PEAK=out_eRange_peak, $
   OUT_PARAMSTR=out_paramStr, $
   TXTOUTPUTDIR=txtOutputDir
  
  COMPILE_OPT idl2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Defaults
  KAPPA_FIT_DEFAULTS, $
     KAPPA=kappa, $
     BOUNDS=bounds, $
     ;; DO_ALL_TIMES=do_all_times, $
     EEB_OR_EES=eeb_or_ees, $
     SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
     ROUTINE=routine, $
     TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
     N_ENERGIES_BELOW_PEAK=n_below_peak, $
     N_ENERGIES_AFTER_PEAK=n_after_peak, $
     ENERGY_ELECTRONS=energy_electrons, $
     ESTIMATE_A_FROM_DATA=estimate_A_from_data

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Get data
  IF N_ELEMENTS(eSpec) EQ 0 OR N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN
     GET_LOSSCONE_EN_SPEC_AND_NFLUX_DATA,T1=t1,T2=t2, $
                                         LOAD_DAT_FROM_FILE=loadFile, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         EN_SPEC=eSpec, $
                                         SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                         DIFF_EFLUX=diff_eFlux, $
                                         JE_EN=je_en, $
                                         OUT_ORB=orb, $
                                         OUT_ANGLERANGE=e_angle, $
                                         ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                         CUSTOM_E_ANGLERANGE=electron_angleRange, $
                                         ANGLESTR=angleStr, $
                                         ELECTRON_ENERGY_LIMS=energy_electrons, $
                                         /GET_MASS_AND_DT, $
                                         OUT_MASS=mass

     IF ~KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        REDUCE_DIFF_EFLUX,diff_eFlux
     ENDIF

     orbStr                            = STRCOMPRESS(orb,/REMOVE_ALL)
  ENDIF

  IF KEYWORD_SET(do_all_times) THEN BEGIN
     PRINT,"Doing all times ..."
     bounds                            = INDGEN(N_ELEMENTS(diff_eFlux.time))
  END

  ;;Onecount curve?
  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
     GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2, $
                                   EEB_OR_EES=eeb_or_ees, $
                                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                   SDT_NAME=dEF_oneCount_name, $
                                   ANGLE=e_angle, $
                                   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                   OUT_ONEDAT=out_oneDat, $
                                   QUIET=quiet

     GET_DATA,dEF_oneCount_name,DATA=dEF_oneCount

     IF ~KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        REDUCE_DIFF_EFLUX,dEF_oneCount
     ENDIF
  ENDIF

  ;;Times and strings
  times                                = diff_eFlux.time
  strings                              = {today:GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                          eeb_or_ees:eeb_or_ees, $
                                          avgStr:KEYWORD_SET(spectra_average_interval) ? $
                                          STRING(FORMAT='("--",I0,"_avgs")',spectra_average_interval) : '' , $
                                          orbStr:orbStr, $
                                          orbDate:STRMID(TIME_TO_STR(diff_eFlux.time),0,10), $
                                          yearStr:STRMID(TIME_TO_STR(times[0],/MSEC),0,10), $
                                          timeStrs:STRMID(TIME_TO_STR(times,/MSEC),11,11), $
                                          timeFNStrs:STRMID(TIME_TO_STR(times,/MSEC),11,11), $
                                          plotTimes:STRMID(TIME_TO_STR(diff_eFlux.time,/MSEC),11,12), $
                                          angleStr:angleStr}
  strings.timeFNStrs                   = strings.timeFNStrs.REPLACE(':', '_')
  strings.timeFNStrs                   = strings.timeFNStrs.REPLACE('.', '__')


  ;;Loop over provided indices, plot data as well as fit, and optionally save
  routine                              = 'get_fa_'+eeb_or_ees
  IF KEYWORD_SET(spectra_average_interval) THEN routine += '_ts'

  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
     dEF_oneCountMod                   = dEF_oneCount.y[bounds,*]
     yMin                              = MIN(dEF_oneCountMod[WHERE(dEF_oneCountMod GT 0)])
     yMin                              = 10.^(FLOOR(ALOG10(yMin)))
  ENDIF ELSE BEGIN
     yMin                              = MIN(diff_eFlux.y[WHERE(diff_eFlux.y GT 0)])
  ENDELSE

  energies                             = TRANSPOSE(diff_eFlux.x)
  data                                 = TRANSPOSE(diff_eFlux.y)
  oneCount_data                        = KEYWORD_SET(add_oneCount_curve) ? TRANSPOSE(dEF_oneCount.y) : !NULL
  angles                               = diff_eFlux.angles

  KAPPA_FIT__LOOP,times,energies,data,oneCount_data,angles, $
                  USING_SDT_DATA=0, $
                  KAPPA=kappa, $
                  BOUNDS=bounds, $
                  EEB_OR_EES=eeb_or_ees, $
                  SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                  ROUTINE=routine, $
                  ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                  USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                  ESTFACS=estFacs, $
                  TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                  N_ENERGIES_BELOW_PEAK=n_below_peak, $
                  N_ENERGIES_AFTER_PEAK=n_after_peak, $
                  ENERGY_ELECTRONS=energy_electrons, $
                  ESTIMATE_A_FROM_DATA=estimate_A_from_data, $
                  E_ANGLE=e_angle, $
                  BULK_OFFSET=bulk_offset, $
                  CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
                  MIN_PEAK_ENERGY=min_peak_energy, $
                  STRINGS=strings, $
                  FIT_TOLERANCE=fit_tol, $
                  MAX_ITERATIONS=max_iter, $
                  ADD_ONECOUNT_CURVE=add_oneCount_curve, $
                  ADD_FITPARAMS_TEXT=add_fitParams_text, $
                  ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                  ELECTRON_ANGLERANGE=electron_angleRange, $
                  NO_PLOTS=no_plots, $
                  SAVE_FITPLOTS=save_fitplots, $
                  PLOT_FULL_FIT=plot_full_fit, $
                  PLOTDIR=plotDir, $
                  OUTPUT_DENSITY_ESTIMATES=output_density_estimates, $
                  OUTPUT_DENSITY__ERANGE=dens_est_eRange, $
                  OUTPUT_DENS__ENERGIES=output_dens__energies, $
                  OUTPUT_DENS__ANGLES=output_dens__angles, $
                  OUT_DENS_STRUCT=out_dens, $
                  OUT_PEAK_DENS_STRUCT=out_peak_dens, $
                  ONLY_DENS_ESTIMATES=only_dens_estimates, $
                  OUT_FITTED_PARAMS=out_fitted_params, $
                  OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
                  OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
                  OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
                  ADD_FULL_FITS=add_full_fits, $
                  OUT_ERANGE_PEAK=out_eRange_peak, $
                  OUT_PARAMSTR=out_paramStr, $
                  TXTOUTPUTDIR=txtOutputDir

  PRINT,"DONE!"

END
