PRO   KAPPA_FIT_DEFAULTS, $
   KAPPA=kappa, $
   BOUNDS=bounds, $
   EEB_OR_EES=eeb_or_ees, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   ROUTINE=routine, $
   TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
   N_ENERGIES_BELOW_PEAK=n_below_peak, $
   N_ENERGIES_AFTER_PEAK=n_after_peak, $
   ENERGY_ELECTRONS=energy_electrons, $
   ESTIMATE_A_FROM_DATA=estimate_A_from_data

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(kappa) THEN BEGIN
     kappa                     = 3.0 ;Why not?
     PRINT,FORMAT='("Default kappa estimate    : ",F-10.4)',kappa
  ENDIF

  IF ~KEYWORD_SET(bounds) THEN BEGIN
     bounds                    = 0 ;just do one
  ENDIF

  IF ~KEYWORD_SET(eeb_or_ees) THEN BEGIN
     eeb_or_ees                = 'ees'
  ENDIF

  routine                      = 'get_fa_'+eeb_or_ees
  IF KEYWORD_SET(spectra_average_interval) THEN routine += '_ts'

  IF N_ELEMENTS(trim_energies_below_peak) EQ 0 THEN BEGIN
     trim_energies_below_peak  = 1

     IF N_ELEMENTS(n_below_peak) EQ 0 THEN BEGIN
        n_below_peak           = 4
     ENDIF
     IF N_ELEMENTS(n_after_peak) EQ 0 THEN BEGIN
        n_after_peak           = 10
     ENDIF

  ENDIF

  IF ~KEYWORD_SET(energy_electrons) THEN BEGIN
     energy_electrons          = [3e1,3.6e4]
  ENDIF

  IF N_ELEMENTS(estimate_A_from_data) EQ 0 THEN BEGIN
     PRINT,'Estimating fit params from SDT data...'
     estimate_A_from_data      = 1
  ENDIF


END