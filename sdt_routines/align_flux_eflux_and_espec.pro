FUNCTION ALIGN_FLUX_EFLUX_AND_ESPEC,flux_time,flux_data, $
                                    eFlux_time,eFlux_data, $
                                    energySpec_time, $
                                    OUT_SC_POT=out_sc_pot, $
                                    OUT_SC_TIME=out_sc_time, $
                                    OUT_SC_MIN_ENERGY_IND=out_sc_min_energy_ind, $
                                    ORBSTR=orbStr, $
                                    FLUXSTRARR=fluxStrArr, $
                                    LOGFILE=logFile, $
                                    BATCH_MODE=batch_mode, $
                                    QUIET=quiet

  maxDiff                                     = 2.0 ;seconds

  IF ~KEYWORD_SET(names) THEN fluxStrArr = ['Flux','Energy flux','Energy spec']
  IF ~KEYWORD_SET(orbStr) THEN orbStr = '???'
  todayStr                                    = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  ;;Are we safe?
  nEFlux                                      = N_ELEMENTS(eFlux_time)
  nFlux                                       = N_ELEMENTS(flux_time)
  nEnergy_spec                                = N_ELEMENTS(energySpec_time)
  IF ( nEFlux   NE nFlux ) OR $
     ( nFlux NE nEnergy_spec ) OR $
     ( nFlux NE nEnergy_spec ) $
  THEN BEGIN
     success                                  = 0 ;Not throwing in the towel yet
     WRITE_MESSAGE_TO_LOGFILE,logFile, $
                              STRING(FORMAT='(A0,T20,A0,T40,"Unequal # of ",A0,"/",A0,"/",A0," inds")',orbStr,todayStr, $ 
                                     fluxStrArr[0],fluxStrArr[1],fluxStrArr[2]), $
                              /APPEND
     ;;We'll handle energy flux first. The energy spectrum is the gold standard
     tmpClosest                               = VALUE_CLOSEST(energySpec_time,eFlux_time,diffs,QUIET=quiet,BATCH_MODE=batch_mode)
     keep                                     = WHERE(ABS(diffs) LT maxDiff)
     IF keep[0] NE -1 THEN BEGIN
        eFlux_time                          = eFlux_time[keep]
        eFlux_data                          = eFlux_data[keep]
     ENDIF ELSE BEGIN
        WRITE_MESSAGE_TO_LOGFILE,logFile, $
                                 STRING(FORMAT='(A0,T20,A0,T40,A0)',orbStr,todayStr,'No ' + fluxStrArr[1] + $
                                        'inds within ' + STRCOMPRESS(maxDiff,/REMOVE_ALL) + ' s of ' + fluxStrArr[2]), $
                                 /APPEND
        success                               = 0
     ENDELSE

     ;;Now handle flux
     tmpClosest                               = VALUE_CLOSEST(energySpec_time,flux_time,diffs,QUIET=quiet,BATCH_MODE=batch_mode)
     keep                                     = WHERE(ABS(diffs) LT maxDiff)
     IF keep[0] NE -1 THEN BEGIN
        flux_time                        = flux_time[keep]
        flux_data                        = flux_data[keep]
        IF KEYWORD_SET(out_sc_pot) THEN out_sc_pot                        = out_sc_pot[keep]
        IF KEYWORD_SET(out_sc_time) THEN out_sc_time                      = out_sc_time[keep]
        IF KEYWORD_SET(out_sc_min_energy_ind) THEN out_sc_min_energy_ind  = out_sc_min_energy_ind[keep]
     ENDIF ELSE BEGIN
        WRITE_MESSAGE_TO_LOGFILE,logFile, $
                                 STRING(FORMAT='(A0,T20,A0,T40,A0)',orbStr,todayStr,'No ' + fluxStrArr[0] + $
                                        ' inds within ' + STRCOMPRESS(maxDiff,/REMOVE_ALL) + ' s of ' + fluxStrArr[2]), $
                                 /APPEND
        success                               = 1
     ENDELSE

     RETURN, success
  ENDIF ELSE BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,'Already aligned!'
     RETURN,1
  ENDELSE

END