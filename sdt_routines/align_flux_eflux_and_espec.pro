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

  maxDiff                                     = 5.0 ;seconds, one spin period

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
     CASE 1 OF
        (nEFlux LT nEnergy_spec): BEGIN
           junk =   {time:energySpec_time,comp1:MAKE_ARRAY(nEnergy_spec,/FLOAT),ncomp:1}
           eFlux = {time:eFlux_time,comp1:eFlux_data}
           
           
           ;; fields=combinets(magz,efield)
           FA_FIELDS_COMBINE,junk,eFlux,result=eFlux_interp,/interp,delt_t=50.,/TALK

           eFlux_time = energySpec_time
           eFlux_data = TEMPORARY(eFlux_interp)

        END
        (nEFlux GT nEnergy_spec): BEGIN
           tmpClosest                               = VALUE_CLOSEST(energySpec_time,eFlux_time,diffs,QUIET=quiet,BATCH_MODE=batch_mode)
           keep                                     = WHERE(ABS(diffs) LT maxDiff)
           IF keep[0] NE -1 THEN BEGIN
              eFlux_time                            = eFlux_time[keep]
              eFlux_data                            = eFlux_data[keep]
              success                               = 1
           ENDIF ELSE BEGIN
              WRITE_MESSAGE_TO_LOGFILE,logFile, $
                                       STRING(FORMAT='(A0,T20,A0,T40,A0)',orbStr,todayStr,'No ' + fluxStrArr[1] + $
                                              'inds within ' + STRCOMPRESS(maxDiff,/REMOVE_ALL) + ' s of ' + fluxStrArr[2]), $
                                       /APPEND
              success                               = 0
           ENDELSE
        END
        (nEFlux EQ nEnergy_spec): BEGIN

        END
     ENDCASE

     ;;Now handle flux
     CASE 1 OF
        (nFlux LT nEnergy_spec): BEGIN ;Interpolate, get data on same time scale
           junk =   {time:energySpec_time,comp1:MAKE_ARRAY(nEnergy_spec,/FLOAT),ncomp:1}
           flux = {time:flux_time,comp1:flux_data}
           sc_pot = {time:out_sc_time,comp1:out_sc_pot}
           sc_pot_ind = {time:out_sc_time,comp1:out_sc_min_energy_ind}
           
           ;; fields=combinets(magz,efield)
           FA_FIELDS_COMBINE,junk,flux,result=flux_interp,/interp,delt_t=50.,/TALK
           FA_FIELDS_COMBINE,junk,sc_pot,result=sc_pot_interp,/interp,delt_t=50.,/TALK
           FA_FIELDS_COMBINE,junk,sc_pot_ind,result=sc_pot_ind_interp,/interp,delt_t=50.,/TALK

           flux_time = energySpec_time
           flux_data = TEMPORARY(flux_interp)

           out_sc_time = energySpec_time
           out_sc_pot  = TEMPORARY(sc_pot_interp)
           out_sc_min_energy_ind = FLOOR(sc_pot_ind_interp) ;FLOOR because these indices are reversed. That is, the lowest energies are at the highest index values

        END
        (nFlux GT nEnergy_spec): BEGIN
           tmpClosest                               = VALUE_CLOSEST(energySpec_time,flux_time,diffs,QUIET=quiet,BATCH_MODE=batch_mode)
           keep                                     = WHERE(ABS(diffs) LT maxDiff)
           IF keep[0] NE -1 THEN BEGIN
              flux_time                        = flux_time[keep]
              flux_data                        = flux_data[keep]
              IF KEYWORD_SET(out_sc_pot) THEN out_sc_pot                        = out_sc_pot[keep]
              IF KEYWORD_SET(out_sc_time) THEN out_sc_time                      = out_sc_time[keep]
              IF KEYWORD_SET(out_sc_min_energy_ind) THEN out_sc_min_energy_ind  = out_sc_min_energy_ind[keep]

              success                               = 1
           ENDIF ELSE BEGIN
              WRITE_MESSAGE_TO_LOGFILE,logFile, $
                                       STRING(FORMAT='(A0,T20,A0,T40,A0)',orbStr,todayStr,'No ' + fluxStrArr[0] + $
                                              ' inds within ' + STRCOMPRESS(maxDiff,/REMOVE_ALL) + ' s of ' + fluxStrArr[2]), $
                                       /APPEND
              success                               = 0
           ENDELSE        
        END
        (nFlux EQ nEnergy_spec): BEGIN

        END
     ENDCASE


     RETURN, success
  ENDIF ELSE BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,'Already aligned!'
     RETURN,1
  ENDELSE

END