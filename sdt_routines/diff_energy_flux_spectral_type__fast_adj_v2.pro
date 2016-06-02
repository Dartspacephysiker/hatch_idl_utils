;+
;2016/05/20 A FAST version of the algorithm presented by Newell et al. [2009]: "Diffuse, monoenergetic, and broadband aurora: The
;           global precipitation budget", based on conversations with Jim.
;           The operating assumption is that our energy bins are about twice as fine
;2016/05/30 This version incorporates some failure codes so that we know what exactly happened. Here's how it works:
;           MONO events
;           ===========
;           If mono 
;
;           BROAD events
;           ============
;Each event has these variables associated with it:
;event = { time:UTC seconds, $
;      	  eSpec_problems:INT(0), $       ;0 = no problems      , N = N bad bins
;         monoenergetic:INT(0), $        ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, 255-N = step N where algorithm failed
;         broadband:INT(0), $            ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband    , 255-N = step N where algorithm failed
;	  diffuse:INT(0), $              ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
;	  Je:DOUBLE(0), $                ;Electron number flux (#/cm^2-s)
;	  Jee:DOUBLE(0), $               ;Electron energy flux (mW/m^2)
;	  Ji:DOUBLE(0), $                ;Ion number flux      (#/cm^2-s)
;	  Jei:DOUBLE(0)}                 ;Ion energy flux      (mW/m^2)
;-
FUNCTION DIFF_ENERGY_FLUX_SPECTRAL_TYPE__FAST_ADJ_V2,eSpec,Je,Jee, $
   mlt,ilat, $
   PRODUCE_FAILCODE_OUTPUT=produce_failCodes, $
   OUT_FAILCODES=failCodes, $
   BATCH_MODE=batch_mode, $
   QUIET=quiet, $
   ERRORMSG=errorMsg            ;, $
   ;; iSpec,Ji,Jei, $
  ;; ORBSTR=orbStr, $
  ;; INCLUDE_IONS=include_ions

  COMPILE_OPT idl2

  DEF_ST_VERSION_NUM                      = BYTE(2)

  ;;return control to caller if no one is watching
  ;; IF KEYWORD_SET(batch_mode) THEN BEGIN
  ;;    ON_ERROR, 2
  ;; ENDIF

  ;;A little error checking
  IF N_ELEMENTS(eSpec) EQ 0 THEN BEGIN
     ;; IF KEYWORD_SET(errorLogFile) THEN BEGIN
     ;; WRITE_MESSAGE_TO_LOGFILE,errorLogFile, $
     ;;                          STRING(FORMAT='(A0,T20,A0,T40,A0)',KEYWORD_SET(orbStr) ? orbStr : '???', $
     ;;                                 GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
     ;;                                 'DIFF_ENERGY_FLUX_SPECTRAL_TYPE: N_ELEMENTS(eSpec) EQ 0!'), $
     ;;                          /APPEND
     errorMsg                             = 'DIFF_ENERGY_FLUX_SPECTRAL_TYPE: N_ELEMENTS(eSpec) EQ 0!'
     ;; ENDIF
     RETURN, -1
  ENDIF
  IF N_ELEMENTS(eSpec.v) LE 1 OR N_ELEMENTS(eSpec.y) LE 1 THEN BEGIN
     ;; IF KEYWORD_SET(errorLogFile) THEN BEGIN
     ;;    WRITE_MESSAGE_TO_LOGFILE,errorLogFile, $
     ;;                             STRING(FORMAT='(A0,T20,A0,T40,A0)',KEYWORD_SET(orbStr) ? orbStr : '???', $
     ;;                                    GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
     ;;                                    'DIFF_ENERGY_FLUX_SPECTRAL_TYPE: N_ELEMENTS(eSpec.{v and/or y}) LE 1!'), $
     ;;                             /APPEND
     ;; ENDIF
     errorMsg                             = 'DIFF_ENERGY_FLUX_SPECTRAL_TYPE: N_ELEMENTS(eSpec.{v and/or y}) LE 1!'
     RETURN, -1
  ENDIF

  time_e                                  = eSpec.x
  energy_e                                = eSpec.v
  spec_e                                  = eSpec.y
  chare                                   = ABS(Jee/Je)*6.242*1.0e11

  ;;Check spectra integrity
  bad_elec_i                              = WHERE(~FINITE(spec_e),nBad_e)
  IF nBad_e GT 0 THEN BEGIN
     IF nBad_e GT 255 THEN BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,'nBad_e is too big to be assigned to a byte variable! Beware!'
        IF KEYWORD_SET(batch_mode) THEN BEGIN
           nBad_e                         = 255
        ENDIF ELSE BEGIN
           STOP
        ENDELSE
     ENDIF
     spec_e[bad_elec_i]                   = 0.
  ENDIF

  ;; IF KEYWORD_SET(include_ions) THEN BEGIN
  

  ;;    bad_ion_i                         = WHERE(~FINITE(iSpec),nBad_i)
  ;;    IF nBad_i GT 0 THEN BEGIN
  ;;       IF nBad_i GT 255 THEN BEGIN
  ;;          PRINT,'nBad_i is too big to be assigned to a byte variable! Beware!'
  ;;          IF KEYWORD_SET(batch_mode) THEN BEGIN
  ;;             nBad_i                   = 255
  ;;          ENDIF ELSE BEGIN
  ;;             STOP
  ;;          ENDELSE
  ;;       ENDIF
  ;;       iSpec[bad_ion_i]               = 0.
  ;;    ENDIF
  ;; ENDIF

  THRESH_PC                               = 0.3
  STRICT_THRESH_PC                        = 0.1
  MONO_BROAD_CHARE_THRESHOLD              = 80

  MONO_MIN_PEAK_ENERGY                    = 100

  CUSP_MIN_EV                             = 300
  NOT_CUSP_MIN_EV                         = 140
  MIN_CHANS_FOR_BROAD                     = 6
  MIN_CHANS_FOR_BROAD_STRICT              = 8
  MONO_N_TO_CHECK                         = 4

  ;;Fail codes
  mFCode                                  = BYTE(0)
  bFCode                                  = BYTE(0)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MONOENERGETIC EVENTS
  ;;I. Identify peak in differential energy flux
  peakFlux                                = MAX(spec_e,peakFlux_ind)

  peakEnergy                              = energy_e[peakFlux_ind]
  IF ~KEYWORD_SET(quiet) THEN PRINT,'Peak flux: ' + STRCOMPRESS(peakFlux,/REMOVE_ALL) + ' eV'
  IF ~KEYWORD_SET(quiet) THEN PRINT,'Peak energy: ' + STRCOMPRESS(peakEnergy,/REMOVE_ALL) + ' eV'
  ;;**
  ;;1. Examine drops in energy channels one and two levels above the peak (or only below, if at the highest energy)
  ;;    i.  If flux_drop_below AND flux_drop_above are 30% or less of the peak, continue to 2b.
  ;;    ii. Else, not monoenergetic. Monoenergetic equals -1
  ;;    NOTE: Assuming a Maxwellian, the drop would be ~80% on either side. 30% is well below Maxwellian.
  above_i                                 = WHERE(energy_e GT peakEnergy,nAbove)
  below_i                                 = WHERE(energy_e LT peakEnergy,nBelow)
  nAbove                                  = nAbove < MONO_N_TO_CHECK
  nBelow                                  = nBelow < MONO_N_TO_CHECK
  mono                                    = 0
  CASE nAbove OF
     0: BEGIN
        cont                              = 1
        strictMono_candidate_above        = 1
     END
     1: BEGIN
        cont                              = (spec_e[peakFlux_ind+1]/peakFlux) LE THRESH_PC 
        IF ~cont THEN BEGIN
           mono                           = -1
           strictMono_candidate_above     = 0
        ENDIF ELSE BEGIN        ;check how awesome
           strictMono_candidate_above     = (spec_e[peakFlux_ind+1]/peakFlux) LE STRICT_THRESH_PC
        ENDELSE
     END
     2: BEGIN
        cont                              = ( (spec_e[peakFlux_ind+1]/peakFlux) LE THRESH_PC) + $
                                      ( (spec_e[peakFlux_ind+2]/peakFlux) LE THRESH_PC )
        IF ~cont THEN BEGIN
           mono                           = -1
           strictMono_candidate_above     = 0
        ENDIF ELSE BEGIN        ;check how awesome
           strictMono_candidate_above     = ( (spec_e[peakFlux_ind+1]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind+2]/peakFlux) LE STRICT_THRESH_PC )
        ENDELSE
     END
     3: BEGIN
        cont                              = ( (spec_e[peakFlux_ind+1]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind+2]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind+3]/peakFlux) LE THRESH_PC )
        IF ~cont THEN BEGIN
           mono                           = -1
           strictMono_candidate_above     = 0
        ENDIF ELSE BEGIN        ;check how awesome
           strictMono_candidate_above     = ( (spec_e[peakFlux_ind+1]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind+2]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind+3]/peakFlux) LE STRICT_THRESH_PC )
        ENDELSE
     END
     4: BEGIN
        cont                              = ( (spec_e[peakFlux_ind+1]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind+2]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind+3]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind+4]/peakFlux) LE THRESH_PC )
        IF ~cont THEN BEGIN
           mono                           = -1
           strictMono_candidate_above     = 0
        ENDIF  ELSE BEGIN       ;check how awesome
           strictMono_candidate_above     = ( (spec_e[peakFlux_ind+1]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                            ( (spec_e[peakFlux_ind+2]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                            ( (spec_e[peakFlux_ind+3]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                            ( (spec_e[peakFlux_ind+4]/peakFlux) LE STRICT_THRESH_PC )
        ENDELSE
     END
  ENDCASE
  mono_nAbove                             = cont
  IF mono EQ -1 AND KEYWORD_SET(produce_failCodes) THEN BEGIN
     mFCode                               = mFCode OR 1
     cont                                 = 1
  ENDIF

  IF cont THEN BEGIN
     CASE nBelow OF
        0: BEGIN
           cont                           = 1
           strictMono_candidate_below     = 1
        END
        1: BEGIN
           cont                           = (spec_e[peakFlux_ind-1]/peakFlux) LE THRESH_PC 
           IF ~cont THEN BEGIN
              mono                        = -1 
              strictMono_candidate_below  = 0
           ENDIF ELSE BEGIN     ;check how awesome
              strictMono_candidate_below  = (spec_e[peakFlux_ind-1]/peakFlux) LE STRICT_THRESH_PC
           ENDELSE
        END
        2: BEGIN
           cont                           = ( (spec_e[peakFlux_ind-1]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind-2]/peakFlux) LE THRESH_PC )
           IF ~cont THEN BEGIN
              mono                        = -1 
              strictMono_candidate_below  = 0
           ENDIF ELSE BEGIN     ;check how awesome
              strictMono_candidate_below  = ( (spec_e[peakFlux_ind-1]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind-2]/peakFlux) LE STRICT_THRESH_PC )
           ENDELSE
        END
        3: BEGIN
           cont                           = ( (spec_e[peakFlux_ind-1]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind-2]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind-3]/peakFlux) LE THRESH_PC )
           IF ~cont THEN BEGIN
              mono                        = -1
              strictMono_candidate_below  = 0
           ENDIF ELSE BEGIN     ;check how awesome
              strictMono_candidate_below  = ( (spec_e[peakFlux_ind-1]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind-2]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind-3]/peakFlux) LE STRICT_THRESH_PC )
           ENDELSE
        END
        4: BEGIN
           cont                           = ( (spec_e[peakFlux_ind-1]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind-2]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind-3]/peakFlux) LE THRESH_PC ) + $
                                      ( (spec_e[peakFlux_ind-4]/peakFlux) LE THRESH_PC )
           IF ~cont THEN BEGIN
              mono                        = -1
              strictMono_candidate_below  = 0
           ENDIF ELSE BEGIN     ;check how awesome
              strictMono_candidate_below  = ( (spec_e[peakFlux_ind-1]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind-2]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind-3]/peakFlux) LE STRICT_THRESH_PC ) OR $
                                      ( (spec_e[peakFlux_ind-4]/peakFlux) LE STRICT_THRESH_PC )
           ENDELSE
        END
     ENDCASE
  ENDIF
  mono_nBelow                             = cont
  IF mono EQ -1 AND KEYWORD_SET(produce_failCodes) THEN BEGIN
     mFCode                               = mFCode OR 1
     cont                                 = 1
  ENDIF

  ;;**
  ;;2.  Is peak_flux GE 1.0e8 eV/cm^2-sr-eV?
  ;;    i.  If so, continue to 3
  ;;    ii. Else, not monoenergetic. Monoenergetic EQ -2
  ;;    NOTE: They reference Newell et al. [1996b] here, saying it is the smallest threshold that seems to exclude homogeneous aurora
  IF cont THEN BEGIN
     cont                                 = peakFlux GE 1.0e8
     IF ~cont THEN BEGIN
        mono                              = -2
        mFCode                            = mFCode OR 2
        IF KEYWORD_SET(produce_failCodes) THEN BEGIN
           cont                           = 1
        ENDIF
     ENDIF
  ENDIF

  ;;**
  ;;3. Identify average energy. Is Eavg GT 80 eV?
  ;;   i.   YES: Continue to 4
  ;;   ii.  NO : Not monoenergetic. Monoenergetic EQ -3
  IF cont THEN BEGIN
     cont                                 = chare GE MONO_BROAD_CHARE_THRESHOLD
     IF ~cont THEN mono                   = -3
     mFCode                               = mFCode OR 4
     IF KEYWORD_SET(produce_failCodes) THEN BEGIN
        cont                              = 1
     ENDIF
  ENDIF

  ;;**
  ;;4. Get peakEnergy. Is peakEnergy GE 100 eV? 
  ;;   i.   YES: Monoenergetic! Go to 5.
  ;;   ii.  NO : Not monoenergetic. Monoenergetic EQ -4
  IF cont THEN BEGIN
     cont                                 = peakEnergy GE MONO_MIN_PEAK_ENERGY
     IF ~cont THEN BEGIN
        mono                              = -4
        mFCode                            = mFCode OR 8
     ENDIF
  ENDIF

  ;;**
  ;;5. Are flux_drop_below AND flux_drop_above LE 10% of peak_flux?
  ;;   i.   YES: strict_monoenergetic! Monoenergetic EQ 2
  ;;   ii.  NO : Not strict_monoenergetic! Monoenergetic EQ 1.
  IF cont THEN BEGIN
     mono                                 = 1 + (KEYWORD_SET(strictMono_candidate_above) AND KEYWORD_SET(strictMono_candidate_below))
  ENDIF
  ;;DONE with monoenergetic


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;BROADBAND EVENTS
  ;;****************
  ;;I. Calculate dJ_E/dE
  ;; dSpec_E                              = DERIV(energy_e,spec_e)
  ;; dSpec_E[bad_elec_i]                  = 0.                 ;I guess?

  ;;**
  ;;1. Get N_good bins WHERE(dJ_E/dE > 2.0e8 eV/cm^2-sr-eV)
  ;;   i.   N_good GE 3 ? Continue to 2.
  ;;   ii.  Else, not broadband. Broadband EQ -1
  ;; broad_i                              = WHERE(dSpec_E GT 2.0e8,nBroad)
  broad_i                                 = WHERE(spec_e GT 2.0e8,nBroad)
  cont                                    = nBroad GE MIN_CHANS_FOR_BROAD
  IF ~cont THEN BEGIN
     broad                                = -1
     bFCode                               = bFCode OR 1
     IF KEYWORD_SET(produce_failCodes) THEN BEGIN
        cont                              = 1
     ENDIF
  ENDIF

  ;;**
  ;;2.  Calculate Eavg
  ;;   i.   Eavg GT 80 eV ? Continue to 3.
  ;;   ii.  Else, not broadband. Broadband EQ -2
  IF cont THEN BEGIN
     cont                                 = chare GE MONO_BROAD_CHARE_THRESHOLD
     IF ~cont THEN BEGIN
        broad                             = -2
        bFCode                            = bFCode OR 2
        IF KEYWORD_SET(produce_failCodes) THEN BEGIN
           cont                           = 1
        ENDIF
     ENDIF
  ENDIF

  ;;II. Determine our MLT: Is 9.5 < MLT < 14.5 ? 
  ;;   i.   YES: We're in the cusp; min eV EQ 300 eV
  ;;   ii.  NO : Not in cusp;       min_eV EQ 140 eV
  min_eV                                  = (MLT GT 9.5 AND MLT LT 14.5) ? CUSP_MIN_EV : NOT_CUSP_MIN_EV

  ;;**
  ;;3. Get N_broad_GE_min_eV(=number of N_good energies at or above min_eV). N_broad_GE_min_eV GE 1?
  ;;   i.   YES: broadband! Go to 4.
  ;;   ii.  NO : Not broadband. Broadband EQ -3
  IF cont THEN BEGIN
     wild_ii                              = WHERE(energy_e[broad_i] GE min_eV,N_broad_GE_min_eV)
     IF N_broad_GE_min_eV GE 1 THEN BEGIN
        ;; strictBroad                    = nBroad GE MIN_CHANS_FOR_BROAD_STRICT
        broad                             = 1 + (nBroad GE MIN_CHANS_FOR_BROAD_STRICT)
     ENDIF ELSE BEGIN
        broad                             = -3
        bFCode                            = bFCode OR 4
        IF KEYWORD_SET(produce_failCodes) THEN BEGIN
           cont                           = 1
        ENDIF
     ENDELSE
  ENDIF

  ;;DIFFUSE AURORA (Events identified as NEITHER monoenergetic NOR broadband)
  ;;*************************************************************************
  ;;**
  ;;1. Flag as diffuse
  ;;   i.   Do electron extrapolation
  ;;   ii.  Event is diffuse
  IF mono LT 0 AND broad LT 0 THEN BEGIN
     diffuse                              = 1
     ;; EXTRAPOLATE_SPECTRUM_TO_50KEV,energy_e,spec_e,extrapolated_e_spectrum
     ;; RECALCULATE_50KEV_FLUXES,extrapolated_e_spectrum,Je,Jee
  ENDIF ELSE BEGIN
     diffuse                              = 0
  ENDELSE 

  ;;ELECTRON EXTRAPOLATION
  ;;**********************
  ;;1. Assume Maxwellian, calculate flux at 50 keV based on Maxwellian fit
  ;;   i.   Set T_ion                     = 0.5 * peak_ion_energy (NOT peak_ion_flux!) (Maxwellian assumption)
  ;;   ii.  Based on Maxwellian assumption, extrapolate {#,energy} flux at extrapolated energy bin up to 50 keV (or one energy step above highest bin)
  ;;
  IF diffuse THEN BEGIN
     ;; EXTRAPOLATE_SPECTRUM_TO_50KEV,energy,spectrum,extrapolated_e_spectrum
     ;; RECALCULATE_50KEV_FLUXES,extrapolated_e_spectrum,Je,Jee
  ENDIF
  
  ;;adjust mono and broad just in case
  IF KEYWORD_SET(produce_failCodes) THEN BEGIN
     IF mono  LT 0 THEN mono              = 250
     IF broad LT 0 THEN broad             = 250
  ENDIF ELSE BEGIN
     IF mono  LT 0 THEN mono              = 255 + mono
     IF broad LT 0 THEN broad             = 255 + broad
  ENDELSE

  ;;ION AURORA
  ;;**********
  ;;1. Determine differential energy flux peak, peak_ion_flux. 
  ;;   i.   Set T_ion                     = 0.5 * peak_ion_energy (NOT peak_ion_flux!) (Maxwellian assumption)
  ;;2. Extrapolation
  ;;   i.   Based on Maxwellian assumption, extrapolate {#,energy} flux at extrapolated energy bin up to 50 keV (or one energy step above highest bin)
  ;; EXTRAPOLATE_SPECTRUM_TO_50KEV,energy_ions,ion_spectrum,extrapolated_i_spectrum
  ;; RECALCULATE_50KEV_FLUXES,extrapolated_e_spectrum,Ji,Jei
  ;; IF KEYWORD_SET(include_ions) THEN BEGIN
  ;;    PRINT,'Currently no routine for doing the ion stuff!'
  ;; ENDIF ELSE BEGIN
  ;;    Ji                                = 0.
  ;;    Jei                               = 0.
  ;;    nBad_i                            = 0
  ;;    time_i                            = DOUBLE(0.)
  ;; ENDELSE

  ;;Set up the event struct
  event                                   = { x:time_e, $         ; When are you?
            MLT:mlt, $
            ILAT:ilat, $
            mono:BYTE(mono), $       
            broad:BYTE(broad), $     
            diffuse:BYTE(diffuse), $ 
            Je:Je, $                 
            Jee:Jee, $               
            nBad_eSpec:BYTE(nBad_e), $
            version:DEF_ST_VERSION_NUM}

  IF KEYWORD_SET(produce_failCodes) THEN BEGIN
     failCodes                            = { mono:mFCode, $
                   mono_nAbove:mono_nAbove, $
                   mono_nBelow:mono_nBelow, $
                   broad:bFCode, $
                   nBroad:nBroad, $
                   N_broad_GE_min_eV:N_broad_GE_min_eV, $
                   peakFlux:peakFlux, $
                   peakEnergy:peakEnergy}
  ENDIF
            ;; time_i:time_i, $         ;Ion time
            ;; Ji:Ji, $                 ;Ion number flux      (#/cm^2-s)
            ;; Jei:Jei, $               ;Ion energy flux      (mW/m^2)
            ;; nBad_iSpec:BYTE(nBad_i)} $     



  RETURN,event

END