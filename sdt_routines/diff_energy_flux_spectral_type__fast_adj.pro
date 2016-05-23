;+
;2016/05/20 A FAST version of the algorithm presented by Newell et al. [2009]: "Diffuse, monoenergetic, and broadband aurora: The
;           global precipitation budget", based on conversations with Jim.
;           The operating assumption is that our energy bins are about twice as fine
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
FUNCTION DIFF_ENERGY_FLUX_SPECTRAL_TYPE__FAST_ADJ,eSpec,Je,Jee, $
   mlt,ilat, $
   ;; iSpec,Ji,Jei, $
   BATCH_MODE=batch_mode, $
   QUIET=quiet                  ;, $
   ;; INCLUDE_IONS=include_ions

  COMPILE_OPT idl2

  ;;return control to caller if no one is watching
  IF KEYWORD_SET(batch_mode) THEN BEGIN
     ON_ERROR, 2
  ENDIF

  time_e    = eSpec.x
  energy_e  = eSpec.v
  spec_e    = eSpec.y
  chare     = ABS(Jee/Je)*6.242*1.0e11

  ;;Check spectra integrity
  bad_elec_i      = WHERE(~FINITE(spec_e),nBad_e)
  IF nBad_e GT 0 THEN BEGIN
     IF nBad_e GT 255 THEN BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,'nBad_e is too big to be assigned to a byte variable! Beware!'
        IF KEYWORD_SET(batch_mode) THEN BEGIN
           nBad_e = 255
        ENDIF ELSE BEGIN
           STOP
        ENDELSE
     ENDIF
     spec_e[bad_elec_i] = 0.
  ENDIF

  ;; IF KEYWORD_SET(include_ions) THEN BEGIN
     

  ;;    bad_ion_i      = WHERE(~FINITE(iSpec),nBad_i)
  ;;    IF nBad_i GT 0 THEN BEGIN
  ;;       IF nBad_i GT 255 THEN BEGIN
  ;;          PRINT,'nBad_i is too big to be assigned to a byte variable! Beware!'
  ;;          IF KEYWORD_SET(batch_mode) THEN BEGIN
  ;;             nBad_i = 255
  ;;          ENDIF ELSE BEGIN
  ;;             STOP
  ;;          ENDELSE
  ;;       ENDIF
  ;;       iSpec[bad_ion_i] = 0.
  ;;    ENDIF
  ;; ENDIF

  threshold_percentage         = 0.3
  strictThreshold_percentage   = 0.1
  chare_threshold              = 80
  cusp_mineV                   = 300
  notCusp_mineV                = 140
  minForBroad                  = 6
  minForBroadStrict            = 8
  monoenergetic_n_to_check     = 4
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MONOENERGETIC EVENTS
  ;;I. Identify peak in differential energy flux
  peakFlux    = MAX(spec_e,peakFlux_ind)

  peak_energy = energy_e[peakFlux_ind]
  IF ~KEYWORD_SET(quiet) THEN PRINT,'Peak flux: ' + STRCOMPRESS(peakFlux,/REMOVE_ALL) + ' eV'
  IF ~KEYWORD_SET(quiet) THEN PRINT,'Peak energy: ' + STRCOMPRESS(peak_energy,/REMOVE_ALL) + ' eV'
  ;;**
  ;;1. Examine drops in energy channels one and two levels above the peak (or only below, if at the highest energy)
  ;;    i.  If flux_drop_below AND flux_drop_above are 30% or less of the peak, continue to 2b.
  ;;    ii. Else, not monoenergetic. Monoenergetic = -1
  ;;    NOTE: Assuming a Maxwellian, the drop would be ~80% on either side. 30% is well below Maxwellian.
  above_i     = WHERE(energy_e GT energy_e[peakFlux_ind],nAbove)
  below_i     = WHERE(energy_e LT energy_e[peakFlux_ind],nBelow)
  nAbove = nAbove < monoenergetic_n_to_check
  nBelow = nBelow < monoenergetic_n_to_check
  CASE nAbove OF
     0: BEGIN
        cont = 1
     END
     1: BEGIN
        cont = (spec_e[peakFlux_ind+1]/peakFlux) LE threshold_percentage 
        IF ~cont THEN mono = -1 ELSE BEGIN ;check how awesome
           strictMono_candidate = (spec_e[peakFlux_ind+1]/peakFlux) LE strictThreshold_percentage
        ENDELSE
     END
     2: BEGIN
        cont = ( (spec_e[peakFlux_ind+1]/peakFlux) LE threshold_percentage) OR $
               ( (spec_e[peakFlux_ind+2]/peakFlux) LE threshold_percentage )
        IF ~cont THEN mono = -1 ELSE BEGIN ;check how awesome
           strictMono_candidate = ( (spec_e[peakFlux_ind+1]/peakFlux) LE strictThreshold_percentage ) OR $
                                  ( (spec_e[peakFlux_ind+2]/peakFlux) LE strictThreshold_percentage )
        ENDELSE
     END
     3: BEGIN
        cont = ( (spec_e[peakFlux_ind+1]/peakFlux) LE threshold_percentage ) OR $
               ( (spec_e[peakFlux_ind+2]/peakFlux) LE threshold_percentage ) OR $
               ( (spec_e[peakFlux_ind+3]/peakFlux) LE threshold_percentage )
        IF ~cont THEN mono = -1 ELSE BEGIN ;check how awesome
           strictMono_candidate = ( (spec_e[peakFlux_ind+1]/peakFlux) LE strictThreshold_percentage ) OR $
                                  ( (spec_e[peakFlux_ind+2]/peakFlux) LE strictThreshold_percentage ) OR $
                                  ( (spec_e[peakFlux_ind+3]/peakFlux) LE strictThreshold_percentage )
        ENDELSE
     END
     4: BEGIN
        cont = ( (spec_e[peakFlux_ind+1]/peakFlux) LE threshold_percentage ) OR $
               ( (spec_e[peakFlux_ind+2]/peakFlux) LE threshold_percentage ) OR $
               ( (spec_e[peakFlux_ind+3]/peakFlux) LE threshold_percentage ) OR $
               ( (spec_e[peakFlux_ind+4]/peakFlux) LE threshold_percentage )
        IF ~cont THEN mono = -1 ELSE BEGIN ;check how awesome
           strictMono_candidate = ( (spec_e[peakFlux_ind+1]/peakFlux) LE strictThreshold_percentage ) OR $
                                  ( (spec_e[peakFlux_ind+2]/peakFlux) LE strictThreshold_percentage ) OR $
                                  ( (spec_e[peakFlux_ind+3]/peakFlux) LE strictThreshold_percentage ) OR $
                                  ( (spec_e[peakFlux_ind+4]/peakFlux) LE strictThreshold_percentage )
        ENDELSE
     END
  ENDCASE

  IF cont THEN BEGIN
     CASE nBelow OF
        0: BEGIN
           cont = 1
        END
        1: BEGIN
           cont = (spec_e[peakFlux_ind-1]/peakFlux) LE threshold_percentage 
           IF ~cont THEN mono      = -1 ELSE BEGIN ;check how awesome
              strictMono_candidate = (spec_e[peakFlux_ind-1]/peakFlux) LE strictThreshold_percentage
           ENDELSE
        END
        2: BEGIN
           cont = ( (spec_e[peakFlux_ind-1]/peakFlux) LE threshold_percentage ) OR $
                  ( (spec_e[peakFlux_ind-2]/peakFlux) LE threshold_percentage )
           IF ~cont THEN mono     = -1 ELSE BEGIN ;check how awesome
              strictMono_candidate = ( (spec_e[peakFlux_ind-1]/peakFlux) LE strictThreshold_percentage ) OR $
                                     ( (spec_e[peakFlux_ind-2]/peakFlux) LE strictThreshold_percentage )
           ENDELSE
        END
        3: BEGIN
           cont = ( (spec_e[peakFlux_ind-1]/peakFlux) LE threshold_percentage ) OR $
                  ( (spec_e[peakFlux_ind-2]/peakFlux) LE threshold_percentage ) OR $
                  ( (spec_e[peakFlux_ind-3]/peakFlux) LE threshold_percentage )
           IF ~cont THEN mono = -1 ELSE BEGIN ;check how awesome
              strictMono_candidate = ( (spec_e[peakFlux_ind-1]/peakFlux) LE strictThreshold_percentage ) OR $
                                     ( (spec_e[peakFlux_ind-2]/peakFlux) LE strictThreshold_percentage ) OR $
                                     ( (spec_e[peakFlux_ind-3]/peakFlux) LE strictThreshold_percentage )
           ENDELSE
        END
        4: BEGIN
           cont = ( (spec_e[peakFlux_ind-1]/peakFlux) LE threshold_percentage ) OR $
                  ( (spec_e[peakFlux_ind-2]/peakFlux) LE threshold_percentage ) OR $
                  ( (spec_e[peakFlux_ind-3]/peakFlux) LE threshold_percentage ) OR $
                  ( (spec_e[peakFlux_ind-4]/peakFlux) LE threshold_percentage )
           IF ~cont THEN mono = -1 ELSE BEGIN ;check how awesome
              strictMono_candidate = ( (spec_e[peakFlux_ind-1]/peakFlux) LE strictThreshold_percentage ) OR $
                                     ( (spec_e[peakFlux_ind-2]/peakFlux) LE strictThreshold_percentage ) OR $
                                     ( (spec_e[peakFlux_ind-3]/peakFlux) LE strictThreshold_percentage ) OR $
                                     ( (spec_e[peakFlux_ind-4]/peakFlux) LE strictThreshold_percentage )
           ENDELSE
        END
     ENDCASE
  ENDIF

  ;;**
  ;;2.  Is peak_flux GE 1.0e8 eV/cm^2-sr-eV?
  ;;    i.  If so, continue to 3
  ;;    ii. Else, not monoenergetic. Monoenergetic = -2
  ;;    NOTE: They reference Newell et al. [1996b] here, saying it is the smallest threshold that seems to exclude homogeneous aurora
  IF cont THEN BEGIN
     cont                     = peakFlux GE 1.0e8
     IF ~cont THEN mono       = -2
  ENDIF

  ;;**
  ;;3. Identify average energy. Is Eavg GT 80 eV?
  ;;   i.   YES: Continue to 4
  ;;   ii.  NO : Not monoenergetic. Monoenergetic = -3
  IF cont THEN BEGIN
     cont                     = chare GE chare_threshold
     IF ~cont THEN mono       = -3
  ENDIF

  ;;**
  ;;4. Get peak_energy. Is peak_energy GE 100 eV? 
  ;;   i.   YES: Monoenergetic! Go to 5.
  ;;   ii.  NO : Not monoenergetic. Monoenergetic = -4
  IF cont THEN BEGIN
     cont                     = energy_e[peakFlux_ind] GE 100
     IF ~cont THEN mono       = -4
  ENDIF

  ;;**
  ;;5. Are flux_drop_below AND flux_drop_above LE 10% of peak_flux?
  ;;   i.   YES: strict_monoenergetic! Monoenergetic = 2
  ;;   ii.  NO : Not strict_monoenergetic! Monoenergetic = 1.
  IF cont THEN BEGIN
     mono                     = 1 + strictMono_candidate
  ENDIF
  ;;DONE with monoenergetic


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;BROADBAND EVENTS
  ;;****************
  ;;I. Calculate dJ_E/dE
  ;; dSpec_E                       = DERIV(energy_e,spec_e)
  ;; dSpec_E[bad_elec_i]                = 0.                 ;I guess?

  ;;**
  ;;1. Get N_good bins WHERE(dJ_E/dE > 2.0e8 eV/cm^2-sr-eV)
  ;;   i.   N_good GE 3 ? Continue to 2.
  ;;   ii.  Else, not broadband. Broadband = -1
  ;; broad_i = WHERE(dSpec_E GT 2.0e8,nBroad)
  broad_i = WHERE(spec_e GT 2.0e8,nBroad)
  cont                        = nBroad GE minForBroad
  IF ~cont THEN broad         = -1

  ;;**
  ;;2.  Calculate Eavg
  ;;   i.   Eavg GT 80 eV ? Continue to 3.
  ;;   ii.  Else, not broadband. Broadband = -2
  IF cont THEN BEGIN
     cont                     = chare GE chare_threshold
     IF ~cont THEN broad      = -2
  ENDIF

  ;;II. Determine our MLT: Is 9.5 < MLT < 14.5 ? 
  ;;   i.   YES: We're in the cusp; min eV = 300 eV
  ;;   ii.  NO : Not in cusp;       min_eV = 140 eV
  min_eV                      = (MLT GT 9.5 AND MLT LT 14.5) ? cusp_mineV : notCusp_mineV

  ;;**
  ;;3. Get N_wild(=number of N_good energies at or above min_eV). N_wild GE 3?
  ;;   i.   YES: broadband! Go to 4.
  ;;   ii.  NO : Not broadband. Broadband = -3
  IF cont THEN BEGIN
     wild_ii                  = WHERE(energy_e[broad_i] GE min_eV,N_wild)
     IF N_wild GE 3 THEN BEGIN
        ;; strictBroad_candidate = N_wild GE 4
        broad                 = 1 + (N_wild GE 8)
     ENDIF ELSE BEGIN
        broad                 = -3
     ENDELSE
  ENDIF

  ;;DIFFUSE AURORA (Events identified as NEITHER monoenergetic NOR broadband)
  ;;*************************************************************************
  ;;**
  ;;1. Flag as diffuse
  ;;   i.   Do electron extrapolation
  ;;   ii.  Event is diffuse
  IF mono LT 0 AND broad LT 0 THEN BEGIN
     diffuse = 1
     ;; EXTRAPOLATE_SPECTRUM_TO_50KEV,energy_e,spec_e,extrapolated_e_spectrum
     ;; RECALCULATE_50KEV_FLUXES,extrapolated_e_spectrum,Je,Jee
  ENDIF ELSE BEGIN
     diffuse = 0
  ENDELSE 

  ;;ELECTRON EXTRAPOLATION
  ;;**********************
  ;;1. Assume Maxwellian, calculate flux at 50 keV based on Maxwellian fit
  ;;   i.   Set T_ion = 0.5 * peak_ion_energy (NOT peak_ion_flux!) (Maxwellian assumption)
  ;;   ii.  Based on Maxwellian assumption, extrapolate {#,energy} flux at extrapolated energy bin up to 50 keV (or one energy step above highest bin)
  ;;
  IF diffuse THEN BEGIN
     ;; EXTRAPOLATE_SPECTRUM_TO_50KEV,energy,spectrum,extrapolated_e_spectrum
     ;; RECALCULATE_50KEV_FLUXES,extrapolated_e_spectrum,Je,Jee
  ENDIF
  
  ;;adjust mono and broad just in case
  IF mono  LT 0 THEN mono  = 255 + mono
  IF broad LT 0 THEN broad = 255 + broad

  ;;ION AURORA
  ;;**********
  ;;1. Determine differential energy flux peak, peak_ion_flux. 
  ;;   i.   Set T_ion = 0.5 * peak_ion_energy (NOT peak_ion_flux!) (Maxwellian assumption)
  ;;2. Extrapolation
  ;;   i.   Based on Maxwellian assumption, extrapolate {#,energy} flux at extrapolated energy bin up to 50 keV (or one energy step above highest bin)
  ;; EXTRAPOLATE_SPECTRUM_TO_50KEV,energy_ions,ion_spectrum,extrapolated_i_spectrum
  ;; RECALCULATE_50KEV_FLUXES,extrapolated_e_spectrum,Ji,Jei
  ;; IF KEYWORD_SET(include_ions) THEN BEGIN
  ;;    PRINT,'Currently no routine for doing the ion stuff!'
  ;; ENDIF ELSE BEGIN
  ;;    Ji     = 0.
  ;;    Jei    = 0.
  ;;    nBad_i = 0
  ;;    time_i = DOUBLE(0.)
  ;; ENDELSE

  ;;Set up the event struct
  ;; event = { time_e:time_e, $         ; When are you?
  event = { x:time_e, $         ; When are you?
            MLT:mlt, $
            ILAT:ilat, $
            mono:BYTE(mono), $       ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
            broad:BYTE(broad), $     ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
            diffuse:BYTE(diffuse), $ ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
            Je:Je, $                 ;Electron number flux (#/cm^2-s)
            Jee:Jee, $               ;Electron energy flux (mW/m^2)
            nBad_eSpec:BYTE(nBad_e)}       ;0 = no problems      , N = N bad bins
            ;; time_i:time_i, $         ;Ion time
            ;; Ji:Ji, $                 ;Ion number flux      (#/cm^2-s)
            ;; Jei:Jei, $               ;Ion energy flux      (mW/m^2)
            ;; nBad_iSpec:BYTE(nBad_i)} $     ;0 = no problems      , N = N bad bins



  RETURN,event

END