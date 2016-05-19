;+
;2016/05/19 My version of the algorithm presented by Newell et al. [2009]: "Diffuse, monoenergetic, and broadband aurora: The global precipitation budget"
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
FUNCTION DIFF_ENERGY_FLUX_SPECTRAL_TYPE__NEWELL_ET_AL_2009,eSpec,Je,Jee, $
   iSpec,Ji,Jei, $
   MLT, $
   BATCH_MODE=batch_mode, $
   INCLUDE_IONS=include_ions

  COMPILE_OPT idl2

  ;;return control to caller if no one is watching
  IF KEYWORD_SET(batch_mode) THEN BEGIN
     ON_ERROR, 2
  ENDIF

  time   = eSpec.time
  energy = eSpec.x
  spec   = eSpec.y

  ;;Check spectra integrity
  bad_elec_i      = WHERE(~FINITE(eSpec),nBad_e)
  IF nBad_e GT 0 THEN BEGIN
     IF nBad_e GT 255 THEN BEGIN
        PRINT,'nBad_e is too big to be assigned to a byte variable! Beware!'
        IF KEYWORD_SET(batch_mode) THEN BEGIN
           nBad_e = 255
        ENDIF ELSE BEGIN
           STOP
        ENDELSE
     ENDIF
     eSpec[bad_elec_i] = 0.
  ENDIF

  IF KEYWORD_SET(include_ions) THEN BEGIN
     bad_ion_i      = WHERE(~FINITE(iSpec),nBad_i)
     IF nBad_i GT 0 THEN BEGIN
        IF nBad_i GT 255 THEN BEGIN
           PRINT,'nBad_i is too big to be assigned to a byte variable! Beware!'
           IF KEYWORD_SET(batch_mode) THEN BEGIN
              nBad_i = 255
           ENDIF ELSE BEGIN
              STOP
           ENDELSE
        ENDIF
        iSpec[bad_ion_i] = 0.
     ENDIF
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MONOENERGETIC EVENTS
  ;;I. Identify peak in differential energy flux
  peakFlux    = MAX(spec,peakFlux_ind)

  ;;**
  ;;1. Examine drops in energy channels one and two levels above the peak (or only below, if at the highest energy)
  ;;    i.  If flux_drop_below AND flux_drop_above are 30% or less of the peak, continue to 2b.
  ;;    ii. Else, not monoenergetic. Monoenergetic = -1
  ;;    NOTE: Assuming a Maxwellian, the drop would be ~80% on either side. 30% is well below Maxwellian.
  above_i     = WHERE(energy GT energy[peakFlux_ind],nAbove)
  below_i     = WHERE(energy LT energy[peakFlux_ind],nBelow)
  nAbove = nAbove < 2
  nBelow = nBelow < 2
  CASE nAbove OF
     0: BEGIN
        cont = 1
     END
     1: BEGIN
        ;; IF (spec[peakFlux_ind+1]/peakFlux) LE .30 THEN BEGIN
        ;;    cont = 1 
        ;; ENDIF ELSE BEGIN
        ;;    cont = 0
        ;;    mono = -1
        ;; ENDELSE
        cont = (spec[peakFlux_ind+1]/peakFlux) LE .30 
        IF ~cont THEN mono = -1 ELSE BEGIN ;check how awesome
           strictMono_candidate = (spec[peakFlux_ind+1]/peakFlux) LE .10
        ENDELSE
     END
     2: BEGIN
        cont = ( (spec[peakFlux_ind+1]/peakFlux) LE .30) AND ( (spec[peakFlux_ind+2]/peakFlux) LE .30 )
        IF ~cont THEN mono = -1 ELSE BEGIN ;check how awesome
           strictMono_candidate = ( (spec[peakFlux_ind+1]/peakFlux) LE .10) AND ( (spec[peakFlux_ind+2]/peakFlux) LE .10 )
        ENDELSE
     END
  ENDCASE

  CASE nBelow OF
     0: BEGIN
        cont = 1
     END
     1: BEGIN
        ;; IF (spec[peakFlux_ind+1]/peakFlux) LE .30 THEN BEGIN
        ;;    cont = 1 
        ;; ENDIF ELSE BEGIN
        ;;    cont = 0
        ;;    mono = -1
        ;; ENDELSE
        cont = (spec[peakFlux_ind-1]/peakFlux) LE .30 
        IF ~cont THEN mono      = -1 ELSE BEGIN ;check how awesome
           strictMono_candidate = (spec[peakFlux_ind-1]/peakFlux) LE .10
        ENDELSE
     END
     2: BEGIN
        cont = ( (spec[peakFlux_ind-1]/peakFlux) LE .30) AND ( (spec[peakFlux_ind-2]/peakFlux) LE .30 )
        IF ~cont THEN mono     = -1 ELSE BEGIN ;check how awesome
           strictMono_candidate = ( (spec[peakFlux_ind-1]/peakFlux) LE .10) AND ( (spec[peakFlux_ind-2]/peakFlux) LE .10 )
        ENDELSE
     END
  ENDCASE

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
     cont                     = (Jee/Je) GE 80
     IF ~cont THEN mono       = -3
  ENDIF

  ;;**
  ;;4. Get peak_energy. Is peak_energy GE 100 eV? 
  ;;   i.   YES: Monoenergetic! Go to 5.
  ;;   ii.  NO : Not monoenergetic. Monoenergetic = -4
  IF cont THEN BEGIN
     cont                     = energy[peakFlux_ind] GE 100
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
  dSpec                       = DERIV(energy,spec)
  dSpec[bad_elec_i]                = 0.                 ;I guess?

  ;;**
  ;;1. Get N_good bins WHERE(dJ_E/dE > 2.0e8 eV/cm^2-sr-eV)
  ;;   i.   N_good GE 3 ? Continue to 2.
  ;;   ii.  Else, not broadband. Broadband = -1
  broad_i = WHERE(dSpec GT 2.0e8,nBroad)
  cont                        = nBroad GE 3
  IF ~cont THEN broad         = -1

  ;;**
  ;;2.  Calculate Eavg
  ;;   i.   Eavg GT 80 eV ? Continue to 3.
  ;;   ii.  Else, not broadband. Broadband = -2
  IF cont THEN BEGIN
     cont                     = (Jee/Je) GE 80
     IF ~cont THEN broad      = -2
  ENDIF

  ;;II. Determine our MLT: Is 9.5 < MLT < 14.5 ? 
  ;;   i.   YES: We're in the cusp; min eV = 300 eV
  ;;   ii.  NO : Not in cusp;       min_eV = 140 eV
  min_eV                      = (MLT GT 9.5 AND MLT LT 14.5) ? 300 : 140

  ;;**
  ;;3. Get N_wild(=number of N_good energies at or above min_eV). N_wild GE 3?
  ;;   i.   YES: broadband! Go to 4.
  ;;   ii.  NO : Not broadband. Broadband = -3
  IF cont THEN BEGIN
     wild_ii                  = WHERE(energy[broad_i] GE min_eV,N_wild)
     IF N_wild GE 3 THEN BEGIN
        ;; strictBroad_candidate = N_wild GE 4
        broad                 = 1 + (N_wild GE 4)
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
     ;; EXTRAPOLATE_SPECTRUM_TO_50KEV,energy,spectrum,extrapolated_e_spectrum
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
  IF KEYWORD_SET(include_ions) THEN BEGIN
     PRINT,'Currently no routine for doing the ion stuff!'
  ENDIF ELSE BEGIN
     Ji = 0.
     Jei = 0.
  ENDELSE

  ;;Set up the event struct
  event = { time:time, $             ; When are you?
            nBad_eSpec:nBad_e, $     ;0 = no problems      , N = N bad bins
            nBad_iSpec:nBad_i, $     ;0 = no problems      , N = N bad bins
            mono:BYTE(mono), $       ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
            broad:BYTE(broad), $     ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
            diffuse:BYTE(diffuse), $ ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
            Je:Je, $                 ;Electron number flux (#/cm^2-s)
            Jee:Jee, $               ;Electron energy flux (mW/m^2)
            Ji:Ji, $                 ;Ion number flux      (#/cm^2-s)
            Jei:Jei}                 ;Ion energy flux      (mW/m^2)

  RETURN,event

END