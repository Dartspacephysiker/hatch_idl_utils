;2018/07/23
;2019/01/16 Added .type to eBound struct.
;           type = 1 means dayside, "from 4 ev up" upflow.
;           type = 2 means nightside, "hovering beam" upflow
PRO IDENTIFY_ION_UPFLOW_ENERGY_BOUNDARY, $
   UPDOWNMINRATIO=upDownMinRatio, $
   MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
   FRACBELOWTHATMUSTBEUPWARD=fracBelowThatMustBeUpward, $
   DOWNESPEC=eSpecDown, $
   UPESPEC=eSpecUp, $
   ALLANGLEESPEC=eSpec, $
   UPDOWNRATIOSPEC=upDownRatioSpec, $
   OUT_EBOUND=eBound

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NYE VEI

  maxIonNRG = 9000
  minIonNRG = 4

  threshRatio = KEYWORD_SET(upDownMinRatio           ) ? upDownMinRatio            : 10
  minNQualNRG = KEYWORD_SET(minNumQualifyingEChannels) ? minNumQualifyingEChannels : 4

  highThreshRatio = 5

  ;; fracNeeded = 3./4
  fracNeeded = KEYWORD_SET(fracBelowThatMustBeUpward) ? fracBelowThatMustBeUpward : 1./2

  nTid = N_ELEMENTS(eSpec.x)
  eBound = {x       : eSpecUp.x                  , $
            y       : eSpecUp.x * 0.             , $
            ind     : LONG(eSpecUp.x * 0)        , $
            yLow    : eSpecUp.x * 0. + minIonNRG , $
            indLow  : LONG(eSpecUp.x * 0)        , $
            type    : BYTE(eSpecUp.x * 0B)         $
           }

  ;; Y               FLOAT     Array[493, 47]
  tryCount = 0
  goodEnergies = 0B
  WHILE ~goodEnergies DO BEGIN
     NRGs     = REFORM(especup.v[(nTid/2 + tryCount) < (nTid-1),*])

     tmpNRG_i = WHERE((NRGs LE maxIonNRG) AND (NRGs GE minIonNRG),nTmpNRG)
     IF nTmpNRG LE 1 THEN BEGIN
        PRINT,"FIND GOOD ENERGIES: BOGUS " + STRING(FORMAT='(I0)',tryCount)
        ;; RETURN
     ENDIF ELSE BEGIN
        goodEnergies = 1B
     ENDELSE

     tryCount += 1

     IF tryCount GT 50 THEN BREAK
  ENDWHILE
  
  IF ~goodEnergies THEN BEGIN
     PRINT,"BOGUS"
     RETURN
  ENDIF
  IsReversedNRG = NRGs[1] LT NRGs[0]

  ;; NRGs     = REFORM(especup.v[nTid/2,*])
  ;; IsReversedNRG = NRGs[1] LT NRGs[0]
  ;; tmpNRG_i = WHERE((NRGs LE maxIonNRG) AND (NRGs GE minIonNRG),nTmpNRG)
  ;; IF nTmpNRG LE 1 THEN BEGIN
  ;;    PRINT,"BOGUS"
  ;;    RETURN
  ;; ENDIF

  trimNRGs = eSpecUp.v[*,tmpNRG_i]
  trimRats = upDownRatioSpec.y[*,tmpNRG_i]

     ;; t1Str ='1998-05-20/19:39:45'
     ;; t1Str ='1998-05-20/19:43:30'
     ;; t1 = S2T(t1Str)

  ;; Loop over all time slices of up/down energy spectra ratios
  ;; Find the ones that have sufficiently high up/down ratios to make us say "yes, that's up/outflow"
  FOR k=0,nTid-1 DO BEGIN

     ;; IF ABS(eSpecUp.x[k]-t1) LT 1 THEN STOP

     tmpRats = trimRats[k,*]
     tmpNRGs = trimNRGs[k,*]

     tmpBoveThresh_ii = WHERE(tmpRats GE threshRatio,nTmpBove)

     ;; Skip ahead if none above threshold ratio for this time slice
     IF nTmpBove LT 1 THEN CONTINUE

     tmpMax  = MAX( (NRGs[tmpNRG_i])[tmpBoveThresh_ii] , ind_iii)

     nQualNRG = N_ELEMENTS(WHERE((tmpNRGs LE tmpMax) AND (tmpNRGs GE minIonNRG),/NULL))

     ;; If at least half of the energy channels below the max are above threshold, call it up/outflow
     IF nTmpBove GE CEIL(FLOAT(nQualNRG)*fracNeeded) AND (nQualNRG GE minNQualNRG) THEN BEGIN
        eBound.y[k] = tmpMax
        eBound.ind[k] = tmpNRG_i[tmpBoveThresh_ii[ind_iii]]
        eBound.type[k] = 1
     ENDIF ELSE BEGIN

        ;; If tmpMax exceeds 500 eV and the ratios are high, it may still be ion outflow

        ;; IF tmpMax GE 500 eV AND nTmpBove GE 3 THEN BEGIN
        tmpBoveHighThresh_ii = WHERE(tmpRats GE highThreshRatio,nTmpBoveHigh)

        IF tmpMax GE 300 AND nTmpBoveHigh GT 3 THEN BEGIN ;
           
           tmpMaxHigh  = MAX( (NRGs[tmpNRG_i])[tmpBoveHighThresh_ii] , indHigh_iii)
           
           ;; tmpLB = tmpMaxHigh
           indTmp_ii = tmpBoveHighThresh_ii[indHigh_iii]
           ;; WHILE tmpRats[indTmp_ii+1] GE highThreshRatio/2. DO BEGIN ;Keep feeling downward until there's more downward than upward
           WHILE tmpRats[indTmp_ii+1] GE 1. DO BEGIN ;Keep feeling downward until there's more downward than upward
              indTmp_ii++
              IF indTmp_ii GE (nTmpNRG-1) THEN BREAK ;Get out if we're about to go below the detector energy limit
           ENDWHILE

           IF tmpNRGs[indTmp_ii] LT tmpMaxHigh THEN BEGIN
              eBound.y[k] = tmpMaxHigh
              eBound.ind[k] = tmpNRG_i[tmpBoveHighThresh_ii[indHigh_iii]] 

              eBound.yLow[k] = tmpNRGs[indTmp_ii]
              eBound.indLow[k] = tmpNRG_i[indTmp_ii]

              eBound.type[k] = 2

           ENDIF

        ENDIF

     ENDELSE

  ENDFOR



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ALD VEI

  ;; maxIonNRG = 500
  ;; minIonNRG = 4

  ;; threshRatio = KEYWORD_SET(upDownMinRatio           ) ? upDownMinRatio            : 10
  ;; minNQualNRG = KEYWORD_SET(minNumQualifyingEChannels) ? minNumQualifyingEChannels : 4

  ;; ;; fracNeeded = 3./4
  ;; fracNeeded = 1./2

  ;; nTid = N_ELEMENTS(eSpec.x)
  ;; eBound = {x   : eSpecUp.x         , $
  ;;           y   : eSpecUp.x * 0.    , $
  ;;           ind : LONG(eSpecUp.x * 0), $
  ;;           yLow  : eSpecUp.x * 0.    , $
  ;;           indLow  : eSpecUp.x * 0. $
  ;;          }

  ;; ;; Y               FLOAT     Array[493, 47]
  ;; NRGs     = REFORM(especup.v[nTid/2,*])
  ;; IsReversedNRG = NRGs[1] LT NRGs[0]
  ;; tmpNRG_i = WHERE((NRGs LE maxIonNRG) AND (NRGs GE minIonNRG),nTmpNRG)
  ;; IF nTmpNRG LE 1 THEN BEGIN
  ;;    PRINT,"BOGUS"
  ;;    RETURN
  ;; ENDIF

  ;; trimNRGs = eSpecUp.v[*,tmpNRG_i]
  ;; trimRats = upDownRatioSpec.y[*,tmpNRG_i]

  ;; ;; Loop over all time slices of up/down energy spectra ratios
  ;; ;; Find the ones that have sufficiently high up/down ratios to make us say "yes, that's up/outflow"
  ;; FOR k=0,nTid-1 DO BEGIN

  ;;    tmpRats = trimRats[k,*]
  ;;    tmpNRGs = trimNRGs[k,*]

  ;;    tmpBoveThresh_ii = WHERE(tmpRats GE threshRatio,nTmpBove)

  ;;    ;; Skip ahead if none above threshold ratio for this time slice
  ;;    IF nTmpBove LT 1 THEN CONTINUE

  ;;    tmpMax  = MAX( (NRGs[tmpNRG_i])[tmpBoveThresh_ii] , ind_iii)

  ;;    nQualNRG = N_ELEMENTS(WHERE((tmpNRGs LE tmpMax) AND (tmpNRGs GE minIonNRG),/NULL))
  ;; nQualNRG = N_ELEMENTS(WHERE((tmpNRGs[tmpBoveThresh_ii] LE tmpMax) AND (tmpNRGs[tmpBoveThresh_ii] GE minIonNRG),/NULL))

  ;;    ;; If at least half of the energy channels below the max are above threshold, call it up/outflow
  ;;    IF nTmpBove GE CEIL(FLOAT(nQualNRG)*fracNeeded) AND (nQualNRG GE minNQualNRG) THEN BEGIN
  ;;       eBound.y[k] = tmpMax
  ;;       eBound.ind[k] = tmpNRG_i[tmpBoveThresh_ii[ind_iii]]
  ;;    ENDIF

  ;; ENDFOR

END
