;2018/07/23
PRO IDENTIFY_ION_UPFLOW_ENERGY_BOUNDARY, $
   UPDOWNMINRATIO=upDownMinRatio, $
   MINNUMQUALIFYINGECHANNELS=minNumQualifyingEChannels, $
   DOWNESPEC=eSpecDown, $
   UPESPEC=eSpecUp, $
   ALLANGLEESPEC=eSpec, $
   UPDOWNRATIOSPEC=upDownRatioSpec, $
   OUT_EBOUND=eBound

  COMPILE_OPT IDL2,STRICTARRSUBS

  maxIonNRG = 500
  minIonNRG = 4

  threshRatio = KEYWORD_SET(upDownMinRatio           ) ? upDownMinRatio            : 10
  minNQualNRG = KEYWORD_SET(minNumQualifyingEChannels) ? minNumQualifyingEChannels : 4

  ;; fracNeeded = 3./4
  fracNeeded = 1./2

  nTid = N_ELEMENTS(eSpec.x)
  eBound = {x   : eSpecUp.x         , $
            y   : eSpecUp.x * 0.    , $
            ind : LONG(eSpecUp.x * 0)}

  ;; Y               FLOAT     Array[493, 47]
  NRGs     = REFORM(especup.v[nTid/2,*])
  IsReversedNRG = NRGs[1] LT NRGs[0]
  tmpNRG_i = WHERE((NRGs LE maxIonNRG) AND (NRGs GE minIonNRG),nTmpNRG)
  IF nTmpNRG LE 1 THEN BEGIN
     PRINT,"BOGUS"
     RETURN
  ENDIF

  trimNRGs = eSpecUp.v[*,tmpNRG_i]
  trimRats = upDownRatioSpec.y[*,tmpNRG_i]

  ;; Loop over all time slices of up/down energy spectra ratios
  ;; Find the ones that have sufficiently high up/down ratios to make us say "yes, that's up/outflow"
  FOR k=0,nTid-1 DO BEGIN

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
     ENDIF

  ENDFOR

END
