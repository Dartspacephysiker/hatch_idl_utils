;2019/05/07
PRO GET_PEAK_ENERGY_FROM_DIFF_EFLUX_AND_ESPEC,diff_eflux, eSpec, $
   N_BELOW_PEAK=n_below_peak, $
   N_ABOVE_PEAK=n_above_peak, $
   PEAKE_BOUNDS_INDSHIFT=peakE_bounds_indShift, $
   OUT_PEAK_INDARR=peak_indArr, $
   OUT_PEAK_ENERGYARR=peak_energyArr, $
   OUT_PEAK_DEARR=peak_dEArr, $
   OUT_PEAK_EBOUNDSARR=peak_EBoundsArr, $
   OUT_PEAKE_INDSHIFT=peakE_indShift

  COMPILE_OPT IDL2,STRICTARRSUBS

  n_below_peak = N_ELEMENTS(n_below_peak) GT 0 ? n_below_peak : -1
  n_above_peak = N_ELEMENTS(n_above_peak) GT 0 ? n_above_peak : 30

  nHere                 = N_ELEMENTS(diff_eFlux.time)
  peak_indArr           = MAKE_ARRAY(nHere,VALUE=-999,/LONG)
  peak_energyArr        = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
  peak_dEArr            = MAKE_ARRAY(nHere,VALUE=-999,/FLOAT)
  peak_EBoundsArr       = MAKE_ARRAY(2,nHere,VALUE=-999,/FLOAT)
  peakE_indShift        = KEYWORD_SET(peakE_bounds_indShift) ? peakE_bounds_indShift : [0,0]

  iAngle                = 0

  FOR iTime=0,nHere-1 DO BEGIN

     ;; XorigArr           = energies[*,*,iTime]
     dat = diff_eflux[iTime]

     IF dat.valid EQ 0 THEN BEGIN

        peak_dEArr[iTime]         = -1
        peak_energyArr[iTime]     = -1
        peak_indArr[iTime]        = -1
        peak_EBoundsArr[*,iTime]  = [[-1],[-1]]

        CONTINUE
     ENDIF
     XorigArr           = dat.energy[0:dat.nenergy-1,0:dat.nbins-1]
     ;; YorigArr        = data[*,*,iTime]
     ;; worigArr        = ddata[*,*,iTime]
     ;; IF KEYWORD_SET(KF2D__Plot_opt.add_oneCount_curve) THEN BEGIN
     ;;    oneCountArr  = oneCount_data[*,*,iTime]
     ;; END
     ;; AorigArr           = angles[*,*,iTime]
     AorigArr           = dat.theta[0:dat.nenergy-1,0:dat.nbins-1]


;; NEED IANGLE

     ;;And now the order becomes [angle,energy] for each of these arrays
     Xorig              = REFORM(eSpec.v[iTime,*])
     Yorig              = REFORM(eSpec.y[iTime,*])
     worig              = REFORM(eSpec.yerr[iTime,*])
     Aorig              = REFORM(AorigArr[iAngle,*])

     ;; IF KEYWORD_SET(also_oneCount) THEN BEGIN
     ;;    oneCurve        = {x:Xorig, $
     ;;                       y:REFORM(oneCount_eSpec.y[iTime,*]), $
     ;;                       NAME:"One Count"}
     ;; ENDIF
     
     ;; CASE 1 OF
     ;;    has_minpe_struct: BEGIN
     ;;       minpe = minpe_arr[iTime]
     ;;       ;; PRINT,"MINPE : ",minpe
     ;;    END
     ;;    ELSE: BEGIN
     ;;       minpe = KEYWORD_SET(min_peak_energy) ? min_peak_energy : !NULL
     ;;    END
     ;; ENDCASE

     ;; IF KEYWORD_SET(minpe) THEN BEGIN
     ;;    ;;Try taking it from the top
     ;;    min_peak_ind    = MAX(WHERE(REFORM(XorigArr[0,*]) GE minpe))
     ;;    IF min_peak_ind EQ -1 THEN BEGIN
     ;;       STOP
     ;;    ENDIF
     ;; ENDIF ELSE BEGIN
     ;; min_peak_ind    = nEnergies-1
     min_peak_ind    = dat.nEnergy-1
     ;; ENDELSE

     ;; CASE 1 OF
     ;;    has_maxpe_struct: BEGIN
     ;;       maxpe = maxpe_arr[iTime]
     ;;       ;; PRINT,"MAXPE : ",maxpe
     ;;    END
     ;;    ELSE: BEGIN
     maxpe = KEYWORD_SET(max_peak_energy) ? max_peak_energy : !NULL
     ;;    END
     ;; ENDCASE


     KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY, $
        Xorig,Yorig, $
        peak_ind,peak_energy, $
        ;; NENERGIES=nEnergies, $
        NENERGIES=dat.nEnergy, $
        MAXEIND=maxEInd, $
        MINEIND=minEInd, $
        ENERGY_INDS=energy_inds, $
        ERANGE_FIT=eRange_fit, $
        N_BELOW_PEAK=n_below_peak, $
        N_ABOVE_PEAK=n_above_peak, $
        BULK_OFFSET=bulk_offset, $
        CHECK_FOR_HIGHER_FLUX_PEAKS=check_higher_peaks_set_peakEn, $
        MIN_PEAK_ENERGY=minpe, $
        MAX_PEAK_ENERGY=maxpe, $
        PEAK_ENERGY__START_AT_HIGHE=peak_energy__start_at_highE, $
        /CONTINUE_IF_NOMATCH, $
        /TEST_NOREV, $
        ONECOUNT_STR=oneCurve, $
        WHICHWY=whichWy

     ;;Note that while these are called maxE and minE, suggesting they refer to the max energy and min energy, they do NOT. 
     ;;Rather, they refer to the lowest and highest indices falling within the user-specified parameters 
     ;;  for fitting—namely, n_below_peak and n_above_peak
     
     ;; OLD 2018/01/12
     ;; maxEInd                   = (peak_ind + n_below_peak) < (dat.nEnergy-1)
     ;; minEInd                   = (peak_ind - n_above_peak) > 0

     ;; NEW 2018/01/12; use as many as possible above peak
     maxEInd                   = (peak_ind + n_below_peak) < (dat.nEnergy-1)
     minEInd                   = (peak_ind - 47          ) > 0

     peak_dEArr[iTime]         = eSpec.vErr[iTime,peak_ind]
     peak_energyArr[iTime]     = TEMPORARY(peak_energy)
     peak_indArr[iTime]        = TEMPORARY(peak_ind)
     peak_EBoundsArr[*,iTime]  = [Xorig[(TEMPORARY(maxEInd)-peakE_indShift[0]*whichWy) < (dat.nEnergy-1)], $
                                  Xorig[(TEMPORARY(minEInd)-peakE_indShift[1]*whichWy) > 0]]
  ENDFOR

END
