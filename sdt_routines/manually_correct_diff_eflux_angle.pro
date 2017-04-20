;2017/04/20
PRO MANUALLY_CORRECT_DIFF_EFLUX_ANGLE,diff_eFlux,manual_angle_correction

  COMPILE_OPT IDL2,STRICTARRSUBS

  PRINT,FORMAT='("Correcting diff_eFlux angle by ",F0.2," degrees")',manual_angle_correction

  nEnergies                         = N_ELEMENTS(diff_eFlux.energy[*,0,0])
  nTotAngles                        = N_ELEMENTS(diff_eFlux.theta[0,*,0])
  
  junk             = MIN(ABS(manual_angle_correction-diff_eFlux.theta[nEnergies/2,*,0]),minInd)
  junk             = MIN(ABS(diff_eFlux.theta[nEnergies/2,*,0]),zeroInd)
  shiftInd         = (minInd-zeroInd) MOD nTotAngles
  diff_eFlux.theta = SHIFT(diff_eFlux.theta,0,shiftInd,0)

END
