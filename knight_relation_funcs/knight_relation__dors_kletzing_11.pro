;;07/05/16
;;Comes from Equation 11 in Dors and Kletzing [1999], the Knight relation
;;T_m   :  Magnetospheric temperature of electrons  (eV)
;;Dens_m:  Magnetospheric plasma density            (cm^-3)
;;R_B   :  Magnetic field ratio, B_ionos/B_msphere  (dimensionless)
;;Pot   :  Potential drop                           (eV)

;;Potbar:  (electron_charge)*Delta_Phi/(k_B*T)      (potential drop normalized by temperature in eV)
FUNCTION KNIGHT_RELATION__DORS_KLETZING_11,kappa,T_m,dens_m,pot,R_B, $
   IN_POTBAR=in_potBar, $
   OUT_POTBAR=potBar

  COMPILE_OPT IDL2

  IF N_ELEMENTS(R_B) EQ 0 THEN BEGIN
     mRat                = 1.0e6
     PRINT,'Setting R_B to 1.0e6 ...'
  ENDIF ELSE BEGIN
     mRat                = R_B
  ENDELSE

  ;;a few constants
  speedOfLight           = DOUBLE(299792.458)*1e3             ;km / s
  electron_mass          = DOUBLE(5.1099891e5)/speedOfLight^2 ;eV/c^2
  
  eCharge                = DOUBLE(1.6e-19) ;Coulombs

  PRINT,'Reminder: Fix electron mass'
  ;; WAIT,1

  ;;Convert input params
  n                      = DOUBLE(Dens_m * 1000000.D)  ;dens_m in m^-3
  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(eCharge * pot/T_m) ;potential normalized by temperature
  ENDELSE
  k                      = DOUBLE(kappa)

  ;;Equation segments
  JVinv                  = (-0.5D) * eCharge * n
  JV1                    = SQRT( T_m / ( !PI * electron_mass ) * (2.D - DOUBLE(3.D / k ) ) )
  JV2                    = k / ( k - 1.D )
  JV3                    = GAMMA(k + 1.D) / ( k^(1.5D) * GAMMA(k - 0.5D) )
  JV4sub                 = 2.D * potBar / ( (2.D * k - 3.D) * (mRat - 1.D) )
  JV4                    = mRat * (1.D - (1.D - 1.D/mRat) * (1.D + JV4sub)^(-k + 1) )

  Jpar                   = JVinv * JV1 * JV2 * JV3 * JV4

  RETURN,Jpar
END
