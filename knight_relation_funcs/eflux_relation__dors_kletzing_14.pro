;2017/12/25
;;Comes from Equation 14 in Dors and Kletzing [1999], the Knight relation
;;T_m   :  Magnetospheric temperature of electrons  (eV)
;;Dens_m:  Magnetospheric plasma density            (cm^-3)
;;R_B   :  Magnetic field ratio, B_ionos/B_msphere  (dimensionless)
;;Pot   :  Potential drop                           (eV)

;;Potbar:  (electron_charge)*Delta_Phi/(k_B*T)      (potential drop normalized by temperature in eV)

;;RETURN: Field-aligned current density in A/m^2

;;NOTE  : We treat earthward as positive here
FUNCTION EFLUX_RELATION__DORS_KLETZING_14,kappa,T_m,dens_m,pot,R_B, $
                                          IN_POTBAR=in_potBar, $
                                          OUT_POTBAR=potBar, $
                                          NO_MULT_BY_CHARGE=no_mult_by_charge, $
                                          MASS=mass

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(R_B) EQ 0 THEN BEGIN
     mRat                = 30
     PRINT,'Setting R_B to 30 ...'
  ENDIF ELSE BEGIN
     mRat                = R_B
  ENDELSE

  ;;a few constants
  IF KEYWORD_SET(mass) THEN BEGIN
     ;;Assume this is in eV/c^2 (with c in km/s instead of m/s)
     ;; speedOfLight        = 299792.458D ;km / s
     inMass              = mass / 1D6 ;(convert c^2 from m^2/s^2 to km^2/s^2)
  ENDIF ELSE BEGIN
     speedOfLight        = 299792458D                 ;m / s
     inMass              = 5.1099891D5/speedOfLight^2 ;eV/c^2
  ENDELSE
  
  eCharge                = DOUBLE(1.6e-19) ;Coulombs

  ;;Convert input params
  n                      = DOUBLE(Dens_m * 1000000.D) ;dens_m in m^-3
  ;; helpMeNotBeZero        = 0.00001D
  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(pot/T_m) ;potential normalized by temperature
     IF ~KEYWORD_SET(no_mult_by_charge) THEN BEGIN
        potBar          *= eCharge
     ENDIF
  ENDELSE

  ;; 2017/12/25 Necessary?
  ;;Fitter, happier
  ;; KAPPA_RB_SAFECHECK,kappa,R_B, $
  ;;                    KAPPAMIN=kappaMin, $
  ;;                    R_BMIN=R_BMin, $
  ;;                    KAPPA_SAFE=kappaS, $
  ;;                    R_B_SAFE=R_BS


  ;; JVinv   = eCharge * n
  EFV1   = n * T_m^(1.5D) / SQRT( 2.D * !PI * inMass ) * (1.D - DOUBLE(1.5D / kappa ) ) * mRat
  EFV2   = ( ( 2.D + potBar ) - ( potBar + 2.D * ( 1.D - 1.D/R_B ) ) * EXP( - potBar / ( R_B - 1.D ) ) )

  RETURN,EFV1*EFV2

END
