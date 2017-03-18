;;07/05/16
;;Comes from Equation 4 in Dors and Kletzing [1999], the Knight relation
;;T_m   :  Magnetospheric temperature of electrons  (eV)
;;Dens_m:  Magnetospheric plasma density            (cm^-3)
;;R_B   :  Magnetic field ratio, B_ionos/B_msphere  (dimensionless)
;;Pot   :  Potential drop                           (eV/C)

;;Potbar:  (electron_charge)*Delta_Phi/(k_B*T)      (potential drop normalized by temperature in eV)

;;RETURN: Field-aligned current density in A/m^2

;;NOTE  : We treat earthward as positive here
FUNCTION KNIGHT_RELATION__DORS_KLETZING_4,T_m,dens_m,pot,R_B, $
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

  mRatS = mRat
  IF mRat EQ 1 THEN mRatS = 1.00001D

  ;;a few constants
  IF KEYWORD_SET(mass) THEN BEGIN
     ;;Assume this is in eV/c^2 (with c in km/s instead of m/s)
     ;; speedOfLight        = 299792.458D ;km / s
     inMass              = mass / 1D6 ;(convert c^2 from m^2/s^2 to km^2/s^2)
  ENDIF ELSE BEGIN
     speedOfLight        = 299792458D ;m / s
     inMass              = 5.1099891D5/speedOfLight^2 ;eV/c^2
  ENDELSE
  
  eCharge                = DOUBLE(1.6e-19) ;Coulombs

  ;;Convert input params
  n                      = DOUBLE(Dens_m * 1000000.D) ;dens_m in m^-3
  ;; helpMeNotBeZero        = 0.000001D
  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(pot/T_m) ;potential normalized by temperature
     IF ~KEYWORD_SET(no_mult_by_charge) THEN BEGIN
        potBar          *= eCharge
     ENDIF
  ENDELSE


  ;;Equation segments
  ;; JVinv                  = (-1.D) * eCharge * n
  JVinv                  = (1.D) * eCharge * n
  JV1                    = SQRT( T_m / ( 2.D * !PI * inMass ) )
  ;; JV2sub                 = (-1.D) * potBar / ( mRat - 1.D + helpMeNotBeZero )
  JV2sub                 = (-1.D) * potBar / ( mRatS - 1.D )
  JV2                    = mRat * (1.D - (1.D - 1.D/mRat) * EXP(JV2sub) )



  Jpar                   = JVinv * JV1 * JV2

  RETURN,Jpar
END
