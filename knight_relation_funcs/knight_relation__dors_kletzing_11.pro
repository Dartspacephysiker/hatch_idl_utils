;;07/05/16
;;Comes from Equation 11 in Dors and Kletzing [1999], the Knight relation
;;T_m   :  Magnetospheric temperature of electrons  (eV)
;;Dens_m:  Magnetospheric plasma density            (cm^-3)
;;R_B   :  Magnetic field ratio, B_ionos/B_msphere  (dimensionless)
;;Pot   :  Potential drop                           (eV)

;;Potbar:  (electron_charge)*Delta_Phi/(k_B*T)      (potential drop normalized by temperature in eV)

;;RETURN: Field-aligned current density in A/m^2

;;NOTE  : We treat earthward as positive here
FUNCTION KNIGHT_RELATION__DORS_KLETZING_11,kappa,T_m,dens_m,pot,R_B, $
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

  ;;Fitter, happier
  ;; KAPPA_RB_SAFECHECK,kappa,R_B, $
  ;;                    KAPPAMIN=kappaMin, $
  ;;                    R_BMIN=R_BMin, $
  ;;                    KAPPA_SAFE=kappaS, $
  ;;                    R_B_SAFE=R_BS

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
  n                      = DOUBLE(Dens_m * 1000000.D)  ;dens_m in m^-3
  ;; helpMeNotBeZero        = 0.00001D
  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(pot/T_m) ;potential normalized by temperature
     IF ~KEYWORD_SET(no_mult_by_charge) THEN BEGIN
        potBar          *= eCharge
     ENDIF
  ENDELSE

  ;;Make sure kappa is fo' real
  kappaS = DOUBLE(kappa)
  CASE N_ELEMENTS(kappa) OF
     1: BEGIN
        IF kappa LE 1.5 THEN BEGIN
           kappaS = 1.500001D
           ;; PRINT,"Kappa must be GE 1.5D!"
           ;; PRINT,"Returning..."
           ;; RETURN,-1
        ENDIF
        ;;Still fo' real
        IF kappa EQ 2.0 THEN kappaS = 2.00001D 

     END
     ELSE: BEGIN
        IF (WHERE(kappa EQ 1.5))[0] NE -1 THEN BEGIN
           kappaS[WHERE(kappa EQ 1.5)] = 1.500001D
        ENDIF
        IF (WHERE(kappa EQ 2.0))[0] NE -1 THEN BEGIN
           kappaS[WHERE(kappa EQ 2.0)] = 2.000001D
        ENDIF
     END
  ENDCASE
  ;; kappaS = DOUBLE(kappa)
  ;; IF kappa LE 1.5 THEN BEGIN
  ;;    kappaS = 1.50001D
  ;;    ;; PRINT,"Kappa must be GE 1.5D!"
  ;;    ;; PRINT,"Returning..."
  ;;    ;; RETURN,-1
  ;; ENDIF

  ;;Still fo' real
  ;; IF kappa EQ 2.0 THEN kappaS = 2.0001D 

  ;;Equation segments
  ;; JVinv                  = (-0.5D) * eCharge * n
  JVinv   = eCharge * n
  JV1     = SQRT( T_m / ( 2.D * !PI * inMass ) * (1.D - DOUBLE(1.5D / kappa ) ) )
  JV2     = kappa / ( kappa - 1.D )

  ;; JV3  = GAMMA(kappa + 1.D) / ( kappa^(1.5D) * GAMMA(kappa - 0.5D) )
  gammaRat = KAPPA_GAMMARAT(kappa)
  ;; CASE 1 OF
  ;;    (kappa GE 20): BEGIN
  ;;       gammaRat = EXP(LNGAMMA( kappa + 1.0D )-LNGAMMA( kappa - 0.5D ))
  ;;    END
  ;;    ELSE: BEGIN
  ;;       gammarat = GAMMA( kappa + 1.D) / GAMMA( kappa - 0.5D )
  ;;    END
  ;; ENDCASE

  JV3        = gammaRat / kappa^(1.5D)

  ;; JV4sub  = 2.D * potBar / ( (2.D * k - 3.D + helpMeNotBeZero ) * (mRat - 1.D) )
  JV4sub     = potBar / ( ( kappaS - 1.5D ) * (mRat - 1.D) )
  JV4s       = ( 1.D + JV4sub )^( (-1.0D) * kappa + 1)

  ;; Binomial approx
  ;; IF (WHERE(JV4sub LE 0.01D))[0] NE -1 THEN BEGIN
  ;;    JV4s[WHERE(JV4sub LE 0.01D)] = 1.D + (1.0D - kappa ) * JV4sub[WHERE(JV4sub LE 0.01D)] 
  ;; ENDIF
  JV4        = mRat * (1.D - (1.D - 1.D/mRat) * JV4s )

  Jpar       = JVinv * JV1 * JV2 * JV3 * JV4

  RETURN,Jpar
END
