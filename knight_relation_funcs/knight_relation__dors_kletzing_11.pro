;;07/05/16
;;Comes from Equation 11 in Dors and Kletzing [1999], the Knight relation
;;T_m   :  Magnetospheric temperature of electrons  (eV)
;;Dens_m:  Magnetospheric plasma density            (cm^-3)
;;R_B   :  Magnetic field ratio, B_ionos/B_msphere  (dimensionless)
;;Pot   :  Potential drop                           (eV)

;;Potbar:  (electron_charge)*Delta_Phi/(k_B*T)      (potential drop normalized by temperature in eV)

;;NOTE  : We treat earthward as positive here

FUNCTION KNIGHT_RELATION__DORS_KLETZING_11,kappa,T_m,dens_m,pot,R_B, $
   IN_POTBAR=in_potBar, $
   OUT_POTBAR=potBar, $
   NO_MULT_BY_CHARGE=no_mult_by_charge

  COMPILE_OPT IDL2

  IF N_ELEMENTS(R_B) EQ 0 THEN BEGIN
     mRat                = 30
     PRINT,'Setting R_B to 30 ...'
  ENDIF ELSE BEGIN
     mRat                = R_B
  ENDELSE

  ;;a few constants
  speedOfLight           = DOUBLE(299792.458)*1.e3            ;m / s
  electron_mass          = DOUBLE(5.1099891e5)/speedOfLight^2 ;eV/c^2
  
  eCharge                = DOUBLE(1.6e-19) ;Coulombs

  ;; PRINT,'Reminder: Fix electron mass'
  ;; WAIT,1

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

  ;;Fitter, happier
  KAPPA_RB_SAFECHECK,kappa,R_B, $
                     KAPPAMIN=kappaMin, $
                     R_BMIN=R_BMin, $
                     KAPPA_SAFE=kappaS, $
                     R_B_SAFE=R_BS

  ;;Make sure kappa is fo' real
  ;; kappaS = DOUBLE(kappa)
  ;; CASE N_ELEMENTS(kappa) OF
  ;;    1: BEGIN
  ;;       IF kappa LE 1.5 THEN BEGIN
  ;;          kappaS = 1.500001D
  ;;          ;; PRINT,"Kappa must be GE 1.5D!"
  ;;          ;; PRINT,"Returning..."
  ;;          ;; RETURN,-1
  ;;       ENDIF
  ;;       ;;Still fo' real
  ;;       IF kappa EQ 2.0 THEN kappaS = 2.00001D 

  ;;    END
  ;;    ELSE: BEGIN
  ;;       IF (WHERE(kappa EQ 1.5))[0] NE -1 THEN BEGIN
  ;;          kappaS[WHERE(kappa EQ 1.5)] = 1.500001D
  ;;       ENDIF
  ;;       IF (WHERE(kappa EQ 2.0))[0] NE -1 THEN BEGIN
  ;;          kappaS[WHERE(kappa EQ 2.0)] = 2.000001D
  ;;       ENDIF
  ;;    END
  ;; ENDCASE
  ;; ;; kappaS = DOUBLE(kappa)
  ;; ;; IF kappa LE 1.5 THEN BEGIN
  ;; ;;    kappaS = 1.50001D
  ;; ;;    ;; PRINT,"Kappa must be GE 1.5D!"
  ;; ;;    ;; PRINT,"Returning..."
  ;; ;;    ;; RETURN,-1
  ;; ;; ENDIF

  ;; ;;Still fo' real
  ;; IF kappa EQ 2.0 THEN kappaS = 2.0001D 

  ;;Equation segments
  ;; JVinv                  = (-0.5D) * eCharge * n
  JVinv   = (0.5D) * eCharge * n
  JV1     = SQRT( 2.D * T_m / ( !PI * electron_mass ) * (1.D - DOUBLE(1.5D / kappa ) ) )
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
