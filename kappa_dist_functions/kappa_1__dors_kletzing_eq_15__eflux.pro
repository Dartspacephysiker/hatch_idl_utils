;2016/10/??
FUNCTION KAPPA_1__DORS_KLETZING_EQ_15__EFLUX,kappa,T_m,dens_m,pot,R_B, $
   IN_POTBAR=in_potBar, $
   OUT_POTBAR=potBar, $
   POT_IN_JOULES=pot_in_joules

  COMPILE_OPT idl2
  
  ;; helpMeNotBeZero        = 1.e-6
  toJ                    = 1.6e-19 ;eV to J

  eCharge                = DOUBLE(1.6e-19)

  speedOfLight           = DOUBLE(299792458.) ;m / s
  electron_mass          = DOUBLE(5.109989e5) / speedOfLight^2.D ;eV/c^2 (where c is in m/s)

  n                      = DOUBLE(Dens_m * 1000000.D)  ;dens_m in m^-3
  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(pot/T_m) ;potential normalized by temperature
     IF KEYWORD_SET(pot_in_joules) THEN BEGIN
        potBar          *= eCharge
     ENDIF
  ENDELSE

  ;;Make sure kappa is fo' real
  kappaS = DOUBLE(kappa)
  IF kappa LE 1.5 THEN BEGIN
     kappaS = 1.50001D
     ;; PRINT,"Kappa must be GE 1.5D!"
     ;; PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  ;;Still fo' real
  IF kappa EQ 2.0 THEN kappaS = 2.0001D 

  R_BS    = R_B
  IF R_B LE 1.0 THEN BEGIN
     PRINT,"R_B must be GT 1.0!"
     R_BS = 1.0001
  ENDIF

  ;;Have to translate T to the most probable speed, w, which is how Dors and Kletzing cast it
  w_sq                   = 2.D * T_m / electron_mass * ( (kappaS - 1.5D) / kappaS )
  
  ;; PI                     = 1.D + potBar / ( (kappaS - 1.5D + helpMeNotBeZero) * ( R_B - 1.D ) )
  PI                     = 1.D + potBar / ( (kappaS - 1.5D ) * ( R_BS - 1.D ) )
  one_m_one_over_R_B     = (1.D - 1.D/R_B)
  CASE 1 OF
     (kappaS GE 20): BEGIN
        gammaRat = EXP(LNGAMMA( kappaS + 1.0D )-LNGAMMA( kappaS - 0.5D ))
     END
     ELSE: BEGIN
        gammarat = GAMMA( kappaS + 1.D) / GAMMA( kappaS - 0.5D )
     END
  ENDCASE
  A_k                    = gammaRat / kappaS^(1.5D)
  ;; A_k                    = GAMMA(kappaS + 1.D) / ( kappaS^(1.5D) * GAMMA( kappaS - 0.5D ) )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  ;; ;;The old way, with helpMeNotBeZero
  ;; Finv                   = n * electron_mass * toJ * (w_sq)^(1.5D) / 4.D / SQRT(!PI) * kappaS^(2.D) * A_k * R_B / ( ( kappaS - 1.D ) * ( kappaS - 2.D + helpMeNotBeZero) )
  ;; FK1                    = 2.D + (kappaS - 2.D + helpMeNotBeZero) / (kappaS - 1.5D + helpMeNotBeZero ) * potBar
  ;; FK2                    = ( ( kappaS - 2.D ) / ( kappaS - 1.D ) + potBar * ( kappaS - 2.D ) / ( kappaS - 1.5D + helpMeNotBeZero ) ) * ( kappaS / ( (kappaS - 1.D) * (R_B - 1.D) ) + 1.D )
  ;; FK3                    = 1.D + ( 1.D + kappaS / ( R_B - 1.D + helpMeNotBeZero ) ) / ( kappaS - 1.D )

  Finv                   = n * electron_mass * toJ * (w_sq)^(1.5D) / 4.D / SQRT(!PI) * kappaS^(2.D) * A_k * R_B / ( ( kappaS - 1.D ) * ( kappaS - 2.D ) )

  ;;First chunk
  FK1                    = 2.D + (kappaS - 2.D ) / (kappaS - 1.5D ) * potBar

  ;;Second chunk
  FK2                    = ( ( kappaS - 2.D ) / ( kappaS - 1.D ) + potBar * ( kappaS - 2.D ) / ( kappaS - 1.5D ) ) * ( kappaS / ( (kappaS - 1.D) * (R_BS - 1.D) ) + 1.D )

  ;;Third chunk, in parts that become useful later for PDs
  FK3                    = 1.D + ( 1.D + kappaS / ( R_BS - 1.D ) ) / ( kappaS - 1.D )

  ;;Fini
  F                      = Finv * ( FK1 - PI^((-1.D)*kappaS+1.D) * one_m_one_over_R_B * FK2 - PI^((-1.D)*kappaS+2.D) * one_m_one_over_R_B^(2.D) * FK3 )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GE 4 THEN BEGIN
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 4: PDs wrt to n

     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;All partial derivatives
  ENDIF

  RETURN,F

END

