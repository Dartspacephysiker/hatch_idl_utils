;2016/09/01
;ENERGY = vector of energies in eV for which f(energy) is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

; This function returns s^3/cm^3-km^3
PRO KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__PARED,X,A,F,pder,UNITS=units

  COMPILE_OPT IDL2,STRICTARRSUBS
  
  ;; speedOfLight   = DOUBLE(29979245800.) ;cm / s
  ;; electron_mass  = DOUBLE(5.685e-16)   ;eV/(cm/s)^2

  energy            = X
  speedOfLight      = DOUBLE(299792.458) ;km / s
  electron_mass     = DOUBLE(5.1099891e5)/speedOfLight^2   ;eV/c^2

  IF N_ELEMENTS(units) EQ 0 THEN BEGIN
     units               = 'eFlux'
  ENDIF
  
  IF N_ELEMENTS(A) LT 4 THEN BEGIN
     PRINT,"Must have all four estimates for kappa dist! ( E_b, T, kappa, n[, bulkAngle, m] )"
     PRINT,"Returning..."
     RETURN
  ENDIF

  energy            = DOUBLE(energy)

  E_b               = DOUBLE(A[0])
  T                 = DOUBLE(A[1])
  kappa             = DOUBLE(A[2])
  n                 = DOUBLE(A[3])
  bulkAngle         = N_ELEMENTS(A) GT 4 ? DOUBLE(A[4])*!PI / 180.0 : 0
  inMass            = 5.6856602e-06 ;mass in eV/(km/s)^2

  ;; inMass         = N_ELEMENTS(A) GT 5 ? DOUBLE(A[5])             : 5.6856602e-06 ;mass in eV/(km/s)^2
  ;; inDT           = DOUBLE(A[4])
  ;; m              = electron_mass

  ;;Make sure kappa is fo' real
  kappaS = DOUBLE(kappa)
  IF kappa LE 1.5 THEN BEGIN
     kappaS = 1.500001D
     ;; PRINT,"Kappa must be GE 1.5D!"
     ;; PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  ;;Still fo' real
  IF kappa EQ 2.0 THEN kappaS = 2.00001D 

  ;; helpMeNotBeZero = 0.00001D
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  ;; Finv                   = n * ( electron_mass / 2.D ) ^ (1.5D) ;* DOUBLE(1e15)

  ;;Also employed in KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC
  ;;Converts to eFlux units
  ;; Finv               = n * ( electron_mass / 2.D ) ^ (1.5D) * DOUBLE(2e5) * energy^2 / inMass^2 ;/  inDT
  Finv            = n * ( electron_mass / 2.D ) ^ (1.5D) ;* DOUBLE(1e15)

  CASE STRUPCASE(units) OF
     'EFLUX': BEGIN
        ;;Converts to differential energy flux units, eV/(cm^2-s-sr-eV)
        
        ;; Finv            = n * ( m / 2.D ) ^ (1.5D) * DOUBLE(2e5) * energy^2 / inMass^2 ;/  inDT
        Finv     *= DOUBLE(2e5) * energy^2 / inMass^2 ;/  inDT
     END
     'FLUX': BEGIN
        ;;Convert to differential number flux units, #/(cm^2-s-sr-eV)
        ;; Finv      = n * ( m / 2.D ) ^ (1.5D) * energy
        ;; Finv     *= energy
        Finv     *= DOUBLE(2e5) * energy / inMass^2 ;/  inDT
     END
  ENDCASE

  ;;First chunk
  ;; FK1                = (DOUBLE((!PI * T * (kappa - 1.5D + helpMeNotBeZero ) )))^(-1.5D)
  FK1                = (DOUBLE((!PI * T * (kappaS - 1.5D ) )))^(-1.5D)

  ;;Second chunk
  ;; FK2                = GAMMA(kappa + 1.D) / GAMMA(kappa - 0.5D)
  CASE 1 OF
     (kappa GE 20): BEGIN
        gammaRat  = EXP(LNGAMMA( kappa + 1.0D )-LNGAMMA( kappa - 0.5D ))
     END
     ELSE: BEGIN
        gammarat  = GAMMA( kappa + 1.0D ) / GAMMA( kappa - 0.5D )
     END
  ENDCASE
  FK2             = gammaRat

  ;;Third chunk, in parts that become useful later for PDs
  f_e                = (SQRT(energy) - SQRT(E_b)*COS(bulkAngle))^2 + E_b * (SIN(bulkAngle))^2
  ;; fk3_in             = 1.D + f_e / ( (kappa - 1.5D + helpMeNotBeZero ) * T )
  fk3_in             = 1.D + f_e / ( (kappaS - 1.5D ) * T )
  FK3                = ( fk3_in ) ^ ( -1.D - kappa )

  ;;Fini
  F                  = Finv*FK1*FK2*FK3

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GE 4 THEN BEGIN
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b
     ;; pdwrtE_b     = Finv * SQRT( !PI^(-3) * (T * (kappa - 1.5D)^(-5) ) ) * FK2 * (-1.D - kappa) * $
     ;;                       ( fk3_in )^(-2.D - kappa) * ( 2.D*( SQRT(energy/E_b) - COS(bulkAngle) ) + (SIN(bulkAngle))^2 )
     ;;Pretty sure there are issues with the above
     ;; pdwrtE_b        = Finv * SQRT( !PI^(-3) * (T * (kappa - 1.5D ) )^(-5) ) * FK2 * (-1.D - kappa) * $
     ;;                   ( fk3_in )^(-2.D - kappa) * ( 1.D - SQRT(energy/E_b) * COS(bulkAngle) )
     pdwrtE_b        = Finv * SQRT( !PI^(-3) * (T * (kappaS - 1.5D ) )^(-5) ) * FK2 * (-1.D - kappa) * $
                       ( fk3_in )^(-2.D - kappa) * ( 1.D - SQRT(energy/E_b) * COS(bulkAngle) )

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     ;; pdwrtT       = (-1.5D) * Finv * SQRT( (!PI * (kappa - 1.5D))^(-3) * T^(-5) ) * FK2 * ( -kappa - 1D) * $
     ;;                       ( -1.D - kappa ) * ( fk3_in )^(-2.D - kappa) * ( (-1.D / T ) * (fk3_in - 1.D) )
     ;;Problems with the above
     ;; pdwrtT          = Finv * ( (-1.5D) * SQRT( (!PI * (kappa - 1.5D))^(-3) * T^(-5) ) * FK2 * FK3 + $
     ;;                            FK1 * FK2 * ( 1.D + kappa ) * ( fk3_in )^(-2.D - kappa) * ( f_e / ( (kappa - 1.5D) * T^2)) )
     pdwrtT          = Finv * ( (-1.5D) * SQRT( (!PI * (kappaS - 1.5D))^(-3) * T^(-5) ) * FK2 * FK3 + $
                                FK1 * FK2 * ( 1.D + kappa ) * ( fk3_in )^(-2.D - kappa) * ( f_e / ( (kappaS - 1.5D) * T^2)) )
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important
     ;; dFK1_dkappa     = (-1.5D) * SQRT( (!PI * T )^(-3) * (kappa - 1.5D + helpMeNotBeZero )^(-5) )
     dFK1_dkappa     = (-1.5D) * SQRT( (!PI * T )^(-3) * (kappaS - 1.5D )^(-5) )
     dFK2_dkappa     = FK2 * ( REAL_DIGAMMA(kappa + 1.0D) - REAL_DIGAMMA(kappa - 0.5D) )

     ;;The third chunk, which is the worst of the worst
     ;; dfk3_in_dkappa  = (-1.D) * f_e / ( T * (kappa - 1.5D + helpMeNotBeZero )^2 )
     dfk3_in_dkappa  = (-1.D) * f_e / ( T * (kappaS - 1.5D )^2 )
     dFK3_dkappa     = (-1.D) * FK3 * ( ALOG(fk3_in) + (kappa + 1) * dfk3_in_dkappa / fk3_in )

     pdwrtkappa      = Finv * (   dFK1_dkappa   * FK2         * FK3         $
                                  + FK1         * dFK2_dkappa * FK3         $
                                  + FK1         * FK2         * dFK3_dkappa )

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 4: PDs wrt to n
     pdwrtn          = F/n

     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;All partial derivatives
     pder            = [[pdwrtE_b]  , $
                        [pdwrtT]    , $
                        [pdwrtkappa], $
                        [pdwrtn]    , $
                        [REPLICATE(0,N_ELEMENTS(pdwrtn))]]
  ENDIF

END
