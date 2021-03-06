;2016/05/09
;ENERGY = vector of energies in eV for which f(energy) is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[6]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

;;We don't use the ones below...
; A[4]: inDT,      The "delta_t" for integration of electron counts (UNUSED)
; A[5]: m,         Particle mass (in this case electron mass), in eV/c^2
;
; This function returns s^3/cm^3-km^3
PRO KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,X,A,F,pder

  COMPILE_OPT IDL2,STRICTARRSUBS
  
  energy                 = X

  ;; speedOfLight           = DOUBLE(29979245800.) ;cm / s
  speedOfLight           = DOUBLE(299792.458) ;km / s

  ;; electron_mass          = DOUBLE(5.685e-16)   ;eV/(cm/s)^2
  electron_mass          = DOUBLE(5.1099891e5)/speedOfLight^2   ;eV/c^2

  
  IF N_ELEMENTS(A) LT 4 THEN BEGIN
     PRINT,"Must have all four estimates for kappa dist! ( E_b, T, kappa, n[, bulkAngle, m] )"
     PRINT,"Returning..."
     RETURN
  ENDIF

  energy                 = DOUBLE(energy)

  E_b                    = DOUBLE(A[0])
  T                      = DOUBLE(A[1])
  kappa                  = DOUBLE(A[2])
  n                      = DOUBLE(A[3])
  inDT                   = N_ELEMENTS(A) GT 4 ? DOUBLE(A[4])             : 0.0D
  inMass                 = N_ELEMENTS(A) GT 5 ? DOUBLE(A[5])             : 5.6856602e-06 ;mass in eV/(km/s)^2
  bulkAngle              = N_ELEMENTS(A) GT 6 ? DOUBLE(A[6])*!DPI / 180.0 : 0
  m                      = N_ELEMENTS(A) GT 7 ? DOUBLE(A[7])             : electron_mass

  helpMeNotBeZero        = 0.000001D
  ;;Make sure kappa is fo' real
  ;; IF kappa LE 1.5D THEN BEGIN
  ;;    ;; PRINT,"Kappa must be GT 1.5D, or else I'll blow up!"
  ;;    ;; PRINT,"Returning..."
  ;;    ;; RETURN
  ;;    ;; kappa = 1.505D
  ;;    kappa = 1.5001D
  ;;    ;; A[2]  = kappa
  ;; ENDIF

  ;; IF n LE 0.0D THEN BEGIN
  ;;    PRINT,"Density must be GT 0, or else this is total bogus!"
  ;;    ;; PRINT,"Returning..."
  ;;    ;; RETURN
  ;;    n = 0.00000001D
  ;; ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  ;; Finv                   = n * ( m / 2.D ) ^ (1.5D) ;* DOUBLE(1e15)

  ;;Converts to eFlux units
  Finv                   = n * ( m / 2.D ) ^ (1.5D) * DOUBLE(2e5) * energy^2 / inMass^2 ;/  inDT

  ;;First chunk
  FK1                    = (DOUBLE((!DPI * T * (kappa - 1.5D + helpMeNotBeZero ) )))^(-1.5D)

  ;;Second chunk
  FK2                    = GAMMA(kappa + 1.D) / GAMMA(kappa - 0.5D)

  ;;Third chunk, in parts that become useful later for PDs
  f_e                    = (SQRT(energy) - SQRT(E_b)*COS(bulkAngle))^2 + E_b * (SIN(bulkAngle))^2
  fk3_innard             = 1.D + f_e / ( (kappa - 1.5D + helpMeNotBeZero ) * T )
  FK3                    = ( fk3_innard ) ^ ( -1.D - kappa )

  ;;Fini
  F                      = Finv*FK1*FK2*FK3

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GE 4 THEN BEGIN
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b
     ;; pdwrtE_b            = Finv * SQRT( !DPI^(-3) * (T * (kappa - 1.5D)^(-5) ) ) * FK2 * (-1.D - kappa) * $
     ;;                       ( fk3_innard )^(-2.D - kappa) * ( 2.D*( SQRT(energy/E_b) - COS(bulkAngle) ) + (SIN(bulkAngle))^2 )
     ;;Pretty sure there are issues with the above
     pdwrtE_b            = Finv * SQRT( !DPI^(-3) * (T * (kappa - 1.5D + helpMeNotBeZero ) )^(-5) ) * FK2 * (-1.D - kappa) * $
                           ( fk3_innard )^(-2.D - kappa) * ( 1.D - SQRT(energy/E_b) * COS(bulkAngle) )

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     ;; pdwrtT              = (-1.5D) * Finv * SQRT( (!DPI * (kappa - 1.5D + helpMeNotBeZero ))^(-3) * T^(-5) ) * FK2 * ( -kappa - 1D) * $
     ;;                       ( -1.D - kappa ) * ( fk3_innard )^(-2.D - kappa) * ( (-1.D / T ) * (fk3_innard - 1.D) )
     ;;Problems with the above
     pdwrtT              = Finv * ( (-1.5D) * SQRT( (!DPI * (kappa - 1.5D + helpMeNotBeZero ))^(-3) * T^(-5) ) * FK2 * FK3 + $
                                    FK1 * FK2 * ( 1.D + kappa ) * ( fk3_innard )^(-2.D - kappa) * ( f_e / ( (kappa - 1.5D + helpMeNotBeZero ) * T^2)) )
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important
     dFK1_dkappa         = (-1.5D) * SQRT( (!DPI * T )^(-3) * (kappa - 1.5D + helpMeNotBeZero )^(-5) )
     dFK2_dkappa         = FK2 * ( REAL_DIGAMMA(kappa + 1) - REAL_DIGAMMA(kappa - 0.5D) )

     ;;The third chunk, which is the worst of the worst
     dfk3_innard_dkappa  = (-1.D) * f_e / ( T * (kappa - 1.5D + helpMeNotBeZero )^2 )
     dFK3_dkappa         = (-1.D) * FK3 * ( ALOG(fk3_innard) + (kappa + 1) * dfk3_innard_dkappa / fk3_innard )

     pdwrtkappa          = Finv * (   dFK1_dkappa   * FK2         * FK3         $
                                      + FK1         * dFK2_dkappa * FK3         $
                                      + FK1         * FK2         * dFK3_dkappa )

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 4: PDs wrt to n
     pdwrtn              = F/n

     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;All partial derivatives
     pder                = [[pdwrtE_b]  , $
                            [pdwrtT]    , $
                            [pdwrtkappa], $
                            [pdwrtn]    , $
                            [REPLICATE(0,N_ELEMENTS(pdwrtn))], $
                            [REPLICATE(0,N_ELEMENTS(pdwrtn))], $
                            [REPLICATE(0,N_ELEMENTS(pdwrtn))]]
  ENDIF

END
