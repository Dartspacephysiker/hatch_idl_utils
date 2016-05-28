;2016/05/09
;ENERGY = vector of energies in eV for which f(energy) is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)

;;We don't use the ones below...
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution
; A[5]: m,         Particle mass (in this case electron mass), in eV/c^2
;
; This function returns s^3/cm^3-km^3
PRO KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,X,A,F,pder

  COMPILE_OPT idl2
  
  energy                 = X

  electron_mass          = DOUBLE(5.109989e9)   ;eV/c^2 (where c is in cm/s)
  speedOfLight           = DOUBLE(29979245800.) ;cm / s
  
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
  inDT                   = DOUBLE(A[4])
  inMass                 = N_ELEMENTS(A) GT 5 ? DOUBLE(A[5]) : 5.6856602e-06 ;mass in eV/(km/s)^2
  bulkAngle              = N_ELEMENTS(A) GT 6 ? DOUBLE(A[6]) : 0
  m                      = N_ELEMENTS(A) GT 7 ? DOUBLE(A[7]) : electron_mass

  ;;Make sure kappa is fo' real
  IF kappa LE 1.5D THEN BEGIN
     PRINT,"Kappa must be GT 1.5D, or else I'll blow up!"
     ;; PRINT,"Returning..."
     ;; RETURN
     kappa = 1.5001D
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  ;; Finv                   = n * ( m / 2.D ) ^ (1.5D) * DOUBLE(1e15)

  ;;Converts to eFlux units
  Finv                   = n * ( m / 2.D ) ^ (1.5D) * DOUBLE(1e15) * 2.D * energy^2 / inMass^2 / DOUBLE(1e5) / inDT

  ;;First chunk
  FK1                    = (DOUBLE((!PI * T * (kappa - 1.5D) )))^(-1.5D)

  ;;Second chunk
  FK2                    = GAMMA(kappa + 1.D) / GAMMA(kappa - 0.5D)

  ;;Third chunk, in parts that become useful later for PDs
  f_e                    = (SQRT(energy) - SQRT(E_b)*COS(bulkAngle))^2 + E_b * (SIN(bulkAngle))^2
  fk3_innard             = 1.D + f_e / ( (kappa - 1.5D) * T )
  FK3                    = ( fk3_innard ) ^ ( -1.D - kappa )

  ;;Fini
  F                      = Finv*FK1*FK2*FK3

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GE 4 THEN BEGIN
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b
     ;; pdwrtE_b            = Finv * SQRT( !PI^(-3) * (T * (kappa - 1.5D)^(-5) ) ) * FK2 * (-1.D - kappa) * $
     ;;                       ( fk3_innard )^(-2.D - kappa) * ( 2.D*( SQRT(energy/E_b) - COS(bulkAngle) ) + (SIN(bulkAngle))^2 )
     ;;Pretty sure there are issues with the above
     pdwrtE_b            = Finv * SQRT( !PI^(-3) * (T * (kappa - 1.5D) )^(-5) ) * FK2 * (-1.D - kappa) * $
                           ( fk3_innard )^(-2.D - kappa) * ( 1.D - SQRT(energy/E_b) * COS(bulkAngle) )

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     ;; pdwrtT              = (-1.5D) * Finv * SQRT( (!PI * (kappa - 1.5D))^(-3) * T^(-5) ) * FK2 * ( -kappa - 1D) * $
     ;;                       ( -1.D - kappa ) * ( fk3_innard )^(-2.D - kappa) * ( (-1.D / T ) * (fk3_innard - 1.D) )
     ;;Problems with the above
     pdwrtT              = Finv * ( (-1.5D) * SQRT( (!PI * (kappa - 1.5D))^(-3) * T^(-5) ) * FK2 * FK3 + $
                                    FK1 * FK2 * ( 1.D + kappa ) * ( fk3_innard )^(-2.D - kappa) * ( f_e / ( (kappa - 1.5D) * T^2)) )
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important
     dFK1_dkappa         = (-1.5D) * SQRT( (!PI * T )^(-3) * (kappa - 1.5D)^(-5) )
     dFK2_dkappa         = FK2 * ( REAL_DIGAMMA(kappa + 1) - REAL_DIGAMMA(kappa - 0.5D) )

     ;;The third chunk, which is the worst of the worst
     dfk3_innard_dkappa  = (-1.D) * f_e / ( T * (kappa - 1.5D)^2 )
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
                            [REPLICATE(0,N_ELEMENTS(pdwrtn))]]                            
  ENDIF

END
