;2016/08/03
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

FUNCTION KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC,X,P,DP

  COMPILE_OPT idl2
  
  parinfo = REPLICATE({value:0.D, $
                       fixed:0, $
                       parname:'', $
                       relstep:0.D, $
                       mpmaxstep:0.D, $
                       limited:[0,0], $
                       limits:[0.D,0]}, 7)

  Alimited         = [[1,1], $
                      [1,1], $
                      [1,0], $
                      [1,1], $
                      [0,0], $
                      [0,0], $
                      [1,1]]
                      
  Alimits         = [[minE,maxE], $
                     [0,3.5e4], $
                     [1.5D,1e9], $
                     [0,100], $
                     [1,0], $
                     [0,0], $
                     [0,0], $
                     [-180,180]]
                      
  AMaxStep        = REPLICATE(0.D,N_ELEMENTS(A))
  AMaxStep[0]     = 10.
  AMaxStep[2]     = 0.1

  parinfo[*].value = A
  parinfo[*].fixed = fixA
  parinfo[*].parName = ["E_b","T","kappa","N","inDT","m","bulkAngle"]
  parinfo[*].mpmaxstep = AMaxStep

  ;;Bound kappa
  parinfo[2].limited = [1,0]

  energy                 = X

  ;; speedOfLight           = DOUBLE(29979245800.) ;cm / s
  speedOfLight           = DOUBLE(299792.458) ;km / s

  ;; electron_mass          = DOUBLE(5.685e-16)   ;eV/(cm/s)^2
  electron_mass          = DOUBLE(5.1099891e5)/speedOfLight^2   ;eV/c^2

  
  IF N_ELEMENTS(P) LT 4 THEN BEGIN
     PRINT,"Must have all four estimates for kappa dist! ( E_b, T, kappa, n[, bulkAngle, m] )"
     PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  energy                 = DOUBLE(energy)

  E_b                    = DOUBLE(P[0])
  T                      = DOUBLE(P[1])
  kappa                  = DOUBLE(P[2])
  n                      = DOUBLE(P[3])
  inDT                   = DOUBLE(P[4])
  inMass                 = N_ELEMENTS(P) GT 5 ? DOUBLE(P[5])             : 5.6856602e-06 ;mass in eV/(km/s)^2
  bulkAngle              = N_ELEMENTS(P) GT 6 ? DOUBLE(P[6])*!PI / 180.0 : 0
  m                      = N_ELEMENTS(P) GT 7 ? DOUBLE(P[7])             : electron_mass

  ;;Make sure kappa is fo' real
  ;; IF kappa LE 1.5D THEN BEGIN
     ;; PRINT,"Kappa must be GT 1.5D, or else I'll blow up!"
     ;; PRINT,"Returning..."
     ;; RETURN
     ;; kappa = 1.505D
     ;; kappa = 1.5001D
     ;; P[2]  = kappa
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

  Finv                   = n * ( m / 2.D ) ^ (1.5D) ;* DOUBLE(1e15)

  ;;Converts to eFlux units
  Finv                   = n * ( m / 2.D ) ^ (1.5D) * DOUBLE(2e5) * energy^2 / inMass^2 ;/  inDT

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
  IF N_PARAMS() GE 3 THEN BEGIN

     requested              = dp
     dp                     = MAKE_ARRAY(N_ELEMENTS(x),N_ELEMENTS(p),VALUE=X[0]*0)
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b
     ;; pdwrtE_b            = Finv * SQRT( !PI^(-3) * (T * (kappa - 1.5D)^(-5) ) ) * FK2 * (-1.D - kappa) * $
     ;;                       ( fk3_innard )^(-2.D - kappa) * ( 2.D*( SQRT(energy/E_b) - COS(bulkAngle) ) + (SIN(bulkAngle))^2 )
     ;;Pretty sure there are issues with the above
     IF requested[0] GT 0 THEN BEGIN
        pdwrtE_b            = Finv * SQRT( !PI^(-3) * (T * (kappa - 1.5D) )^(-5) ) * FK2 * (-1.D - kappa) * $
                              ( fk3_innard )^(-2.D - kappa) * ( 1.D - SQRT(energy/E_b) * COS(bulkAngle) )
        dp[*,0]             = TEMPORARY(pdwrtE_b)
     ENDIF
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     ;; pdwrtT              = (-1.5D) * Finv * SQRT( (!PI * (kappa - 1.5D))^(-3) * T^(-5) ) * FK2 * ( -kappa - 1D) * $
     ;;                       ( -1.D - kappa ) * ( fk3_innard )^(-2.D - kappa) * ( (-1.D / T ) * (fk3_innard - 1.D) )
     ;;Problems with the above
     IF requested[1] GT 0 THEN BEGIN
        pdwrtT              = Finv * ( (-1.5D) * SQRT( (!PI * (kappa - 1.5D))^(-3) * T^(-5) ) * FK2 * FK3 + $
                                       FK1 * FK2 * ( 1.D + kappa ) * ( fk3_innard )^(-2.D - kappa) * ( f_e / ( (kappa - 1.5D) * T^2)) )
        dp[*,1]             = TEMPORARY(pdwrtT)
     ENDIF
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important
     IF requested[2] GT 0 THEN BEGIN
        dFK1_dkappa         = (-1.5D) * SQRT( (!PI * T )^(-3) * (kappa - 1.5D)^(-5) )
        dFK2_dkappa         = FK2 * ( REAL_DIGAMMA(kappa + 1) - REAL_DIGAMMA(kappa - 0.5D) )

        ;;The third chunk, which is the worst of the worst
        dfk3_innard_dkappa  = (-1.D) * f_e / ( T * (kappa - 1.5D)^2 )
        dFK3_dkappa         = (-1.D) * FK3 * ( ALOG(fk3_innard) + (kappa + 1) * dfk3_innard_dkappa / fk3_innard )

        pdwrtkappa          = Finv * (   dFK1_dkappa   * FK2         * FK3         $
                                         + FK1         * dFK2_dkappa * FK3         $
                                         + FK1         * FK2         * dFK3_dkappa )
        dp[*,2]             = TEMPORARY(pdwrtkappa)
     ENDIF
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 4: PDs wrt to n
     IF requested[3] GT 0 THEN BEGIN
        pdwrtn              = F/n
        dp[*,2]             = TEMPORARY(pdwrtn)
     ENDIF
     
  ENDIF

  RETURN,F

END
