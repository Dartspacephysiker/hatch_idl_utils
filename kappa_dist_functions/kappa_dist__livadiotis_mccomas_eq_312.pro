;2016/05/12
;ENERGY = vector of energies in eV for which f(energy) is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution
; A[4]: m,         Particle mass (in this case electron mass), in eV/c^2
; A[5]: n,         Plasma density (NO!)
PRO KAPPA_DIST__LIVADIOTIS_MCCOMAS_EQ_312,energy,A,F,pders

  COMPILE_OPT idl2
  
  ;; electron_mass          = DOUBLE(1.6e-19)
  electron_mass          = DOUBLE(5.109989e5)   ;eV/c^2

  IF N_ELEMENTS(A) LT 3 THEN BEGIN
     PRINT,"Must have all three estimates for kappa dist! ( E_b, T, kappa[, bulkAngle, m] )"
     PRINT,"Returning..."
     RETURN
  ENDIF

  energy                 = DOUBLE(energy)

  E_b                    = DOUBLE(A[0])
  T                      = DOUBLE(A[1])
  kappa                  = DOUBLE(A[2])
  ;; bulkAngle              = N_ELEMENTS(A) GT 3 ? DOUBLE(A[3]) : 0
  m                      = N_ELEMENTS(A) GT 4 ? DOUBLE(A[4]) : electron_mass
  ;; n                      = N_ELEMENTS(A) GT 5 ? DOUBLE(A[5])

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  ;;First chunk
  FK1             = (2.D * !PI * T / m * (kappa - 1.5D) )^(-1.5D)

  ;;Second chunk
  CASE 1 OF
     (kappa GE 20): BEGIN
        gammaRat  = EXP(LNGAMMA( kappa + 1.0D )-LNGAMMA( kappa - 0.5D ))
     END
     ELSE: BEGIN
        gammarat  = GAMMA( kappa + 1.D) / GAMMA( kappa - 0.5D )
     END
  ENDCASE
  FK2             = gammaRat
  ;; FK2             = GAMMA(kappa + 1.D) / GAMMA(kappa - 0.5D)

  ;;Third chunk, in parts that become useful later for PDs
  ;; f_e          = (SQRT(energy) - SQRT(E_b)*COS(bulkAngle))^2 + E_b * (SIN(bulkAngle))^2
  f_e             = (energy - 2.D * SQRT(E_b) * SQRT(energy) + E_b)
  fk3_innard      = 1.D + f_e / ( (kappaS - 1.5D) * T )
  FK3             = ( fk3_innard ) ^ ( -1.D - kappa )

  ;;Fini
  F               = FK1*FK2*FK3

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GE 4 THEN BEGIN
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b
     pdwrtE_b            = FK1 * FK2 * (-1.D / T) * ( ( kappa + 1) / ( kappaS - 1.5D) ) * fk3_innard^((-1.D) * (kappa + 2.D)) * (1.D - SQRT(energy/E_b))

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     pdwrtT              = FK2 * FK3 * (-1.5D) * ( 2.D * !PI / m * ( kappa - 1.5D) )^(-1.5D) * T^(-2.5D) $
                           + FK1 * FK2 * f_e / T * (kappa + 1.D ) / ( kappaS - 1.5D) * fk3_innard^((-1.D)*(kappa+2.D))
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important
     dFK1_dkappa         = FK2 * FK3 * (-1.5D) * ( 2.D * !PI / m * T )^(-1.5D) * ( kappa - 1.5D ) ^(-2.5D)
     dFK2_dkappa         = FK2 * ( REAL_DIGAMMA(kappa + 1) - REAL_DIGAMMA(kappa - 0.5D) )

     ;;The third chunk, which is the worst of the worst
     ;; dfk3_innard_dkappa  = (-1.D) * f_e / ( T * (kappa - 1.5D)^2 )
     dFK3_dkappa         = f_e / T * ( kappa + 1 ) * ( kappaS - 1.5D )^(-2.D) * fk3_innard^((-1.D)*(kappa+2.D))

     pdwrtkappa          = dFK1_dkappa   * FK2         * FK3         $
                           + FK1         * dFK2_dkappa * FK3         $
                           + FK1         * FK2         * dFK3_dkappa 

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 4: PDs wrt to n
     ;; pdwrtn              = F/n

     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;All partial derivatives
     pder                = [[pdwrtE_b]  , $
                            [pdwrtT]    , $
                            [pdwrtkappa]] ;, $
                            ;; [pdwrtn]]
  ENDIF

END
