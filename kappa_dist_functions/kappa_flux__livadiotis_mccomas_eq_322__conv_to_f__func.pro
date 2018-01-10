;2016/08/03
;ENERGY = vector of energies in eV for which f(energy) is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

; This function returns s^3/cm^3-km^3
;
; If in DFSTD units, the max val should be of order 10^-16 or less (or thereabouts).
; Why? look: n = 1.0 cm^-3 = 10^6 m^-3.   T = 500 eV. m = 5.11 keV/c^2. c = 3.0e8
; The frontmost term is n (m / 2 / !DPI / T)^(3.D/2.D)
; code: n = 1D6 & T = 500 & c = 3.0D8 & m = 5.11D5 / c^2 & maxTerm = n * (m / 2.D / !DPI / T)^(3.D/2.D) & PRINT,maxTerm

FUNCTION KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F__FUNC,X,P,DP, $
   UNITS=units, $
   MASS=mass

  COMPILE_OPT IDL2,STRICTARRSUBS

  energy                 = X

  ;; speedOfLight           = DOUBLE(29979245800.) ;cm / s
  speedOfLight           = 299792.458D ;km / s

  inMass                 = KEYWORD_SET(mass) ? DOUBLE(mass) $
                           : 5.1099891D5/speedOfLight^2 ;eV/c^2

  
  IF N_ELEMENTS(P) LT 4 THEN BEGIN
     PRINT,"Must have all four estimates for kappa dist! ( E_b, T, kappa, n[, bulkAngle, m] )"
     PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  IF N_ELEMENTS(units) EQ 0 THEN BEGIN
     units               = 'eFlux'
  ENDIF

  energy                 = DOUBLE(energy)

  E_b                    = DOUBLE(P[0])
  T                      = DOUBLE(P[1])
  kappa                  = DOUBLE(P[2])
  n                      = DOUBLE(P[3])
  bulkAngle              = DOUBLE(P[4])*!DPI / 180.D
  ;; inMass                 = 5.6856602e-06             ;mass in eV/(km/s)^2
  ;; m                      = TEMPORARY(electron_mass)
  ;; inMass                 = TEMPORARY(electron_mass)

  kappaS = DOUBLE(kappa)
  IF kappa LE 1.5 THEN BEGIN
     kappaS = 1.500001D
     ;; PRINT,"Kappa must be GE 1.5D!"
     ;; PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  ;;Still fo' real
  IF kappa EQ 2.0 THEN kappaS = 2.00001D 

  ;; helpMeNotBeZero        = 0.00001D
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  Finv            = n * ( inMass / 2.D ) ^ (1.5D) ;* DOUBLE(1e15)

  CASE STRUPCASE(units) OF
     'EFLUX': BEGIN
        ;;Converts to differential energy flux units, eV/(cm^2-s-sr-eV)
        
        ;; Finv            = n * ( inMass / 2.D ) ^ (1.5D) * 2.D5 * energy^2 / inMass^2 ;/  inDT
        Finv     *= 2.D5 * energy^2 / inMass^2 ;/  inDT
     END
     'FLUX': BEGIN
        ;;Convert to differential number flux units, #/(cm^2-s-sr-eV)
        ;; Finv      = n * ( m / 2.D ) ^ (1.5D) * energy
        ;; Finv     *= energy
        Finv     *= 2.D5 * energy / inMass^2 ;/  inDT
     END
     'DF': BEGIN                ; 'DF'     :  #/(km^3-(cm/s)^3)
        ;;Just leave 'er, Ted!
     END
     'DFSTD': BEGIN             ; 'DFSTD'  :  s^3/m^6
        Finv /= DOUBLE(1D3)
     END
  ENDCASE

  ;;First chunk
  ;; FK1          = (DOUBLE((!DPI * T * (kappa - 1.5D + helpMeNotBeZero ) )))^(-1.5D)
  FK1             = (DOUBLE((!DPI * T * (kappaS - 1.5D ) )))^(-1.5D)

  ;;Second chunk
  ;; FK2          = GAMMA(kappa + 1.D) / GAMMA(kappa - 0.5D)
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
  f_e             = (SQRT(energy) - SQRT(E_b)*COS(bulkAngle))^2 + E_b * (SIN(bulkAngle))^2
  ;; fk3_innard      = 1.D + f_e / ( ( kappa - 1.5D + helpMeNotBeZero ) * T )
  fk3_innard      = 1.D + f_e / ( ( kappaS - 1.5D ) * T )
  FK3             = ( fk3_innard ) ^ ( -1.D - kappa )

  ;;Fini
  F               = Finv*FK1*FK2*FK3

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GT 2 THEN BEGIN

     requested              = dp
     dp                     = MAKE_ARRAY(N_ELEMENTS(x),N_ELEMENTS(p),VALUE=X[0]*0)
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b
     ;; pdwrtE_b            = Finv * SQRT( !DPI^(-3) * (T * (kappa - 1.5D)^(-5) ) ) * FK2 * (-1.D - kappa) * $
     ;;                       ( fk3_innard )^(-2.D - kappa) * ( 2.D*( SQRT(energy/E_b) - COS(bulkAngle) ) + (SIN(bulkAngle))^2 )
     ;;Pretty sure there are issues with the above
     IF requested[0] GT 0 THEN BEGIN
        ;; pdwrtE_b            = Finv * SQRT( !DPI^(-3) * (T * ( kappa - 1.5D + helpMeNotBeZero ) )^(-5) ) * FK2 * (-1.D - kappa) * $
        ;;                       ( fk3_innard )^(-2.D - kappa) * ( 1.D - SQRT(energy/E_b) * COS(bulkAngle) )
        pdwrtE_b            = Finv * SQRT( !DPI^(-3) * (T * ( kappaS - 1.5D ) )^(-5) ) * FK2 * (-1.D - kappa) * $
                              ( fk3_innard )^(-2.D - kappa) * ( 1.D - SQRT(energy/E_b) * COS(bulkAngle) )
        dp[*,0]             = TEMPORARY(pdwrtE_b)
     ENDIF
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     ;; pdwrtT              = (-1.5D) * Finv * SQRT( (!DPI * (kappa - 1.5D + helpMeNotBeZero ))^(-3) * T^(-5) ) * FK2 * ( -kappa - 1D) * $
     ;;                       ( -1.D - kappa ) * ( fk3_innard )^(-2.D - kappa) * ( (-1.D / T ) * (fk3_innard - 1.D) )
     ;;Problems with the above
     IF requested[1] GT 0 THEN BEGIN
        ;; pdwrtT              = Finv * ( (-1.5D) * SQRT( (!DPI * (kappa - 1.5D + helpMeNotBeZero ))^(-3) * T^(-5) ) * FK2 * FK3 + $
        ;;                                FK1 * FK2 * ( 1.D + kappa ) * ( fk3_innard )^(-2.D - kappa) * ( f_e / ( (kappa - 1.5D + helpMeNotBeZero ) * T^2)) )
        pdwrtT              = Finv * ( (-1.5D) * SQRT( (!DPI * (kappaS - 1.5D ))^(-3) * T^(-5) ) * FK2 * FK3 + $
                                       FK1 * FK2 * ( 1.D + kappa ) * ( fk3_innard )^(-2.D - kappa) * ( f_e / ( (kappaS - 1.5D ) * T^2)) )
        dp[*,1]             = TEMPORARY(pdwrtT)
     ENDIF
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important
     IF requested[2] GT 0 THEN BEGIN
        ;; dFK1_dkappa         = (-1.5D) * SQRT( (!DPI * T )^(-3) * (kappa - 1.5D + helpMeNotBeZero )^(-5) )
        dFK1_dkappa         = (-1.5D) * SQRT( (!DPI * T )^(-3) * (kappaS - 1.5D )^(-5) )
        dFK2_dkappa         = FK2 * ( REAL_DIGAMMA(kappa + 1.0D) - REAL_DIGAMMA(kappa - 0.5D) )

        ;;The third chunk, which is the worst of the worst
        ;; dfk3_innard_dkappa  = (-1.D) * f_e / ( T * (kappa - 1.5D + helpMeNotBeZero )^2 )
        dfk3_innard_dkappa  = (-1.D) * f_e / ( T * (kappaS - 1.5D )^2 )
        dFK3_dkappa         = (-1.D) * FK3 * ( ALOG(fk3_innard) + (kappa + 1.0D) * dfk3_innard_dkappa / fk3_innard )

        pdwrtkappa          = Finv * (   dFK1_dkappa   * FK2         * FK3         $
                                         + FK1         * dFK2_dkappa * FK3         $
                                         + FK1         * FK2         * dFK3_dkappa )
        dp[*,2]             = TEMPORARY(pdwrtkappa)
     ENDIF
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 4: PDs wrt to n
     IF requested[3] GT 0 THEN BEGIN
        pdwrtn              = F/n
        dp[*,3]             = TEMPORARY(pdwrtn)
     ENDIF
     
  ENDIF

  RETURN,F

END

