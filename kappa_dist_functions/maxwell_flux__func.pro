;2017/02/20
;X      = vector of energies in eV for which f(energy) is to be calcked
;P      = vector of function params:
;DP     = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

; This function returns s^3/cm^3-km^3

FUNCTION MAXWELL_FLUX__FUNC,X,P,DP, $
                            UNITS=units, $
                            MASS=mass

  COMPILE_OPT IDL2,STRICTARRSUBS

  energy                 = X

  ;; speedOfLight           = DOUBLE(29979245800.) ;cm / s
  speedOfLight           = DOUBLE(299792.458) ;km / s

  electron_mass          = KEYWORD_SET(mass) ? DOUBLE(mass) : $
                           DOUBLE(5.1099891e5)/speedOfLight^2 ;eV/c^2

  
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
  bulkAngle              = DOUBLE(P[4])*!PI / 180.0
  ;; inMass                 = 5.6856602e-06             ;mass in eV/(km/s)^2
  inMass                 = electron_mass                ;mass in eV/(km/s)^2
  m                      = TEMPORARY(electron_mass)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  Finv            = n * ( m / 2.D ) ^ (1.5D) ;* DOUBLE(1e15)

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
  FK1             = (DOUBLE(!PI * T ))^(-1.5D)*EXP( (-1.D) * ( SQRT(energy)-SQRT(E_b) )^2 / T )

  ;;Fini
  F               = Finv*FK1

  RETURN,F

END

