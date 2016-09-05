;;2016/09/05
;ENERGY = vector of energies in eV for which f(energy) is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

; This function returns s^3/cm^3-km^3
PRO MAXWELL_FLUX,X,A,F,pder

  COMPILE_OPT idl2
  
  ;; speedOfLight   = DOUBLE(29979245800.) ;cm / s
  ;; electron_mass  = DOUBLE(5.685e-16)   ;eV/(cm/s)^2

  energy            = X
  speedOfLight      = DOUBLE(299792.458) ;km / s
  electron_mass     = DOUBLE(5.1099891e5)/speedOfLight^2   ;eV/c^2


  
  IF N_ELEMENTS(A) LT 4 THEN BEGIN
     PRINT,"Must have all four estimates for Maxwell dist! ( E_b, T, kappa, n[, bulkAngle, m] )"
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
  IF kappa LE 1.5D THEN BEGIN
     kappa          = 1.5001D
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FG1*FG2*FG3

  ;; Finv                   = n * ( electron_mass / 2.D ) ^ (1.5D) ;* DOUBLE(1e15)

  ;;Converts to eFlux units
  Finv               = n * ( electron_mass / 2.D ) ^ (1.5D) * DOUBLE(2e5) * energy^2 / inMass^2 ;/  inDT

  ;;First chunk
  FG1                = DOUBLE(( !PI * T  ))^(-1.5D)

  ;;Second chunk
  FG2_innie          = SQRT(energy) - SQRT(E_b)
  FG2                = EXP( (-1.D) * ( FG2_innie )^2 / T ) 

  ;;Third chunk, in parts that become useful later for PDs
  ;; f_e                = (SQRT(energy) - SQRT(E_b)*COS(bulkAngle))^2 + E_b * (SIN(bulkAngle))^2
  ;; fk3_in             = 1.D + f_e / ( (kappa - 1.5D) * T )
  ;; FG3                = ( fk3_in ) ^ ( -1.D - kappa )

  ;;Fini
  F                  = Finv*FG1*FG2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GE 4 THEN BEGIN
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b
     pdwrtE_b        = Finv * FG1 * FG2 * ( FG2_innie / T / SQRT(E_b) ) 

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     pdwrtT          = (-1.D) * Finv * FG1 * FG2 / T * ( ( FG2_innie )^2 / T + 1.5D ) 
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important
     pdwrtkappa      = REPLICATE(0,N_ELEMENTS(energy))

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
