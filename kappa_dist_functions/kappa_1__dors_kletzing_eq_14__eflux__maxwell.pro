;2016/10/20
;POT    = vector of bulk energies (or rather, the potential) in eV for which ε_||(pot) is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: blank, to keep things parallel with KAPPA_1__DORS_KLETZING_EQ_15__EFLUX
; A[3]: n,         Plasma density in the magnetosphere, *NOT* the ionosphere (but they should be about the same :)
; A[4]: R_B,       Magnetic mirror ratio, B_alt / B_msphere
; A[5]: m,         Particle mass (in this case, electron mass
;
;A future version may want to fit to R_B
;; PRO KAPPA_1__DORS_KLETZING_EQ_14__EFLUX__MAXWELL,pot,A,F,pders

;;   COMPILE_OPT IDL2,STRICTARRSUBS
  
;;   electron_charge        = DOUBLE(1.6e-19)

;;   electron_mass          = DOUBLE(5.109989e9)   ;eV/c^2 (where c is in cm/s)
;;   speedOfLight           = DOUBLE(29979245800.) ;cm / s
  
;;   IF N_ELEMENTS(A) LT 4 THEN BEGIN
;;      PRINT,"Must have all four estimates for kappa dist! ( E_b, T, kappa, n[, bulkAngle, m] )"
;;      PRINT,"Returning..."
;;      RETURN
;;   ENDIF

;;   pot                 = DOUBLE(pot)

;;   CASE NDIMEN(A) OF
;;      1: BEGIN
;;         E_b                    = DOUBLE(A[0])
;;         T                      = DOUBLE(A[1])
;;         kappa                  = DOUBLE(A[2])
;;         n                      = DOUBLE(A[3])
;;         R_B                    = N_ELEMENTS(A) GT 4 ? DOUBLE(A[4]) : 0
;;         m                      = N_ELEMENTS(A) GT 5 ? DOUBLE(A[5]) : (electron_mass*(speedOfLight^(2.D)))
;;      END
;;      ELSE: BEGIN
;;         E_b                    = DOUBLE(A[*,0])
;;         T                      = DOUBLE(A[*,1])
;;         kappa                  = DOUBLE(A[*,2])
;;         n                      = DOUBLE(A[*,3])
;;         R_B                    = (SIZE(A))[1] GT 4 ? DOUBLE(A[*,4]) : REPLICATE(0,(SIZE(A))[1])
;;         m                      = (SIZE(A))[1] GT 5 ? DOUBLE(A[*,5]) : (electron_mass*(speedOfLight^(2.D)))
;;      END
;;   ENDCASE

;;   Chi                    = pot/T

;;   ;;Make sure kappa is fo' real
;;   IF kappa LT 1.5D THEN BEGIN
;;      PRINT,"Kappa must be GE 1.5D!"
;;      PRINT,"Returning..."
;;      RETURN
;;   ENDIF

FUNCTION KAPPA_1__DORS_KLETZING_EQ_14__EFLUX__MAXWELL,T_m,dens_m,pot,R_B, $
   IN_POTBAR=in_potBar, $
   OUT_POTBAR=potBar, $
   POT_IN_JOULES=pot_in_joules, $
   MASS=mass

  COMPILE_OPT IDL2,STRICTARRSUBS
  
  eCharge                = DOUBLE(1.6e-19)

  ;;a few constants
  IF KEYWORD_SET(mass) THEN BEGIN
     ;;Assume this is in eV/c^2 (with c in km/s instead of m/s)
     ;; speedOfLight        = 299792.458D ;km / s
     inMass              = mass / 1D6 ;(convert c^2 from m^2/s^2 to km^2/s^2)
  ENDIF ELSE BEGIN
     speedOfLight        = 299792458D                 ;m / s
     inMass              = 5.1099891D5/speedOfLight^2 ;eV/c^2
  ENDELSE


  ;; speedOfLight           = DOUBLE(299792458.) ;m / s
  ;; electron_mass          = DOUBLE(5.109989e5) / speedOfLight^2.D ;eV/c^2 (where c is in m/s)

  n                      = DOUBLE(dens_m * 1000000.D)  ;dens_m in m^-3
  IF KEYWORD_SET(pot) THEN BEGIN
     pot                 = DOUBLE(pot)
  ENDIF

  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(pot/T_m) ;potential normalized by temperature
     IF KEYWORD_SET(pot_in_joules) THEN BEGIN
        potBar          *= eCharge
     ENDIF
  ENDELSE

  ;;Have to translate T to the most probable speed, w, which is how Dors and Kletzing cast it
  ;; w_sq  = 
  toJ                    = 1.6D-19 ;eV to J

  R_BS    = R_B
  IF R_B LE 1.0 THEN BEGIN
     PRINT,"R_B must be GT 1.0!"
     R_BS = 1.0001
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  ;;DONE
  Finv                   = n * toJ * (T_m)^(1.5D) / SQRT(2.D * !PI * inMass) * R_B

  ;;First chunk
  FK1                    = 2.D + potBar

  ;;Second chunk
  FK2                    = ( potBar + 2.D ) * (1.D - 1.D / R_B) * EXP( (-1.D) * potBar / ( R_BS - 1.D ) )

  ;;Fini
  F                      = Finv * ( FK1 - FK2 )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  ;; IF N_PARAMS() GE 4 THEN BEGIN
  ;;    ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;    ;;Slot 1: PDs wrt to pot

  ;;    ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;    ;;Slot 2: PDs wrt to T
  ;;    ;; pdwrtT              = (-1.5D) * Finv * SQRT( (!PI * (kappa - 1.5D))^(-3) * T^(-5) ) * FK     
  ;;    ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;    ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important

  ;;    ;;The third chunk, which is the worst of the worst

  ;;    ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;    ;;Slot 4: PDs wrt to n

     
  ;;    ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;    ;;All partial derivatives

  ;;    STOP

  ;; ENDIF

  RETURN,F

END

