;2016/04/22
;ENERGY = vector of energies in eV for which f(energy) is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,    Plasma bulk energy (eV)
; A[1]: T,      Plasma kinetic temperature (eV)
; A[2]: kappa,  kappa (of course!)
PRO KAPPA_ENERGY__LIVADIOTIS_MCCOMAS_EQ_B12,energy,A,F


  IF N_ELEMENTS(A) LT 3 THEN BEGIN
     PRINT,"Must have all three estimates for kappa dist! [E_b, T, kappa]"
     PRINT,"Returning..."
     RETURN
  ENDIF

  E_b     = DOUBLE(A[0])
  T       = DOUBLE(A[1])
  kappa   = DOUBLE(A[2])

  F       = (4 * !PI * E_b * T * (kappa - 1.5))^(-0.5) $
            * (GAMMA(kappa)/GAMMA(kappa-0.5)) $
            * (   (1 + (SQRT(energy)-SQRT(E_b))^2/( (kappa - 1.5) * T) )^(-kappa) $
                  - (1 + (SQRT(energy)+SQRT(E_b))^2/( (kappa - 1.5) * T) )^(-kappa) )

  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  ;;Slot 1: PDs wrt to E_b
  ;;Slot 2: PDs wrt to T
  ;;Slot 3: PDs wrt to kappa
  IF N_PARAMS() GE 4 THEN BEGIN
     PRINT,"Sorry, not equipped for this. Out."

     ;; pdwrtn     = F/n
     ;; pdwrtw     = (  ((-3)*n/((w^4)*(SQRT(!PI)^3))) $ ;first additive term
     ;;                 * (GAMMA(kappa + 1)/(kappa^(1.5) * GAMMA(kappa-0.5))) $
     ;;                 * (1 + ((v/w)^2)/kappa)^(-(kappa+1))  ) $
     ;;              - (  (kappa+1)*(n/(w*SQRT(!PI))^3) $ ;second '' term
     ;;                   * (GAMMA(kappa + 1)/(kappa^(1.5) * GAMMA(kappa-0.5))) $
     ;;                   * (  (1 + ((v/w)^2)/kappa)^(-(kappa+2)) $
     ;;                        * ((-2)*(V^2)/(w^3)/kappa)   ) )
     ;; pdwrtkappa =  REPLICATE(1.0,N_ELEMENTS(X))
     
     
     ;; pder       = [[pdwrtE_b], $
     ;;               [pdwrtT], $
     ;;               [pdwrtkappa]]
  ENDIF



END