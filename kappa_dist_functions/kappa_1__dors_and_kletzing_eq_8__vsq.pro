;2016/04/22
;V=vector of (1-D) velocities (in m/s), squared
;A=vector of function params:
; A[0]: n,      Plasma density in m^-3
; A[1]: w,    Most likely speed (in m/s)
; A[2]: kappa,  kappa (of course!)
PRO KAPPA_1__DORS_KLETZING_EQ_8__VSQ,Vsq,A,F,pder

  IF N_ELEMENTS(A) LT 3 THEN BEGIN
     PRINT,"Must have all thre estimates for kappa dist! [n, w, kappa]"
     PRINT,"Returning..."
     RETURN
  ENDIF

  n       = DOUBLE(A[0])
  w       = DOUBLE(A[1])
  kappa   = DOUBLE(A[2])

  ;;Make sure kappa is fo' real
  kappaS = DOUBLE(kappa)
  IF kappa LE 1.5 THEN BEGIN
     kappaS = 1.50001D
     ;; PRINT,"Kappa must be GE 1.5D!"
     ;; PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  ;;Still fo' real
  IF kappa EQ 2.0 THEN kappaS = 2.0001D 

  CASE 1 OF
     (kappaS GE 20): BEGIN
        gammaRat  = EXP(LNGAMMA( kappaS + 1.0D )-LNGAMMA( kappaS - 0.5D ))
     END
     ELSE: BEGIN
        gammarat  = GAMMA( kappaS + 1.D) / GAMMA( kappaS - 0.5D )
     END
  ENDCASE
  A_k             = gammaRat / kappaS^(1.5D)

  F               = (n/(w*SQRT(!PI))^3) $
                    ;; * (GAMMA(kappa + 1)/(kappa^(1.5) * GAMMA(kappa-0.5))) $
                    * A_k $
                    * (1 + (Vsq/w^2)/kappaS)^(-(kappaS+1))

  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  ;;Slot 1: PDs wrt to n
  ;;Slot 2: PDs wrt to w
  ;;Slot 3: PDs wrt to kappa
  IF N_PARAMS() GE 4 THEN BEGIN
     pdwrtn     = F/n
     pdwrtw     =   (  ((-3)*n/((w^4)*(SQRT(!PI)^3))) $ ;first additive term
                       ;; * (GAMMA(kappaS + 1)/(kappaS^(1.5) * GAMMA(kappaS-0.5))) $
                       * A_k $
                       * (1 + ((v/w)^2)/kappaS)^(-(kappaS+1))  ) $
                  - (  (kappaS+1)*(n/(w*SQRT(!PI))^3) $ ;second '' term
                       ;; * (GAMMA(kappaS + 1)/(kappaS^(1.5) * GAMMA(kappaS-0.5))) $
                       * A_k $
                       * (  (1 + (Vsq/(w^2))/kappaS)^(-(kappaS+2)) $
                            * ((-2)*(Vsq/w^3)/kappaS) ) )
     pdwrtkappa =  REPLICATE(1.0,N_ELEMENTS(Vsq))
     
     
     pder       = [[pdwrtn], $
                   [pdwrtw], $
                   [pdwrtkappa]]
  ENDIF

END