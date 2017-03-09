;;11/08/16
PRO KAPPA_RB_SAFECHECK,kappa,R_B, $
                       KAPPAMIN=kappaMin, $
                       R_BMIN=R_BMin, $
                       KAPPA_SAFE=kappaS, $
                       R_B_SAFE=R_BS

  COMPILE_OPT IDL2,STRICTARRSUBS


  increm = 0.00001D

  ;; kappaM = N_ELEMENTS(kappaMin) EQ 1 ? kappaMin : 1.5D
  ;; R_BM   = N_ELEMENTS(R_BMin)   EQ 1 ? R_BMin   : 1.0D

  CASE N_ELEMENTS(kappa) OF
     0:
     1: BEGIN
        kappaS    = DOUBLE(kappa)

        IF kappa LE 1.5 THEN BEGIN
           ;; kappaS = 1.500001D
           kappaS = 1.5D + increm
           ;; PRINT,"Kappa must be GE 1.5D!"
           ;; PRINT,"Returning..."
           ;; RETURN,-1
        ENDIF

        ;;Still fo' real
        ;; IF kappa EQ 2.0 THEN kappaS = 2.00001D 
        IF kappa EQ 2.0 THEN kappaS = 2.0D + increm

     END
     ELSE: BEGIN
        kappaS = DOUBLE(kappa)

        IF (WHERE(kappa EQ 1.5))[0] NE -1 THEN BEGIN
           ;; kappaS[WHERE(kappa EQ 1.5)] = 1.500001D
           kappaS[WHERE(kappa EQ 1.5)] = kappaM + increm
        ENDIF
        IF (WHERE(kappa EQ 2.0))[0] NE -1 THEN BEGIN
           ;; kappaS[WHERE(kappa EQ 2.0)] = 2.000001D
           kappaS[WHERE(kappa EQ 2.0)] = 2.0D + increm
        ENDIF
     END
  ENDCASE

  CASE N_ELEMENTS(R_B) OF
     0:
     1: BEGIN
        R_BS    = DOUBLE(R_B)
        IF R_B LE 1.0 THEN BEGIN
           ;; R_BS = 1.000001D
           R_BS = 1.0D + increm
           ;; PRINT,"R_B must be GE 1.5D!"
           ;; PRINT,"Returning..."
           ;; RETURN,-1
        ENDIF
     END
     ELSE: BEGIN
        R_BS    = DOUBLE(R_B)
        IF (WHERE(R_B EQ 1.0))[0] NE -1 THEN BEGIN
           ;; R_BS[WHERE(R_B EQ 1.0)] = 1.000001D
           R_BS[WHERE(R_B EQ 1.0)] = 1.0D + increm
        ENDIF
     END
  ENDCASE

END
