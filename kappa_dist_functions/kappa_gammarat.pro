;;11/08/16
FUNCTION KAPPA_GAMMARAT,kappa

  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE N_ELEMENTS(kappa) OF
     0:
     1: BEGIN
        CASE 1 OF
           (kappa GE 20): BEGIN
              gammaRat = EXP(LNGAMMA( kappa + 1.0D )-LNGAMMA( kappa - 0.5D ))
           END
           ELSE: BEGIN
              gammaRat = GAMMA( kappa + 1.D) / GAMMA( kappa - 0.5D )
           END
        ENDCASE
     END
     ELSE: BEGIN
        gammaRat = MAKE_ARRAY(N_ELEMENTS(kappa),/DOUBLE)

        bigKappa = WHERE(kappa GE 20,nBig,COMPLEMENT=smKappa)

        ;;For starters
        gammaRat[smKappa] = GAMMA( kappa[smKappa] + 1.D) / GAMMA( kappa[smKappa] - 0.5D )

        CASE nBig OF
           0:
           ELSE: BEGIN
              gammaRat[bigKappa] = EXP(LNGAMMA( kappa[bigKappa] + 1.0D ) - $
                                      LNGAMMA( kappa[bigKappa] - 0.5D ))
           END

        ENDCASE
     END
  ENDCASE

  RETURN,gammaRat

END
