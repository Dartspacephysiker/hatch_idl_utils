;2017/04/19
;; E_b = 1000.
;; T = 300.
;; n = 1.0
;; R_Bs = [1,2,3,4,5,6,10,20,30,40,50,60,70,80,100,200,300,400,500,600,700,800,1000,2000,3000,4000,5000,6000,7000,8000,1D4,3D4,3D5]
;; dens = DENSITY_FACTOR__BARBOSA_1977(E_b,T,0,n,R_Bs)
;; plot = PLOT(R_Bs,dens,XTITLE='R!DB!N',YTITLE='N (cm!U-3!N)',/XLOG)
;;'
;;Wanna see the original Fig. 1b in Barbosa [1977
;; E_b = 60^2
;; T = 12^2
;; n = 1.0
;; R_Bs = [1,2,3,4,5,6,10,20,30,40,50,60,70,80,100,200,300,400,500,600,700,800,1000,2000,3000,4000,5000,6000,7000,8000,1D4,3D4,3D5]
;; dens = DENSITY_FACTOR__BARBOSA_1977(E_b,T,0,n,R_Bs)
;; plot = PLOT(R_Bs,1.D/dens,XTITLE='R!DB!N',YTITLE='N!Dionos!N/N!Dmag!N',/XLOG,XRANGE=[1,100])
;;Nailed it, bro
FUNCTION ONEYOUNEED,X

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON B77,alpha

  RETURN,EXP(alpha * x^2) * ERFC(x)

END

FUNCTION DENSITY_FACTOR__BARBOSA_1977,E_b,T,kappa,n,R_B

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON B77,alpha

;  nNeeded = MAX([N_ELEMENTS(E_b),N_ELEMENTS(T),N_ELEMENTS(kappa),N_ELEMENTS(n),N_ELEMENTS(R_B)])
  nNeeded = MAX([N_ELEMENTS(E_b),N_ELEMENTS(T),N_ELEMENTS(n),N_ELEMENTS(R_B)])

  ;; E_b        = DOUBLE(A[0])
  ;; T          = DOUBLE(A[1])
  ;; kappa      = DOUBLE(A[2])
  ;; n          = DOUBLE(A[3])
  ;; R_B        = DOUBLE(A[4])

  IF nNeeded GT 1 THEN BEGIN

     CASE 1 OF
        N_ELEMENTS(E_b) EQ nNeeded : E_b2 = E_b
        N_ELEMENTS(E_b) EQ 1       : E_b2 = REPLICATE(E_b,nNeeded)
     ENDCASE

     CASE 1 OF
        N_ELEMENTS(T) EQ nNeeded : T2 = T
        N_ELEMENTS(T) EQ 1       : T2 = REPLICATE(T,nNeeded)
     ENDCASE

     CASE 1 OF
        N_ELEMENTS(n) EQ nNeeded : n2 = n
        N_ELEMENTS(n) EQ 1       : n2 = REPLICATE(n,nNeeded)
     ENDCASE

     CASE 1 OF
        N_ELEMENTS(R_B) EQ nNeeded : R_B2 = R_B
        N_ELEMENTS(R_B) EQ 1       : R_B2 = REPLICATE(R_B,nNeeded)
     ENDCASE

     alpha2      = DOUBLE(1.D / R_B2)
     potBar2     = DOUBLE(E_b2) / DOUBLE(T2)

     integLims2  = [[-SQRT(potBar2)],[( (alpha2 * potBar2) < 20.D)]]

     nOut        = MAKE_ARRAY(nNeeded,/FLOAT,VALUE=0.D)
     FOR k=0,nNeeded-1 DO BEGIN

        alpha     = alpha2[k]
        integLims = integLims2[k,*]
        potBar    = potBar2[k]

        mFac      = 1.D + $
                    2.D * (1.D - alpha) * EXP( -alpha * potBar ) * SQRT(potBar) / ERFC(-SQRT(potBar)) * QSIMP('ONEYOUNEED',integLims[0],integLims[1])

        nOut[k]   = n2[k] / mFac

     ENDFOR

     RETURN,nOut

  ENDIF ELSE BEGIN

     alpha       = DOUBLE(1.D / R_B)
     potBar      = DOUBLE(E_b) / DOUBLE(T)

     integLims   = [-SQRT(potBar),( (alpha * potBar) < 10.D) ]



     mFac        = 1.D + $
                  2.D * (1.D - alpha) * EXP( -alpha * potBar ) * SQRT(potBar) / ERFC(-SQRT(potBar)) * QSIMP('ONEYOUNEED',integLims[0],integLims[1],JMAX=25)

     RETURN,n / mFac

  ENDELSE


END
