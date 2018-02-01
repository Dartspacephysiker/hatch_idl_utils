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

  RETURN,EXP(alpha * x^(2.D)) * ERFC(x)

END

FUNCTION DENSITY_FACTOR__BARBOSA_1977,E_b,T,kappa,n,R_B, $
                                      EXHAUSTIVE_LIMITCHECK=exhaustive_limitCheck, $
                                      MAGICFAC1_OUT=magicFac1O, $
                                      MAGICFAC2_OUT=magicFac2O

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON B77,alpha

;  nNeeded = MAX([N_ELEMENTS(E_b),N_ELEMENTS(T),N_ELEMENTS(kappa),N_ELEMENTS(n),N_ELEMENTS(R_B)])
  nNeeded = MAX([N_ELEMENTS(E_b),N_ELEMENTS(T),N_ELEMENTS(n),N_ELEMENTS(R_B)])

  ;; E_b        = DOUBLE(A[0])
  ;; T          = DOUBLE(A[1])
  ;; kappa      = DOUBLE(A[2])
  ;; n          = DOUBLE(A[3])
  ;; R_B        = DOUBLE(A[4])

  IF N_ELEMENTS(WHERE(E_b LT 0,/NULL)) OR $
     N_ELEMENTS(WHERE(T LT 0,/NULL)) OR $
     N_ELEMENTS(WHERE(n LT 0,/NULL)) OR $
     N_ELEMENTS(WHERE(R_B LT 0,/NULL)) $
     THEN STOP

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

     integLims2  = [[-SQRT(potBar2)],[( (alpha2 * potBar2) < 10.D)]]

     nOut        = MAKE_ARRAY(nNeeded,/FLOAT,VALUE=0.D)
     magicFac1O  = MAKE_ARRAY(nNeeded,/DOUBLE,VALUE=0.D)
     magicFac2O  = MAKE_ARRAY(nNeeded,/DOUBLE,VALUE=0.D)
     FOR k=0,nNeeded-1 DO BEGIN

        ;; IF k EQ 81 THEN STOP
        alpha     = alpha2[k]
        integLims = integLims2[k,*]
        potBar    = potBar2[k]

        IF ABS(alpha-1.D) LT 0.00001D THEN BEGIN ;because in this case, magicFac1 is just ZERO
           nOut[k] = n2[k]
           CONTINUE
        ENDIF 

        IF KEYWORD_SET(exhaustive_limitCheck) THEN BEGIN
           cont = 0

           cond1 = (alpha * potBar) GT 50
           cond2 = SQRT(1.D/potBar) GT 50

           CASE 1 OF
              cond1 AND cond2: BEGIN
                 STOP
              END
              cond1: BEGIN ; alpha * V^2 / a^2 >> 1; Eq. 4 in Barbosa [1977]
                 nOut[k] = n2[k] * alpha
                 cont = 1
              END
              cond2: BEGIN
                 nOut[k] = n2[k] / (1.D + 2.D * potBar)
                 cont = 1
              END
              ELSE: 
           ENDCASE

           IF cont THEN CONTINUE

        ENDIF

        CASE 1 OF
           (potBar*alpha) GE 100: BEGIN ;strict-compression limit
              mFac = 1/alpha

              magicFac1 = 0.D
              magicFac2 = 0.D

           END
           ELSE: BEGIN          ;Intermediate cases
              ;;MagicFac 1
              magicFac1 = 2.D * (1.D - alpha) * EXP( DOUBLE(-alpha * potBar) ) * SQRT(potBar) / ERFC(-SQRT(potBar))

              ;;Using Simpson's rule
              ;; magicFac2 = QSIMP('ONEYOUNEED',integLims[0],integLims[1])

              ;;Using five-point Newton-Cotes integration
              dx        = 0.005
              nx        = (integLims[1]-integLims[0])/dx
              in_x      = LINDGEN(nx)*dx + integLims[0]
              in_y      = ONEYOUNEED(in_x)
              magicFac2 = INT_TABULATED(TEMPORARY(in_x),TEMPORARY(in_y), $
                                        /DOUBLE, $
                                        /SORT)
              
              mFac      = 1.D + magicFac1 * magicFac2

           END
        ENDCASE

        magicFac1O[k] = TEMPORARY(magicFac1)
        magicFac2O[k] = TEMPORARY(magicFac2)


        nOut[k]   = n2[k] / mFac

     ENDFOR

     RETURN,nOut

  ENDIF ELSE BEGIN

     alpha       = DOUBLE(1.D / R_B)
     potBar      = DOUBLE(E_b) / DOUBLE(T)

     integLims   = [-SQRT(potBar),( (alpha * potBar) < 10.D) ]



     mFac        = 1.D + $
                  2.D * (1.D - alpha) * EXP( -alpha * potBar ) * SQRT(potBar) / ERFC(-SQRT(potBar)) * QSIMP('ONEYOUNEED',integLims[0],integLims[1],JMAX=21)

     RETURN,n / mFac

  ENDELSE


END
