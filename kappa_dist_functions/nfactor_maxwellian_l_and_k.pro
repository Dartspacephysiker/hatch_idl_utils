;2018/02/01
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
FUNCTION DAWSONINTEGRAND,t

  COMPILE_OPT IDL2,STRICTARRSUBS

  RETURN,EXP(t^2.D)

END

FUNCTION NFACTOR_MAXWELLIAN_L_AND_K,E_b,T,kappa,n,R_B, $
                                    MAGNETOSPHERE_TO_FAST__NOT_FAST_TO_MSPH=msph_to_fast, $
                                    EXHAUSTIVE_LIMITCHECK=exhaustive_limitCheck ;; , $
  
                                  ;; MAGICFAC1_OUT=magicFac1O, $
                                  ;; MAGICFAC2_OUT=magicFac2O

  COMPILE_OPT IDL2,STRICTARRSUBS

  nNeeded = MAX([N_ELEMENTS(E_b),N_ELEMENTS(T),N_ELEMENTS(n),N_ELEMENTS(R_B)])

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

     IF (WHERE(R_B2 LE 1))[0] NE -1 THEN STOP

     integLims2  = [[REPLICATE(0.D,nNeeded)],[( SQRT( potBar2/(R_B2-1) ) < 100.D)]]

     nOut        = MAKE_ARRAY(nNeeded,/FLOAT,VALUE=0.D)
     ;; magicFac1O  = MAKE_ARRAY(nNeeded,/DOUBLE,VALUE=0.D)
     ;; magicFac2O  = MAKE_ARRAY(nNeeded,/DOUBLE,VALUE=0.D)
     FOR k=0,nNeeded-1 DO BEGIN

        ;; IF k EQ 81 THEN STOP
        ;; alpha     = alpha2[k]
        integLims = integLims2[k,*]
        potBar    = potBar2[k]
        R_Btmp    = R_B2[k]

        IF KEYWORD_SET(exhaustive_limitCheck) THEN BEGIN
           cont = 0

           cond1 = SQRT(potBar/ (R_Btmp -1) ) GT 15

           CASE 1 OF
              cond1: BEGIN ; SQRT( (RB-1) / !DPI) * Dawson F( SQRT(potBar / (R_Btmp - 1) ) -> SQRT(potBar/!DPI)
                 nOut[k] = 0.5D * ( EXP(potBar) * ERFC(SQRT(potBar)) ) + SQRT(potBar/!DPI)
                 cont = 1
              END
              ELSE: 
           ENDCASE

           IF cont THEN CONTINUE

        ENDIF

        CASE 1 OF
           SQRT(potBar/ (R_Btmp -1) ) LE 0.01: BEGIN ;Use R_B Inf limit?
              
              magicFac1 = 0.5D * ( EXP(potBar) * ERFC(SQRT(potBar)) )
              magicFac2 = SQRT(potBar/!DPI)

              mFac = magicFac1 + magicFac2

           END
           SQRT(potBar/ (R_Btmp -1) ) GE 200: BEGIN ;Use potBar >> R_B limit?
              

              magicFac1 = 0.5D * ( EXP(potBar) * ERFC(SQRT(potBar)) )
              magicFac2 = 0.5D * (R_Btmp - 1.D) / SQRT(potBar * !DPI)

              mFac = magicFac1 + magicFac2

           END
           ELSE: BEGIN          ;Intermediate cases
              ;;MagicFac 1
              magicFac1 = 0.5D * ( EXP(potBar) * ERFC(SQRT(potBar)) )

              ;;Using Simpson's rule
              ;; magicFac2 = QSIMP('ONEYOUNEED',integLims[0],integLims[1])

              magicFac2a = SQRT(( R_Btmp - 1) / !DPI)
              ;;Using five-point Newton-Cotes integration
              dx         = 0.0001D
              nx         = (integLims[1]-integLims[0])/dx
              in_x       = LINDGEN(nx)*dx + integLims[0]
              in_y       = DAWSONINTEGRAND(in_x)
              magicFac2b = EXP(- (potBar / (R_Btmp - 1.D) ) ) * $
                           INT_TABULATED(TEMPORARY(in_x),TEMPORARY(in_y), $
                                         /DOUBLE, $
                                         /SORT)
              
              mFac      = magicFac1 + magicFac2a * magicFac2b

           END
        ENDCASE

        ;; magicFac1O[k] = TEMPORARY(magicFac1)
        ;; magicFac2O[k] = TEMPORARY(magicFac2) + TEMPORARY(magicFac2b)

        nOut[k]   = n2[k] / mFac

     ENDFOR

     IF KEYWORD_SET(msph_to_fast) THEN nOut = 1.D / nOut

     RETURN,nOut

  ENDIF ELSE STOP

END
