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
FUNCTION NFACTOR_KAPPA_L_AND_K,E_b,T,kappa,n,R_B, $
                               MAGNETOSPHERE_TO_FAST__NOT_FAST_TO_MSPH=msph_to_fast, $
                               EXHAUSTIVE_LIMITCHECK=exhaustive_limitCheck ;; , $
  
                                  ;; MAGICFAC1_OUT=magicFac1O, $
                                  ;; MAGICFAC2_OUT=magicFac2O

  COMPILE_OPT IDL2,STRICTARRSUBS

  nNeeded = MAX([N_ELEMENTS(E_b),N_ELEMENTS(kappa),N_ELEMENTS(T),N_ELEMENTS(n),N_ELEMENTS(R_B)])

  IF N_ELEMENTS(WHERE(E_b LT 0,/NULL)) OR $
     N_ELEMENTS(WHERE(T LT 0,/NULL)) OR $
     N_ELEMENTS(WHERE(kappa LT 0,/NULL)) OR $
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
        N_ELEMENTS(kappa) EQ nNeeded : kappa2 = kappa
        N_ELEMENTS(kappa) EQ 1       : kappa2 = REPLICATE(kappa,nNeeded)
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

     nOut        = MAKE_ARRAY(nNeeded,/FLOAT,VALUE=0.D)

     dir  = '/SPENCEdata/Research/Satellites/FAST/kappa_dists/saves_output_etc/'
     file = 'kappaLandKFacs-TFAST_eq_110_0-nFAST_eq_1_88.sav'

     RESTORE,dir+file

     ;; User requests needs to match assumptions of file
     ;; IF (WHERE(ABS(potBar2-(LK.assumes.potAbove/LK.assumes.T)) GT 1))[0] NE -1 THEN STOP

     ;; Should only ask for one kappa
     IF N_ELEMENTS(UNIQ(kappa2,SORT(kappa2))) GT 1 THEN STOP
     IF N_ELEMENTS(WHERE(kappa2 LT 1.5)) GT 1 THEN STOP
     
     IF N_ELEMENTS(R_B2) EQ N_ELEMENTS(LK.RB) THEN BEGIN

        IF (WHERE(ABS(R_B2-LK.RB)/LK.RB GT 0.2))[0] NE -1 THEN STOP

     ENDIF ELSE BEGIN
        PRINT,"Mismatch between # of RBs available in LK DB and # of RBs you request"
        STOP
     ENDELSE

     matchK = VALUE_CLOSEST2(LK.kappa,kappa2[0],/CONSTRAINED)

     nOut   = n2 / REFORM(LK.ratio[matchK,*])

     IF KEYWORD_SET(msph_to_fast) THEN nOut = 1.D / nOut

     RETURN,nOut

  ENDIF ELSE BEGIN

     R_Btmp     = R_B

     potBar     = DOUBLE(E_b) / DOUBLE(T)

     integLims  = [0.D,( SQRT( potBar/(R_Btmp-1) ) < 100.D)]

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
        ELSE: BEGIN             ;Intermediate cases
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

     nOut   = n / mFac

     IF KEYWORD_SET(msph_to_fast) THEN nOut = 1.D / nOut

     RETURN,nOut

  ENDELSE

END
