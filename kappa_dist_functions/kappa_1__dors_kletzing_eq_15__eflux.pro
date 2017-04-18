;2016/10/??
FUNCTION KAPPA_1__DORS_KLETZING_EQ_15__EFLUX,kappa,T_m,dens_m,pot,R_B, $
   IN_POTBAR=in_potBar, $
   OUT_POTBAR=potBar, $
   OUT_P_OVER_K_TH=pot_over_K_th, $
   POT_IN_JOULES=pot_in_joules, $
   PLOT_TERMS=plot_terms, $
   MASS=mass

  COMPILE_OPT IDL2,STRICTARRSUBS
  
  ;; helpMeNotBeZero        = 1.e-6
  toJ                    = 1.6e-19 ;eV to J

  eCharge                = DOUBLE(1.6e-19)

  speedOfLight           = DOUBLE(299792458.) ;m / s

  ;;a few constants
  IF KEYWORD_SET(mass) THEN BEGIN
     ;;Assume this is in eV/c^2 (with c in km/s instead of m/s)
     ;; speedOfLight        = 299792.458D ;km / s
     inMass              = mass / 1D6 ;(convert c^2 from m^2/s^2 to km^2/s^2)
  ENDIF ELSE BEGIN
     speedOfLight        = 299792458D ;m / s
     inMass              = 5.1099891D5/speedOfLight^2 ;eV/c^2
  ENDELSE

  ;; electron_mass          = DOUBLE(5.109989e5) / speedOfLight^2.D ;eV/c^2 (where c is in m/s)

  n                      = DOUBLE(Dens_m * 1000000.D)  ;dens_m in m^-3
  IF KEYWORD_SET(in_potBar) THEN BEGIN
     potBar              = in_potBar
  ENDIF ELSE BEGIN
     potBar              = DOUBLE(pot/T_m) ;potential normalized by temperature
     IF KEYWORD_SET(pot_in_joules) THEN BEGIN
        potBar          *= eCharge
     ENDIF
  ENDELSE

  IF ARG_PRESENT(pot_over_K_th) THEN BEGIN
     pot_over_K_th       = potBar ; / ( kappa - 1.5D)
  ENDIF

  ;;Make sure kappa is fo' real
  kappaS = DOUBLE(kappa)
  IF kappa LE 1.5 THEN BEGIN
     kappaS = 1.500001D
     ;; PRINT,"Kappa must be GE 1.5D!"
     ;; PRINT,"Returning..."
     ;; RETURN,-1
  ENDIF

  ;;Still fo' real
  IF kappa EQ 2.0 THEN kappaS = 2.00001D 

  R_BS    = R_B
  IF R_B LE 1.0 THEN BEGIN
     PRINT,"R_B must be GT 1.0!"
     R_BS = 1.00001D
  ENDIF

  ;;Have to translate T to the most probable speed, w, which is how Dors and Kletzing cast it
  w_sq   = 2.D * T_m / inMass * ( (kappaS - 1.5D) / kappaS )
  
  ;; PI  = 1.D + potBar / ( (kappaS - 1.5D + helpMeNotBeZero) * ( R_B - 1.D ) )
  PItmp  = potBar / ( (kappaS - 1.5D ) * ( R_BS - 1.D ) )
  PI     = 1.D + PItmp
  PI1    = PI^((-1.D)*kappa+1.D)
  PI2    = PI^((-1.D)*kappa+2.D)
  ;;Binomial approx
  ;; IF (WHERE(PItmp LE 0.01D))[0] NE -1 THEN BEGIN
  ;;    ;; PI[WHERE(PItmp LE 0.01D)] = 1.D + (1.0D - kappa ) * PItmp[WHERE(PItmp LE 0.01D)] 
  ;;    ind = WHERE(PItmp LE 0.01D)
  ;;    PI1[ind] = 1.D + ( 1.D - kappa ) * PItmp[ind]
  ;;    PI2[ind] = 1.D + ( 2.D - kappa ) * PItmp[ind]
  ;; ENDIF

  one_m_one_over_R_B     = (1.D - 1.D/R_B)
  CASE 1 OF
     (kappa GE 20): BEGIN
        gammaRat = EXP(LNGAMMA( kappa + 1.0D )-LNGAMMA( kappa - 0.5D ))
     END
     ELSE: BEGIN
        gammarat = GAMMA( kappa + 1.D ) / GAMMA( kappa - 0.5D )
     END
  ENDCASE
  A_k                    = gammaRat / kappa^(1.5D)
  ;; A_k                    = GAMMA(kappa + 1.D) / ( kappa^(1.5D) * GAMMA( kappa - 0.5D ) )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  ;; ;;The old way, with helpMeNotBeZero
  ;; Finv                   = n * inMass * toJ * (w_sq)^(1.5D) / 4.D / SQRT(!PI) * kappaS^(2.D) * A_k * R_B / ( ( kappaS - 1.D ) * ( kappaS - 2.D + helpMeNotBeZero) )
  ;; FK1                    = 2.D + (kappaS - 2.D + helpMeNotBeZero) / (kappaS - 1.5D + helpMeNotBeZero ) * potBar
  ;; FK2                    = ( ( kappaS - 2.D ) / ( kappaS - 1.D ) + potBar * ( kappaS - 2.D ) / ( kappaS - 1.5D + helpMeNotBeZero ) ) * ( kappaS / ( (kappaS - 1.D) * (R_B - 1.D) ) + 1.D )
  ;; FK3                    = 1.D + ( 1.D + kappaS / ( R_B - 1.D + helpMeNotBeZero ) ) / ( kappaS - 1.D )


  ;;This relationship is not valid for potBar below ~0.5
  CASE 1 OF
     kappa LT 2.1: BEGIN
        potThresh = 10
     END
     ELSE: BEGIN
        potThresh = 1.0
     END
  ENDCASE

  IF KEYWORD_SET(old_way) THEN BEGIN
     ;;The other old way, before shifting kappa - 2.D inside
     Finv  = n * inMass * toJ * w_sq^(1.5D) / 4.D / SQRT(!PI) $
             * kappa^(2.D) * A_k * R_B $
             / ( ( kappa - 1.D ) * ( kappaS - 2.D ) )
     ;;First chunk
     FK1   = 2.D + ( kappa - 2.D ) / ( kappaS - 1.5D ) * potBar
     
     ;;Second chunk
     FK2   = ( ( kappa - 2.D ) / ( kappa - 1.D ) + potBar * ( kappa - 2.D ) / ( kappaS - 1.5D ) ) * ( kappa / ( (kappa - 1.D) * (R_BS - 1.D) ) + 1.D )
     
     ;;Third chunk, in parts that become useful later for PDs
     FK3   = 1.D + ( 1.D + kappa / ( R_BS - 1.D ) ) / ( kappa - 1.D )
     
     ;;Fini
     F     = Finv * ( FK1 - PI1 * one_m_one_over_R_B * FK2 - PI2 * one_m_one_over_R_B^(2.D) * FK3 )
     
  ENDIF ELSE BEGIN

     Finv  = n * inMass * toJ * w_sq^(1.5D) / 4.D / SQRT(!PI) $
             * kappa^(2.D) * A_k * R_B / ( kappa - 1.D )

     ;;First chunk
     FK1   = 2.D / ( kappaS - 2.D ) + 1.D / ( kappaS - 1.5D ) * potBar

     ;;Second chunk
     FK2a  = ( 1.D / ( kappa - 1.D ) + potBar / ( kappaS - 1.5D ) )
     CASE 1 OF
        (kappa GE 100): BEGIN
           FK2b  = ( kappa / ( (kappa - 1.D) * (R_BS - 1.D) ) + 1.D )
        END
        ELSE: BEGIN
           ;; FK2a  = ( 1.D / ( kappa - 1.D ) + potBar / ( kappaS - 1.5D ) )
           FK2b  = ( 1.D / ( (1.D - 1.D/kappa) * (R_BS - 1.D) ) + 1.D )
        END
     ENDCASE

     ;; FK2   = ( 1.D / ( kappa - 1.D ) + potBar / ( kappaS - 1.5D ) ) * ( kappa / ( (kappa - 1.D) * (R_BS - 1.D) ) + 1.D )
     FK2   = FK2a * FK2b

     ;;Third chunk, in parts that become useful later for PDs
     FK3   = ( 1.D + ( 1.D + kappa / ( R_BS - 1.D ) ) / ( kappa - 1.D ) ) / ( kappaS - 2.D )

     ;;Fini
     F     = Finv * ( FK1 - PI1 * one_m_one_over_R_B * FK2 - PI2 * one_m_one_over_R_B^(2.D) * FK3 )

  ENDELSE

  IF KEYWORD_SET(plot_terms) THEN BEGIN
     ;;Plot terms
     t1      = Finv * FK1
     t2      = Finv * ( (-1.D) * PI1 * FK2 )
     t3      = Finv * ( (-1.D) * PI2 * one_m_one_over_R_B^(2.D) * FK3 )
     Tot     = t1+t2+t3

     name1   = 'Term 1' ;; + ( N_ELEMENTS(WHERE(t1 LT 0)) GE (N_ELEMENTS(t1) / 2.) ? $
     ;;     ' (most neg!)' : '')
     name2   = 'Term 2' ;; + ( N_ELEMENTS(WHERE(t2 LT 0)) GE (N_ELEMENTS(t2) / 2.) ? $
     ;;     ' (most neg!)' : '')
     name3   = 'Term 3' ;; + ( N_ELEMENTS(WHERE(t3 LT 0)) GE (N_ELEMENTS(t3) / 2.) ? $
     ;;     ' (most neg!)' : '')
     nameTot = 'Sum   ' ;; + ( N_ELEMENTS(WHERE(t3 LT 0)) GE (N_ELEMENTS(t3) / 2.) ? $
     ;;     ' (most neg!)' : '')

     negt1   = WHERE(t1 LT 0,nNegt1)
     negt2   = WHERE(t2 LT 0,nNegt2)
     negt3   = WHERE(t3 LT 0,nNegt3)
     negTot  = WHERE(tot LT 0,nNegTot)
     ;; IF (nNegt1 GE (N_ELEMENTS(t1) / 2.)) THEN BEGIN
     ;; name1 += ' (most neg!)'
     IF nNegt1 GT 0 THEN BEGIN
        name1 += ' (some neg!)'
        t1     = ABS(t1)
     ENDIF

     ;; IF (nNegt2 GE (N_ELEMENTS(t2) / 2.)) THEN BEGIN
     ;; name2 += ' (most neg!)'
     IF nNegt2 GT 0 THEN BEGIN
        name2 += ' (some neg!)'
        t2     = ABS(t2)
     ENDIF

     ;; IF (nNegt3 GE (N_ELEMENTS(t3) / 2.)) THEN BEGIN
     ;; name3 += ' (most neg!)'
     IF nNegt3 GT 0 THEN BEGIN
        name3 += ' (some neg!)'
        t3     = ABS(t3)
     ENDIF

     IF nNegTot GT 0 THEN BEGIN
        nameTot += ' (some neg!)'
        tot     = ABS(tot)
     ENDIF

     t2Col   = 'Green'
     t3Col   = 'Red'
     totCol  = 'Blue'

     t1Sym   = '+'
     t2Sym   = 'x'
     t3Sym   = '*'
     totSym  = 'tu'

     yRange  = MINMAX([t1,t2,t3,tot])
     t1Plot  = PLOT(potbar,t1, $
                    NAME=name1, $
                    XRANGE=MINMAX(potBar), $
                    YRANGE=yRange, $
                    XLOG=1, $
                    YLOG=1, $
                    /CURRENT)
     t2Plot  = PLOT(potbar,t2, $
                    NAME=name2, $
                    XRANGE=MINMAX(potBar), $
                    XLOG=1, $
                    YLOG=1, $
                    COLOR=t2Col, $
                    /OVERPLOT)
     t3Plot  = PLOT(potbar,t3, $
                    NAME=name3, $
                    XRANGE=MINMAX(potBar), $
                    XLOG=1, $
                    YLOG=1, $
                    COLOR=t3Col, $
                    /OVERPLOT)
     totPlot = PLOT(potbar,tot, $
                    NAME=nameTot, $
                    XRANGE=MINMAX(potBar), $
                    XLOG=1, $
                    YLOG=1, $
                    COLOR=totCol, $
                    /OVERPLOT)
     legend  = LEGEND(TARGET=[t1plot,t2plot,t3plot,totPlot], $
                      POSITION=[0.4,0.8], $
                      /NORMAL)

     IF nNegt1 GT 0 THEN BEGIN
        t1bad = PLOT(potbar[negt1],t1[negt1], $
                     LINESTYLE='', $
                     SYMBOL=t1Sym, $
                     ;; COLOR=t1Col, $
                     /OVERPLOT)
     ENDIF

     IF nNegt2 GT 0 THEN BEGIN
        t2bad = PLOT(potbar[negt2],t2[negt2], $
                     LINESTYLE='', $
                     SYMBOL=t2Sym, $
                     COLOR=t2Col, $
                     /OVERPLOT)
     ENDIF

     IF nNegt3 GT 0 THEN BEGIN
        t3bad = PLOT(potbar[negt3],t3[negt3], $
                     LINESTYLE='', $
                     SYMBOL=t3Sym, $
                     COLOR=t3Col, $
                     /OVERPLOT)
     ENDIF

     IF nNegTot GT 0 THEN BEGIN
        totBad = PLOT(potbar[negtot],tot[negTot], $
                      LINESTYLE='', $
                      SYMBOL=totSym, $
                      COLOR=totCol, $
                      /OVERPLOT)
     ENDIF
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;If the procedure is called with four parameters, calculate the
  ;;partial derivatives.
  IF N_PARAMS() GE 4 THEN BEGIN
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 1: PDs wrt to E_b

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 2: PDs wrt to T
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 3: PDs wrt to kappa--The worst of all, and the most important

     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Slot 4: PDs wrt to n

     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;All partial derivatives
  ENDIF

  ;;Replace with low-pot vals if dangerous
  ;; danger                 = WHERE(potBar LT potThresh,nDanger)
  ;; IF nDanger GT 0 THEN BEGIN
  ;;    ;; PRINT,nDanger, ' dangerous eFluxes'
  ;;    FDanger = KAPPA_1__DORS_KLETZING_EQ_14__EFLUX__MAXWELL(T_m,dens_m,pot,R_B, $
  ;;                                                           IN_POTBAR=in_potBar, $
  ;;                                                           OUT_POTBAR=potBar, $
  ;;                                                           POT_IN_JOULES=pot_in_joules)
  ;;    FDanger *= A_k * ( ( kappa - 1.5D ) / ( kappa - 1.D ) )^(1.5D)
  ;;    IF nDanger EQ N_ELEMENTS(potBar) THEN RETURN,FDanger
  ;; ENDIF

  ;; IF nDanger GT 0 THEN F[danger] = FDanger[danger]

  RETURN,F

END

