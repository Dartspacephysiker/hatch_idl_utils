;2016/11/01
FUNCTION KAPPA_1__DORS_KLETZING_EQ_15__EFLUX__POT_EQ_0,kappa,T_m,dens_m,pot,R_B, $
   IN_POTBAR=in_potBar, $
   OUT_POTBAR=potBar, $
   OUT_P_OVER_K_TH=pot_over_K_th, $
   POT_IN_JOULES=pot_in_joules, $
   PLOT_TERMS=plot_terms, $
   PLOT_AS_FUNCTION_OF_KAPPA=plot_kappa

  COMPILE_OPT idl2
  
  ;; helpMeNotBeZero        = 1.e-6
  toJ                    = 1.6e-19 ;eV to J

  eCharge                = DOUBLE(1.6e-19)

  speedOfLight           = DOUBLE(299792458.) ;m / s
  electron_mass          = DOUBLE(5.109989e5) / speedOfLight^2.D ;eV/c^2 (where c is in m/s)

  n                      = DOUBLE(Dens_m * 1000000.D)  ;dens_m in m^-3
  potBar                 = 0

  ;;Make sure kappa is fo' real
  kappaS = DOUBLE(kappa)
  CASE N_ELEMENTS(kappa) OF
     1: BEGIN
        IF kappa LE 1.5 THEN BEGIN
           kappaS = 1.500001D
           ;; PRINT,"Kappa must be GE 1.5D!"
           ;; PRINT,"Returning..."
           ;; RETURN,-1
        ENDIF
        ;;Still fo' real
        IF kappa EQ 2.0 THEN kappaS = 2.00001D 

     END
     ELSE: BEGIN
        IF (WHERE(kappa EQ 1.5))[0] NE -1 THEN BEGIN
           kappaS[WHERE(kappa EQ 1.5)] = 1.500001D
        ENDIF
        IF (WHERE(kappa EQ 2.0))[0] NE -1 THEN BEGIN
           kappaS[WHERE(kappa EQ 2.0)] = 2.000001D
        ENDIF
     END
  ENDCASE


  R_BS    = DOUBLE(R_B)
  CASE N_ELEMENTS(R_B) OF
     1: BEGIN
        IF R_B LE 1.0 THEN BEGIN
           R_BS = 1.000001D
           ;; PRINT,"R_B must be GE 1.5D!"
           ;; PRINT,"Returning..."
           ;; RETURN,-1
        ENDIF
     END
     ELSE: BEGIN
        IF (WHERE(R_B EQ 1.0))[0] NE -1 THEN BEGIN
           R_BS[WHERE(R_B EQ 1.0)] = 1.000001D
        ENDIF
     END
  ENDCASE

  ;;Have to translate T to the most probable speed, w, which is how Dors and Kletzing cast it
  w_sq   = 2.D * T_m / electron_mass * ( (kappa - 1.5D) / kappa )
  
  ;; PI  = 1.D + potBar / ( (kappaS - 1.5D + helpMeNotBeZero) * ( R_B - 1.D ) )
  PItmp  = 0.D
  PI     = 1.D + PItmp
  PI1    = PI^((-1.D)*kappa+1.D)
  PI2    = PI^((-1.D)*kappa+2.D)
  ;;Binomial approx
  IF (WHERE(PItmp LE 0.01D))[0] NE -1 THEN BEGIN
     ;; PI[WHERE(PItmp LE 0.01D)] = 1.D + (1.0D - kappa ) * PItmp[WHERE(PItmp LE 0.01D)] 
     ind = WHERE(PItmp LE 0.01D)
     PI1[ind] = 1.D + ( 1.D - kappa ) * PItmp[ind]
     PI2[ind] = 1.D + ( 2.D - kappa ) * PItmp[ind]
  ENDIF

  one_m_one_over_R_B     = (1.D - 1.D/R_B)
  ;; CASE 1 OF
  ;;    (kappa GE 20): BEGIN
  ;;       gammaRat = EXP(LNGAMMA( kappa + 1.0D )-LNGAMMA( kappa - 0.5D ))
  ;;    END
  ;;    ELSE: BEGIN
  ;;       gammarat = GAMMA( kappa + 1.D ) / GAMMA( kappa - 0.5D )
  ;;    END
  ;; ENDCASE
  gammaRat = GAMMA( kappa + 1.D ) / GAMMA( kappa - 0.5D )
  CASE (WHERE(kappa GE 20))[0] OF
     -1: BEGIN
        ;; gammarat = GAMMA( kappa + 1.D ) / GAMMA( kappa - 0.5D )
     END
     ELSE: BEGIN
        in = WHERE(kappa GE 20)
        gammaRat[in] = EXP(LNGAMMA( kappa[in] + 1.0D )-LNGAMMA( kappa[in] - 0.5D ))
     END
  ENDCASE
  A_k                    = gammaRat / kappa^(1.5D)
  ;; A_k                    = GAMMA(kappa + 1.D) / ( kappa^(1.5D) * GAMMA( kappa - 0.5D ) )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chunks of the function
  ;;The whole thing is, as you see below, Finv*FK1*FK2*FK3

  Finv  = n * electron_mass * toJ * w_sq^(1.5D) / 4.D / SQRT(!PI) $
          * kappa^(2.D) * A_k * R_B / ( kappa - 1.D )

  ;;First chunk
  FK1   = 2.D / ( kappaS - 2.D ) 

  ;;Second chunk
  FK2a  = 1.D / ( kappa - 1.D ) 
  FK2b  = ( kappa / ( (kappa - 1.D) * (R_BS - 1.D) ) + 1.D )

  CASE (WHERE(kappa GE 100))[0] OF
     -1: BEGIN
     END
     ELSE: BEGIN
        in    = WHERE(kappa GE 1000)
        FK2b[in]  = ( 1.D / ( (1.D - 1.D/kappa[in]) * (R_BS - 1.D) ) + 1.D )
     END
  ENDCASE

  FK2   = FK2a * FK2b

  ;;Third chunk, in parts that become useful later for PDs
  FK3   = ( 1.D + ( 1.D + kappa / ( R_BS - 1.D ) ) / ( kappa - 1.D ) ) / ( kappaS - 2.D )

  ;;Fini
  F     = Finv * ( FK1 - PI1 * one_m_one_over_R_B * FK2 - PI2 * one_m_one_over_R_B^(2.D) * FK3 )


  IF KEYWORD_SET(plot_terms) THEN BEGIN

     IF KEYWORD_SET(plot_kappa) THEN BEGIN
        plotVar = kappa
        xTitle  = 'Kappa'
     ENDIF ELSE BEGIN
        plotVar = R_B
        xTitle  = 'R!DB!N'
     ENDELSE

     ;;Plot terms
     t1      = Finv * FK1
     t2      = Finv * ( (-1.D) * PI1 * one_m_one_over_R_B * FK2 )
     t3      = Finv * ( (-1.D) * PI2 * one_m_one_over_R_B^(2.D) * FK3 )
     Tot     = t1+t2+t3

     yRange  = MINMAX([t1[WHERE(FINITE(t1))], $
                       t2[WHERE(FINITE(t2))], $
                       t3[WHERE(FINITE(t3))], $
                       tot[WHERE(FINITE(tot))]])

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
        ;; t1     = ABS(t1)
     ENDIF

     ;; IF (nNegt2 GE (N_ELEMENTS(t2) / 2.)) THEN BEGIN
     ;; name2 += ' (most neg!)'
     IF nNegt2 GT 0 THEN BEGIN
        name2 += ' (some neg!)'
        ;; t2     = ABS(t2)
     ENDIF

     ;; IF (nNegt3 GE (N_ELEMENTS(t3) / 2.)) THEN BEGIN
     ;; name3 += ' (most neg!)'
     IF nNegt3 GT 0 THEN BEGIN
        name3 += ' (some neg!)'
        ;; t3     = ABS(t3)
     ENDIF

     IF nNegTot GT 0 THEN BEGIN
        nameTot += ' (some neg!)'
        ;; tot     = ABS(tot)
     ENDIF

     t2Col   = 'Green'
     t3Col   = 'Red'
     totCol  = 'Blue'

     t1Sym   = '+'
     t2Sym   = 'x'
     t3Sym   = '*'
     totSym  = 'tu'

     t1Plot  = PLOT(plotVar,t1, $
                    NAME=name1, $
                    XTITLE=xTitle, $
                    XRANGE=MINMAX(plotVar), $
                    YRANGE=yRange, $
                    XLOG=(~KEYWORD_SET(PLOT_KAPPA)), $
                    YLOG=((WHERE(yRange LE 0.00))[0] EQ -1), $
                    /CURRENT)
     t2Plot  = PLOT(plotVar,t2, $
                    NAME=name2, $
                    XRANGE=MINMAX(plotVar), $
                    XLOG=(~KEYWORD_SET(PLOT_KAPPA)), $
                    YLOG=((WHERE(yRange LE 0.00))[0] EQ -1), $
                    COLOR=t2Col, $
                    /OVERPLOT)
     t3Plot  = PLOT(plotVar,t3, $
                    NAME=name3, $
                    XRANGE=MINMAX(plotVar), $
                    XLOG=(~KEYWORD_SET(PLOT_KAPPA)), $
                    YLOG=((WHERE(yRange LE 0.00))[0] EQ -1), $
                    COLOR=t3Col, $
                    /OVERPLOT)
     totPlot = PLOT(plotVar,tot, $
                    NAME=nameTot, $
                    XRANGE=MINMAX(plotVar), $
                    XLOG=(~KEYWORD_SET(PLOT_KAPPA)), $
                    YLOG=((WHERE(yRange LE 0.00))[0] EQ -1), $
                    COLOR=totCol, $
                    /OVERPLOT)
     legend  = LEGEND(TARGET=[t1plot,t2plot,t3plot,totPlot], $
                      POSITION=[0.4,0.8], $
                      /NORMAL)

     IF nNegt1 GT 0 THEN BEGIN
        t1bad = PLOT(plotVar[negt1],t1[negt1], $
                     LINESTYLE='', $
                     SYMBOL=t1Sym, $
                     ;; COLOR=t1Col, $
                     /OVERPLOT)
     ENDIF

     IF nNegt2 GT 0 THEN BEGIN
        t2bad = PLOT(plotVar[negt2],t2[negt2], $
                     LINESTYLE='', $
                     SYMBOL=t2Sym, $
                     COLOR=t2Col, $
                     /OVERPLOT)
     ENDIF

     IF nNegt3 GT 0 THEN BEGIN
        t3bad = PLOT(plotVar[negt3],t3[negt3], $
                     LINESTYLE='', $
                     SYMBOL=t3Sym, $
                     COLOR=t3Col, $
                     /OVERPLOT)
     ENDIF

     IF nNegTot GT 0 THEN BEGIN
        totBad = PLOT(plotVar[negtot],tot[negTot], $
                      LINESTYLE='', $
                      SYMBOL=totSym, $
                      COLOR=totCol, $
                      /OVERPLOT)
     ENDIF
  ENDIF

  RETURN,F

END

