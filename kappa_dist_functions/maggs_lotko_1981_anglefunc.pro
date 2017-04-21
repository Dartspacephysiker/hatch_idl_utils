;2017/04/21
;lcw : loss-cone width
;Angular inputs (theta, lcw) assumed to be in degrees
;
;Example:
;; lcw      = 20
;; steep    = 2
;; tmpTheta = (LINDGEN(2001))/2000.D*!PI/2.D
;; window = WINDOW(dimensions=[800,600])
;; colors = GENERATE_LIST_OF_RANDOM_COLORS(10)
;; lstyles = GENERATE_LIST_OF_RANDOM_LINESTYLES(10)
;; plotarr = MAKE_ARRAY(10,/OBJ)
;; FOR k=0,9 DO plotarr[k] = PLOT(tmpTheta*!RADEG, $
;;                                MAGGS_LOTKO_1981_ANGLEFUNC(tmpTheta,lcw,DELTA=k,STEEP=steep), $
;;                                OVERPLOT=k GT 0, $
;;                                COLOR=colors[k], $
;;                                LINESTYLE=lStyles[k], $
;;                                NAME=STRING(FORMAT='("$\delta$ = ",I0)',k), $
;;                                /CURRENT, $
;;                                XTITLE='Angle (deg)', $
;;                                YTITLE='g($\theta$)', $
;;                                THICK=2)
;; leg = LEGEND(TARGET=plotArr)
FUNCTION ML81_AFUNC,theta

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON ML81,Mlcw,Mdelta,MexpFac,MNorm
  
  tanNum  = (TAN( theta / 2.D ))^MexpFac

  ;;angle func, pre-normalization
  RETURN,(1.D - Mdelta * tanNum / ( tanNum + (TAN( Mlcw / 2.D ))^MexpFac )) > 0
  
END
PRO ML81_INIT,lcw,delta,steep, $
              DEGREES=deg

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON ML81
  
  Mlcw  = lcw
  IF KEYWORD_SET(deg) THEN BEGIN
     Mlcw  *= !DTOR
  ENDIF;;  ELSE BEGIN
  ;; ENDELSE

  Mdelta   = N_ELEMENTS(delta) GT 0 ? delta : 1.D
  MexpFac  = 2 * (N_ELEMENTS(steep) GT 0 ? steep : 1)
  
  ;; tmpTheta = (LINDGEN(2001)-1000)/1000.D*!PI
  tmpTheta = (LINDGEN(2001))/2000.D*!PI/2.D
  ;; MNorm    = 1.D / INT_TABULATED(tmpTheta,ML81_AFUNC(tmpTheta))
  PRINT,"Msteep : ",MexpFac/2
  PRINT,"Mdelta : ",Mdelta
  PRINT,"Mlcw   : ",Mlcw
  ;; PRINT,"MNorm  : ",MNorm

END
FUNCTION MAGGS_LOTKO_1981_ANGLEFUNC,theta,lcw, $
                                    DELTA=delta, $
                                    STEEP=steep, $
                                    DEGREES=deg, $
                                    NOINIT=noInit

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; COMMON ML81,Mlcw,Mdelta,MexpFac
  COMMON ML81

  theta2 = theta
  IF KEYWORD_SET(deg) THEN BEGIN
     theta2 *= !DTOR
  ENDIF
     ;; Mlcw   = lcw
  ;; ENDIF ELSE BEGIN
  ;;    ;; Mlcw   = lcw   * !DTOR
  ;; ENDELSE

  ;;Initialize common vars
  IF ~KEYWORD_SET(noInit) THEN BEGIN
     ML81_INIT,lcw,delta,steep, $
               DEGREES=deg
  ENDIF

  ;; RETURN,ML81_AFUNC(theta2)*MNorm
  RETURN,ML81_AFUNC(theta2) / INT_TABULATED(theta2,ML81_AFUNC(theta2))

END
