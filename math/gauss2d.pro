;+
; NAME:              GAUSS2D
;
;
;
; PURPOSE:           Return the value of a 2D Gaussian given delta_x, delta_y, and the values of the std. deviations in each direction
;
;
;
; CATEGORY:          Matemática
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;                           ct  = COLORTABLE(72, /REVERSE)                             
;                           dx  = MAKE_ARRAY(101,VALUE=1.0) # (INDGEN(101) - 50.)/50.
;                           dy  = (INDGEN(101) - 50.)/50.   # MAKE_ARRAY(101,VALUE=1.0)
;                           sX  = 0.25 & sY = 0.25                               
;                           dat = GAUSS2D(dx,dy,sx,sy)
;                           c   = CONTOUR(dat,dx,dy,RGB_TABLE=ct,/FILL)               
;                           cb  = COLORBAR(TITLE='2D Gaussian')
;                           PRINT,TOTAL(GAUSS2D(dx,dy,sx,sy))
;
;
;
; MODIFICATION HISTORY:     2016/12/01 Barnebarn
;
;-
FUNCTION GAUSS2D,dX,dY,sX,sY, $
                 DOUBLE=double, $
                 INTEGRAL=integral, $
                 INTEG_XLIM=integ_xLim, $
                 INTEG_YLIM=integ_yLim, $
                 INTEG_NPTS=integ_nPts
                 

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__gauss2d.pro
  ;; RETURN,( 1. / ( 2. * !PI * sX * sY ) ) * EXP( (-1.) * ( dX^2 / ( 2. * sX^2 ) + dY^2 / ( 2. * sY^2 )) )
  ;; RETURN,( 0.5 / ( !PI * sX * sY ) ) * EXP( (-1.) * ( ( dX / ( SQRT(2.) * sX ))^2 + (dY / ( SQRT(2.) * sY ))^2 ) )
  ;; RETURN,( 0.5 / ( !PI * sX * sY ) ) * EXP( (-0.5) * ( ( dX / sX )^2 + ( dY / sY )^2 ) )

  IF KEYWORD_SET(integral) THEN BEGIN
     g2d__sX  = sX
     g2d__sY  = sY
     g2d__yBounds = KEYWORD_SET(integ_yLim) ? integ_yLim : [MIN(dY),MAX(dY)]
     nPts     = ([6,10,20,48,96])[VALUE_LOCATE([6,10,20,48,96],KEYWORD_SET(integ_nPts) ? integ_nPts : N_ELEMENTS(dX))]
     integral = INT_2D('GAUSS2D_INTEGRAL', $
                       KEYWORD_SET(integ_xLim) ? integ_xLim : [MIN(dX),MAX(dX)], $
                       'GAUSS2D_YLIMS',nPts, $
                       DOUBLE=double, $
                       /ORDER)
     RETURN,integral
  ENDIF

  answer = KEYWORD_SET(double) ? DOUBLE(dx) : dx
  answer[*] = 0.D

  ;;Min expressible on this 'chine
  minexp = ALOG((MACHAR(DOUBLE=double)).xmin)
  
        ;; z = ( xi - parms[1] )/sigma
  z = ( ( dX / sX )^2 + ( dY / sY )^2 )
  ;; zz = z*z

  w = WHERE( z LT -2*minexp, nw )
  IF (nw GT 0) THEN BEGIN
     answer[w] = ( 0.5 / ( !PI * sX * sY ) ) * EXP( -z[w] / 2 )
  ENDIF

  ;; answer = ( 0.5 / ( !PI * sX * sY ) ) * EXP( ( ( dX / sX )^2 + ( dY / sY )^2 ) / 2 )

  RETURN,answer


END
