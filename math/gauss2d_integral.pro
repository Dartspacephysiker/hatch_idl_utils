;;12/01/16
FUNCTION GAUSS2D_INTEGRAL,x,y,DOUBLE=double
  @common__gauss2d.pro

  minexp = ALOG((MACHAR(DOUBLE=double)).xmin)
  
  answer    = KEYWORD_SET(double) ? DOUBLE(x) : x
  answer[*] = 0.D

  z = ( ( x / g2d__sX )^2 + ( y / g2d__sY )^2 )

  w = WHERE( z LT -2*minexp, nw )
  IF (nw GT 0) THEN BEGIN
     answer[w] = ( 1 / ( 2.D * !PI * g2d__sX * g2d__sY ) ) * EXP( -z[w] / 2 )
  ENDIF

  ;; RETURN,( 1 / ( 2.D * !PI * g2d__sX * g2d__sY ) ) * EXP( ( ( x / g2d__sX )^2 + ( y / g2d__sY )^2 ) / (-2) )
  RETURN,answer
END