;2017/04/18
FUNCTION GET_DENTON_ET_AL_2006__EQUATORIAL_DENSITY,ne_F,mlt,RE_F,Lshell

  COMPILE_OPT IDL2,STRICTARRSUBS

  a =  6.0D
  b = -3.0D
  c = 0.28D

  alpha_LT = 2.0D - 0.43D * mlt

  logne = ALOG10(ne_F)
  logRd = ALOG10(Lshell / RE_F)

  IF logRd LT 0 THEN BEGIN
     PRINT,"Bogusness, that's not how it works. You mean to tell me you're farther out than the distance at which this field line threads the equatorial plane?"
     STOP
  ENDIF

  l = a + b * ( logne - alpha_LT * logRd ) + c * ( logne^2.D - 2.D * alpha_LT * logne * logRd + alpha_LT^2.D * logRd^2.D )
  m = 2.D * c * logRd * ( alpha_LT * logRd - logne ) -  ( b * logRd + 1 )
  n = c * logRd^2.D

  determinant  = SQRT( m^2.D - 4.D * l * n )

  alpha_0_pos  = ( (-1.D) * m + determinant ) / 2.D / l
  alpha_0_neg  = ( (-1.D) * m - determinant ) / 2.D / l

  alpha_model  = alpha_LT + [alpha_0_pos,alpha_0_neg]

  ne_0         = ne_F * ( Lshell / RE_F )^((-1.D) * alpha_model)

  RE_step      = 0.05
  dRE          = CEIL ((Lshell - 1.D)/RE_step)
  RE_arr       = INDGEN(dRE)*RE_step + 1.D

  ne_pos       = ne_0[0] * ( Lshell / RE_F )^alpha_model[0]
  ne_neg       = ne_0[1] * ( Lshell / RE_F )^alpha_model[1]

  window       = WINDOW(DIMENSIONS=[800,600])

  plotPos      = PLOT(RE_arr,ne_pos, $
                      NAME='N!De!N (+$\alpha$)', $
                      XTITLE='R!DE!N', $
                      YTITLE='Density (cm!U-3!N)', $
                      /CURRENT)
  
  plotNeg      = PLOT(RE_arr,ne_neg, $
                      NAME='N!De!N (-$\alpha$)', $
                      /OVERPLOT)

  legend       = LEGEND(TARGET=[plotPos,plotNeg])

  STOP

END
