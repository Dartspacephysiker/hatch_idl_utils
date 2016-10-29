;;10/29/16
PRO JOURNAL__20161029__HOW_TO_DEAL_WITH_LARGE_KAPPA_VALS

  COMPILE_OPT IDL2

  ;;Check it out
  x       = INDGEN(30)+ 0.9
  beta    = 1.0D
  alpha   = -0.5D
  xPlus   = x+beta
  xMinus  = x+alpha

  fmtStr           = '(F0.1)'
  bStr             = STRING(FORMAT=fmtStr,beta)
  aStr             = STRING(FORMAT=fmtStr,alpha)
  gStr             = STRING(FORMAT=fmtStr,beta-alpha)

  straightGammaStr = '$\frac{\Gamma(x+ ' + bStr + ')}{\Gamma(x + ' + aStr + ')}$'
  approxStr        = 'x!U' + gStr + '!N'
  lnGammaStr       = 'e!ULn$\Gamma$(x+ ' + bStr + ')-Ln$\Gamma$(x + ' + aStr + ')!N'

  this    = PLOT(x,GAMMA(xPlus)/GAMMA(xMinus), $
                 YLOG=1, $
                 NAME=straightGammaStr)
  that    = PLOT(x,x^(beta-alpha), $
                 NAME=approxStr, $
                 COLOR='RED', $
                 /OVERPLOT)
  those   = PLOT(x,EXP(LNGAMMA(xPlus)-LNGAMMA(xMinus)), $
                 NAME=lnGammaStr, $
                 /OVERPLOT, $
                 COLOR='BLUE')
  legend  = LEGEND(TARGET=[this,that,those], $
                   POSITION=[0.4,0.8], $
                   /NORMAL)

END
