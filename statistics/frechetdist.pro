;2018/05/12
;; The Fréchet distribution
FUNCTION FRECHETDIST,x,alpha,s,pM, $
                     CDF=cdf, $
                     STATS=stats

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~(KEYWORD_SET(alpha) AND KEYWORD_SET(s)) THEN BEGIN
     PRINT,'FRECHETDIST,x,alpha,shape[,min]'
     PRINT,"Provide shape parameter alpha and scale parameter s!"
     RETURN,-1
  ENDIF

  m = N_ELEMENTS(pM) GT 0 ? pM : 0

  IF ARG_PRESENT(stats) THEN BEGIN

     mean = m + s * GAMMA(1.-1./alpha)
     median = m + s / (ALOG(2.))^(1./alpha)
     mode   = m + s * (alpha/(1.+alpha))^(1./alpha)
     var = alpha GT 2 ? $
                s^2 * (GAMMA(1. - 2./alpha) - (GAMMA(1.-1./alpha))^2.) : !VALUES.F_INFINITY

     stats = {mean   : mean  , $
              median : median, $
              mode   : mode  , $
              var    : var   }

  ENDIF

  IF KEYWORD_SET(cdf) THEN BEGIN

     RETURN,EXP((-1.)*((x-m)/s)^((-1.)*alpha))

  ENDIF

  RETURN,alpha / s $
         * ((x-m)/s)^(-1.-alpha) $
         * EXP((-1.)*((x-m)/s)^((-1.)*alpha))

END
