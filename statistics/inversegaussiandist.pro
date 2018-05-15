;2018/05/12
FUNCTION INVERSEGAUSSIANDIST,x,mu,lambda, $
                        STATS=stats

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ARG_PRESENT(stats) THEN BEGIN

     mean = mu
     median = "No known, closed analytic form"
     mode = mu * ( SQRT(1. + 9. * mu^2 / (4. * lambda^2.) ) $
                   - 3. * mu / (2. * lambda) )
     variance = mu^3. / lambda

     stats = {mean   : mean, $
              median : median, $
              mode   : mode, $
              var    : variance}

  ENDIF

  IF KEYWORD_SET(cdf) THEN BEGIN
     
     RETURN,STDNORMALDIST(SQRT(lambda/x) * (x/mu -1.)) $
            + EXP(2. * lambda / mu) * STDNORMALDIST((-1.)*SQRT(lambda/x) * (x/mu + 1.))

  ENDIF

  dist = SQRT(lambda / (2. * !DPI * x^3. ) ) $
     * EXP((-1.)* lambda * (x-mu)^2. / (2. * mu^2. * x))

  this = WHERE(x EQ 0,nZero)
  IF nZero GT 0 THEN dist[this] = 0

  this = WHERE(x LT 0,nLTZero)
  IF nLTZero GT 0 THEN dist[this] = !VALUES.F_NaN

  RETURN,dist

END
