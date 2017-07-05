;2017/07/03
PRO LOGNORMAL_PARAM_ESTIMATES,data,muHat,sigmaHat, $
                              OUT_BESTFIT=bestFit

  COMPILE_OPT IDL2,STRICTARRSUBS

  n  = N_ELEMENTS(data)

  IF n LE 1 THEN $
     MESSAGE,"Nope."
  
  ;; muHat    = TOTAL( ALOG(data) )/n
  muHat    = MEAN(ALOG(data))
  ;; sigmaHat = SQRT(TOTAL( (ALOG(data)-muHat)^2.D ) / n )
  sigmaHat = STDDEV(ALOG(data))

  IF ARG_PRESENT(bestFit) THEN BEGIN

     ;;From CGHISTOPLOT
     binsize = (3.5 * STDDEV(ALOG10(data))) / n^(0.3333)

     this    = HISTOGRAM(ALOG10(data), $
                         BINSIZE=binsize, $
                         LOCATIONS=locations)
     maxN    = MAX(this,maxInd)

     xVals   = POWGEN(MIN(data),MAX(data),1D0+binsize)

     bestFit = 1.D / ( xVals * sigmaHat * SQRT(2 * !PI) ) * EXP( -( ALOG(xVals) - muHat )^(2.D) / ( 2.D * sigmaHat^2.D ) ) 

     bestFit = TRANSPOSE([[xVals],[bestFit * maxN / MAX(bestFit)]])

  ENDIF

END
