;2017/07/03
PRO INVERSE_GAUSSIAN_ESTIMATES,data,alpha,beta

  COMPILE_OPT IDL2,STRICTARRSUBS

  PRINT,"NEVER WORKED"
  STOP

  n  = N_ELEMENTS(data)

  IF n LE 1 THEN $
     MESSAGE,"Yeah right."

  mu = MEAN(data)
  nu = 1.D / (n - 1.D) * TOTAL( (data-mu)^2 )

  alpha = mu^2 / nu + 2

  meanArLog = MEAN(ALOG(data))

  ;;initialize C
  C = -ALOG(TOTAL(1.D/data))-1.D/n * TOTAL(ALOG(data)) 

  psiTemp = ALOG(n * alpha) - ALOG(TOTAL(1.D/data))-MEAN(ALOG(data))

  newEst1overAlpha = 1/alpha + (C - psiTemp + ALOG(n * alpha) ) / (alpha^2.D * (1.D/alpha - psiPrimeTemp) )

  

END
