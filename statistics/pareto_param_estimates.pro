;2017/07/03
;Maximum-likelihood estimators (at least for c—mu is just a shift) from
;https://math.stackexchange.com/questions/465619/maximum-likelihood-function-mle-for-levy-distribution
PRO PARETO_PARAM_ESTIMATES,data,c,mu

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; IF N_ELEMENTS(WHERE(data) LT 0,/NULL) GT 0 THEN BEGIN
  ;;    PRINT,"No way it's a Pareto, bro. Bag it."

  n  = N_ELEMENTS(data)
  IF n LE 0 THEN $
     MESSAGE,"Must provide datazzz"

  mu = MIN(data)
  
  c  = n / TOTAL(1 / (data-mu))

END
