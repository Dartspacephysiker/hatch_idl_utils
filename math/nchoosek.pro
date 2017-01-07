;;2017/01/07 Ripped off Erik Rosolowaky's BICO function
FUNCTION NCHOOSEK,n,k

  COMPILE_OPT idl2

  ON_ERROR,2

  L64 = 0B
  
  IF N_ELEMENTS(n) NE N_ELEMENTS(k) THEN BEGIN
     MESSAGE,'Input vectors must be the same size'
  ENDIF

  IF TOTAL(k GT n) GT 0 THEN BEGIN
     MESSAGE,'Elements in second vector must be smaller than those in first'
  ENDIF

  result = (LNGAMMA(n+1)-LNGAMMA(k+1)-LNGAMMA(n-k+1))

  IF MAX(result,/NAN) GT 21 THEN L64 = 1B
  IF MAX(result,/NAN) GT 88 THEN RETURN,EXP(DOUBLE(RESULT))
  IF MAX(result,/NAN) GT 43 THEN RETURN,EXP(RESULT)

  RETURN,FLOOR(0.5+EXP(result),L64=L64)

END