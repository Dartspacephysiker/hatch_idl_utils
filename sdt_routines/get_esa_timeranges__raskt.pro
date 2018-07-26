;;2018/07/25
;;Returns only timeranges for valid ESA data, and is therefore much quicker than GET_ESA_TIMERANGES: we avoid all calculation of moments.
;;Can also get timestamps for all valid ESA data via OUT_TIME_ARRAY keyword

FUNCTION GET_ESA_TIMERANGES__RASKT, $
   IONS=ions, $
   BURST=burst, $
   OUT_TIME_ARRAY=times

  COMPILE_OPT IDL2,STRICTARRSUBS

  which = (KEYWORD_SET(ions) ? "ie" : "ee") + (KEYWORD_SET(burst) ? "b" : "s")
  routine = "GET_FA_"+which

  t           = 0.
  tEnd        = 0.
  dat         = CALL_FUNCTION(routine,t,/ST)
  IF ~dat.valid THEN BEGIN
     print,' ERROR: No FAST electron survey data -- ' + routine + '(t,/st) returned invalid data'
     RETURN,-1
  ENDIF
  endDat      = CALL_FUNCTION(routine,tEnd,/EN)
  IF ~endDat.valid THEN BEGIN
     print,' ERROR: Bad FAST electron survey data?? ' + routine + '(t,/EN) returned invalid data'
     RETURN,-1
  ENDIF

  t1 = t
  t2 = tEnd
  minInd = dat.index
  maxInd = endDat.index

  n = 0
  max = maxInd-minInd + 100 < 300000l ; this limits a tplot str to < 77 MBytes
  ;; max = 30000                   ; this could be improved
  tmax = 1.e30

  times   = DBLARR(max)

  ;; times[0] = t1

  WHILE (dat.valid NE 0) AND (n LT max) DO BEGIN
     IF (dat.valid EQ 1) THEN BEGIN

        t2 = dat.time
        times[n] = dat.time
        n = n+1
     ENDIF ELSE BEGIN
        print,'Invalid packet, dat.valid ne 1, at: ',time_to_str(dat.time)
     ENDELSE

     dat = call_function(routine,t,CALIB=calib,/ad)
;	dat = call_function(routine,t,CALIB=calib,index=idx)
     IF dat.valid NE 0 THEN IF dat.time GT tmax THEN dat.valid=0

  ENDWHILE

  times = times[0:n-1]

  RETURN,[t1,t2]

END
