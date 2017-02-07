;;10/06/16
FUNCTION MAKE_EVENLY_SPACED_TIME_SERIES,time, $
                                        START_T=start_t, $
                                        STOP_T=stop_t, $
                                        DELTA_T=delta_t, $
                                        N_POINTS=n_points, $
                                        EXCLUDE_ENDPOINT=exclude_endpoint

  COMPILE_OPT IDL2

  ;;Get start and stop times
  nTimes = N_ELEMENTS(time)
  CASE 1 OF
     nTimes GT 0: BEGIN
        IF SIZE(time,/TYPE) NE 5 THEN BEGIN
           PRINT,'Time not of type double! Out ...'
           RETURN,-1
        ENDIF
        strt = time[0]
        stop = time[1]

        IF KEYWORD_SET(start_t) THEN BEGIN
           strt = start_t
        ENDIF

        IF KEYWORD_SET(stop_t) THEN BEGIN
           stop      = stop_t
           have_stop = 1
        ENDIF

     END
     ELSE: BEGIN
        IF ~KEYWORD_SET(start_t) OR ~KEYWORD_SET(stop_t) THEN BEGIN
           PRINT,"Must provide start and stop time if not providing time series!"
           RETURN,-1
        ENDIF

        strt      = start_t
        stop      = stop_t
        have_stop = 1
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(n_points) AND KEYWORD_SET(delta_t) AND (have_stop): BEGIN
        PRINT,"Can't set delta_t, n_points, and stop_t all at once! Try again ..." 
        RETURN,-1
     END
     KEYWORD_SET(n_points): BEGIN
        IF N_ELEMENTS(n_points) GT 1 THEN BEGIN
           PRINT,"N points must be a single value ..."
           RETURN,-1
        ENDIF
        IF n_points LT 0 THEN BEGIN
           PRINT,"n_points must be positive ..."
           RETURN,-1
        ENDIF
     END
     KEYWORD_SET(delta_t): BEGIN
        delta_t  = DOUBLE(delta_t)
        n_points = CEIL((stop-strt)/delta_t) - KEYWORD_SET(exclude_endpoint)
     END
     ELSE: BEGIN

        CASE 1 OF
           nTimes GT 0: BEGIN
              PRINT,"No delta provided! Going to just use the mean delta ..."
              delta_t = MEAN(time[1:-1]-time[0:-2])
           END
           ELSE: BEGIN
              PRINT,"No delta provided! Going to just use 1 s ..."
              delta_t = 1.D
           END
        ENDCASE
        
        n_points = CEIL((stop-strt)/delta_t) - KEYWORD_SET(exclude_endpoint)

     END
  ENDCASE

  RETURN,(strt + DINDGEN(n_points)*delta_t)

END
