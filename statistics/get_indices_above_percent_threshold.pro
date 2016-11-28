;;11/28/16
FUNCTION GET_INDICES_ABOVE_PERCENT_THRESHOLD, $
   data,threshold, $
   ;; PERCENT_TOLERANCE=tolerance, $
   ;; STARTVAL=startVal, $
   ;; N_ATTEMPTS=n_attempts, $
   BELOW_NOT_ABOVE=below, $
   OUT_FINAL_VAL=val, $
   NAN=nan, $
   COMPLEMENT_I=complement_i, $
   VERBOSE=verbose

  COMPILE_OPT IDL2

  ;; this = LINDGEN(N_ELEMENTS(data))/DOUBLE(N_ELEMENTS(data))
  CASE 1 OF
     KEYWORD_SET(NaN): BEGIN
        this = WHERE(FINITE(data),nTot)
        IF nTot EQ 0 THEN BEGIN
           PRINT,'All datas are NaNheads. Out.'
           RETURN,-1
        ENDIF

        nTot = DOUBLE(nTot)
        adjThresh = threshold/100.*nTot
        srtInds = SORT(data[this])
        ;; data = data[srtInds]
        
        srtAbove_i = VALUE_CLOSEST2(LINDGEN(nTot),adjThresh)
        val = data[this[srtInds[srtAbove_i]]]
        CASE 1 OF
           KEYWORD_SET(below): BEGIN
              above_ii = WHERE(data[this] LE val,nAbove,COMPLEMENT=complement_ii)
           END
           ELSE: BEGIN
              above_ii = WHERE(data[this] GE val,nAbove,COMPLEMENT=complement_ii)
           END
        ENDCASE
        above_i       = this[TEMPORARY(above_ii)]
        complement_i  = TEMPORARY(this[TEMPORARY(complement_ii)])
        finalProsjent = nAbove/nTot*100.

     END
     ELSE: BEGIN
        nTot = DOUBLE(N_ELEMENTS(data))
        adjThresh = threshold/100.*nTot
        srtInds = SORT(data)
        ;; data = data[srtInds]
        
        srtAbove_i = VALUE_CLOSEST2(LINDGEN(nTot),adjThresh)
        val = data[srtInds[srtAbove_i]]
        CASE 1 OF
           KEYWORD_SET(below): BEGIN
              above_i = WHERE(data LE val,nAbove,COMPLEMENT=complement_i)
           END
           ELSE: BEGIN
              above_i = WHERE(data GE val,nAbove,COMPLEMENT=complement_i)
           END
        ENDCASE
        finalProsjent = nAbove/nTot*100.

     END
  ENDCASE

  PRINT,'Final prosjent: ' + STRCOMPRESS(finalProsjent,/REMOVE_ALL) + '%'

  RETURN,above_i

  ;; STOP

  ;; IF N_ELEMENTS(data) EQ 0 THEN BEGIN
  ;;    PRINT,"GET_INDICES_ABOVE_PERCENT_THRESHOLD: No data provided!"
  ;;    RETURN,-1
  ;; ENDIF

  ;; IF N_ELEMENTS(data) EQ 1 THEN BEGIN
  ;;    PRINT,"GET_INDICES_ABOVE_PERCENT_THRESHOLD: Seriously?"
  ;;    STOP
  ;;    ;; RETURN,-1
  ;; ENDIF

  ;; thresh = N_ELEMENTS(threshold) GT 0 ? threshold : 5.0
  ;; tol    = N_ELEMENTS(tolerance) GT 0 ? tolerance : 0.5
  ;; n      = N_ELEMENTS(n_attempts) GT 0 ? n_attempts : 1000

  ;; Ntot   = DOUBLE(N_ELEMENTS(data))

  ;; ;; med = 
  ;; ;; curBar = med

  ;; botBar = KEYWORD_SET(startVal) ? startVal : MEDIAN(data)
  ;; ;; topBar = med * 1.5

  ;; botProsjent = 100.-N_ELEMENTS(WHERE(data GE botBar))/Ntot*100.
  ;; ;; topProsjent = 100.-N_ELEMENTS(WHERE(data GE (topBar),/NULL))/Ntot*100.

  ;; botDist     = (botProsjent-threshold)
  ;; ;; topDist     = (topProsjent-threshold)
  ;; ;; prosjent    = ( ABS(botDist) LT ABS(topDist) ) ? botProsjent : topProsjent
  ;; prosjent = botProsjent
  ;; nTries = 0
  ;; WHILE ( ABS( prosjent - threshold ) GT tol ) DO BEGIN

  ;;    ;; IF ABS(topDist) GT ABS(botDist) THEN BEGIN
  ;;    ;;    ;; topBar = (botBar + topBar)/2.
  ;;    ;;    topBar = topBar*(1+(threshold-botProsjent)/100.)
  ;;    ;;    topProsjent = 100.-N_ELEMENTS(WHERE(data GE (topBar),/NULL))/Ntot*100.
  ;;    ;;    IF KEYWORD_SET(verbose) THEN BEGIN
  ;;    ;;       PRINT,FORMAT='("Top bar, prosjent now ",T30,G0.3,T45,F0.2)', $
  ;;    ;;             topBar,topProsjent
  ;;    ;;    ENDIF
        
  ;;    ;; ENDIF ELSE BEGIN
  ;;       ;; IF botProsjent LT threshold THEN BEGIN
  ;;       botBar      = botBar*(1+(threshold-botProsjent)/100.)
  ;;       botProsjent = 100.-N_ELEMENTS(WHERE(data GE botBar,/NULL))/Ntot*100.
  ;;       ;; ENDIF ELSE BEGIN
        
  ;;       ;; ENDELSE
  ;;       IF KEYWORD_SET(verbose) THEN BEGIN
  ;;          PRINT,FORMAT='("Bot bar, prosjent now ",T30,G0.3,T45,F0.2)', $
  ;;                botBar,botProsjent
  ;;       ENDIF
  ;;    ;; ENDELSE

  ;;    ;;Recalculate percentages
  ;;    botDist     = (botProsjent-threshold)
  ;;    ;; topDist     = (topProsjent-threshold)
  ;;    ;; prosjent    = ( ABS(botDist) LT ABS(topDist) ) ? botProsjent : topProsjent
  ;;    prosjent    = botProsjent

  ;;    nTries++
  ;;    IF nTries GE n THEN BEGIN
  ;;       couldnt = 1
  ;;       BREAK
  ;;    ENDIF
  ;; ENDWHILE

  ;; IF KEYWORD_SET(couldnt) THEN BEGIN
  ;;    PRINT,"Couldn't locate threshold after " + $
  ;;          STRCOMPRESS(nTries,/REMOVE_ALL) + " attempts"
  ;; ENDIF

  ;; ;; winner  = ( ABS(botDist) LT ABS(topDist) ) ? 0 : 1
  ;; ;; candidates = [botBar,topBar]

  ;; ;; winner  = 0
  ;; ;; candidates = botBar

  ;; above_i = WHERE(data GE botBar,nAbove)
  ;; finalProsjent = nAbove/nTot*100.

  ;; PRINT,'Final prosjent: ' + STRCOMPRESS(finalProsjent,/REMOVE_ALL) + '%'

  ;; RETURN,above_i
END
