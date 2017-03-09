;;11/26/16
FUNCTION GET_POS_NEG_INDICES,data, $
                             USER_INDS=user_inds, $
                             POS_ONLY=pos_only, $
                             NEG_ONLY=neg_only, $
                             INCLUDE_WHERE_EQ_0=inc_0, $
                             FINITE=finite, $
                             NULL=null, $
                             ;; RETURN_STRUCT=struct, $
                             OUT_STATS_NAME=stats_name, $
                             VERBOSE=verbose

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(stats_name) EQ 0 THEN stats_name = ''

  CASE 1 OF
     KEYWORD_SET(user_inds): BEGIN
        inds = user_inds
     END
     ELSE: BEGIN
        inds = LINDGEN(N_ELEMENTS(data))
     END
  ENDCASE

  IF KEYWORD_SET(finite) THEN BEGIN
     inds = CGSETUNION(inds,WHERE(FINITE(data),/NULL))
        stats_name += '--finite'
  ENDIF

  ;; CASE 1 OF
  ;;    KEYWORD_SET(pos_only): BEGIN
  ;;       get_pos = 1
  ;;       get_neg = 0
  ;;    END
  ;;    KEYWORD_SET(neg_only): BEGIN
  ;;       get_pos = 0
  ;;       get_neg = 1
  ;;    END
  ;;    ELSE: BEGIN
  ;;       get_pos = 1
  ;;       get_neg = 1
  ;;    END
  ;; ENDCASE

  get_pos = ~KEYWORD_SET(neg_only)
  get_neg = ~KEYWORD_SET(pos_only)
  makeStr = get_pos AND get_neg

  IF ~KEYWORD_SET(neg_only) THEN BEGIN
     posInds = CGSETINTERSECTION(inds, $
                              WHERE(data GT 0), $
                              NORESULT=-1, $
                              COUNT=nDiscret)

     IF KEYWORD_SET(pos_only) THEN BEGIN

        stats_name += '--pos'
        inds = TEMPORARY(posInds)

     ENDIF

     IF KEYWORD_SET(verbose) THEN BEGIN
        PRINT,STRCOMPRESS(nDiscret,/REMOVE_ALL) + ' positive inds here'
        PRINT,"(i.e., " + $
              STRCOMPRESS(N_ELEMENTS(inds)-nDiscret,/REMOVE_ALL) + $
              " are negative)"
     ENDIF
  ENDIF

  IF ~KEYWORD_SET(pos_only) THEN BEGIN
     negInds = CGSETINTERSECTION(inds, $
                              WHERE(data LT 0), $
                              NORESULT=-1, $
                              COUNT=nDiscret)

     IF KEYWORD_SET(neg_only) THEN BEGIN

        stats_name += '--neg'
        inds = TEMPORARY(negInds)

     ENDIF

     IF KEYWORD_SET(verbose) THEN BEGIN
        PRINT,STRCOMPRESS(nDiscret,/REMOVE_ALL) + ' negative inds here'
        PRINT,"(i.e., " + $
              STRCOMPRESS(N_ELEMENTS(inds)-nDiscret,/REMOVE_ALL) + $
              " are positive)"
     ENDIF

  ENDIF

  IF ( KEYWORD_SET(pos_only) OR KEYWORD_SET(neg_only) ) AND $
     KEYWORD_SET(inc_0) $
  THEN BEGIN
     inds = CGSETUNION(inds,WHERE(data EQ 0,/NULL))
        stats_name += '--inc_0'
  ENDIF

  CASE 1 OF
     KEYWORD_SET(makeStr): BEGIN
        IF KEYWORD_SET(null) THEN BEGIN
           IF posInds[0] EQ -1 THEN BEGIN
              posInds = !NULL
              stats_name = "NULL"
           ENDIF
           IF negInds[0] EQ -1 THEN BEGIN
              negInds = !NULL
              stats_name = "NULL"
           ENDIF
        ENDIF

        inds = {pos:posInds, $
                neg:negInds}
        
     END
     ELSE: BEGIN
        IF KEYWORD_SET(null) THEN BEGIN
           IF inds[0] EQ -1 THEN BEGIN
              inds = !NULL
              stats_name = "NULL"
           ENDIF
        ENDIF
     END
  ENDCASE
     

  RETURN,inds
END
