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

  COMPILE_OPT IDL2

  IF N_ELEMENTS(stats_name) EQ 0 THEN stats_name = ''

  CASE 1 OF
     KEYWORD_SET(user_inds): BEGIN
        inds = user_inds
     END
     ELSE: BEGIN
        inds = LINDGEN(N_ELEMENTS(data))
     END
  ENDCASE

  CASE 1 OF
     KEYWORD_SET(pos_only): BEGIN
        inds = CGSETINTERSECTION(inds, $
                                 WHERE(data GT 0), $
                                 NORESULT=-1, $
                                 COUNT=nDiscret)

        stats_name += '--pos'

        IF KEYWORD_SET(verbose) THEN BEGIN
           PRINT,STRCOMPRESS(nDiscret,/REMOVE_ALL) + ' positive inds here'
           PRINT,"(i.e., " + $
                 STRCOMPRESS(N_ELEMENTS(inds)-nDiscret,/REMOVE_ALL) + $
                 " are negative)"
        ENDIF
     END
     KEYWORD_SET(neg_only): BEGIN
        inds = CGSETINTERSECTION(inds, $
                                 WHERE(data LT 0), $
                                 NORESULT=-1, $
                                 COUNT=nDiscret)

        stats_name += '--neg'

        IF KEYWORD_SET(verbose) THEN BEGIN
           PRINT,STRCOMPRESS(nDiscret,/REMOVE_ALL) + ' negative inds here'
           PRINT,"(i.e., " + $
                 STRCOMPRESS(N_ELEMENTS(inds)-nDiscret,/REMOVE_ALL) + $
                 " are positive)"
        ENDIF
     END
     ELSE: BEGIN
        ;; inds = inds
        ;; nDiscret     = N_ELEMENTS(inds)
     END
  ENDCASE

  IF ( KEYWORD_SET(pos_only) OR KEYWORD_SET(neg_only) ) AND $
     KEYWORD_SET(inc_0) $
  THEN BEGIN
     inds = CGSETUNION(inds,WHERE(data EQ 0,/NULL))
        stats_name += '--inc_0'
  ENDIF

  IF KEYWORD_SET(finite) THEN BEGIN
     inds = CGSETUNION(inds,WHERE(FINITE(data),/NULL))
        stats_name += '--finite'
  ENDIF

  IF KEYWORD_SET(null) THEN BEGIN
     IF inds[0] EQ -1 THEN BEGIN
        inds = !NULL
        stats_name = "NULL"
     ENDIF
  ENDIF

  RETURN,inds
END
