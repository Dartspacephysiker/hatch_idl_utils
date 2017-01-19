;;11/23/16
PRO EPS2PDF,filNavn_uten_ekst, $
            REMOVE_EPS=remove_eps, $
            PS=ps, $
            TO_PNG=to_PNG, $
            QUIET=quiet

  COMPILE_OPT IDL2

  ekst = KEYWORD_SET(ps) ? '.ps' : '.eps'

  IF ~KEYWORD_SET(quiet) THEN PRINT,"EPS2PDF: "  + filNavn_uten_ekst

  CASE 1 OF
     KEYWORD_SET(to_PNG): BEGIN
        SPAWN,'/SPENCEdata/Research/tips_tricks_n_assorted/media_manipulation/png_ps_and_image_manipulation/convert_ps_to_png__white_rotated.sh ' + filNavn_uten_ekst + ekst, $
              EXIT_STATUS=exitStat

     END
     ELSE: BEGIN

        SPAWN,'epspdf ' + filNavn_uten_ekst + ekst + ' ' + $
              filNavn_uten_ekst + '.pdf' + $
              (KEYWORD_SET(quiet) ? ' > /dev/null' : ''), $
              (KEYWORD_SET(quiet) ? Result : !NULL), $
              EXIT_STATUS=exitStat
              

     END
  ENDCASE

  IF exitStat NE 0 THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,"Conversion of " + filNavn_uten_ekst + ' failed!'
  ENDIF

  IF KEYWORD_SET(remove_eps) AND (exitStat EQ 0) THEN BEGIN
     SPAWN,'rm ' + filNavn_uten_ekst + ekst,EXIT_STATUS=exitRm
     IF exitRm NE 0 THEN BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,"Couldn't remove " + filNavn_uten_ekst + ekst + '!!'
     ENDIF
  ENDIF

END
