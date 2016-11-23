;;11/23/16
PRO EPS2PDF,filNavn_uten_ekst, $
            REMOVE_EPS=remove_eps

  COMPILE_OPT IDL2

  PRINT,"EPS2PDF: "  + filNavn_uten_ekst

  SPAWN,'epspdf ' + filNavn_uten_ekst +'.eps' + ' ' + filNavn_uten_ekst + '.pdf', $
        EXIT_STATUS=exitStat

  IF exitStat NE 0 THEN PRINT,"Conversion of " + filNavn_uten_ekst + ' failed!'

  IF KEYWORD_SET(remove_eps) AND (exitStat EQ 0) THEN BEGIN
     SPAWN,'rm ' + filNavn_uten_ekst + '.eps',EXIT_STATUS=exitRm
     IF exitRm NE 0 THEN PRINT,"Couldn't remove " + filNavn_uten_ekst + '.eps' + '!!'
  ENDIF

END
