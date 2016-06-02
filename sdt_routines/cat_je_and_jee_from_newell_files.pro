PRO CAT_JE_AND_JEE_FROM_NEWELL_FILES,je,je_parsed, $
                                     jee,jee_parsed, $
                                     cat_i

  IF N_ELEMENTS(cat_i) NE 0 THEN BEGIN
     je_tmp   = je_parsed[cat_i]
     jee_tmp  = jee_parsed[cat_i]
  ENDIF ELSE BEGIN
     je_tmp   = je_parsed
     jee_tmp  = jee_parsed
  ENDELSE

  IF N_ELEMENTS(je) EQ 0 THEN BEGIN
     je       = je_tmp
  ENDIF ELSE BEGIN
     je       = [je,je_tmp]
  ENDELSE
  
  IF N_ELEMENTS(jee) EQ 0 THEN BEGIN
     jee      = jee_tmp
  ENDIF ELSE BEGIN
     jee      = [jee,jee_tmp]
  ENDELSE


END