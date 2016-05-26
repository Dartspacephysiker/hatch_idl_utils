;Explanation of members
;;x: When are you?
;;MLT:
;;ILAT:
;;mono:       0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
;;broad:      0 = not broadband    , 1 = broadband    , 2 = strict_broadband
;;diffuse:    0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
;;Je:         Electron number flux (#/cm^2-s)
;;Jee:        Electron energy flux (mW/m^2)
;;nBad_eSpec: 0 = no problems      , N = N bad bins

PRO CAT_EVENTS_FROM_PARSED_SPECTRAL_STRUCT,events,eSpecs_parsed,cat_i

  IF KEYWORD_SET(cat_i) THEN BEGIN
     temp_parsed = { x:eSpecs_parsed.x[cat_i], $
                     MLT:eSpecs_parsed.mlt[cat_i], $
                     ILAT:eSpecs_parsed.ilat[cat_i], $
                     mono:eSpecs_parsed.mono[cat_i], $
                     broad:eSpecs_parsed.broad[cat_i], $
                     diffuse:eSpecs_parsed.diffuse[cat_i], $
                     Je:eSpecs_parsed.Je[cat_i], $          
                     Jee:eSpecs_parsed.Jee[cat_i], $        
                     nBad_eSpec:eSpecs_parsed.nBad_eSpec[cat_i]}
  ENDIF ELSE BEGIN
     temp_parsed = eSpecs_parsed
  ENDELSE

  IF N_ELEMENTS(events) EQ 0 THEN BEGIN
     events = temp_parsed
  ENDIF ELSE BEGIN

     events = { x:[events.x,temp_parsed.x], $
                MLT:[events.mlt,temp_parsed.mlt], $
                ILAT:[events.ilat,temp_parsed.ilat], $
               mono:[events.mono,temp_parsed.mono], $ 
               broad:[events.broad,temp_parsed.broad], $
               diffuse:[events.diffuse,temp_parsed.diffuse], $
               Je:[events.Je,temp_parsed.Je], $               
               Jee:[events.Jee,temp_parsed.Jee], $            
               nBad_eSpec:[events.nBad_eSpec,temp_parsed.nBad_eSpec]}

  ENDELSE
  

END