PRO CAT_EVENTS_FROM_PARSED_SPECTRAL_STRUCT,events,eSpecs_parsed,cat_i

  ;; event = { time_e:[events.time_e,tempEvent.time_e], $ ; When are you?

  IF N_ELEMENTS(events) EQ 0 THEN BEGIN
     events = { x:eSpecs_parsed.x[cat_i], $       ; When are you?
                MLT:eSpecs_parsed.mlt[cat_i], $
                ILAT:eSpecs_parsed.ilat[cat_i], $
               mono:eSpecs_parsed.mono[cat_i], $ ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
               broad:eSpecs_parsed.broad[cat_i], $     ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
               diffuse:eSpecs_parsed.diffuse[cat_i], $ ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
               Je:eSpecs_parsed.Je[cat_i], $                ;Electron number flux (#/cm^2-s)
               Jee:eSpecs_parsed.Jee[cat_i], $             ;Electron energy flux (mW/m^2)
               nBad_eSpec:eSpecs_parsed.nBad_eSpec[cat_i]}   ;0 = no problems      , N = N bad bins
  ENDIF ELSE BEGIN
     ;; events = { x:[events.x,tempEvent.x], $       ; When are you?
     ;;           mono:[events.mono,tempEvent.mono], $ ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
     ;;           broad:[events.broad,tempEvent.broad], $     ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
     ;;           diffuse:[events.diffuse,tempEvent.diffuse], $ ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
     ;;           Je:[events.Je,tempEvent.Je], $                ;Electron number flux (#/cm^2-s)
     ;;           Jee:[events.Jee,tempEvent.Jee], $             ;Electron energy flux (mW/m^2)
     ;;           time_i:[events.time_i,tempEvent.time_i], $    ;Ion time
     ;;           Ji:[events.Ji,tempEvent.Ji], $                ;Ion number flux      (#/cm^2-s)
     ;;           Jei:[events.Jei,tempEvent.Jei], $             ;Ion energy flux      (mW/m^2)
     ;;           nBad_iSpec:[events.nBad_eSpec,tempEvent.nBad_eSpec], $ ;0 = no problems      , N = N bad bins
     ;;           nBad_eSpec:[events.nBad_iSpec,tempEvent.nBad_iSpec]}   ;0 = no problems      , N = N bad bins

     events = { x:[events.x,eSpecs_parsed.x[cat_i]], $       ; When are you?
                MLT:[events.mlt,eSpecs_parsed.mlt[cat_i]], $
                ILAT:[events.ilat,eSpecs_parsed.ilat[cat_i]], $
               mono:[events.mono,eSpecs_parsed.mono[cat_i]], $ ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
               broad:[events.broad,eSpecs_parsed.broad[cat_i]], $     ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
               diffuse:[events.diffuse,eSpecs_parsed.diffuse[cat_i]], $ ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
               Je:[events.Je,eSpecs_parsed.Je[cat_i]], $                ;Electron number flux (#/cm^2-s)
               Jee:[events.Jee,eSpecs_parsed.Jee[cat_i]], $             ;Electron energy flux (mW/m^2)
               nBad_eSpec:[events.nBad_eSpec,eSpecs_parsed.nBad_eSpec[cat_i]]}   ;0 = no problems      , N = N bad bins
               ;; time_i:[events.time_i,eSpecs_parsed.time_i[cat_i]], $    ;Ion time
               ;; Ji:[events.Ji,eSpecs_parsed.Ji[cat_i]], $                ;Ion number flux      (#/cm^2-s)
               ;; Jei:[events.Jei,eSpecs_parsed.Jei[cat_i]], $             ;Ion energy flux      (mW/m^2)
               ;; nBad_iSpec:[events.nBad_iSpec,eSpecs_parsed.nBad_iSpec[cat_i]], $ ;0 = no problems      , N = N bad bins

  ENDELSE
  

END