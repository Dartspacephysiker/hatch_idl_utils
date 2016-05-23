PRO ADD_EVENT_TO_SPECTRAL_STRUCT,events,tempEvent

  ;; event = { time_e:[events.time_e,tempEvent.time_e], $ ; When are you?

  IF N_ELEMENTS(events) EQ 0 THEN BEGIN
     ;; event = { x:time_e, $              ; When are you?
     ;;           mono:BYTE(mono), $       ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
     ;;           broad:BYTE(broad), $     ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
     ;;           diffuse:BYTE(diffuse), $ ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
     ;;           Je:Je, $                 ;Electron number flux (#/cm^2-s)
     ;;           Jee:Jee, $               ;Electron energy flux (mW/m^2)
     ;;           time_i:time_i, $         ;Ion time
     ;;           Ji:Ji, $                 ;Ion number flux      (#/cm^2-s)
     ;;           Jei:Jei, $               ;Ion energy flux      (mW/m^2)
     ;;           nBad_iSpec:nBad_e, $     ;0 = no problems      , N = N bad bins
     ;;           nBad_eSpec:nBad_i}       ;0 = no problems      , N = N bad bins
     events = tempEvent
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

     events = { x:[events.x,tempEvent.x], $       ; When are you?
                MLT:[events.mlt,tempEvent.mlt], $
                ILAT:[events.ilat,tempEvent.ilat], $
               mono:[events.mono,tempEvent.mono], $ ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
               broad:[events.broad,tempEvent.broad], $     ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
               diffuse:[events.diffuse,tempEvent.diffuse], $ ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
               Je:[events.Je,tempEvent.Je], $                ;Electron number flux (#/cm^2-s)
               Jee:[events.Jee,tempEvent.Jee], $             ;Electron energy flux (mW/m^2)
               nBad_eSpec:[events.nBad_eSpec,tempEvent.nBad_eSpec]}   ;0 = no problems      , N = N bad bins
               ;; time_i:[events.time_i,tempEvent.time_i], $    ;Ion time
               ;; Ji:[events.Ji,tempEvent.Ji], $                ;Ion number flux      (#/cm^2-s)
               ;; Jei:[events.Jei,tempEvent.Jei], $             ;Ion energy flux      (mW/m^2)
               ;; nBad_iSpec:[events.nBad_iSpec,tempEvent.nBad_iSpec], $ ;0 = no problems      , N = N bad bins

  ENDELSE
  

END