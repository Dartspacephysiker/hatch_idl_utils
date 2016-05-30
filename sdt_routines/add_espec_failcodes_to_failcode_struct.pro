PRO ADD_ESPEC_FAILCODES_TO_FAILCODE_STRUCT,failCodes,tempFailCodes

  IF N_ELEMENTS(failCodes) EQ 0 THEN BEGIN
     failCodes = tempFailCodes
  ENDIF ELSE BEGIN
     failCodes = { mono:[events.mono,tempFailCodes.mono], $ 
                   mono_nAbove:[events.mono_nAbove,tempFailCodes.mono_nAbove], $ 
                   mono_nBelow:[events.mono_nBelow,tempFailCodes.mono_nBelow], $ 
                   broad:[events.broad,tempFailCodes.broad], $
                   nBroad:[events.nBroad,tempFailCodes.nBroad], $
                   N_broad_GE_min_eV:[events.N_broad_GE_min_eV,tempFailCodes.N_broad_GE_min_eV], $
                   peakFlux:[events.peakFlux,tempFailCodes.peakFlux], $
                   peakEnergy:[events.peakEnergy,tempFailCodes.peakEnergy]}
  ENDELSE
  

END