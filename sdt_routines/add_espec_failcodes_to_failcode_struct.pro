PRO ADD_ESPEC_FAILCODES_TO_FAILCODE_STRUCT,failCodes,tempFailCodes

  IF N_ELEMENTS(failCodes) EQ 0 THEN BEGIN
     failCodes = tempFailCodes
  ENDIF ELSE BEGIN
     failCodes = { mono:[failCodes.mono,tempFailCodes.mono], $ 
                   mono_nAbove:[failCodes.mono_nAbove,tempFailCodes.mono_nAbove], $ 
                   mono_nBelow:[failCodes.mono_nBelow,tempFailCodes.mono_nBelow], $ 
                   broad:[failCodes.broad,tempFailCodes.broad], $
                   nBroad:[failCodes.nBroad,tempFailCodes.nBroad], $
                   N_broad_GE_min_eV:[failCodes.N_broad_GE_min_eV,tempFailCodes.N_broad_GE_min_eV], $
                   peakFlux:[failCodes.peakFlux,tempFailCodes.peakFlux], $
                   peakEnergy:[failCodes.peakEnergy,tempFailCodes.peakEnergy]}
  ENDELSE
  

END