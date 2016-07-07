PRO ADD_OMNIDAT_TO_OMNI2D_STRUCT,omniDat,tempDat

  COMPILE_OPT idl2

  IF N_ELEMENTS(omniDat) EQ 0 THEN BEGIN

     omniDat = tempDat

  ENDIF ELSE BEGIN

     omniDat = [omniDat,tempDat]

     ;; IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

        ;; omniDat = {x:     [[omniDat.x]   ,[tempDat.x]   ], $
        ;;               y:     [[omniDat.y]   ,[tempDat.y]   ], $
        ;;               angles:[omniDat.angles,tempDat.angles], $
        ;;               time:  [omniDat.time  ,tempDat.time  ]}
     ;; ENDIF ELSE BEGIN
        ;; omniDat = {x:     [ [[omniDat.x]]   ,[[tempDat.x]]    ], $
        ;;               y:     [ [[omniDat.y]]   ,[[tempDat.y]]    ], $
        ;;               angles:[ [omniDat.angles],[tempDat.angles] ], $
        ;;               time:  [  omniDat.time   ,tempDat.time     ]}

     ;; ENDELSE
  ENDELSE


END