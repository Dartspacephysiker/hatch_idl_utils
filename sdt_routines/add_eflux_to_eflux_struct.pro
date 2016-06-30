PRO ADD_EFLUX_TO_EFLUX_STRUCT,diff_eflux,tempDiff_eflux, $
                              ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle


  IF N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN

     diff_eFlux = tempDiff_eFlux

  ENDIF ELSE BEGIN

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

        diff_eFlux = {x:     [[diff_eFlux.x]   ,[tempDiff_eflux.x]   ], $
                      y:     [[diff_eFlux.y]   ,[tempDiff_eflux.y]   ], $
                      angles:[diff_eflux.angles,tempDiff_eflux.angles], $
                      time:  [diff_eFlux.time  ,tempDiff_eFlux.time  ]}
     ENDIF ELSE BEGIN
        diff_eFlux = {x:     [ [[diff_eFlux.x]]   ,[[tempDiff_eflux.x]]    ], $
                      y:     [ [[diff_eFlux.y]]   ,[[tempDiff_eflux.y]]    ], $
                      angles:[ [diff_eflux.angles],[tempDiff_eflux.angles] ], $
                      time:  [  diff_eFlux.time   ,tempDiff_eFlux.time     ]}

     ENDELSE
  ENDELSE


END