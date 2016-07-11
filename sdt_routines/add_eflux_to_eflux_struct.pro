PRO ADD_EFLUX_TO_EFLUX_STRUCT,diff_eflux,tempDiff_eflux, $
                              ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                              TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct


  IF N_ELEMENTS(diff_eFlux) EQ 0 THEN BEGIN

     diff_eFlux = tempDiff_eFlux

  ENDIF ELSE BEGIN

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN

        diff_eFlux = {x:     [[diff_eFlux.x]   ,[tempDiff_eflux.x]   ], $
                      y:     [[diff_eFlux.y]   ,[tempDiff_eflux.y]   ], $
                      angles:[diff_eflux.angles,tempDiff_eflux.angles], $
                      time:  [diff_eFlux.time  ,tempDiff_eFlux.time  ]}
     ENDIF ELSE BEGIN
        IF ~KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
           IF ~ARRAY_EQUAL(SIZE(diff_eflux.x[*,*,0],/DIM),SIZE(tempDiff_eflux.x,/DIM)) THEN BEGIN
              PRINT,"Unequal numbers of angles, and I don't know what it means."
              STOP
           ENDIF
           

           diff_eFlux = {x:     [ [[diff_eFlux.x]]   ,[[tempDiff_eflux.x]]    ], $
                         y:     [ [[diff_eFlux.y]]   ,[[tempDiff_eflux.y]]    ], $
                         complete_angles:[ [diff_eflux.complete_angles],[tempDiff_eflux.complete_angles] ], $
                         angles:[ [diff_eflux.angles],[tempDiff_eflux.angles] ], $
                         time:  [  diff_eFlux.time   ,tempDiff_eFlux.time     ]}
        ENDIF ELSE BEGIN
           IF ~ARRAY_EQUAL(SIZE(diff_eflux.energy[*,*,0],/DIM),SIZE(tempDiff_eflux.energy,/DIM)) THEN BEGIN
              PRINT,"Unequal numbers of angles, and I don't know what it means."
              STOP
           ENDIF 

           diff_eFlux = {data_name:[diff_eFlux.data_name,tempDiff_eFlux.data_name], $
                         valid:[diff_eFlux.valid, tempDiff_eFlux.valid], $
                         project_name:[diff_eFlux.project_name,tempDiff_eFlux.project_name], $
                         units_name:diff_eFlux.units_name, $
                         units_procedure:diff_eFlux.units_procedure, $
                         time:[diff_eFlux.time,tempDiff_eFlux.time], $
                         end_time:[diff_eFlux.end_time,tempDiff_eFlux.end_time], $
                         integ_t:[diff_eFlux.integ_t,tempDiff_eFlux.integ_t], $
                         nbins:[diff_eFlux.nbins,tempDiff_eFlux.nbins], $
                         nenergy:diff_eFlux.nenergy, $
                         data:[ [[diff_eFlux.data]]   , [[tempDiff_eFlux.data]] ], $
                         ddata:[ [[diff_eFlux.ddata]]   , [[tempDiff_eFlux.ddata]] ], $
                         energy:[ [[diff_eFlux.energy]]   , [[tempDiff_eFlux.energy]] ], $
                         complete_theta:[ [[diff_eFlux.complete_theta]], [[tempDiff_eflux.complete_theta]] ], $
                         theta:[ [diff_eFlux.theta], [tempDiff_eflux.theta] ], $
                         geom:[ [diff_eFlux.geom], [tempDiff_eFlux.geom] ], $
                         denergy:[ [[diff_eFlux.denergy]]   , [[tempDiff_eFlux.denergy]] ], $
                         ;; denergy:diff_eFlux.denergy[*,minAngleInd], $
                         dtheta:[ [diff_eFlux.dtheta],[tempDiff_eFlux.dtheta] ], $
                         eff:[ [diff_eFlux.eff],[tempDiff_eFlux.eff] ], $
                         mass:diff_eFlux.mass, $
                         geomfactor:[diff_eFlux.geomfactor,tempDiff_eFlux.geomfactor], $
                         header_bytes:[ [diff_eFlux.header_bytes], [tempDiff_eFlux.header_bytes] ], $
                         st_index:[diff_eFlux.st_index,tempDiff_eFlux.st_index], $
                         en_index:[diff_eFlux.en_index,tempDiff_eFlux.en_index], $
                         npts:[diff_eFlux.npts,tempDiff_eFlux.npts], $
                         index:[diff_eFlux.index,tempDiff_eFlux.index]}
        ENDELSE
     ENDELSE
  ENDELSE


END