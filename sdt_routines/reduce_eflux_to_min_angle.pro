PRO REDUCE_EFLUX_TO_MIN_ANGLE,diff_eFlux, $
                              TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
     minAngle   = MIN(ABS(diff_eFlux.angles),minAngleInd) 
     diff_eFlux = {x:diff_eFlux.x[*,minAngleInd], $
                   y:diff_eFlux.y[*,minAngleInd], $
                   angles:diff_eFlux.angles[minAngleInd], $
                   time:diff_eFlux.time}
  ENDIF ELSE BEGIN
     minAngle   = MIN(ABS(diff_eFlux.theta),minAngleInd) 
     diff_eFlux = {data_name:diff_eFlux.data_name, $
                   valid:diff_eFlux.valid, $
                   project_name:diff_eFlux.project_name, $
                   units_name:diff_eFlux.units_name, $
                   units_procedure:diff_eFlux.units_procedure, $
                   time:diff_eFlux.time, $
                   end_time:diff_eFlux.end_time, $
                   integ_t:diff_eFlux.integ_t, $
                   nbins:1, $
                   nenergy:diff_eFlux.nenergy, $
                   data:diff_eFlux.data[*,minAngleInd], $
                   ddata:(conv_units( ( ( conv_units(diff_eFlux,'counts') ).data)^.5,'eflux' ) )[*,minAngleInd], $
                   energy:xdat, $
                   theta:diff_eFlux.theta[*,minAngleInd], $
                   geom:diff_eFlux.geom, $
                   denergy:diff_eFlux.denergy[*,minAngleInd], $
                   dtheta:diff_eFlux.dtheta, $
                   eff:diff_eFlux.eff, $
                   mass:diff_eFlux.mass, $
                   geomfactor:diff_eFlux.geomfactor, $
                   header_bytes:diff_eFlux.header_bytes, $
                   st_index:diff_eFlux.st_index, $
                   en_index:diff_eFlux.en_index, $
                   npts:diff_eFlux.npts, $
                   index:diff_eFlux.index}

  ENDELSE
END