;2016/07/11
FUNCTION REDUCE_SYNTH_DIFF_EFLUX_STRUCT,synthStr,ind, $
                                        NO_SHIFTVALS=no_shiftVals

  COMPILE_OPT IDL2,STRICTARRSUBS


  IF KEYWORD_SET(no_shiftVals) THEN BEGIN

     red_synthStr = {data_name:synthStr.data_name[ind], $
                     valid:synthStr.valid[ind], $
                     project_name:synthStr.project_name[ind], $
                     units_name:synthStr.units_name, $
                     units_procedure:synthStr.units_procedure, $
                     time:synthStr.time[ind], $
                     end_time:synthStr.end_time[ind], $
                     integ_t:synthStr.integ_t[ind], $
                     nbins:synthStr.nbins[ind], $
                     nenergy:synthStr.nenergy, $
                     data:REFORM(synthStr.data[*,*,ind]), $
                     ddata:REFORM(synthStr.ddata[*,*,ind]), $
                     energy:REFORM(synthStr.energy[*,*,ind]), $
                     denergy:REFORM(synthStr.denergy[*,*,ind]), $
                     ;; complete_theta:synthStr.theta[*,i], $
                     theta:synthStr.theta[*,*,ind], $
                     dtheta:synthStr.dtheta[*,ind], $
                     geom:synthStr.geom[*,ind], $
                     eff:synthStr.eff[*,ind], $
                     mass:synthStr.mass, $
                     geomfactor:synthStr.geomfactor[ind], $
                     header_bytes:synthStr.header_bytes[*,ind], $
                     st_index:synthStr.st_index[ind], $
                     en_index:synthStr.en_index[ind], $
                     npts:synthStr.npts[ind], $
                     index:synthStr.index[ind]}

  ENDIF ELSE BEGIN

     red_synthStr = {data_name:synthStr.data_name[ind], $
                     valid:synthStr.valid[ind], $
                     project_name:synthStr.project_name[ind], $
                     units_name:synthStr.units_name, $
                     units_procedure:synthStr.units_procedure, $
                     time:synthStr.time[ind], $
                     end_time:synthStr.end_time[ind], $
                     integ_t:synthStr.integ_t[ind], $
                     nbins:synthStr.nbins[ind], $
                     nenergy:synthStr.nenergy, $
                     data:REFORM(synthStr.data[ind,*]), $
                     ddata:REFORM(synthStr.ddata[ind,*,*]), $
                     energy:REFORM(synthStr.energy[ind,*]), $
                     denergy:REFORM(synthStr.denergy[ind,*,*]), $
                     ;; complete_theta:synthStr.theta[*,i], $
                     theta:synthStr.theta[ind], $
                     dtheta:synthStr.dtheta[ind,*], $
                     geom:synthStr.geom[ind,*], $
                     eff:synthStr.eff[ind,*], $
                     mass:synthStr.mass, $
                     geomfactor:synthStr.geomfactor[ind], $
                     header_bytes:synthStr.header_bytes[*,ind], $
                     st_index:synthStr.st_index[ind], $
                     en_index:synthStr.en_index[ind], $
                     npts:synthStr.npts[ind], $
                     index:synthStr.index[ind]}

     IF ~KEYWORD_SET(no_shiftVals) THEN BEGIN
        STR_ELEMENT,red_synthStr,'shiftVals',synthStr.shiftVals[ind*2:ind*2+1],/ADD_REPLACE
     ENDIF

  ENDELSE

  RETURN,red_synthStr
END