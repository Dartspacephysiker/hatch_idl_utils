;;2016/07/??
;;This routine averages the angle range of the diff_energyFlux struct and simply totals the eFlux in each bin without averaging.
;;The scary thing, I suppose, is that it treats all energy fluxes as field-aligned.
PRO REDUCE_DIFF_EFLUX,diff_eFlux, $
                      TRY_SYNTHETIC_SDT_STRUCT=try_synthetic_SDT_struct

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF SIZE(diff_eFlux,/TYPE) NE 8 THEN BEGIN
     PRINT,'diff_eFlux must be a struct!'
     STOP
  ENDIF
  
  ;;New struct elements--they'll be modified
  IF ~KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN

     xNew              = diff_eFlux.x
     yNew              = diff_eFlux.y

     aNew              = diff_eFlux.angles
  ENDIF ELSE BEGIN
     xNew              = diff_eFlux.energy
     yNew              = diff_eFlux.data

     aNew              = diff_eFlux.theta
  ENDELSE

  shiftMe           = WHERE(aNew GT 180.)
  IF shiftMe[0] NE -1 THEN BEGIN
     aNew[shiftMe]  = aNew[shiftMe] - 360.
  ENDIF

  tNew              = diff_eFlux.time

  sNew              = diff_eFlux.shiftVals
  sNew[*]           = 0

  CASE SIZE(xNew,/N_DIMENSIONS) OF
     ;; 0: BEGIN
     ;; PRINT,"No diff_eFlux struct provided!!"
     ;; STOP
     ;; END
     1: BEGIN
        PRINT,'Huh?'
        STOP
     END
     2: BEGIN
        PRINT,'Huh?'
        STOP
     END
     3: BEGIN
        nAngles     = N_ELEMENTS(aNew[0,*])
        xNew        = MEAN(xNew,DIMENSION=2) ;Sum over angles

        yNew        = TOTAL(yNew,2) 
        IF KEYWORD_SET(average_diff_eFlux_over_angles) THEN BEGIN
           yNew = yNew/nAngles
        ENDIF

        aNew        = TOTAL(aNew,2)/FLOAT(nAngles)

        IF ~KEYWORD_SET(try_synthetic_SDT_struct) THEN BEGIN
           diff_eFlux  = {x:xNew, $
                          y:yNew, $
                          angles:aNew, $
                          time:tNew, $
                          shiftVals:sNew}
        ENDIF ELSE BEGIN
           diff_eFlux = {data_name:diff_eFlux.data_name, $
                         valid:diff_eFlux.valid, $
                         project_name:diff_eFlux.project_name, $
                         units_name:diff_eFlux.units_name, $
                         units_procedure:diff_eFlux.units_procedure, $
                         time:diff_eFlux.time, $
                         end_time:diff_eFlux.end_time, $
                         integ_t:diff_eFlux.integ_t, $
                         nbins:diff_eFlux.nbins, $
                         nenergy:diff_eFlux.nenergy, $
                         ;; data:diff_eFlux.data, $
                         data:yNew, $
                         ddata:diff_eFlux.ddata, $
                         ;; energy:diff_eFlux.energy, $
                         energy:xNew, $
                         denergy: diff_eFlux.denergy, $
                         ;; theta:diff_eFlux.theta, $
                         theta:aNew, $
                         dtheta: diff_eFlux.dtheta, $
                         geom: diff_eFlux.geom, $
                         eff: diff_eFlux.eff, $
                         mass:diff_eFlux.mass, $
                         geomfactor:diff_eFlux.geomfactor, $
                         header_bytes: diff_eFlux.header_bytes, $
                         st_index:diff_eFlux.st_index, $
                         en_index:diff_eFlux.en_index, $
                         npts:diff_eFlux.npts, $
                         index:diff_eFlux.index, $
                         shiftVals:sNew}
        ENDELSE
     END
  ENDCASE

END