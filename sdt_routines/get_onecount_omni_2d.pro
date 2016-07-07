PRO GET_ONECOUNT_OMNI_2D,t1,t2, $
                         EEB_OR_EES=eeb_or_ees, $
                         SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                         SDT_NAME=name, $
                         ANGLE=angle, $
                         ;; ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                         OUT_ONEDAT=out_oneDat, $
                         QUIET=quiet

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(name) THEN BEGIN
     name               = 'oneCount_OMNI_2d'
  ENDIF

  routine               = 'get_fa_'+eeb_or_ees+'_ts'
  dat                   = CALL_FUNCTION(routine,t1,t2,CALIB=calib)
  
  IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
     dat                = AVERAGE_SUM3D(dat,spectra_average_interval)
  ENDIF
  
  FOR i=0,N_ELEMENTS(dat)-1 DO BEGIN
     tempDat            = dat[i]

     ;;Make sure units are OK
     CALL_PROCEDURE,tempDat.units_procedure,tempDat,'COUNTS'

     ;;Set count to one
     tempDat.data[*,*]  = 1

     ;;Convert to eFlux
     CALL_PROCEDURE,tempDat.units_procedure,tempDat,'eFlux'

     ;;swap angles
     IF angle[0] GT angle[1] THEN BEGIN
        swap_i = WHERE(tempdat.theta GT 180)
        IF swap_i[0] NE -1 THEN BEGIN
           tempdat.theta[swap_i] -= 360
        ENDIF
     ENDIF

     ;;Now reduce
     tempDat            = OMNI2D(tempDat,ANGLE=angle)

     ADD_OMNIDAT_TO_OMNI2D_STRUCT,omniDat_oneCount,tempDat

  ENDFOR
  
  STORE_DATA,name,DATA=omniDat_oneCount

END