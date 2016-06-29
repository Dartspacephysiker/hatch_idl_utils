PRO GET_DIFF_EFLUX,T1=t1,T2=t2, $
                   EEB_OR_EES=eeb_or_ees, $
                   NAME__DIFF_EFLUX=name__diff_eFlux, $
                   ANGLE=angle, $
                   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(name__diff_eFlux) THEN name__diff_eFlux = 'diff_eFlux'

  routine        = 'get_fa_'+eeb_or_ees+'_ts'
  dat                = CALL_FUNCTION(routine,t1,t2,CALIB=calib)
  
  IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
     dat         = AVERAGE_SUM3D(dat,spectra_average_interval)
  ENDIF
  
  FOR i=0,N_ELEMENTS(dat)-1 DO BEGIN
     tempDat     = dat[i]

     spec2d,tempDat,UNITS='eflux',MSEC=msec,ANGLE=angle, $
            OUT_DAT=tempdiff_eFlux,/NO_PLOT

     REDUCE_EFLUX_TO_MIN_ANGLE,tempdiff_eFlux     

     ADD_EFLUX_TO_EFLUX_STRUCT,diff_eFlux,tempdiff_eFlux

  ENDFOR
  
  diff_eFlux    =  {x:      TRANSPOSE(diff_eFlux.x), $
                    y:      TRANSPOSE(diff_eFlux.y), $
                    angles: diff_eflux.angles, $
                    time:   diff_eFlux.time}

  STORE_DATA,name__diff_eFlux,DATA=diff_eFlux


END