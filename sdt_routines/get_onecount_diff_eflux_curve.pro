PRO GET_ONECOUNT_DIFF_EFLUX_CURVE,t1,t2,oneCurve, $
                             EEB_OR_EES=eeb_or_ees, $
                             ANGLE=e_angle, $
                             ENERGY=energy_electrons, $
                             SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                             SDT_NAME=name, $
                             OUT_ONEDAT=out_oneDat, $
                             QUIET=quiet

  COMPILE_OPT idl2

  IF ~KEYWORD_SET(name) THEN BEGIN
     name                   = 'diffEflux_oneCount'
  ENDIF

  func                      = STRING(FORMAT='("GET_FA_",A0)',STRUPCASE(eeb_or_ees))
  IF N_ELEMENTS(t2) GT 0 THEN BEGIN
     func += '_TS'
     oneDat                 = CALL_FUNCTION(func,t1,t2)
  ENDIF ELSE BEGIN
     oneDat                 = CALL_FUNCTION(func,t1)
  ENDELSE

  ;;Make sure--is it safe?
  IF ~oneDat[0].valid THEN BEGIN
     PRINT,'Invalid data from SDT!'
     oneDat                 = !NULL
     oneCurve               = !NULL
  ENDIF

  IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
     oneDat                 = AVERAGE_SUM3D(oneDat,spectra_average_interval)
  ENDIF

  nDat                      = N_ELEMENTS(oneDat)

  tempDatArr                = !NULL
  oneCurve                  = !NULL
  tempTime                  = !NULL
  tempEnergy                = !NULL
  FOR i=0,nDat-1 DO BEGIN
     tempDat                = oneDat[i]

     ;;Make sure units are OK
     CALL_PROCEDURE,tempDat.units_procedure,tempDat,'COUNTS'

     ;;Set count to one
     tempDat.data[*,*]      = 1

     ;;Get the one-count curve
     ;; tempCurve              = {x:tempDat.time, $
     ;;                           y:J_2D_FS_EN(tempDat,ANGLE=e_angle,ENERGY=energy_electrons), $
     ;;                           ytitle:'One-count curve'}
     tempCurve              = J_2D_FS_EN(tempDat,ANGLE=e_angle,ENERGY=energy_electrons)

     ;;store 'em
     tempTime               = [tempTime,tempDat.time]
     tempDatArr             = [tempDatArr,tempDat]
     oneCurve               = [[oneCurve],[tempCurve]]
     tempEnergy             = [[tempEnergy],[REFORM(tempDat.energy[*,0])]]
  ENDFOR

  oneCurve                  = {x:tempTime, $
                               y:TRANSPOSE(oneCurve), $
                               v:TRANSPOSE(tempEnergy), $
                               ytitle:'One-count curve'}


  STORE_DATA,name,DATA=oneCurve
  IF ~KEYWORD_SET(quiet) THEN PRINT,'Stored one-count nFlux curve as "' + name + '"'

  IF ARG_PRESENT(out_oneDat) THEN out_oneDat = tempDat

END