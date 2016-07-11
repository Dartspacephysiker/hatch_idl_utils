;2016/07/11
FUNCTION GET_CURRENT_FROM_FLUXMAG,t1,t2, $
                             magStr,velocityStr, $
                             USE_DESPUN=use_despun, $
                             SDTNAME__JMAG=jMagName, $
                             INFERRED_E_NUMFLUX=inferred_e_numFlux, $
                             SDTNAME__INFERRED_E_NUMFLUX=e_numFluxName, $
                             QUIET=quiet
  
  COMPILE_OPT idl2

  IF KEYWORD_SET(t1) THEN BEGIN
     CASE SIZE(t1,/TYPE) OF
        5: time1 = t1
        7: time1 = STR_TO_TIME(t1)
        ELSE: BEGIN
           IF ~KEYWORD_SET(quiet) THEN BEGIN
              PRINT,"Bad t1 provided! Needs to be a string or a double."
           ENDIF
           RETURN,-1
        END
     ENDCASE
  ENDIF 

  IF KEYWORD_SET(t2) THEN BEGIN
     CASE SIZE(t2,/TYPE) OF
        5: time2 = t2
        7: time2 = STR_TO_TIME(t2)
        ELSE: BEGIN
           IF ~KEYWORD_SET(quiet) THEN BEGIN
              PRINT,"Bad t2 provided! Needs to be a string or a double."
           ENDIF
           RETURN,-1
        END
     ENDCASE
  ENDIF

  IF N_ELEMENTS(jMagName) EQ 0 THEN BEGIN
     jMagName            = 'jMag'
  ENDIF

  IF SIZE(magStr,/TYPE) NE 8 THEN BEGIN
     IF KEYWORD_SET(use_despun) THEN BEGIN
        UCLA_MAG_DESPIN

        GET_DATA,'dB_fac_v',DATA=db_fac
        IF N_ELEMENTS(time1) NE 0 AND N_ELEMENTS(time2) NE 0 THEN BEGIN
           mintime          = MIN(ABS(time1-db_fac.x),ind1)
           mintime          = MIN(ABS(time2-db_fac.x),ind2)
           
           magx             = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,0]}
           magy             = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,2]}
           magz             = {x:db_fac.x[ind1:ind2],y:db_fac.y[ind1:ind2,1]}
        ENDIF ELSE BEGIN
           magx             = {x:db_fac.x,y:db_fac.y[*,0]}
           magy             = {x:db_fac.x,y:db_fac.y[*,2]}
           magz             = {x:db_fac.x,y:db_fac.y[*,1]}
        ENDELSE
     ENDIF ELSE BEGIN
        GET_DATA,'MagDCcomp1',DATA=magx
        GET_DATA,'MagDCcomp2',DATA=magy
        GET_DATA,'MagDCcomp3',DATA=magz
     ENDELSE

  ENDIF ELSE BEGIN

     IF N_ELEMENTS(time1) NE 0 AND N_ELEMENTS(time2) NE 0 THEN BEGIN
        mintime          = MIN(ABS(time1-magStr.x),ind1)
        mintime          = MIN(ABS(time2-magStr.x),ind2)
        
        magx             = {x:magStr.x[ind1:ind2],y:magStr.y[ind1:ind2,0]}
        magy             = {x:magStr.x[ind1:ind2],y:magStr.y[ind1:ind2,2]}
        magz             = {x:magStr.x[ind1:ind2],y:magStr.y[ind1:ind2,1]}
     ENDIF ELSE BEGIN
        magx             = {x:magStr.x,y:magStr.y[*,0]}
        magy             = {x:magStr.x,y:magStr.y[*,2]}
        magz             = {x:magStr.x,y:magStr.y[*,1]}
     ENDELSE
  ENDELSE

  ;;Get speed and position for calculation of mag stuff
  IF SIZE(velocityStr,/TYPE) NE 8 THEN BEGIN
     GET_FA_ORBIT,magz.x,/TIME_ARRAY ;,/all

     GET_DATA,'fa_vel',DATA=vel
  ENDIF ELSE BEGIN
     vel                 = velocityStr
  ENDELSE

  speed                  = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0

  old_pos                = 0.
  position               = MAKE_ARRAY(N_ELEMENTS(magz.x),/DOUBLE)
  speed_mag_point        = MAKE_ARRAY(N_ELEMENTS(magz.x),/DOUBLE)
  FOR j=0L,N_ELEMENTS(magz.x)-2 DO BEGIN
     speed_point_ind     = MIN(ABS(vel.x-magz.x[j]),ind)

     speed_mag_point[j]  = speed[ind]
     samplingperiod      = magz.x[j+1] - magz.x[j]

     position[j]         = old_pos + speed_mag_point[j]*samplingperiod
     old_pos             = position[j]
  ENDFOR


  ;;Calculate the current from mag
  deltaBX                = DERIV(position,SMOOTH(magz.y,100))
  jMag                   = {x:magz.x, $
                            y:1.0e-3*(deltaBx)/1.26e-6, $
                            units:CGGREEK('mu')+'A/m!U2!N'}
  ;; jtemp               = 1.0e-3*(deltaBx)/1.26e-6

  IF ~KEYWORD_SET(quiet) THEN PRINT,'Storing magnetometer-derived current as ' + jMagName + ' ...'
  STORE_DATA,jMagName,DATA={x:magz.x,y:jMag}

  sign_jtemp             = ABS(deltaBx)/deltaBx

  ;;In number flux units
  IF ARG_PRESENT(inferred_e_numFlux) THEN BEGIN
     IF N_ELEMENTS(e_numFluxName) EQ 0 THEN BEGIN
        e_numFluxName    = 'mag_inferred_eNumFlux'
     ENDIF
     inferred_e_numFlux  = {x:magz.x, $
                            y:1.0e-3*(deltaBx)/1.26e-6  * (DOUBLE(1. / 1.6e-9)), $
                            units:"cm!U-2!Ns!U-1!N"}

     IF ~KEYWORD_SET(quiet) THEN PRINT,'Storing magnetometer-inferred e- number flux as ' + e_numFluxName + ' ...'
     STORE_DATA,e_numFluxName,DATA=inferred_e_numFlux
  ENDIF
  
  RETURN,jMag

END