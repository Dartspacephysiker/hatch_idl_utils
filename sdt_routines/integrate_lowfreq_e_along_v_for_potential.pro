;2016/05/17
PRO INTEGRATE_LOWFREQ_E_ALONG_V_FOR_POTENTIAL,eAlongV,DATA=data,TOTAL_METHOD=total_method

  GET_FA_ORBIT,eAlongV.x,/TIME_ARRAY,/ALL

  GET_DATA,'fa_vel',DATA=vel
  speed                   = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0

  old_pos                 = 0.
  position                = MAKE_ARRAY(N_ELEMENTS(eAlongV.x),/DOUBLE)
  speed_eAV_point         = MAKE_ARRAY(N_ELEMENTS(eAlongV.x),/DOUBLE)
  FOR j=0L,N_ELEMENTS(eAlongV.x)-2 DO BEGIN
     speed_point_ind      = MIN(ABS(vel.x-eAlongV.x[j]),ind)

     speed_eAV_point[j]   = speed[ind]
     samplingperiod       = eAlongV.x[j+1] - eAlongV.x[j]

     position[j]          = old_pos + speed_eAV_point[j]*samplingperiod
     old_pos              = position[j]
  ENDFOR
  ;; position[-1]            = position[-2]+samplingperiod*speed_eAV_point[-1]
  ;; speed[-1]               = speed[-2]

  IF KEYWORD_SET(total_method) THEN BEGIN
     ;; temp                 = position * 
  ENDIF ELSE BEGIN
     potential               = MAKE_ARRAY(N_ELEMENTS(eAlongV.x),/DOUBLE)
     potential[0]            = 0.0D
     FOR i=1,N_ELEMENTS(potential)-1 DO BEGIN
        ;; potential[i]         = TSUM(position[0:i],eAlongV.y[0:i])
        potential[i]         = INT_TABULATED(position[0:i],eAlongV.y[0:i])
     ENDFOR
  ENDELSE     
  STORE_DATA,'POTENTIAL',DATA={x:eAlongV.x,y:potential/1000.}

  IF ARG_PRESENT(data) THEN data = {x:eAlongV.x,y:potential/1000.}
END