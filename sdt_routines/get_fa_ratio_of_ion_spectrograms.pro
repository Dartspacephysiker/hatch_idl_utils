;2018/07/23
PRO GET_FA_RATIO_OF_ION_SPECTROGRAMS, $
   T1=t1, $
   T2=t2, $
   DOWNVARNAME=downVarName, $
   UPVARNAME=upVarName, $
   ALLANGLEVARNAME=allAngleVarName, $
   UP_ARANGE=up_aRange, $
   DOWN_ARANGE=down_aRange, $
   UNITS=units, $
   GET_EN_SPEC_PRO=get_en_spec_pro, $
   CALIB=calib, $
   MAKEZEROTHRESHEFLUX=makeZeroThreshEFlux, $
   MAKEZEROVAL=makeZeroVal, $
   OUT_DOWNESPEC=eSpecDown, $
   OUT_UPESPEC=eSpecUp, $
   OUT_ALLANGLEESPEC=eSpec, $
   OUT_UPDOWNRATIOSPEC=upDownRatioSpec, $
   OUT_UPALLRATIOSPEC=upAllRatioSpec

  COMPILE_OPT IDL2,STRICTARRSUBS

  uniter = KEYWORD_SET(units) ? units : 'eflux'

     varName = upVarName
     GET_EN_SPEC,get_en_spec_pro, $
                 T1=t1, $
                 T2=t2, $
                 UNITS=uniter, $
                 NAME=varName,ANGLE=up_aRange,RETRACE=1,CALIB=calib
     GET_DATA,varName,DATA=eSpecUp

     varName = downVarName
     GET_EN_SPEC,get_en_spec_pro, $
                 T1=t1, $
                 T2=t2, $
                 units=uniter, $
                 NAMe=varName,angle=down_aRange,RETRACE=1,CALIB=calib
     GET_DATA,varName,DATA=eSpecDown

     varName = allAngleVarName
     GET_EN_SPEC,get_en_spec_pro, $
                 T1=t1, $
                 T2=t2, $
                 UNITS=uniter, $
                 NAMe=varName,retrace=1,CALIB=calib
     GET_DATA,varName,DATA=eSpec

     upAllRatioSpec = {x : eSpec.x, $
                        y : eSpecUp.y / eSpec.y, $
                        v : eSpec.v}

     upDownRatioSpec = {x : eSpec.x, $
                       y : eSpecUp.y / eSpecDown.y, $
                       v : eSpec.v}

  makeZero = WHERE(eSpecUp.y LT makeZeroThreshEFlux,/NULL)
  ;; upAllRatioSpec.y[makeZero] = !VALUES.F_NaN
  upAllRatioSpec.y[makeZero] = makeZeroVal
  upDownRatioSpec.y[makeZero] = makeZeroVal



END
