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
   OUT_UPALLRATIOSPEC=upAllRatioSpec, $
   OUT_ESPECUP_TIME=out_time, $
   OUT_BAD_TIME=bad_time, $
   USE_DIFF_EFLUX=use_diff_eFlux, $
   DIFF_EFLUX=diff_eFlux, $
   IS_MCFADDEN_DIFF_EFLUX=is_McFadden_diff_eFlux

  COMPILE_OPT IDL2,STRICTARRSUBS

  uniter = KEYWORD_SET(units) ? units : 'eflux'

  IF KEYWORD_SET(use_diff_eflux) THEN BEGIN

     varName = upVarName
     eSpecUp = GET_EN_SPEC__FROM_DIFF_EFLUX( $
               diff_eFlux, $
               T1=t1, $
               T2=t2, $
               /RETRACE, $
               ANGLE=up_aRange, $
               UNITS=eSpecUnits, $
               NAME=varName, $
               OUT_AVGFACTORARR=avgFactorArr, $
               OUT_NORMARR=normArr, $
               BAD_TIME=bad_time, $
               OUT_TIME=out_time, $
               IS_MCFADDEN_DIFF_EFLUX=is_McFadden_diff_eFlux)
     STORE_DATA,varName,DATA=eSpecUp

     varName = downVarName
     eSpecDown = GET_EN_SPEC__FROM_DIFF_EFLUX( $
                 diff_eFlux, $
                 T1=t1, $
                 T2=t2, $
                 /RETRACE, $
                 ANGLE=down_aRange, $
                 UNITS=eSpecUnits, $
                 NAME=varName, $
                 OUT_AVGFACTORARR=avgFactorArr, $
                 OUT_NORMARR=normArr, $
                 IS_MCFADDEN_DIFF_EFLUX=is_McFadden_diff_eFlux)
     STORE_DATA,varName,DATA=eSpecDown

     varName = allAngleVarName
     eSpec = GET_EN_SPEC__FROM_DIFF_EFLUX( $
             diff_eFlux, $
             T1=t1, $
             T2=t2, $
             /RETRACE, $
             UNITS=eSpecUnits, $
             NAME=varName, $
             OUT_AVGFACTORARR=avgFactorArr, $
             OUT_NORMARR=normArr, $
             IS_MCFADDEN_DIFF_EFLUX=is_McFadden_diff_eFlux)
     STORE_DATA,varName,DATA=eSpec
     
  ENDIF ELSE BEGIN

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

  ENDELSE

  upAllRatioSpec = {x : eSpecUp.x, $
                    y : eSpecUp.y / eSpec.y, $
                    v : eSpecUp.v}

  upDownRatioSpec = {x : eSpecUp.x, $
                     y : eSpecUp.y / eSpecDown.y, $
                     v : eSpecUp.v}

  makeZero = WHERE(eSpecUp.y LT makeZeroThreshEFlux,/NULL)
  ;; upAllRatioSpec.y[makeZero] = !VALUES.F_NaN
  upAllRatioSpec.y[makeZero] = makeZeroVal
  upDownRatioSpec.y[makeZero] = makeZeroVal

END
