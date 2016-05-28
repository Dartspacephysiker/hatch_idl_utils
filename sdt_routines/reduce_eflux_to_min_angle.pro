PRO REDUCE_EFLUX_TO_MIN_ANGLE,diff_eFlux


     minAngle   = MIN(ABS(diff_eFlux.angles),minAngleInd) 
     diff_eFlux = {x:diff_eFlux.x[*,minAngleInd], $
                   y:diff_eFlux.y[*,minAngleInd], $
                   angles:diff_eFlux.angles[minAngleInd], $
                   time:diff_eFlux.time}


END