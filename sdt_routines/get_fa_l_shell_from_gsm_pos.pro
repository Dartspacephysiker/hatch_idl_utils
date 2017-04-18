;2017/04/17
;;NOTE!! This assume GEOPACK_RECALC has already been run and has been provided SW data!
FUNCTION GET_FA_L_SHELL_FROM_GSM_POS,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z,parMod, $
                                     trace_to_equator, $
                                     T89=T89, $
                                     T96=T96, $
                                     T01=T01, $
                                     TS04=TS04, $
                                     IGRF=IGRF, $
                                     TILT=tilt, $ ;should be in degrees
                                     EPOCH=epoch, $
                                     DSMAX=dsMax, $
                                     ERR=traceErr


  COMPILE_OPT IDL2,STRICTARRSUBS

  GEOPACK_TRACE_08,FAST_GSM_x,FAST_GSM_y,FAST_GSM_z, $
                   trace_to_equator,parMod, $
                   Eq_GSM_x,Eq_GSM_y,Eq_GSM_z, $
                   /REFINE, $
                   /EQUATOR, $
                   R0=R0, $
                   RLIM=RLimFull, $
                   FLINE=fLine_toEq, $
                   T89=T89, $
                   T96=T96, $
                   T01=T01, $
                   TS04=TS04, $
                   IGRF=IGRF, $
                   TILT=tilt, $ ;should be in degrees
                   EPOCH=epoch, $
                   DSMAX=dsMax, $
                   ERR=traceErr


  RETURN,SQRT(TOTAL(([EQ_GSM_x,EQ_GSM_y,EQ_GSM_z])^2.D))

END
