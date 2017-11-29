;2017/11/29
FUNCTION T2S, secin, FMT=fmt, MSEC = msec, dateonly=dateonly, $
                      CONV_TO_UNDERSCORE=conv_to_underscore

  COMPILE_OPT IDL2,STRICTARRSUBS

  RETURN,TIME_TO_STR(secin, FMT=fmt, MSEC = msec, dateonly=dateonly, $
                      CONV_TO_UNDERSCORE=conv_to_underscore)

END
