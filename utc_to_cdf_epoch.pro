FUNCTION UTC_TO_CDF_EPOCH,utc

  COMPILE_OPT idl2

  t_to_1970 = 62167219200000.0000D

  cdf_epoch = utc*1000.0D + t_to_1970

  RETURN,cdf_epoch

END