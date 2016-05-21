PRO WRITE_MESSAGE_TO_LOGFILE,logFile,string,APPEND=append

  IF N_PARAMS() LT 2 THEN RETURN

  OPENW,logLun,logFile,/GET_LUN,APPEND=append

  PRINTF,logLun,string

  CLOSE,logLun
  FREE_LUN,logLun


END