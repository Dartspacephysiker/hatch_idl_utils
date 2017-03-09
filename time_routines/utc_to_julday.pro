FUNCTION UTC_TO_JULDAY,UTC_Arr, $
                       VERBOSE=verbose

  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE SIZE(UTC_Arr,/TYPE) OF
     0: BEGIN
        PRINT,"UTC_TO_JULDAY: Why did you lie to me? There's nothing here."
        RETURN,-1
     END
     5: BEGIN
        IF KEYWORD_SET(verbose) THEN PRINT,"Converting UTC times to Julian dates..."
        tempTimes        = UTC_Arr
     END
     7: BEGIN
        IF KEYWORD_SET(verbose) THEN PRINT,"Converting UTC strings to Julian dates..."
        tempTimes        = STR_TO_TIME(UTC_Arr)
        IF tempTimes[0]  EQ -1 THEN BEGIN
           PRINT,"Failed to convert UTC strings to doubles! Quitting..."
           RETURN,-1
        ENDIF
     END
     ELSE: BEGIN
        PRINT,"UTC_TO_JULDAY: What did you pass? I don't know what to do with this..."
        RETURN,-1
     END
  ENDCASE

  t_to_1970              = 62167219200000.0000D
  juldayArr              = CDF_EPOCH_TOJULDAYS(tempTimes*1000.0D + t_to_1970)

  RETURN,juldayArr
END