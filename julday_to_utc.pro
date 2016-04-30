FUNCTION JULDAY_TO_UTC,juldayArr

  COMPILE_OPT idl2

  CASE SIZE(juldayArr,/TYPE) OF
     0: BEGIN
        PRINT,"JULDAY_TO_UTC: Why did you lie to me? There's nothing here."
        RETURN,-1
     END
     2: BEGIN
        PRINT,"Converting Julian dates to UTC in seconds..."
     END
     3: BEGIN
        PRINT,"Converting Julian dates to UTC in seconds..."
     END
     4: BEGIN
        PRINT,"Converting Julian dates to UTC in seconds..."
     END
     5: BEGIN
        PRINT,"Converting Julian dates to UTC in seconds..."
     END
     12: BEGIN
        PRINT,"Converting Julian dates to UTC in seconds..."
     END
     13: BEGIN
        PRINT,"Converting Julian dates to UTC in seconds..."
     END
     14: BEGIN
        PRINT,"Converting Julian dates to UTC in seconds..."
     END
     15: BEGIN
        PRINT,"Converting Julian dates to UTC in seconds..."
     END
     ;; 7: BEGIN
     ;;    PRINT,"Converting strings of Julian dates to UTC in seconds..."
     ;;    CALDAT,juldayArr,mArr,dArr,yArr,hArr,mArr,sArr
     ;; END
     ELSE: BEGIN
        PRINT,"JULDAY_TO_UTC: What did you pass? I don't know what to do with this..."
        RETURN,-1
     END
  ENDCASE

  utcArr                 = DOUBLE((juldayArr - JULDAY(1,1,1970,0,0,0)) * 24. * 60 * 60)

  RETURN,utcArr
END