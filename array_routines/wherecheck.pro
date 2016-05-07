;2015/10/14 A quick check on returns from where
PRO WHERECHECK,arg1,arg2,arg3,arg4,arg5

  IF N_ELEMENTS(arg1) EQ 0 THEN BEGIN
     PRINT,"WHERECHECK: Arg1 is undefined!"
     RETURN
  ENDIF ELSE BEGIN
     IF arg1[0] EQ -1 THEN BEGIN
        PRINT,"arg1[0] is -1!"
        WAIT,0.5
     ENDIF
  ENDELSE

  IF N_ELEMENTS(arg2) NE 0 THEN BEGIN
     IF arg2[0] EQ -1 THEN BEGIN
        PRINT,"arg2[0] is -1!"
        WAIT,0.5
     ENDIF
  ENDIF

  IF N_ELEMENTS(arg3) NE 0 THEN BEGIN
     IF arg3[0] EQ -1 THEN BEGIN
        PRINT,"arg3[0] is -1!"
        WAIT,0.5
     ENDIF
  ENDIF

  IF N_ELEMENTS(arg4) NE 0 THEN BEGIN
     IF arg4[0] EQ -1 THEN BEGIN
        PRINT,"arg4[0] is -1!"
        WAIT,0.5
     ENDIF
  ENDIF

  IF N_ELEMENTS(arg5) NE 0 THEN BEGIN
     IF arg5[0] EQ -1 THEN BEGIN
        PRINT,"arg5[0] is -1!"
        WAIT,0.5
     ENDIF
  ENDIF


END
