;;10/10/16
FUNCTION VARS_EXIST,saveFile,varNames,defVarnames, $
                    P0,P1,P2,P3,P4,P5,P6,P7,P8,P9, $
                    P10,P11,P12,P13,P14,P15,P16,P17,P18,P19, $
                    P20,P21,P22,P23,P24

  COMPILE_OPT IDL2

  IF FILE_TEST(saveFile) THEN BEGIN
     RESTORE,saveFile
  ENDIF ELSE BEGIN
     PRINT,"File doesn't exist: " + saveFile
     PRINT,"Can't check for existence of vars."
     RETURN,0
  ENDELSE

  fail = 0
  FOR k=0,N_ELEMENTS(varNames)-1 DO BEGIN
     
     test = EXECUTE('junk = SIZE(' + varNames[k] + ',/TYPE)')

     IF ~test THEN BEGIN
        fail = 1
        failNum = k
        BREAK
     ENDIF

     IF junk EQ 0 THEN BEGIN
        test = EXECUTE('junk = SIZE(' + defVarNames[k] + ',/TYPE)')        

        IF ~test THEN BEGIN
           fail = 2
           failNum = k
           BREAK
        ENDIF

        IF junk EQ 0 THEN BEGIN
           fail = 3
           failNum = k
        ENDIF

     ENDIF

  ENDFOR

  IF fail NE 0 THEN BEGIN
     CASE fail OF
        1: BEGIN
           PRINT,"Failed execution of 'junk = SIZE(...' looking for " + varNames[k]
           RETURN,0
        END
        2: BEGIN
           PRINT,"Failed execution of 'junk = SIZE(...' looking for " + defVarNames[k]
           RETURN,0           
        END
        3: BEGIN
           PRINT,FORMAT='(A0,A0,A0,A0,A0)',"Variables ",varNames[k], $
                 " and ", $
                 defVarnames[k], $
                 " dont' exist in this file"
           RETURN,0           
        END
     ENDCASE

     RETURN,0
  ENDIF

  IF ARG_PRESENT(P0) THEN BEGIN
     this = EXECUTE('P0 = ' + varNames[0])
  ENDIF

  IF ARG_PRESENT(P1) THEN BEGIN
     this = EXECUTE('P1 = ' + varNames[1])
  ENDIF

  IF ARG_PRESENT(P2) THEN BEGIN
     this = EXECUTE('P2 = ' + varNames[2])
  ENDIF

  IF ARG_PRESENT(P3) THEN BEGIN
     this = EXECUTE('P3 = ' + varNames[3])
  ENDIF

  IF ARG_PRESENT(P4) THEN BEGIN
     this = EXECUTE('P4 = ' + varNames[4])
  ENDIF

  IF ARG_PRESENT(P5) THEN BEGIN
     this = EXECUTE('P5 = ' + varNames[5])
  ENDIF

  IF ARG_PRESENT(P6) THEN BEGIN
     this = EXECUTE('P6 = ' + varNames[6])
  ENDIF

  IF ARG_PRESENT(P7) THEN BEGIN
     this = EXECUTE('P7 = ' + varNames[7])
  ENDIF

  IF ARG_PRESENT(P8) THEN BEGIN
     this = EXECUTE('P8 = ' + varNames[8])
  ENDIF

  IF ARG_PRESENT(P9) THEN BEGIN
     this = EXECUTE('P9 = ' + varNames[9])
  ENDIF

  IF ARG_PRESENT(P10) THEN BEGIN
     this = EXECUTE('P10 = ' + varNames[10])
  ENDIF

  IF ARG_PRESENT(P11) THEN BEGIN
     this = EXECUTE('P11 = ' + varNames[11])
  ENDIF

  IF ARG_PRESENT(P12) THEN BEGIN
     this = EXECUTE('P12 = ' + varNames[12])
  ENDIF

  IF ARG_PRESENT(P13) THEN BEGIN
     this = EXECUTE('P13 = ' + varNames[13])
  ENDIF

  IF ARG_PRESENT(P14) THEN BEGIN
     this = EXECUTE('P14 = ' + varNames[14])
  ENDIF

  IF ARG_PRESENT(P15) THEN BEGIN
     this = EXECUTE('P15 = ' + varNames[15])
  ENDIF

  IF ARG_PRESENT(P16) THEN BEGIN
     this = EXECUTE('P16 = ' + varNames[16])
  ENDIF

  IF ARG_PRESENT(P17) THEN BEGIN
     this = EXECUTE('P17 = ' + varNames[17])
  ENDIF

  IF ARG_PRESENT(P18) THEN BEGIN
     this = EXECUTE('P18 = ' + varNames[18])
  ENDIF

  IF ARG_PRESENT(P19) THEN BEGIN
     this = EXECUTE('P19 = ' + varNames[19])
  ENDIF

  IF ARG_PRESENT(P20) THEN BEGIN
     this = EXECUTE('P20 = ' + varNames[20])
  ENDIF

  IF ARG_PRESENT(P21) THEN BEGIN
     this = EXECUTE('P21 = ' + varNames[21])
  ENDIF

  IF ARG_PRESENT(P22) THEN BEGIN
     this = EXECUTE('P22 = ' + varNames[22])
  ENDIF

  IF ARG_PRESENT(P23) THEN BEGIN
     this = EXECUTE('P23 = ' + varNames[23])
  ENDIF

  IF ARG_PRESENT(P24) THEN BEGIN
     this = EXECUTE('P24 = ' + varNames[24])
  ENDIF

  RETURN,test AND (junk GT 0)

END
