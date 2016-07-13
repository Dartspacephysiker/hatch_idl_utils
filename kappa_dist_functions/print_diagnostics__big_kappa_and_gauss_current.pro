;2016/07/12
PRO PRINT_DIAGNOSTICS__BIG_KAPPA_AND_GAUSS_CURRENT,refCurrent,pot, $
   kappa_current,kappaTime,Astruct, $
   maxwell_current,gaussTime,AstructGauss

  COMPILE_OPT idl2

  IF N_ELEMENTS(kappa_current) GT 0 AND N_ELEMENTS(kappaTime) GT 0 AND N_ELEMENTS(Astruct) GT 0 THEN BEGIN
     bigKCur_i    = WHERE(ABS(kappa_current) GE MAX(ABS(refCurrent),maxInd_jMag),nBigKCur)  
     nKappa       = N_ELEMENTS(kappa_current)
     IF nBigKCur NE 0 THEN BEGIN
        PRINT,""
        PRINT,"******************************"
        PRINT,"Kappa fit params"
        FOR k=0,nBigKCur-1 DO BEGIN
           tempI  = bigKCur_i[k]

           PRINT,''
           PRINT,FORMAT='("#",I0,"/",I0," (",I0,"/",I0,")")',k,nBigKCur,tempI,nKappa
           IF N_ELEMENTS(kappaTime)     GT 0 THEN PRINT,"Time   : ", TIME_TO_STR(kappaTime[tempI],/MS)
           IF N_ELEMENTS(pot)           GT 0 THEN PRINT,"Pot    : ", pot[tempI]
           IF N_ELEMENTS(kappa_current) GT 0 THEN PRINT,"Current: ",kappa_current[tempI]
           PRINT_KAPPA_FLUX_FIT_PARAMS,[Astruct.bulk_energy[tempI],Astruct.temp[tempI],Astruct.kappa[tempI],Astruct.N[tempI]]
        ENDFOR
     ENDIF
  ENDIF

  IF N_ELEMENTS(maxwell_current) GT 0 AND N_ELEMENTS(gaussTime) GT 0 AND N_ELEMENTS(AstructGauss) GT 0 THEN BEGIN
     bigGCur_i    = WHERE(ABS(maxwell_current) GE MAX(ABS(refCurrent),maxInd_jMag),nBigGCur)  
     nGauss       = N_ELEMENTS(maxwell_current)
     IF nBigGCur NE 0 THEN BEGIN
        

        PRINT,""
        PRINT,"******************************"
        PRINT,"Maxwell fit params"
        FOR k=0,nBigGCur-1 DO BEGIN
           tempI  = bigGCur_i[k]

           PRINT,''
           PRINT,FORMAT='("#",I0,"/",I0," (",I0,"/",I0,")")',k,nBigGCur,tempI,nGauss
           IF N_ELEMENTS(kappaTime)       GT 0 THEN PRINT,"Time   : ", TIME_TO_STR(gaussTime[tempI],/MS)
           IF N_ELEMENTS(pot)             GT 0 THEN PRINT,"Pot    : ", pot[tempI]
           IF N_ELEMENTS(maxwell_current) GT 0 THEN PRINT,"Current: ",maxwell_current[tempI]
           PRINT_KAPPA_FLUX_FIT_PARAMS,[AstructGauss.bulk_energy[tempI],AstructGauss.temp[tempI],AstructGauss.kappa[tempI],AstructGauss.N[tempI]]
        ENDFOR
     ENDIF
  ENDIF

END