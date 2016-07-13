;2016/07/12
PRO PRINT_KAPPA_FITS,pot, $
                     kappa_current, $
                     fit_struct, $
                     ;; maxwell_current,gaussTime,AstructGauss, $
                     OUTFILE=filename, $
                     OUTDIR=outDir

  COMPILE_OPT idl2

  PARSE_KAPPA_FIT_STRUCTS,fit_struct, $
                          A=A, $
                          STRUCT_A=AStruct, $
                          TIME=kappaTime, $
                          NAMES_A=A_names, $
                          CHI2=chi2, $
                          PVAL=pVal, $
                          FITSTATUS=fitStatus  


  IF N_ELEMENTS(filename) GT 0 THEN BEGIN
     IF N_ELEMENTS(outDir) EQ 0 THEN BEGIN
        outDir = ''
     ENDIF
     OPENW,lun,outDir+filename,/GET_LUN
  ENDIF ELSE BEGIN
     lun = -1
  ENDELSE

  success = fitStatus EQ 0

  IF N_ELEMENTS(kappa_current) GT 0 AND N_ELEMENTS(kappaTime) GT 0 AND N_ELEMENTS(Astruct) GT 0 THEN BEGIN
     nKappa       = N_ELEMENTS(kappa_current)
     ;; PRINTF,lun,"******************************"
     ;; PRINTF,lun,"Fit params"
     ;; PRINTF,lun,"******************************"
     ;; PRINTF,lun,''
     PRINTF,lun,FORMAT='("Fit",T7,"Success",T16,"Time",T42,"Potential",T54,' + $
            '"Current   ",T66,"Peak energy",T79,"Temperature",T92,"Kappa",T102,"Density",A0)',''
     PRINTF,lun,FORMAT='("   ",T7,"       ",T16,"    ",T42,"   eV    ",T54,' + $
            '"microA/m^2",T66,"     eV    ",T79,"    eV     ",T92,"     ",T102," cm^-3 ",A0)',''

     FOR k=0,nKappa-1 DO BEGIN
        tempI  = k

        PRINTF,lun,FORMAT='(I0,T7,       I0,T16,   A23,T42,       G8.3,T54,' + $
               '     G8.3,T66,         G9.3,T79,         G9.3,T92,   F7.3,T102,     G9.3)', $
               k, $
               success[k], $
               TIME_TO_STR(kappaTime[k],/MS), $
               pot[k], $
               kappa_current[k], $
               Astruct.bulk_energy[k], $
               Astruct.temp[k], $
               Astruct.kappa[k], $
               Astruct.N[k]
     ENDFOR
  ENDIF

  IF N_ELEMENTS(filename) GT 0 THEN BEGIN
     CLOSE,lun
     FREE_LUN,lun
  ENDIF

END