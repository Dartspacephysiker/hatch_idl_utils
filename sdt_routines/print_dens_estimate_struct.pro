PRO PRINT_DENS_ESTIMATE_STRUCT,N_est, $
                               ;; DENS_FILE_PREF=dens_
                               TO_FILE=to_file, $
                               OUTDIR=outDir

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF KEYWORD_SET(to_file) THEN BEGIN
     IF N_ELEMENTS(outDir) EQ 0 THEN BEGIN
        outDir = './'
     ENDIF

     PRINT,'Opening ' + to_file + ' ...'
     OPENW,fLun,outDir+to_file,/GET_LUN
  ENDIF ELSE BEGIN
     fLun = -1
  ENDELSE

  densStr = 'N est (cm^-2-s^-1)'
  dDeltStr = 'N delta'

  spaceStr   = 'T' + STRCOMPRESS((INDGEN(10)+1)*20,/REMOVE_ALL)

  pHFormat1 = '(A0,'+spaceStr[1]+',A0,'+spaceStr[2]+',A0)'
  pFormat1  = '(G-10.4,'+spaceStr[1]+',F-10.4,'+spaceStr[2]+',F-10.5)'

  pHFormat2 = '(A0,'+spaceStr[0]+',A0,'+spaceStr[1]+',A0,'+spaceStr[2]+',A0)'
  pFormat2  = '(G-10.4,'+spaceStr[0]+',G-10.4,'+spaceStr[1]+',F-10.4,'+spaceStr[2]+',F-10.5)'

  CASE N_est.var_dim OF
     1: BEGIN
        header = STRING(FORMAT=pHFormat1, $
                        N_est.loopType, $
                        densStr, $
                       dDeltStr)
        PRINTF,fLun,header


        FOR iLoop=0,N_ELEMENTS(N_est.N)-2 DO BEGIN
           
           PRINTF,fLun,FORMAT=pFormat1,N_est.vars[iLoop], $
                  N_est.N[iLoop],N_est.N_delta[iLoop]
        ENDFOR
        ;;Print the last one, where no delta exists
        PRINTF,fLun,FORMAT=pFormat1,N_est.vars[iLoop], $
                  N_est.N[iLoop],0
     END
     2: BEGIN
        header = STRING(FORMAT=pHFormat2, $
                        N_est.loopType, $
                        'angle 2', $
                        densStr, $
                       dDeltStr)
        PRINTF,fLun,header

        FOR iLoop=0,N_ELEMENTS(N_est.N)-2 DO BEGIN
           PRINTF,fLun,FORMAT=pFormat2,N_est.vars[iLoop,0],N_est.vars[iLoop,1], $
                  N_est.N[iLoop],N_est.N_delta[iLoop]
        ENDFOR
        ;;Print the last one, where no delta exists
        PRINTF,fLun,FORMAT=pFormat2,N_est.vars[iLoop,0],N_est.vars[iLoop,1], $
               N_est.N[iLoop],0
     END
  ENDCASE


  IF KEYWORD_SET(to_file) THEN BEGIN
     PRINT,'Closing ' + to_file + ' ...'
     CLOSE,fLun
     FREE_LUN,fLun
  ENDIF

END