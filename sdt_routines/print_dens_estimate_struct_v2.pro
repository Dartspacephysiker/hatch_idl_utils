;2016/06/30 This one prints the energies and angles considered, every single time
PRO PRINT_DENS_ESTIMATE_STRUCT_V2,N_est, $
                                  ;; DENS_FILE_PREF=dens_
                                  TO_FILE=to_file, $
                                  OUTDIR=outDir

  COMPILE_OPT idl2

  IF KEYWORD_SET(to_file) THEN BEGIN
     IF N_ELEMENTS(outDir) EQ 0 THEN BEGIN
        outDir   = './'
     ENDIF

     PRINT,'Opening ' + to_file + ' ...'
     OPENW,fLun,outDir+to_file,/GET_LUN
  ENDIF ELSE BEGIN
     fLun        = -1
  ENDELSE

  en1Str         = 'Energy1 (ev)'
  en2Str         = 'Energy2 (ev)'

  an1Str         = 'Angle1 (deg)'
  an2Str         = 'Angle2 (deg)'

  densStr        = 'N (kappa)'
  densStr_Gauss  = 'N (Gauss)'
  densStr_SDT    = 'N (N_2D_FS)'
  dDeltStr       = 'N delta'

  eFormat        = 'G-10.4'     ;energy
  aFormat        = 'F-10.4'     ;angle
  dFormat        = 'F-10.5'     ;density


  space          = 20
  spaceStr       = 'T' + STRCOMPRESS((INDGEN(10)+1)*space,/REMOVE_ALL)

  ;;Header format
  pHFormat       = '(A0,'+spaceStr[0]+',A0,'+spaceStr[1]+ $
                   ',A0,'+spaceStr[2]+',A0,'+spaceStr[3]+ $
                   ',A0,'+spaceStr[4]+',A0,'+spaceStr[5]+ $
                   ',A0,'+spaceStr[6]+',A0)'

  ;;Line-entry format
  pFormat        = '('+ $
                   eFormat+','+spaceStr[0]+','+eFormat+','+spaceStr[1]+','+ $
                   aFormat+','+spaceStr[2]+','+aFormat+','+spaceStr[3]+','+ $
                   dFormat+','+spaceStr[4]+','+dFormat+','+spaceStr[5]+','+ $
                   dFormat+','+spaceStr[6]+','+dFormat+ ')'

  ;;Print the header
  header         = STRING(FORMAT=pHFormat, $
                          en1Str, $
                          en2Str, $
                          an1Str, $
                          an2Str, $
                          densStr, $
                          dDeltStr, $
                          densStr_Gauss, $
                          densStr_SDT)

  PRINTF,fLun,header

  ;;Print the estimates
  FOR iLoop=0,N_ELEMENTS(N_est.N)-2 DO BEGIN
     
     PRINTF,fLun,FORMAT=pFormat, $
            N_est.energies[0,iLoop], $
            N_est.energies[1,iLoop], $
            N_est.angles[0,iLoop], $
            N_est.angles[1,iLoop], $
            N_est.N[iLoop], $
            N_est.N_delta[iLoop], $
            N_est.N_Gauss, $
            N_est.N_SDT
  ENDFOR
  ;;Print the last one, where no delta exists
  PRINTF,fLun,FORMAT=pFormat, $
         N_est.energies[0,iLoop], $
         N_est.energies[1,iLoop], $
         N_est.angles[0,iLoop], $
         N_est.angles[1,iLoop], $
         N_est.N[iLoop], $
         0, $
         N_est.N_Gauss, $
         N_est.N_SDT

  IF KEYWORD_SET(to_file) THEN BEGIN
     PRINT,'Closing ' + to_file + ' ...'
     CLOSE,fLun
     FREE_LUN,fLun
  ENDIF

END