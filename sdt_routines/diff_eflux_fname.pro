;2018/07/26
PRO DIFF_EFLUX_FNAME, $
   T1=t1, $
   T2=t2, $
   ORBIT=orbit, $
   EEB_OR_EES=eeb_or_ees, $
   BONUSPREF=bonusPref ,$
   SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file ,$
   SAVE_DIFF_EFLUX_FILE=save_diff_eFlux_file ,$
   LOAD_DIFF_EFLUX_FILE=load_diff_eFlux_file ,$
   MCFADDEN_DIFF_EFLUX=McFadden_diff_eFlux, $
   OUT_DIFF_EFLUX_FILE=diff_eFlux_file, $
   ENFORCE_DIFF_EFLUX_SRATE=enforce_diff_eFlux_sRate, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   LOADDIR=loadDir

  COMPILE_OPT IDL2,STRICTARRSUBS

  plotNamePref    = KEYWORD_SET(bonusPref) ? bonusPref : ''
  
  avgItvlStr       = ''
  CASE 1 OF
     KEYWORD_SET(enforce_diff_eFlux_sRate): BEGIN
        avgItvlStr    = '-sRate' + (STRING(FORMAT='(F0.2)',enforce_diff_eFlux_sRate)).Replace('.','_')
        plotNamePref += avgItvlStr
     END
     KEYWORD_SET(spectra_average_interval): BEGIN
        avgItvlStr    = '-avg_itvl' + STRING(FORMAT='(I0)',spectra_average_interval)
        plotNamePref += avgItvlStr
     END
  ENDCASE

  ;; tString files
  t1Str = 'unspecified'
  t2Str = ''
  
  IF N_ELEMENTS(t1) GT 0 THEN BEGIN
     CASE SIZE(t1,/TYPE) OF
        5: BEGIN
           t1Str = T2S(t1,/MS)
        END
        7: BEGIN
           t1Str = t1
        END
        ELSE: BEGIN
           STOP
        END
     ENDCASE

     CASE SIZE(t2,/TYPE) OF
        5: BEGIN
           t2Str = T2S(t2,/MS)
        END
        7: BEGIN
           t2Str = t2
        END
        ELSE: BEGIN
           STOP
        END
     ENDCASE

     t1Str = '-' + (STRSPLIT(t1Str,'/',/EXTRACT))[1]
     t1Str = STRJOIN(STRSPLIT(t1Str,':',/EXTRACT),'_')
     t1Str = STRJOIN(STRSPLIT(t1Str,'.',/EXTRACT),'__')

     t2Str = '-' + (STRSPLIT(t2Str,'/',/EXTRACT))[1]
     t2Str = STRJOIN(STRSPLIT(t2Str,':',/EXTRACT),'_')
     t2Str = STRJOIN(STRSPLIT(t2Str,'.',/EXTRACT),'__')

  ENDIF
  
  McFaddenString = KEYWORD_SET(McFadden_diff_eFlux) ? '-McFaddenStyle' : ''

  defEFluxFile = 'orb_' + STRCOMPRESS(orbit,/REMOVE_ALL) + '-diff_eflux' + McFaddenString $
                 + '-' + eeb_or_ees + avgItvlStr + t1Str + t2Str + '.sav'

  IF KEYWORD_SET(save_diff_eFlux_file) THEN BEGIN
     CASE SIZE(save_diff_eFlux_file,/TYPE) OF
        7: BEGIN
           IF ~(FILE_TEST(save_diff_eFlux_file) OR FILE_TEST(loadDir + save_diff_eFlux_file)) THEN BEGIN
              PRINT,"Couldn't find " + save_diff_eFlux_file + '!'
              STOP
           ENDIF
        END
        ELSE: BEGIN
           save_diff_eFlux_to_file = defEFluxFile
        END
     ENDCASE
  ENDIF

  ;; IF KEYWORD_SET(load_diff_eFlux_file) THEN BEGIN
  CASE SIZE(load_diff_eFlux_file,/TYPE) OF
     7: BEGIN
        IF ~(FILE_TEST(load_diff_eFlux_file) OR FILE_TEST(loadDir+load_diff_eFlux_file)) THEN BEGIN
           PRINT,"Couldn't find file: " + load_diff_eFlux_file
           STOP
        ENDIF ELSE BEGIN
           diff_eFlux_file = load_diff_eFlux_file
        ENDELSE
     END
     ELSE: BEGIN
        diff_eFlux_file    = defEFluxFile
     END
  ENDCASE

END
