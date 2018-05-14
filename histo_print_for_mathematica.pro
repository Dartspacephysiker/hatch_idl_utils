;2018/05/11
PRO HISTO_PRINT_FOR_MATHEMATICA, $
   names,histList,binList,dataList, $
   NORMALIZE_HIST=normalize_hist, $
   ALSO_PRINT_DATA=also_print_data, $
   WRITE_TO_FILE=write_to_file, $
   FILEPREFS=filePrefs, $
   FILESUFF=fileSuff, $
   WRITEDIR=writeDir, $
   BINSIZE=kHBinSize

  COMPILE_OPT IDL2,STRICTARRSUBS
  

  IF KEYWORD_SET(write_to_file) THEN BEGIN

     wDir   = N_ELEMENTS(writeDir) GT 0 ? writeDir : './'
     fPrefs = N_ELEMENTS(filePrefs) GT 0 ? filePrefs : names
     fSuff  = N_ELEMENTS(fileSuff) GT 0 ? fileSuff : ''

     todayStr = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  ENDIF ELSE BEGIN

     lun = -1

  ENDELSE

  FOR jj=0,N_ELEMENTS(histList)-1 DO BEGIN

     name = names[jj]
     hist = histList[jj]
     bins = binList[jj]+kHBinSize/2.

     IF KEYWORD_SET(normalize_hist) THEN BEGIN

        normFac = INT_TABULATED(bins,FLOAT(hist))
        hist    = FLOAT(hist)/normFac

     ENDIF

     IF KEYWORD_SET(write_to_file) THEN BEGIN

        fName = todayStr + '-' + fPrefs[jj]+'histo'+fSuff+'.csv'
        PRINT,"Writing to " + fName + ' ...'

        OPENW,lun,wDir+fName,/GET_LUN

        FOR k=0,N_ELEMENTS(hist)-1 DO BEGIN
           PRINTF,lun,FORMAT='(F0.4,",",F0.5)', $
                  bins[k], $
                  hist[k]
        ENDFOR

        CLOSE,lun
        FREE_LUN,lun

     ENDIF ELSE BEGIN

        PRINTF,lun,FORMAT='(A0," = {")',name

        FOR k=0,N_ELEMENTS(hist)-1 DO BEGIN
           PRINTF,lun,FORMAT='("{",F0.4,",",F0.5,"}",A0)', $
                  bins[k], $
                  hist[k], $
                  (k EQ (N_ELEMENTS(hist)-1) ? '' : ',')
        ENDFOR

        PRINTF,lun,'};'


     ENDELSE

  ENDFOR

  IF KEYWORD_SET(also_print_data) THEN BEGIN
     FOR jj=0,N_ELEMENTS(dataList)-1 DO BEGIN
        name = names[jj]
        data = dataList[jj]

        IF KEYWORD_SET(write_to_file) THEN BEGIN

           fName = todayStr + '-' + fPrefs[jj]+'data'+fSuff+'.csv'
           PRINT,"Writing to " + fName + ' ...'
           OPENW,lun,wDir+fName,/GET_LUN

           FOR k=0,N_ELEMENTS(data)-1 DO PRINTF,lun,FORMAT='(F0.5)',data[k]

           CLOSE,lun
           FREE_LUN,lun

        ENDIF ELSE BEGIN


           PRINTF,lun,FORMAT='(A0," = {")',name
           PRINTF,lun,FORMAT='(100000(F0.5,:,","))',data
           PRINTF,lun,'};'

        ENDELSE

     ENDFOR
  ENDIF

END
