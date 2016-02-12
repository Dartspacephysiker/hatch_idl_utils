;2016/02/10
PRO MAKE_DIR_IF_NOT_EXIST,directory, $
                          ADD_TODAY=add_today, $
                          ADD_SUFF=add_suff, $
                          VERBOSE=verbose, $
                          MAKE_PARENTS=make_parents, $
                          LUN=lun
  
  IF N_ELEMENTS(lun) EQ 0 THEN lun         = -1

  IF N_ELEMENTS(directory) EQ 0 THEN BEGIN
     PRINTF,lun,'Must provide directory!'
     STOP
  ENDIF

  IF N_ELEMENTS(verbose) EQ 0 THEN verbose = 1

  IF KEYWORD_SET(make_parents) THEN BEGIN
     makeStr = 'mkdir -p '
  ENDIF ELSE BEGIN
     makeStr = 'mkdir '
  ENDELSE

  IF KEYWORD_SET(add_today) AND KEYWORD_SET(add_suff) THEN BEGIN
     directory                             = directory + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--' + add_suff + '/'
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(add_today) THEN BEGIN
        directory                          = directory + GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '/'
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(add_suff) THEN BEGIN
           directory                       = directory + add_suff + '/'
        ENDIF
     ENDELSE
  ENDELSE 

  ;;check if this dir exists
  IF KEYWORD_SET(add_today) OR KEYWORD_SET(add_suff) THEN BEGIN
     directory_exists                      = FILE_TEST(directory,/DIRECTORY)
     IF ~directory_exists THEN BEGIN
        IF KEYWORD_SET(verbose) THEN PRINTF,lun,"MAKE_DIR_IF_NOT_EXIST: Making directory " + directory
        SPAWN,makeStr + directory
     ENDIF
     directory_exists                      = FILE_TEST(directory,/DIRECTORY)
     IF ~directory_exists THEN BEGIN
        PRINTF,lun,'Failed to make directory: ' + directory
        STOP
     ENDIF
  ENDIF ELSE BEGIN
     directory_exists                      = FILE_TEST(directory,/DIRECTORY)
     IF ~directory_exists THEN BEGIN
        IF KEYWORD_SET(verbose) THEN PRINTF,lun,"MAKE_DIR_IF_NOT_EXIST: Making directory " + directory
        SPAWN,makeStr + directory
     ENDIF
     directory_exists                      = FILE_TEST(directory,/DIRECTORY)
     IF ~directory_exists THEN BEGIN
        PRINTF,lun,'Failed to make directory: ' + directory
        STOP
     ENDIF
  ENDELSE 

END