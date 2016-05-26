PRO CHECK_UNIQUE,ARRAY,is_unique, $
                 MAKE_UNIQUE_IF_NOT_UNIQUE=make_unique, $
                 UNIQ_I=uniq_i, $
                 QUIET=quiet

  IF N_ELEMENTS(ARRAY) EQ 0 THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,"CHECK_SORTED: Provided array is empty! Returning..."
     RETURN
  ENDIF

  uniq_i = UNIQ(array,SORT(array))

  is_unique = ARRAY_EQUAL(ARRAY[uniq_i],ARRAY)

  IF is_unique THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,"UNIQUE"
  ENDIF ELSE BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,"NOT UNIQUE"
     IF KEYWORD_SET(make_unique) THEN BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,'Making unique at your request...'
        ARRAY = ARRAY[uniq_i]
     ENDIF
  ENDELSE

END