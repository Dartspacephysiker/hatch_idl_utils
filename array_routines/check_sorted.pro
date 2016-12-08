PRO CHECK_SORTED,ARRAY,is_sorted, $
                 SORT_IF_UNSORTED=sort_if_unsorted, $
                 SORTED_I=sorted_i, $
                 QUIET=quiet

  IF N_ELEMENTS(ARRAY) EQ 0 THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,"CHECK_SORTED: Provided array is empty! Returning..."
     is_sorted = -9
     RETURN
  ENDIF

  sorted_i  = SORT(array)

  is_sorted = ARRAY_EQUAL(ARRAY[sorted_i],ARRAY)

  IF is_sorted THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,"This array is sorted!"
  ENDIF ELSE BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,"Not sorted!"
     IF KEYWORD_SET(sort_if_unsorted) THEN BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,'Sorting at your request...'
        ARRAY = ARRAY[sorted_i]
     ENDIF
  ENDELSE

END