PRO CHECK_SORTED,ARRAY,is_sorted,SORT_IF_UNSORTED=sort_if_unsorted,SORTED_I=sorted_i

  IF N_ELEMENTS(ARRAY) EQ 0 THEN BEGIN
     PRINT,"CHECK_SORTED: Provided array is empty! Returning..."
     RETURN
  ENDIF

  sorted_i=sort(array)

  is_sorted=ARRAY_EQUAL(ARRAY[sorted_i],ARRAY)

  IF is_sorted THEN BEGIN
     PRINT,"This array is sorted!"
  ENDIF ELSE BEGIN
     PRINT,"Not sorted!"
     IF KEYWORD_SET(sort_if_unsorted) THEN BEGIN
        PRINT,'Sorting at your request...'
        ARRAY = ARRAY[sorted_i]
     ENDIF
  ENDELSE

END