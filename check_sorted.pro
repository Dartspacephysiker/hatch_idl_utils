PRO CHECK_SORTED,ARRAY,is_sorted

  IF N_ELEMENTS(ARRAY) EQ 0 THEN BEGIN
     PRINT,"CHECK_SORTED: Provided array is empty! Returning..."
     RETURN
  ENDIF

  si=sort(array)

  is_sorted=ARRAY_EQUAL(ARRAY(si),ARRAY)

  IF is_sorted THEN PRINT,"This array is sorted!" ELSE PRINT,"Not sorted!"

END