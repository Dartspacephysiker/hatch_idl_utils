;2015/10/27
FUNCTION LIST_TO_1DARRAY,list, $
                         SKIP_NEG1_ELEMENTS=skip_neg1, $
                         SKIP_NANS=skip_nans, $
                         SORT=sort, $
                         WARN=warn, $
                         PRESERVE_DIMENSIONALITY=preserve_dimensionality, $
                         LUN=lun

  IF N_ELEMENTS(lun) EQ 0 THEN lun = -1

  ;;Get the first one that isn't yunk
  iFirst = 0
  WHILE N_ELEMENTS(array) EQ 0 DO BEGIN

     IF KEYWORD_SET(warn) THEN BEGIN
        IF N_ELEMENTS(list[iFirst]) EQ 0 THEN BEGIN
           PRINTF,lun,FORMAT='("Warning! list element ",I0," is null!")',iFirst
        ENDIF ELSE BEGIN
           IF list[iFirst,0] EQ -1 THEN PRINTF,lun,FORMAT='("Warning! list element [",I0,",",I0,"] is -1!")',iFirst,0
           IF ~FINITE(list[iFirst,0]) THEN PRINTF,lun,FORMAT='("Warning! list element [",I0,",",I0,"] is not finite!")',iFirst,0
        ENDELSE
     ENDIF

     IF N_ELEMENTS(list[iFirst]) GT 0 THEN BEGIN

        IF KEYWORD_SET(preserve_dimensionality) THEN BEGIN
           dims = [SIZE(list[iFirst],/DIMENSIONS),1]
        ENDIF

        IF KEYWORD_SET(skip_neg1) THEN BEGIN
           IF list[iFirst,0] NE -1 THEN qualified = 1 ELSE qualified = 0
        ENDIF ELSE BEGIN
           qualified = 1
        ENDELSE

        IF KEYWORD_SET(skip_nans) THEN BEGIN
           IF FINITE(list[iFirst,0])  THEN qualified = 1 ELSE qualified = 0
        ENDIF ELSE BEGIN
           qualified = 1
        ENDELSE

        ;;qualified?
        IF qualified THEN BEGIN
           IF KEYWORD_SET(skip_nans) THEN BEGIN
              safe_i = WHERE(FINITE(list[iFirst]))
              IF safe_i[0] NE -1 THEN BEGIN
                 array  = list[iFirst,safe_i]
              ENDIF
           ENDIF ELSE BEGIN
              array  = REFORM(list[iFirst],N_ELEMENTS(list[iFirst]))
           ENDELSE
        ENDIF

     ENDIF

     iFirst++

  ENDWHILE

  ;;Get the rest that aren't yunk
  FOR i = iFirst,N_ELEMENTS(list) -1 DO BEGIN

     IF N_ELEMENTS(list[i]) GT 0 THEN BEGIN

        IF KEYWORD_SET(skip_neg1) THEN BEGIN
           IF list[i,0] NE -1 THEN qualified = 1 ELSE qualified = 0
        ENDIF ELSE BEGIN
           qualified = 1
        ENDELSE

        IF KEYWORD_SET(skip_nans) THEN BEGIN
           IF FINITE(list[i,0])  THEN qualified = 1 ELSE qualified = 0
        ENDIF ELSE BEGIN
           qualified = 1
        ENDELSE

        ;;qualified?
        IF qualified THEN BEGIN

           IF KEYWORD_SET(skip_nans) THEN BEGIN
              safe_i = WHERE(FINITE(list[i]))
              IF safe_i[0] NE -1 THEN BEGIN
                 array  = [array,list[i,safe_i]]
              ENDIF
           ENDIF ELSE BEGIN
              array  = [array,REFORM(list[i],N_ELEMENTS(list[i]))]
           ENDELSE

           IF KEYWORD_SET(preserve_dimensionality) THEN BEGIN
              dims[-1]++
           ENDIF

        ENDIF

     ENDIF

     IF KEYWORD_SET(warn) THEN BEGIN
        IF N_ELEMENTS(list[i]) EQ 0 THEN BEGIN
           PRINTF,lun,FORMAT='("Warning! list element ",I0," is null!")',i
        ENDIF ELSE BEGIN
           IF list[i,0] EQ -1 THEN PRINTF,lun,FORMAT='("Warning! list element [",I0,",",I0"] is -1!")',i,0
        ENDELSE
     ENDIF

  ENDFOR

  IF KEYWORD_SET(preserve_dimensionality) THEN BEGIN
     array = REFORM(array,dims)
  ENDIF

  IF KEYWORD_SET(sort) THEN BEGIN
     IF KEYWORD_SET(preserve_dimensionality) THEN BEGIN
        PRINT,"WRONG! You've got to pay attention when the ball's coming your way!"
        STOP
     ENDIF
     array = array[SORT(array)]
  ENDIF

  RETURN,array

END