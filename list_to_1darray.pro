;2015/10/27
FUNCTION LIST_TO_1DARRAY,list, $
                         SKIP_NEG1_ELEMENTS=skip, $
                         WARN=warn, $
                         LUN=lun

  IF N_ELEMENTS(lun) EQ 0 THEN lun = -1

  iFirst = 0
  WHILE N_ELEMENTS(array) EQ 0 DO BEGIN
     IF KEYWORD_SET(warn) THEN BEGIN
        IF N_ELEMENTS(list[iFirst] EQ 0) THEN BEGIN
           PRINTF,lun,FORMAT='("Warning! list element ",I0," is null!")',iFirst
        ENDIF ELSE BEGIN
           IF list[iFirst,0] EQ -1 THEN PRINTF,lun,FORMAT='("Warning! list element [",I0,",",I0"] is -1!")',iFirst,0
        ENDELSE
     ENDIF

     IF N_ELEMENTS(list[iFirst]) GT 0 THEN BEGIN
        IF KEYWORD_SET(skip) THEN BEGIN
           IF list[iFirst,0] NE -1 THEN array = list[iFirst]
        ENDIF ELSE BEGIN
           array = list[iFirst]
        ENDELSE
     ENDIF
     iFirst++
  ENDWHILE

  FOR i = iFirst,N_ELEMENTS(list) -1 DO BEGIN
     array = [array,list[i]]
     IF KEYWORD_SET(warn) THEN BEGIN
        IF N_ELEMENTS(list[i] EQ 0) THEN BEGIN
           PRINTF,lun,FORMAT='("Warning! list element ",I0," is null!")',i
        ENDIF ELSE BEGIN
           IF list[i,0] EQ -1 THEN PRINTF,lun,FORMAT='("Warning! list element [",I0,",",I0"] is -1!")',i,0
        ENDELSE
     ENDIF
  ENDFOR

  RETURN,array

END