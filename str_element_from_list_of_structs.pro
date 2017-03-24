PRO STR_ELEMENT_FROM_LIST_OF_STRUCTS,list,name, $
                                     VALUE=value, $
                                     INDEX=index, $
                                     ERROR=error, $
                                     PRESERVE_DIMENSIONALITY=preserve_dimensionality


  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(name) EQ 0 THEN BEGIN
     error = 'Name undefined'
     RETURN
  ENDIF

  IF SIZE(list,/TYPE) NE 11 THEN BEGIN
     error = 'No list provided'
     RETURN
  ENDIF

  IF SIZE(list[0],/TYPE) NE 8 THEN BEGIN
     error = 'Member NE struct'
     RETURN
  ENDIF

  ;;Give one a test
  STR_ELEMENT,list[0],name,VALUE=temp,INDEX=index

  IF index[0] LT 0 THEN BEGIN
     RETURN
  ENDIF

  nStr = N_ELEMENTS(list)

  retThing = !NULL
  CASE 1 OF
     KEYWORD_SET(preserve_dimensionality): BEGIN

        STR_ELEMENT,list[0],name,VALUE=temp,INDEX=index

        retThing = MAKE_ARRAY([nStr,SIZE(temp,/DIMENSIONS)],TYPE=SIZE(temp,/TYPE))

        FOR i=0,nStr-1 DO BEGIN
           STR_ELEMENT,list[i],name,VALUE=temp,INDEX=index
           retThing[i,*] = temp
        ENDFOR

     END
     ELSE: BEGIN

        FOR i=0,nStr-1 DO BEGIN
           STR_ELEMENT,list[i],name,VALUE=temp,INDEX=index
           retThing = [retThing,temp]
        ENDFOR

     END
  ENDCASE

  
  value = retThing
END