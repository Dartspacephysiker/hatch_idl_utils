PRO STR_ELEMENT_FROM_LIST_OF_STRUCTS,list,name, $
                                          VALUE=value, $
                                          INDEX=index, $
                                          ERROR=error

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

  IF index LT 0 THEN BEGIN
     RETURN
ENDIF

  nStr = N_ELEMENTS(list)

  retThing = !NULL
  FOR i=0,N_ELEMENTS(list)-1 DO BEGIN
     STR_ELEMENT,list[i],name,VALUE=temp,INDEX=index
     retThing = [retThing,temp]
  ENDFOR
  
  value = retThing
END