;;2017/03/06
FUNCTION EXTRACT_MEMBER_FROM_LIST_OF_STRUCTS,list,member, $
   INDEX_INSTANCES_AT_REAR=transpose

  COMPILE_OPT IDL2,STRICTARRSUBS

  yeahRight = ~(ISA(list) AND KEYWORD_SET(member))
  IF yeahRight THEN BEGIN
     PRINT,"Yeah right"
     RETURN,-1
  ENDIF ELSE BEGIN
     ohYeah = (SIZE(list[0],/TYPE) NE 8)

     IF ohYeah THEN BEGIN
        PRINT,"No one believes you"
        RETURN,-1
     ENDIF

  ENDELSE
     

  this = WHERE(STRMATCH(TAG_NAMES(list[0]),member,/FOLD_CASE),nThis)

  CASE nThis OF
     0: BEGIN
        PRINT,"Couldn't find " + member + " among struct members ..."
        RETURN,-1
     END
     1: BEGIN

        nHere = N_ELEMENTS(list)
        nMem  = N_ELEMENTS((list[0]).(this))
        dims  = SIZE((list[0]).(this),/DIMENSIONS)
        nDim  = N_ELEMENTS(dims)

        IF nDim EQ 1 THEN BEGIN
           IF (dims[0] EQ 0) OR (dims[0] EQ 1) THEN BEGIN
              dims = nHere
           ENDIF ELSE BEGIN
              dims  = [nHere,dims]
           ENDELSE

        ENDIF

        ;;What we'll output
        finale = MAKE_ARRAY(dims,TYPE=SIZE((list[0]).(this),/TYPE))

        CASE nDim OF
           0: BEGIN
              FOR k=0,nHere-1 DO finale[k] = (list[k]).(this)
           END
           1: BEGIN
              FOR k=0,nHere-1 DO finale[k,*] = (list[k]).(this)
           END
           2: BEGIN
              FOR k=0,nHere-1 DO finale[k,*,*] = (list[k]).(this)
           END
           3: BEGIN
              FOR k=0,nHere-1 DO finale[k,*,*,*] = (list[k]).(this)
           END
        ENDCASE
     END
  ENDCASE

  RETURN,KEYWORD_SET(transpose) ? TRANSPOSE(finale) : finale

END
