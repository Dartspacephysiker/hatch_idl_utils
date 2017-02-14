;;2017/02/14
FUNCTION CHAR_ENERGY,je,jee

  COMPILE_OPT IDL2

  CASE SIZE(je,/TYPE) OF
     8: BEGIN
        tmpje = je.y
     END
     ELSE: BEGIN
        tmpje = je
     END
  ENDCASE

  CASE SIZE(jee,/TYPE) OF
     8: BEGIN
        tmpjee = jee.y
     END
     ELSE: BEGIN
        tmpjee = jee
     END
  ENDCASE

  RETURN,tmpjee/tmpje*6.242*1.0e11

END
