;;2017/02/14
FUNCTION CHAR_ENERGY,j,je

  COMPILE_OPT IDL2,STRICTARRSUBS

  CASE SIZE(j,/TYPE) OF
     8: BEGIN
        tmpj = j.y
     END
     ELSE: BEGIN
        tmpj = j
     END
  ENDCASE

  CASE SIZE(je,/TYPE) OF
     8: BEGIN
        tmpje = je.y
     END
     ELSE: BEGIN
        tmpje = je
     END
  ENDCASE

  RETURN,tmpje/tmpj*6.242*1.0e11

END
