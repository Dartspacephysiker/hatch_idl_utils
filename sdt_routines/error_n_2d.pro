;;2017/03/06
PRO ERROR_N_2D,n,errors,nerr

  COMPILE_OPT IDL2,STRICTARRSUBS

     nerr = n.y  * errors.n
  
END
