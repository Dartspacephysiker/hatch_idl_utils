;;2015/09/17
FUNCTION VALUE_CLOSEST2,vector,values,diffs,ONLY_GE=only_ge,ONLY_LE=only_LE, $
                       SUCCESS=success, $
                       QUIET=quiet

  COMPILE_OPT idl2

  inds = VALUE_LOCATE(vector,values)
  
  check = WHERE(inds GT -1 AND inds LT (N_ELEMENTS(vector)-1))
  
  test = [TRANSPOSE(vector[inds[check]]-values[check[0]:check[-1]]), $
          TRANSPOSE(vector[inds[check]+1]-values[check[0]:check[-1]])]

  bro = REFORM(ABS(test[0,*]) GT ABS(test[1,*]))

  inds[(check[0]) : (check[-1] < (N_ELEMENTS(bro) - 1) )] += bro

  RETURN,inds
END