;;2015/09/17
FUNCTION VALUE_CLOSEST2,vector,values,diffs,ONLY_GE=only_ge,ONLY_LE=only_LE, $
                        SUCCESS=success, $
                        EXTREME_I=extreme_i, $
                        ONLY_HAVE_EXTREME=onlyExtreme, $
                        QUIET=quiet

  COMPILE_OPT idl2

  inds = VALUE_LOCATE(vector,values)
  
  check = WHERE(inds GT -1 AND inds LT (N_ELEMENTS(vector)-1),nCheck,COMPLEMENT=extreme_i)
  
  ;; IF nCheck GT 0 THEN BEGIN
  ;;    test = [TRANSPOSE(vector[inds[check]]-values[check[0]:check[-1]]), $
  ;;            TRANSPOSE(vector[inds[check]+1]-values[check[0]:check[-1]])]

  ;;    bro = REFORM(ABS(test[0,*]) GT ABS(test[1,*]))

  ;;    inds[(check[0]) : (check[-1] < (N_ELEMENTS(bro) - 1) )] += bro
  ;; ENDIF ELSE BEGIN
  ;;    onlyExtreme = 1
  ;; ENDELSE

  CASE nCheck OF
     0: BEGIN
        onlyExtreme = 1
     END
     1: BEGIN
        test = [vector[inds[check[0]]]-values[check[0]], $
                vector[inds[check[0]]+1]-values[check[0]]]

        bro = REFORM(ABS(test[0,*]) GT ABS(test[1,*]))

        inds[check[0]] += bro
     END
     ELSE: BEGIN
        test = [TRANSPOSE(vector[inds[check]]-values[check[0]:check[-1]]), $
                TRANSPOSE(vector[inds[check]+1]-values[check[0]:check[-1]])]

        bro = REFORM(ABS(test[0,*]) GT ABS(test[1,*]))

        inds[(check[0]) : (check[-1] < (N_ELEMENTS(bro) - 1) )] += bro
     END
  ENDCASE

  RETURN,inds
END