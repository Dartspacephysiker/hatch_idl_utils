;;2015/09/17
FUNCTION VALUE_CLOSEST2,vector,values,diffs,ONLY_GE=only_ge,ONLY_LE=only_LE, $
                        SUCCESS=success, $
                        EXTREME_II=extreme_ii, $
                        ONLY_HAVE_EXTREME=onlyExtreme, $
                        CONSTRAINED=constrained, $
                        QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  inds = VALUE_LOCATE(vector,values)
  
  check = WHERE(inds GT -1 AND inds LT (N_ELEMENTS(vector)-1),nCheck,COMPLEMENT=extreme_ii,NCOMPLEMENT=nExtreme)

  CASE 1 OF
     KEYWORD_SET(only_GE): BEGIN
        CASE nCheck OF
           0: BEGIN
              onlyExtreme = 1
           END
           1: BEGIN
              IF ((vector[inds[check]]-values[check]) LT 0) AND (N_ELEMENTS(vector) GT (inds[check]+1)) THEN inds[check]++
           END
           ELSE: BEGIN
              adjMe = WHERE( ((vector[inds]-values) LT 0) AND (N_ELEMENTS(vector) GT (inds+1)),nAdj)
              IF nAdj GT 0 THEN BEGIN
                 inds[adjMe] += 1
              ENDIF
           END
        ENDCASE
     END
     KEYWORD_SET(only_LE): BEGIN
        CASE nCheck OF
           0: BEGIN
              onlyExtreme = 1
           END
           1: BEGIN
              IF ((vector[inds[check]]-values[check]) GT 0) AND (inds[check] GT 0) THEN inds[check]--
           END
           ELSE: BEGIN
              adjMe = WHERE( ((vector[inds]-values) GT 0) AND (inds GT 0),nAdj)
              IF nAdj GT 0 THEN BEGIN
                 inds[adjMe] -= 1
              ENDIF
           END
        ENDCASE
     END
     ELSE: BEGIN
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

              IF check[0] LE (check[-1] < (N_ELEMENTS(bro) - 1) ) THEN BEGIN

                 inds[(check[0]) : (check[-1] < (N_ELEMENTS(bro) - 1) )] += bro

              ENDIF
           END
        ENDCASE
     END
  ENDCASE

  IF KEYWORD_SET(constrained) AND nExtreme GT 0 THEN BEGIN
     inds = inds > 0 < (N_ELEMENTS(vector)-1)
  ENDIF

  RETURN,inds
END