;2016/04/25 Sick of typing N_ELEMENTS(WHERE(blah blah blah...))
PRO NWHERE,input,comparison_string,count,out_i, $
           COMPLEMENT=complement, $
           NCOMPLEMENT=nComplement, $
           NULL=NULL

  IF KEYWORD_SET(NULL) THEN nullStr = ',NULL=1' ELSE nullStr = ''

  executeStr        = " out_i = WHERE(input " + comparison_string + ',count' + $
                    nullStr + $
                    ",COMPLEMENT=complement,NCOMPLEMENT=nComplement)"

  IF ~KEYWORD_SET(quiet) THEN PRINT,executeStr

  ;; success = EXECUTE(" out_i = WHERE(input " + comparison_string + ',count' + $
  ;;                   nullStr + $
  ;;                   ",COMPLEMENT=complement,NCOMPLEMENT=nComplement)")
  success = EXECUTE(executeStr)

  PRINT,count


END