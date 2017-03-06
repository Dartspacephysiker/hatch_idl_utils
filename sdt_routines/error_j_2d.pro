;;2017/03/06
PRO ERROR_J_2D,j,errors,jerr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; FOR l=0,N_ELEMENTS(j.x)-1 DO BEGIN
     ;; jerr[l] = SQRT((j.y[l])^(2.D) * $
     ;;                ( (errors[l].n)^(2.D) + (errors[l].Uz)^(2.D) + errors[l].n*errors[l].Uz*errors[l].R[0,3] ) )  
     ;; jerr[l] = SQRT((j.y[l])^(2.D) * $
     ;;                ( (errors.n[l])^(2.D) + (errors.Uz[l])^(2.D) + errors.n[l]*errors.Uz[l]*errors.R[0,3,l] ) )  
  ;; ENDFOR
  jerr = SQRT((j.y)^(2.D) * $
                 ( (errors.n)^(2.D) + (errors.Uz)^(2.D) + errors.n*errors.Uz*errors.R[*,0,3] ) )  

END
