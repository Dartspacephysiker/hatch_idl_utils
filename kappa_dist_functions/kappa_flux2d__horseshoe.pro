;    FUNCTION MYFUNCT, X, Y, P
;     ; The independent variables are X and Y
;     ; Parameter values are passed in "P"
;     ZMOD = ... computed model values at (X,Y) ...
;     return, ZMOD
;    END
;2016/09/01 
;Now we include a functional form for the horseshoe that depends on mu = v_par/v_tot,
; after work by folks like like Bingham et al. [1999] or Bingham and Cairns [2000]
;
;Previously only one parameter in P, and that was density
;As for A, well ... we retired him.
; P[0]: E_b,       Plasma bulk energy (eV)
; P[1]: T,         Plasma kinetic temperature (eV)
; P[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; P[3]: n,         Plasma density (cm^-3)

FUNCTION KAPPA_FLUX2D__HORSESHOE,X,Y,P, $
                                 ;; KAPPA_1D_FITPARAMS=kappa1Dparams, $
                                 BINGHAM_STYLE=Bingham_style, $
                                 MU_0=mu_0

  COMPILE_OPT idl2

  ;;Structure of each is [energies,angles]
  nEnergies        = N_ELEMENTS(X[*,0])
  nAngles          = N_ELEMENTS(Y[0,*])

  mu_vals          = COS(Y/180.*!PI)

  ;;Handle mu vals, gfunction
  IF KEYWORD_SET(Bingham_style) THEN BEGIN   

     ;;Bingham and Cairns [1999]–style

     mu_0          = N_ELEMENTS(mu_0) GT 0 ? mu_0 : 0.0  ;mu_0 must be less than one
     calc_i        = WHERE(mu_vals GE mu_0,nCalc)

     IF nCalc EQ 0 THEN PRINT,"KAPPA_FLUX2D__HORSESHOE: All mu vals are invalid!"

     gFunc         = MAKE_ARRAY(nEnergies,nAngles, $
                                VALUE=0.0, $
                                FLOAT=float, $
                                DOUBLE=double)

     gFunc[calc_i] = ABS( ( mu_vals[calc_i] - mu_0 ) / ( 1. - mu_0 ) )^0.5
     ;; plot,y[0,*],gfunc[0,*]

  ENDIF ELSE BEGIN
     ;;It's going to make an upgoing horseshoe!
     gFunc         = ( mu_vals + 1 )^2 
  ENDELSE

  ;;Loop over angles
  Zmodel           = MAKE_ARRAY(nEnergies,nAngles, $
                                VALUE=0.0, $
                                FLOAT=float, $
                                DOUBLE=double)

  KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,X,P,Zmodel
     
  Zmodel *= gFunc       ; Bingham and Cairns [2000]


  RETURN,Zmodel

END

  ;; FOR i=0,nAngles-1 DO BEGIN

  ;;    tempEn   = energies[*,i]
     
  ;;    KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__PARED,tempEn,P,angleSlice

     
  ;;    ;; KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,tempEn,A,angleSlice
     
  ;;    ;; Zmodel[*,i]  = angleSlice                          ; No horseshoe nothin.
     
  ;;    Zmodel[*,i]  = angleSlice * gFunc[*,i]                ; Bingham and Cairns [2000]
  ;;    ;; Zmodel[*,i]  = angleSlice * ( mu_vals[*,i] + 1 )^2 ; Bingham and Cairns [2000]
     
  ;;    ;; IF KEYWORD_SET(Bingham_style) THEN BEGIN
  ;;    ;;    Zmodel[*,i]  = angleSlice * ( mu_vals[*,i] + 1 ) ; Bingham et al.     [1999]
  ;;    ;; ENDIF

  ;; ENDFOR

