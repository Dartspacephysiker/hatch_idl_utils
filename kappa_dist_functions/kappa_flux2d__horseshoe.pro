;    FUNCTION MYFUNCT, X, Y, P
;     ; The independent variables are X and Y
;     ; Parameter values are passed in "P"
;     ZMOD = ... computed model values at (X,Y) ...
;     return, ZMOD
;    END

;2018/04/06 Basically … it's deprecated. Just don't use it. Now I use routines with unimaginable titles like KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__LINEAR_ENERGY_SHIFT__COMMON

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

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; @kappa_unitconversion_common.pro

  PRINT,"DEPRECATED, DUDE!"
  STOP

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

     ;; gFunc[calc_i] = ABS( ( mu_vals[calc_i] - mu_0 ) / ( 1. - mu_0 ) )^0.5

     gFunc[calc_i] = ABS( ( mu_vals[calc_i] - mu_0 ) / ( 1. - mu_0 ) )^(3.0)

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

  KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__PARED,X,P,Zmodel

  ;; KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,X,P,Zmodel
     

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;BEGIN HARD WORK
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;In eFlux units
  ;; energy        = X[*,0]
  convFromDF    = (U__gf * U__geom) * X^2 * 2./U__mass/U__mass*1e5
  convToDF      = 1. / (U__dt * U__gf * U__geom  * X^2 * 2./((U__mass)^2)*1e5 )

  convFromEFlux = U__gf * U__geom
  convToEflux   = 1. / (U__dt * U__gf * U__geom)

  ;;First convert model to counts
  tmp           = convFromEFlux * Zmodel
  tmp           = ROUND(U__dt*tmp/(1.+tmp*U__dead)) ;;Remove dead-time correction
  denom         = 1.- U__dead*tmp/U__dt

  ;;Dead-time correction
  void          = WHERE(denom LT .1,count)
  IF count GT 0 THEN BEGIN
     PRINT,MIN(denom,ind)
     denom = denom > .1 
     print,' Error: convert_esa_units2 dead time error.'
     print,' Dead time correction limited to x10 for ',count,' bins'
     print,' Time= ','some time or other' ;TIME_TO_STR(data.time,/msec)
  ENDIF

  ;;Now convert it to DF
  tmp           = tmp * convToDF / denom 
  
  
  ;;Now apply gFunc
  tmp          *= gFunc       ; Bingham and Cairns [2000]

  ;;Now convert back to counts
  tmp           = convFromDF * tmp
  tmp           = ROUND(U__dt*tmp/(1.+tmp*U__dead)) ;;Remove dead-time correction

  ;;Dead time correction
  void          = WHERE(denom LT .1,count)
  IF count GT 0 THEN BEGIN
     PRINT,MIN(denom,ind)
     denom = denom > .1 
     print,' Error: convert_esa_units2 dead time error.'
     print,' Dead time correction limited to x10 for ',count,' bins'
     print,' Time= ','some time or other' ;TIME_TO_STR(data.time,/msec)
  ENDIF

  ;;Now convert it BACK to eFlux
  Zmodel        = tmp * convToEFlux / denom 

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;END HARD WORK
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;Alternatively, skip all this
  ;; Zmodel *= gFunc       ; Bingham and Cairns [2000]


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

