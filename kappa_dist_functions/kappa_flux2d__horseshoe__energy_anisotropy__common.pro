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

;2016/09/02 Here we're trying to understand how to be the biggest helpers we can
;2016/09/03 This one uses info from the ol' common block
FUNCTION KAPPA_FLUX2D__HORSESHOE__ENERGY_ANISOTROPY__COMMON,X,Y,P, $
   ;; KAPPA_1D_FITPARAMS=kappa1Dparams, $
   BINGHAM_STYLE=Bingham_style, $
   MU_0=mu_0, $
   ;; GFUNC=gFunc, $
   ;; BULK_E_ANISOTROPY=bFunc, $
   ;; BULK_E_ANGLE=bulk_e_angle, $
   IS_MAXWELLIAN_FIT=is_maxwellian_fit

  COMPILE_OPT idl2

  @common__kappa_flux2d__horseshoe__eanisotropy.pro

  ;;Structure of each is [energies,angles]
  nEnergies        = N_ELEMENTS(X[*,0])
  nAngles          = N_ELEMENTS(Y[0,*])

  ;;I know--you wanna know what on earth I'm doing below here.
  ;;Let me tell you: 

  ;; IF nAngles NE N_ELEMENTS(K_EA__angles) THEN BEGIN
  ;;    PRINT,'Trouble!'
  ;; ENDIF
  sortKEA = SORT(k_ea__angles)
  ;; sortA   = SORT(Y[0,*])

  ;; unsortKEA = VALUE_CLOSEST2(k_ea__angles[sortKEA],k_ea__angles)
  ;; unsortA   = VALUE_CLOSEST2((Y[0,*])[sortKEA],Y[0,*])

  smallAngle = MIN(ABS(MEAN(Y,DIMENSION=1)),fa_i)
  k_ea_ii   = sortKEA[VALUE_CLOSEST2(K_EA__angles[sortKEA],REFORM(Y[0,*]))]

  ;; k_ea_ii      = INDGEN(N_ELEMENTS(k_ea__angles))

  mu_vals          = COS(Y/180.*!PI)

  ;;Handle mu vals, gfunction
  ;; IF (N_ELEMENTS(gFunc) EQ 0) OR (N_ELEMENTS(gFunc) NE nAngles) THEN BEGIN
  ;;    IF KEYWORD_SET(Bingham_style) THEN BEGIN   

  ;;       ;;Bingham and Cairns [1999]–style

  ;;       mu_0          = N_ELEMENTS(mu_0) GT 0 ? mu_0 : 0.0 ;mu_0 must be less than one
  ;;       calc_i        = WHERE(mu_vals GE mu_0,nCalc)

  ;;       IF nCalc EQ 0 THEN PRINT,"KAPPA_FLUX2D__HORSESHOE: All mu vals are invalid!"

  ;;       gFunc         = MAKE_ARRAY(nEnergies,nAngles, $
  ;;                                  VALUE=0.0, $
  ;;                                  FLOAT=float, $
  ;;                                  DOUBLE=double)

  ;;       gFunc[calc_i] = ABS( ( mu_vals[calc_i] - mu_0 ) / ( 1. - mu_0 ) )^(3.0)
  ;;       ;; plot,y[0,*],gfunc[0,*]

  ;;    ENDIF ELSE BEGIN
  ;;       ;;It's going to make an upgoing horseshoe!
  ;;       gFunc         = ( mu_vals + 1 )^2 
  ;;    ENDELSE
  ;; ENDIF

  ;; IF N_ELEMENTS(bFunc) EQ 0 THEN BEGIN
     ;; K_EA__bFunc = MAKE_ARRAY(nAngles,VALUE=1.0)
  ;; ENDIF ELSE BEGIN
  ;;    ;; PRINT,'Bulking out'
  ;; ENDELSE

  ;;Loop over angles
  Zmodel           = MAKE_ARRAY(nEnergies,nAngles, $
                                VALUE=0.0, $
                                FLOAT=float, $
                                DOUBLE=double)

  CASE 1 OF
     KEYWORD_SET(is_maxwellian_fit): BEGIN

        IF nAngles NE N_ELEMENTS(k_ea_ii) THEN STOP

        FOR i=0,nAngles-1 DO BEGIN

           tempEn   = X[*,i]
           tempP    = P
           tempP[0] = tempP[0]*K_EA__bFunc[k_ea_ii[i]]
           ;; tempP[0] = tempP[0]*K_EA__bFunc[i]

           MAXWELL_FLUX,tempEn,tempP,angleSlice

           Zmodel[*,i]  = angleSlice * K_EA__gFunc[k_ea_ii[i]] ; Bingham and Cairns [2000]
           ;; Zmodel[*,i]  = angleSlice * K_EA__gFunc[i]          ; Bingham and Cairns [2000]

        ENDFOR

     END
     ELSE: BEGIN
        FOR i=0,nAngles-1 DO BEGIN

           tempEn   = X[*,i]
           tempP    = P
           tempP[0] = tempP[0]*K_EA__bFunc[k_ea_ii[i]]
           ;; tempP[0] = tempP[0]*K_EA__bFunc[i]

           ;; PRINT,FORMAT='("tempP, factor, angle, aIntended ",T35,": ",F0.2,T50,F0.2,T65,F0.2,T80,F0.2)', $
           ;;       tempP[0], $
           ;;       K_EA__bFunc[k_ea_ii[i]], $
           ;;       MEAN(Y[*,i]), $
           ;;       bulk_e_angle[i]


           KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__PARED,tempEn,tempP,angleSlice
           
           ;; Zmodel[*,i]  = angleSlice                          ; No horseshoe nothin.
           
           Zmodel[*,i]  = angleSlice * K_EA__gFunc[k_ea_ii[i]] ; Bingham and Cairns [2000]
           ;; Zmodel[*,i]  = angleSlice * K_EA__gFunc[i] ; Bingham and Cairns [2000]
           ;; Zmodel[*,i]  = angleSlice * ( mu_vals[*,i] + 1 )^2 ; Bingham and Cairns [2000]
           
           ;; IF KEYWORD_SET(Bingham_style) THEN BEGIN
           ;;    Zmodel[*,i]  = angleSlice * ( mu_vals[*,i] + 1 ) ; Bingham et al.     [1999]
           ;; ENDIF

        ENDFOR

        ;; KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__PARED,X,P,Zmodel

        ;; KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,X,P,Zmodel
        
        ;; Zmodel *= gFunc       ; Bingham and Cairns [2000]


     END
  ENDCASE


  RETURN,Zmodel

END

