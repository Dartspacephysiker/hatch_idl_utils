;    FUNCTION MYFUNCT, X, Y, P
;     ; The independent variables are X and Y
;     ; Parameter values are passed in "P"
;     ZMOD = ... computed model values at (X,Y) ...
;     return, ZMOD
;    END
;2016/07/19
;Only one parameter here, and that's density
; P[0]: E_b,       Plasma bulk energy (eV)
; P[1]: T,         Plasma kinetic temperature (eV)
; P[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; P[3]: n,         Plasma density (cm^-3)
; P[6]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution

;;We don't use the ones below...
; P[4]: inDT,      The "delta_t" for integration of electron counts (UNUSED)
; P[5]: m,         Particle mass (in this case electron mass), in eV/c^2

FUNCTION KAPPA_FLUX2D__SCALE_DENSITY,X,Y,P, $
                                     KAPPA_1D_FITPARAMS=kappa1Dparams

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Structure of each is [energies,angles]
  energies         = X
  angles           = Y

  nAngles          = N_ELEMENTS(Y[0,*])

  A                = kappa1Dparams
  
  ;;Update density estimate so that MPFIT2DFUN thinks it's doing something
  A[3]             = P[0]


  ;;Loop over angles
  Zmodel           = !NULL
  FOR i=0,nAngles-1 DO BEGIN

     tempEn   = energies[*,i]
     
     KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,tempEn,A,angleSlice

     Zmodel   = [[Zmodel],[angleSlice]]

  ENDFOR

  RETURN,Zmodel

END