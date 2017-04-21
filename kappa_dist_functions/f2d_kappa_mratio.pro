;2017/04/21
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa  EQ kappa_0 + 3/2
; A[3]: n,         Plasma density (cm^-3)
;E.g.,
;; P = [5D3,1D3,90,1]
;; R_B = 1
;; f2d = F2D_KAPPA_MRATIO(P,R_B,VZNORM=vzNorm,VPNORM=vPNorm)
;; this = CONTOUR(ALOG10(f2d) > (MAX(ALOG10(f2d)) - 5),vpnorm,vznorm,/FILL,XTITLE='v!Dperp!N',YTITLE='v!Dz!N') & cb = COLORBAR()
;; ;;OR
;; this = CONTOUR(ALOG10(f2d) > (MAX(ALOG10(f2d)) - 5),ALOG10(vpnorm),ALOG10(vznorm),XTITLE='v!Dperp!N',YTITLE='v!Dz!N') & cb = COLORBAR()
FUNCTION F2D_K_MRAT,vp,vz

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON F2DK,fKE_b,fKT,fKkappa,fKn,fKR_B,fKNorm,fKVZLims

  CASE 1 OF
     (fKkappa GE 20): BEGIN
        gammaRat  = EXP(LNGAMMA( fKkappa + 1.0D )-LNGAMMA( fKkappa - 0.5D ))
     END
     ELSE: BEGIN
        gammarat  = GAMMA( fKkappa + 1.0D ) / GAMMA( fKkappa - 0.5D )
     END
  ENDCASE

  edgeries        = vp^2 + vz^2 + fKE_b / fKT - 2 * SQRT(fKE_b / fKT) * SQRT( vz^2 + vp^2 * (1.D - 1.D/fKR_B))

  ;;Unnecessary factor of T in denominator immediately below?
  ;; f2D          = ( !PI * T * (kappa - 1.5D) )^(-1.5D) * gammaRat * ( 1.D + edgeries / (kappa - 1.5D ) / T )^((-1.D) * (kappa + 1))

  f2D             = ( !PI * fKT * (fKkappa - 1.5D) )^(-1.5D) * gammaRat * ( 1.D + edgeries / (fKkappa - 1.5D ) )^((-1.D) * (fKkappa + 1))
  
  RETURN,f2D

END
FUNCTION F2D_K_VZLIMS,x
  COMMON F2DK

  RETURN,fKVZLims
END
PRO F2D_K_INIT,E_b,T,kappa,n,R_B

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON F2DK

  fKE_b    = E_b
  fKT      = T
  fKkappa  = kappa
  fKR_B    = R_B

  nVs      = 1001
  vOff     = (nVs-1)/2.D
  norm     = vOff*0.1
  vzNorm   = (LINDGEN(nVs)-vOff)/norm # REPLICATE(1.0D,nVs)
  vPNorm   = TRANSPOSE(vzNorm)

  ;; xLims    = [MIN(vpNorm) > (-1),MAX(vpNorm) < 1]
  xLims    = [MIN(vpNorm),MAX(vpNorm)]
  fKVZLims = xLims
  fKNorm   = 1.D / INT_2D('F2D_K_MRAT',xLims,'F2D_K_VZLIMS',20,/DOUBLE)
  ;; fKNorm = 1.0D
  PRINT,'fKNorm : ',fKNorm

END
FUNCTION F2D_KAPPA_MRATIO,P,R_B, $
                          VZNORM=vzNorm, $
                          VPNORM=vPNorm, $
                          JUSTPOS=justPos, $
                          NOINIT=noInit

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON F2DK

  E_b                = DOUBLE(P[0])
  T                  = DOUBLE(P[1])
  kappa              = DOUBLE(P[2])
  n                  = DOUBLE(P[3])

  ;;Velocities normalized by thermal velocity
  nVs                = 201
  vOff               = (nVs-1)/2.D
  norm               = vOff
  vzNorm             = (LINDGEN(nVs)-vOff)/norm # REPLICATE(1.0D,nVs)
  vPNorm             = TRANSPOSE(vzNorm)

  ;;Alternate, log approach
  CASE 1 OF
     KEYWORD_SET(justPos): BEGIN
        vzNorm       = POWGEN(1D-3,20,1.1)
        vzNorm       = vzNorm # REPLICATE(1.0D,N_ELEMENTS(vzNorm))
     END
     ELSE: BEGIN
        maxV         = 10
        minV         = -10
        dV           = maxV-minV

        nVs          = 201
        ;; vOff      = (nVs-1)/2.D
        ;; vzNorm    = (LINDGEN(nVs)-vOff)/norm # REPLICATE(1.0D,nVs)
        ;; vzNorm    = [-REVERSE(vzNorm),vzNorm] # REPLICATE(1.0D,N_ELEMENTS(vzNorm)*2)

        vOff         = (maxV+minV)/2.D
        norm         = vOff
        vzNorm       = (LINDGEN(nVs)/(nVs-1.D)*dV+minV) # REPLICATE(1.0D,nVs)

        ;; vPNorm    = TRANSPOSE(vzNorm)

     END
  ENDCASE
  vPNorm             = TRANSPOSE(vzNorm)

  IF ~KEYWORD_SET(noInit) THEN F2D_K_INIT,E_b,T,kappa,n,R_B

  ;; CASE 1 OF
  ;;    (kappa GE 20): BEGIN
  ;;       gammaRat  = EXP(LNGAMMA( kappa + 1.0D )-LNGAMMA( kappa - 0.5D ))
  ;;    END
  ;;    ELSE: BEGIN
  ;;       gammarat  = GAMMA( kappa + 1.0D ) / GAMMA( kappa - 0.5D )
  ;;    END
  ;; ENDCASE

  ;; vzNorm         -= (E_b / T )
  ;; edgeries        = ( vPNorm^2 + vZNorm^2 + E_b / T - 2 * SQRT(E_b / T) * SQRT(vzNorm ^2 + vPNorm^2 * (1.D - 1.D/R_B)) )

  ;;Unnecessary factor of T in denominator immediately below?
  ;; f2D             = ( !PI * T * (kappa - 1.5D) )^(-1.5D) * gammaRat * ( 1.D + edgeries / (kappa - 1.5D ) / T )^((-1.D) * (kappa + 1))

  ;; f2D             = ( !PI * T * (kappa - 1.5D) )^(-1.5D) * gammaRat * ( 1.D + edgeries / (kappa - 1.5D ) )^((-1.D) * (kappa + 1))

  f2D = F2D_K_MRAT(vPNorm,vzNorm) * fKNorm

  RETURN,f2D

END
