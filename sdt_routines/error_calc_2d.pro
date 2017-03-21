PRO ERROR_N,n,errors,nerr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; FOR l=0,N_ELEMENTS(n.x)-1 DO BEGIN
     ;; nerr[l] = n.y[l]  * errors[l].n
     ;; nerr[l] = n.y[l]  * errors.n[l]
  ;; ENDFOR
     nerr = n.y  * errors.n
  
END

PRO ERROR_J,j,errors,jerr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;In #/cm^2-s
  jerr = SQRT((j.y)^(2.D) * $
                 ( (errors.n)^(2.D) + (errors.Uz)^(2.D) + errors.n*errors.Uz*errors.R[*,0,3] ) )  

END

PRO ERROR_JE,n,j,je,T,errors,jeErr

  COMPILE_OPT IDL2,STRICTARRSUBS

  eV_cm2sec_to_mW_m2 = 1.60218D-12
  eV_cm3_to_J_m3     = 1.60218D-13

  ;;I guess we'll SI-a-tize everything here
  vPar              = j.y/n.y/1D2                             ;j.y in #/cm^2-sec and n.y in cm^-3, so mult. by 1e-2 to get m/s
  PPar              = REFORM(T.y[2,*])*n.y*3.D*eV_cm3_to_J_m3 ;T.y in eV, so P in eV/cm^3
  PPrp              = REFORM(T.y[0,*])*n.y*3.D*eV_cm3_to_J_m3
  ;; jePar          = je.y*1D-3                               ;W/m^3

  ;;Parallel heat flux, from Hvec = Qvec - vVec dot P_tensor - 0.5 * vVec * Trace(P_tensor)
  ;; HPar_mW_m2        = je.y - 1.5D * eV_cm2sec_to_mW_m2 * (vPar * PPar - vPar * PPrp) * 1D2 /eV_cm3_to_J_m3 ;in mW/m2
  HPar              = (je.y*1D-3) - 1.5D * (vPar * PPar - vPar * PPrp)                                     ;in W/m2

  ;;sigmas
  sigmaVParSquared  = vPar * vPar * errors.Uz  * errors.Uz
  sigmaPParSquared  = PPar * PPar * errors.Pzz * errors.Pzz
  sigmaPPrpSquared  = PPrp * PPrp * errors.Pxx * errors.Pxx
  sigmaHParSquared  = errors.Hz * errors.Hz * HPar * HPar

  ;;covars
  covarVParPPar     = errors.R[*,3, 6] * (vPar * errors.Uz ) * (PPar * errors.Pzz)
  covarVParPPrp     = errors.R[*,3, 4] * (vPar * errors.Uz ) * (PPar * errors.Pxx)
  covarVParHPar     = errors.R[*,3,12] * (vPar * errors.Uz ) * (HPar * errors.Hz )
  covarPParPPrp     = errors.R[*,6, 4] * (PPar * errors.Pzz) * (PPrp * errors.Pxx)
  covarPParHPar     = errors.R[*,6,12] * (PPar * errors.Pzz) * (HPar * errors.Hz)
  covarPPrpHPar     = errors.R[*,4,12] * (PPrp * errors.Pxx) * (HPar * errors.Hz)

  jeErr = SQRT(sigmaHParSquared + $
               (1.5D*PPar + PPrp) * ( 2.D * TEMPORARY(covarVParHPar) + (1.5D*PPar + PPrp) * TEMPORARY(sigmaVParSquared) ) + $
               vPar * (3.D * TEMPORARY(covarPParHPar) + 2.D * TEMPORARY(covarPPrpHPar) + $
                       (1.5D * PPar + PPrp) * (3.D * TEMPORARY(covarVParPPar) + 2.D * TEMPORARY(covarVParPprp) )        ) + $
               vPar * vPar * (2.25D * TEMPORARY(sigmaPParSquared) + 3.D * covarPParPPrp + TEMPORARY(sigmaPPrpSquared)   )   ) $
          * 1D3 ;Convert back to mW/m^2

END

PRO ERROR_CHARE,j,je,jerr,jeErr,jje_coVar,errors,charEErr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; PRINT,"MISSING TERM: COVAR__JE_PAR__J_PAR (IT'S BEEN SET TO ZERO IN THE MEANTIME)!!!!"
  ;; PRINT,"MISSING TERM: COVAR__JE_PAR__J_PAR (IT'S BEEN SET TO ZERO IN THE MEANTIME)!!!!"
  ;; PRINT,"MISSING TERM: COVAR__JE_PAR__J_PAR (IT'S BEEN SET TO ZERO IN THE MEANTIME)!!!!"

  const          = 6.242D*1.0D11

  ;; covarJParJePar = 0.D * jerr * jeErr

  ;; charEErr       = const * SQRT( (jeErr / j.y)^2.D + ( je.y * jErr / j.y^2.D )^2.D - 2.D * je.y / j.y^3.D * covarJParJePar )
  charEErr       = const * SQRT( (jeErr / j.y)^2.D + ( je.y * jErr / j.y^2.D )^2.D - 2.D * je.y / j.y^3.D * jje_coVar )

END

PRO ERROR_T,T,n,errors,Terr

  COMPILE_OPT IDL2,STRICTARRSUBS

  Tavg             = REFORM(T.y[3,*])
  PPar             = REFORM(T.y[2,*])*n.y*3.D
  PPrp             = REFORM(T.y[0,*])*n.y*3.D

  sigma_N_PPar     = errors.R[*,0,6]*(errors.N*N.y)*(errors.Pzz*PPar)
  sigma_N_PPrp     = errors.R[*,0,4]*(errors.N*N.y)*(errors.Pxx*PPrp)
  sigma_PPar_PPrp  = errors.R[*,6,4]*(errors.Pzz*PPar)*(errors.Pxx*PPrp)

  ;;pi=1/3N, and is for convenience
  piSq             = 1.D/(9.D*n.y*n.y)

  ;;other          = 2*pi*Tavg/N, and is also for convenience
  other            = 2.D*Tavg/(3.D*n.y*n.y)

  Terr             = SQRT( piSq*((errors.Pzz*PPar)^2.D                                        + $
                                 4.D*errors.R[*,6,4]*(errors.Pzz*PPar)*(errors.Pxx*PPrp)      + $
                                 4.D*(errors.Pxx*PPrp)^2.D                                    ) $
                           +                                                                    $
                           (-1.D)*other*(sigma_N_PPar     + $
                                         2.D*sigma_N_PPrp ) $
                           + $
                           (Tavg*errors.n)^(2.D)                                                )
                           ;; See? the n.y terms cancel each other. Hence the simplification above
                           ;; (Tavg/n.y*errors.n*n.y)^(2.D)

END

;2017/03/21
PRO ERROR_CALC_2D,diff_eFlux,errors,n,j,je,T,nerr,jerr,jeErr,charEErr,Terr,jje_coVar;; , $
               ;; ENERGY_ERROR=enErr

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;Raise
  ERROR_N ,n,errors,nerr

  ;The
  ERROR_J ,j,errors,jerr
  ERROR_JE,n,j,je,T,errors,jeErr
  ERROR_CHARE,j,je,jerr,jeErr,jje_coVar,errors,charEErr

  ;Stakes
  ;; IF KEYWORD_SET(enErr) THEN BEGIN
     ;; ERROR_T,T,n,enErr,errors,Terr
  ERROR_T,T,n,errors,Terr
  ;; ENDIF

END
