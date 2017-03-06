;;2017/03/06
PRO ERROR_T_2D,T,n,errors,Terr

  COMPILE_OPT IDL2,STRICTARRSUBS

  Tavg             = REFORM(T.y[3,*])
  PPar             = REFORM(T.y[2,*])*n.y
  PPrp             = REFORM(T.y[0,*])*n.y

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
