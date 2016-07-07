PRO KAPPA__GET_A_ESTIMATES,dat,Xorig, $
                           minEInd,maxEInd,nEnergies, $
                           peak_ind,peak_energy,eRange_peak, $
                           ANGLES=angles, $
                           KAPPA_EST=kappa, $
                           MASS=mass, $
                           ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                           ESTFACS=estFacs, $
                           A_OUT=A, $
                           AGAUSS_OUT=AGauss

  COMPILE_OPT idl2

  IF SIZE(estFacs,/TYPE) NE 8 THEN BEGIN
     estFacs      = {T:1.0, $
                     N:10., $
                     B_E:1.0, $
                     TGauss:1.0, $
                     NGauss:5.0, $
                     B_EGauss:1.0}
  ENDIF

  min_energy      =  Xorig[(minEInd - 2) > 0]
  max_energy      =  Xorig[(maxEInd + 2) < (nEnergies - 1)]

  eRange_peak     = [min_energy,max_energy]

  bulk_energy     = peak_energy*estFacs.B_E
  T               = (T_2D_FS(dat,ENERGY=eRange_peak,ANGLE=e_angle))[3]*estFacs.T ;T_avg
  n_est           = N_2D_FS(dat,ENERGY=eRange_peak,ANGLE=e_angle)*estFacs.N
  A               = DOUBLE([bulk_energy,T,kappa,n_est, $
                            dat.integ_t,mass,angles]) 
  
  PRINT,"Here's my initial estimate based on spectral properties: "
  PRINT_KAPPA_FLUX_FIT_PARAMS,A
  PRINT,''

  IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
     bulk_EGauss  = peak_energy*estFacs.B_EGauss

     TGauss       = (T_2D_FS(dat,ENERGY=eRange_peak,ANGLE=e_angle))[3]*estFacs.TGauss
     n_estGauss   = N_2D_FS(dat,ENERGY=eRange_peak,ANGLE=e_angle)*estFacs.NGauss
     
     kappaGauss   = 180

     AGauss       = DOUBLE([bulk_EGauss,TGauss,kappaGauss,n_estGauss, $
                            dat.integ_t,mass,angles])

     PRINT,"Here's my initial Gaussian estimate based on spectral properties: "
     PRINT_KAPPA_FLUX_FIT_PARAMS,AGauss
     PRINT,''
  ENDIF

END
