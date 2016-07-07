PRO KAPPA__GET_A_ESTIMATES,dat,Xorig,Yorig, $
                           minEInd,maxEInd,nEnergies, $
                           peak_ind,peak_energy,eRange_peak, $
                           ANGLES=angles, $
                           KAPPA_EST=kappa, $
                           E_ANGLE=e_angle, $
                           ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                           USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                           ESTFACS=estFacs, $
                           A_OUT=A, $
                           AGAUSS_OUT=AGauss

  COMPILE_OPT idl2

  COMMON FIT_MASS,mass

  ;; dat.mass is a scaler with units of eV/(km/s)^2. Use 1.6e-12erg/eV * (km/1.e5cm)^2 to convert
  mass = dat.mass*1.6e-22

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
  A               = DOUBLE([bulk_energy,T,kappa,n_est,dat.integ_t,dat.mass,MEAN(angles)]) 
  
  PRINT,"Here's my initial estimate based on spectral properties: "
  PRINT_KAPPA_FLUX_FIT_PARAMS,A
  PRINT,''

  IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN

     IF KEYWORD_SET(use_SDT_Gaussian_fit) THEN BEGIN
        MAXWELLIAN_1,Xorig,AGauss,Yorig,pder,INDEX=[peak_ind,maxEInd],UNITS='eFlux'

        T  = -1./Agauss[1]
        f0 = AGauss[0]
        density = AGauss[0]*exp(AGauss[1]*Xorig[peak_ind])*1.e-15 * (-1.*2.*3.1416*1.6e-12/(AGauss[1]*(mass)))^1.5 

        ;;Get AGauss in the form we like it
        PRINT,"Here's my initial Gaussian estimate based on spectral properties: "
        PRINT_KAPPA_FLUX_FIT_PARAMS,[Xorig[peak_ind],T,999,density,A[4],A[5]]

     ENDIF ELSE BEGIN

        bulk_EGauss  = peak_energy*estFacs.B_EGauss

        TGauss       = (T_2D_FS(dat,ENERGY=eRange_peak,ANGLE=e_angle))[3]*estFacs.TGauss
        n_estGauss   = N_2D_FS(dat,ENERGY=eRange_peak,ANGLE=e_angle)*estFacs.NGauss
        
        kappaGauss   = 100

        AGauss       = DOUBLE([bulk_EGauss,TGauss,kappaGauss,n_estGauss, $
                               dat.integ_t,dat.mass,MEAN(angles)])

        PRINT,"Here's my initial Gaussian estimate based on spectral properties: "
        PRINT_KAPPA_FLUX_FIT_PARAMS,AGauss
        PRINT,''
     ENDELSE
  ENDIF

END
