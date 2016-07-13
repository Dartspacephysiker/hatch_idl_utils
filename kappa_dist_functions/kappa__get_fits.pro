PRO KAPPA__GET_FITS,Xorig,Yorig, $
                    orig,kappaFit,gaussFit, $
                    ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
                    USE_SDT_GAUSSIAN_FIT=use_SDT_Gaussian_fit, $
                    ENERGY_INDS=energy_inds, $
                    ERANGE_PEAK=eRange_peak, $
                    PEAK_IND=peak_ind, $
                    BOUNDS_I=bounds_i, $
                    KAPPA_A=A, $
                    GAUSS_A=AGauss, $
                    YMAX=yMax, $
                    MAX_ITER=max_iter, $
                    FIT_TOL=fit_tol, $
                    STRINGS=strings, $
                    TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
                    OUT_FITTED_PARAMS=out_fitted_params, $
                    OUT_FITTED_GAUSS_PARAMS=out_fitted_Gauss_params, $
                    OUT_KAPPA_FIT_STRUCTS=out_kappa_fit_structs, $
                    OUT_GAUSS_FIT_STRUCTS=out_gauss_fit_structs, $
                    ADD_FULL_FITS=add_full_fits, $
                    OUT_ERANGE_PEAK=out_eRange_peak, $
                    OUT_PARAMSTR=out_paramStr

  COMMON FIT_MASS,mass

  COMPILE_OPT idl2

  orig                        = {x:Xorig, $
                                 y:Yorig, $
                                 name:strings.plotTimes[bounds_i]}
  
  ;;Trim energies vector if attempting to fit below peak
  IF KEYWORD_SET(trim_energies_below_peak) THEN BEGIN 
     X                        = Xorig[energy_inds[0]:energy_inds[1]] 
     Y                        = Yorig[energy_inds[0]:energy_inds[1]] 
  ENDIF

  weights                     = 1./ABS(Y)
  yFit                        = CURVEFIT(X,Y,weights,A,SIGMA, $
                                         FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F' , $
                                         /DOUBLE, $
                                         FITA=[1,1,1,1,0,0,0], $
                                         ITMAX=KEYWORD_SET(max_iter) ? max_iter : 150, $
                                         CHI2=chi2, $
                                         ITER=itNum, $
                                         TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                                         STATUS=fitStatus)
  IF FINITE(chi2) THEN BEGIN
     pVal                        = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X)-4) ;4 for the 4 params that were allowed to participate in this fit
  ENDIF ELSE BEGIN
     pVal                        = -1
  ENDELSE

  ;;need to adjust Y bounds?
  yMax                        = MAX(yFit) 

  PRINT,"Fitted spectral properties: "
  PRINT_KAPPA_FLUX_FIT_PARAMS,A
  PRINT,''
  out_fitted_params           = N_ELEMENTS(out_fitted_params) GT 0 ? $
                                [[out_fitted_params],[A]] : A
  out_eRange_peak             = N_ELEMENTS(out_eRange_peak) GT 0 ? $
                                [[out_eRange_peak],[eRange_peak]] : eRange_peak
  out_paramStr                = STRING(FORMAT='(A0,"--",A0,A0,"--orb_",A0,"__",A0,"--",I0,"-",I0,".txt")', $
                                       strings.timeFNStrs[bounds_i], $
                                       strings.eeb_or_ees, $
                                       strings.avgStr, $
                                       strings.orbStr, $
                                       strings.orbDate)

  CASE fitStatus OF 
     0: BEGIN 
        PRINT,'Fit success!' 
        fitFail               = 0 
     END 
     1: BEGIN 
        PRINT,'Fit failure! Chi-square increasing without bound!' 
        fitFail               = 1 
     END 
     2: BEGIN 
        PRINT,'Fit failure! No convergence in ' + STRCOMPRESS(itNum,/REMOVE_ALL) + ' iterations!' 
        fitFail               = 1 
     END 
  ENDCASE

  kappaFit                    = {x:X, $
                                 y:yFit, $
                                 NAME:"Fitted spectrum", $
                                 A:A, $
                                 ;; time:STR_TO_TIME(strings.yearstr[bounds_i]+'/'+strings.plotTimes[bounds_i]), $
                                 time:STR_TO_TIME(strings.yearstr+'/'+strings.plotTimes[bounds_i]), $
                                 ;; XRANGE:xRange, $
                                 ;; YRANGE:yRange, $
                                 ;; XLOG:1, $
                                 ;; YLOG:1, $
                                 fitStatus:fitStatus, $
                                 chi2:chi2, $
                                 pVal:pVal}

  IF KEYWORD_SET(add_full_fits) THEN BEGIN
     KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,Xorig,A,yFull
     kappaFit                 = CREATE_STRUCT(kappaFit,"xFull",Xorig,"yFull",yFull)
  ENDIF

  ;; out_kappa_fit_structs       = N_ELEMENTS(out_kappa_fit_structs) EQ 0 ? kappaFit : [out_kappa_fit_structs,kappaFit]
  IF N_ELEMENTS(out_kappa_fit_structs) EQ 0 THEN BEGIN
     out_kappa_fit_structs    = LIST(kappaFit)
  ENDIF ELSE BEGIN
     out_kappa_fit_structs.Add,kappaFit
  ENDELSE

  IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
     ;; weights               = SQRT(ABS(Y))
     weights                  = 1./ABS(Y)
     ;; weights[0]            = SQRT(SQRT(weights[0]))
     ;; weights[0:-1]         = 1.

     CASE 1 OF
        KEYWORD_SET(use_SDT_Gaussian_fit): BEGIN
           X_SDT              = Xorig[peak_ind-2:energy_inds[1]]
           Y_SDT              = Yorig[peak_ind-2:energy_inds[1]]
           yGaussFit          = CURVEFIT(X_SDT, Y_SDT, weights, AGauss, sigma, FUNCTION_NAME='MAXWELLIAN_1', $
                                         /DOUBLE, $
                                         ITMAX=KEYWORD_SET(max_iter) ? max_iter : 150, $
                                         ITER=itNum, $
                                         CHI2=chi2, $
                                         TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                                         STATUS=gaussFitStatus)
           PRINT,'Final fit     : afit=',AGauss
           IF FINITE(chi2) THEN BEGIN
              pValGauss       = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X_SDT)-2) ;2 for the 2 params that participated in this fit
              real_chi2       = TOTAL((yGaussFit-Y_SDT)^2*weights)/(N_ELEMENTS(yGaussFit) - 3) ; 3 for the number that could have participated
              real_pValGauss  = 1 - CHISQR_PDF(real_chi2,N_ELEMENTS(X_SDT) - 3)
           ENDIF ELSE BEGIN
              pValGauss       = -1
              real_chi2       = -1
              real_pValGauss  = -1
           ENDELSE
        END
        ELSE: BEGIN
           yGaussFit          = CURVEFIT(X, Y, weights, AGauss, sigma, FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F' , $
                                         /DOUBLE, $
                                         FITA=[1,1,0,1,0,0,0], $
                                         ITMAX=KEYWORD_SET(max_iter) ? max_iter : 150, $
                                         ITER=itNum, $
                                         CHI2=chi2, $
                                         TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                                         STATUS=gaussFitStatus)
           IF FINITE(chi2) THEN BEGIN
              pValGauss       = 1 - CHISQR_PDF(chi2,N_ELEMENTS(X)-3) ;3 for the 3 params that were allowed to participate in this fit
           ENDIF ELSE BEGIN
              pValGauss       = -1
           ENDELSE
        END
     ENDCASE

     ;;need to adjust Y bounds?
     ;; yRange[1]             = MAX(yGaussFit) > yRange[1]
     yMax                     = MAX(yGaussFit) > yMax

     PRINT,''

     CASE gaussFitStatus OF 
        0: BEGIN 
           PRINT,'GaussFit success!' 
           gaussFitFail       = 0 
        END 
        1: BEGIN 
           PRINT,'GaussFit failure! Chi-square increasing without bound!' 
           gaussFitFail       = 1 
        END 
        2: BEGIN 
           PRINT,'GaussFit failure! No convergence in ' + STRCOMPRESS(itNum,/REMOVE_ALL) + ' iterations!' 
           gaussFitFail       = 1 
        END 
     ENDCASE

     IF KEYWORD_SET(use_SDT_Gaussian_fit) THEN BEGIN
        T  = -1./AGauss[1]
        f0 = AGauss[0]
        density = AGauss[0]*exp(AGauss[1]*X_SDT[1])*1.e-15 * (-1.*2.*3.1416*1.6e-12/(AGauss[1]*(mass)))^1.5 

        ;;Get AGauss in the form we like it
        maxThing              = MAX(yGaussFit,maxGaussInd)
        AGauss_SDT            = AGauss
        AGauss                = [X_SDT[maxGaussInd],T,999,density,A[4],A[5],A[6]]

     ENDIF

     PRINT,"Gaussian fitted spectral properties: "
     PRINT_KAPPA_FLUX_FIT_PARAMS,AGauss

     gaussFit                 = {x:KEYWORD_SET(use_SDT_Gaussian_fit) ? X_SDT : X, $
                                 y:yGaussFit, $
                                 name:"Gaussian Fitted spectrum" + (KEYWORD_SET(use_SDT_Gaussian_fit) ? "_SDT" : ''), $
                                 A:AGauss, $
                                 ;; time:STR_TO_TIME(strings.yearstr[bounds_i]+'/'+strings.plotTimes[bounds_i]), $
                                 time:STR_TO_TIME(strings.yearstr+'/'+strings.plotTimes[bounds_i]), $
                                 ;; XRANGE:xRange, $
                                 ;; YRANGE:yRange, $
                                 ;; XLOG:1, $
                                 ;; YLOG:1, $
                                 fitStatus:gaussFitStatus, $
                                 chi2:chi2, $
                                 pVal:pValGauss, $
                                 is_sdt_fit:KEYWORD_SET(use_SDT_Gaussian_fit)}

     IF KEYWORD_SET(use_SDT_Gaussian_fit) THEN BEGIN
        gaussFit              = CREATE_STRUCT(gaussFit,"chi2_3vars",real_chi2,"pVal_3vars",real_pValGauss)
     ENDIF

     IF KEYWORD_SET(add_full_fits) THEN BEGIN
        CASE 1 OF
           KEYWORD_SET(use_SDT_Gaussian_fit): BEGIN
              MAXWELLIAN_1,Xorig,AGauss_SDT,yGaussFull
           END
           ELSE: BEGIN
              KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F,Xorig,AGauss,yGaussFull
           END
        ENDCASE
        gaussFit                 = CREATE_STRUCT(gaussFit,"xFull",Xorig,"yFull",yGaussFull)

     ENDIF

     ;; out_gauss_fit_structs    = N_ELEMENTS(out_gauss_fit_structs) EQ 0 ? gaussFit : [out_gauss_fit_structs,gaussFit]
     IF N_ELEMENTS(out_gauss_fit_structs) EQ 0 THEN BEGIN
        out_gauss_fit_structs    = LIST(gaussFit)
     ENDIF ELSE BEGIN
        out_gauss_fit_structs.Add,gaussFit
     ENDELSE

     out_fitted_Gauss_params  = N_ELEMENTS(out_fitted_Gauss_params) GT 0 ? $
                                [[out_fitted_Gauss_params],[AGauss]] : $
                                AGauss


  ENDIF

END
