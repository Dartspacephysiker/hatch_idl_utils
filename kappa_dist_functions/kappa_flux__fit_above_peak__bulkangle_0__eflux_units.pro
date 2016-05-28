;2016/05/13
;X      = vector of energies in eV for which number flux, j(X), is to be calcked
;A      = vector of function params:
;F      = vector of returned probabilities for given energies
; A[0]: E_b,       Plasma bulk energy (eV)
; A[1]: T,         Plasma kinetic temperature (eV)
; A[2]: kappa,     Kappa (of course!)--or more specifically 3D kappa index, so that kappa = kappa_0 + 3/2
; A[3]: n,         Plasma density
; A[4]: bulkAngle, Angle between bulk velocity, u_b, and velocity in direction for which we're interested in the distribution
;;Assuming a field-aligned beam, only fit energies above peak energy

;;get yourself some data from orb 10000
;; @startup
;; timeStr = '99-3-2/18:08:42' & t=str_to_time(timeStr) &  dat = get_fa_ees(t) ; get electron esa survey
;; kappa_flux__fit_above_peak__bulkangle_0,TEMPERATURE=100,SDT_DAT=dat

PRO KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0__EFLUX_UNITS, $;X,A,F,pders, $
   T1=t1, $
   T2=t2, $
   EEB_OR_EES=eeb_or_ees, $
   SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
   SDT_TIME_INDS=bounds, $
   DENSITY_EST=n_est, $
   TEMPERATURE_EST=T, $
   KAPPA_EST=kappa, $
   SDT_DAT=dat, $
   BULK_OFFSET=bulk_offset, $
   ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
   TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
   N_ENERGIES_BELOW_PEAK=n_below_peak, $
   N_ENERGIES_AFTER_PEAK=n_after_peak, $
   CHECK_FOR_HIGHER_FLUX_PEAKS__SET_CORRESPONDING_PEAK_ENERGY=check_for_higher_flux_peaks__set_corresponding_peak_energy, $
   FIT_TOLERANCE=fit_tol, $
   MAX_ITERATIONS=max_iter, $
   ADD_GAUSSIAN_ESTIMATE=add_gaussian_estimate, $
   ADD_ONECOUNT_CURVE=add_oneCount_curve, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
   GET_MASS_AND_DT=get_mass_and_dt, $
   SAVE_FITPLOTS=save_fitplots
   ;; SAVE_FITS=save_fits

;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;;From Kivelson & Russell, Table 2.2 "Properties of Typical Plasmas"
;;   ;;"Magnetosphere"
;;   T_def                 = 1000L    ;eV
;;   n_def                 = 1L       ;cm^-3
;;   E_b_def               = 500L     ;bulk energy, eV

;;   IF N_ELEMENTS(kappa)       EQ 0 THEN kappa        = 5        ;a guess
;;   IF N_ELEMENTS(T)           EQ 0 THEN T            = T_def    ;temperature guess in eV
;;   IF N_ELEMENTS(n_est)       EQ 0 THEN n_est        = n_def    ;n guess in cm^-3
;;   IF N_ELEMENTS(peak_energy) EQ 0 THEN peak_energy  = E_b_def  ;peak energy guess, eV    

;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;;From Orb 10000 at 18:08:46
  ;; inDir       = '/SPENCEdata/software/sdt/batch_jobs/20160420--fit_Maxwellians_kappas_for_inverted_Vs/'
  ;; restFile    = inDir + 'nFlux_and_eSpec--orb_10000__18_08_36-18_09_00.sav'
  ;; RESTORE,restFile

  IF N_ELEMENTS(estimate_A_from_data) EQ 0 THEN BEGIN
     PRINT,'Estimating fit params from SDT data...'
     estimate_A_from_data = 1
  ENDIF

  IF N_ELEMENTS(eSpec) EQ 0 THEN BEGIN
     GET_LOSSCONE_EN_SPEC_AND_NFLUX_DATA,T1=t1,T2=t2, $
                                         EEB_OR_EES=eeb_or_ees, $
                                         EN_SPEC=eSpec, $
                                         SPECTRA_AVERAGE_INTERVAL=spectra_average_interval, $
                                         JE_EN=je_en, $
                                         OUT_ORB=orb, $
                                         OUT_LC_ANGLERANGE=e_angle, $
                                         ELECTRON_ENERGY_LIMS=energy_electrons, $
                                         ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
                                         /GET_MASS_AND_DT, $
                                         OUT_MASS=mass, $
                                         OUT_DT=dt
                                         ;; /SAVE_ESPEC_AND_NFLUX, $
                                         ;; SAVEFILENAME=saveFN


     orbStr   = STRCOMPRESS(orb,/REMOVE_ALL)
  ENDIF

  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
     GET_ONECOUNT_NFLUX_CURVE,t1,t2,oneCurve, $
                              EEB_OR_EES=eeb_or_ees, $
                              ANGLE=e_angle, $
                              ENERGY=energy_electrons, $
                              SPECTRA_AVERAGE_INTERVAL=spectra_average_interval
  ENDIF

  ;; times       = je_en.x
  times       = eSpec.x
  yearStr     = STRMID(TIME_TO_STR(times[0],/MSEC),0,10)
  timeStrs    = STRMID(TIME_TO_STR(times,/MSEC),11,11)
  timeFNStrs  = timeStrs.REPLACE(':', '_')
  timeFNStrs  = timeFNStrs.REPLACE('.', '__')

  IF N_ELEMENTS(trim_energies_below_peak) EQ 0 THEN BEGIN
     trim_energies_below_peak = 1
  ENDIF


  IF ~KEYWORD_SET(n_est) THEN  BEGIN
     n_est = 0.004
     PRINT,FORMAT='("Default density estimate  : ",F-10.4)',n_est
  ENDIF
  IF ~KEYWORD_SET(kappa) THEN BEGIN
     kappa    = 2.0             ;Why not?
     PRINT,FORMAT='("Default kappa estimate    : ",F-10.4)',kappa
  ENDIF

  IF ~KEYWORD_SET(bounds) THEN BEGIN
     ;; bounds      = 15
     bounds      = 0            ;just do one
  ENDIF

  b_offset       = KEYWORD_SET(bulk_offset) ? bulk_offset : 0

  ;;Loop over provided indices, plot data as well as fit, and optionally save
  routine        = 'get_fa_'+eeb_or_ees
  IF KEYWORD_SET(spectra_average_interval) THEN routine += '_ts'

  IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN
     oneCurveMod = oneCurve.y[bounds,*]
     yMin        = MIN(oneCurveMod[WHERE(oneCurveMod GT 0)])
     ;; yMin        = MIN(oneCurve.y[bounds,WHERE(oneCurveMod GT 0)])
  ENDIF ELSE BEGIN
     je_en_mod   = je_en.y[bounds,*]
     yMin        = MIN(je_en_mod[WHERE(je_en_mod GT 0)])
     ;; yMin        = MIN(je_en.y[bounds,WHERE(je_en_mod GT 0)])
  ENDELSE

  FOR i=0,N_ELEMENTS(bounds)-1 DO BEGIN

     Xorig       = REVERSE(REFORM(eSpec.v[bounds[i],*]))
     Y           = REVERSE(REFORM(je_en.y[bounds[i],*]))

     IF KEYWORD_SET(check_for_higher_flux_peaks__set_corresponding_peak_energy) THEN BEGIN
        ;;Figure out where most energetic maximum is
        max_ys                   = GET_N_MAXIMA_IN_ARRAY(Y,N=3,OUT_I=maxima_i)
        peak_y                   = MAX(max_ys,max_y_ii)
        peak_ind                 = maxima_i[max_y_ii]
        peak_energy              = Xorig[peak_ind]
        peak_tol_percent         = .25
        
        FOR i=0,N_ELEMENTS(maxima_i)-1 DO BEGIN
           testMax_X             = Xorig[maxima_i[i]]
           testMax               = max_ys[i]
           curMax_i              = maxima_i[i]
           PRINT,'testval:',STRCOMPRESS((ABS(testMax-peak_y)/peak_y),/REMOVE_ALL)
           IF testMax_X GT Xorig[peak_ind] AND (ABS(testMax-peak_y)/peak_y) LT peak_tol_percent THEN BEGIN
              peak_ind           = curMax_i
              peak_energy        = testMax_X
           ENDIF
        ENDFOR
        PRINT,peak_ind
     ENDIF ELSE BEGIN
        max_y                    = MAX(Y,peak_ind)
        peak_ind                -= b_offset
        peak_energy              = Xorig[peak_ind]
        PRINT,peak_ind
     ENDELSE

     nEnergies                   = N_ELEMENTS(Xorig)
     minEInd                     = (peak_ind - n_below_peak) > 0
     maxEInd                     = (peak_ind + n_after_peak) < nEnergies-1

     Y                           = REVERSE(REFORM(eSpec.y[bounds[i],*]))

     ;;estimate from the data!
     IF KEYWORD_SET(estimate_A_from_data) THEN BEGIN 

        t                     = times[bounds[i]]

        IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
           dat                = CALL_FUNCTION(routine,t,CALIB=calib,NPTS=spectra_average_interval)
           dat                = AVERAGE_SUM3D(dat,spectra_average_interval)
        ENDIF ELSE BEGIN
           dat                = CALL_FUNCTION(routine,t,CALIB=calib)
        ENDELSE

        ;; min_energy            = peak_energy 
        ;; min_energy            =  Xorig[( (peak_ind-n_below_peak) > 0 ) ]
        min_energy            =  Xorig[(minEInd - 2) > 0]
        max_energy            =  Xorig[(maxEInd + 2) < (nEnergies - 1)]

        ;; eRange                = [min_energy,30000]
        eRange                = [min_energy,max_energy]

        bulk_energy           = peak_energy
        ;; T                     = (T_2D_FS(dat,energy=eRange))[3] ;T_avg
        T                     = (T_2D_FS(dat,ENERGY=eRange,ANGLE=e_angle))[3] ;T_avg
        T                     = (T_2D_FS(dat,ENERGY=eRange,ANGLE=e_angle))[3] ;T_avg
        ;; n_est                 = N_2D_FS(dat,ENERGY=eRange,ANGLE=e_angle)/20.
        n_est                 = N_2D_FS(dat,ENERGY=eRange,ANGLE=e_angle)*5.
        A                     = DOUBLE([bulk_energy,T,kappa,n_est,dt[i],mass]) 
        
        PRINT,"Here's my initial estimate based on spectral properties: "
        PRINT_KAPPA_FLUX_FIT_PARAMS,A
        PRINT,''

     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        weights    = SQRT(ABS(Y))
        
        bulk_energy           = peak_energy

        TGauss                = (T_2D_FS(dat,ENERGY=eRange,ANGLE=e_angle))[3]/2. ;T_avg
        ;; n_estGauss            = n_2d_fs(dat,ENERGY=eRange,ANGLE=e_angle)/20.
        n_estGauss            = n_2d_fs(dat,ENERGY=eRange,ANGLE=e_angle)*5.
        
        kappaGauss            = 100

        AGauss                = DOUBLE([bulk_energy,TGauss,kappaGauss,n_estGauss,dt[i],mass])

        PRINT,"Here's my initial Gaussian estimate based on spectral properties: "
        PRINT_KAPPA_FLUX_FIT_PARAMS,AGauss
        PRINT,''
     ENDIF

     ENDIF ELSE BEGIN
        A                     = DOUBLE([peak_energy,T,kappa,n_est])
     ENDELSE
     


     title     = STRING(FORMAT='("Loss-cone e!U-!N # flux, (Orbit ",I0,", ",A0,")")', $
                        orbStr, $
                        STRMID(TIME_TO_STR(je_en.x[bounds[i]]),0,10))
     xTitle    = "Energy (eV)"
     ;; yTitle    = "Losscone Number flux (#/cm!U2!N-s)"
     yTitle    = "Differential Energy Flux!C(eV/cm!U2!N-sr-s)"

     xRange    = [MIN(Xorig[WHERE(Xorig GT 0)]),MAX(Xorig)]

     ;; yRange    = [yMin,MAX(je_en.y)]
     yRange    = [yMin,MAX(eSpec.y)]

     orbDate   = STRMID(TIME_TO_STR(je_en.x[bounds[i]]),0,10)

     IF KEYWORD_SET(spectra_average_interval) THEN BEGIN
        avgStr = STRING(FORMAT='("--",I0,"_avgs")',spectra_average_interval)
     ENDIF ELSE BEGIN
        avgStr = ''
     ENDELSE
     plotSN    = STRING(FORMAT='("nFlux_fit--",A0,"--",A0,A0,"--orb_",I0,"__",A0,".png")', $
                        timeFNStrs[bounds[i]], $
                        eeb_or_ees, $
                        avgStr, $
                        orbStr, $
                        orbDate)

     ;;plot things
     nPlots    = 2+KEYWORD_SET(add_gaussian_estimate)+KEYWORD_SET(add_oneCount_curve) ;Bud and me
     window    = WINDOW(DIMENSION=[800,600])
     plotArr   = MAKE_ARRAY(nPlots,/OBJ) 

     colorList = LIST('RED','BLACK','BLUE','GRAY')

     plotArr[0] = PLOT(Xorig, $     ;x, $
                       Y, $
                       TITLE=title, $
                       NAME=STRMID(TIME_TO_STR(je_en.x[bounds[i]],/MSEC),11,12), $
                       XTITLE=xTitle, $
                       YTITLE=yTitle, $
                       XRANGE=xRange, $
                       YRANGE=yRange, $
                       YLOG=1, $
                       XLOG=1, $
                       THICK=2.2, $
                       COLOR=colorList[0], $
                       ;; OVERPLOT=i GT 0, $
                       CURRENT=window) 
     
     
     ;;Trim energies vector if attempting to fit below peak
     IF KEYWORD_SET(trim_energies_below_peak) THEN BEGIN 

        IF N_ELEMENTS(n_below_peak) EQ 0 THEN n_below_peak = 4
        IF N_ELEMENTS(n_after_peak) EQ 0 THEN n_after_peak = 10

        X                  = Xorig[minEInd:maxEInd-1] 
        Y                  = Y[minEInd:maxEInd-1] 
     ENDIF

     ;; KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322,X,A,yFit,pders, $
     ;;                                       /CMSQ_S_UNITS

     weights    = 1./SQRT(ABS(Y))
     yFit = CURVEFIT(X, Y, weights, A, SIGMA, FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F' , $
                     /DOUBLE, $
                     FITA=[1,1,0,1,0,0], $
                     ITMAX=KEYWORD_SET(max_iter) ? max_iter : 150, $
                     ITER=itNum, $
                     TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                     STATUS=fitStatus)

     ;;need to adjust Y bounds?
     yRange[1]  = MAX(yFit) > yRange[1]

     PRINT,"Fitted spectral properties: "
     PRINT_KAPPA_FLUX_FIT_PARAMS,A
     PRINT,''

     plotArr[1] = PLOT(X, $     ;x, $
                       yFit, $
                       TITLE=title, $
                       NAME="Fitted spectrum", $
                       XTITLE=xTitle, $
                       YTITLE=yTitle, $
                       XRANGE=xRange, $
                       YRANGE=yRange, $
                       YLOG=1, $
                       XLOG=1, $
                       THICK=2.2, $
                       LINESTYLE='--', $
                       COLOR=colorList[1], $
                       /OVERPLOT, $
                       CURRENT=window) 

     CASE fitStatus OF 
        0: BEGIN 
           PRINT,'Fit success!' 
           failMe     = 0 
        END 
        1: BEGIN 
           PRINT,'Fit failure! Chi-square increasing without bound!' 
           failMe     = 1 
        END 
        2: BEGIN 
           PRINT,'Fit failure! No convergence in ' + STRCOMPRESS(itNum,/REMOVE_ALL) + ' iterations!' 
           failMe     = 1 
        END 
     ENDCASE

     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        weights    = SQRT(ABS(Y))
        
        bulk_energy           = peak_energy
        
        yGaussFit  = CURVEFIT(X, Y, weights, AGauss, SIGMA, FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322__CONV_TO_F' , $
                              /DOUBLE, $
                              FITA=[1,1,0,1,0,0], $
                              ITMAX=KEYWORD_SET(max_iter) ? max_iter : 150, $
                              ITER=itNum, $
                              TOL=KEYWORD_SET(fit_tol) ? fit_tol : 1e-3, $
                              STATUS=gaussFitStatus)
        
        ;; KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322,X,AGauss,yGaussFit,pders, $
        ;;                                       /CMSQ_S_UNITS

        ;;need to adjust Y bounds?
        yRange[1]  = MAX(yGaussFit) > yRange[1]
        
        PRINT,"Gaussian fitted spectral properties: "
        PRINT_KAPPA_FLUX_FIT_PARAMS,AGauss
        PRINT,''
        
        plotArr[2] = PLOT(X, $
                          yGaussFit, $
                          TITLE=title, $
                          NAME="Gaussian Fitted spectrum", $
                          XTITLE=xTitle, $
                          YTITLE=yTitle, $
                          XRANGE=xRange, $
                          YRANGE=yRange, $
                          YLOG=1, $
                          XLOG=1, $
                          THICK=2.2, $
                          LINESTYLE='-.', $
                          COLOR=colorList[2], $
                          /OVERPLOT, $
                          CURRENT=window) 
        

        CASE gaussFitStatus OF 
           0: BEGIN 
              PRINT,'GaussFit success!' 
              failMe     = 0 
           END 
           1: BEGIN 
              PRINT,'GaussFit failure! Chi-square increasing without bound!' 
              failMe     = 1 
           END 
           2: BEGIN 
              PRINT,'GaussFit failure! No convergence in ' + STRCOMPRESS(itNum,/REMOVE_ALL) + ' iterations!' 
              failMe     = 1 
           END 
        ENDCASE
     ENDIF

     IF KEYWORD_SET(add_oneCount_curve) THEN BEGIN

        plotArr[3] = PLOT(Xorig, $
                          REVERSE(REFORM(oneCurve.y[bounds[i],*])), $
                          NAME="One Count", $
                          THICK=2.2, $
                          LINESTYLE='-:', $
                          COLOR=colorList[3], $
                          /OVERPLOT, $
                          CURRENT=window) 

     ENDIF
        
     legend           = LEGEND(TARGET=plotArr[*],POSITION=[0.55,0.85],/NORMAL)
     IF KEYWORD_SET(add_fitParams_text) THEN BEGIN
        fitTitle      = ["Bulk energy (eV)","Plasma temp. (eV)","Kappa","Density (cm^-3)"]
        fitInfoStr    = [STRING(FORMAT='(F-15.2)',A[0]), $
                         STRING(FORMAT='(F-15.2)',A[1]), $
                         STRING(FORMAT='(F-7.3)',A[2]), $
                         STRING(FORMAT='(F-8.4)',A[3])]

        fitParamsText = TEXT(0.2,0.25, $
                             STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                             STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                             STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                             STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                             STRING(FORMAT='("Fit success",T20,": ",A0)',(fitStatus EQ 0 ? 'Y' : 'N')), $
                             FONT_SIZE=10, $
                             FONT_NAME='Courier', $
                             /NORMAL)

     ENDIF

     IF KEYWORD_SET(add_gaussian_estimate) THEN BEGIN
        fitTitle      = ["Bulk energy (eV)","Plasma temp. (eV)","Kappa","Density (cm^-3)"]
        fitInfoStr    = [STRING(FORMAT='(F-15.2)',AGauss[0]), $
                         STRING(FORMAT='(F-15.2)',AGauss[1]), $
                         STRING(FORMAT='(F-7.3)',AGauss[2]), $
                         STRING(FORMAT='(F-8.4)',AGauss[3])]

        fitParamsText = TEXT(0.52,0.25, $
                             STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[0],fitInfoStr[0]) + '!C' + $
                             STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[1],fitInfoStr[1]) + '!C' + $
                             STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[2],fitInfoStr[2]) + '!C' + $
                             STRING(FORMAT='(A0,T20,": ",A0)',fitTitle[3],fitInfoStr[3]) + '!C' + $
                             STRING(FORMAT='("GaussFit success",T20,": ",A0)',(gaussFitStatus EQ 0 ? 'Y' : 'N')), $
                             FONT_SIZE=10, $
                             FONT_NAME='Courier', $
                             /NORMAL, $
                             FONT_COLOR=colorList[2])
     ENDIF

     IF KEYWORD_SET(save_fitplots) THEN BEGIN
        PRINT,'Saving plot to ' + plotSN + '...'
        WINDOW.save,plotSN
        WINDOW.close
     ENDIF

  ENDFOR
;;      IF ~failMe THEN BEGIN

;;      ENDIF ELSE BEGIN
;;         PRINT,'No fit to plot ...'
;;      ENDELSE

END