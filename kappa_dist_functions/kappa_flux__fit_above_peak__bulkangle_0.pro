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

PRO KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0, $;X,A,F,pders, $
   T1=t1, $
   T2=t2, $
   SDT_TIME_INDS=bounds, $
   DENSITY_EST=n_est, $
   TEMPERATURE_EST=T, $
   KAPPA_EST=kappa, $
   SDT_DAT=dat, $
   ESTIMATE_FITPARAMS_FROM_SDT_DAT=estimate_A_from_data, $
   TRIM_ENERGIES_BELOW_PEAK=trim_energies_below_peak, $
   N_ENERGIES_BELOW_PEAK=n_below_peak, $
   N_ENERGIES_AFTER_PEAK=n_after_peak, $
   ADD_FITPARAMS_TEXT=add_fitParams_text, $
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
     ;; @startup
  ENDIF

  ;; IF KEYWORD_SET(estimate_A_from_data) AND N_ELEMENTS(dat) EQ 0 THEN BEGIN
  ;;    PRINT,'Requested that we use SDT data to estimate, but no SDT data provided! Pulling it in...'
  ;;    @startup
  ;;    timeStr = '99-3-2/18:08:42'
  ;;    t=str_to_time(timeStr) 
  ;;    dat = get_fa_ees(t)        ; get electron esa survey

  ;; ENDIF


  IF N_ELEMENTS(eSpec) EQ 0 THEN BEGIN
     GET_LOSSCONE_EN_SPEC_AND_NFLUX_DATA,T1=t1,T2=t2, $
                                         EN_SPEC=eSpec, $
                                         JE_EN=je_en
     ;; GET_EN_SPEC,'fa_ees',UNITS=eSpecUnits,NAME='el',RETRACE=1,T1=t1,T2=t2,ANGLE=e_angle
     ;; ;;GET the spectrogram data struct
     ;; GET_DATA,'el',data=eSpec
  ENDIF

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
     bounds      = 15           ;minute 10
  ENDIF

  ;;Loop over provided indices, plot data as well as fit, and optionally save
  FOR i=0,N_ELEMENTS(bounds)-1 DO BEGIN

     X           = REVERSE(REFORM(eSpec.v[bounds[i],*]))
     Y           = REVERSE(REFORM(je_en.y[bounds[i],*]))

     max_y                    = MAX(y,peak_ind)
     peak_energy              = X[peak_ind]

     ;;estimate from the data!
     IF KEYWORD_SET(estimate_A_from_data) THEN BEGIN 

        t                     = times[bounds[i]]
        dat                   = get_fa_ees(t)     ; get electron esa survey

        min_energy            = peak_energy 
        ;; min_energy            = 50 

        ;; bulk_energy           = (V_2D_FS(dat,energy=[min_energy,30000]))[2] 
        ;; bulk_energy           = 9.1e-31*(bulk_energy*1000)^2/2/1.6e-19       ;in eV
        bulk_energy           = peak_energy
        T                     = (T_2D_FS(dat,energy=[min_energy,30000]))[3] ;T_avg
        n_est                 = n_2d_fs(dat,energy=[min_energy,30000])/3    ; print density >100 eV, #/cm3
        ;; print,v_2d_fs(dat,ENERGY=[min_energy,30000])                        ; print Vx,Vy,Vz, km/s
        ;; print,p_2d_fs(dat,ENERGY=[min_energy,30000])                        ; print Pxx,Pyy,Pzz,Pxy,Pxz,Pyz, eV/cm^3
        ;; print,t_2d_fs(dat,ENERGY=[min_energy,30000])                        ; print Tx,Ty,Tz,Tavg, eV

        
        A                     = DOUBLE([bulk_energy,T,kappa,n_est]) 
        
        PRINT,"Here's my initial estimate based on spectral properties: "
        PRINT_KAPPA_FLUX_FIT_PARAMS,A
        
     ENDIF ELSE BEGIN
        A                     = DOUBLE([peak_energy,T,kappa,n_est])
     ENDELSE
     
     title     = STRING(FORMAT='("Loss-cone e!U-!N # flux, (Orbit ",I0,", ",A0)', $
                        orb, $
                        STRMID(TIME_TO_STR(je_en.x[bounds[i]]),0,10))
     xTitle    = "Energy (eV)"
     yTitle    = "Losscone Number flux (#/cm!U2!N-s)"
     ;; xRange    = [eSpec.v[bounds[0],-1],eSpec.v[bounds[0],0]]
     xRange    = [MIN(X[WHERE(X GT 0)]),MAX(X)]
     yRange    = [MIN(je_en.y[bounds[i],WHERE(je_en.y[bounds[i],*] GT 0)]),MAX(je_en.y)]

     orbDate   = STRMID(TIME_TO_STR(je_en.x[bounds[i]]),0,10)
     plotSN    = STRING(FORMAT='("nFlux_fit--",A0,"--orb_",I0,"__",A0,".png")', $
                        timeFNStrs[bounds[i]], $
                        orb, $
                        orbDate)

     ;;plot things
     nPlots    = 2              ;Bud and me
     window    = WINDOW(DIMENSION=[800,600])
     plotArr   = MAKE_ARRAY(nPlots,/OBJ) 
     ;; colorList = GENERATE_LIST_OF_RANDOM_COLORS(nPlots) 
     colorList = LIST('RED','BLACK')

     plotArr[0] = PLOT(X, $     ;x, $
                       Y, $
                       TITLE=title, $
                       NAME=STRMID(TIME_TO_STR(je_en.x[bounds[i]]),11,9), $
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

        nEnergies          = N_ELEMENTS(X)

        ;; keepme             = WHERE(X GE peak_energy,nKeeps)
        ;; keepme             = keepme[0:(n_after_peak < nKeeps)-1]
        ;; minKeep            = (keepme[0] - n_below_peak) > 0
        ;; maxKeep            = (n_after_peak < nKeeps)-1
        ;; keepme             = [keepme[0]-4,keepme[0]-3,keepme[0]-2,keepme[0]-1,keepme] 
        ;; X                  = X[keepme] 
        ;; Y                  = Y[keepme] 

        minKeep            = (peak_ind - n_below_peak) > 0
        maxKeep            = (peak_ind + n_after_peak) < nEnergies
        X                  = X[minKeep:maxKeep-1] 
        Y                  = Y[minKeep:maxKeep-1] 
     ENDIF

     ;; KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322,X,A,yFit,pders, $
     ;;                                       /CMSQ_S_UNITS

     weights    = SQRT(ABS(Y))
     yFit = CURVEFIT(X, Y, weights, A, SIGMA, FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322' , $
                     /DOUBLE, $
                     ITMAX=20, $
                     ITER=itNum, $
                     ;; TOL=1e-3, $
                     STATUS=fitStatus)

     ;;need to adjust Y bounds?
     yRange[1]  = MAX(yFit) > yRange[1]

     PRINT_KAPPA_FLUX_FIT_PARAMS,A

     plotArr[1] = PLOT(X, $     ;x, $
                       yFit, $
                       TITLE=title, $
                       ;; NAME=STRMID(TIME_TO_STR(je_en.x[bounds[i]]),11,9), $
                       NAME="Fitted spectrum", $
                       XTITLE=xTitle, $
                       YTITLE=yTitle, $
                       XRANGE=xRange, $
                       YRANGE=yRange, $
                       YLOG=1, $
                       XLOG=1, $
                       THICK=2.2, $
                       COLOR=colorList[1], $
                       /OVERPLOT, $
                       CURRENT=window) 


     legend           = LEGEND(TARGET=plotArr[*],POSITION=[0.45,0.45],/NORMAL)
     IF KEYWORD_SET(add_fitParams_text) THEN BEGIN
        fitTitle      = STRING(FORMAT='("Bulk energy (eV)",T20,"Plasma temp. (eV)",T40,"Kappa",T50,"Density (cm^-3)",A0)','')
        fitInfoStr    = STRING(FORMAT='(F-15.2,T20,F-15.2,T40,F-7.3,T50,F-8.4)', $
                               A[0], $
                               A[1], $
                               A[2], $
                               A[3])

        fitParamsText = TEXT(0.3,0.3, $
                                fitTitle + '!C' + fitInfoStr, $
                                FONT_SIZE=14, $
                                FONT_NAME='Helvetica', $
                                /NORMAL)
     ENDIF

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
           PRINT,'Fit failure! No convergence in ' + STRCOMPRESS(nIter,/REMOVE_ALL) + ' iterations!' 
           failMe     = 1 
        END 
     ENDCASE

     IF KEYWORD_SET(save_fitplots) THEN BEGIN
        PRINT,'Saving plot to ' + saveFitName + '...'
        WINDOW.save,saveFitName
        WINDOW.close
     ENDIF

  ENDFOR
;;      IF ~failMe THEN BEGIN

;;      ENDIF ELSE BEGIN
;;         PRINT,'No fit to plot ...'
;;      ENDELSE

END