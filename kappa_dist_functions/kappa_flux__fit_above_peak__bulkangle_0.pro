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
PRO KAPPA_FLUX__FIT_ABOVE_PEAK__BULKANGLE_0,X,A,F,pders, $
   ;; SDT_JFLUX=sdt_jFlux, $
   SDT_FIELDALIGNED_N_EST=n_est, $
   PEAK_ENERGY=peak_energy, $
   TEMPERATURE=T

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;From Kivelson & Russell, Table 2.2 "Properties of Typical Plasmas"
  ;;"Magnetosphere"
  T_def                 = 1000L    ;eV
  n_def                 = 1L       ;cm^-3
  E_b_def               = 500L     ;bulk energy, eV

  IF N_ELEMENTS(kappa)       EQ 0 THEN kappa        = 5        ;a guess
  IF N_ELEMENTS(T)           EQ 0 THEN T            = T_def    ;temperature guess in eV
  IF N_ELEMENTS(n_est)       EQ 0 THEN n_est        = n_def    ;n guess in cm^-3
  IF N_ELEMENTS(peak_energy) EQ 0 THEN peak_energy  = E_b_def  ;peak energy guess, eV    

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;From Orb 10000 at 18:08:46
  inDir    = '/SPENCEdata/software/sdt/batch_jobs/20160420--fit_Maxwellians_kappas_for_inverted_Vs/'
  restFile = inDir + 'nFlux_and_eSpec--orb_10000__18_08_36-18_09_00.sav'
  RESTORE,restFile

  bounds    = 15 ;minute 10
  X         = REVERSE(REFORM(eSpec.v[bounds[0],*]))
  Y         = REVERSE(REFORM(je_en.y[bounds[0],*]))

  trim_energies_below_peak = 1
  max_y                 = MAX(y,maxInd)
  ;; peak_energy           = 344.95999
  peak_energy           = x[maxInd-1]
  ;; bulk_energy           = MEAN(x[maxInd:-1]*dn_2d[maxInd:-1])/TOTAL(dn_2d[maxInd:-1]) 
  ;; bulk_energy           = MEAN(x[maxInd:-1])
  bulk_energy           = 1000
  ;; n_est                 = TOTAL((REVERSE(dn_2d))[maxInd:-1]) 
  n_est                 = 0.007
  kappa                 = 3.0 ;Why not?
  ;;Param vector
  A                     = DOUBLE([peak_energy,T,kappa,n_est])

  title     = STRING(FORMAT='("Loss-cone e!U-!N spectra from Orbit ",I0,", ",A0)',orb,STRMID(TIME_TO_STR(je_en.x[bounds[0]]),0,10))
  xTitle    = "Energy (eV)"
  yTitle    = "Losscone Number flux (#/cm!U2!N-s)"
  ;; xRange    = [eSpec.v[bounds[0],-1],eSpec.v[bounds[0],0]]
  xRange    = [MIN(x[WHERE(x GT 0)]),MAX(x)]
  yRange    = [MIN(je_en.y[bounds[0],WHERE(je_en.y[bounds[0],*] GT 0)]),MAX(je_en.y)]

  orbDate   = STRMID(TIME_TO_STR(je_en.x[bounds[0]]),0,10)
  plotSN    = STRING(FORMAT='("nFlux_fit--orb_",I0,"__",A0,".png")',orb,orbDate)

  ;;plot things
  nPlots    = 2 ;Bud and me
  window    = WINDOW(DIMENSION=[800,600])
  plotArr   = MAKE_ARRAY(nPlots,/OBJ) 
  colorList = GENERATE_LIST_OF_RANDOM_COLORS(nPlots) 

  plotArr[0] = PLOT(X, $        ;x, $
                    Y, $
                    TITLE=title, $
                    NAME=STRMID(TIME_TO_STR(je_en.x[bounds[0]]),11,9), $
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
     keepme             = (WHERE(X GE peak_energy))[0:4]
     X                  = X[keepme]
     Y                  = Y[keepme]
  ENDIF

  ;; KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322,X,A,yFit,pders, $
  ;;                                       /CMSQ_S_UNITS

  yFit = CURVEFIT(X, Y, weights, A, SIGMA, FUNCTION_NAME='KAPPA_FLUX__LIVADIOTIS_MCCOMAS_EQ_322' , $
                  /DOUBLE, $
                  STATUS=fitStatus)
  yRange[1]  = MAX(yFit)

  plotArr[1] = PLOT(x, $        ;x, $
                    yFit, $
                    TITLE=title, $
                    NAME=STRMID(TIME_TO_STR(je_en.x[bounds[0]]),11,9), $
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

     IF ~failMe THEN BEGIN

     ENDIF ELSE BEGIN
        PRINT,'No fit to plot ...'
     ENDELSE

END