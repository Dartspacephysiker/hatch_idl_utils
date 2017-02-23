;;2017/02/21
FUNCTION MOMENTERRORS_2D,dat2, $
                         ENERGY=en, $
                         ERANGE=er, $
                         EBINS=ebins, $
                         ANGLE=an, $
                         ARANGE=ar, $
                         BINS=bins;; , $
                         ;; CONV_TO_CM=conv_to_cm

  COMPILE_OPT IDL2

  IF dat2.valid EQ 0 THEN BEGIN
     PRINT,'Invalid Data'
     RETURN, 0.
  ENDIF

  dat = CONV_UNITS(dat2,'dfStd') ;distribution function, in "standard" units (s^3/m^6)
  na  = dat.nenergy
  nb  = dat.nbins
  
  EBINS2 = REPLICATE(1b,na)
  IF KEYWORD_SET(en) THEN BEGIN
     ebins2[*] = 0
     er2 = [ENERGY_TO_EBIN(dat,en)]
     IF ER2[0] GT er2[1] THEN er2 = REVERSE(er2)
     ebins2[er2[0]:er2[1]] = 1
  ENDIF
  IF KEYWORD_SET(er) THEN BEGIN
     ebins2[*] = 0
     er2 = er
     IF er2[0] GT er2[1] THEN er2 = REVERSE(er2)
     ebins2[er2[0]:er2[1]] = 1
  ENDIF
  IF KEYWORD_SET(ebins) THEN ebins2 = ebins

  abins = REPLICATE(1b,nb)
  IF KEYWORD_SET(an) THEN BEGIN
     IF NDIMEN(an) NE 1 OR DIMEN1(an) NE 2 THEN BEGIN
        print,'Error - angLE keyword must be fltarr(2)'
     ENDIF ELSE BEGIN
        abins = ANGLE_TO_BINS(dat,an)
     ENDELSE
  ENDIF
  IF KEYWORD_SET(ar) THEN BEGIN
     abins[*] = 0
     IF ar[0] GT ar[1] THEN BEGIN
        abins[ar[0]:nb-1] = 1
        abins[0:ar[1]] = 1
     ENDIF ELSE BEGIN
        abins[ar[0]:ar[1]] = 1
     ENDELSE
  ENDIF
  IF KEYWORD_SET(bins) THEN bins2 = bins

  IF NDIMEN(bins2) ne 2 THEN bins2 = ebins2#abins

  bins2_i  = WHERE(bins2)
  ebins_i  = WHERE(ebins2,nEBins)
  abins_i  = WHERE(abins,nABins)

  data     = REFORM(dat.data[bins2_i],nEBins,nABins)
  ddata    = REFORM(dat.ddata[bins2_i],nEBins,nABins)

  energy   = dat.energy[ebins_i]
  denergy  = dat.denergy[ebins_i]
  theta    = REFORM(dat.theta[0,abins_i])
  dtheta   = REFORM((dat.dtheta[0,abins_i]))
  ;; mass     = dat.mass * 1.6e-22
  ;; Const    = 1.6e-12

  ;; denergy  = dat.denergy

  species  = dat.mass GT 6e-6 ;0=electron, 1=ion; electron mass is 5.68566e-06 eV/c^2 (c in km/s)

  ;; errThing = PLASMA_MOMENTERRORS_2D(data,ddata,species,energy,theta, $ ;phi, $
                                    ;; DENERGY=dEnergy, $
                                    ;; DTHETA=dTheta, $
                                    ;; PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                    ;; HEATFLUX_COVAR_CALC=heatFlux_covar_calc, $
                                    ;; /SANS_PHI, $
                                    ;; CONV_TO_CM=conv_to_cm)
  errThing = PLASMA_MOMENTERRORS__GERSHMAN(data,ddata,species,energy,theta, $ ;phi, $
                                    DENERGY=dEnergy, $
                                    DTHETA=dTheta, $
                                    PRESSURE_COVAR_CALC=pressure_covar_calc, $
                                    HEATFLUX_COVAR_CALC=heatFlux_covar_calc, $
                                    /SANS_PHI);; , $
                                    ;; CONV_TO_CM=conv_to_cm



  ;; IF (theta[0,0] EQ theta[na-1,0]) THEN nna = 0 ELSE nna = na-1
  ;; IF NDIMEN(dtheta) EQ 1 THEN dtheta = REPLICATE(1.,na)#dtheta
  ;; domega = theta
  ;; FOR a=0,nna DO BEGIN
  ;;    FOR b=0,nb-1 DO BEGIN
  ;;       IF (ABS(theta[a,b]-!pi) LT dtheta[a,b]/2.) THEN BEGIN 
  ;;          th1 = (!pi+theta[a,b]-dtheta[a,b]/2.)/2.
  ;;          dth1 = [!pi-th1]
  ;;          th2 = (!pi+theta[a,b]+dtheta[a,b]/2.)/2.
  ;;          dth2 = [th2-!pi]
  ;;          domega[a,b]=2.*!pi*(ABS(SIN(th1))*SIN(dth1)*COS(th1)*COS(dth1) + ABS(SIN(th2))*SIN(dth2)*COS(th1)*COS(dth2)) 
  ;;       ENDIF ELSE IF (ABS(theta[a,b]-2*!pi) LT dtheta[a,b]/2.) THEN BEGIN
  ;;          th1 = (2.*!pi+theta[a,b]-dtheta[a,b]/2.)/2.
  ;;          dth1 = [2.*!pi-th1]
  ;;          th2 = (2.*!pi+theta[a,b]+dtheta[a,b]/2.)/2.
  ;;          dth2 = [th2-2.*!pi]
  ;;          domega[a,b]=2.*!pi*(ABS(SIN(th1))*SIN(dth1)*COS(th1)*COS(dth1) + ABS(SIN(th2))*SIN(dth2)*COS(th1)*COS(dth2)) 
  ;;       ENDIF ELSE IF (ABS(theta[a,b]) LT dtheta[a,b]/2.) THEN BEGIN
  ;;          th1 = (theta[a,b]-dtheta[a,b]/2.)/2.
  ;;          dth1 = ABS(th1)
  ;;          th2 = (theta[a,b]+dtheta[a,b]/2.)/2.
  ;;          dth2 = [th2]
  ;;          domega[a,b]=2.*!pi*(ABS(SIN(th1))*SIN(dth1)*COS(th1)*COS(dth1) + ABS(SIN(th2))*SIN(dth2)*COS(th1)*COS(dth2)) 
  ;;       ENDIF ELSE BEGIN
  ;;          th1 = theta[a,b]
  ;;          dth1 = dtheta[a,b]/2.
  ;;          domega[a,b]=2.*!pi*ABS(SIN(th1))*SIN(dth1)*(COS(th1)*COS(dth1))
  ;;       ENDELSE
  ;;    ENDFOR
  ;; ENDFOR
  ;; IF (nna EQ 0) THEN FOR a=1,na-1 DO domega[a,*] = domega[0,*]

  ;; sumdataz = TOTAL(data*domega,2)
  ;; flux3dz = TOTAL((denergy*(energy^(-1)))*sumdataz)

; units are #/cm^2-sec

  RETURN,errThing
end

