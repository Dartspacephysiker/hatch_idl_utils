;+
;FUNCTION:	j_2d_b(dat,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
;INPUT:
;	dat:	structure,	2d data structure filled by get_eesa_surv, get_eesa_burst, etc.
;KEYWORDS
;	ENERGY:	fltarr(2),	optional, min,max energy range for integration
;	ERANGE:	fltarr(2),	optional, min,max energy bin numbers for integration
;	EBINS:	bytarr(na),	optional, energy bins array for integration
;					0,1=exclude,include,
;					na = dat.nenergy
;	ANGLE:	fltarr(2),	optional, min,max pitch angle range for integration
;	ARANGE:	fltarr(2),	optional, min,max angle bin numbers for integration
;	BINS:	bytarr(nb),	optional, angle bins array for integration
;					0,1=exclude,include,
;					nb = dat.ntheta
;	BINS:	bytarr(na,nb),	optional, energy/angle bins array for integration
;					0,1=exclude,include
;PURPOSE:
;	Returns the solid angle for field aligned fluxes, for each energy, AND assumes a narrow (< 5 deg) field aligned beam
;       You ask, "But why?" Well, for example, if you wanted to pick up the field-aligned number flux for these bins, select
;       your angle range and do the following:
;  sumdataz = total(data*domega,2)       ;data and domega are 48x64 corresponding to energy and theta, so this gives a sum over thetas
;  flux3dz_en = (denergy*(energy^(-1)))*sumdataz
; units are then #/cm^2-sec




  ;;
;NOTES:
;	Similar to j_2d.pro, treats the anodes within 5 deg of the magnetic field differently.
;
;
;
;CREATED BY:
;	S.Hatch         2016-07-15		Ripped off j_2d_b.pro
;
;LAST MODIFICATION:
;
;-
FUNCTION CALC_2D_SOLID_ANGLE_B,dat2, $
                               ENERGY=en, $
                               ERANGE=er, $
                               EBINS=ebins, $
                               ANGLE=an, $
                               ARANGE=ar, $
                               BINS=bins

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF dat2.valid EQ 0 THEN BEGIN
     PRINT,'Invalid Data'
     RETURN, -1.
  ENDIF

  dat                       = CONV_UNITS(dat2,"eflux") ; Use Energy Flux
  na                        = dat.nenergy
  nb                        = dat.nbins

  ebins2                    = REPLICATE(1b,na)
; User-provided energy range?
  IF KEYWORD_SET(en) THEN BEGIN
     ebins2[*]              = 0
     er2                    = [ENERGY_TO_EBIN(dat,en)]
     IF er2[0] GT er2[1] THEN BEGIN
        er2                 = REVERSE(er2)
     ENDIF
     ebins2[er2[0]:er2[1]]  = 1
  ENDIF
  IF KEYWORD_SET(er) THEN BEGIN
     ebins2[*]              = 0
     er2                    = er
     IF er2[0] GT er2[1] THEN BEGIN
        er2                 = REVERSE(er2)
     ENDIF
     ebins2[er2[0]:er2[1]]  = 1
  ENDIF
; User-provided energy bins?
  IF KEYWORD_SET(ebins) THEN BEGIN
     ebins2                 = ebins
  ENDIF

  bins2                     = REPLICATE(1b,nb)

; User-provided angle-bin range?
  IF KEYWORD_SET(ar) THEN BEGIN
     bins2[*]               = 0
     IF ar[0] GT ar[1] THEN BEGIN
        bins2[ar[0]:nb-1]   = 1
        bins2[0:ar[1]]      = 1
     ENDIF ELSE BEGIN
        bins2[ar[0]:ar[1]]  = 1
     ENDELSE
  ENDIF
; User-provided bins?
  IF KEYWORD_SET(bins) THEN BEGIN
     bins2                  = bins
  ENDIF

  IF NDIMEN(bins2) NE 2 THEN BEGIN
     bins2                  = ebins2#bins2
  ENDIF

  data                      = dat.data*bins2
  energy                    = dat.energy
  denergy                   = dat.denergy
  theta                     = dat.theta
  dtheta                    = dat.dtheta
  IF NDIMEN(dtheta) EQ 1 THEN BEGIN
     dtheta                 = REPLICATE(1.,na)#dtheta
  ENDIF
  mass                      = dat.mass * 1.6e-22
  Const                     = 1.6e-12
  esa_dth                   = 5. < !PI*MIN(dtheta)/4.

  minvar                    = MIN(theta[0,*],indminvar)
  IF indminvar GT 1 THEN BEGIN
     an_shift               = theta(0,0) LT theta(0,1)
  ENDIF ELSE an_shift       = theta(0,2) LT theta(0,3)
  an_shift                  = 2*an_shift-1

; Provided pitch angle range?
  IF KEYWORD_SET(an) THEN BEGIN
     ann                    = (360.*(an/360.-floor(an/360.)))
     IF an[1] EQ 360. THEN BEGIN
        ann[1]              = 360.
     ENDIF
  ENDIF ELSE BEGIN
     ann                    = [0.,360.]
  ENDELSE
  IF ann[0] GT ann[1] THEN BEGIN
     ann                    = REVERSE(ann)
     tfrev                  = 1
  ENDIF ELSE BEGIN
     tfrev                  = 0
  ENDELSE

  ;; Calculate solid angle for 0.<th<180.
  th2_tmp                   = theta + dtheta/2.
  th2_tmp                   = (360.*(th2_tmp/360.-floor(th2_tmp/360.)))
  th1_tmp                   = th2_tmp - dtheta
  th1                       = th1_tmp > ann[0] < ann[1]
  th1                       = th1 > 0. < 180.
  th2                       = th2_tmp > ann[0] < ann[1]
  th2                       = th2 > 0. < 180.
  th_plus                   = (th1 LT esa_dth) AND (th1 NE th2)      ;This is the part that treats field-aligned anodes differently
  th_minus                  = (th2 GT 180.-esa_dth) AND (th1 NE th2) ;and here
  th_other                  = 1 - th_plus - th_minus                 ;and here
  sin_sq                    = .5*!PI*((SIN(th2/!RADEG))^2 - (SIN(th1/!RADEG))^2)
  sin_other                 = th_other*sin_sq
  sin_plus                  = th_plus*(esa_dth*(th2-th1)/(!RADEG)^2 < ABS(sin_sq))
  sin_plus_shift            = SHIFT(th_plus*sin_sq - sin_plus,0,an_shift)
  sin_minus                 = -1.*th_minus*(esa_dth*(th2-th1)/(!RADEG)^2 < ABS(sin_sq))
  sin_minus_shift           = SHIFT(th_minus*sin_sq - sin_minus,0,-an_shift)

  domega1                   = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift

  IF tfrev THEN BEGIN
     tth1                   = th1_tmp > 0. < 180.
     tth2                   = th2_tmp > 0. < 180.
     th_plus                = (tth1 LT esa_dth) AND (tth1 NE tth2)      ;This is the part that treats field-aligned anodes differently
     th_minus               = (tth2 GT 180.-esa_dth) AND (tth1 NE tth2) ;and here
     th_other               = 1 - th_plus - th_minus                    ;and here
     sin_sq                 = .5*!PI*((SIN(tth2/!RADEG))^2 - (SIN(tth1/!RADEG))^2)
     sin_other              = th_other*sin_sq
     sin_plus               = th_plus*(esa_dth*(tth2-tth1)/(!RADEG)^2 < ABS(sin_sq))
     sin_plus_shift         = SHIFT(th_plus*sin_sq - sin_plus,0,an_shift)
     sin_minus              = -1.*th_minus*(esa_dth*(tth2-tth1)/(!RADEG)^2 < ABS(sin_sq))
     sin_minus_shift        = SHIFT(th_minus*sin_sq - sin_minus,0,-an_shift)
     domega1                = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift - domega1
  ENDIF

  ;; Calculate solid angle for 180.<th<360.
  th3_tmp                   = theta - dtheta/2.
  th3_tmp                   = (360.*(th3_tmp/360.-floor(th3_tmp/360.)))
  th4_tmp                   = th3_tmp + dtheta
  th3                       = th3_tmp > ann[0] < ann[1]
  th3                       = th3 > 180. < 360.
  th4                       = th4_tmp > ann[0] < ann[1]
  th4                       = th4 > 180. < 360.
  th_plus                   = (th4 GT 360.-esa_dth) AND (th4 NE th3) ;This is the part that treats field-aligned anodes differently
  th_minus                  = (th3 LT 180.+esa_dth) AND (th4 NE th3) ;and here
  th_other                  = 1 - th_plus - th_minus                 ;and here
  sin_sq                    = -.5*!PI*((SIN(th4/!RADEG))^2 - (SIN(th3/!RADEG))^2)
  sin_other                 = th_other*sin_sq
  sin_plus                  = th_plus*(esa_dth*(th4-th3)/(!RADEG)^2 < ABS(sin_sq))
  sin_plus_shift            = SHIFT(th_plus*sin_sq - sin_plus,0,-an_shift)
  sin_minus                 = -1.*th_minus*(esa_dth*(th4-th3)/(!RADEG)^2 < ABS(sin_sq))
  sin_minus_shift           = SHIFT(th_minus*sin_sq - sin_minus,0,an_shift)

  domega2                   = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift

  IF tfrev THEN BEGIN
     tth3                   = th3_tmp > 180. < 360.
     tth4                   = th4_tmp > 180. < 360.
     th_plus                = (tth4 GT 360.-esa_dth) AND (tth4 NE tth3) ;This is the part that treats field-aligned anodes differently
     th_minus               = (tth3 LT 180.+esa_dth) AND (tth4 NE tth3) ;and here
     th_other               = 1 - th_plus - th_minus                    ;and here
     sin_sq                 = -.5*!PI*((SIN(tth4/!RADEG))^2 - (SIN(tth3/!RADEG))^2)
     sin_other              = th_other*sin_sq
     sin_plus               = th_plus*(esa_dth*(tth4-tth3)/(!RADEG)^2 < ABS(sin_sq))
     sin_plus_shift         = SHIFT(th_plus*sin_sq - sin_plus,0,-an_shift)
     sin_minus              = -1.*th_minus*(esa_dth*(tth4-tth3)/(!RADEG)^2 < ABS(sin_sq))
     sin_minus_shift        = SHIFT(th_minus*sin_sq - sin_minus,0,an_shift)
     domega2                = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift - domega2
  ENDIF

  domega                    = domega1 + domega2

  RETURN,domega
END