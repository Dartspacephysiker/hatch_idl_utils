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
FUNCTION OMEGA_ZZ_2D_B,dat2, $
                    ENERGY=en, $
                    ERANGE=er, $
                    EBINS=ebins, $
                    ANGLE=an, $
                    ARANGE=ar, $
                    BINS=bins

  ;; COMPILE_OPT IDL2,STRICTARRSUBS

  common last_pot,pot
  if not keyword_set(pot) then pot=0.

  flux2dz = 0.

  if dat2.valid eq 0 then begin
     print,'Invalid Data'
     return, flux2dz
  endif

  dat = conv_units(dat2,"df")   ; Use distribution function
  na = dat.nenergy
  nb = dat.nbins
  
;  Correct for spacecraft potential
;  the following needs modifications to be consistent with FAST
  charge=1.                     ; charge of species
  value=0 & str_element,dat,'charge',value
  if value le 0 or value ge 0 then value=value else value=0
  if value ne 0 then charge=dat.charge		
  if ((value eq 0) and (dat.mass lt 0.00010438871)) then charge=-1. ; this line works for Wind which does not have dat.charge
  value=0 & str_element,dat,'sc_pot',value
  if value le 0 or value ge 0 then value=value else value=0
  sc_pot = value*1.2 + 1.
  if value eq 0 then sc_pot=pot
  if sc_pot gt 66.*1.2 and charge eq -1. then begin
     peak_e=cold_peak_2d(dat,energy=[66.*1.2,1000.])
     ind = where(dat.energy(*,0) eq peak_e)
     ind1 = ind
     maxcnt=max(dat.data(ind1,*),ind2)
     d0=dat.data(ind1,ind2)
     e0=dat.energy(ind1,ind2)
     dm=dat.data(ind1-1,ind2)
     em=dat.energy(ind1-1,ind2)
     dp=dat.data(ind1+1,ind2)
     ep=dat.energy(ind1+1,ind2)
     if dm ge dp then sc_pot=(dm*em+d0*e0)/(dm+d0)
     if dm lt dp then sc_pot=(dp*ep+d0*e0)/(dp+d0)
  endif
  if value ne 0 then pot=sc_pot

; The following rotates the measurement in angle to partly account 
;	for s/c potential deflection of low energy electrons


  ;;Do we need to get pot?
  val = !NULL
  STR_ELEMENT,dat,'sc_pot',val;,/ADD_REPLACE
  IF val EQ !NULL THEN BEGIN

     GET_DATA,'sc_pot',DATA=sc_pot

     IF SIZE(sc_pot,/TYPE) EQ 8 THEN BEGIN
        
        check = VALUE_CLOSEST2(sc_pot.x,dat.time,/CONSTRAINED)

        IF ABS(sc_pot.x[check]-dat.time) LT 5.0 THEN BEGIN

           STR_ELEMENT,dat,'sc_pot',sc_pot.y[check],/ADD_REPLACE

        ENDIF ELSE BEGIN

           PRINT,"Badness ..."
           STR_ELEMENT,dat,'sc_pot',0.,/ADD_REPLACE

        ENDELSE

     ENDIF ELSE BEGIN

        PRINT,"Couldn't get sc_pot ..."
        STR_ELEMENT,dat,'sc_pot',0.,/ADD_REPLACE

     ENDELSE

  ENDIF
  
  if dat.data_name ne 'HSPAD' and dat.energy(0)-dat.denergy(0) lt dat.sc_pot then begin
     tmpmin=min(abs(dat.energy(*,0)-sc_pot),ind1)
     tmpmax=max(dat.data(ind1,*),ind2)
     th=dat.theta(ind1,ind2)
     if (sc_pot gt 20. and th ne 0. and th ne 180.) then begin
        if th le 90. then begin
           rot_ind=-fix(th/15.)
           if dat.data(ind1,ind2-1) lt dat.data(ind1,ind2+1) then rot_ind=rot_ind-1
           dat3=dat
           dat3.data=shift(dat3.data,0,rot_ind)
           dat.data(ind1-2:ind1+1,*)=dat3.data(ind1-2:ind1+1,*)
        endif
        if th gt 90. then begin
           rot_ind=fix(180-th/15.)
           if dat.data(ind1,ind2-1) gt dat.data(ind1,ind2+1) then rot_ind=rot_ind+1
           dat3=dat
           dat3.data=shift(dat3.data,0,rot_ind)
           dat.data(ind1-2:ind1+1,*)=dat3.data(ind1-2:ind1+1,*)
        endif
     endif
  endif
  
  energy=dat.energy+(charge*(sc_pot)/abs(charge))	
  emin=energy-dat.denergy/2.>0.				
  emax=energy+dat.denergy/2.>0.				
  dat.energy=(emin+emax)/2.
  dat.denergy=(emax-emin)

;ind=where(abs(dat.denergy-dat2.denergy) gt .01 and dat.denergy gt 0.01)
                                ;if n_elements(ind) gt 1 then dat.data(ind)=0.
                                ;if n_elements(ind) gt 1 then dat.data(ind)=dat.data(ind)*dat2.denergy(ind)/dat.denergy(ind)
                                ;if n_elements(ind) gt 1 then bgnd=0.7*dat.data(ind-1)*(dat2.denergy(ind)-dat.denergy(ind))/dat2.denergy(ind)
;if n_elements(ind) gt 1 then bgnd=dat.data(ind-1)*(dat2.denergy(ind)-dat.denergy(ind))/dat2.denergy(ind)
;if n_elements(ind) gt 1 then dat.data(ind)=((dat.data(ind)-bgnd)>0.)*dat2.denergy(ind)/dat.denergy(ind)
                                ;print,dat.energy(*,0)
                                ;print,dat.denergy(*,0)
  
  ebins2=replicate(1b,na)
  if keyword_set(en) then begin
     ebins2(*)=0
     er2=[energy_to_ebin(dat,en)]
     if er2(0) gt er2(1) then er2=reverse(er2)
     ebins2(er2(0):er2(1))=1
  endif
  if keyword_set(er) then begin
     ebins2(*)=0
     er2=er
     if er2(0) gt er2(1) then er2=reverse(er2)
     ebins2(er2(0):er2(1))=1
  endif
  if keyword_set(ebins) then ebins2=ebins
;print,en,er2,ebins2

  bins2=replicate(1b,nb)
  if keyword_set(an) then begin
     if ndimen(an) ne 1 or dimen1(an) ne 2 then begin
        print,'Error - angle keyword must be fltarr(2)'
     endif else begin
        bins2=angle_to_bins(dat,an)
     endelse
  endif
  if keyword_set(ar) then begin
     bins2(*)=0
     if ar(0) gt ar(1) then begin
        bins2(ar(0):nb-1)=1
        bins2(0:ar(1))=1
     endif else begin
        bins2(ar(0):ar(1))=1
     endelse
  endif
  if keyword_set(bins) then bins2=bins

  if ndimen(bins2) ne 2 then bins2=ebins2#bins2

  data = dat.data*bins2
  energy = dat.energy
  denergy = dat.denergy
  theta = dat.theta/!radeg
  dtheta = dat.dtheta/!radeg
  mass = dat.mass     
  value=0 & str_element,dat,'domega',value
  if n_elements(value) ne 1 then domega = dat.domega

; check to see if pitch-angles wrap around past 180 (to 360)
; (e.g. FAST data) or not. If they do, then calculate an appropriate
; domega. 

; This section for FAST
; the more complex calculation of domega*cos(theta) works better for narrow beams
; the commented out lines below are for simplified integrals over solid angle where 
;	cos(theta) in the integral is assumed to be the center values
  if max(dat.theta) gt 200. OR dat.project_name EQ 'FAST' then begin
     if (theta(0,0) eq theta(na-1,0)) then nna=0 else nna=na-1
     if ndimen(dtheta) eq 1 then dtheta=replicate(1.,na)#dtheta
     domega = theta
     for a=0,nna do begin
        for b=0,nb-1 do begin
           if (abs(theta(a,b)-!pi) lt dtheta(a,b)/2.) then begin 
              th1 = (!pi+theta(a,b)-dtheta(a,b)/2.)/2.
              dth1 = (!pi-th1)
              th2 = (!pi+theta(a,b)+dtheta(a,b)/2.)/2.
              dth2 = (th2-!pi)
;        	domega(a, b) = 2.*!pi*(abs(sin(th1))*sin(dth1)+abs(sin(th2))*sin(dth2)) 
              domega(a,b)=2.*!pi*(abs(sin(th1))*sin(dth1)*cos(th1)*cos(dth1) + abs(sin(th2))*sin(dth2)*cos(th1)*cos(dth2)) 
           endif else if (abs(theta(a,b)-2*!pi) lt dtheta(a,b)/2.) then begin
              th1 = (2.*!pi+theta(a,b)-dtheta(a,b)/2.)/2.
              dth1 = (2.*!pi-th1)
              th2 = (2.*!pi+theta(a,b)+dtheta(a,b)/2.)/2.
              dth2 = (th2-2.*!pi)
;        	domega(a, b) = 2.*!pi*(abs(sin(th1))*sin(dth1)+abs(sin(th2))*sin(dth2)) 
              domega(a,b)=2.*!pi*(abs(sin(th1))*sin(dth1)*cos(th1)*cos(dth1) + abs(sin(th2))*sin(dth2)*cos(th1)*cos(dth2)) 
           endif else if (abs(theta(a,b)) lt dtheta(a,b)/2.) then begin
              th1 = (theta(a,b)-dtheta(a,b)/2.)/2.
              dth1 = abs(th1)
              th2 = (theta(a,b)+dtheta(a,b)/2.)/2.
              dth2 = (th2)
;        	domega(a, b) = 2.*!pi*(abs(sin(th1))*sin(dth1)+abs(sin(th2))*sin(dth2)) 
              domega(a,b)=2.*!pi*(abs(sin(th1))*sin(dth1)*cos(th1)*cos(dth1) + abs(sin(th2))*sin(dth2)*cos(th1)*cos(dth2)) 
           endif else begin
              th1 = theta(a,b)
              dth1 = dtheta(a,b)/2.
;        	domega(a, b) = 2.*!pi*abs(sin(th1))*sin(dth1)
              domega(a,b)=2.*!pi*abs(sin(th1))*sin(dth1)*(cos(th1)*cos(dth1))
           endelse
        endfor
     endfor
     if (nna eq 0) then for a=1,na-1 do domega(a,*)=domega(0,*)
  endif else domega=domega*cos(theta)

  RETURN,domega

END

;; IF dat2.valid EQ 0 THEN BEGIN
;;    PRINT,'Invalid Data'
;;    RETURN, -1.
;; ENDIF

;; dat                       = CONV_UNITS(dat2,"eflux") ; Use Energy Flux
;; na                        = dat.nenergy
;; nb                        = dat.nbins

;; ebins2                    = REPLICATE(1b,na)
;; ;; User-provided energy range?
;; IF KEYWORD_SET(en) THEN BEGIN
;;    ebins2[*]              = 0
;;    er2                    = [ENERGY_TO_EBIN(dat,en)]
;;    IF er2[0] GT er2[1] THEN BEGIN
;;       er2                 = REVERSE(er2)
;;    ENDIF
;;    ebins2[er2[0]:er2[1]]  = 1
;; ENDIF
;; IF KEYWORD_SET(er) THEN BEGIN
;;    ebins2[*]              = 0
;;    er2                    = er
;;    IF er2[0] GT er2[1] THEN BEGIN
;;       er2                 = REVERSE(er2)
;;    ENDIF
;;    ebins2[er2[0]:er2[1]]  = 1
;; ENDIF
;; ;; User-provided energy bins?
;; IF KEYWORD_SET(ebins) THEN BEGIN
;;    ebins2                 = ebins
;; ENDIF

;; bins2                     = REPLICATE(1b,nb)

;; ;; User-provided angle-bin range?
;; IF KEYWORD_SET(ar) THEN BEGIN
;;    bins2[*]               = 0
;;    IF ar[0] GT ar[1] THEN BEGIN
;;       bins2[ar[0]:nb-1]   = 1
;;       bins2[0:ar[1]]      = 1
;;    ENDIF ELSE BEGIN
;;       bins2[ar[0]:ar[1]]  = 1
;;    ENDELSE
;; ENDIF
;; ;; User-provided bins?
;; IF KEYWORD_SET(bins) THEN BEGIN
;;    bins2                  = bins
;; ENDIF

;; IF NDIMEN(bins2) NE 2 THEN BEGIN
;;    bins2                  = ebins2#bins2
;; ENDIF

;; data                      = dat.data*bins2
;; energy                    = dat.energy
;; denergy                   = dat.denergy
;; theta                     = dat.theta
;; dtheta                    = dat.dtheta
;; IF NDIMEN(dtheta) EQ 1 THEN BEGIN
;;    dtheta                 = REPLICATE(1.,na)#dtheta
;; ENDIF
;; mass                      = dat.mass * 1.6D-22

;; ;;Constants from J_2D_B, P_2D_B,
;; Const_n                   = (mass/(2.*1.6D-12))^(.5D)

;; Const_j                   = 1.6e-12
;; Const_p                   = (mass/(2.*1.6D-12))^(-.5D)

;; esa_dth                   = 5. < !PI*MIN(dtheta)/4.
;; esa_drad                  = esa_dth/!RADEG ;Used in P_2D_B

;; minvar                    = MIN(theta[0,*],indminvar)
;; IF indminvar GT 1 THEN BEGIN
;;    an_shift               = theta[0,0] LT theta[0,1]
;; ENDIF ELSE an_shift       = theta[0,2] LT theta[0,3]
;; an_shift                  = 2*an_shift-1

;; ;; Provided pitch angle range?
;; IF KEYWORD_SET(an) THEN BEGIN
;;    ann                    = (360.*(an/360.-FLOOR(an/360.)))
;;    IF an[1] EQ 360. THEN BEGIN
;;       ann[1]              = 360.
;;    ENDIF
;; ENDIF ELSE BEGIN
;;    ann                    = [0.,360.]
;; ENDELSE
;; IF ann[0] GT ann[1] THEN BEGIN
;;    ann                    = REVERSE(ann)
;;    tfrev                  = 1
;; ENDIF ELSE BEGIN
;;    tfrev                  = 0
;; ENDELSE

;; CASE 1 OF
;;    KEYWORD_SET(yust_1D): BEGIN

;;       ;; Calculate solid angle for 0.<th<180.
;;       th2_tmp                   = theta + dtheta/2.
;;       th2_tmp                   = (360.*(th2_tmp/360.-floor(th2_tmp/360.)))
;;       th1_tmp                   = th2_tmp - dtheta
;;       th1                       = th1_tmp > ann[0] < ann[1]
;;       th1                       = th1 > 0. < 180.
;;       th2                       = th2_tmp > ann[0] < ann[1]
;;       th2                       = th2 > 0. < 180.
;;       th_plus                   = (th1 LT esa_dth) AND (th1 NE th2)      ;This is the part that treats field-aligned anodes differently
;;       th_minus                  = (th2 GT 180.-esa_dth) AND (th1 NE th2) ;and here
;;       th_other                  = 1 - th_plus - th_minus                 ;and here
;;       sin_sq                    = .5D *!PI * ((SIN(th2/!RADEG))^2 - (SIN(th1/!RADEG))^2)
;;       sin_other                 = th_other * sin_sq
;;       sin_plus                  = th_plus*(esa_dth*(th2-th1)/(!RADEG)^2 < ABS(sin_sq))
;;       sin_plus_shift            = SHIFT(th_plus*sin_sq - sin_plus,0,an_shift)
;;       sin_minus                 = -1.*th_minus*(esa_dth*(th2-th1)/(!RADEG)^2 < ABS(sin_sq))
;;       sin_minus_shift           = SHIFT(th_minus*sin_sq - sin_minus,0,-an_shift)

;;       domega1                   = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift

;;       IF tfrev THEN BEGIN
;;          tth1                   = th1_tmp > 0. < 180.
;;          tth2                   = th2_tmp > 0. < 180.
;;          th_plus                = (tth1 LT esa_dth) AND (tth1 NE tth2)      ;This is the part that treats field-aligned anodes differently
;;          th_minus               = (tth2 GT 180.-esa_dth) AND (tth1 NE tth2) ;and here
;;          th_other               = 1 - th_plus - th_minus                    ;and here
;;          sin_sq                 = .5*!PI*((SIN(tth2/!RADEG))^2 - (SIN(tth1/!RADEG))^2)
;;          sin_other              = th_other*sin_sq
;;          sin_plus               = th_plus*(esa_dth*(tth2-tth1)/(!RADEG)^2 < ABS(sin_sq))
;;          sin_plus_shift         = SHIFT(th_plus*sin_sq - sin_plus,0,an_shift)
;;          sin_minus              = -1.*th_minus*(esa_dth*(tth2-tth1)/(!RADEG)^2 < ABS(sin_sq))
;;          sin_minus_shift        = SHIFT(th_minus*sin_sq - sin_minus,0,-an_shift)
;;          domega1                = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift - domega1
;;       ENDIF

;;       ;; Calculate solid angle for 180.<th<360.
;;       th3_tmp                   = theta - dtheta/2.
;;       th3_tmp                   = (360.*(th3_tmp/360.-floor(th3_tmp/360.)))
;;       th4_tmp                   = th3_tmp + dtheta
;;       th3                       = th3_tmp > ann[0] < ann[1]
;;       th3                       = th3 > 180. < 360.
;;       th4                       = th4_tmp > ann[0] < ann[1]
;;       th4                       = th4 > 180. < 360.
;;       th_plus                   = (th4 GT 360.-esa_dth) AND (th4 NE th3) ;This is the part that treats field-aligned anodes differently
;;       th_minus                  = (th3 LT 180.+esa_dth) AND (th4 NE th3) ;and here
;;       th_other                  = 1 - th_plus - th_minus                 ;and here
;;       sin_sq                    = -.5*!PI*((SIN(th4/!RADEG))^2 - (SIN(th3/!RADEG))^2)
;;       sin_other                 = th_other*sin_sq
;;       sin_plus                  = th_plus*(esa_dth*(th4-th3)/(!RADEG)^2 < ABS(sin_sq))
;;       sin_plus_shift            = SHIFT(th_plus*sin_sq - sin_plus,0,-an_shift)
;;       sin_minus                 = -1.*th_minus*(esa_dth*(th4-th3)/(!RADEG)^2 < ABS(sin_sq))
;;       sin_minus_shift           = SHIFT(th_minus*sin_sq - sin_minus,0,an_shift)

;;       domega2                   = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift

;;       IF tfrev THEN BEGIN
;;          tth3                   = th3_tmp > 180. < 360.
;;          tth4                   = th4_tmp > 180. < 360.
;;          th_plus                = (tth4 GT 360.-esa_dth) AND (tth4 NE tth3) ;This is the part that treats field-aligned anodes differently
;;          th_minus               = (tth3 LT 180.+esa_dth) AND (tth4 NE tth3) ;and here
;;          th_other               = 1 - th_plus - th_minus                    ;and here
;;          sin_sq                 = -.5D * !PI * ((SIN(tth4/!RADEG))^2 - (SIN(tth3/!RADEG))^2)
;;          sin_other              = th_other * sin_sq
;;          sin_plus               = th_plus  * (esa_dth*(tth4-tth3)/(!RADEG)^2 < ABS(sin_sq))
;;          sin_plus_shift         = SHIFT(th_plus*sin_sq - sin_plus,0,-an_shift)
;;          sin_minus              = -1.*th_minus*(esa_dth*(tth4-tth3)/(!RADEG)^2 < ABS(sin_sq))
;;          sin_minus_shift        = SHIFT(th_minus*sin_sq - sin_minus,0,an_shift)
;;          domega2                = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift - domega2
;;       ENDIF

;;       domega                    = domega1 + domega2

;;    END
;;    ELSE: BEGIN
;;       ;; Calculate solid angle for 0.<th<180.

;;       th2_tmp = theta + dtheta/2.
;;       th2_tmp = (360.*(th2_tmp/360.-floor(th2_tmp/360.)))
;;       th1_tmp = th2_tmp - dtheta
;;       th1 = th1_tmp > ann(0) < ann(1)
;;       th1 = th1 > 0. < 180.
;;       th2 = th2_tmp > ann(0) < ann(1)
;;       th2 = th2 > 0. < 180.
;;       th_plus = (th1 lt esa_dth) and (th1 ne th2)
;;       th_minus = (th2 gt 180.-esa_dth) and (th1 ne th2)
;;       th_other = 1 - th_plus - th_minus

;;       th1 = th1/!radeg
;;       th2 = th2/!radeg
;;       domega_zz = !pi*((cos(th1))^3 - (cos(th2))^3)/3.
;;       domega_xx = !pi*((cos(th1)) - (cos(th2))) - domega_zz
;;       other_zz = th_other*domega_zz
;;       ;;other_xx = th_other*domega_xx

;;       ;;plus_zz = th_plus*((esa_drad*(th2-th1)-esa_drad^3*(th2-th1)/3.-esa_drad*(th2-th1)^3/3.) < abs(domega_zz))
;;       plus_zz = th_plus*((esa_drad*(th2-th1)) < abs(domega_zz))
;;       plus_zz_shift = shift(th_plus*domega_zz - plus_zz,0,an_shift)
;;       ;;plus_xx = th_plus*(esa_drad^3*(th2-th1)/3.+esa_drad*(th2-th1)^3/3. < abs(domega_xx))
;;       ;;plus_xx_shift = shift(th_plus*domega_xx - plus_xx,0,an_shift)

;;       ;;minus_zz = th_minus*((esa_drad*(th2-th1)-esa_drad^3*(th2-th1)/3.-esa_drad*(th2-th1)^3/3.) < abs(domega_zz))
;;       minus_zz = th_minus*((esa_drad*(th2-th1)) < abs(domega_zz))
;;       minus_zz_shift = shift(th_minus*domega_zz - minus_zz,0,-an_shift)
;;       ;;minus_xx = th_minus*(esa_drad^3*(th2-th1)/3.+esa_drad*(th2-th1)^3/3. < abs(domega_xx))
;;       ;;minus_xx_shift = shift(th_minus*domega_xx - minus_xx,0,-an_shift)

;;       domega1_zz = other_zz+plus_zz+plus_zz_shift+minus_zz+minus_zz_shift
;;       ;;domega1_xx = other_xx+plus_xx+plus_xx_shift+minus_xx+minus_xx_shift
;;       domega1_xx = domega_xx

;;       if tfrev then begin
;;          tth1 = th1_tmp > 0. < 180.
;;          tth2 = th2_tmp > 0. < 180.
;;          th_plus = (tth1 lt esa_dth) and (tth1 ne tth2)
;;          th_minus = (tth2 gt 180.-esa_dth) and (tth1 ne tth2)
;;          th_other = 1 - th_plus - th_minus

;;          th1 = tth1/!radeg
;;          th2 = tth2/!radeg
;;          domega_zz = !pi*((cos(th1))^3 - (cos(th2))^3)/3.
;;          domega_xx = !pi*((cos(th1)) - (cos(th2))) - domega_zz
;;          other_zz = th_other*domega_zz
;;          ;;	other_xx = th_other*domega_xx

;;          ;;	plus_zz = th_plus*((esa_drad*(th2-th1)-esa_drad^3*(th2-th1)/3.-esa_drad*(th2-th1)^3/3.) < abs(domega_zz))
;;          plus_zz = th_plus*((esa_drad*(th2-th1)) < abs(domega_zz))
;;          plus_zz_shift = shift(th_plus*domega_zz - plus_zz,0,an_shift)
;;          ;;	plus_xx = th_plus*(esa_drad^3*(th2-th1)/3.+esa_drad*(th2-th1)^3/3. < abs(domega_xx))
;;          ;;	plus_xx_shift = shift(th_plus*domega_xx - plus_xx,0,an_shift)

;;          ;;	minus_zz = th_minus*((esa_drad*(th2-th1)-esa_drad^3*(th2-th1)/3.-esa_drad*(th2-th1)^3/3.) < abs(domega_zz))
;;          minus_zz = th_minus*((esa_drad*(th2-th1)) < abs(domega_zz))
;;          minus_zz_shift = shift(th_minus*domega_zz - minus_zz,0,-an_shift)
;;          ;;	minus_xx = th_minus*(esa_drad^3*(th2-th1)/3.+esa_drad*(th2-th1)^3/3. < abs(domega_xx))
;;          ;;	minus_xx_shift = shift(th_minus*domega_xx - minus_xx,0,-an_shift)

;;          domega1_zz = other_zz+plus_zz+plus_zz_shift+minus_zz+minus_zz_shift - domega1_zz
;;          ;;	domega1_xx = other_xx+plus_xx+plus_xx_shift+minus_xx+minus_xx_shift - domega1_xx
;;          domega1_xx = domega_xx - domega1_xx

;;       endif

;;       ;; Calculate solid angle for 180.<th<360.

;;       th3_tmp = theta - dtheta/2.
;;       th3_tmp = (360.*(th3_tmp/360.-floor(th3_tmp/360.)))
;;       th4_tmp = th3_tmp + dtheta
;;       th3 = th3_tmp > ann(0) < ann(1)
;;       th3 = th3 > 180. < 360.
;;       th4 = th4_tmp > ann(0) < ann(1)
;;       th4 = th4 > 180. < 360.
;;       th_plus = (th4 gt 360.-esa_dth) and (th4 ne th3)
;;       th_minus = (th3 lt 180.+esa_dth) and (th4 ne th3)
;;       th_other = 1 - th_plus - th_minus

;;       th1 = th3/!radeg
;;       th2 = th4/!radeg
;;       domega_zz = !pi*((cos(th2))^3 - (cos(th1))^3)/3.
;;       domega_xx = !pi*((cos(th2)) - (cos(th1))) - domega_zz
;;       other_zz = th_other*domega_zz
;;       ;;other_xx = th_other*domega_xx

;;       ;;plus_zz = th_plus*((esa_drad*(th2-th1)-esa_drad^3*(th2-th1)/3.-esa_drad*(th2-th1)^3/3.) < abs(domega_zz))
;;       plus_zz = th_plus*((esa_drad*(th2-th1)) < abs(domega_zz))
;;       plus_zz_shift = shift(th_plus*domega_zz - plus_zz,0,-an_shift)
;;       ;;plus_xx = th_plus*(esa_drad^3*(th2-th1)/3.+esa_drad*(th2-th1)^3/3. < abs(domega_xx))
;;       ;;plus_xx_shift = shift(th_plus*domega_xx - plus_xx,0,an_shift)

;;       ;;minus_zz = th_minus*((esa_drad*(th2-th1)-esa_drad^3*(th2-th1)/3.-esa_drad*(th2-th1)^3/3.) < abs(domega_zz))
;;       minus_zz = th_minus*((esa_drad*(th2-th1)) < abs(domega_zz))
;;       minus_zz_shift = shift(th_minus*domega_zz - minus_zz,0,an_shift)
;;       ;;minus_xx = th_minus*(esa_drad^3*(th2-th1)/3.+esa_drad*(th2-th1)^3/3. < abs(domega_xx))
;;       ;;minus_xx_shift = shift(th_minus*domega_xx - minus_xx,0,-an_shift)

;;       domega2_zz = other_zz+plus_zz+plus_zz_shift+minus_zz+minus_zz_shift
;;       ;;domega2_xx = other_xx+plus_xx+plus_xx_shift+minus_xx+minus_xx_shift
;;       domega2_xx = domega_xx

;;       if tfrev then begin
;;          tth3 = th3_tmp > 180. < 360.
;;          tth4 = th4_tmp > 180. < 360.
;;          th_plus = (tth4 gt 360.-esa_dth) and (tth4 ne tth3)
;;          th_minus = (tth3 lt 180.+esa_dth) and (tth4 ne tth3)
;;          th_other = 1 - th_plus - th_minus

;;          th1 = tth3/!radeg
;;          th2 = tth4/!radeg
;;          domega_zz = !pi*((cos(th2))^3 - (cos(th1))^3)/3.
;;          domega_xx = !pi*((cos(th2)) - (cos(th1))) - domega_zz
;;          other_zz = th_other*domega_zz
;;          ;;	other_xx = th_other*domega_xx

;;          ;;	plus_zz = th_plus*((esa_drad*(th2-th1)-esa_drad^3*(th2-th1)/3.-esa_drad*(th2-th1)^3/3.) < abs(domega_zz))
;;          plus_zz = th_plus*((esa_drad*(th2-th1)) < abs(domega_zz))
;;          plus_zz_shift = shift(th_plus*domega_zz - plus_zz,0,-an_shift)
;;          ;;	plus_xx = th_plus*(esa_drad^3*(th2-th1)/3.+esa_drad*(th2-th1)^3/3. < abs(domega_xx))
;;          ;;	plus_xx_shift = shift(th_plus*domega_xx - plus_xx,0,an_shift)

;;          ;;	minus_zz = th_minus*((esa_drad*(th2-th1)-esa_drad^3*(th2-th1)/3.-esa_drad*(th2-th1)^3/3.) < abs(domega_zz))
;;          minus_zz = th_minus*((esa_drad*(th2-th1)) < abs(domega_zz))
;;          minus_zz_shift = shift(th_minus*domega_zz - minus_zz,0,an_shift)
;;          ;;	minus_xx = th_minus*(esa_drad^3*(th2-th1)/3.+esa_drad*(th2-th1)^3/3. < abs(domega_xx))
;;          ;;	minus_xx_shift = shift(th_minus*domega_xx - minus_xx,0,-an_shift)

;;          domega2_zz = other_zz+plus_zz+plus_zz_shift+minus_zz+minus_zz_shift - domega2_zz
;;          ;;	domega2_xx = other_xx+plus_xx+plus_xx_shift+minus_xx+minus_xx_shift - domega2_xx
;;          domega2_xx = domega_xx - domega2_xx
;;       endif

;;       domega_zz = domega1_zz + domega2_zz
;;       domega_xx = .5*(domega1_xx + domega2_xx)

;;       domega    = [domega_zz,domega_xx]

;;    END
;; ENDCASE

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;Just add water!!

;; ;;Last lines from n_2d_b
;; ;; domega = domega1 + domega2

;; sumdata = TOTAL(data*domega,2)
;; density = Const_n*TOTAL(denergy*(energy^(-1.5D))*sumdata) ; units are 1/cm^3



;; RETURN,domega
;; END