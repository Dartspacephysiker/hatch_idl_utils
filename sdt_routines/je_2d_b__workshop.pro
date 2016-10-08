;+
;FUNCTION:	je_2d_b(dat,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=angleRange,ARANGE=ar,BINS=bins)
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
;	Returns the field aligned energy flux, JEz, ergs/cm^2-sec, assumes a narrow (< 5 deg) field aligned beam
;NOTES:	
;	Similar to je_2d.pro, treats the anodes within 5 deg of the magnetic field differently.
;	Function normally called by "get_2dt.pro" to generate 
;	time series data for "tplot.pro".
;
;CREATED BY:
;	J.McFadden	97-5-14		Created from je_2d.pro
;					Treats narrow beams correctly, no do loops
;LAST MODIFICATION:
;	97-5-14		J.McFadden
;-
function je_2d_b__workshop,dat2,ENERGY=energyRange,ERANGE=energyBinRange,EBINS=ebins,ANGLE=angleRange,ARANGE=angleBinRange,BINS=bins

eflux3dz = 0.

if dat2.valid eq 0 then begin
  print,'Invalid Data'
  return, eflux3dz
endif

dat = conv_units(dat2,"eflux")		; Use Energy Flux
na = dat.nenergy
nb = dat.nbins
	
ebins2=replicate(1b,na)
if keyword_set(energyRange) then begin
	ebins2(*)=0
	energyBinRange2=[energy_to_ebin(dat,energyRange)]
	if energyBinRange2(0) gt energyBinRange2(1) then energyBinRange2=reverse(energyBinRange2)
	ebins2(energyBinRange2(0):energyBinRange2(1))=1
endif
if keyword_set(energyBinRange) then begin
	ebins2(*)=0
	energyBinRange2=energyBinRange
	if energyBinRange2(0) gt energyBinRange2(1) then energyBinRange2=reverse(energyBinRange2)
	ebins2(energyBinRange2(0):energyBinRange2(1))=1
endif
if keyword_set(ebins) then ebins2=ebins

bins2=replicate(1b,nb)

if keyword_set(angleBinRange) then begin
	bins2(*)=0
	if angleBinRange(0) gt angleBinRange(1) then begin
		bins2(angleBinRange(0):nb-1)=1
		bins2(0:angleBinRange(1))=1
	endif else begin
		bins2(angleBinRange(0):angleBinRange(1))=1
	endelse
endif
if keyword_set(bins) then bins2=bins

if ndimen(bins2) ne 2 then bins2=ebins2#bins2

data = dat.data*bins2
energy = dat.energy
denergy = dat.denergy
theta = dat.theta
dtheta = dat.dtheta
	if ndimen(dtheta) eq 1 then dtheta=replicate(1.,na)#dtheta
mass = dat.mass * 1.6e-22
Const = 1.6e-12
esa_dth = 5. < !pi*min(dtheta)/4.

minvar = min(theta(0,*),indminvar)
if indminvar gt 1 then begin
	an_shift = theta(0,0) lt theta(0,1)
endif else an_shift = theta(0,2) lt theta(0,3)
an_shift = 2*an_shift-1

if keyword_set(angleRange) then begin
	ann = (360.*(angleRange/360.-floor(angleRange/360.)))
	if angleRange(1) eq 360. then ann(1)=360.
endif else ann=[0.,360.]
if ann(0) gt ann(1) then begin
	ann=reverse(ann) 
	tfrev=1
endif else tfrev=0

; Calculate solid angle for 0.<th<180.

th2_tmp = theta + dtheta/2.
th2_tmp = (360.*(th2_tmp/360.-floor(th2_tmp/360.)))
th1_tmp = th2_tmp - dtheta
th1 = th1_tmp > ann(0) < ann(1)
th1 = th1 > 0. < 180.
th2 = th2_tmp > ann(0) < ann(1)
th2 = th2 > 0. < 180.
th_plus = (th1 lt esa_dth) and (th1 ne th2)
th_minus = (th2 gt 180.-esa_dth) and (th1 ne th2)
th_other = 1 - th_plus - th_minus
sin_sq = .5*!pi*((sin(th2/!radeg))^2 - (sin(th1/!radeg))^2)
sin_other = th_other*sin_sq
sin_plus = th_plus*(esa_dth*(th2-th1)/(!radeg)^2 < abs(sin_sq))
sin_plus_shift = shift(th_plus*sin_sq - sin_plus,0,an_shift)
sin_minus = -1.*th_minus*(esa_dth*(th2-th1)/(!radeg)^2 < abs(sin_sq)) 
sin_minus_shift = shift(th_minus*sin_sq - sin_minus,0,-an_shift)

domega1 = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift

if tfrev then begin
	tth1 = th1_tmp > 0. < 180.
	tth2 = th2_tmp > 0. < 180.
	th_plus = (tth1 lt esa_dth) and (tth1 ne tth2)
	th_minus = (tth2 gt 180.-esa_dth) and (tth1 ne tth2)
	th_other = 1 - th_plus - th_minus
	sin_sq = .5*!pi*((sin(tth2/!radeg))^2 - (sin(tth1/!radeg))^2)
	sin_other = th_other*sin_sq
	sin_plus = th_plus*(esa_dth*(tth2-tth1)/(!radeg)^2 < abs(sin_sq))
	sin_plus_shift = shift(th_plus*sin_sq - sin_plus,0,an_shift)
	sin_minus = -1.*th_minus*(esa_dth*(tth2-tth1)/(!radeg)^2 < abs(sin_sq)) 
	sin_minus_shift = shift(th_minus*sin_sq - sin_minus,0,-an_shift)
	domega1 = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift - domega1
endif

; Calculate solid angle for 180.<th<360.

th3_tmp = theta - dtheta/2.
th3_tmp = (360.*(th3_tmp/360.-floor(th3_tmp/360.)))
th4_tmp = th3_tmp + dtheta
th3 = th3_tmp > ann(0) < ann(1)
th3 = th3 > 180. < 360.
th4 = th4_tmp > ann(0) < ann(1)
th4 = th4 > 180. < 360.
th_plus = (th4 gt 360.-esa_dth) and (th4 ne th3)
th_minus = (th3 lt 180.+esa_dth) and (th4 ne th3)
th_other = 1 - th_plus - th_minus
sin_sq = -.5*!pi*((sin(th4/!radeg))^2 - (sin(th3/!radeg))^2)
sin_other = th_other*sin_sq
sin_plus = th_plus*(esa_dth*(th4-th3)/(!radeg)^2 < abs(sin_sq)) 
sin_plus_shift = shift(th_plus*sin_sq - sin_plus,0,-an_shift)
sin_minus = -1.*th_minus*(esa_dth*(th4-th3)/(!radeg)^2 < abs(sin_sq)) 
sin_minus_shift = shift(th_minus*sin_sq - sin_minus,0,an_shift)

domega2 = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift

if tfrev then begin
	tth3 = th3_tmp > 180. < 360.
	tth4 = th4_tmp > 180. < 360.
	th_plus = (tth4 gt 360.-esa_dth) and (tth4 ne tth3)
	th_minus = (tth3 lt 180.+esa_dth) and (tth4 ne tth3)
	th_other = 1 - th_plus - th_minus
	sin_sq = -.5*!pi*((sin(tth4/!radeg))^2 - (sin(tth3/!radeg))^2)
	sin_other = th_other*sin_sq
	sin_plus = th_plus*(esa_dth*(tth4-tth3)/(!radeg)^2 < abs(sin_sq)) 
	sin_plus_shift = shift(th_plus*sin_sq - sin_plus,0,-an_shift)
	sin_minus = -1.*th_minus*(esa_dth*(tth4-tth3)/(!radeg)^2 < abs(sin_sq)) 
	sin_minus_shift = shift(th_minus*sin_sq - sin_minus,0,an_shift)
	domega2 = sin_other+sin_plus+sin_plus_shift+sin_minus+sin_minus_shift - domega2
endif

domega = domega1 + domega2

sumdataz = total(data*domega,2)
eflux3dz = Const*total(denergy*sumdataz)

; units are ergs/cm^2-sec

return, eflux3dz
end

