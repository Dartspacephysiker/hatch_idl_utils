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
;	Returns the field aligned flux, Jz, #/cm^2-sec, assumes a narrow (< 5 deg) field aligned beam
;NOTES:	
;	Similar to j_2d.pro, treats the anodes within 5 deg of the magnetic field differently.
;	Function normally called by "get_2dt.pro" to generate 
;	time series data for "tplot.pro".
;
;CREATED BY:
;	J.McFadden	97-5-14		Created from j_2d.pro
;					Treats narrow beams correctly, no do loops
;LAST MODIFICATION:
;	97-5-14		J.McFadden
;-
function olsson_janhunen_1998_eq_5__fa_conductance_2d_b,dat2, $
   ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins

flux3dz = 0.

if dat2.valid eq 0 then begin
  print,'Invalid Data'
  return, flux3dz
endif

dat = conv_units(dat2,"dfstd")		; Use standard dist. func. units, s^3/m^6
na = dat.nenergy
nb = dat.nbins
	
ebins2=replicate(1b,na)
; User-provided energy range?
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
; User-provided energy bins?
if keyword_set(ebins) then ebins2=ebins

bins2=replicate(1b,nb)

; User-provided angle-bin range?
if keyword_set(ar) then begin
	bins2(*)=0
	if ar(0) gt ar(1) then begin
		bins2(ar(0):nb-1)=1
		bins2(0:ar(1))=1
	endif else begin
		bins2(ar(0):ar(1))=1
	endelse
endif
; User-provided bins?
if keyword_set(bins) then bins2=bins

if ndimen(bins2) ne 2 then bins2=ebins2#bins2

data = dat.data*bins2
energy = dat.energy
denergy = dat.denergy
theta = dat.theta
dtheta = dat.dtheta
	if ndimen(dtheta) eq 1 then dtheta=replicate(1.,na)#dtheta
mass = dat.mass * 1.6e-22 ;puts 'er in kg
Const = 1.6e-12
esa_dth = 5. < !pi*min(dtheta)/4.

minvar = min(theta(0,*),indminvar)
if indminvar gt 1 then begin
	an_shift = theta(0,0) lt theta(0,1)
endif else an_shift = theta(0,2) lt theta(0,3)
an_shift = 2*an_shift-1

; Provided pitch angle range?
if keyword_set(an) then begin
	ann = (360.*(an/360.-floor(an/360.)))
	if an(1) eq 360. then ann(1)=360.
endif else ann=[0.,360.]
if ann(0) gt ann(1) then begin
	ann=reverse(ann) 
	tfrev=1
endif else tfrev=0

outAngle = WHERE(theta LT ann[0] OR theta GT ann[1],nBad)
IF nBad GT 0 THEN BEGIN
   data[outAngle] = 0.
ENDIF

; Integrate dat
energy *= 1.6e-19
integ = 0.D
FOR k=0,N_ELEMENTS(data[0,*])-1 DO BEGIN
   integ += INT_TABULATED(REVERSE(energy[*,k]),REVERSE(data[*,k]),/DOUBLE)
ENDFOR

;note, mass in eV/(km/s)^2
;convert it ...


;;The overall multiplicative/dimensional factor is huge:
;2.D* !PI * ( 1.6e-19 / 9.11e-31 )^2 / 1.6e-19
; =   1.2113315295060807e+42

integ *= 2. * !PI * DOUBLE(1.602e-19/mass)^2

;;One final division by 1.6e-19 to get the units to be SI
;; integ /= 1.6e-19

; units are #/cm^2-sec

return, integ
end

