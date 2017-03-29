;2017/03/29
function n_2d_b__real2d,dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins,OUT_DOMEGA=domega

density = 0.

if dat2.valid eq 0 then begin
  print,'Invalid Data'
  return, density
endif

dat = conv_units(dat2,"eflux")		; Use Energy Flux
na = dat.nenergy
nb = dat.nbins
	
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

bins2=replicate(1b,nb)
if keyword_set(an) then begin
	if ndimen(an) ne 2 then begin
		print,'Error - angle keyword must be (2,2)'
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
;; phi = dat.phi/!radeg
dtheta = dat.dtheta/!radeg
;; dphi = dat.dphi/!radeg
mass = dat.mass * 1.6D-22
Const = (mass/(2.D*1.6D-12))^(.5D)
esa_dth = 5. < !pi*min(dtheta)/4.

;  Use the following lines until Davin gets  WIND "denergy" correct
if dat.project_name eq 'Wind 3D Plasma' then begin
	for a=0,na-1 do begin
		if a eq 0 then denergy(a,*) = abs(energy(a,*)-energy(a+1,*)) else $
		if a eq na-1 then denergy(a,*) = abs(energy(a-1,*)-energy(a,*)) else $
		denergy(a,*) = .5*abs(energy(a-1,*)-energy(a+1,*))
	endfor
endif

str_element,dat,"domega",value=domega,index=ind
if ind ge 0 then begin
	if ndimen(domega) eq 1 then domega=replicate(1.,na)#domega
endif else begin
	if ndimen(esa_dth) eq 1 then esa_dth=replicate(1.,na)#esa_dth
	;; if ndimen(dphi) eq 1 then dphi=replicate(1.,na)#dphi
	domega=2.D*sin(.5D*esa_dth)
endelse

;; sumdata = total(data*domega,2)
;; density = Const*total(denergy*(energy^(-1.5D))*sumdata)
density = Const*total(denergy*(energy^(-1.5D))*data*domega)
; units are 1/cm^3

return, density
end

