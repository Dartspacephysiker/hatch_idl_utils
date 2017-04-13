;2017/03/29
FUNCTION MOMENTS_2D_NEW,dat2, $
                    ENERGY=en, $
                    ERANGE=er, $
                    EBINS=ebins, $
                    ANGLE=an, $
                    ARANGE=ar, $
                    BINS=bins

  ;; COMPILE_OPT IDL2,STRICTARRSUBS

  common last_pot,pot
  if not keyword_set(pot) then pot=0.

  mass     = dat2.mass     
  Const_n  = (mass)^(-1.5)*(2.)^(.5)
  Const_j  = (mass)^(-2.)*(2.)*(1.e5)
  Const_je = (mass)^(-2.)*(2.)*(1.e5)*1.6e-12
  Const_p  = (mass)^(-2.5)*(2.)^1.5


  moments  = 0.

  if dat2.valid eq 0 then begin
     print,'Invalid Data'
     return, moments
  endif

  dat = conv_units(dat2,"df")   ; Use distribution function
  na = dat.nenergy
  nb = dat.nbins
  
;  Correct for spacecraft potential
;  the following needs modifications to be consistent with FAST

  ;;Get charge
  charge=1.                     ; charge of species
  value=0 & str_element,dat,'charge',value
  if value le 0 or value ge 0 then value=value else value=0
  if value ne 0 then charge=dat.charge		
  if ((value eq 0) and (dat.mass lt 0.00010438871)) then charge=-1. ; this line works for Wind which does not have dat.charge

; The following rotates the measurement in angle to partly account 
;	for s/c potential deflection of low energy electrons
  ;;Now try for potential
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



  ;;Do we need to get pot?
  ;; val = !NULL
  ;; STR_ELEMENT,dat,'sc_pot',val;,/ADD_REPLACE
  ;; IF val EQ !NULL THEN BEGIN

  ;;    GET_DATA,'sc_pot',DATA=sc_pot

  ;;    IF SIZE(sc_pot,/TYPE) EQ 8 THEN BEGIN
        
  ;;       check = VALUE_CLOSEST2(sc_pot.x,dat.time,/CONSTRAINED)

  ;;       IF ABS(sc_pot.x[check]-dat.time) LT 5.0 THEN BEGIN

  ;;          STR_ELEMENT,dat,'sc_pot',sc_pot.y[check],/ADD_REPLACE

  ;;       ENDIF ELSE BEGIN

  ;;          PRINT,"Badness ..."
  ;;          STR_ELEMENT,dat,'sc_pot',0.,/ADD_REPLACE

  ;;       ENDELSE

  ;;    ENDIF ELSE BEGIN

  ;;       PRINT,"Couldn't get sc_pot ..."
  ;;       STR_ELEMENT,dat,'sc_pot',0.,/ADD_REPLACE

  ;;    ENDELSE

  ;; ENDIF
  
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
  
  energy       = dat.energy+(charge*(sc_pot)/abs(charge))	
  emin         = energy-dat.denergy/2.>0.				
  emax         = energy+dat.denergy/2.>0.				
  dat.energy   = (emin+emax)/2.
  dat.denergy  = (emax-emin)

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

  energy   = dat.energy
  denergy  = dat.denergy
  theta    = dat.theta/!radeg
  dtheta   = dat.dtheta/!radeg
  data     = dat.data*bins2

  value=0 & STR_ELEMENT,dat,'domega',value
  ;; if n_elements(value) ne 1 then domega = dat.domega ELSE BEGIN

  domega_zz = OMEGA_ZZ_2D_B(dat2, $
                            ENERGY=en, $
                            ERANGE=er, $
                            EBINS=ebins, $
                            ANGLE=an, $
                            ARANGE=ar, $
                            BINS=bins)

  domega  = OMEGA_2D_B(dat2, $
                       ENERGY=en, $
                       ERANGE=er, $
                       EBINS=ebins, $
                       ANGLE=an, $
                       ARANGE=ar, $
                       BINS=bins)

  ;; ENDELSE
  
  solid_angle_corr=4.*!pi/total(domega[0,*]) ; this should be correct in the structure
  if (solid_angle_corr lt .99 or solid_angle_corr gt 1.01) and max(theta) gt 1.2 then print,'Error in dat.domega.  Solid angle = ', solid_angle_corr   

  ;; units are #/cm^2-sec
  ;; fluxz   = j_2d_new(dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
  ;; density = n_2d_new(dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)

  ;;just flux
  flux    = Const_j  * TOTAL(data*denergy  *energy        *domega   )
  eflux   = Const_je * TOTAL(data*denergy  *energy^2      *domega   )

  ;;Par comps
  fluxz   = Const_j  * TOTAL(data*denergy  *energy        *domega_zz)
  efluxz  = Const_je * TOTAL(data*denergy  *energy^2      *domega_zz)
  density = Const_n  * TOTAL(data*denergy  *(energy^(0.5))*domega   )
  effluxz = Const_je * TOTAL(data*denergy^2*energy^3      *domega_zz)

  ;;Perp comps
  fluxx   = Const_j  * TOTAL(data*denergy  *energy  *domega*sin(theta))
  efluxx  = Const_je * TOTAL(data*denergy  *energy^2*domega*sin(theta))
  effluxx = Const_je * TOTAL(data*denergy^2*energy^3*domega*sin(theta))

  ;;Vel comps
  speed   = flux /density
  velz    = fluxz/density
  velx    = fluxx/density

  ;;See just how much improvement we get for all those fancy field-aligned beam corrections
  ;; testfluxz   = Const_j  * TOTAL(data*denergy*energy*domega*cos(theta))
  ;; testefluxz  = Const_je * TOTAL(data*denergy*energy^2*domega*cos(theta))
  ;; PRINT,(fluxz-testfluxz)/fluxz
  ;; PRINT,(efluxz-testefluxz)/efluxz

  ;; Pressure is in units of eV/cm**3
  p2dxx = Const_p * TOTAL(data*denergy*energy^1.5*domega*(sin(theta))^2)/2.
  p2dyy = p2dxx
  p2dzz = Const_p * TOTAL(data*denergy*energy^1.5*domega*(cos(theta))^2)
  p2dxy = 0.
  p2dxz = 0.
  p2dyz = 0.
  p2dxx = mass*(p2dxx)
  p2dyy = mass*(p2dyy)
  p2dzz = mass*(p2dzz-velz*fluxz/1.e10)
  p2dzz = mass*(p2dzz-velz*fluxz/1.e10)

  press = [p2dxx,p2dyy,p2dzz,p2dxy,p2dxz,p2dyz]

  Tavg = (press(0)+press(1)+press(2))/(density*3.)
  Tx = press(0)/(density)
  Ty = press(1)/(density)
  Tz = press(2)/(density)

  T  = [press(0),press(1),press(2),TOTAL(press)/3.D]/density

  moments = {n     : density  , $
             j     : fluxz    , $
             je    : efluxz   , $
             jje   : effluxz  , $
             p     : press    , $
             T     : T        , $
             v     : velz*1.D-5, $
             charE : efluxz/fluxz*6.242D*1.0D11, $
             perp  : {j     : fluxx, $
                      je    : efluxx, $
                      jje   : effluxx, $
                      v     : velx*1.D-5, $
                      charE : efluxx/fluxx*6.242D*1.0D11}, $
             all   : {speed : speed, $
                      j     : flux, $
                      je    : eflux, $
                      charE : eflux/flux*6.242D*1.0D11}}

  RETURN,moments
  
END
