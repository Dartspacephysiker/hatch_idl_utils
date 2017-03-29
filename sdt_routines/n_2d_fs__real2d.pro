;2017/03/29
function n_2d_fs__real2d,dat,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins

if dat.valid eq 0 then begin
   print,'Invalid Data'
   density = 0.
   return, density
endif

if dat.nbins eq 32 or dat.project_name ne 'FAST' then begin
   return, n_2d_b__real2d(dat,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
endif

ind1=findgen(32)*2
ind2=ind1+1
if ndimen(dat.geom) eq 1 then begin
   geom1=dat.geom[ind1] 
   geom2=dat.geom[ind2] 
endif else begin
   geom1=dat.geom[*,ind1]
   geom2=dat.geom[*,ind2]
endelse
if ndimen(dat.dtheta) eq 1 then begin
   dtheta1=dat.dtheta[ind1]*2. 
   dtheta2=dat.dtheta[ind2]*2. 
endif else begin
   dtheta1=dat.dtheta[*,ind1]*2.
   dtheta2=dat.dtheta[*,ind2]*2.
endelse

dat1 = 		{data_name:		dat.data_name, 			$
                 valid: 			1, 				$
                 project_name:		dat.project_name, 		$
                 units_name: 		dat.units_name, 		$
                 units_procedure: 	dat.units_procedure, 		$
                 time: 			dat.time, 			$
                 end_time: 		dat.end_time, 			$
                 integ_t: 		dat.integ_t,			$
                 nbins: 			32, 				$
                 nenergy: 		dat.nenergy, 			$
                 data: 			dat.data[*,ind1], 		$
                 energy: 		dat.energy[*,ind1], 		$
                 theta: 			dat.theta[*,ind1],  		$
                 geom: 			geom1, 	 			$
                 denergy: 		dat.denergy[*,ind1],       	$
                 dtheta: 		dtheta1, 			$
                 eff: 			dat.eff,	 		$
                 mass: 			dat.mass, 			$
                 geomfactor: 		dat.geomfactor, 		$
                 header_bytes: 		dat.header_bytes}


dat2 = 		{data_name:		dat.data_name, 			$
                 valid: 			1, 				$
                 project_name:		dat.project_name, 		$
                 units_name: 		dat.units_name, 		$
                 units_procedure: 	dat.units_procedure, 		$
                 time: 			dat.time, 			$
                 end_time: 		dat.end_time, 			$
                 integ_t: 		dat.integ_t,			$
                 nbins: 			32, 				$
                 nenergy: 		dat.nenergy, 			$
                 data: 			dat.data[*,ind2], 		$
                 energy: 		dat.energy[*,ind2], 		$
                 theta: 			dat.theta[*,ind2],  		$
                 geom: 			geom2, 	 			$
                 denergy: 		dat.denergy[*,ind2],       	$
                 dtheta: 		dtheta2, 			$
                 eff: 			dat.eff,	 		$
                 mass: 			dat.mass, 			$
                 geomfactor: 		dat.geomfactor, 		$
                 header_bytes: 		dat.header_bytes}


;	Note that the EBINS, ARANGE, and BINS keywords below may not work properly.

n1=n_2d_b__real2d(dat1,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
n2=n_2d_b__real2d(dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)

n_avg=(n1+n2)/2.

return,n_avg

END