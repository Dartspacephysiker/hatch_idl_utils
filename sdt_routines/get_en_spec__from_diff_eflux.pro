;+
;PROCEDURE:	get_en_spec
;PURPOSE:	
;	Generates energy-time spectrogram data structures for tplot
;INPUT:		
;	data_str, 	a string (either 'eh','pl','eesa_surv','ess', ...)
;			where get_'string' returns a 2D or 3D 
;			data structure
;KEYWORDS:
;	T1:		start time, seconds since 1970
;	T2:		end time, seconds since 1970		
;	ANGLE:		fltarr(2),fltarr(4)	angle range to sum over
;	ARANGE:		intarr(2)		bin range to sum over
;	BINS:		bytarr(dat.nbins)	bins to sum over
;	gap_time: 	time gap big enough to signify a data gap 
;			(default 200 sec, 8 sec for FAST)
;	NO_DATA: 	returns 1 if no_data else returns 0
;	UNITS:		convert to these units if included
;	NAME:  		New name of the Data Quantity
;	BKG:  		A 3d data structure containing the background counts.
;	FLOOR:  	Sets the minimum value of any data point to sqrt(bkg).
;	MISSING: 	value for bad data.
;	RETRACE: 	Set to number of retrace energy steps to be eliminated starting at energy step 0
;
;
;
;CREATED BY:	J.McFadden
;VERSION:	1
;LAST MODIFICATION:  97/03/04
;MOD HISTORY:
;		97/03/04	T1,T2 keywords added
;		97/05/22	CALIB keyword added
;
;
;NOTES:	  
;	Current version only works for FAST
;-

FUNCTION GET_EN_SPEC__FROM_DIFF_EFLUX,diff_eFlux,  $
                                      T1=t1, $
                                      T2=t2, $
                                      ;;	ENERGY=en, $
                                      ;;	ERANGE=er, $
                                      ;;	EBINS=ebins, $
                                      ANGLE=an, $
                                      ARANGE=ar, $
                                      BINS=bins, $
                                      GAP_TIME=gap_time, $ 
                                      NO_DATA=no_data, $
                                      UNITS=units,  $
                                      BKG=bkg, $
                                      MISSING=missing, $
                                      FLOOR=floor, $
                                      RETRACE=retrace, $
                                      STORE=store, $
                                      NAME=name, $
                                      IS_MCFADDEN_DIFF_EFLUX=is_McFadden_diff_eFlux, $
                                      OUT_AVGFACTORARR=avgFactorArr, $
                                      OUT_NORMARR=normArr

  COMPILE_OPT IDL2,STRICTARRSUBS
  
  ;;	Time how long the routine takes
  ex_start = SYSTIME(1)

  ;;	Set defaults for keywords, etc.
  ;; n        = 0
  max      = N_ELEMENTS(diff_eFlux.data_name)
  all_same = 1

  ;; ytitle  = data_str + '_en_spec'
  nbins   = diff_eFlux.nbins[0]
  nmaxvar = diff_eFlux.nenergy

  default_gap_time = 200.
  projName = ""
  ;; STR_ELEMENT,diff_eFlux,"project_name",projName
  ;; IF projName[0] EQ 'FAST' THEN BEGIN
     nmaxvar = 96
     default_gap_time = 8.
  ;; ENDIF
  IF NOT KEYWORD_SET(gap_time) THEN gap_time = default_gap_time

  time   = DBLARR(max)
  data   = FLTARR(max,nmaxvar)
  ddata  = FLTARR(max,nmaxvar)
  var    = FLTARR(max,nmaxvar)
  dvar   = FLTARR(max,nmaxvar)
  nvar   = is_McFadden_diff_eFlux ? diff_eFlux[0].nenergy : diff_eFlux.nenergy
  nmax   = nvar

  avgFactorArr = LONARR(max)
  normArr      = LONARR(max)

  IF NOT KEYWORD_SET(units) THEN BEGIN
     units  = is_McFadden_diff_eFlux ? diff_eFlux[0].units_name : diff_eFlux[0].units_name[0] ;Added [0] on 2018/07/20 because of McFadden-style cleaning
     ;; units = 'Counts'
  ENDIF
  IF NOT KEYWORD_SET(missing) THEN missing = !VALUES.F_NaN

  last_time = (is_McFadden_diff_eFlux ? diff_eFlux[0].time : diff_eFlux.time[0])

  ;; avgFactorArr = !NULL
  ;; normArr  = !NULL
  ;;	Collect the data - Main Loop
  ;; n = 0
  FOR k=0,max-1 DO BEGIN

     dat = MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX( $
           diff_eFlux,k, $
           IS_MCFADDEN_DIFF_EFLUX=is_McFadden_diff_eFlux)

     ;; IF diff_eFlux.valid[k] EQ 0 THEN BEGIN
     IF dat.valid EQ 0 THEN BEGIN
        IF k GE 2 THEN dbadtime = time[k-1] - time[k-2] else dbadtime = gap_time/2.
        time[k]    = (last_time) + dbadtime
        data[k,*]  = missing
        ddata[k,*] = missing
        var[k,*]   = missing
        dvar[k,*]  = missing

        ;; PRINT,'Invalid packet, diff_eFlux.valid ne 1, at: ',TIME_TO_STR(diff_eFlux.time[k])
        PRINT,'Invalid packet, diff_eFlux.valid ne 1, at: ',TIME_TO_STR(dat.time)

        CONTINUE
     ENDIF

     ;; count = diff_eFlux.nbins[k]
     count = dat.nbins
     ;; IF KEYWORD_SET(an) THEN bins = ANGLE_TO_BINS(MAKE_SDT_STRUCT_FROM_PREPPED_EFLUX(dat,k),an)
     IF KEYWORD_SET(an) THEN bins = ANGLE_TO_BINS(dat,an)
     if KEYWORD_SET(ar) THEN BEGIN
        ;; nb   = diff_eFlux.nbins
        nb   = dat.nbins
        bins = BYTARR(nb)
        IF ar[0] GT ar[1] THEN BEGIN
           bins[ar[0]:nb-1]  = 1
           bins[0:ar[1]]     = 1
        ENDIF ELSE BEGIN
           bins[ar[0]:ar[1]] = 1
        ENDELSE
     ENDIF

     ;; Set the "count" to the number of bins summed over
     ;; IF NOT KEYWORD_SET(bins) THEN ind = INDGEN(diff_eFlux.nbins[k]) ELSE ind = WHERE(bins,count)
     IF NOT KEYWORD_SET(bins) THEN ind = INDGEN(dat.nbins) ELSE ind = WHERE(bins,count)
     IF units EQ 'Counts' THEN norm = 1 ELSE norm = count


     ;;Skip the ones with a big gap
     ;; IF ABS((diff_eFlux.time[k]+diff_eFlux.end_time[k])/2.-last_time) GE gap_time THEN BEGIN
     IF ABS((dat.time+dat.end_time)/2.-last_time) GE gap_time THEN BEGIN
        IF k GE 2 THEN dbadtime = time[k-1] - time[k-2] else dbadtime = gap_time/2.
        time[k]    = (last_time) + dbadtime
        data[k,*]  = missing
        ddata[k,*] = missing
        var[k,*]   = missing
        dvar[k,*]  = missing
        ;; IF (diff_eFlux.time[k]+diff_eFlux.end_time[k])/2. GT time[k-1] + gap_time THEN BEGIN
        IF (dat.time+dat.end_time)/2. GT time[k-1] + gap_time THEN BEGIN
           ;; time[k]   = (diff_eFlux.time[k]+diff_eFlux.end_time[k])/2. - dbadtime
           time[k]    = (dat.time+dat.end_time)/2. - dbadtime
           data[k,*]  = missing
           ddata[k,*] = missing
           var[k,*]   = missing
           dvar[k,*]  = missing
           k++
        endif
     endif

     ;; avgFactorArr = [avgFactorArr,count]
     ;; normArr  = [normArr,norm]
     avgFactorArr[k] = count
     normArr[k]      = norm

     IF KEYWORD_SET(bkg  ) THEN dat = SUB3D(dat,bkg)
     IF KEYWORD_SET(units) THEN dat = CONV_UNITS(dat,units)

     ;; nvar = diff_eFlux.nenergy
     nvar = dat.nenergy
     IF nvar GT nmax THEN nmax = nvar
     ;; time[k]  = (diff_eFlux.time[k]+diff_eFlux.end_time[k])/2.
     time[k]  = (dat.time+dat.end_time)/2.
     IF ind[0] NE -1 THEN BEGIN
        ;; data[k,0:nvar-1]  = TOTAL( diff_eFlux.data[*,ind,k], 2)/norm
        ;; var[k,0:nvar-1]   = TOTAL( diff_eFlux.energy[*,ind,k], 2)/count
        data[k,0:nvar-1]  = TOTAL( dat.data[*,ind],  2)/norm
        ddata[k,0:nvar-1] = TOTAL( dat.ddata[*,ind], 2)/norm
        var[k,0:nvar-1]   = TOTAL( dat.energy[*,ind],2)/count
        dvar[k,0:nvar-1]  = TOTAL( dat.denergy[*,ind],2)/count
     ENDIF ELSE BEGIN
        data[k,0:nvar-1]  = 0
        ddata[k,0:nvar-1] = 0
        ;; var[k,0:nvar-1]   = TOTAL( diff_eFlux.energy[*,ind,k], 2)
        var[k,0:nvar-1]   = TOTAL( dat.energy[*,ind], 2)
        dvar[k,0:nvar-1]  = TOTAL( dat.denergy[*,ind], 2)
     endelse

; test the following lines, the 96-6-19 version of tplot did not work with !values.f_nan
;	if nvar lt nmaxvar THEN data[k,nvar:nmaxvar-1) = !values.f_nan
;	if nvar lt nmaxvar THEN var[k,nvar:nmaxvar-1) = !values.f_nan
     ;; if nvar lt nmaxvar THEN data[k,nvar:nmaxvar-1] = data[k,nvar-1]
     ;; if nvar lt nmaxvar THEN var[k,nvar:nmaxvar-1] = 1.5*var[k,nvar-1]-.5*var[k,nvar-2]
     IF nvar LT nmaxvar THEN BEGIN
        data[k,nvar:nmaxvar-1]  = data[k,nvar-1]
        ddata[k,nvar:nmaxvar-1] = ddata[k,nvar-1]
        var[k,nvar:nmaxvar-1]   = 1.5*var[k,nvar-1]-.5*var[k,nvar-2]
        dvar[k,nvar:nmaxvar-1]  = 1.5*dvar[k,nvar-1]-.5*dvar[k,nvar-2]
     ENDIF

     IF (all_same EQ 1) THEN BEGIN
        IF DIMEN1(WHERE(var[k,0:nvar-1] NE var[0,0:nvar-1])) GT 1 THEN all_same = 0
     ENDIF
     last_time = time[k]

  ENDFOR

  IF NOT KEYWORD_SET(retrace) THEN BEGIN
;	If you want to plot the retrace, set the retrace flag to 1.
     data  = data[0:k-1,0:nmax-1]
     ddata = ddata[0:k-1,0:nmax-1]
     var   = var[0:k-1,0:nmax-1]
     dvar  = dvar[0:k-1,0:nmax-1]
  ENDIF ELSE BEGIN
     data  = data[0:k-1,retrace:nmax-1]
     ddata = ddata[0:k-1,retrace:nmax-1]
     var   = var[0:k-1,retrace:nmax-1]
     dvar  = dvar[0:k-1,retrace:nmax-1]
  ENDELSE

  PRINT,'all_same=',all_same

  IF KEYWORD_SET(t1) THEN BEGIN
     ind   = WHERE(time ge t1)
     time  = TIME[ind]
     data  = data[ind,*]
     ddata = ddata[ind,*]
     var   = var[ind,*]
     dvar  = dvar[ind,*]
  ENDIF
  IF KEYWORD_SET(t2) THEN BEGIN
     ind   = WHERE(time LE t2)
     time  = time[ind]
     data  = data[ind,*]
     ddata = ddata[ind,*]
     var   = var[ind,*]
     dvar  = dvar[ind,*]
  endif

  datastr = {x:time,y:data,v:var,yerr:ddata,verr:dvar}

  IF KEYWORD_SET(store) THEN BEGIN
     
     IF (diff_eflux.mass GT 5.7D-06) THEN BEGIN

        navn =  'Ion'

        lims = [4,8]
        edg  = [4,2.4D4]

     ENDIF ELSE BEGIN

        navn = 'Electron'

        lims = [5,9]
        edg  = [4,3D4]

     ENDELSE
     ;;	Store the data
     ;; IF count NE nbins THEN ytitle = ytitle+'_'+STRTRIM(count,2)
     ;; IF ~KEYWORD_SET(name) THEN name = ytitle else ytitle = name
     ;; ytitle = ytitle+' ('+units+')'
     STORE_DATA,name,DATA=datastr

     OPTIONS,name,'spec',1	
     ZLIM,name, $
          10.^(MIN(ALOG10(dataStr.y[WHERE(FINITE(dataStr.y))])) > lims[0] ), $
          10.^(MAX(ALOG10(dataStr.y[WHERE(FINITE(dataStr.y))])) < lims[1]), $
          1

     YLIM,name,edg[0],edg[1],1
     ;; OPTIONS,name,'ytitle',navn+'!C!CEnergy (eV)'
     OPTIONS,name,'ytitle',name+' (eV)'
     OPTIONS,name,'ztitle','Log eV!C/cm!U2!N-s-sr-eV'
     OPTIONS,name,'x_no_interp',1
     OPTIONS,name,'y_no_interp',1
     OPTIONS,name,'panel_size',2
     OPTIONS,name,'ztickformat','exponentlabel'

  ENDIF


  ex_time = SYSTIME(1) - ex_start
  MESSAGE,STRING(ex_time)+' seconds execution time.',/cont,/info
  PRINT,'Number of data points = ',k

  RETURN,dataStr

END
