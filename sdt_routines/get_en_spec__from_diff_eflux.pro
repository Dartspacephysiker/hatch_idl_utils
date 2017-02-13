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
;	CALIB:		Calib keyword passed on to get_"get_dat"_ts
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

FUNCTION GET_EN_SPEC__FROM_DIFF_EFLUX,dat,  $
                                      T1=t1, $
                                      T2=t2, $
                                      ;;	ENERGY=en, $
                                      ;;	ERANGE=er, $
                                      ;;	EBINS=ebins, $
                                      ANGLE=an, $
                                      ARANGE=ar, $
                                      BINS=bins, $
                                      gap_time=gap_time, $ 
                                      no_data=no_data, $
                                      units = units,  $
                                      name  = name, $
                                      bkg = bkg, $
                                      missing = missing, $
                                      floor = floor, $
                                      retrace = retrace, $
                                      CALIB = calib

  COMPILE_OPT idl2
  
  ;;	Time how long the routine takes
  ex_start = SYSTIME(1)

  ;;	Set defaults for keywords, etc.
  ;; n        = 0
  max      = N_ELEMENTS(dat.data_name)
  all_same = 1

  ;; ytitle  = data_str + '_en_spec'
  nbins   = dat.nbins[0]
  nmaxvar = dat.nenergy

  default_gap_time = 200.
  IF dat.project_name[0] EQ 'FAST' THEN BEGIN
     nmaxvar = 96
     default_gap_time = 8.
  ENDIF
  IF NOT KEYWORD_SET(gap_time) THEN gap_time = default_gap_time

  time   = DBLARR(max)
  data   = FLTARR(max,nmaxvar)
  var    = FLTARR(max,nmaxvar)
  nvar   = dat.nenergy
  nmax   = nvar

  units  = dat.units_name
  IF NOT KEYWORD_SET(units)   THEN   units = 'Counts'
  IF NOT KEYWORD_SET(missing) THEN missing = !values.f_nan

  last_time = dat.time[0]
  ;;	Collect the data - Main Loop
  n = 0
  FOR k=0,max-1 DO BEGIN

     IF dat.valid[k] EQ 0 THEN BEGIN
        time[k]   = (last_time) + dbadtime
        data[k,*] = missing
        var[k,*]  = missing

        PRINT,'Invalid packet, dat.valid ne 1, at: ',TIME_TO_STR(dat.time[k])

        CONTINUE
     ENDIF

     count = dat.nbins[k]
     IF KEYWORD_SET(an) THEN bins = ANGLE_TO_BINS(REDUCE_SYNTH_DIFF_EFLUX_STRUCT(dat,k,/NO_SHIFTVALS),an)
     if KEYWORD_SET(ar) THEN BEGIN
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
     IF NOT KEYWORD_SET(bins) THEN ind = INDGEN(dat.nbins[k]) ELSE ind = WHERE(bins,count)
     IF units EQ 'Counts' THEN norm = 1 ELSE norm = count

     IF ABS((dat.time[k]+dat.end_time[k])/2.-last_time) GE gap_time THEN BEGIN
        IF k GE 2 THEN dbadtime = time[k-1] - time[k-2] else dbadtime = gap_time/2.
        time[k]   = (last_time) + dbadtime
        data[k,*] = missing
        var[k,*]  = missing
        IF (dat.time[k]+dat.end_time[k])/2. GT time[k-1] + gap_time THEN BEGIN
           time[k]   = (dat.time[k]+dat.end_time[k])/2. - dbadtime
           data[k,*] = missing
           var[k,*]  = missing
           n=n+1
        endif
     endif

     IF KEYWORD_SET(bkg  ) THEN dat = SUB3D(dat,bkg)
     IF KEYWORD_SET(units) THEN dat = CONV_UNITS(dat,units)

     nvar = dat.nenergy
     IF nvar GT nmax THEN nmax = nvar
     time[k]  = (dat.time[k]+dat.end_time[k])/2.
     IF ind[0] NE -1 THEN BEGIN
        data[k,0:nvar-1]  = TOTAL( dat.data[*,ind,k], 2)/norm
        var[k,0:nvar-1]   = TOTAL( dat.energy[*,ind,k], 2)/count
     ENDIF ELSE BEGIN
        data[k,0:nvar-1]  = 0
        var[k,0:nvar-1]   = TOTAL( dat.energy[*,ind,k], 2)
     endelse

; test the following lines, the 96-6-19 version of tplot did not work with !values.f_nan
;	if nvar lt nmaxvar THEN data[k,nvar:nmaxvar-1) = !values.f_nan
;	if nvar lt nmaxvar THEN var[k,nvar:nmaxvar-1) = !values.f_nan
     if nvar lt nmaxvar THEN data[k,nvar:nmaxvar-1] = data[k,nvar-1]
     if nvar lt nmaxvar THEN var[k,nvar:nmaxvar-1] = 1.5*var[k,nvar-1]-.5*var[k,nvar-2]

     IF (all_same EQ 1) THEN BEGIN
        IF DIMEN1(WHERE(var[k,0:nvar-1] NE var[0,0:nvar-1])) GT 1 THEN all_same = 0
     ENDIF
     last_time = time[k]

     n++
  ENDFOR

  ;;	Store the data
  ;; IF count NE nbins THEN ytitle = ytitle+'_'+STRTRIM(count,2)
  ;; IF ~KEYWORD_SET(name) THEN name = ytitle else ytitle = name
  ;; ytitle = ytitle+' ('+units+')'

  IF NOT KEYWORD_SET(retrace) THEN BEGIN
;	If you want to plot the retrace, set the retrace flag to 1.
     data = data[0:k-1,0:nmax-1]
     var  = var[0:k-1,0:nmax-1]
  ENDIF ELSE BEGIN
     data = data[0:k-1,retrace:nmax-1]
     var  = var[0:k-1,retrace:nmax-1]
  ENDELSE

  PRINT,'all_same=',all_same

  IF KEYWORD_SET(t1) THEN BEGIN
     ind  = WHERE(time ge t1)
     time = TIME[ind]
     data = data[ind,*]
     var  = var[ind,*]
  ENDIF
  IF KEYWORD_SET(t2) THEN BEGIN
     ind  = WHERE(time LE t2)
     time = time[ind]
     data = data[ind,*]
     var  = var[ind,*]
  endif

  datastr = {x:time,y:data,v:var}

  ex_time = SYSTIME(1) - ex_start
  MESSAGE,STRING(ex_time)+' seconds execution time.',/cont,/info
  PRINT,'Number of data points = ',n

  RETURN,dataStr

END
