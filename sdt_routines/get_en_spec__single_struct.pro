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
;LAST MODIFICATION:  2017/12/27
;MOD HISTORY:
;		1997/03/04	T1,T2 keywords added
;		1997/05/22	CALIB keyword added
;               2017/12/27      Barn av GET_EN_SPEC__DIFF_EFLUX (SMH) 
;
;NOTES:	  
;	Current version only works for FAST
;-

FUNCTION GET_EN_SPEC__SINGLE_STRUCT,dat,  $
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
                                    OUT_AVGFACTORARR=avgFactorArr, $
                                    OUT_NORMARR=normArr

  COMPILE_OPT IDL2,STRICTARRSUBS
  
  ;;	Time how long the routine takes
  ex_start = SYSTIME(1)

  ;;	Set defaults for keywords, etc.
  ;;Max is used in GET_EN_SPEC__DIFF_EFLUX to carry around the number of structs
  max      = 1                  

  ;; ytitle  = data_str + '_en_spec'
  nbins   = dat.nbins
  nmaxvar = dat.nenergy

  default_gap_time = 200.
  IF dat.project_name EQ 'FAST' THEN BEGIN
     nmaxvar = 96
     default_gap_time = 8.
  ENDIF
  IF NOT KEYWORD_SET(gap_time) THEN gap_time = default_gap_time

  time   = DBLARR(max)
  data   = FLTARR(max,nmaxvar)
  ddata  = FLTARR(max,nmaxvar)
  var    = FLTARR(max,nmaxvar)
  dvar   = FLTARR(max,nmaxvar)
  nvar   = dat.nenergy
  nmax   = nvar

  avgFactorArr = LONARR(max)
  normArr      = LONARR(max)

  IF NOT KEYWORD_SET(units) THEN BEGIN
     units  = dat.units_name
     ;; units = 'Counts'
  ENDIF
  IF NOT KEYWORD_SET(missing) THEN missing = !VALUES.F_NaN

  ;; avgFactorArr = !NULL
  ;; normArr  = !NULL
  ;;	Collect the data - Main Loop
  ;; n = 0
  ;; FOR k=0,max-1 DO BEGIN

     IF dat.valid EQ 0 THEN BEGIN
        time    = (dat.time[0]) + dbadtime
        data[*]  = missing
        ddata[*] = missing
        var[*]   = missing
        dvar[*]  = missing

        PRINT,'Invalid packet, dat.valid ne 1, at: ',TIME_TO_STR(dat.time)

        RETURN,-1
     ENDIF

     count = dat.nbins
     IF KEYWORD_SET(an) THEN bins = ANGLE_TO_BINS(dat,an)
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

     IF NOT KEYWORD_SET(bins) THEN ind = INDGEN(dat.nbins) ELSE ind = WHERE(bins,count)
     IF STRUPCASE(units) EQ 'Counts' THEN norm = 1 ELSE norm = count

     ;; avgFactorArr = [avgFactorArr,count]
     ;; normArr  = [normArr,norm]
     avgFactorArr = count
     normArr      = norm

     IF KEYWORD_SET(bkg  ) THEN dat = SUB3D(dat,bkg)
     IF KEYWORD_SET(units) THEN IF STRUPCASE(dat.units_name) NE STRUPCASE(units) THEN dat = CONV_UNITS(dat,units)

     nvar = dat.nenergy
     IF nvar GT nmax THEN nmax = nvar
     time  = (dat.time+dat.end_time)/2.
     IF ind[0] NE -1 THEN BEGIN
        data[0:nvar-1]  = TOTAL( dat.data[0:dat.nEnergy-1,ind],  2)/norm
        ddata[0:nvar-1] = TOTAL( dat.ddata[0:dat.nEnergy-1,ind], 2)/norm
        var[0:nvar-1]   = TOTAL( dat.energy[0:dat.nEnergy-1,ind],2)/count
        dvar[0:nvar-1]  = TOTAL( dat.denergy[0:dat.nEnergy-1,ind],2)/count
     ENDIF ELSE BEGIN
        data[0:nvar-1]  = 0
        ddata[0:nvar-1] = 0
        var[0:nvar-1]   = TOTAL( dat.energy[0:dat.nEnergy-1,ind], 2)
        dvar[0:nvar-1]  = TOTAL( dat.denergy[0:dat.nEnergy-1,ind], 2)
     endelse

; test the following lines, the 96-6-19 version of tplot did not work with !values.f_nan
;	if nvar lt nmaxvar THEN data[nvar:nmaxvar-1) = !values.f_nan
;	if nvar lt nmaxvar THEN var[nvar:nmaxvar-1) = !values.f_nan
     ;; if nvar lt nmaxvar THEN data[nvar:nmaxvar-1] = data[nvar-1]
     ;; if nvar lt nmaxvar THEN var[nvar:nmaxvar-1] = 1.5*var[nvar-1]-.5*var[nvar-2]
     IF nvar LT nmaxvar THEN BEGIN
        data[nvar:nmaxvar-1]  = data[nvar-1]
        ddata[nvar:nmaxvar-1] = ddata[nvar-1]
        var[nvar:nmaxvar-1]   = 1.5*var[nvar-1]-.5*var[nvar-2]
        dvar[nvar:nmaxvar-1]  = 1.5*dvar[nvar-1]-.5*dvar[nvar-2]
     ENDIF

  IF NOT KEYWORD_SET(retrace) THEN BEGIN
;	If you want to plot the retrace, set the retrace flag to 1.
     data  = data[0:nmax-1]
     ddata = ddata[0:nmax-1]
     var   = var[0:nmax-1]
     dvar  = dvar[0:nmax-1]
  ENDIF ELSE BEGIN
     data  = data[retrace:nmax-1]
     ddata = ddata[retrace:nmax-1]
     var   = var[retrace:nmax-1]
     dvar  = dvar[retrace:nmax-1]
  ENDELSE

  datastr = {x:time,y:data,v:var,yerr:ddata,verr:dvar}

  IF KEYWORD_SET(store) THEN BEGIN
     
     IF (dat.mass GT 5.7D-06) THEN BEGIN

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

  RETURN,dataStr

END
