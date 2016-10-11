;+
;PROCEDURE: PREP_EFLUX_DATA,data
;PURPOSE:
;   Gets 2d data as energy spectra, organizes bins, etc.
;INPUTS:
;   data   - structure containing 2d data  (obtained from get_fa_??() routine)
;		e.g. "get_fa_ees, get_fa_ies, etc."
;KEYWORDS:
;	UNITS  - convert to given data units before plotting
;	RETRACE - set to number of retrace steps removed, 
;		- typically set to 1 for FAST esas
;		- minus number will remove -(retrace) steps from end of sweep
;	VEL - If set, x-axis is velocity km/s  -- Default is Energy (eV) 
;
;	ANGLE:		intarr(n)		n=1 nearest angle to be plotted
;						n=2 angle range to be plotted
;						n>2 angle list to be plotted
;	ARANGE:		intarr(2)		angle bin range to be plotted
;	BINS - 		bytarr(dat.nbins)	bins to be plotted  
;	NO_SORT - if set will prevent sorting by angle bin
;
;CREATED BY:	S. Hatch	2016-07-14
;FILE:  prep_eflux_data.pro
;VERSION 1
;LAST MODIFICATION: 2016/07/14
;MOD HISTORY:
;		2016/07/14	Ripped from spec2d.pro
;-
FUNCTION PREP_EFLUX_DATA, $
   tempdat,   $
   CALIB=calib, $
   UNITS=units,   $          
   RETRACE=retrace, $
   VEL=vel, $
   ANGLE=an, $
   ARANGE=ar, $
   BINS=bins,     $
   NO_SORT=no_sort
  
  COMPILE_OPT idl2

  IF (data_type(tempdat) ne 8) OR (tempdat.valid EQ 0) THEN BEGIN
     print,'Invalid Data'
     RETURN,0
  ENDIF

  IF NOT KEYWORD_SET(units) THEN STR_ELEMENT,limits,'units',VALUE=units

  data3d                      = tempdat

  IF KEYWORD_SET(retrace) THEN BEGIN
     IF retrace GT 0 THEN BEGIN
        tmpI                  = [0:retrace-1]
        data3d.data[tmpI,*]   = 0. 
     ENDIF ELSE BEGIN
        tempI                 = [data3d.nenergy+retrace:data3d.nenergy-1]
        data3d.data[tempI,*]  = 0.
     ENDELSE
  ENDIF

  STR_ELEMENT,data3d,'ddata',VALUE=ddata

  IF NOT KEYWORD_SET(ddata) THEN BEGIN
     data3d                   = CONV_UNITS(data3d,'counts')
     ADD_STR_ELEMENT,data3d,'ddata',(data3d.data)^.5
     data3d                   = CONV_UNITS(data3d,units)
  ENDIF ELSE data3d           = CONV_UNITS(data3d,units)

  ydat                        = data3d.data

  ;; IF KEYWORD_SET(error_bars) THEN BEGIN
  dydat                       = data3d.ddata 
  ;; ENDIF ELSE BEGIN
  ;;    dydat                 = data3d.data
  ;; ENDELSE

  ;; IF NDIMEN(data3d.theta) eq 2 THEN BEGIN
  thetaDim                    = NDIMEN(data3d.theta)
  theta                       = data3d.theta
  geom                        = data3d.geom
  ;; theta                    = REFORM(data3d.theta[FIX(data3d.nenergy/2),*]) 
  ;; ENDIF ELSE BEGIN
  ;;    theta                 = data3d.theta
  ;; ENDELSE
  theta                       = ((360.*(theta/360.-FLOOR(theta/360.)) + 180.) MOD 360.) - 180.
  ;; theta                       = ((360.*(theta/360.-FLOOR(theta/360.)) + 180.) MOD 360.)

  IF NDIMEN(geom) LT thetaDim THEN BEGIN
     geom = REPLICATE(1.,N_ELEMENTS(theta[*,0]))#geom
  ENDIF

  ;; IF not KEYWORD_SET(vel) THEN STR_ELEMENT,limits,'velocity',VALUE=vel

  IF KEYWORD_SET(vel) THEN BEGIN
     xdat                     = VELOCITY(data3d.energy,data3d.mass)
  ENDIF ELSE BEGIN
     xdat                     = data3d.energy
  endelse

  bins2                       = REPLICATE(1b,data3d.nbins)

  ;;Handle angle ranges
  IF KEYWORD_SET(an) THEN BEGIN
     IF NDIMEN(an) gt 1 THEN BEGIN
        PRINT,'Error - angle keyword must be fltarr(n)'
     ENDIF ELSE BEGIN
        IF DIMEN1(an) EQ 1 THEN BEGIN
           bins2        = ANGLE_TO_BINS(data3d,[an,an])
        ENDIF
        IF DIMEN1(an) EQ 2 THEN  BEGIN
           bins2        = ANGLE_TO_BINS(data3d,an)
        ENDIF
        IF DIMEN1(an) GT 2 THEN BEGIN 
           ibin         = ANGLE_TO_BIN(data3d,an)
           bins2[*]     = 0 
           bins2[ibin]  = 1
        ENDIF
     ENDELSE
  ENDIF

  ;;Handle angle ranges as bins
  IF KEYWORD_SET(ar) THEN BEGIN
     bins2[*]                        = 0
     IF ar[0] gt ar[1] THEN BEGIN
        bins2[ar[0]:data3d.nbins-1]  = 1
        bins2[0:ar[1]]               = 1
     ENDIF ELSE BEGIN
        bins2[ar[0]:ar[1]]           = 1
     ENDELSE
  ENDIF

  ;;...or handle bins as directly specified
  IF KEYWORD_SET(bins) THEN BEGIN
     bins2              = bins
  ENDIF

  ;; Sort data so angle increases with index number
  IF NOT KEYWORD_SET(no_sort) AND data3d.nbins GT 2 THEN BEGIN
     minvar             = MIN(theta,indminvar)
     IF (indminvar GT 1) THEN BEGIN
        CASE thetaDim OF
           1: BEGIN
              theta1    = theta[0]
              theta2    = theta[1]
           END
           2: BEGIN
              theta1    = theta[0,0]
              theta2    = theta[0,1]
           END
        ENDCASE
        IF theta1 GT theta2 THEN BEGIN
           xdat         = REVERSE(xdat,2)
           ydat         = REVERSE(ydat,2)
           dydat        = REVERSE(dydat,2)
           bins2        = REVERSE(bins2)
           CASE thetaDim OF
              1: BEGIN
                 theta  = REVERSE(theta)
                 geom   = REVERSE(geom)
              END
              2: BEGIN
                 theta  = REVERSE(theta,2)
                 CASE 1 OF
                    KEYWORD_SET(calib): BEGIN
                       geom   = REVERSE(geom,2)
                    END
                    ELSE: BEGIN
                       geom   = geom
                    END
                 ENDCASE
              END
           ENDCASE
        ENDIF
     ENDIF ELSE BEGIN
        CASE thetaDim OF
           1: BEGIN
              theta1    = theta[2]
              theta2    = theta[3]
           END
           2: BEGIN
              theta1    = theta[0,2]
              theta2    = theta[0,3]
           END
        ENDCASE
        IF theta1 GT theta2 THEN BEGIN
           xdat         = REVERSE(xdat,2)
           ydat         = REVERSE(ydat,2)
           dydat        = REVERSE(dydat,2)
           bins2        = REVERSE(bins2)
           CASE thetaDim OF
              1: theta  = REVERSE(theta)
              2: theta  = REVERSE(theta,2)
           ENDCASE
        ENDIF
     ENDELSE

     minvar             = MIN(theta,indminvar)

     xdat               = TRANSPOSE(xdat)
     ydat               = TRANSPOSE(ydat)
     dydat              = TRANSPOSE(dydat)

     xdat               = SHIFT(xdat,-indminvar,0)
     ydat               = SHIFT(ydat,-indminvar,0)
     dydat              = SHIFT(dydat,-indminvar,0)
     bins2              = SHIFT(bins2,-indminvar)

     CASE thetaDim OF
        1: BEGIN
           theta        = SHIFT(theta,-indminvar)
           geom         = SHIFT(geom ,-indminvar)
        END
        2: BEGIN
           theta        = TRANSPOSE(theta)
           theta        = SHIFT(theta,-indminvar,0)
           theta        = TRANSPOSE(theta)

           CASE 1 OF
              KEYWORD_SET(calib): BEGIN
                 geom        = TRANSPOSE(geom)
                 geom        = SHIFT(geom,-indminvar,0)
                 geom        = TRANSPOSE(geom)
              END
              ELSE: BEGIN
                 geom        = SHIFT(geom ,-indminvar)
              END
           ENDCASE
        END
     ENDCASE
     

     xdat               = TRANSPOSE(xdat)
     ydat               = TRANSPOSE(ydat)
     dydat              = TRANSPOSE(dydat)
  ENDIF

  i                     = WHERE(bins2,count)
  ydat                  = ydat[*,i]
  dydat                 = dydat[*,i]
  xdat                  = xdat[*,i]
  CASE thetaDim OF
     1: BEGIN
        theta           = theta[i]
        geom            = geom[i]
     END
     2: BEGIN
        theta           = theta[*,i]
        CASE 1 OF
           KEYWORD_SET(calib): BEGIN
              geom      = geom[*,i]

           END
           ELSE: BEGIN
              geom      = geom[i]
           END
        ENDCASE
     END
  ENDCASE


;; IF arg_present(out_fastStr) THEN BEGIN

  out_fastStr           = {data_name:data3d.data_name, $
                           valid:data3d.valid, $
                           project_name:data3d.project_name, $
                           units_name:data3d.units_name, $
                           units_procedure:data3d.units_procedure, $
                           time:data3d.time, $
                           end_time:data3d.end_time, $
                           integ_t:data3d.integ_t, $
                           nbins:N_ELEMENTS(bins2), $
                           nenergy:data3d.nenergy, $
                           data:ydat, $
                           ddata:dydat, $
                           energy:xdat, $
                           ;; complete_theta:data3d.theta[*,i], $
                           theta:theta, $
                           geom:geom, $
                           denergy:data3d.denergy[*,i], $
                           dtheta:data3d.dtheta[i], $
                           eff:data3d.eff, $
                           mass:data3d.mass, $
                           geomfactor:data3d.geomfactor, $
                           header_bytes:data3d.header_bytes, $
                           st_index:data3d.st_index, $
                           en_index:data3d.en_index, $
                           npts:data3d.npts, $
                           index:data3d.index}

;; ENDIF

  RETURN,out_fastStr
END
