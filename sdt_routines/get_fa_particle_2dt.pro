;;2016/09/30
PRO GET_FA_PARTICLE_2DT,type,routine, $
   T1=t1, $
   T2=t2, $
   DATA=data, $
   NAME=name, $
   ENERGY=energy, $
   ERANGE=er, $
   EBINS=ebins, $
   ANGLE=an, $
   ARANGE=ar, $
   BINS=bins, $
   GAP_TIME=gap_time, $ 
   NO_DATA=no_data, $
   BKG=bkg, $
   MISSING=missing, $
   FLOOR=floor, $
   CALIB=calib, $
   TITLE=title, $
   LIMS=lims, $
   OUTFLOW_POSITIVE=outflow_positive, $
   PRECIPITATION_POSITIVE=precipitation_positive

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(lims) EQ 2 THEN lims = [lims,0] ;;Assume not logged

  PRINT,"Getting " + name + ' ...'

  GET_2DT,type,routine, $
          NAME=name, $
          T1=t1, $
          T2=t2, $
          ENERGY=energy, $
          ERANGE=er, $
          EBINS=ebins, $
          ANGLE=an, $
          ARANGE=ar, $
          BINS=bins, $
          GAP_TIME=gap_time, $ 
          NO_DATA=no_data, $
          BKG=bkg, $
          MISSING=missing, $
          FLOOR=floor, $
          CALIB=calib

  GET_DATA,name,DATA=data

  IF SIZE(data,/TYPE) NE 8 THEN BEGIN
     PRINT,name + ' appears to be out of commission ...'
     RETURN
  ENDIF

  keep  = WHERE(FINITE(data.y))
  data.x = data.x[keep]
  data.y = data.y[keep]
  
  keep  = WHERE(ABS(data.y) GT 0.0)
  data.x = data.x[keep]
  data.y = data.y[keep]
  
  ;;get timescale monotonic
  time_order = SORT(data.x)
  data.x = data.x[time_order]
  data.y = data.y[time_order]

  ;;kill dupes
  dupe_i      = WHERE(ABS(data.x[1:-1]-data.x[0:-2]) LT 0.0001,nDupes, $
                      COMPLEMENT=keep,NCOMPLEMENT=nKeep)
  PRINT,STRCOMPRESS(nDupes,/REMOVE_ALL) + ' ' + name + ' duplicates here'
  data.x       = data.x[keep]
  data.y       = data.y[keep]
  
  IF KEYWORD_SET(precipitation_positive) OR KEYWORD_SET(outflow_positive) THEN BEGIN
     GET_FA_ORBIT,data.x,/TIME_ARRAY,/DEFINITIVE
     GET_DATA,'ILAT',DATA=ilat

     north_i = WHERE(ilat.y GT 0,nNorth, $
                     COMPLEMENT=south_i, $
                     NCOMPLEMENT=nSouth)
  ENDIF

  IF KEYWORD_SET(outflow_positive) THEN BEGIN
     IF nNorth GT 0 THEN BEGIN
        data.y[north_i] *= (-1.)
     ENDIF
  ENDIF

  IF KEYWORD_SET(precipitation_positive) THEN BEGIN
     IF nSouth GT 0 THEN BEGIN
        data.y[south_i] *= (-1.)
     ENDIF
  ENDIF

  STORE_DATA,name,DATA=data

  CASE N_ELEMENTS(lims) OF
     2: BEGIN
        YLIM,name,lims[0],lims[1]
     END
     3: BEGIN
        YLIM,name,lims[0],lims[1],lims[2] ; set y limits
     END
     ELSE:
  ENDCASE
  OPTIONS,name,'ytitle',title       ; set y title
  OPTIONS,name,'panel_size',3       ; set panel size
  OPTIONS,name,'x_no_interp',1
  OPTIONS,name,'y_no_interp',1


END
