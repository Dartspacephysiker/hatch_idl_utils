;+
; NAME:                 AVERAGE_SUM3D
;
;
;
; PURPOSE:              Average data over specified interval (e.g., burst electron ESA data).
;
;
;
; CATEGORY:             SDT ROUTINES
;
;
;
; CALLING SEQUENCE:     Called from GET_2DT_TS
;
;
;
; INPUTS:               dat: A data structure returned by the get_fa_{ees,eeb} routines; possibly others?
;                       avg_interval: Number of members to average over. (Default: 2)
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:              avg_dat: The averaged data
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-

FUNCTION AVERAGE_SUM3D,dat,avg_interval


  IF N_ELEMENTS(avg_interval) EQ 0 THEN avg_interval = 2

  avg_dat           = !NULL
  nStructs          = N_ELEMENTS(dat)
  loopCount         = nStructs
  WHILE loopCount GT 0 DO BEGIN
     nAvgs          = avg_interval < loopCount
     iStruct        = nStructs - loopCount
     temp_dat       = dat[iStruct] 
     FOR i=0,nAvgs-1 DO BEGIN
        temp_dat    = SUM3D(temp_dat,dat[iStruct+i])
     ENDFOR
     ;; temp_dat.data /= nAvgs
     avg_dat        = [avg_dat,temp_dat]
     loopCount     -= nAvgs
  ENDWHILE

  RETURN,avg_dat
END