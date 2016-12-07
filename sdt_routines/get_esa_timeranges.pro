;;12/07/16
FUNCTION GET_ESA_TIMERANGES, $
   BURST=burst, $
   AURORAL_OVAL=auroral_oval, $
   RETURN_STRUCT=return_struct, $
   STRINGS=strings, $
   TPLOT_VAR_LIST=tplot_var_list

  COMPILE_OPT IDL2

  t           = 0
  dat         = GET_FA_EES(t,/st)
  IF ~dat.valid THEN BEGIN
     print,' ERROR: No FAST electron survey data -- get_fa_ees(t,/st) returned invalid data'
     RETURN,-1
  ENDIF

  ;; Electron current - line plot
  IF KEYWORD_SET(burst) THEN BEGIN
     get_2dt_ts,'j_2d_b','fa_eeb',t1=t1,t2=t2,name='Je',energy=energy_electrons
  ENDIF ELSE BEGIN
     get_2dt_ts,'j_2d_b','fa_ees',t1=t1,t2=t2,name='Je',energy=energy_electrons
  ENDELSE
  
  ;;remove spurious crap
  GET_DATA,'Je',DATA=tmpj
  
  keep        = WHERE(FINITE(tmpj.y) NE 0)
  tmpj.x      = tmpj.x[keep]
  tmpj.y      = tmpj.y[keep]
  
  keep        = WHERE(ABS(tmpj.y) GT 0.0)
  tx          = tmpj.x[keep]
  ty          = tmpj.y[keep]
  
  ;;get timescale monotonic
  time_order  = SORT(tx)
  tx          = tx[time_order]
  ty          = ty[time_order]
  
  
  ;;throw away the first 10  points since they are often corrupted
  if not KEYWORD_SET(burst) THEN begin
     STORE_DATA,'Je',DATA={x:tx[10:N_ELEMENTS(tx)-1],y:ty[10:N_ELEMENTS(tx)-1]}
  endif else begin
     STORE_DATA,'Je',DATA={x:tx,y:ty}
  endelse
  
  ;;eliminate data from latitudes below the Holzworth/Meng auroral oval 
  GET_DATA,'Je',DATA=je
  GET_FA_ORBIT,/TIME_ARRAY,je.x
  GET_DATA,'MLT',DATA=mlt
  GET_DATA,'ILAT',DATA=ilat
  IF KEYWORD_SET(auroral_oval) THEN BEGIN
     keep = WHERE(ABS(ilat.y) GT AURORAL_ZONE(mlt.y,7,/lat)/(!DPI)*180.)
     belowAurOvalStr = ''
  ENDIF ELSE BEGIN
     keep = WHERE(ABS(ilat.y) GE 50.0 )
     belowAurOvalStr = '--below_aur_oval'
  ENDELSE

  STORE_DATA,'Je',DATA={x:je.x[keep],y:je.y[keep]}

  ;;Use the electron data to define the time ranges for this orbit	
  GET_DATA,'Je',DATA=je
  part_res_je = make_array(N_ELEMENTS(Je.x),/double)
  for j=1,N_ELEMENTS(Je.x)-1 do begin
     part_res_je[j] = ABS(Je.x[j]-Je.x[j-1])
  endfor
  part_res_Je[0] = part_res_Je[1]
  gap = WHERE(part_res_je GT 10.0)
  if gap[0] NE -1 THEN begin
     separate_start = [0,WHERE(part_res_je GT 10.0)]
     separate_stop = [WHERE(part_res_je GT 10.0),N_ELEMENTS(Je.x)-1]
  endif else begin
     separate_start = [0]
     separate_stop = [N_ELEMENTS(Je.x)-1]
  endelse
  
  ;;remove esa burp when switched on
  if not KEYWORD_SET(burst) THEN begin
     turn_on = WHERE(part_res_je GT 300.0)
     if turn_on[0] NE -1 THEN begin
        turn_on_separate = MAKE_ARRAY(N_ELEMENTS(turn_on),/DOUBLE)
        for j=0,N_ELEMENTS(turn_on)-1 do turn_on_separate[j] = WHERE(separate_start EQ turn_on[j])
        separate_start[turn_on_separate+1] = separate_start[turn_on_separate+1]+5
     endif
  endif

  ;;identify time indices for each interval
  count = 0.0
  for j=0,N_ELEMENTS(separate_start)-1 do begin
     if (separate_stop[j]-separate_start[j]) GT 10 THEN begin
        count = count+1
        if count EQ 1.0 THEN begin
           time_range_indices = TRANSPOSE([separate_start[j]+1,separate_stop[j]-1])
        endif else begin
           time_range_indices = [time_range_indices,TRANSPOSE([separate_start[j],separate_stop[j]-1])]
        endelse
     endif
  endfor
  
  ;;identify interval times
  time_ranges         = je.x[time_range_indices]
  number_of_intervals = N_ELEMENTS(time_ranges[*,0])
  
  PRINT,'number_of_intervals',number_of_intervals


  IF ARG_PRESENT(tplot_var_list) THEN BEGIN
     tplot_var_list = ['Je'     , $	  
                       'ORBIT'  , $
                       'fa_pos' , $
                       'ALT'    , $      
                       'ILAT'   , $
                       'ILNG'   , $
                       'MLT'    , $      
                       'fa_vel' ]
  ENDIF

  IF ARG_PRESENT(strings) THEN BEGIN
     strings = TIME_TO_STR(time_ranges,/MS)
     strings = REFORM(strings,SIZE(time_ranges,/DIMENSIONS))
  ENDIF

  IF KEYWORD_SET(return_struct) THEN BEGIN

     struct = {number_of_intervals  : number_of_intervals    , $
                   time_ranges          : time_ranges            , $
                   time_range_indices   : time_range_indices     , $
                   burst                : BYTE(KEYWORD_SET(burst))}

     RETURN,struct
  ENDIF

  RETURN,time_ranges

END
