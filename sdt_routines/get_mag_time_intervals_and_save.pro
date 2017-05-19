;2017/05/19
PRO GET_MAG_TIME_INTERVALS_AND_SAVE,tx, $
                                    TIME_RANGES_OUT=time_ranges, $
                                    TIME_RANGE_INDICES_OUT=time_range_indices, $
                                    NINTERVALS_OUT=number_of_intervals, $
                                    SAVE_MAG_TIME_INTERVALS=save_mag_time_intervals

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;get timescale monotonic
  time_order                             = SORT(tx)
  tx                                     = tx[time_order]

  part_res_mag                            = MAKE_ARRAY(N_ELEMENTS(tx),/DOUBLE)
  FOR j=1,N_ELEMENTS(tx)-1 DO BEGIN
     part_res_mag[j]                      = ABS(tx[j]-tx[j-1])
  ENDFOR
  part_res_mag[0]                         = part_res_mag[1]
  gap                                    = WHERE(part_res_mag GT 10.0)
  IF gap[0] NE -1 THEN BEGIN
     separate_start                      = [0,where(part_res_mag GT 10.0)]
     separate_stop                       = [where(part_res_mag GT 10.0),N_ELEMENTS(tx)-1]
  ENDIF ELSE BEGIN
     separate_start                      = [0]
     separate_stop                       = [N_ELEMENTS(tx)-1]
  ENDELSE

  ;;identify time indices for each interval
  count                                  = 0.0
  FOR j=0,N_ELEMENTS(separate_start)-1 DO BEGIN
     IF (separate_stop[j]-separate_start[j]) GT 10 THEN BEGIN
        count                            = count+1
        IF count EQ 1.0 THEN BEGIN
           time_range_indices            = TRANSPOSE([separate_start[j]+1,separate_stop[j]-1])
        ENDIF ELSE BEGIN
           time_range_indices            = [time_range_indices,TRANSPOSE([separate_start[j],separate_stop[j]-1])]
        ENDELSE
     ENDIF
  ENDFOR
  
  ;;identify interval times
  time_ranges                            = tx[time_range_indices]
  number_of_intervals                    = N_ELEMENTS(time_ranges[*,0])
  
  print,'number_of_intervals',number_of_intervals
  
  
  IF KEYWORD_SET(save_mag_time_intervals) THEN BEGIN
     
     STOP

     GET_FA_ORBIT,tx[time_range_indices[0,0]],tx[time_range_indices[0,1]]
     ;;now get orbit quantities
     GET_DATA,'ORBIT',DATA=orb
     orbit_num                              = orb.y[0]
     orbStr                                 = STRCOMPRESS(orbit_num,/REMOVE_ALL)
     outFile                                = STRING(FORMAT='(A0,I0,"--",I0,"_intervals.sav")', $
                                                     filePref,orbit_num,number_of_intervals)

     PRINT,'Saving ' + outFile + ' ...'
     SAVE,je,orbit_num,time_range_indices,number_of_intervals,time_ranges, $
          FILENAME=outDir+outFile

     ;;...and if there were dupes, report that too
     ;; IF nDupes GT 0 THEN BEGIN
     ;;    OPENW,dupeLun,dupeReportDir+dupeReportFile,/GET_LUN,/APPEND
     ;;    PRINTF,dupeLun,FORMAT='(I0,T10,I0,T20,I0)',orbit_num,nOrig,nDupes
     ;;    CLOSE,dupeLun
     ;;    FREE_LUN,dupeLun
     ;; ENDIF

  ENDIF

END
