PRO PRINT_RUNNING_STATS_RANGES,x,y,ranges, $
                               WINDOW_SUM=window_sum, $
                               AVERAGE=average, $
                               MEDIAN=median, $
                               LUN=lun

  IF N_ELEMENTS(lun) EQ 0 THEN lun = -1 ;stdout
  
  IF KEYWORD_SET(window_sum) THEN BEGIN
     statStr                          = 'Window sum' 
     stat                             = 0
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(average) THEN BEGIN
        statStr                       = 'Running average'
        stat                          = 1
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(median) THEN BEGIN
           statStr                    = 'Running median'
           stat                       = 2
        ENDIF ELSE BEGIN
           PRINTF,lun,"No keyword set for PRINT_RUNNING_STATS_RANGES! Ending..."
           RETURN
        ENDELSE
     ENDELSE
  ENDELSE

  PRINTF,lun,"***" + STRUPCASE(statStr) + "--STATISTICS FOR REQUESTED RANGES***"

  PRINTF,lun,FORMAT='("Range",T20,"N points",T40,A0,T60,"10^",A0)',statStr,statStr
  nRanges                    = N_ELEMENTS(ranges[0,*])
  FOR k=0,nRanges-1 DO BEGIN
     lb                      = ranges[0,k]
     ub                      = ranges[1,k]
     temp_i                  = WHERE(x GT lb AND x LE ub,nTemp)
     
     CASE stat OF
        0: BEGIN
           val               = TOTAL(y[temp_i],/DOUBLE)
        END
        1: BEGIN
           val               = TOTAL(y[temp_i],/DOUBLE)/nTemp
           
        END
        2: BEGIN
           val               = MEDIAN(y[temp_i])
        END
     ENDCASE
     
     valp10                  = (val LT 30) ? 10.0^(val) : 0.00
     
     PRINTF,lun,FORMAT='(F-0.2,"–",F-0.2,T20,I0,T40,F-0.5,T60,G-0.5)',lb,ub,nTemp,val,valp10
  ENDFOR

END