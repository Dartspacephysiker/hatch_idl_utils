;+
; NAME:             GPS_TO_UTC
;
;
;
; PURPOSE:          Convert GPS time to UTC
;
;
;
; CATEGORY:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:   INVALID : Specify what value to insert for invalid conversion (default: !VALUES.F_NaN)
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
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
;;2017/02/13
;;********************************************************************************
;;From http://www.leapsecond.com/java/gpsclock.htm: 
;; GPS, Global Positioning System time, is the atomic time scale implemented by the atomic clocks in the GPS ground control stations and the GPS satellites themselves. GPS time was zero at 0h 6-Jan-1980 and since it is not perturbed by leap seconds GPS is now ahead of UTC by 18 seconds.
;;
;;********************************************************************************
;;From https://confluence.qps.nl/display/KBE/UTC+to+GPS+Time+Correction

;; (Limits of validity @ 0 UTC)   == TAI-UTC (s) == GPS-UTC (s)
;; 1973-01-01 - 1974-01-01        --   12        --    
;; 1974-01-01 - 1975-01-01        --   13        --    
;; 1975-01-01 - 1976-01-01        --   14        --    
;; 1976-01-01 - 1977-01-01        --   15        --    
;; 1977-01-01 - 1978-01-01        --   16        --    
;; 1978-01-01 - 1979-01-01        --   17        --    
;; 1979-01-01 - 1980-01-01        --   18        --    
;; 1980-01-01 - 1981-07-01        --   19        --    0 
;; 1981-07-01 - 1982-07-01        --   20        --    1 
;; 1982-07-01 - 1983-07-01        --   21        --    2 
;; 1983-07-01 - 1985-07-01        --   22        --    3 
;; 1985-07-01 - 1988-01-01        --   23        --    4 
;; 1988-01-01 - 1990-01-01        --   24        --    5 
;; 1990-01-01 - 1991-01-01        --   25        --    6 
;; 1991-01-01 - 1992-07-01        --   26        --    7 
;; 1992-07-01 - 1993-07-01        --   27        --    8 
;; 1993-07-01 - 1994-07-01        --   28        --    9 
;; 1994-07-01 - 1996-01-01        --   29        --    10
;; 1996-01-01 - 1997-07-01        --   30        --    11
;; 1997-07-01 - 1999-01-01        --   31        --    12
;; 1999-01-01 - 2006-01-01        --   32        --    13
;; 2006-01-01 - 2009-01-01        --   33        --    14
;; 2009-01-01 - 2012-07-01        --   34        --    15
;; 2012-07-01 - 2015-07-01        --   35        --    16
;; 2015-07-01 - 2017-01-01        --   36        --    17
;; 2017-01-01 - ????-??-??        --   37        --    18


FUNCTION GPS_TO_UTC,GPS_week,GPS_mSec_of_week, $
               INVALID=invalid

  COMPILE_OPT IDL2

  nWeek                   = N_ELEMENTS(GPS_week        )
  nSec                    = N_ELEMENTS(GPS_mSec_of_week)

  CASE 1 OF
     ((nWeek EQ 0) AND (nSec EQ 0)): BEGIN
        PRINT,"No data provided! Out ..."
        RETURN,-1
     END
     (nWeek EQ 0): BEGIN
        ;;If no week, assume week 0 (6 Jan 1980, 00:00 UT)
        GPS_week          = MAKE_ARRAY(nSec,VALUE=0.D,/DOUBLE)
     END
     (nWeek EQ 1): BEGIN
        ;;If no week, assume week 0 (6 Jan 1980, 00:00 UT)
        GPS_week          = MAKE_ARRAY(nSec,VALUE=GPS_week[0],/DOUBLE)
     END
     (nSec EQ 0): BEGIN
        ;;If no week, assume week 0 (6 Jan 1980, 00:00 UT)
        GPS_mSec_of_week  = MAKE_ARRAY(nWeek,VALUE=0.D,/DOUBLE)
     END
     (nSec EQ 1): BEGIN
        ;;If no week, assume week 0 (6 Jan 1980, 00:00 UT)
        GPS_mSec_of_week  = MAKE_ARRAY(nWeek,VALUE=GPS_mSec_of_week[0],/DOUBLE)
     END
  ENDCASE

  IF N_ELEMENTS(invalid) EQ 0 THEN BEGIN
     invalid              = !VALUES.F_NaN
  ENDIF

  ;; tAdjStr = ['1980-01-01/00:00:00', '1981-07-01/00:00:00', '1982-07-01/00:00:00', $
  ;;            '1983-07-01/00:00:00', '1985-07-01/00:00:00', '1988-01-01/00:00:00', $
  ;;            '1990-01-01/00:00:00', '1991-01-01/00:00:00', '1992-07-01/00:00:00', $
  ;;            '1993-07-01/00:00:00', '1994-07-01/00:00:00', '1996-01-01/00:00:00', $
  ;;            '1997-07-01/00:00:00', '1999-01-01/00:00:00', '2006-01-01/00:00:00', $
  ;;            '2009-01-01/00:00:00', '2012-07-01/00:00:00', '2015-07-01/00:00:00', $
  ;;            '2017-01-01/00:00:00']

  ;; tAdj                = STR_TO_TIME(tAdjStr) + GPS_TO_UTC_adj


  tAdj                = [ 315532800.D, $       362793601.D, $       394329602.D, $
                          425865603.D, $       489024004.D, $       567993605.D, $
                          631152006.D, $       662688007.D, $       709948808.D, $
                          741484809.D, $       773020810.D, $       820454411.D, $
                          867715212.D, $       915148813.D, $      1136073614.D, $
                          1230768015.D, $      1341100816.D, $      1435708817.D, $
                          1483228818.D]

  GPS_TO_UTC_adj      = [0  , 1  , 2  , 3  , 4  , $
                         5  , 6  , 7  , 8  , 9  , $
                         10 , 11 , 12 , 13 , 14 , $
                         15 , 16 , 17 , 18]
  
  ;;s_to_1980 = STR_TO_TIME('1980-01-06/00:00:00')
  s_to_1980           = 315964800L

  ;;Number of seconds in a week
  ;;nDays = 7L & nHours = 24L & nMin = 60 & nSec = 60 & nSecInWeek = nDays*nHours*nMin*nSec
  ;;NSECINWEEK      LONG      =       604800
  nSecInWeek          = 604800L

  GPS_nSec_since_1970 = s_to_1980                   + $
                        DOUBLE(GPS_week)*nSecInWeek + $
                        GPS_mSec_of_week/1000.D

  
  ;;Find out where bogus
  invalid_i             = WHERE(GPS_nSec_since_1970 LT s_to_1980,nInvalid)
  IF nInvalid GT 0 THEN BEGIN
     GPS_nSec_since_1970[invalid_i] = invalid
  ENDIF

  ;;Get adjustments for UTC
  adj_i               = VALUE_LOCATE(tAdj,GPS_nSec_since_1970)
  
  GPS_nSec_since_1970[WHERE(adj_i EQ -1,/NULL)] = invalid
  ;; GPS_nSec_since_1970[WHERE(adj_i GE N_ELEMENTS(tAdj),/NULL)] = invalid
  tooBig_ii = WHERE(adj_i GE N_ELEMENTS(tAdj),nTooBig)
  IF nTooBig GT 0 THEN BEGIN
     adj_i[tooBig_ii] = adj_i[tooBig_ii] - 1
  ENDIF
  

  ;;Make adjustment
  GPS_nSec_since_1970 = GPS_nSec_since_1970 + GPS_TO_UTC_adj[adj_i]

  RETURN,GPS_nSec_since_1970

END
