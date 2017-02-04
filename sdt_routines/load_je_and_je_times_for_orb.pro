;;2016/08/26
;;Each file contains je_hash, je_trange_hash, je_trange_inds_hash, and je_keys

;;NOTE, at least for the dupeless files, the returned data has 
;;      1. had all ~FINITE values removed,
;;      2. been made monotonic, 
;;      3. had all duplicates within 1/10 of a millisecond removed,
;;      4. had the ESA burp at turn-on removed, and 
;;      5. had all periods separated by more than ten seconds separated into different start/stop times.
;;      (and the dupeless files originate from /SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/journal__20160705__slap_together_the_je_time_ind_files.pro
FUNCTION LOAD_JE_AND_JE_TIMES_FOR_ORB,orbit_num, $
                                      RETURN_STRUCT=return_struct, $
                                      USE_DUPELESS_FILES=use_dupeless, $
                                      JE_OUT=je, $
                                      TIME_RANGES_OUT=time_ranges, $
                                      TIME_RANGE_INDICES_OUT=time_range_indices, $
                                      NINTERVALS_OUT=number_of_intervals, $
                                      OUT_JEFILENAME=jeFileName, $
                                      OUT_FILE_ORB1=orb1, $
                                      OUT_FILE_ORB2=orb2, $
                                      CLEAN_DUPES=clean_dupes, $
                                      ;; OUT_JEFILEDIR=jeFileDir, $
                                      QUIET=quiet

  COMPILE_OPT IDL2

  @common__jeload_vars.pro

  dbDir      = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/eesa_time_intervals/'

  IF KEYWORD_SET(use_dupeless) THEN BEGIN

     ;; dbPref  = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds__0-16361--noDupes.sav--orbs_'
     dbPref  = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds__500-25007--noDupes.sav--orbs_'

     minOrb  = 500
     ;; maxOrb  = 16361
     maxOrb  = 24507

     IF (orbit_num LT minOrb) OR (orbit_num GT maxOrb) THEN BEGIN
        PRINT,"Don't have a file corresponding to orbit " + $
              STRCOMPRESS(orbit_num,/REMOVE_ALL) + '...'
        RETURN,-1
     ENDIF

     ;; orb1    = LONG(ROUND_TO_NTH_DECIMAL_PLACE(orbit_num,3,/FLOOR) > 500)
     orb1    = LONG(ROUND_TO_NTH_DECIMAL_PLACE(orbit_num,3,/FLOOR))
     ;; orb2    = LONG((ROUND_TO_NTH_DECIMAL_PLACE(orbit_num+1,3,/CEIL) - 1) < 16362)
     orb2    = LONG((ROUND_TO_NTH_DECIMAL_PLACE(orbit_num+1,3,/CEIL) - 1) < (maxOrb+1))

  ENDIF ELSE BEGIN

     dbPref  = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds__0-50000.sav--orbs_'

     CASE 1 OF 
        (orbit_num GE 0     ) AND (orbit_num LE 999   ): BEGIN
           orb1 = 0
           orb2 = 999
        END      
        (orbit_num GE 1000  ) AND (orbit_num LE 1999  ): BEGIN
           orb1 = 1000
           orb2 = 1999
        END  
        (orbit_num GE 2000  ) AND (orbit_num LE 2999  ): BEGIN
           orb1 = 2000
           orb2 = 2999
        END  
        (orbit_num GE 3000  ) AND (orbit_num LE 3999  ): BEGIN
           orb1 = 3000
           orb2 = 3999
        END  
        (orbit_num GE 4000  ) AND (orbit_num LE 4999  ): BEGIN
           orb1 = 4000
           orb2 = 4999
        END  
        (orbit_num GE 5000  ) AND (orbit_num LE 5999  ): BEGIN
           orb1 = 5000
           orb2 = 5999
        END
        (orbit_num GE 6000  ) AND (orbit_num LE 6999  ): BEGIN
           orb1 = 6000
           orb2 = 6999
        END
        (orbit_num GE 7000  ) AND (orbit_num LE 8999  ): BEGIN
           orb1 = 7000
           orb2 = 8999
        END
        (orbit_num GE 9000  ) AND (orbit_num LE 9999  ): BEGIN
           orb1 = 9000
           orb2 = 9999
        END
        (orbit_num GE 10000 ) AND (orbit_num LE 11999 ): BEGIN
           orb1 = 10000
           orb2 = 11999
        END
        (orbit_num GE 12000 ) AND (orbit_num LE 12999 ): BEGIN
           orb1 = 12000
           orb2 = 12999
        END
        (orbit_num GE 13000 ) AND (orbit_num LE 13999 ): BEGIN
           orb1 = 13000
           orb2 = 13999
        END
        (orbit_num GE 14000 ) AND (orbit_num LE 14999 ): BEGIN
           orb1 = 14000
           orb2 = 14999
        END
        (orbit_num GE 15000 ) AND (orbit_num LE 15999 ): BEGIN
           orb1 = 15000
           orb2 = 15999
        END
        (orbit_num GE 16000 ) AND (orbit_num LE 16999 ): BEGIN
           orb1 = 16000
           orb2 = 16999
        END
        (orbit_num GE 17000 ) AND (orbit_num LE 17999 ): BEGIN
           orb1 = 17000
           orb2 = 17999
        END
        (orbit_num GE 18000 ) AND (orbit_num LE 19999 ): BEGIN
           orb1 = 18000
           orb2 = 19999
        END
        (orbit_num GE 20000 ) AND (orbit_num LE 20999 ): BEGIN
           orb1 = 20000
           orb2 = 20999
        END
        (orbit_num GE 21000 ) AND (orbit_num LE 21999 ): BEGIN
           orb1 = 21000
           orb2 = 21999
        END
        (orbit_num GE 22000 ) AND (orbit_num LE 22999 ): BEGIN
           orb1 = 22000
           orb2 = 22999
        END
        (orbit_num GE 23000 ) AND (orbit_num LE 23999 ): BEGIN
           orb1 = 23000
           orb2 = 23999
        END
        (orbit_num GE 24000 ) AND (orbit_num LE 24999 ): BEGIN
           orb1 = 24000
           orb2 = 24999
        END
        (orbit_num GE 25000 ) AND (orbit_num LE 25999 ): BEGIN
           orb1 = 25000
           orb2 = 25999
        END
        (orbit_num GE 26000 ) AND (orbit_num LE 28999 ): BEGIN
           orb1 = 26000
           orb2 = 28999
        END
        (orbit_num GE 29000 ) AND (orbit_num LE 29999 ): BEGIN
           orb1 = 29000
           orb2 = 29999
        END
        (orbit_num GE 30000 ) AND (orbit_num LE 30999 ): BEGIN
           orb1 = 30000
           orb2 = 30999
        END
        (orbit_num GE 31000 ) AND (orbit_num LE 31999 ): BEGIN
           orb1 = 31000
           orb2 = 31999
        END
        (orbit_num GE 32000 ) AND (orbit_num LE 33999 ): BEGIN
           orb1 = 32000
           orb2 = 33999
        END
        (orbit_num GE 34000 ) AND (orbit_num LE 36999 ): BEGIN
           orb1 = 34000
           orb2 = 36999
        END
        (orbit_num GE 37000 ) AND (orbit_num LE 37999 ): BEGIN
           orb1 = 37000
           orb2 = 37999
        END
        (orbit_num GE 38000 ) AND (orbit_num LE 38999 ): BEGIN
           orb1 = 38000
           orb2 = 38999
        END
        (orbit_num GE 39000 ) AND (orbit_num LE 39999 ): BEGIN
           orb1 = 39000
           orb2 = 39999
        END
        (orbit_num GE 40000 ) AND (orbit_num LE 40999 ): BEGIN
           orb1 = 40000
           orb2 = 40999
        END
        (orbit_num GE 41000 ) AND (orbit_num LE 41999 ): BEGIN
           orb1 = 41000
           orb2 = 41999
        END
        (orbit_num GE 42000 ) AND (orbit_num LE 42999 ): BEGIN
           orb1 = 42000
           orb2 = 42999
        END
        (orbit_num GE 43000 ) AND (orbit_num LE 43999 ): BEGIN
           orb1 = 43000
           orb2 = 43999
        END
        (orbit_num GE 44000 ) AND (orbit_num LE 44999 ): BEGIN
           orb1 = 44000
           orb2 = 44999
        END
        (orbit_num GE 45000 ) AND (orbit_num LE 48999 ): BEGIN
           orb1 = 45000
           orb2 = 48999
        END
        (orbit_num GE 49000 ) AND (orbit_num LE 49999 ): BEGIN
           orb1 = 49000
           orb2 = 49999
        END
        (orbit_num GE 49001 ) AND (orbit_num LE 50001 ): BEGIN
           orb1 = 49001
           orb2 = 50001
        END
        ELSE: BEGIN
           PRINT,"Don't have a file corresponding to orbit " + $
                 STRCOMPRESS(orbit_num,/REMOVE_ALL) + '...'
           RETURN,-1
        END
     ENDCASE
  ENDELSE

  ;;If we've already got them ...
  IF N_ELEMENTS(JEL__use_dupeless) GT 0 THEN BEGIN
     IF KEYWORD_SET(JEL__use_dupeless) NE KEYWORD_SET(use_dupeless) THEN BEGIN
        PRINT,"Swapping " + (KEYWORD_SET(use_dupeless) ? "for" : "out" ) + " dupeless Je stuff ..."
        reset_JEL = 1
        JEL__use_dupeless = KEYWORD_SET(use_dupeless)
     ENDIF
  ENDIF ELSE BEGIN
     JEL__use_dupeless = KEYWORD_SET(use_dupeless)
  ENDELSE

  IF N_ELEMENTS(JEL__orb1) GT 0 AND N_ELEMENTS(JEL__orb2) GT 0 AND $
     ~KEYWORD_SET(reset_JEL) $
  THEN BEGIN
     IF (orb1 EQ JEL__orb1) AND (orb2 EQ JEL__orb2) THEN BEGIN

        IF (JEL__je_keys.WHERE(orbit_num)) EQ !NULL THEN BEGIN
           IF ~KEYWORD_SET(quiet) THEN PRINT,"Couldn't get orbit " + STRCOMPRESS(orbit_num,/REMOVE_ALL) + "!"
           RETURN,-1
        ENDIF

        number_of_intervals  = N_ELEMENTS((JEL__je_trange_inds_hash[orbit_num])[*,0])

        je                 = JEL__je_hash[orbit_num]

        time_ranges        = JEL__je_trange_hash[orbit_num] 
        time_range_indices = JEL__je_trange_inds_hash[orbit_num]  ;; PRINT,'Restoring i
        
        IF ARG_PRESENT(jeFileName) THEN jeFileName = JEL__curFile ;dbPref+orbSuff

        IF KEYWORD_SET(return_struct) THEN BEGIN
           struct = {je                  : TEMPORARY(je                 ), $
                     time_ranges         : TEMPORARY(time_ranges        ), $
                     time_range_indices  : TEMPORARY(time_range_indices ), $
                     number_of_intervals : TEMPORARY(number_of_intervals)}
           
           RETURN,struct
        ENDIF
     ENDIF ELSE BEGIN
        reset_JEL = 1
     ENDELSE
  ENDIF ELSE BEGIN
     reset_JEL    = 1
  ENDELSE

  orbSuff              = STRCOMPRESS(orb1,/REMOVE_ALL) + '-' + STRCOMPRESS(orb2,/REMOVE_ALL) 

  IF FILE_TEST(dbDir+dbPref+orbSuff) THEN BEGIN
     ;;Update orbs in COMMON block
     JEL__orb1 = orb1
     JEL__orb2 = orb2

     IF KEYWORD_SET(reset_JEL) THEN BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,"Restoring Je, Je time stuff for orbs " + orbSuff + ' ...'
        RESTORE,dbDir+dbPref+orbSuff

        ;;Update common vars
        JEL__curFile              = dbPref+orbSuff
        JEL__je_hash              = TEMPORARY(je_hash)
        JEL__je_tRange_hash       = TEMPORARY(je_tRange_hash)
        JEL__je_tRange_inds_hash  = TEMPORARY(je_tRange_inds_hash)
        IF N_ELEMENTS(je_keys) EQ 0 THEN BEGIN
           gen_je_keys = 1
        ENDIF ELSE BEGIN
           JEL__je_keys           = TEMPORARY(je_keys)
        ENDELSE
     ENDIF
  ENDIF ELSE BEGIN
     PRINT,"LOAD_JE_AND_JE_TIMES_FOR_ORB: Can't find " + dbPref+orbSuff + '!'
     RETURN,-1
  ENDELSE

  ;;Generate keys if we don't have them
  IF KEYWORD_SET(gen_je_keys) THEN BEGIN
     PRINT,"Generating je_keys ..."
     JEL__je_keys = JEL__je_hash.Keys()
     PRINT,'Saving keys to file ...'
     je_hash              = JEL__je_hash
     je_keys              = JEL__je_keys
     je_tRange_hash       = JEL__je_tRange_hash
     je_tRange_inds_hash  = JEL__je_tRange_inds_hash
     SAVE,je_hash,je_keys,je_tRange_hash,je_tRange_inds_hash,FILENAME=dbDir+dbPref+orbSuff
  ENDIF
     
  ;;Get us out (of the U.N.!) in case there is nothing to talk about
  IF (WHERE(JEL__je_keys EQ orbit_num))[0] EQ -1 THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,'No data for orb ' + STRCOMPRESS(orbit_num,/REMOVE_ALL)
     RETURN,-1
  ENDIF

  number_of_intervals  = N_ELEMENTS((JEL__je_trange_inds_hash[orbit_num])[*,0])

  je                   = JEL__je_hash[orbit_num]

  time_ranges          = JEL__je_trange_hash[orbit_num] 
  time_range_indices   = JEL__je_trange_inds_hash[orbit_num]  ;; PRINT,'Restoring i

  IF ARG_PRESENT(jeFileName) THEN jeFileName = JEL__curFile ;dbPref+orbSuff

  IF KEYWORD_SET(clean_dupes) THEN BEGIN

     cleanFile = dbPref+orbSuff+'--dupesRemoved'
     JEL__curFile = cleanFile
     CASE FILE_TEST(dbDir+JEL__curFile) OF
        0: BEGIN
           IF ~KEYWORD_SET(quiet) THEN PRINT,'Making new clean list ...'
           cleanHash = HASH(JEL__je_keys,MAKE_ARRAY(N_ELEMENTS(JEL__je_keys),VALUE=0,/BYTE))
        END
        1: BEGIN
           RESTORE,dbDir+JEL__curFile
           IF (cleanHash[orbit_num]) THEN BEGIN

              number_of_intervals  = N_ELEMENTS((JEL__je_trange_inds_hash[orbit_num])[*,0])

              je                 = JEL__je_hash[orbit_num]

              time_ranges        = JEL__je_trange_hash[orbit_num] 
              time_range_indices = JEL__je_trange_inds_hash[orbit_num]  ;; PRINT,'Restoring i



              IF ~KEYWORD_SET(quiet) THEN PRINT,"Je & Co. already cleaned"

           ENDIF 
        END
     ENDCASE

     IF ~(cleanHash[orbit_num]) THEN BEGIN

        IF ~KEYWORD_SET(quiet) THEN PRINT,'Cleaning stuff ...'
        CHECK_DUPES,je.x,HAS_DUPES=hasDupes,IS_SORTED=isSort,OUT_UNIQ_I=uniq_i, $
                    QUIET=quiet

        IF hasDupes OR ~isSort THEN BEGIN
           IF ~KEYWORD_SET(quiet) THEN PRINT,"Sorting/junking dupes ..."

           je = {x:je.x[uniq_i],y:je.y[uniq_i]}

           ;;It's possible for time_ranges to have dupes. Consider orbit 9859:
           ;;PRINT,TIME_TO_STR(time_ranges,/ms)
           ;;1999-02-17/18:33:07.831
           ;;1999-02-17/19:42:28.676
           ;;1999-02-17/18:33:07.831
           ;;1999-02-17/20:14:11.895
           ;;
           ;;So what you actually need is to find the nearest time AFTER the last
           ;; FOR k=0,number_of_intervals-2 DO BEGIN
           ;;    IF time_ranges[0,k+1] LT time_ranges[1,k] THEN BEGIN
           ;;       PRINT,FORMAT='("time_ranges[",I0,",",I0"] > time_ranges[",I0,",",I0"]!")', $
           ;;             0,k+1,1,k
           ;;       apropos_i = WHERE(je.x GE time_ranges[1,k],nApropos)
           ;;       IF nApropos EQ 0 THEN BEGIN
           ;;          PRINT,"Well now you're really in a bind."
           ;;          STOP
           ;;       ENDIF
           
           ;;       tmpMin = MIN(ABS(je.x[apropos_i]-
           ;;    ENDIF

           ;; ENDFOR


           FOR k=0,number_of_intervals-1 DO BEGIN

              ;;Update time ranges and tRange indices
              tmpMin = MIN(ABS(je.x-time_ranges[k,0]),minLow)
              ;; PRINT,tmpMin

              IF tmpMin GT 3 THEN BEGIN
                 PRINT,FORMAT='(F0.3,"-sec diff ''twixt time_ranges[",I0,",",I0,"] ' + $
                       'and nearest je.x")',tmpMin,k,0
                 STOP
              ENDIF

              tmpMin = MIN(ABS(je.x-time_ranges[k,1]),minHigh)
              ;; PRINT,tmpMin
              IF tmpMin GT 3 THEN BEGIN
                 PRINT,FORMAT='(F0.3,"-sec diff ''twixt time_ranges[",I0,",",I0,"] ' + $
                       'and nearest je.x")',tmpMin,k,1

                 STOP
              ENDIF

              time_ranges[k,*]        = [je.x[minLow],je.x[minHigh]]
              time_range_indices[k,*] = [minLow,minHigh]
           ENDFOR

           ;;Update hashes
           JEL__je_hash[orbit_num]             = je
           JEL__je_trange_hash[orbit_num]      = time_ranges
           JEL__je_trange_inds_hash[orbit_num] = time_range_indices

           cleanHash[orbit_num] = 1B

           ;;Save
           IF ~KEYWORD_SET(quiet) THEN PRINT, "Saving cleaned file: " + JEL__curFile
           je_hash              = JEL__je_hash
           je_keys              = JEL__je_keys
           je_tRange_hash       = JEL__je_tRange_hash
           je_tRange_inds_hash  = JEL__je_tRange_inds_hash
           SAVE,je_hash,je_keys,je_tRange_hash,je_tRange_inds_hash,cleanHash, $
                FILENAME=dbDir+JEL__curFile

        ENDIF
     ENDIF

     IF ARG_PRESENT(jeFileName) THEN jeFileName = JEL__curFile

  ENDIF

  IF KEYWORD_SET(return_struct) THEN BEGIN
     struct = {je                  : TEMPORARY(je                 ), $
               time_ranges         : TEMPORARY(time_ranges        ), $
               time_range_indices  : TEMPORARY(time_range_indices ), $
               number_of_intervals : TEMPORARY(number_of_intervals)}
     
     RETURN,struct
  ENDIF

  RETURN,0
END
