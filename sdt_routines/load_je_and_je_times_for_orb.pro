;;2016/08/26
;;Each file contains je_hash, je_trange_hash, je_trange_inds_hash, and je_keys

FUNCTION LOAD_JE_AND_JE_TIMES_FOR_ORB,orbit_num, $
                                      JE_OUT=je, $
                                      TIME_RANGES_OUT=time_ranges, $
                                      TIME_RANGE_INDICES_OUT=time_range_indices, $
                                      NINTERVALS_OUT=number_of_intervals, $
                                      OUT_JEFILENAME=jeFileName, $
                                      ;; OUT_JEFILEDIR=jeFileDir, $
                                      QUIET=quiet

  COMPILE_OPT IDL2

  dbDir  = '/home/spencerh/software/sdt/batch_jobs/saves_output_etc/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/'
  dbPref = 'cleaned_Je__Je_tRanges__and_Je_tRange_inds__0-50000.sav--orbs_'


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

  orbSuff              = STRCOMPRESS(orb1,/REMOVE_ALL) + '-' + STRCOMPRESS(orb2,/REMOVE_ALL) 
  PRINT,"Restoring Je, Je time stuff for orbs " + orbSuff + ' ...'

  IF FILE_TEST(dbDir+dbPref+orbSuff) THEN BEGIN
     RESTORE,dbDir+dbPref+orbSuff
  ENDIF ELSE BEGIN
     PRINT,"LOAD_JE_AND_JE_TIMES_FOR_ORB: Can't find " + dbPref+orbSuff + '!'
     RETURN,-1
  ENDELSE

  ;;Generate keys if we don't have them
  IF N_ELEMENTS(je_keys) EQ 0 THEN BEGIN
     PRINT,"Generating je_keys ..."
     je_keys = je_hash.Keys()
     PRINT,'Saving keys to file ...'
     SAVE,je_hash,je_keys,je_tRange_hash,je_tRange_inds_hash,FILENAME=dbDir+dbPref+orbSuff
  ENDIF
     
  ;;Get us out (of the U.N.!) in case there is nothing to talk about
  IF (WHERE(je_keys EQ orbit_num))[0] EQ -1 THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINT,'No data for orb ' + STRCOMPRESS(orbit_num,/REMOVE_ALL)
     RETURN,-1
  ENDIF

  number_of_intervals  = N_ELEMENTS((je_trange_inds_hash[orbit_num])[*,0])

  je                   = je_hash[orbit_num]

  time_ranges          = je_trange_hash[orbit_num] 
  time_range_indices   = je_trange_inds_hash[orbit_num]  ;; PRINT,'Restoring i

  IF ARG_PRESENT(jeFileName) THEN jeFileName = dbPref+orbSuff

  RETURN,0
END
