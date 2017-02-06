;;11/22/16
;;For example usage of this feller, see JOURNAL__20161122__CONVERT_THE_REMAINING_FASTLOC_INDS_TO_AACGM
PRO GET_FAST_GEI_COORDS,times, $
                        SAVE_GEI_COORDS=save_GEI_coords, $
                        GEI_COORD_DIR=GEI_coord_dir, $
                        GEI_COORD_FILENAME=GEI_coord_filename, $ 
                        GEI_STRUCT_NAME=GEI_struct_name, $
                        ORIG_ROUTINENAME=orig_routineName, $
                        QUIET=quiet

  COMPILE_OPT IDL2

  IF ~KEYWORD_SET(quiet) THEN PRINT,"Getting " + STRCOMPRESS(N_ELEMENTS(times),/REMOVE_ALL) + " FAST ephemeris points in GEI coords ..."

  IF FILE_TEST(GEI_coord_dir + '/' + GEI_coord_filename) THEN BEGIN
     PRINT,"File exists: " + GEI_coord_dir + '/' + GEI_coord_filename
     PRINT,"You need to confirm that you want it."
     RETURN
  ENDIF

  GET_FA_ORBIT,times,/TIME_ARRAY,/ALL,/DEFINITIVE

  GET_DATA,'ORBIT',DATA=orbit
  GET_DATA,'fa_pos',DATA=fa_pos
  GET_DATA,'ALT',DATA=alt
  GET_DATA,'ILAT',DATA=ilat
  GET_DATA,'ILNG',DATA=ilng
  GET_DATA,'LAT',DATA=lat
  GET_DATA,'LNG',DATA=lng
  GET_DATA,'fa_vel',DATA=fa_vel

  GEIcoords   = {orbit              :(TEMPORARY(orbit)).y, $
                 fa_pos             :(TEMPORARY(fa_pos)).y, $
                 alt                :(TEMPORARY(alt)).y, $
                 lat                :(TEMPORARY(lat)).y, $
                 lng                :(TEMPORARY(lng)).y, $
                 fa_vel             :(TEMPORARY(fa_vel)).y, $
                 pos_and_vel_coords :'GEI (per GET_FA_ORBIT)', $
                 orig_routineName   :''}

  IF KEYWORD_SET(orig_routineName) THEN BEGIN
     GEIcoords.orig_routineName = orig_routineName
  ENDIF

  IF KEYWORD_SET(GEI_struct_name) THEN BEGIN
     this = EXECUTE(GEI_struct_name + ' = TEMPORARY(GEIcoords)')
  ENDIF ELSE BEGIN
     GEI_struct_name = 'GEIcoords'
  ENDELSE

  IF KEYWORD_SET(save_GEI_coords) THEN BEGIN
     PRINT,"Saving GEI coord struct (" + GEI_struct_name +") to " + GEI_coord_filename + ' ...'

     this = EXECUTE('SAVE,' + GEI_struct_name + ',FILENAME="' + GEI_coord_dir + '/' + GEI_coord_filename + '"')
     
  ENDIF

END

