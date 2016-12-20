  IF N_ELEMENTS(defGEIStructName) EQ 0 THEN defGEIStructName = 'GEICoords'

  IF N_ELEMENTS(R_E             ) EQ 0 THEN R_E              = 6371.2D            ;Earth radius in km, from IGRFLIB_V2.pro

  IF N_ELEMENTS(altitude_max    ) EQ 0 THEN altitude_max     = 4400      ;in km, and risk nothing
  IF N_ELEMENTS(allow_fl_trace  ) EQ 0 THEN allow_fl_trace   = 1B        ;Allow fieldline tracing for AACGM_v2?
  IF N_ELEMENTS(check_if_exists ) EQ 0 THEN check_if_exists  = 1B

  ;;Var names
  IF N_ELEMENTS(in_names) EQ 0 THEN BEGIN
     in_names = {GEOSph       : 'GEOSph_arr'   , $
                 AACGMSph     : 'AACGMSph_arr' , $
                 GEOStruct    : 'GEO'          , $
                 AACGMStruct  : 'AACGM'        , $
                 coordStruct  : 'GEICoords'    , $
                 timeStr      : 'timeTmpStr'   , $
                 DBInd        : 'remaining_i'}
  ENDIF

  IF N_ELEMENTS(defNames) EQ 0 THEN BEGIN 
     defNames = {AACGMSph    : 'AACGMSph'      , $
                 AACGMStruct : 'AACGMStruct'   , $
                 restrictVar : 'restrict_ii'   , $
                 DBInd       : 'DBInds'        , $
                 DBIndName   : 'db_i'}
  ENDIF


