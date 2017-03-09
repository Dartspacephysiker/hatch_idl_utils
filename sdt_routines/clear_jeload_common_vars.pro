;;12/28/16
PRO CLEAR_JELOAD_COMMON_VARS, $
   QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__jeload_vars.pro

  IF ~KEYWORD_SET(quiet) THEN PRINT,'Clearing JELOAD COMMON vars ...'

   JEL__curFile             = !NULL
   JEL__je_hash             = !NULL
   JEL__je_keys             = !NULL
   JEL__je_tRange_hash      = !NULL
   JEL__je_tRange_inds_hash = !NULL
   JEL__orb1                = !NULL
   JEL__orb2                = !NULL
   JEL__orbSuff             = !NULL
   JEL__use_dupeless        = !NULL


END
