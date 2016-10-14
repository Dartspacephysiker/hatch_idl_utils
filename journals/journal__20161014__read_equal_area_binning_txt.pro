;;10/14/16
PRO JOURNAL__20161014__READ_EQUAL_AREA_BINNING_TXT

  COMPILE_OPT IDL2

  todayStr     = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  inDir        = '/SPENCEdata/Research/database/equal-area_binning/'
  inFile       = 'equalArea_binRules--txtMod.txt'
  EAbins_file  = 'equalArea--' + todayStr + '--struct_and_ASCII_tmplt.idl'

  tmplt        = ASCII_TEMPLATE(inDir+inFile)
  EA           = READ_ASCII(inDir+inFile,TEMPLATE=tmplt)

  PRINT,'Saving struct to ' + EAbins_file + ' ...'
  SAVE,EA,tmplt,FILENAME=inDir+EAbins_file

END
