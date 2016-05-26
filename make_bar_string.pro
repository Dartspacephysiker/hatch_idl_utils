;2016/05/26 Yes, very cheap. It just makes a text bar
PRO MAKE_BAR_STRING,barStr,str_needing_bar,barSym

  IF N_PARAMS() LT 3 THEN barSym = '*'

  barStr          = barSym
  FOR i=1,STRLEN(str_needing_bar) DO barStr += barSym

END

  