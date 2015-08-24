PRO GENERATE_SET_DEFAULT_PRO

  ;;Put the name of your PRO here

  ;;Put your list of words below, then use the following regexp search to generate the PRO keywords here
  ;;[[:blank:]]*\([[:alnum:]_]\)[[:blank:]]*
  ;;Replace with '\,(upcase \1)=\1,'


  ;;Put your list of words below, then use the following regexp search to generate the defaults:
  ;;[[:blank:]]*\([[:alnum:]_]\)[[:blank:]]*
  ;;Replace with '  def\,(capitalize \1) = '


  ;;Put your list of words below, then use the following regexp search to set unset keywords:
  ;;[[:blank:]]*\([[:alnum:]_]\)[[:blank:]]*
  ;;Replace with '  IF N_ELEMENTS(\1) EQ 0 THEN \1 = def\,(capitalize \1)'
  


  ;;DONE!

END