FUNCTION GENERATE_LIST_OF_RANDOM_LINESTYLES,N
  linestyle_list = LIST()

  FOR i=0,N-1 DO linestyle_list.add,FIX(5*RANDOMU(seed))

  RETURN,linestyle_list

END