FUNCTION GENERATE_LIST_OF_RANDOM_COLORS,N
  color_list = LIST()

  FOR i=0,N-1 DO color_list.add,FIX(255*RANDOMU(seed,3))

  RETURN,color_list

END