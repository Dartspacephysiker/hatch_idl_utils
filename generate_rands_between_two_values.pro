FUNCTION GENERATE_RANDS_BETWEEN_TWO_VALUES,nVals,min,max,NO_DUPLICATES=no_duplicates,SORT_PLEASE=sort_please

  diff=max-min
  this=ROUND(randomu(seed,/UNIFORM)*diff+min)
  FOR i=1,nVals-1 DO this=[this,ROUND(randomu(seed,/UNIFORM)*diff+min)]
  check_dupes,this,out_dupe_i=out_dupe_i
  
  WHILE N_ELEMENTS(out_dupe_i) GT 0 DO BEGIN
     FOR i=out_dupe_i[0],N_ELEMENTS(out_dupe_i)-1 DO this[out_dupe_i[i]]=ROUND(randomu(seed,/UNIFORM)*diff+min)
     check_dupes,this,out_dupe_i=out_dupe_i
  ENDWHILE

  IF N_ELEMENTS(sort_please) GT 0 THEN BEGIN
     IF sort_please THEN this=this(sort(this))
  ENDIF


  RETURN, this
END