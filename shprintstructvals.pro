PRO shPrintStructVals, structIn, index_start, index_end 

structIn_tags=tag_names(structIn)
  
  CASE N_Params() OF
     
     1: BEGIN
        PRINT, "Usage: shPrintStructVals,structIn,index_start,index_end"
        PRINT, "Provide name of structure and at least one index..."
     ENDCASE
     
     2: BEGIN
        FOR i=0,N_TAGS(structIn)-1 DO PRINT,structIn_tags(i),' = ',structIn.(i)(index_start)
     ENDCASE
    
     3: BEGIN
        FOR i=0,N_TAGS(structIn)-1 DO PRINT,structIn_tags(i),' = ',structIn.(i)(index_start:index_end)
     ENDCASE
  ENDCASE

END 
