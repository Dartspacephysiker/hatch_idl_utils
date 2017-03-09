;;2017/03/09
FUNCTION WHOAMIHOST

  COMPILE_OPT IDL2,STRICTARRSUBS

  SPAWN,'hostname',bros

  navn   = ''

  noDice = 1B
  bro    = ['-','.']
  tries  = 0
  FOREACH dude,bro DO BEGIN

     CASE N_ELEMENTS(STRSPLIT(bros,dude,/EXTRACT)) OF
        1: BEGIN
           ;; PRINT,"WHAHAAHAHTTTT"
        END
        3: BEGIN
           navn    = (STRSPLIT(bros,dude,/EXTRACT))[0]
           noDice  = 0B
        END
     ENDCASE

  ENDFOREACH

  RETURN,navn
  
END
