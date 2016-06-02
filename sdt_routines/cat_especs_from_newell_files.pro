PRO CAT_ESPECS_FROM_NEWELL_FILES,eSpec,eSpecs_newell,cat_i

  IF N_ELEMENTS(cat_i) NE 0 THEN BEGIN
     tmpeSpec = { x:eSpecs_newell.x[cat_i], $
                  y:eSpecs_newell.y[cat_i,*], $
                  v:eSpecs_newell.v[cat_i,*]}

  ENDIF ELSE BEGIN
     tmpeSpec = eSpecs_newell
  ENDELSE

  IF N_ELEMENTS(eSpec) EQ 0 THEN BEGIN
     eSpec = tmpeSpec
  ENDIF ELSE BEGIN

     eSpec = { x:[eSpec.x,tmpeSpec.x], $
               y:[eSpec.y,tmpeSpec.y], $
               v:[eSpec.v,tmpeSpec.v]}

  ENDELSE
  

END