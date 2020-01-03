PRO GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI,t1,t2, $
   lc_angleRange, $
   i_angle,i_angle_up, $
   north_south, $
   ALLEXCLATM_ARANGE=allExclAtm_aRange, $
   EARTHWARD_ARANGE=earthward_aRange, $
   ANTIEARTHWARD_ARANGE=antiearthward_aRange, $
   OUT_LCW=lcw, $
   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
   CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
   UPGOING=upgoing, $
   OUT_E_ANGLE=e_angle, $
   OUT_MAPRATIO=mapRatio, $
   ANGLESTR=angleStr, $
   SDTSTRUCT=struc, $
   JUST_ONE=just_one, $
   IGNORE_MIXED_HEMISPHERE=ignore_mixed_hemisphere

  ;;get_orbit data if need be
  IF N_ELEMENTS(struc) EQ 0 THEN BEGIN

     ;; GET_DATA,'ALT',DATA=alt
     ;; GET_DATA,'ILAT',DATA=ilat
     ;; IF ( SIZE(alt,/TYPE) NE 8 ) OR $
     ;;    ( SIZE(ilat,/TYPE) NE 8 )    $
     ;; THEN BEGIN
     GET_FA_ORBIT,t1,t2,/ALL,STRUC=struc
        ;; GET_DATA,'ALT',DATA=alt
        ;; GET_DATA,'ILAT',DATA=ilat
     ;; ENDIF
  ENDIF;;  ELSE BEGIN
  ;;    ;; alt = {x: struc.x, $
  ;;    ;;        y: struc.alt}
  ;;    ;; ilat = {x: struc.x, $
  ;;    ;;         y: struc.ilat}

  ;; ENDELSE

  IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
     lcw               = FLOAT(REPLICATE(5,N_ELEMENTS(alt.y)))
     PRINT,"How is the above line in any way meaningful?"
     STOP
  ENDIF ELSE BEGIN
     ;; loss_cone_alt     = alt.y*1000.0
     ;; lcw               = FLOAT(LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI)
     mag1      = (struc.B_model[*,0]*struc.B_model[*,0]+ $
                  struc.B_model[*,1]*struc.B_model[*,1]+ $
                  struc.B_model[*,2]*struc.B_model[*,2])^0.5
     mag2      = (struc.bFoot[*,0]*struc.bFoot[*,0]+ $
                  struc.bFoot[*,1]*struc.bFoot[*,1]+ $
                  struc.bFoot[*,2]*struc.bFoot[*,2])^0.5
     mapRatio  = TEMPORARY(mag2)/TEMPORARY(mag1)
     lcw       = 180./!DPI * ATAN(SQRT(1.D/(mapRatio-1.D)))

  ENDELSE
  
  north_south       = LONG(ABS(struc.ilat)/struc.ilat)

  IF KEYWORD_SET(just_one) THEN BEGIN
     IF N_ELEMENTS(WHERE(north_south EQ north_south[0])) NE N_ELEMENTS(north_south) AND ~KEYWORD_SET(ignore_mixed_hemisphere) $
     THEN BEGIN
        PRINT,"You're in a mixed-hemisphere situation, which spells trouble. Better call Doc."
        STOP
     ENDIF

     north_south    = north_south[0]
     lcw            = lcw[0]
  ENDIF
  
  ;;Dus one?
  nHere                = N_ELEMENTS(north_south)
  IF KEYWORD_SET(just_one) THEN BEGIN
     lc_angleRange     = MAKE_ARRAY(2,/FLOAT,VALUE=0.0)
     allExclAtm_aRange = MAKE_ARRAY(2,/FLOAT,VALUE=0.0)
     earthward_aRange  = MAKE_ARRAY(2,/FLOAT,VALUE=0.0)
     antiearthward_aRange = MAKE_ARRAY(2,/FLOAT,VALUE=0.0)
     i_angle           = MAKE_ARRAY(2,/FLOAT,VALUE=0.0)
     i_angle_up        = MAKE_ARRAY(2,/FLOAT,VALUE=0.0)
  ENDIF ELSE BEGIN
     lc_angleRange     = MAKE_ARRAY(2,nHere,/FLOAT,VALUE=0.0)
     allExclAtm_aRange = MAKE_ARRAY(2,nHere,/FLOAT,VALUE=0.0)
     earthward_aRange  = MAKE_ARRAY(2,nHere,/FLOAT,VALUE=0.0)
     antiearthward_aRange = MAKE_ARRAY(2,nHere,/FLOAT,VALUE=0.0)
     i_angle           = MAKE_ARRAY(2,nHere,/FLOAT,VALUE=0.0)
     i_angle_up        = MAKE_ARRAY(2,nHere,/FLOAT,VALUE=0.0)
  ENDELSE
  
  ;;Collect angles
  FOR i=0,N_ELEMENTS(north_south)-1 DO BEGIN
     IF north_south[i] EQ -1 THEN BEGIN

        ;; for Southern Hemisphere

        earthward_aRange[*,i]      = [90.     ,270.    ]
        antiearthward_aRange[*,i]  = [270.    ,90.     ]

        CASE 1 OF
           KEYWORD_SET(upgoing): BEGIN

              ;; lc_angleRange     = [[lc_angleRange],[360.-lcw,lcw]]
              ;; ;; i_angle  = [90.,270.0]
              ;; ;; eliminate ram from data
              ;; i_angle     = [[i_angle],[0.0,180.0]]
              ;; i_angle_up  = [[i_angle_up],[90.0,180.0]]

              lc_angleRange[*,i] = [360.-lcw[i],lcw[i]]
              ;; i_angle  = [90.,270.0]

              ;; eliminate ram from data
              i_angle[*,i]    = [ 0.0,180.0]
              i_angle_up[*,i] = [90.0,180.0]

           END
           ELSE: BEGIN

              ;; lc_angleRange     = [[lc_angleRange    ],[180.-lcw,180.+lcw]] 
              ;; allExclAtm_aRange = [[allExclAtm_aRange],[lcw     ,360.-lcw]] 
              ;; earthward_aRange  = [[earthward_aRange ],[90.     ,270.    ]] 

              lc_angleRange[*,i]     = [180.-lcw[i],180.+lcw[i]]
              allExclAtm_aRange[*,i] = [lcw[i]     ,360.-lcw[i]]

              ;; i_angle  = [270.0,90.0]	
              ;; eliminate ram from data
              ;; i_angle     = [[i_angle]   ,[180.0,360.0]]
              ;; i_angle_up  = [[i_angle_up],[270.0,360.0]]

              i_angle[*,i]    = [180.0,360.0]
              i_angle_up[*,i] = [270.0,360.0]


           END
        ENDCASE
        
     ENDIF ELSE BEGIN

        ;; for Northern Hemis.

        earthward_aRange[*,i]      = [270.    ,90.    ]
        antiearthward_aRange[*,i]  = [90.     ,270.   ]

        CASE 1 OF
           KEYWORD_SET(upgoing): BEGIN

              ;; lc_angleRange     = [[lc_angleRange],[180.-lcw,180+lcw]] 
              ;; ;; i_angle  = [270.0,90.0]	
              ;; ;; eliminate ram from data
              ;; i_angle     = [[i_angle],[180.0,360.0]]
              ;; i_angle_up  = [[i_angle_up],[270.0,360.0]]

              lc_angleRange[*,i] = [180.-lcw[i],180+lcw[i]]
              ;; i_angle  = [270.0,90.0]	
              ;; eliminate ram from data
              i_angle[*,i]     = [180.0,360.0]
              i_angle_up[*,i]  = [270.0,360.0]

           END
           ELSE: BEGIN

              ;; lc_angleRange     = [[lc_angleRange],[360.-lcw,lcw]] 
              ;; allExclAtm_aRange = [[allExclAtm_aRange],[180.+lcw,180.-lcw]]
              ;; earthward_aRange  = [[earthward_aRange ],[270.     ,90.    ]] 

              ;; ;; i_angle  = [90.,270.0]
              ;; ;; eliminate ram from data
              ;; i_angle     = [[i_angle],[0.0,180.0]]
              ;; i_angle_up  = [[i_angle_up],[90.0,180.0]]
              
              lc_angleRange[*,i]     = [360.-lcw[i],     lcw[i]]
              allExclAtm_aRange[*,i] = [180.+lcw[i],180.-lcw[i]]

              ;; i_angle  = [90.,270.0]
              ;; eliminate ram from data
              i_angle[*,i]     = [0.0,180.0]
              i_angle_up[*,i]  = [90.0,180.0]
              
           END
        ENDCASE

     ENDELSE
  ENDFOR

  ;;Handle angle ranges
  IF N_ELEMENTS(custom_e_angleRange) EQ 0 THEN BEGIN
     PRINT,FORMAT='("Using loss-cone angle range: [",F0.2,",",F0.2,"]")', $
           lc_angleRange[0], $
           lc_angleRange[1]
     e_angle           = lc_angleRange

     IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
        angleStr       = STRING(FORMAT='("-loss-cone_e_angle_",F0.1,"-",F0.1)', $
                          lc_angleRange[0], $
                          lc_angleRange[1])
     ENDIF ELSE BEGIN
        angleStr       = STRING(FORMAT='("-only_field-aligned_e_angle_",F0.1,"-",F0.1)', $
                          lc_angleRange[0], $
                          lc_angleRange[1])
     ENDELSE
  ENDIF ELSE BEGIN
     PRINT,FORMAT='("Using provided angle range: [",F0.2,",",F0.2,"]")', $
           custom_e_angleRange[0], $
           custom_e_angleRange[1]
     e_angle           = custom_e_angleRange
     angleStr          = STRING(FORMAT='("-e_angle_",F0.1,"-",F0.1)', $
                       custom_e_angleRange[0], $
                       custom_e_angleRange[1])
  ENDELSE


END
