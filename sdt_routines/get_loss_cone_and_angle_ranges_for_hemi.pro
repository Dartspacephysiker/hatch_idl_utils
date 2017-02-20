PRO GET_LOSS_CONE_AND_ANGLE_RANGES_FOR_HEMI,t1,t2, $
   lc_angleRange, $
   i_angle,i_angle_up, $
   north_south, $
   ONLY_FIT_FIELDALIGNED_ANGLE=only_fit_fieldaligned_angle, $
   CUSTOM_E_ANGLERANGE=custom_e_angleRange, $
   OUT_E_ANGLE=e_angle, $
   ANGLESTR=angleStr, $
   JUST_ONE=just_one

  ;;get_orbit data if need be
  GET_DATA,'ALT',DATA=alt
  GET_DATA,'ILAT',DATA=ilat
  IF ( SIZE(alt,/TYPE) NE 8 ) OR $
     ( SIZE(alt,/TYPE) NE 8 )    $
  THEN BEGIN
     GET_FA_ORBIT,t1,t2 ;;,/all
     GET_DATA,'ALT',DATA=alt
     GET_DATA,'ILAT',DATA=ilat
  ENDIF
  
  IF KEYWORD_SET(only_fit_fieldaligned_angle) THEN BEGIN
     lcw               = REPLICATE(5,N_ELEMENTS(alt.y))
  ENDIF ELSE BEGIN
     loss_cone_alt     = alt.y*1000.0
     loss_cone_alt     = alt.y*1000.0
     lcw               = LOSS_CONE_WIDTH(loss_cone_alt)*180.0/!DPI
  ENDELSE
  
  ;; GET_DATA,'ILAT',DATA=ilat
  north_south       = ABS(ilat.y)/ilat.y
  
  ;;Loss cone stuff
  lc_angleRange     = !NULL
  i_angle           = !NULL
  i_angle_up        = !NULL

  ;;Collect angles
  IF KEYWORD_SET(just_one) THEN BEGIN
     north_south    = north_south[0]
     lcw            = lcw[0]
  ENDIF
  
  FOR i=0,N_ELEMENTS(north_south)-1 DO BEGIN
     IF north_south[i] EQ -1 THEN BEGIN
        lc_angleRange     = [[lc_angleRange],[180.-lcw,180+lcw]] ; for Southern Hemis.
        
        ;; i_angle  = [270.0,90.0]	
        ;; eliminate ram from data
        i_angle     = [[i_angle],[180.0,360.0]]
        i_angle_up  = [[i_angle_up],[270.0,360.0]]
        
     ENDIF ELSE BEGIN
        lc_angleRange     = [[lc_angleRange],[360.-lcw,lcw]] ;	for Northern Hemis.
        ;; i_angle  = [90.,270.0]
        ;; eliminate ram from data
        i_angle     = [[i_angle],[0.0,180.0]]
        i_angle_up  = [[i_angle_up],[90.0,180.0]]
        
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