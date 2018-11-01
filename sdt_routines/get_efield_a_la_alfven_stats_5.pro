;2018/11/01
FUNCTION GET_EFIELD_A_LA_ALFVEN_STATS_5,BURST=burst

  ;; COMPILE_OPT,idl2,S

  data_valid=1B
  dat=GET_FA_FIELDS('MagDC',t,/START)
  IF dat.valid EQ 0 THEN BEGIN
     PRINT,' ERROR: No FAST mag data-get_fa_fields returned invalid data'
     data_valid=0B
  ENDIF

  dat=GET_FA_FIELDS('V5-V8_S',t,/START)

  IF ~dat.valid THEN BEGIN
     PRINT,' ERROR: No FAST V5-V8 data-GET_FA_FIELDS returned invalid data'
     data_valid=0B
  ENDIF ELSE BEGIN

     ;; spacecraft_potential=GET_FA_FIELDS('V8_S',time_ranges(jjj,0),time_ranges(jjj,1))

     efieldV58=GET_FA_FIELDS('V5-V8_S',time_ranges(jjj,0),time_ranges(jjj,1)) ;WANT

     efieldV1214=GET_FA_FIELDS('V1-V2_S',time_ranges(jjj,0),time_ranges(jjj,1)) ;WANT

     IF efieldV1214.valid EQ 0 THEN BEGIN
        PRINT,'No V1-V2_S data - trying V1-V4_S'
        efieldV1214=GET_FA_FIELDS('V1-V4_S',time_ranges(jjj,0),time_ranges(jjj,1)) ;WANT
        IF efieldV1214.valid EQ 0 AND KEYWORD_SET(burst) THEN BEGIN
           PRINT,'No V1-V4_S data - trying V1-V2_4k (burst)'
           efieldV1214=GET_FA_FIELDS('V1-V2_4k',time_ranges(jjj,0),time_ranges(jjj,1)) ;WANT
           IF efieldV1214.valid EQ 0 THEN BEGIN
              PRINT,'No V1-V2_4k data - trying V1-V4_4k (burst)'
              efieldV1214=GET_FA_FIELDS('V1-V4_4k',time_ranges(jjj,0),time_ranges(jjj,1)) ;WANT
              IF efieldV1214.valid EQ 0 THEN BEGIN
                 PRINT,'No FAST fields data-GET_FA_FIELDS returned invalid data'
                 data_valid=0B
              ENDIF
           ENDIF
        ENDIF ELSE BEGIN
           PRINT,'No FAST fields data-GET_FA_FIELDS returned invalid data'
           data_valid=0B
        ENDELSE
     ENDIF 
  ENDELSE
  
  IF ~data_valid THEN BEGIN
     RETURN,0
  ENDIF

  ;;get E field and B field on same time scale
  ;; efields_combine=combinets({x:efieldV1214.time,y:efieldV1214.comp1},{x:efieldV58.time,y:efieldV58.comp1})
  FA_FIELDS_COMBINE,efieldV1214,efieldV58,result=efields_combine,/talk
  
  ;;get magnitude of electric and magnetic field
  ;; for k=0,10,1 do BEGIN
  ;;    PRINT, "This is efieldV1214.comp1["+string(k)+"]: " + string(efieldV1214.comp1[k])
  ;;    PRINT, "This is efieldV58.comp1["+string(k)+"]: " + string(efieldV58.comp1[k])
  ;;    PRINT, "This is efields_combine["+string(k)+"]: " + string(efields_combine[k])
  ;; endfor
  ;; help, efieldV1214,/str
  ;; help, efieldV58,/str
  ;; help,efields_combine
  efield={x:efieldV1214.time,y:sqrt(efieldV1214.comp1^2+efields_combine^2)}


  get_data,'MagDCcomp1',data=magxDC
  get_data,'MagDCcomp2',data=magyDC
  get_data,'MagDCcomp3',data=magzDC

  ;;get the prootn cyc frequency for smoothing the e field data later
  proton_cyc_freq={x: magxDC.x, $ 
                   y: 1.6e-19*sqrt(magxDC.y^2+magyDC.y^2+magzDC.y^2)*1.0e-9/1.67e-27/(2.*!DPI)}; in Hz

  efield = CREATE_STRUCT(efield, $
                         'proton_cyc_freq',TEMPORARY(proton_cyc_freq))

  RETURN,efield
END
