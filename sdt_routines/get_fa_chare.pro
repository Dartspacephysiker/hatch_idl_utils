;2017/02/21
FUNCTION GET_FA_CHARE, $
   T1=t1, $
   T2=t2, $
   ADD_CHARE_PANEL=add_chare_panel, $
   CHAREE=charEe, $
   CHAREI=charEi, $
   EEB_OR_EES=eeb_or_ees, $
   IEB_OR_IES=ieb_or_ies, $
   ENERGY_ELECTRONS=energy_electrons, $
   ENERGY_IONS=energy_ions, $
   EANGLE=eAngle, $
   IANGLE=iAngle, $
   TPLT_VARS=tplt_vars, $
   QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(eeb_or_ees) EQ 0 THEN BEGIN
     eeb_or_ees = 'ees'
  ENDIF

  IF N_ELEMENTS(ieb_or_ies) EQ 0 THEN BEGIN
     ieb_or_ies = 'ies'
  ENDIF

  IF N_ELEMENTS(charEe) EQ 0 AND N_ELEMENTS(charEi) EQ 0 THEN BEGIN
     charEe = 1B
  ENDIF

  IF N_ELEMENTS(eAngle) EQ 0 THEN BEGIN
     eAngle       = [360.-30.,30.]
     eAngleChare  = eAngle
  ENDIF 

  IF N_ELEMENTS(iAngle) EQ 0 THEN BEGIN
     iAngle       = [135.,225.]
     iAngleChari  = iAngle
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Chare panel
  ;; IF KEYWORD_SET(add_chare_panel) OR KEYWORD_SET(add_kappa_panel) OR KEYWORD_SET(add_Newell_panel) THEN BEGIN
  t1eeb = 0.D 
  t2eeb = 0.D
  bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t1eeb,/ST)
  bro   = CALL_FUNCTION('GET_FA_' + STRUPCASE(eeb_or_ees),t2eeb,/EN)
  t1eeb = N_ELEMENTS(t1) GT 0 ? t1 > t1eeb : t1eeb
  t2eeb = N_ELEMENTS(t2) GT 0 ? t2 < t2eeb : t2eeb

  IF ~KEYWORD_SET(load_from_offline) THEN BEGIN     

     IF KEYWORD_SET(charEe) THEN BEGIN
        GET_2DT,'j_2d_fs','fa_' + eeb_or_ees + '_c',NAME='Je',T1=t1eeb,T2=t2eeb,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
        GET_2DT,'je_2d_fs','fa_' + eeb_or_ees + '_c',NAME='Jee',T1=t1eeb,T2=t2eeb,ENERGY=energy_electrons,ANGLE=eAngleChare,/CALIB
     ENDIF
     IF KEYWORD_SET(charEi) THEN BEGIN
        GET_2DT,'j_2d_fs','fa_' + ieb_or_ies + '_c',NAME='Ji',T1=t1eeb,T2=t2eeb,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
        GET_2DT,'je_2d_fs','fa_' + ieb_or_ies + '_c',NAME='Jei',T1=t1eeb,T2=t2eeb,ENERGY=energy_ions,ANGLE=iAngleChari,/CALIB
     ENDIF

  ENDIF

  ;;For electrons
  IF KEYWORD_SET(charEe) THEN BEGIN

     ;;Remove_crap
     GET_DATA,'Je',DATA=tmp
     nJe                            = N_ELEMENTS(tmp.y)
     IF KEYWORD_SET(save_for_offline) THEN BEGIN
        Je_off  = tmp
        saveStr += 'Je_off,'
     ENDIF
     keep1                          = WHERE(FINITE(tmp.y) NE 0)
     keep2                          = WHERE(ABS(tmp.y) GT 0.0)

     GET_DATA,'Jee',DATA=tmp
     IF KEYWORD_SET(save_for_offline) THEN BEGIN
        Jee_off  = tmp
        saveStr += 'Jee_off,'
     ENDIF
     nJee                           = N_ELEMENTS(tmp.y)
     IF nJe NE nJee THEN STOP

     keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
     keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))

     GET_DATA,'Je',DATA=tmp
     tmp.x                          = tmp.x[keep1]
     tmp.y                          = tmp.y[keep1]
     je_tmp_time                    = tmp.x[keep2]
     je_tmp_data                    = tmp.y[keep2]
     STORE_DATA,'Je',DATA={x:TEMPORARY(je_tmp_time),y:TEMPORARY(je_tmp_data)}
     GET_DATA,'Jee',DATA=tmp
     tmp.x                          = tmp.x[keep1]
     tmp.y                          = tmp.y[keep1]
     jee_tmp_time                   = tmp.x[keep2]
     jee_tmp_data                   = tmp.y[keep2]
     STORE_DATA,'Jee',DATA={x:TEMPORARY(jee_tmp_time),y:TEMPORARY(jee_tmp_data)}

     GET_DATA,'Je',DATA=Je
     GET_DATA,'Jee',DATA=Jee

     chare                          = Jee.y/Je.y*6.242*1.0e11

  ENDIF

  ;;For ions
  IF KEYWORD_SET(charEi) THEN BEGIN

     GET_DATA,'Ji',DATA=tmp
     IF KEYWORD_SET(save_for_offline) THEN BEGIN
        Ji_off  = tmp
        saveStr += 'Ji_off,'
     ENDIF
     nJi                            = N_ELEMENTS(tmp.y)

     IF KEYWORD_SET(charEe) THEN BEGIN
        keep1                       = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
        keep2                       = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))
     ENDIF ELSE BEGIN
        keep1                       = WHERE(FINITE(tmp.y) NE 0)
        keep2                       = WHERE(ABS(tmp.y) GT 0.0)
     ENDELSE

     GET_DATA,'Jei',DATA=tmp
     IF KEYWORD_SET(save_for_offline) THEN BEGIN
        Jei_off  = tmp
        saveStr += 'Jei_off,'
     ENDIF
     nJei                           = N_ELEMENTS(tmp.y)
     IF nJi NE nJei THEN STOP
     keep1                          = CGSETINTERSECTION(keep1,WHERE(FINITE(tmp.y) NE 0))
     keep2                          = CGSETINTERSECTION(keep2,WHERE(ABS(tmp.y) GT 0.0))

     GET_DATA,'Ji',DATA=tmp
     tmp.x                          = tmp.x[keep1]
     tmp.y                          = tmp.y[keep1]
     ji_tmp_time                    = tmp.x[keep2]
     ji_tmp_data                    = tmp.y[keep2]
     STORE_DATA,'Ji',DATA={x:TEMPORARY(ji_tmp_time),y:TEMPORARY(ji_tmp_data)}
     GET_DATA,'Jei',DATA=tmp
     tmp.x                          = tmp.x[keep1]
     tmp.y                          = tmp.y[keep1]
     jei_tmp_time                   = tmp.x[keep2]
     jei_tmp_data                   = tmp.y[keep2]
     STORE_DATA,'Jei',DATA={x:TEMPORARY(jei_tmp_time),y:TEMPORARY(jei_tmp_data)}

     GET_DATA,'Ji',DATA=Ji
     GET_DATA,'Jei',DATA=Jei

     chari                          = Jei.y/Ji.y*6.242*1.0e11
     IF KEYWORD_SET(charEe) THEN BEGIN
        chari_interp     = DATA_CUT({x:Jei.x,y:chari},Jee.x,/IGNORE_NAN,GAP_DIST=3)
     ENDIF ELSE BEGIN
        chari_interp     = TEMPORARY(chari)
     ENDELSE

  ENDIF
  
  IF KEYWORD_SET(add_chare_panel) THEN BEGIN

     red                = 250
     green              = 130

     CASE 1 OF
        KEYWORD_SET(charEe) AND KEYWORD_SET(charEi): BEGIN
           labels       = ['Ion','Electron','Total']
           colors       = [red,green,20]
           chartot      = chare+chari_interp
           charEBounds  = [MIN(chare[WHERE(chare GT 0)]) + MIN(chari[WHERE(chari GT 0)]), $
                           MAX(chare[WHERE(chare GT 0)]) + MAX(chari[WHERE(chari GT 0)])]
           times        = Jee.x
           charEData    = {x:[[times],[times],[times]],y:[[chari_interp],[chare],[chartot]],label:labels}
        END
        KEYWORD_SET(charEe): BEGIN
           labels       = ['Electron']
           colors       = [green]

           chartot      = chare
           charEBounds  = [MIN(chare[WHERE(chare GT 0)]), $
                           MAX(chare[WHERE(chare GT 0)])]
           times        = Jee.x
           charEData    = {x:times,y:chare,label:labels}
        END
        KEYWORD_SET(charEi): BEGIN
           labels       = ['ion']
           colors       = red
           chartot      = chari_interp
           charEBounds  = [MIN(chari[WHERE(chari GT 0)]), $
                           MAX(chari[WHERE(chari GT 0)])]
           times        = Jei.x
           charEData    = {x:times,y:chari_interp,label:labels}
        END
     ENDCASE

     ;; charEBounds      = [MIN(chare[WHERE(chare GT 0)]) + MIN(chari[WHERE(chari GT 0)]), $
     ;;                     MAX(chare[WHERE(chare GT 0)]) + MAX(chari[WHERE(chari GT 0)])]
     ;; showLog_charE    = (ALOG10(MAX(chare[WHERE(chare GT 0)]))-ALOG10(MIN(chare[WHERE(chare GT 0)]))) GT 2
     showLog_charE    = (ALOG10(charEBounds[1])-ALOG10(charEBounds[0])) GT 2
     IF showLog_charE THEN BEGIN
        charEBounds[0] -= (charEBounds[0]*0.1)
        charEBounds[1] += (charEBounds[1]*0.1)
     ENDIF ELSE BEGIN
        charEBounds[0] /= 1.1
        charEBounds[1] *= 1.1
     ENDELSE

     STORE_DATA,'charepanel',DATA=charEData

     YLIM,'charepanel',charEBounds[0],charEBounds[1],showLog_charE
     OPTIONS,'charepanel','tplot_routine','mplot'
     OPTIONS,'charepanel','ytitle','E/q Volts'
     OPTIONS,'charepanel','labels',labels
     OPTIONS,'charepanel','colors',colors
     OPTIONS,'charepanel','labflag',-1

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars=['charepanel'] ELSE tPlt_vars=[tPlt_vars,'charepanel']

     ;; IF (KEYWORD_SET(screen_plot)) AND ~(KEYWORD_SET(save_png) OR KEYWORD_SET(save_ps)) THEN BEGIN
     ;;    LOADCT2,40
     ;;    TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT']
     ;; ENDIF

     IF ~KEYWORD_SET(quiet) THEN PRINT,'CharEPanel data stored as "charepanel"'

  ENDIF

  RETURN,charEData

END
