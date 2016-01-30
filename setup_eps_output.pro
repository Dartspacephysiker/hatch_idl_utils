;+
; NAME:                       SETUP_EPS_OUTPUT
;
;
;
; PURPOSE:                    Make some publishable figs, bro!
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; MODIFICATION HISTORY:       2016/01/15 Barnebarn
;
;-
PRO SETUP_EPS_OUTPUT,FILENAME=filename, $
                     OLD_DEVICE=old_device, $
                     CLOSE=close

  IF ~KEYWORD_SET(close) THEN BEGIN
     FOR i=0,N_ELEMENTS(filenames)-1 DO BEGIN
        IF STRMATCH(filenames[i],'*.png',/FOLD_CASE) THEN BEGIN
           filenames[i] = filenames[i].replace('.png','.eps',/FOLD_CASE)
           PRINT,FORMAT='("Swapped ending to make .eps   :",A0)',filenames[i]
        ENDIF
     ENDFOR
     
     old_device = !D.NAME
     IF STRMATCH(old_device,'PS',/FOLD_CASE) THEN BEGIN

     ENDIF ELSE BEGIN
        SET_PLOT, 'PS'
        DEVICE, /ENCAPSULATED, FILENAME=filename
        
        PRINT,'Device set to Encapsulated PostScript'
     ENDELSE
  ENDIF ELSE BEGIN
     DEVICE, /CLOSE
     PRINT,'Closed Encapsulated PostScript device ...'
  ENDELSE

END