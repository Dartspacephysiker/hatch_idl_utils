;2018/03/02
function get_phase_from_attctrl_spence,debug=debug,quiet=quiet

; patch the Sun phase information from attitude control quantities


; check that housekeeping is displayed in sdt - if not notify user and return

; JBV, 2011/05/18.   If we are running Multi-User SDT, we need
; to get the SDT index for this run.  Otherwise "showDQIs" won't
; return.  If this is old, single-user SDT, "sdt_idx" is returned
; as 255 and we handle the call in the old way.
sdt_idx2 = get_sdt_run_idx()

prog = getenv('FASTBIN') + '/showDQIs'
if ((sdt_idx2 GE 0) AND (sdt_idx2 LT 100)) then begin
    if (sdt_idx2 GE 10) then begin
        sidstr = string(sdt_idx2, format='(I2)')
    endif else begin
        sidstr = string(sdt_idx2, format='(I1)')
    endelse
    spawn, [prog, sidstr], result, /noshell
endif else begin
    spawn, prog, result, /noshell
endelse
b = where (strpos(result,'AttitudeCtrl') ge 0,nb)

if (nb eq 0) then begin

    print,''
    print,'UCLA_MAG_DESPIN needs AttitudeCtrl data from SDT'
    print,'  Add any of the AttitudeCtrl data quantities (e.g. SUN)'
    print,''
    return,0

endif else begin

   IF ~KEYWORD_SET(quiet) THEN BEGIN
      print,''
      print,'Getting phase data from AttitudeCtrl'
      print,''
   ENDIF
   
   data=get_ts_from_sdt('AttitudeCtrl',2001,/all)

;  force sun into data.comp24/25 - under test - now using MUE reported times etc.
;  only force in if object not 176

   patch = intarr(n_elements(data.comp24))

      safety=data

;     subtract MUE - OBJ OFFSET time from MUE reported times

      data.comp16 = data.comp16 - 0.000183105d0
      data.comp22 = data.comp22 - 0.000183105d0

      dc = data.comp17(1:*)-data.comp17(0:*)
      bcs = where (dc ne 0 and data.comp28(1:*) ne 176, ncs)

      if (debug) then begin
         print,'NCS',ncs
         print,'MEDIAN MUE-OBJ',median(data.comp16-data.comp24,/even)
      endif
      if (ncs ne 0) then begin
         data.comp24(bcs+1)=data.comp16(bcs+1)
         data.comp25(bcs+1)=data.comp17(bcs+1)
         data.comp28(bcs+1)=176
         patch(bcs+1)=1
         if (bcs(0) eq 0) then begin
            data.comp24(0)=data.comp16(0)
            data.comp25(0)=data.comp17(0)
            data.comp28(0)=176
            patch(0)=1
         endif
         dt = data.comp24(1:*)-data.comp24(0:*)
         nsp = round(dt/data.comp26)
         b_notz = where (nsp ne 0, n_notz)
         if (n_notz gt 0) then dt(b_notz) = dt(b_notz)/nsp(b_notz)
         b_isz = where (nsp eq 0, n_isz)
         if (n_isz gt 0) then dt(b_isz) = !values.d_nan
         data.comp26(bcs+1)=dt(bcs)
         if (bcs(0) eq 0) then data.comp26(0)=data.comp26(1)
         if (debug) then begin
            plot,data.comp24-safety.comp24,psym=3,yrange=[-.01,.01]
            ans = ''
            read, ans, prompt='Are new times reasonable? '
            if (ans eq 'N' or ans eq 'n') then begin
               data=safety
               patch(*)=0
            endif
         endif
      endif
   spin_per=data.comp26
   spin_zero=data.comp24 + data.time(0) - (data.time(0) mod 86400.d0)
   is_sun = data.comp28

   nadir_zero=data.comp22 + data.time(0) - (data.time(0) mod 86400.d0)

   phase_data={spin_zero:spin_zero,spin_per:spin_per,is_sun:is_sun,patch:patch,nadir_zero:nadir_zero}
   return,phase_data

endelse

end
PRO GET_FA_SDT_ORBIT,orbit

  COMPILE_OPT IDL2,STRICTARRSUBS

  phase_data = GET_PHASE_FROM_ATTCTRL_SPENCE(DEBUG=0,/QUIET)
  nn = n_elements(phase_data.nadir_zero)-1L
  t1 = phase_data.nadir_zero[0]
  t2 = phase_data.nadir_zero[nn]
  ;; t2 = t1+10
  GET_FA_ORBIT,t1,t2,delta=60.,/definitive,/NO_STORE,STRUC=struc
  nHere = N_ELEMENTS(struc.orbit)
  orbit = (TEMPORARY(struc)).orbit[TEMPORARY(nHere)/2]

END
