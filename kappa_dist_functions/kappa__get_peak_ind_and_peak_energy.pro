PRO KAPPA__GET_PEAK_IND_AND_PEAK_ENERGY,Xorig,Yorig,peak_ind,peak_energy, $
                                        BULK_OFFSET=bulk_offset, $
                                        CHECK_FOR_HIGHER_FLUX_PEAKS=check_for_higher_flux_peaks, $
                                        MIN_PEAK_ENERGY=min_peak_energy

  COMPILE_OPT idl2


  b_offset                             = KEYWORD_SET(bulk_offset) ? bulk_offset : 0 ;bulk offset

  IF KEYWORD_SET(check_for_higher_flux_peaks) THEN BEGIN
     ;;Figure out where most energetic maximum is
     max_ys             = GET_N_MAXIMA_IN_ARRAY(Yorig,N=3,OUT_I=maxima_i)
     peak_y             = MAX(max_ys,max_y_ii)
     peak_ind           = maxima_i[max_y_ii]
     peak_energy        = Xorig[peak_ind]
     peak_tol_percent   = .25
     
     FOR i=0,N_ELEMENTS(maxima_i)-1 DO BEGIN
        testMax_X       = Xorig[maxima_i[i]]
        testMax         = max_ys[i]
        curMax_i        = maxima_i[i]
        PRINT,'testval:',STRCOMPRESS((ABS(testMax-peak_y)/peak_y),/REMOVE_ALL)
        IF testMax_X GT Xorig[peak_ind] AND (ABS(testMax-peak_y)/peak_y) LT peak_tol_percent THEN BEGIN
           peak_ind     = curMax_i
           peak_energy  = testMax_X
        ENDIF
     ENDFOR
     PRINT,peak_ind
  ENDIF ELSE BEGIN
     CASE 1 OF
        KEYWORD_SET(min_peak_energy): BEGIN
           inds         = WHERE(Xorig GE min_peak_energy)
           IF inds[0] EQ -1 THEN BEGIN
              PRINT,"Can't find any good energy inds! Consider lowering your energy requirement."
              STOP
           ENDIF
           max_y        = MAX(Yorig[inds],peak_ind)
           peak_ind     = inds[peak_ind]
           peak_ind    -= b_offset
           peak_energy  = Xorig[peak_ind]
        END
        ELSE: BEGIN
           max_y        = MAX(Yorig,peak_ind)
           peak_ind    -= b_offset
           peak_energy  = Xorig[peak_ind]
        END
     ENDCASE
     PRINT,peak_ind
  ENDELSE

END

