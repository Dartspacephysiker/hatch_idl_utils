PRO KAPPA__GET_DENSITY_ESTIMATES,dat, $
                                 OUTPUT_DENS__ANGLES=output_dens__angles, $
                                 OUTPUT_DENS__ENERGIES=output_dens__energies, $
                                 ERANGE_PEAK=eRange_peak, $
                                 DENS_EST_ERANGE=dens_est_eRange, $
                                 STRINGS=strings, $
                                 TXTOUTPUTDIR=txtOutputDir

  COMPILE_OPT idl2

  IF N_ELEMENTS(txtOutputDir) EQ 0 THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(output_dens__angles): BEGIN
           suff                  = '/dens_est--angles'
        END
        KEYWORD_SET(output_dens__energies): BEGIN
           suff                  = '/dens_est--energies'
        END
     ENDCASE
     SET_TXTOUTPUT_DIR,txtOutputDir,/FOR_SDT,/ADD_TODAY,/VERBOSE, $
                       ADD_SUFF=TEMPORARY(suff)
  ENDIF


  out_n_ests                     = !NULL
  out_peak_n_ests                = !NULL
  CASE 1 OF
     KEYWORD_SET(output_dens__angles): BEGIN
        out_N_str                = 'Angles (deg)'
        out_N_fN_str             = 'angles'
        var_delta                = 10
        nLoops                   = 17
        out_N_loop               = [[(INDGEN(nLoops)+1)*(-var_delta)], $
                                    [(INDGEN(nLoops)+1)*( var_delta)]]
        out_N_loop               = TRANSPOSE([[TRANSPOSE(out_N_loop)],[0,360]])
        nLoops++
        dim                      = 2
        FOR iLoop=0,nLoops-1 DO BEGIN
           tmp_N_est             = N_2D_FS(dat, $
                                           ENERGY=dens_est_eRange, $
                                           ANGLE=REFORM(out_N_loop[iLoop,*]))
           out_n_ests            = [out_n_ests,tmp_N_est]
        ENDFOR
        FOR iLoop=0,nLoops-1 DO BEGIN
           tmp_N_est             = N_2D_FS(dat, $
                                           ENERGY=eRange_peak, $
                                           ANGLE=REFORM(out_N_loop[iLoop,*]))
           out_peak_n_ests       = [out_peak_n_ests,tmp_N_est]
        ENDFOR
     END
     KEYWORD_SET(output_dens__energies): BEGIN
        out_N_str                = 'Energies (eV)'
        out_N_fN_str             = 'energies'
        var_delta                = 1.5
        en_start                 = 10 ;eV
        out_N_loop               = en_start
        nLoops                   = 1
        dim                      = 1
        WHILE out_N_loop[-1] LT 3.5e4 DO BEGIN
           out_N_Loop            = [out_N_loop,out_N_loop[-1]*var_delta]
           nLoops++
        ENDWHILE

        FOR iLoop=1,nLoops-1 DO BEGIN
           tmp_N_est             = N_2D_FS(dat, $
                                           ENERGY=[out_N_loop[0],out_N_loop[iLoop]], $
                                           ANGLE=e_angle)
           out_n_ests            = [out_n_ests,tmp_N_est]
        ENDFOR
     END
  ENDCASE

  out_dens                       = {loopType:out_N_str, $
                                    vars:out_N_loop, $
                                    N:out_n_ests, $
                                    N_range:[MIN(out_n_ests),MAX(out_n_ests)], $
                                    N_delta:(out_n_ests[1:-1]-out_n_ests[0:-2]), $
                                    var_delta:var_delta, $
                                    var_dim:dim, $
                                    is_multiplicative:KEYWORD_SET(output_dens__energies), $
                                    fName_suff:out_N_fN_str}

  IF KEYWORD_SET(output_dens__angles) THEN BEGIN
     out_peak_dens               = {loopType:out_N_str, $
                                    vars:out_N_loop, $
                                    N:out_peak_n_ests, $
                                    N_range:[MIN(out_peak_n_ests),MAX(out_peak_n_ests)], $
                                    N_delta:(out_n_ests[1:-1]-out_n_ests[0:-2]), $
                                    var_delta:var_delta, $
                                    var_dim:dim, $
                                    is_multiplicative:KEYWORD_SET(output_dens__energies), $
                                    fName_suff:out_N_fN_str}

     out_N_ratios            =out_peak_n_ests/out_n_ests
     out_dens_ratios             = {loopType:out_N_str, $
                                    vars:out_N_loop, $
                                    N:out_N_ratios, $
                                    N_range:[MIN(out_N_ratios),MAX(out_N_ratios)], $
                                    N_delta:(out_n_ests[1:-1]-out_n_ests[0:-2]), $
                                    var_delta:var_delta, $
                                    var_dim:dim, $
                                    is_multiplicative:KEYWORD_SET(output_dens__energies), $
                                    fName_suff:out_N_fN_str}
     out_dens_ratios.loopType    = 'peak/whole-spec N'
     out_dens_ratios.fName_suff  = 'ratios'
  ENDIF

  IF KEYWORD_SET(output_dens__angles) THEN BEGIN

     densFN                      = STRING(FORMAT='(A0,"--density_est__",A0,"--",A0,"--",A0,A0,"--orb_",A0,A0,"--en__",I0,"-",I0,".txt")', $
                                          strings.today, $
                                          out_dens.fName_suff, $
                                          strings.timeFNStrs[bounds[i]], $
                                          strings.eeb_or_ees, $
                                          strings.avgStr, $
                                          strings.orbStr, $
                                          strings.orbDate, $
                                          dens_est_eRange[0], $
                                          dens_est_eRange[1])

     PRINT_DENS_ESTIMATE_STRUCT,out_dens, $
                                ;; DENS_FILE_PREF=dens_
                                TO_FILE=densFN, $
                                OUTDIR=txtOutputDir

     peak_densFN                 = STRING(FORMAT='(A0,"--peak_density_est__",A0,"--",A0,"--",A0,A0,"--orb_",A0,"__",A0,"--peak_en__",I0,"-",I0,".txt")', $
                                          strings.today, $
                                          out_dens.fName_suff, $
                                          strings.timeFNStrs[bounds[i]], $
                                          strings.eeb_or_ees, $
                                          strings.avgStr, $
                                          strings.orbStr, $
                                          strings.orbDate, $
                                          eRange_peak[0], $
                                          eRange_peak[1])

     PRINT_DENS_ESTIMATE_STRUCT,out_peak_dens, $
                                ;; DENS_FILE_PREF=dens_
                                TO_FILE=peak_densFN, $
                                OUTDIR=txtOutputDir

     ratio_densFN                = STRING(FORMAT='(A0,"--density_est_ratios__",A0,"--",A0,"--",A0,A0,"--orb_",A0,"__",A0,"--peak_en__",I0,"-",I0,".txt")', $
                                          strings.today, $
                                          out_dens_ratios.fName_suff, $
                                          strings.timeFNStrs[bounds[i]], $
                                          strings.eeb_or_ees, $
                                          strings.avgStr, $
                                          strings.orbStr, $
                                          strings.orbDate, $
                                          eRange_peak[0], $
                                          eRange_peak[1])

     PRINT_DENS_ESTIMATE_STRUCT,out_dens_ratios, $
                                ;; DENS_FILE_PREF=dens_
                                TO_FILE=ratio_densFN, $
                                OUTDIR=txtOutputDir

  ENDIF

  IF KEYWORD_SET(output_dens__energies) THEN BEGIN
     densFN                      = STRING(FORMAT='(A0,"--density_est__",A0,"--",A0,"--",A0,A0,"--orb_",A0,"__",A0,A0,".txt")', $
                                          strings.today, $
                                          out_dens.fName_suff, $
                                          strings.timeFNStrs[bounds[i]], $
                                          strings.eeb_or_ees, $
                                          strings.avgStr, $
                                          strings.orbStr, $
                                          strings.orbDate, $
                                          strings.angleStr)

     PRINT_DENS_ESTIMATE_STRUCT,out_dens, $
                                ;; DENS_FILE_PREF=dens_
                                TO_FILE=densFN, $
                                OUTDIR=txtOutputDir


  ENDIF

END
