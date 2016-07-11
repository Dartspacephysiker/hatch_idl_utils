PRO PARSE_KAPPA_FIT_STRUCTS,fits, $
                            A=a, $
                            TIME=time, $
                            STRUCT_A=Astruct, $
                            NAMES_A=A_names, $
                            CHI2=chi2, $
                            PVAL=pVal, $
                            FITSTATUS=fitStatus

  COMPILE_OPT idl2

  NStructs = N_ELEMENTS(fits)

  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'A',VALUE=A
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'chi2',VALUE=chi2
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'pVal',VALUE=pVal
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'fitStatus',VALUE=fitStatus
  STR_ELEMENT_FROM_LIST_OF_STRUCTS,fits,'time',VALUE=time

  ;;Get A back in top shape
  A = REFORM(A,N_ELEMENTS(A)/NStructs,NStructs)

  A_names = ['bulk_energy', $
             'Temp', $
             'Kappa', $
             'N', $
             'delta_t', $       ;bogus
             'mass', $          ;(eV/(km/s)^2)
             'Bulk angle']
  Astruct = CREATE_STRUCT(A_names, $
                          REFORM(A[0,*]),REFORM(A[1,*]),REFORM(A[2,*]),REFORM(A[3,*]), $
                          REFORM(A[4,*]),REFORM(A[5,*]),REFORM(A[6,*]))

END