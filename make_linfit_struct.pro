FUNCTION MAKE_LINFIT_STRUCT,x,y,Y_ERRORS=measures

     fitParams    = LINFIT(x, y, $
                           CHISQR=chisqr, $
                           COVAR=covar, $
                           MEASURE_ERRORS=measures, $
                           PROB=prob, $
                           SIGMA=sigma, $
                           YFIT=yfit)

  linFitStr       = { $
                    fitParams:fitParams, $
                    sigma:sigma, $  ;;Estimated goodness of fit; meaningless if no errors provided
                    measure_errors:KEYWORD_SET(measures) ? measures : MAKE_ARRAY(N_ELEMENTS(y),/DOUBLE), $
                    yFit:yfit, $
                    chiSqr:chisqr, $
                    prob:prob, $
                    coVar:covar $
                    } 

  RETURN,linFitStr

END