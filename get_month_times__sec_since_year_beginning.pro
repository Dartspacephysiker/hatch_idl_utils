;2016/04/29
FUNCTION GET_MONTH_TIMES__SEC_SINCE_YEAR_BEGINNING,OUT_MONTHNAMES=monthNames

  COMPILE_OPT idl2

  monthDelta  = DOUBLE(LONG([0,31,28,31,30,31,30,31,31,30,31,30])*24*60*60)
  monthSum    = TOTAL(monthDelta,/CUMULATIVE,/DOUBLE)
  monthNames  = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']


  RETURN,monthSum

END