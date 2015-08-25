;2015/08/25
;Pro to print boxplot data
PRO PRINT_BPDATA,bpData,NAME=bpName,DATFORMAT=datFormat

  defDatFormat='F0.2'

  IF N_ELEMENTS(bpName) EQ 0 THEN bpName = 'Boxplot Data'
  IF N_ELEMENTS(datFormat) EQ 0 THEN datFormat = defDatFormat

  PRINT,'****Stats for ' + bpName + '****'
  PRINT,FORMAT='("MIN    :",T10,'+datFormat+')',bpData[0]
  PRINT,FORMAT='("Q1     :",T10,'+datFormat+')',bpData[1]
  PRINT,FORMAT='("MEDIAN :",T10,'+datFormat+')',bpData[2]
  PRINT,FORMAT='("Q3     :",T10,'+datFormat+')',bpData[3]
  PRINT,FORMAT='("MAX    :",T10,'+datFormat+')',bpData[4]
  

END