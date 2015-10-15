FUNCTION ILAT_TO_LSHELL,ilats

  lShells=(cos(ilats*!PI/180.))^(-2)

  return,lShells

END