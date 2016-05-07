FUNCTION CONV_QUANTITY_TO_POS_NEG_OR_ABS,data, $
                                         QUANTITY_NAME=qName, $
                                         ONLY_POS=only_pos, $
                                         ONLY_NEG=only_neg, $
                                         ABSVAL=absVal, $
                                         INDICES=new_i, $
                                         USER_RESPONSE=user_response, $
                                         ADD_SUFF_TO_THIS_STRING=addSuff, $
                                         LUN=lun

  IF ~KEYWORD_SET(qName) then qName = 'these data'
  IF ~KEYWORD_SET(lun) THEN lun = -1
  IF ~KEYWORD_SET(addSuff) THEN addSuff = ''

  nBefore = N_ELEMENTS(data)

  CASE 1 OF
     KEYWORD_SET(only_pos) : BEGIN
        PRINTF,lun,'Keep the pos data...'
        new_i    = WHERE(data GE 0)
        newData  = data[new_i]
        addSuff  = addSuff + '--pos'
        breakout = 1
     END
     KEYWORD_SET(only_neg) : BEGIN
        PRINTF,lun,'Keep the negs, make em pos'
        new_i    = WHERE(data LT 0)
        newData  = ABS(data[new_i])
        addSuff  = addSuff + '--abs_neg'
        breakout = 1
     END
     KEYWORD_SET(absVal) : BEGIN
        PRINTF,lun,'Absolute val...'
        addSuff  = addSuff + '--abs'
        new_i    = INDGEN(nBefore)
        newData  = ABS(data)
        breakout = 1
     END
     ELSE: $
        newData = prompt__conv_quantity_to_pos_neg_or_abs(data, $
                                                          INDICES=new_i, $
                                                          ADD_SUFF_TO_THIS_STRING=addSuff, $
                                                          USER_RESPONSE=user_response, $
                                                          LUN=lun)
  ENDCASE
  
  nAfter = N_ELEMENTS(newData)
  PRINTF,lun,"N before : " + STRCOMPRESS(nBefore,/REMOVE_ALL) + "; N after : " + STRCOMPRESS(nAfter,/REMOVE_ALL)

  RETURN,newData

END