FUNCTION PROMPT__CONV_QUANTITY_TO_POS_NEG_OR_ABS,data, $
   QUANTITY_NAME=qName, $
   INDICES=new_i, $
   USER_RESPONSE=user_response, $
   ADD_SUFF_TO_THIS_STRING=addSuff, $
   LUN=lun

  IF ~KEYWORD_SET(qName) then qName = 'these data'
  IF ~KEYWORD_SET(lun) THEN lun = -1
  IF ~KEYWORD_SET(addSuff) THEN addSuff = ''
  ;; Define as a string before 
  choice   = ''
  breakout = 0
  ;; Read input from the terminal:

  nBefore = N_ELEMENTS(data)

  WHILE breakout EQ 0 DO BEGIN
     READ, choice, PROMPT='Make ' + qName + ' pos,neg,abs neg,abs, or cancel (p/n/an/a/c)? '

     CASE STRUPCASE(choice) OF
        'P': BEGIN
           PRINTF,lun,'Keep the pos data...'
           new_i    = WHERE(data GE 0)
           newData  = data[new_i]
           addSuff  = addSuff + '--pos'
           breakout = 1
        END
        'N': BEGIN
           PRINTF,lun,'Keep the negs...'
           new_i    = WHERE(data LT 0)
           newData  = data[new_i]
           addSuff  = addSuff + '--neg'
           breakout = 1
        END
        'AN': BEGIN
           PRINTF,lun,'Keep the negs, make em pos'
           new_i    = WHERE(data LT 0)
           newData  = ABS(data[new_i])
           addSuff  = addSuff + '--abs_neg'
           breakout = 1
           END
        'A': BEGIN
           PRINTF,lun,'Absolute val...'
           addSuff  = addSuff + '--abs'
           new_i    = INDGEN(nBefore)
           newData  = ABS(data)
           breakout = 1
           END
        'C': BEGIN
           PRINTF,lun,'Cancel!'
           newData  = data
           breakout = 1
           END
        ELSE: PRINTF,lun,'Bad selection; please choose pos,neg,abs neg,abs, or cancel (p/n/an/a/c)'
     ENDCASE

  ENDWHILE

  nAfter = N_ELEMENTS(newData)
  PRINTF,lun,"N before : " + STRCOMPRESS(nBefore,/REMOVE_ALL) + "; N after : " + STRCOMPRESS(nAfter,/REMOVE_ALL)

  RETURN,newData

END