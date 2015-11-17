FUNCTION PROMPT__CONV_QUANTITY_TO_POS_NEG_OR_ABS,data, $
   QUANTITY_NAME=qName, $
   USER_RESPONSE=user_response, $
   LUN=lun

  IF ~KEYWORD_SET(lun) THEN lun = -1

  ;; Define as a string before 
  choice   = ''
  breakout = 0
  ;; Read input from the terminal:

  nBefore = N_ELEMENTS(data)

  WHILE breakout EQ 0 DO BEGIN
     READ, choice, PROMPT='Make these data pos,neg,abs neg,abs, or cancel (p/n/an/a/c)? '

     CASE STRUPCASE(choice) OF
        'P': BEGIN
           PRINTF,lun,'Keep the pos data...'
           newData  = data[WHERE(data GE 0)]
           breakout = 1
        END
        'N': BEGIN
           PRINTF,lun,'Keep the negs...'
           newData  = data[WHERE(data LT 0)]
           breakout = 1
        END
        'AN': BEGIN
           PRINTF,lun,'Keep the negs, make em pos'
           newData  = ABS(data[WHERE(data LT 0)])
           breakout = 1
           END
        'A': BEGIN
           PRINTF,lun,'Absolute val...'
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

  nAfter = N_ELEMENTS(data)
  PRINTF,lun,"N before : " + STRCOMPRESS(nBefore,/REMOVE_ALL) + "; N after : " + STRCOMPRESS(nAfter,/REMOVE_ALL)

  RETURN,newData

END