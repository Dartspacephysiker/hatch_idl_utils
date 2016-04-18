;2015/08/13 Now we can check for duplicates in the data!
;2016/04/18 Added HAS_DUPES keyword
PRO CHECK_DUPES,data,rev_ind,dupes_rev_ind,dataenum, $
                OUT_DUPE_I=out_dupe_i, $
                N_DUPES=n_dupes, $
                PRINTDUPES=printDupes, $
                PRINT_SAMPLE=print_sample, $
                NSAMPLETOPRINT=nSampleToPrint, $
                HAS_DUPES=has_dupes

  IF ~KEYWORD_SET(nSampleToPrint) THEN nSampleToPrint = 100

  IF N_ELEMENTS(data) EQ 0 THEN BEGIN
     PRINT,"CHECK_DUPES: Provided array is empty! Returning..."
     RETURN
  ENDIF
  
  sorteddata    = data[SORT(data)]

  dataenum      = SORTEDDATA[UNIQ(sorteddata)]
  mappeddata    = VALUE_LOCATE(dataenum, data)

  h             = HISTOGRAM(mappeddata,REVERSE_INDICES=R)

  dupes         = WHERE(h gt 1,/NULL)
  IF N_ELEMENTS(dupes) EQ 0 THEN BEGIN
     PRINT,"No duplicates in this array!"
     out_dupe_i = !NULL
     n_dupes    = 0
     has_dupes  = 0
     RETURN
  ENDIF

  rev_ind       = r
  dupes_rev_ind = dupes

  ;;master dupe ind list
  out_dupe_i    = R[R[dupes[0]] : R[dupes[0]+1]-1]
  FOR i=1,N_ELEMENTS(dupes)-1 DO BEGIN
     out_dupe_i = [out_dupe_i, (R[R[dupes[i]] : R[dupes[i]+1]-1]) ]
  ENDFOR

  totDupes      = TOTAL(h[dupes])
  IF totDupes GT 500  THEN BEGIN 

     answer     = ''
     IF N_ELEMENTS(printDupes) EQ 0 THEN BEGIN
        IF ~KEYWORD_SET(print_sample) THEN BEGIN
           READ,answer, PROMPT="More than 500 duplicates (" + STRCOMPRESS(totDupes,/REMOVE_ALL) + ", in fact)!  Print them? [y/n/s(=print sample)]"
        ENDIF ELSE BEGIN
           PRINT,"Printing sample of dupes ..."
        ENDELSE
     ENDIF ELSE BEGIN
        PRINT,"printDupes: " + printDupes
        answer  = printDupes
     ENDELSE

     IF STRLOWCASE(STRMID(answer,0,1)) NE 'y' AND STRLOWCASE(STRMID(answer,0,1)) NE 's' THEN BEGIN
        PRINT,"OK, exiting..."
        RETURN
     ENDIF
  ENDIF ELSE BEGIN
     answer = 'y' 
  ENDELSE

  IF STRLOWCASE(STRMID(answer,0,1)) EQ 'y' THEN BEGIN
     print, "Duplicate elements"
     print, "==================="
     Print, dataenum[dupes]
     print, ''
     
     print, "Indices of dupes"
     print, "================"
     FOR i=0,N_ELEMENTS(dupes)-1 DO BEGIN
        print,'ELEMENT: ' + STRCOMPRESS(dataenum(dupes[i]),/REMOVE_ALL)
        print,R[R[dupes[i]] : R[dupes[i]+1]-1]
        print,''
     ENDFOR
     
  ENDIF ELSE BEGIN
     PRINT,"Sample of dupes"
     PRINT,"==============="
     PRINT,FORMAT='("Index",T10,"Value",T20,"N")'
     fmtString             = '(I0,T10,G9.2,T20,I0)'
     nSampleToPrint        = totDupes < nSampleToPrint
     FOR i=0,nSampleToPrint-1 DO BEGIN
        iDupe              = out_dupe_i[FIX(RANDOMU(seed)*totDupes)]
        PRINT,FORMAT=fmtString,iDupe,data[iDupe],N_ELEMENTS(WHERE(data EQ data[iDupe]))
     ENDFOR
  ENDELSE

  print,"Total number of dupes: " + STRCOMPRESS(totDupes)

  n_dupes                  = totDupes
  has_dupes                = 1

END