;2015/08/13 Now we can check for duplicates in the data!
PRO CHECK_DUPES,data,rev_ind,dupes_rev_ind,dataenum,OUT_DUPE_I=out_dupe_i,PRINTDUPES=printDupes

  IF N_ELEMENTS(data) EQ 0 THEN BEGIN
     PRINT,"CHECK_DUPES: Provided array is empty! Returning..."
     RETURN
  ENDIF
  
  sorteddata = data[Sort(data)]

  dataenum = sorteddata[Uniq(sorteddata)]
  mappeddata = Value_Locate(dataenum, data)

  h = histogram(mappeddata,REVERSE_INDICES=R)

  dupes=Where(h gt 1,/NULL)
  IF N_ELEMENTS(dupes) EQ 0 THEN BEGIN
     PRINT,"No duplicates in this array!"
     out_dupe_i = !NULL
     RETURN
  ENDIF

  rev_ind = r
  dupes_rev_ind = dupes

  ;;master dupe ind list
  out_dupe_i = R[R[dupes[0]] : R[dupes[0]+1]-1]
  FOR i=1,N_ELEMENTS(dupes)-1 DO BEGIN
     out_dupe_i=[out_dupe_i, (R[R[dupes[i]] : R[dupes[i]+1]-1]) ]
  ENDFOR

  totDupes = TOTAL(h[dupes])
  IF totDupes GT 500  THEN BEGIN 

     answer = ''
     IF N_ELEMENTS(printDupes) EQ 0 THEN $
        READ,answer, PROMPT="More than 500 duplicates (" + STRCOMPRESS(totDupes,/REMOVE_ALL) + ", in fact)!  Print them? (y/n)" $
     ELSE BEGIN
        PRINT,"printDupes: " + printDupes
        answer=printDupes
     ENDELSE

     IF STRLOWCASE(STRMID(answer,0,1)) NE 'y' THEN BEGIN
        PRINT,"OK, exiting..."
        RETURN
     ENDIF
  ENDIF

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

  print,"Total number of dupes: " + STRCOMPRESS(totDupes)

END