;2015/08/13 Now we can check for duplicates in the data!
PRO CHECK_DUPES,data,rev_ind,dupes_rev_ind,dataenum

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
     RETURN
  ENDIF

  totDupes = TOTAL(h[dupes])
  IF totDupes GT 500 THEN BEGIN 
     READ,answer, PROMPT="More than 500 duplicates! Print them? (y/n)"
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

  rev_ind = r
  dupes_rev_ind = dupes

  print,"Total number of dupes: " + STRCOMPRESS(totDupes)

END