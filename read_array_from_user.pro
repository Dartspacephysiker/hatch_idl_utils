;2016/02/17 Read in an array from the command line (up to 12 variables)
PRO READ_ARRAY_FROM_USER,array, $
                         N_TO_READ=n_to_read, $
                         STRING=string, $
                         LONG=long, $
                         LONG64=long64, $
                         BYTE=byte, $
                         FLOAT=float, $
                         DOUBLE=double, $
                         PROMPT=prompt, $
                         NPROMPT=nPrompt, $
                         QUIET=quiet

  CASE 1 OF
     KEYWORD_SET(string): BEGIN
        typeStr              = 'STRING'
        var1                 = ''
        var2                 = ''
        var3                 = ''
        var4                 = ''
        var5                 = ''
        var6                 = ''
        var7                 = ''
        var8                 = ''
        var9                 = ''
        var10                = ''
        var11                = ''
        var12                = ''
     END
     KEYWORD_SET(long): BEGIN
        typeStr              = 'LONG'
        var1                 = 0L
        var2                 = 0L
        var3                 = 0L
        var4                 = 0L
        var5                 = 0L
        var6                 = 0L
        var7                 = 0L
        var8                 = 0L
        var9                 = 0L
        var10                = 0L
        var11                = 0L
        var12                = 0L
     END
     KEYWORD_SET(long64): BEGIN
        typeStr              = 'LONG64'
        var1                 = 0LL
        var2                 = 0LL
        var3                 = 0LL
        var4                 = 0LL
        var5                 = 0LL
        var6                 = 0LL
        var7                 = 0LL
        var8                 = 0LL
        var9                 = 0LL
        var10                = 0LL
        var11                = 0LL
        var12                = 0LL
     END
     KEYWORD_SET(double): BEGIN
        typeStr              = 'DOUBLE'
        var1                 = 0.0D
        var2                 = 0.0D
        var3                 = 0.0D
        var4                 = 0.0D
        var5                 = 0.0D
        var6                 = 0.0D
        var7                 = 0.0D
        var8                 = 0.0D
        var9                 = 0.0D
        var10                = 0.0D
        var11                = 0.0D
        var12                = 0.0D
     END
     KEYWORD_SET(float): BEGIN
        typeStr              = 'FLOAT'
        var1                 = 0.0
        var2                 = 0.0
        var3                 = 0.0
        var4                 = 0.0
        var5                 = 0.0
        var6                 = 0.0
        var7                 = 0.0
        var8                 = 0.0
        var9                 = 0.0
        var10                = 0.0
        var11                = 0.0
        var12                = 0.0
     END
     KEYWORD_SET(byte): BEGIN
        typeStr              = 'BYTE'
        var1                 = 0B
        var2                 = 0B
        var3                 = 0B
        var4                 = 0B
        var5                 = 0B
        var6                 = 0B
        var7                 = 0B
        var8                 = 0B
        var9                 = 0B
        var10                = 0B
        var11                = 0B
        var12                = 0B
     END
     ELSE: BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,'Assuming you will give me longs to read in...'
        typeStr              = 'LONG'
        var1                 = 0B
        var2                 = 0B
        var3                 = 0B
        var4                 = 0B
        var5                 = 0B
        var6                 = 0B
        var7                 = 0B
        var8                 = 0B
        var9                 = 0B
        var10                = 0B
        var11                = 0B
        var12                = 0B
     END
  ENDCASE

  IF ~KEYWORD_SET(nPrompt) THEN BEGIN
     nPrompt = 'How many vars to read?  (Dumb question, yes, but I am not a very bright programmer.)'
  ENDIF

  IF ~KEYWORD_SET(prompt) THEN BEGIN
     prompt  = 'Enter ' + typeStr + ' vars to read, separated by a space (or press enter after each one)'
  ENDIF

  
  READ,n_to_read,PROMPT=nPrompt

  ;; READ,var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,PROMPT=prompt

  CASE n_to_read OF 
     1: BEGIN
        READ,var1,PROMPT=prompt
        array = var1
     END
     2: BEGIN
        READ,var1,var2,PROMPT=prompt
        array = [var1,var2]
     END
     3: BEGIN
        READ,var1,var2,var3,PROMPT=prompt
        array = [var1,var2,var3]
     END
     4: BEGIN
        READ,var1,var2,var3,var4,PROMPT=prompt
        array = [var1,var2,var3,var4]
     END
     5: BEGIN
        READ,var1,var2,var3,var4,var5,PROMPT=prompt
        array = [var1,var2,var3,var4,var5]
     END
     6: BEGIN
        READ,var1,var2,var3,var4,var5,var6,PROMPT=prompt
        array = [var1,var2,var3,var4,var5,var6]
     END
     7: BEGIN
        READ,var1,var2,var3,var4,var5,var6,var7,PROMPT=prompt
        array = [var1,var2,var3,var4,var5,var6,var7]
     END
     8: BEGIN
        READ,var1,var2,var3,var4,var5,var6,var7,var8,PROMPT=prompt
        array = [var1,var2,var3,var4,var5,var6,var7,var8]
     END
     9: BEGIN
        READ,var1,var2,var3,var4,var5,var6,var7,var8,var9,PROMPT=prompt
        array = [var1,var2,var3,var4,var5,var6,var7,var8,var9]
     END
     10: BEGIN
        READ,var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,PROMPT=prompt
        array = [var1,var2,var3,var4,var5,var6,var7,var8,var9,var10]
     END
     11: BEGIN
        READ,var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,PROMPT=prompt
        array = [var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11]
     END
     12: BEGIN
        READ,var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,PROMPT=prompt
        array = [var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12]
     END
     ELSE: BEGIN
        PRINT,'Can only read in up to 12 vars! Anything beyond that just got junked...'
     END
  ENDCASE
  
END