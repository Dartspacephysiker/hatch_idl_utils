;;10/10/16
PRO CONVERT_VARNAMES_AND_RESAVE_OUTFILES,outFile, $
                                         ;; AACGMSPHNAME=AACGMSphName, $
                                         ;; NAME__AACGMSTRUCT=AACGMStructName, $
                                         var0Name,defVar0Name, $
                                         var1Name,defVar1Name, $
                                         var2Name,defVar2Name, $
                                         var3Name,defVar3Name, $
                                         var4Name,defVar4Name, $
                                         var5Name,defVar5Name, $
                                         var6Name,defVar6Name, $
                                         var7Name,defVar7Name, $
                                         var8Name,defVar8Name, $
                                         var9Name,defVar9Name, $
                                         var10Name,defVar10Name, $
                                         var11Name,defVar11Name, $
                                         var12Name,defVar12Name, $
                                         var13Name,defVar13Name, $
                                         var14Name,defVar14Name, $
                                         var15Name,defVar15Name, $
                                         var16Name,defVar16Name, $
                                         var17Name,defVar17Name, $
                                         var18Name,defVar18Name, $
                                         var19Name,defVar19Name, $
                                         var20Name,defVar20Name, $
                                         var21Name,defVar21Name, $
                                         var22Name,defVar22Name, $
                                         var23Name,defVar23Name, $
                                         var24Name,defVar24Name


  COMPILE_OPT idl2

  varNames    = !NULL
  defVarNames = !NULL

  IF ARG_PRESENT(var0Name) THEN BEGIN
     var0NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var0Name]
     defVarNames     = [defVarNames,defVar0Name]
  ENDIF
  IF ARG_PRESENT(var1Name) THEN BEGIN
     var1NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var1Name]
     defVarNames     = [defVarNames,defVar1Name]
  ENDIF
  IF ARG_PRESENT(var2Name) THEN BEGIN
     var2NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var2Name]
     defVarNames     = [defVarNames,defVar2Name]
  ENDIF
  IF ARG_PRESENT(var3Name) THEN BEGIN
     var3NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var3Name]
     defVarNames     = [defVarNames,defVar3Name]
  ENDIF
  IF ARG_PRESENT(var4Name) THEN BEGIN
     var4NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var4Name]
     defVarNames     = [defVarNames,defVar4Name]
  ENDIF
  IF ARG_PRESENT(var5Name) THEN BEGIN
     var5NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var5Name]
     defVarNames     = [defVarNames,defVar5Name]
  ENDIF
  IF ARG_PRESENT(var6Name) THEN BEGIN
     var6NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var6Name]
     defVarNames     = [defVarNames,defVar6Name]
  ENDIF
  IF ARG_PRESENT(var7Name) THEN BEGIN
     var7NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var7Name]
     defVarNames     = [defVarNames,defVar7Name]
  ENDIF
  IF ARG_PRESENT(var8Name) THEN BEGIN
     var8NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var8Name]
     defVarNames     = [defVarNames,defVar8Name]
  ENDIF
  IF ARG_PRESENT(var9Name) THEN BEGIN
     var9NameIndex   = N_ELEMENTS(varNames)
     varNames        = [varNames,var9Name]
     defVarNames     = [defVarNames,defVar9Name]
  ENDIF
  IF ARG_PRESENT(var10Name) THEN BEGIN
     var10NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var10Name]
     defVarNames     = [defVarNames,defVar10Name]
  ENDIF
  IF ARG_PRESENT(var11Name) THEN BEGIN
     var11NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var11Name]
     defVarNames     = [defVarNames,defVar11Name]
  ENDIF
  IF ARG_PRESENT(var12Name) THEN BEGIN
     var12NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var12Name]
     defVarNames     = [defVarNames,defVar12Name]
  ENDIF
  IF ARG_PRESENT(var13Name) THEN BEGIN
     var13NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var13Name]
     defVarNames     = [defVarNames,defVar13Name]
  ENDIF
  IF ARG_PRESENT(var14Name) THEN BEGIN
     var14NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var14Name]
     defVarNames     = [defVarNames,defVar14Name]
  ENDIF
  IF ARG_PRESENT(var15Name) THEN BEGIN
     var15NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var15Name]
     defVarNames     = [defVarNames,defVar15Name]
  ENDIF
  IF ARG_PRESENT(var15Name) THEN BEGIN
     var15NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var15Name]
     defVarNames     = [defVarNames,defVar15Name]
  ENDIF
  IF ARG_PRESENT(var16Name) THEN BEGIN
     var16NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var16Name]
     defVarNames     = [defVarNames,defVar16Name]
  ENDIF
  IF ARG_PRESENT(var17Name) THEN BEGIN
     var17NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var17Name]
     defVarNames     = [defVarNames,defVar17Name]
  ENDIF
  IF ARG_PRESENT(var18Name) THEN BEGIN
     var18NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var18Name]
     defVarNames     = [defVarNames,defVar18Name]
  ENDIF
  IF ARG_PRESENT(var19Name) THEN BEGIN
     var19NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var19Name]
     defVarNames     = [defVarNames,defVar19Name]
  ENDIF
  IF ARG_PRESENT(var20Name) THEN BEGIN
     var20NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var20Name]
     defVarNames     = [defVarNames,defVar20Name]
  ENDIF
  IF ARG_PRESENT(var21Name) THEN BEGIN
     var21NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var21Name]
     defVarNames     = [defVarNames,defVar21Name]
  ENDIF
  IF ARG_PRESENT(var22Name) THEN BEGIN
     var22NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var22Name]
     defVarNames     = [defVarNames,defVar22Name]
  ENDIF
  IF ARG_PRESENT(var23Name) THEN BEGIN
     var23NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var23Name]
     defVarNames     = [defVarNames,defVar23Name]
  ENDIF
  IF ARG_PRESENT(var24Name) THEN BEGIN
     var24NameIndex  = N_ELEMENTS(varNames)
     varNames        = [varNames,var24Name]
     defVarNames     = [defVarNames,defVar24Name]
  ENDIF

  ;;Check if they're defined
  callString = 'test2 = VARS_EXIST(outFile,varNames,defVarNames,'
  FOR j=0,N_ELEMENTS(varNames)-1 DO BEGIN
     callString += defVarNames[j] + ','
  ENDFOR
  callString += 'VARNAMES_REQUIRED_UPDATE=varNames_required_update,VARNAMES_WERE_NOT_DEFNAMES=varNames_were_not_defNames,/UPDATE_VARNAMES)'

  test = EXECUTE(callString)
  IF ~test THEN BEGIN
     PRINT,'WHATLSJDLFJK'
     STOP
  ENDIF
  IF ~test2 THEN BEGIN
    RETURN
  ENDIF
  ;; IF ~VARS_EXIST(outFile,varNames,defVarnames, $
  ;;                ;; GEOSph,AACGMSph,GEOStruct,AACGMStruct, $
  ;;                AACGMSph,AACGMStruct,restrict_ii,eSpec_i, $ ;notRestrict_ii,
  ;;                VARNAMES_REQUIRED_UPDATE=varNames_required_update, $
  ;;                VARNAMES_WERE_NOT_DEFNAMES=varNames_were_not_defNames, $
  ;;                /UPDATE_VARNAMES) THEN BEGIN
  ;;    RETURN
  ;; ENDIF
  
  ;;Update varNames
  IF ARG_PRESENT(var0Name) THEN BEGIN
     var0Name = varNames[var0NameIndex]
  ENDIF
  IF ARG_PRESENT(var1Name) THEN BEGIN
     var1Name = varNames[var1NameIndex]
  ENDIF
  IF ARG_PRESENT(var2Name) THEN BEGIN
     var2Name = varNames[var2NameIndex]
  ENDIF
  IF ARG_PRESENT(var3Name) THEN BEGIN
     var3Name = varNames[var3NameIndex]
  ENDIF
  IF ARG_PRESENT(var4Name) THEN BEGIN
     var4Name = varNames[var4NameIndex]
  ENDIF
  IF ARG_PRESENT(var5Name) THEN BEGIN
     var5Name = varNames[var5NameIndex]
  ENDIF
  IF ARG_PRESENT(var6Name) THEN BEGIN
     var6Name = varNames[var6NameIndex]
  ENDIF
  IF ARG_PRESENT(var7Name) THEN BEGIN
     var7Name = varNames[var7NameIndex]
  ENDIF
  IF ARG_PRESENT(var8Name) THEN BEGIN
     var8Name = varNames[var8NameIndex]
  ENDIF
  IF ARG_PRESENT(var9Name) THEN BEGIN
     var9Name = varNames[var9NameIndex]
  ENDIF
  IF ARG_PRESENT(var10Name) THEN BEGIN
     var10Name = varNames[var10NameIndex]
  ENDIF
  IF ARG_PRESENT(var11Name) THEN BEGIN
     var11Name = varNames[var11NameIndex]
  ENDIF
  IF ARG_PRESENT(var12Name) THEN BEGIN
     var12Name = varNames[var12NameIndex]
  ENDIF
  IF ARG_PRESENT(var13Name) THEN BEGIN
     var13Name = varNames[var13NameIndex]
  ENDIF
  IF ARG_PRESENT(var14Name) THEN BEGIN
     var14Name = varNames[var14NameIndex]
  ENDIF
  IF ARG_PRESENT(var15Name) THEN BEGIN
     var15Name = varNames[var15NameIndex]
  ENDIF
  IF ARG_PRESENT(var15Name) THEN BEGIN
     var15Name = varNames[var15NameIndex]
  ENDIF
  IF ARG_PRESENT(var16Name) THEN BEGIN
     var16Name = varNames[var16NameIndex]
  ENDIF
  IF ARG_PRESENT(var17Name) THEN BEGIN
     var17Name = varNames[var17NameIndex]
  ENDIF
  IF ARG_PRESENT(var18Name) THEN BEGIN
     var18Name = varNames[var18NameIndex]
  ENDIF
  IF ARG_PRESENT(var19Name) THEN BEGIN
     var19Name = varNames[var19NameIndex]
  ENDIF
  IF ARG_PRESENT(var20Name) THEN BEGIN
     var20Name = varNames[var20NameIndex]
  ENDIF
  IF ARG_PRESENT(var21Name) THEN BEGIN
     var21Name = varNames[var21NameIndex]
  ENDIF
  IF ARG_PRESENT(var22Name) THEN BEGIN
     var22Name = varNames[var22NameIndex]
  ENDIF
  IF ARG_PRESENT(var23Name) THEN BEGIN
     var23Name = varNames[var23NameIndex]
  ENDIF
  IF ARG_PRESENT(var24Name) THEN BEGIN
     var24Name = varNames[var24NameIndex]
  ENDIF

  ;;Yes, the French 'reponse' (not response)
  IF varNames_were_not_defNames THEN BEGIN

     reponse = ''
     cont    = 0
     PRINT,'About to save renamed vars to ' + outFile + '. Is this OK?' 
     READ,reponse
     WHILE ~cont DO BEGIN
        CASE 1 OF
           STRMID(STRUPCASE(reponse),0,1) EQ 'Y': BEGIN
              saveString = 'SAVE,'
              FOR j=0,N_ELEMENTS(varNames)-1 DO BEGIN
                 saveString += varNames[j] + ','
              ENDFOR
              saveString += 'FILENAME="'+outFile+'"'
              ;; saveString = 'SAVE,'+$
              ;;              ;; defGeoSphName+','+$
              ;;              defVar0Name+','+$
              ;;              ;; defGEOStructName+','+$
              ;;              defVar1Name+','+$
              ;;              defVar3+','+$
              ;;              defVar4+','+$
              ;; 'FILENAME="'+outFile+'"'
              PRINT,saveString
              IF ~EXECUTE(saveString) THEN STOP
              cont = 1
           END
           STRMID(STRUPCASE(reponse),0,1) EQ 'N': BEGIN
              PRINT,"K, whatever ..."
              cont = 1 
           END
           ELSE: BEGIN
              PRINT,"I SAID YES OR NO!!!"
              READ,reponse
              cont = 0
           END
        ENDCASE
     ENDWHILE

  ENDIF ELSE BEGIN
     PRINT,"Variables already saved with defNames!"
  ENDELSE

END

