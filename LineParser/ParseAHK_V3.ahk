  File := A_ScriptFullPath
  SearchRE = i)\Qparse\E
  DocComment = ???
  ; StartTC := A_TickCount
  FileStructure := ScanFiles(File, SearchRE, DocComment)
  ; EndTC := A_TickCount
  ot(FileStructure)

  ; MsgBox % EndTC - StartTC

  ; ListVars   ;for this to show correctly the vars includes for TestScript and SyntaxTestParcour  have to be commented out

  #Include TestScript.ahk
Return

#Include AHK_V1_SyntaxTestParcour.ahk
#Include ParseAHK_V3_Parser.ahk
#Include ParseAHK_V3_LineInfo.ahk
#Include Attach.ahk
#Include ObjTree.ahk

/*
Potential Enhancements to capture all potential information for call tips or auto-completion
- scan also all library files in the 3 lib locations
- scan also active document when not already covered by the files scanned
*/

ScanFiles(File, SearchRE, DocComment, WithInStack := ""){
  FileRead, FileContent, %File%
  LineInfo.File(File, WithInStack)
  Result := ParseAHK(FileContent, SearchRE, DocComment)
  Result[File, "GuessDocComment"] := LineInfo.GuessDocComment()
  LineInfo.DeleteAll()
  SplitPath, File, OutFileName, CurrentOutDir, OutExtension, OutNameNoExt, OutDrive

  ;recurse into Includes
  IncludeDir := CurrentOutDir
  For line, v in Result[File, "Include"] {
    v := ReplaceVars(v, OutFileName, CurrentOutDir)
    If InStr(FileExist(CurrentOutDir "\" v),"D")
      IncludeDir := CurrentOutDir "\" v
    Else If InStr(FileExist(v),"D")
      IncludeDir := v
    Else {
      If FileExist(v) 
        Result[File, "Include", line] := ScanFiles(CurrentOutDir "\" v, SearchRE, DocComment, [])
      Else If FileExist(IncludeDir "\" v) 
        Result[File, "Include", line] := ScanFiles(IncludeDir "\" v, SearchRE, DocComment, [])
    }
  } 
  Return Result                    

}

ReplaceVars(v, OutFileName, CurrentOutDir){
  AHKVars := {"%A_AhkPath%":         A_AhkPath
            , "%A_AhkVersion%":      A_AhkVersion
            , "%A_ComputerName%":    A_ComputerName
            , "%A_ComSpec%":         A_ComSpec
            , "%A_Desktop%":         A_Desktop
            , "%A_DesktopCommon%":   A_DesktopCommon
            , "%A_IsCompiled%":      A_IsCompiled
            , "%A_IsUnicode%":       A_IsUnicode
            , "%A_MyDocuments%":     A_MyDocuments
            , "%A_ProgramFiles%":    A_ProgramFiles
            , "%A_Programs%":        A_Programs
            , "%A_ProgramsCommon%":  A_ProgramsCommon
            , "%A_PtrSize%":         A_PtrSize
            , "%A_ScriptFullPath%":    CurrentOutDir "\" OutFileName
            , "%A_LineFile%":          CurrentOutDir "\" OutFileName
            , "%A_ScriptDir%":         CurrentOutDir
            , "%A_ScriptName%":        OutFileName
            , "%A_Space%":           A_Space
            , "%A_StartMenu%":       A_StartMenu
            , "%A_StartMenuCommon%": A_StartMenuCommon
            , "%A_Startup%":         A_Startup
            , "%A_StartupCommon%":   A_StartupCommon
            , "%A_Tab%":             A_Tab
            , "%A_Temp%":            A_Temp
            , "%A_UserName%":        A_UserName
            , "%A_WinDir%":          A_WinDir }

  For var,value in AHKVars
    v := StrReplace(v, var, value) 
  Return v  
}
