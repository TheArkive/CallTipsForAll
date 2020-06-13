  File := A_ScriptFullPath
  SearchRE = i)\Qparse\E
  DocComment = ???
  FileStructure := ScanFiles(File, SearchRE, DocComment)
  ot(FileStructure)
Return

#Include AHK_V1_SyntaxTestParcour.ahk
#Include ParseAHK_V3_Parser.ahk
#Include Attach.ahk
#Include ObjTree.ahk

ScanFiles(File, SearchRE, DocComment){
  ;read file
  FileRead, FileContent, %File%
  Result := ParseAHK(FileContent, SearchRE, DocComment)
  SplitPath, File, OutFileName, OutDir, OutExtension, OutNameNoExt, OutDrive
  CurrentOutDir := OutDir ? OutDir : CurrentOutDir

  ;recurse into Includes
  IncludeDir := CurrentOutDir
  FileStructure := {"File":OutFileName, "Result":Result, "Path": CurrentOutDir, "IncludeFiles":[]}                                                                        
  For k,v in Result.Includes {
    v := ReplaceVars(v, OutFileName, CurrentOutDir)
    If InStr(FileExist(CurrentOutDir "\" v),"D")
      IncludeDir := CurrentOutDir "\" v
    Else If InStr(FileExist(v),"D")
      IncludeDir := v
    Else {
      If FileExist(v) 
        FileStructure.IncludeFiles.Push(ScanFiles(v, SearchRE, DocComment))
      Else If FileExist(IncludeDir "\" v) 
        FileStructure.IncludeFiles.Push(ScanFiles(IncludeDir "\" v, SearchRE, DocComment))
    }
  } 
  Return FileStructure                    
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

_D(Title := "Debug", Vars*){      ;small helper to Debug
  local Var,Value,Text := ""
  For Var,Value in Vars           ;call it like this
    Text .= Var ": " Value "`n"   ;>>> _D("end of line", {i:i, Line:Line, FuncBlockLevel:FuncBlockLevel}*)
  MsgBox, 0, %Title%, %Text%
}