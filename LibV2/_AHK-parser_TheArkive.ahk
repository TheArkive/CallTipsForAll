; ahk parser by TheArkive

ahk_parser_launcher(baseFile:="") {
	regex := Map()
	srcFiles := oCallTip.srcFiles
	regexFile := srcFiles "\Other\List_Regex.txt"
	regexList := FileRead(regexFile)
	
	regArr := StrSplit(regexList,"`n","`r")
	for i, line in regArr
		regex[SubStr(line,1,InStr(line,":")-1)] := Trim(SubStr(line,InStr(line,":")+1))
	
	If (!regex.Count) {
		MsgBox "Regex list not found."
		return
	}
	
	oCallTip.regex := regex
	
	If (baseFile)
		IncludesList := [], GetIncludes([baseFile]), GetLibs()
	
	; msgbox Jxon_dump(IncludesList,4)
	
	ahk_parser_thearkive()
}

GetIncludes(inputIncludes) { ; input should start with [baseFile]
	newIncludes := []
	Loop inputIncludes.Length {
		curFile := inputIncludes[A_Index]
		
		If (IncludeCheckDupe(curFile))
			continue
		Else
			IncludesList.Push(curFile)
		
		curDocText := FileRead(curFile)
		curDocArr := StrSplit(curDocText,"`n","`r")
		
		SplitPath curFile, , baseFolderP
		curBaseFolder := baseFolderP
		includesSearch := "^(#Include(Again)?[ \t]+.*)"
		
		Loop curDocArr.Length {
			curLine := curDocArr[A_Index]
			
			If (!r1 := RegExMatch(curLine,"mi)" includesSearch,match))
				continue
			
			curInc := Trim(RegExReplace(match.Value(1),"i)(^#Include(Again)?|\*i|" Chr(34) ")",""))
			curInclude := RegExReplace(curInc,"<|>","")
			
			If (curInc != curInclude) ; include is library file and will automatically be scanned
				continue
			
			curInclude := StrReplace(curInclude,"%A_ScriptDir%",curBaseFolder) ;ZZZ - need to replace a larger set of vars here
			finalFile := (FileExist(curBaseFolder "\" curInclude)) ? curBaseFolder "\" curInclude : FileExist(curInclude) ? curInclude : ""
			incType := FileExist(finalFile)
			
			If (InStr(incType,"D"))
				curBaseFilder := finalFile ; change baseFolder if dir
			Else If (incType)
				newIncludes.Push(finalFile) ; add to new includes
		}
	}
	
	If (newIncludes.Length)
		GetIncludes(newIncludes)	
}

IncludeCheckDupe(inFile) {
	dupe := false
	For i, curInc in IncludesList { ; check for duplicate includes
		If (inFile = curInc) {
			dupe := true
			Break
		}
	}
	
	return dupe
}

GetLibs() {
	If (IncludesList.Length) {
		baseFile := IncludesList[1]
		SplitPath baseFile,, baseFolder
		
		f1 := baseFolder "\Lib\*" ;??? - simplified handling of 3 lib locations   ;ZZZ - yes finally simplified!
		Loop Files f1
		{
			If (!IncludeCheckDupe(A_LoopFileFullPath))
				IncludesList.Push(A_LoopFileFullPath)
		}
		
		f2 := A_MyDocuments "\AutoHotkey\Lib\*"
		Loop Files f2
		{
			If (!IncludeCheckDupe(A_LoopFileFullPath))
				IncludesList.Push(A_LoopFileFullPath)
		}
		
		f3 := ""
		Try f3 := RegRead("HKEY_LOCAL_MACHINE\SOFTWARE\AutoHotkey","InstallDir") "\Lib\*"
		If (f3) {
			Loop Files f3
			{
				If (!IncludeCheckDupe(A_LoopFileFullPath))
					IncludesList.Push(A_LoopFileFullPath)
			}
		}
	}
}

; ReplaceVars(v, OutFileName, CurrentOutDir){ ; made by toralf
  ; AHKVars := {"%A_AhkPath%":         A_AhkPath
            ; , "%A_AhkVersion%":      A_AhkVersion
            ; , "%A_ComputerName%":    A_ComputerName
            ; , "%A_ComSpec%":         A_ComSpec
            ; , "%A_Desktop%":         A_Desktop
            ; , "%A_DesktopCommon%":   A_DesktopCommon
            ; , "%A_IsCompiled%":      A_IsCompiled
            ; , "%A_IsUnicode%":       A_IsUnicode
            ; , "%A_MyDocuments%":     A_MyDocuments
            ; , "%A_ProgramFiles%":    A_ProgramFiles
            ; , "%A_Programs%":        A_Programs
            ; , "%A_ProgramsCommon%":  A_ProgramsCommon
            ; , "%A_PtrSize%":         A_PtrSize
            ; , "%A_ScriptFullPath%":    CurrentOutDir "\" OutFileName
            ; , "%A_LineFile%":          CurrentOutDir "\" OutFileName
            ; , "%A_ScriptDir%":         CurrentOutDir
            ; , "%A_ScriptName%":        OutFileName
            ; , "%A_Space%":           A_Space
            ; , "%A_StartMenu%":       A_StartMenu
            ; , "%A_StartMenuCommon%": A_StartMenuCommon
            ; , "%A_Startup%":         A_Startup
            ; , "%A_StartupCommon%":   A_StartupCommon
            ; , "%A_Tab%":             A_Tab
            ; , "%A_Temp%":            A_Temp
            ; , "%A_UserName%":        A_UserName
            ; , "%A_WinDir%":          A_WinDir }

  ; For var,value in AHKVars
    ; v := StrReplace(v, var, value) 
  ; Return v  
; }

ahk_parser_thearkive() { ; () = \x28 \x29 ... [] = \x5B \x5D ... {} = \x7B \x7D
	q := Chr(34)
	
	LineComment := "^[ \t]*\;"
	BlockCommentStart := "^[ \t]*/\*"
	BlockComment := "^[ \t]*/\*.*\*/"
	
	FunctionStart := "^[ \t]*([\w]+)\x28"
	FunctionOpener := "^[ \t]*([\w]+)(\x28[^\x29]*\x29)([ \t\r\n]*\{)?"
	FunctionBody := ""
	FunctionReturn := ""
	
	ClassStart := ""
	ClassEnd := ""
	MethodStart := ""
	MethodEnd := ""
	MethodOneLine := ""
	PropertyStart := ""
	PropertyEnd := ""
	PropertyOneLine := ""
	
	ClassInstance := ""
	
	For i, includeFile in IncludesList {
		curDocText := FileRead(includeFile)
		docArr := StrSplit(curDocText,"`n","`r")
		
		i := 1
		Loop docArr.Length {
			curLine := docArr[i], curChunk := curLine
			
			If (RegExMatch(curChunk,LineComment)) { ; comment so skip
				i++
				Continue
			}
			
			If (!enclosureCheck(curChunk)) { ; no enclosures, not a function/class
				i++
				Continue
			}
			
			While (!enclosureComplete(curChunk))
				curChunk .= "`r`n" docArr[++i]
			
			
			
			i++
		}
	}
}

enclosureCheck(sInput) { ; checks for even numbers of enclosures () [] {}, return 0 means even
	noStr := StringOutline(sInput)
	
	noStr := StrReplace(StrReplace(noStr,"(","(",LP),")",")",RP)
	; noStr := StrReplace(StrReplace(noStr,"[","[",LB),"]","]",RB)
	noStr := StrReplace(StrReplace(noStr,"{","{",LC),"}","}",RC)
	
	r1 := (LP + RP + LC + RC) ? true : false
	If (!r1)
		return false
	Else
		return true
}

enclosureComplete(sInput) { ; checks for even numbers of enclosures () [] {}, return 0 means even
	noStr := StringOutline(sInput)
	
	noStr := StrReplace(StrReplace(noStr,"(","(",LP),")",")",RP)
	; noStr := StrReplace(StrReplace(noStr,"[","[",LB),"]","]",RB)
	noStr := StrReplace(StrReplace(noStr,"{","{",LC),"}","}",RC)
	
	p := Abs(LP - RP), c := Abs(LC - RC) ; b := Abs(LB - RB)
	r2 := p + c
	
	If (!r2)
		return true
	Else
		return false
}

GetCustomFunctions(curDocText) { ;ZZZ - this should work better
	curDocTextNoStr := StringOutline(curDocText)
	; funcList := Map(), funcList.CaseSense := 0
	curPos1 := 1
	
	; While (result := RegExMatch(curDocTextNoStr,"mi)" oCallTip.funcStart,match,curPos1)) {
	curRegex := oCallTip.regex["FunctionStart"] ".*" oCallTip.regex["FunctionEnd"]
	result := RegExMatch(curDocTextNoStr,"mi)" ,match)
	
	funcName := "", bodyText := "", obj := Map()
	If (IsObject(match) And match.Count()) {
		curPos1 := match.Pos(0)
		funcName := match.Value(1)
		If (funcName = "" Or funcName = "If" Or funcName = "While" Or funcName = "For" Or funcName = "Else")
			return ""
		
		; curPos2 := curPos1
		; r2 := RegExMatch(curDocTextNoStr,"mi)" oCallTip.funcEnd,match2,curPos2))
		
		; bodyText := SubStr(curDocTextNoStr,curPos1,match2.Pos(0)+match2.Len(0)-curPos1)
		; curPos2 := match2.Pos(0) + match2.Len(0)
		w := StrReplace(StrReplace(bodyText,"{","{",lB),"}","}",rB)
		
		If (lB = rB) {
			; index := GetLineNum(curPos1)
			; bodyText := SubStr(curDocText,curPos1,match2.Pos(0)+match2.Len(0)-curPos1)
			; params := GetCustFuncParams(bodyText)
			; obj := Map("type","CustomFunction","desc",funcName params,"funcBody",bodyText,"index",index,"params",params)
			; curPos1 := curPos2
			; Break
		}
		
		
		returnObj := Map(), curPos3 := 1
		While (r3 := RegExMatch(bodyText,"mi)" oCallTip.funcReturn,match3,curPos3)) {
			If (match3.Count()) {
				cMatch := match3.Value(1)
				returnObj[cMatch] := match3.Pos(1)
			}
			curPos3 := match3.Pos(1) + match3.Len(1)
		}
		
		If (returnObj.Count > 0)
			obj["return"] := returnObj ; attach returnObj list of "return" lines if exist
		
		If (!funcList.Has(funcName))
			funcList[funcName] := obj
	}
	
	; }
	
	return obj
}