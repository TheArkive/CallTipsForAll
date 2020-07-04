; ahk parser by TheArkive

ahk_parser_launcher(baseFile:="") {
	curTicks := A_TickCount
	
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
	
	finalTick := A_TickCount - curTicks
	mins := Integer(Integer(finalTick / 1000) / 60)
	secs := finalTick/1000 - (mins * 60)
	
	MsgBox mins " mins " secs " secs"
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
	oCallTip.newParseClass := 1
	
	CustomFunctions := Map(), CustomFunctions.CaseSense := 0
	ClassesList := Map(), ClassesList.CaseSense := 0
	ObjectList := Map(), ObjectList.CaseSense := 0
	VariablesList := Array()
	
	For i, includeFile in IncludesList {
		; debug.msg(includeFile)
		
		curDocText := FileRead(includeFile)
		docArr := StrSplit(curDocText,"`n","`r")
		
		i := 1, totLines := docArr.Length
		lastVarStatus := ""
		While (i <= totLines) {
			; Debug.Msg("i: " i "`r`n" includeFile)
			
			curType := ""
			iStart := 0 ; start of multi-line statement
			
			oCallTip.LPar := 0, oCallTip.RPar := 0 ; reset brace count
			oCallTip.LBrace := 0, oCallTip.RBrace := 0
			oCallTip.LCBrace := 0, oCallTip.RCBrace := 0
			
			multi := false
			curLine := docArr[i], curChunk := curLine "`r`n" (docArr.Has(i+1) ? docArr[i+1] : "")
			
			If (Trim(curLine," `t") = "") { ; blank line so skip
				i++
				Continue
			}
			
			If (RegExMatch(curLine,"^[ \t]*\;")) { ; comment so skip
				i++
				Continue
			}
			
			If (RegExMatch(curLine,"^[ \t]*Global"))
				lastVarStatus := "Global"
			
			; debug.msg(curLine)
			
			; varList := getVar(curLine,includeFile,i) ; collect vars outside functions / classes
			; For i, obj in varList
				; obj["elementName"] := "", VariablesList.Push(obj) ; vars NOT in a function / class / etc...
			
			If (RegExMatch(StringOutline(curChunk),"mi)class ([\w]+)( extends ([\w\.]+))?[ \t\r\n]*\{")) { ; - it's a class!
				encl := enclosureCheck(curChunk), curType := "class", curChunk := curLine, lastVarStatus := ""
			} Else If (RegExMatch(StringOutline(curChunk),"^[ \t]*([\w]+)\x28")) { ; ------------------------- it's a function!
				encl := enclosureCheck(curLine), curType := "function", curChunk := curLine, lastVarStatus := ""
			} Else { ; ------------------------------------------------------------------------ it's something else ...
				encl := enclosureCheck(curLine)
				curLineNoStr := Trim(StringOutline(curLine)," `t")
				
				If (RegExMatch(curLine,"^[ \t]*Global"))
					lastVarStatus := "Global"
				Else If (RegExMatch(curLine,"^[ \t]*Static"))
					lastVarStatus := "Static"
				
				; Debug.Msg(curLine)
				
				i++
				
				varList := getVar(curLine,includeFile,i,lastVarStatus) ; collect vars outside functions / classes
				If (curLineNoStr And !varList.Length)
					lastVarStatus := ""
				
				For i, obj in varList {
					KeywordList[obj["varName"]] := "UserVar"
					obj["elementName"] := "", VariablesList.Push(obj) ; vars NOT in a function / class / etc...
				}
				
				continue
				
				
				; If (!encl.exist) { ; no enclosures, not a function/class
					; i++
					
					; varList := getVar(curLine,includeFile,i,lastVarStatus) ; collect vars outside functions / classes
					; If (curLineNoStr And !varList.Length)
						; lastVarStatus := ""
					
					; For i, obj in varList {
						; KeywordList[obj["varName"]] := "UserVar"
						; obj["elementName"] := "", VariablesList.Push(obj) ; vars NOT in a function / class / etc...
					; }
					; Continue
				; }
				
				; If (encl.exist And encl.even) { ; single-line statement, probably a function call, or setting object
					; i++
					
					; varList := getVar(curLine,includeFile,i,lastVarStatus) ; collect vars outside functions / classes
					; If (curLineNoStr And !varList.Length)
						; lastVarStatus := ""
					
					; For i, obj in varList {
						; KeywordList[obj["varName"]] := "UserVar"
						; obj["elementName"] := "", VariablesList.Push(obj) ; vars NOT in a function / class / etc...
					; }
					
					; Continue
				; }
			}
			
			; msgbox "test1: " curChunk " / i: " i
			
			tempVarList := []
			While (encl.exist And !encl.even) { ; compiles a multi-line statment
				If (!iStart) {
					iStart := i ; set start line of multi-line statement
					; debug.msg("iStart: " iStart "`r`n" includeFile "`r`n" curChunk "`r`ncurType: " curType)
				}
				
				i++
				If (docArr.Has(i)) {
					nextLine := docArr[i]
					; debug.msg(nextLine)
					
					If (RegExMatch(curLine,"^[ \t]*Global"))
						lastVarStatus := "Global"
					Else If (RegExMatch(curLine,"^[ \t]*Static"))
						lastVarStatus := "Static"
					
					; debug.msg("getVar3")
					varList := getVar(nextLine,includeFile,i,lastVarStatus) ; collect vars
					If (curLineNoStr And !varList.Length)
						lastVarStatus := ""
					
					For i, obj in varList
						tempVarList.Push(obj)
					
					multi := true
					encl := enclosureCheck(nextLine)
					
					curChunk .= "`r`n" nextLine
				} Else
					Break
			}
			
			; Debug.Msg(curChunk "`r`n`r`n`r`n")
			
			If (curType = "class")
				c := GetClasses(curChunk,includeFile,i) ; not a big performance hit
			
			If (curType = "function") {
				f := GetCustomFunction(curChunk,includeFile,iStart) ; not a big performance hit
				If (f And !CustomFunctions.Has(f["funcName"])) {
					For i, obj in tempVarList {
						KeywordList[obj["varName"]] := "UserVar"
						obj["elementName"] := f["funcName"], VariablesList.Push(obj)
					}
					
					; Debug.Msg("getting function: " f["funcName"])
					
					CustomFunctions[f["funcName"]] := f ; add function to call tip data
				}
			}
			
			; If (InStr(includeFile, "module-fim-thumbs.ahk"))
				; debug.msg(curChunk)
			
			i++
		}
	}
	
	; debug.msg("done maybe")
	
	If (c) { ; save classes to global var
		For k, obj in c
			ClassesList[k] := obj
	}
}

enclosureCheck(sInput) { ; checks for even numbers of enclosures () [] {}, return total:0 means even
	noStr := StringOutline(sInput)
	
	p := 0, b := 0, c := 0
	noStr := StrReplace(StrReplace(noStr,"(","(",LP),")",")",RP)
	noStr := StrReplace(StrReplace(noStr,"[","[",LB),"]","]",RB)
	noStr := StrReplace(StrReplace(noStr,"{","{",LC),"}","}",RC)
	
	oCallTip.LPar += LP, oCallTip.RPar += RP
	oCallTip.LBrace += LB, oCallTip.RBrace += RB
	oCallTip.LCBrace += LC, oCallTip.RCBrace += RC
	
	
	p := Abs(oCallTip.LPar - oCallTip.RPar)
	b := Abs(oCallTip.LBrace - oCallTip.RBrace)
	c := Abs(oCallTip.LCBrace - oCallTip.RCBrace)
	
	r2 := p + b + c
	
	even := (r2) ? false : true
	pRes := (oCallTip.LPar or oCallTip.RPar) ? true : false ; parenthesis exist?
	bRes := (oCallTip.LBrace or oCallTip.RBrace) ? true : false ; brackets exist?
	cRes := (oCallTip.LCBrace or oCallTip.RCBrace) ? true : false ; curly braces exist?
	
	exist := (!pRes And !bRes And !cRes) ? false : true
	
	p := {exist:pRes, total:p} ; result = exist? true/false   total = if 0 then even
	b := {exist:bRes, total:b}
	c := {exist:cRes, total:c}
	
	return {p:p, b:b, c:c, even:even, exist:exist} ; if enclosures exist, are they even, and how many of each
}

GetCustomFunction(curDocText,fileName,lineNum) { ;ZZZ - this should work better
	noStr := StringOutline(curDocText), obj := ""
	curRegex := "^[ \t]*([\w]+)\x28"
	
	If (r1 := RegExMatch(noStr,"mi)" curRegex, match)) {
		curPos1 := match.Pos(0)
		fn := match.Value(1)
		If (fn = "" Or fn = "If" Or fn = "While" Or fn = "For" Or fn = "Else")
			return "" ; not a function
		
		curPos2 := curPos1
		While (r2 := InStr(noStr, ")",, curPos2)) {
			curPos2 := r2
			funcStart := SubStr(noStr,curPos1,curPos2-curPos1+1)
			w := StrReplace(StrReplace(funcStart,"(","(",LP),")",")",RP)
			
			If (LP = RP) {
				funcStart := SubStr(curDocText,curPos1,curPos2-curPos1+1)
				funcStart := RegExReplace(RegExReplace(funcStart,"\r|\n",""),"[ \t]+"," ")
				r3 := RegExMatch(funcStart,"(\x28.*\x29)",match3)
				params := match3.Value(0)
				Break ; fn, funcStart, params, fileName, lineNum
			}
		}
	} Else
		return "" ; not a function
	
	curPos4 := 1, retObj := Array()
	While (r4 := RegExMatch(curDocText, "[ \t]*return[ \t]+(.*)", match4, curPos4)) {
		curPos4 := match4.Pos(0) + match4.Len(0)
		retObj.Push(match4.Value(1))
	}
	
	obj := Map("type","CustomFunction","desc",fn params,"funcName",fn,"params",params,"file",fileName,"lineNum",lineNum,"return",retObj)
	
	return obj
}

CheckShowMode(strBody,helper:="") { ; checks class, methods, properties for "; show" or "; hide" comment
	showStyle := ""
	arr := StrSplit(strBody,"`n","`r"), firstLine := StringOutline(arr[1],false) ; blank out strings, but not comments
	r := RegExMatch(firstLine,"\;(.*)",match)
	
	If (IsObject(match)) {
		showStyle := Trim(match.Value(1)," `t;")
		showStyle := (InStr(showStyle,"hide") = 1) ? "hide" : (InStr(showStyle,"show") = 1) ? "show" : ""
	}
	
	return showStyle
}

PruneFirstLast(strBody) {
	testArr := StrSplit(strBody,"`n","`r"), output := ""
	Loop testArr.Length {
		If (A_Index > 1 And A_Index < testArr.Length)
			output .= testArr[A_Index] "`r`n"
	}
	return output
}

GetClasses(curDocText, fileName, lineNum, parent := "") {
	curDocTextNoStr := StringOutline(curDocText) ; pull this from oCallTip so it only has to be done once
	static classList := Map()
	
	If (oCallTip.newParseClass)
		classList := Map(), oCallTip.newParseClass := 0
	
	If (!classList.Count)
		classList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	curPos1 := 1 ;??? oCallTip.classStart is not robust, see comments on function needle   ;ZZZ - do you mainly mean that it doesn't include #@$ ?  I thought class parsing was easier to write (imho).  Can classes include #@$ ?
	
	; ClassStart: class ([\w]+)( extends ([\w\.]+))?[ \t\r\n]*\{
	; ClassEnd: (\})
	; MethodStart: ^[ \t]*(Static[ \t]+)?([\w\.]+)(\x28.*\x29)[ \t\r\n]*\{
	; MethodEnd: \}
	; MethodOneLine: ^[ \t]*(Static[ \t]+)?([\w\.]+)(\x28.*\x29)[ \t]*\=\>.+
	; PropertyStart: ^[ \t]*(Static[ \t]+)?([\w\.]+)(\[.*?\])?[ \t\r\n]*\{
	; PropertyEnd: \}
	; PropertyOneLine: ^[ \t]*(Static[ \t]+)?([\w\.]+)(\x5B[^\x5D]*\x5D)?[ \t]*\=\>.+
	; SmallPropExt: (([\w]+)[ \t]*\:\=)[^\;\r\n]*(\;.*)?
	; SmallPropInt: this\.([\w]+)[ \t]*\:\=[^\;\r\n]*(\;.*)?

	
	While (result := RegExMatch(curDocTextNoStr,"mi)class ([\w]+)( extends ([\w\.]+))?[ \t\r\n]*\{",match,curPos1)) { ; parse classes
		classBody := ""
		If (IsObject(match) And match.Count()) {
			innerClassBody := ""
			className := match.Value(1)
			
			If (match.Pos(1) < curPos1)
				break
			
			curPos1 := match.Pos(1)
			extends := match.Value(3)
			
			If (className = "")
				Continue
			
			curPos12 := curPos1 ;ZZZ - now closing brace can be caught even if not using proper indentation
			While (r12 := RegExMatch(curDocTextNoStr,"mi)\}",match12,curPos12)) {
				classBody := SubStr(curDocTextNoStr,curPos1,match12.Pos(0)+match12.Len(0)-curPos1)
				
				curPos12 := match12.Pos(0) + match12.Len(0)
				w := StrReplace(StrReplace(classBody,"{","{",lB),"}","}",rB)
				
				If (lB = rB) {
					classBody := SubStr(curDocText,curPos1,match12.Pos(0)+match12.Len(0)-curPos1)
					curPos1 := curPos12
					classShowStyle := CheckShowMode(classBody,"normal class - " className)
					Break
				}
			}
			
			obj := Map("type","Class","desc",className,"extends",extends,"parent",parent,"fileName",fileName,"lineNum",lineNum,"classBody",Trim(classBody," `t`r`n"))
			classBody := PruneFirstLast(classBody) ; prune first and last lines
			memberList := Map()
			classBodyNoStr := StringOutline(classBody)
			
			curPos14 := 1
			While (r14 := RegExMatch(classBodyNoStr,"mi)class ([\w]+)( extends ([\w\.]+))?[ \t\r\n]*\{",match14,curPos14)) { ; try to get sub classes
				curPos14 := match14.Pos(0)
				
				curPos13 := curPos14
				While (r13 := RegExMatch(classBodyNoStr,"mi)\}",match13,curPos13)) {
					innerClassBody := SubStr(classBodyNoStr,curPos14,match13.Pos(0)+match13.Len(0)-curPos14)
					curPos13 := match13.Pos(0) + match13.Len(0)
					w := StrReplace(StrReplace(innerClassBody,"{","{",lB),"}","}",rB)
					
					If (lB = rB) {
						innerClassBody := SubStr(classBody,curPos14,match13.Pos(0)+match13.Len(0)-curPos14)
						
						GetClasses(innerClassBody, fileName, lineNum, className)
						blankReplace := RegExReplace(innerClassBody,".*"," ")
						classBody := StrReplace(classBody,innerClassBody,blankReplace) ; blank out the sub class after parsing it
						classBodyNoStr := StringOutline(classBody)
						
						Break
					}
				}
			}
			
			curPos2 := 1
			While (r2 := RegExMatch(classBodyNoStr,"mi)^[ \t]*(Static[ \t]+)?([\w\.]+)(\x28.*\x29)[ \t\r\n]*\{",match2,curPos2)) { ; parse methods - multi-line
				isStatic := match2.Value(1) ? true : false
				memberName := match2.Value(2)
				memberParams := match2.Value(3)
				curPos2 := match2.Pos(0)
				
				curPos5 := curPos2
				While (r5 := RegExMatch(classBodyNoStr,"mi)\}",match5,curPos5)) {
					methBody := SubStr(classBodyNoStr,curPos2,match5.Pos(0)+match5.Len(0)-curPos2) ; this needs to be fixed, should be parsing the "NoStr" var
					curPos5 := match5.Pos(0) + match5.Len(0)
					w := StrReplace(StrReplace(methBody,"{","{",lB),"}","}",rB)
					
					If (lB = rB) {
						methBody := SubStr(classBody,curPos2,match5.Pos(0)+match5.Len(0)-curPos2)
						curPos2 := curPos5
						showStyle := CheckShowMode(methBody,"method - " memberName)
						
						If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide")) {
							memberParams := GetCustFuncParams(methBody)
							memberList[memberName] := Map("name",memberName,"params",memberParams,"type","method","static",isStatic,"body",methBody)
						}
						methBodyNoStr := StringOutline(methBody)
						
						; curPos9 := 1
						; While (r9 := RegExMatch(methBodyNoStr,"mi)" oCallTip.classSmallPropInt,match9,curPos9)) {
							; tinyProp := match9.Value(1)
							; showStyle := Trim(match9.Value(2)," `t;")
							; curPos9 := match9.Pos(1) + match9.Len(1)
							
							; If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
								; memberList[tinyProp] := Map("name",tinyProp,"params","","type","propertySmall","body",match9.Value(0),"static",false)
						; }
						
						methBody := ""
						break
					}
				}
			}
			
			curPos4 := 1
			While (r4 := RegExMatch(classBodyNoStr,"mi)^[ \t]*(Static[ \t]+)?([\w\.]+)(\[.*?\])?[ \t\r\n]*\{",match4,curPos4)) {
				; If (r15 := RegExMatch(match4.Value(0),"mi)" oCallTip.classStart))
					; continue
				
				isStatic := match4.Value(1) ? true : false
				memberName := match4.Value(2)
				memberParams := match4.Value(3)
				curPos4 := match4.Pos(0)
				
				curPos6 := curPos4
				While (r6 := RegExMatch(classBodyNoStr,"mi)\}",match6,curPos6)) {
					propBody := SubStr(classBodyNoStr,curPos4,match6.Pos(0)+match6.Len(0)-curPos4)
					curPos6 := match6.Pos(0)
					w := StrReplace(StrReplace(propBody,"{","{",lB),"}","}",rB)
					
					If (lB = rB) {
						propBody := SubStr(classBody,curPos4,match6.Pos(0)+match6.Len(0)-curPos4)
						curPos4 := curPos6
						showStyle := CheckShowMode(propBody,"property - " memberName)
						
						If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide")) {
							memberParams := GetPropParams(propBody)
							memberList[memberName] := Map("name",memberName,"params",memberParams,"type","property","static",isStatic,"body",propBody)
						}
						propBodyNoStr := StringOutline(propBody)
						
						; curPos10 := 1
						; While (r10 := RegExMatch(propBodyNoStr,"mi)" oCallTip.classSmallPropInt,match10,curPos10)) {
							; tinyProp := match10.Value(1)
							; showStyle := Trim(match10.Value(2)," `t;")
							; curPos10 := match10.Pos(1) + match10.Len(1)
							
							; If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
								; memberList[tinyProp] := Map("name",tinyProp,"params","","type","propertySmall","body",match10.Value(0),"static",false)
						; }
						
						propBody := ""
						break
					}
				}
			}
			
			curPos7 := 1
			While (r7 := RegExMatch(StringOutline(classBody,false),"im)^[ \t]*(Static[ \t]+)?([\w\.]+)(\x28.*\x29)[ \t]*\=\>.+",match7,curPos7)) {
				isStatic := match7.Value(1) ? true : false
				memberName := match7.Value(2)
				memberParams := match7.Value(3)
				showStyle := CheckShowMode(match7.Value(0))
				curPos7 := match7.Pos(0) + match7.Len(0)
				
				If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
					memberList[memberName] := Map("name",memberName,"params",memberParams,"type","method","static",isStatic,"body",Trim(match7.Value(0),"`r`n"))
			}
			
			curPos8 := 1
			While (r8 := RegExMatch(StringOutline(classBody,false),"mi)^[ \t]*(Static[ \t]+)?([\w\.]+)(\x5B[^\x5D]*\x5D)?[ \t]*\=\>.+",match8,curPos8)) {
				isStatic := match8.Value(1) ? true : false
				memberName := match8.Value(2)
				memberParams := match8.Value(3)
				showStyle := CheckShowMode(match8.Value(0))
				curPos8 := match8.Pos(0) + match8.Len(0)
				
				If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
					memberList[memberName] := Map("name",memberName,"params",memberParams,"type","property","static",isStatic,"body",Trim(match8.Value(0),"`r`n"))
			}
		
			classBodyRemain := classBody
			
			For memName, memObj in memberList {
				memBody := memObj["body"]
				classBodyRemain := StrReplace(classBodyRemain,memBody)
				memberList[memName].Delete("body")
			}
			
			Loop Parse classBodyRemain, "`n", "`r"
			{
				curLine := Trim(A_LoopField," `t")
				isStatic := (InStr(curLine,"Static ")) ? true : false
				
				curPos11 := 1
				While (r11 := RegExMatch(curLine,"(([\w]+)[ \t]*\:\=)[^\;\r\n]*(\;.*)?",match11,curPos11)) {
					memberName := match11.Value(2)
					showStyle := Trim(match11.Value(3)," `t;")
					
					If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
						memberList[memberName] := Map("name",memberName,"params","","type","property","static",isStatic)
					
					curPos11 := match11.Pos(1) + match11.Len(1)
				}
			}
			
			obj["members"] := memberList, obj.Delete("classBody")
			classList[className] := obj
		}
		
		funcBody := "", funcName := "", match := "", params := "", returnObj := "", className := ""
	}
	
	curDocText := "", obj := ""
	return classList
}

ScanClasses(curDocText) { ; scan doc for class instances
	cList := Map()
	curPos := 1 ;??? oCallTip.classInstance is most likely not robust enough   ;ZZZ - do you mean not accounting for #$@ in class names?
	While (RegExMatch(curDocText,"i)([\w\.]+)[ \t]*\:\=[ \t]*([\w\.]+?)\.New\x28",match,curPos)) {
		instName := match.Value(1)
		className := match.Value(2)
		curPos := match.Pos(0) + match.Len(0)
		cList[instName] := Map("type","Instance","name",instName,"params",params,"class",className)
	}
	
	return cList
}

GetCustFuncParams(funcBody) {
	r := RegExMatch(funcBody,"mi)^[ \t]*(Static[ \t]+)?([\w\.]+)(\x28.*\x29)([ \t]*\;.*)?[ \t\r\n]*\{",match)
	If (match.HasMethod("Value"))
		return match.Value(3)
	Else
		return ""
}

GetPropParams(paramBody) {
	r := RegExMatch(paramBody,"mi)^[ \t]*(Static[ \t]+)?([\w\.]+)(\[.*?\])?([ \t]*\;.*)?[ \t\r\n]*\{",match)
	If (match.HasMethod("Value"))
		return match.Value(3)
	Else
		return ""
}

getVar(stringText,fileName,lineNum,lastStatus) {
	status := ""
	If (RegExMatch(stringText,"^[ \t]*Global"))
		status := "Global"
	Else If (RegExMatch(stringText,"^[ \t]*Static"))
		status := "Static"
	Else If (lastStatus And RegExMatch(stringText,"^[ \t]*\,")) {
		; debug.msg("continuation section... " lastStatus)
		status := lastStatus
	}
	
	stringText := RegExReplace(stringText,"(^[ \t]*Global[ \t]+|^[ \t]*Static[ \t]+)","")
	noStr := RTrim(StringOutline(stringText)," `t")
	stringText := Trim(SubStr(stringText,1,StrLen(noStr))," `t,")
	
	varList := Array()
	
	varArr := StrSplit(noStr,",")
	
	If (status) {
		; Debug.Msg(stringText)
		
		For i, txt in varArr {
			; Debug.Msg(txt)
			txt := Trim(txt," `t")
			If (!txt) {
				; debug.msg("maybe this is screwing the pooch")
				Continue
			}
			realLine := Trim(SubStr(stringText,1,StrLen(txt)))
			
			; debug.msg(realLine "`r`ni: " i "`r`nline: " lineNum)
			
			If (splitter := InStr(realLine,":="))
				varName := Trim(SubStr(realLine,1,splitter-1)), varValue := Trim(SubStr(realLine,splitter)," `t:=")
			Else
				varName := Trim(realLine," `t"), varValue := ""
			
			varList.Push(Map("varName",varName,"varValue",varValue,"fileName",fileName,"lineNum",lineNum,"status",status))
			
			stringText := SubStr(stringText,StrLen(txt)+1)
			stringText := Trim(stringText," ,")
		}
	} Else
		return varList
	
	
	
	
	; curPos := 1
	; rg := "(([\w\.]+)[ \t]*\:\=)"
	; lastPos := 0, lastLen := 0, lastVar := "", lastValue := "", t := 0
	; While (r := RegExMatch(noStr,rg,match,curPos)) {
		; t := A_Index
		; If (lastPos = 0) {
			; lastVar := match.Value(2)
			; lastPos := match.Pos(0)
			; lastLen := match.Len(0)
			; lastValue := match.Value(0)
		; } Else {
			; varName := lastVar
			; startPos := lastPos + lastLen
			; curLen := match.Pos(0) - lastPos - lastLen
			; varValue := Trim(SubStr(stringText,startPos,curLen)," `t")
			
			; varList.Push(Map("varName",varName,"varValue",varValue,"fileName",fileName,"lineNum",lineNum,"status",status))
			; lastVar := match.Value(2), lastPos := match.Pos(0), lastLen := match.Len(0)
		; }
		; curPos := match.Pos(0) + match.Len(0)
	; }
	
	; If (t > 0) {
		; varName := lastVar
		; startPos := lastPos + lastLen
		; varValue := Trim(SubStr(stringText,startPos)," `t")
		
		; varList.Push(Map("varName",varName,"varValue",varValue,"fileName",fileName,"lineNum",lineNum,"status",status))
	; }
	
	; varList.Push(Map("varName",varName,"varValue",varValue,"fileName",fileName,"lineNum",lineNum,"status",status))
	return varList
}

ParseObjects() {
	listObj := ObjectCreateList
	For i, obj in VariablesList {
		
	}
}

        ; "elementName": "GetTextDims",
        ; "fileName": "C:\\Users\\Shmoo\\Documents\\GitHub\\CallTipsForAll\\LibV2\\_Init_and_Support_Funcs.ahk",
        ; "lineNum": 262,
        ; "status": "",
        ; "varName": "maxWidth",
        ; "varValue": "0"