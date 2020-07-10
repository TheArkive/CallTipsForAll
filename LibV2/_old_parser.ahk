; ============================================================
; Parse document and gereate list of object names and other data. - done in ReloadElements()
; ============================================================
; ObjectList := CreateObjList(curDocText)
; curDocArr := "", curDocText := "" ; these will be recreated on demand
; ReloadElements()

; ObjectList Structure
;
;	ObjectList
;
;		objName / typeObjList
;
;			typeObj / typeObj
;
;				label / labelStr
;				match / matchStr

; ================================================================
; Generates a list of objects in the document as defined by the user.
; This will NOT do class objects.  That is a separate function.
; If an object in the code document is assigned to a member of an object,
; and that member is a keyword (built-in method or property) then that entry
; will not be added to this list.
; ================================================================
CreateObjList(curDocText) { ; v2 - loops full text in one chunk, hopefully uses less CPU
	oList := Map() ; , oList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	
	For level, lvlObj in ObjectCreateList {
		For label, lblObj in lvlObj {
			i := 1, type := lblObj["type"], regex := lblObj["regex"], direct := lblObj["direct"]
			
				curPos := 1
				
				If (direct) {
					While(result := RegExMatch(curDocText,"i)" regex,match,curPos)) {
						c := match.Count()
						If (IsObject(match) And c >= 1) {
							typeObj := Map(), objName := match.Value(1) ; obj := Map()
							objNameArr := StrSplit(objName,".")
							If (objNameArr.Length > 1)
								objName := objNameArr[objNameArr.Length]
							
							If (!oList.Has(objName))
								obj := Map(), obj["types"] := Map()
							Else
								obj := oList[objName]
							
							objMatch := (c = 2) ? match.Value(2) : match.Value(1)
							typeObj["label"] := label
							typeObj["match"] := objMatch
							If (type)
								obj["types"][type] := typeObj
							
							obj["index"] := GetLineNum(match.Pos(0))
							oList[objName] := obj
							
							curPos := match.Pos(c) + match.Len(c)
						} Else
							curPos := StrLen(curDocText)
					}
				} Else { ; perform {substitution} before doing regex match and adding to ObjectList
					r1 := RegExMatch(regex,"\{(.*?)\}",match), listType := match.Value(1)
					
					For curObjName, curLblObj in oList {
						curLblObj := curLblObj["types"]
						For curType, curTypeObj in curLblObj {
							If (curType = listType) {
								newRegex := StrReplace(regex,"{" listType "}",curObjName)
								
								While (result := RegExMatch(curDocText,"i)" newRegex,match,curPos)) {	
									c := match.Count()
									If (IsObject(match) And c = 2) {
										typeObj := Map(), objName := match.Value(1) ; obj := Map()
										objNameArr := StrSplit(objName,".")
										If (objNameArr.Length > 1)
											objName := objNameArr[objNameArr.Length]
										
										If (!oList.Has(objName))
											obj := Map(), obj["types"] := Map()
										Else
											obj := oList[objName]
										
										objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
										typeObj["label"] := label
										typeObj["match"] := objMatch
										If (type)
											obj["types"][type] := typeObj
										
										obj["index"] := GetLineNum(match.Pos(0))
										oList[objName] := obj
										
										curPos := match.Pos(c) + match.Len(c)
									} Else
										curPos := StrLen(curDocText)
								} ; end while
							}
						} ; end for
					} ; end for
				}
		} ; end for
	} ; end for
	
	For custFuncName, curLblObj in CustomFunctions {
		curRetStr := "", curObjType := "", newRegex := "", doMatch := false
		
		If (curLblObj.Has("return")) {
			returnObj := curLblObj["return"]
			For retStr, L in returnObj { ; loop through return entries in Func
				For curObjName, curLblObj in oList { ; loop through oList to find objMatch
					If (retStr = curObjName) {
						doMatch := true
						newRegex := "([\w\.]+)[ \t]*:=[ \t]*({CustomFunction}\x28)"
						newRegex := StrReplace(newRegex,"{CustomFunction}",custFuncName)
						
						curRetStr := retStr ; return var in matched function
						curObjType := curLblObj
						Break ; break on match
					}
				} ; end for
				
				If (curRetStr)
					Break
			} ; end for
		}
		
		curPos := 1
		While (result := RegExMatch(curDocText,"i)" newRegex,match,curPos) And doMatch) {
			c := match.Count()
			If (IsObject(match) And match.Count() = 2 and curRetStr) { ; if match add obj to list
				obj := Map(), objName := match.Value(1)
				objNameArr := StrSplit(objName,".")
				If (objNameArr.Length > 1)
					objName := objNameArr[objNameArr.Length]
				
				objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
				
				quit := false
				If (KeywordList.Has(objName)) ; omit in ObjectList if objName is a keyword
					quit := true
				
				If (!quit) { ; If (!oList.Has(objName))
					curObjType["index"] := match.Pos(0) ; hope this works
					oList[objName] := curObjType
				}
				
				curPos := match.Pos(c) + match.Len(c)
			} Else
				curPos := StrLen(curDocText)
		} ; end while
	} ; end for
	
	return oList
}

; ================================================================
; Creates a list of user defined functions.  The call tip only shows FuncName(params...) and
; it is not currently possible to add extra help in the call tip for custom functions.
; ================================================================

GetLineNum(inPos) { ; DocumentMap.Push(Map("fileName",File, "lineNum",lineNum, "start",start, "end",end, "length",lineLen))
	retVal := ""
	For i, obj in DocumentMap {
		start := obj["start"], end := obj["end"]
		If (inPos >= start And inPos <= end) {
			retVal := i
			Break
		}
	}
	
	return retVal
}

GetCustomFunctions(curDocText) { ;ZZZ - this should work better
	curDocTextNoStr := oCallTip.docTextNoStr
	funcList := Map(), funcList.CaseSense := 0
	curPos1 := 1
	
	While (result := RegExMatch(curDocTextNoStr,"mi)" oCallTip.funcStart,match,curPos1)) {
		funcName := "", bodyText := "", obj := Map()
		If (IsObject(match) And match.Count()) {
			curPos1 := match.Pos(0)
			funcName := match.Value(1)
			If (funcName = "" Or funcName = "If" Or funcName = "While" Or funcName = "For" Or funcName = "Else")
				Continue
			
			curPos2 := curPos1
			While (r2 := RegExMatch(curDocTextNoStr,"mi)" oCallTip.funcEnd,match2,curPos2)) {
				bodyText := SubStr(curDocTextNoStr,curPos1,match2.Pos(0)+match2.Len(0)-curPos1)
				curPos2 := match2.Pos(0) + match2.Len(0)
				w := StrReplace(StrReplace(bodyText,"{","{",lB),"}","}",rB)
				
				If (lB = rB) {
					index := GetLineNum(curPos1)
					bodyText := SubStr(curDocText,curPos1,match2.Pos(0)+match2.Len(0)-curPos1)
					params := GetCustFuncParams(bodyText)
					obj := Map("type","CustomFunction","desc",funcName params,"funcBody",bodyText,"index",index,"params",params)
					curPos1 := curPos2
					Break
				}
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
	}
	
	return funcList
}

GetCustFuncParams(funcBody) {
	r := RegExMatch(funcBody,"mi)" oCallTip.funcStart,match)
	return match.Value(2)
}

CheckShowMode(strBody,helper:="") { ; checks class, methods, properties for "; show" or "; hide" comment
	showStyle := ""
	arr := StrSplit(strBody,"`n","`r"), firstLine := StringOutline(arr[1],false) ; blank out strings, but not comments
	r := RegExMatch(firstLine,oCallTip.lineComment,match)
	
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

GetClasses(curDocText, parent := "") {
	curDocTextNoStr := StringOutline(curDocText) ; pull this from oCallTip so it only has to be done once
	static classList := Map()
	If (!classList.Count)
		classList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	curPos1 := 1 ;??? oCallTip.classStart is not robust, see comments on function needle   ;ZZZ - do you mainly mean that it doesn't include #@$ ?  I thought class parsing was easier to write (imho).  Can classes include #@$ ?
	
	While (result := RegExMatch(curDocTextNoStr,"mi)" oCallTip.classStart,match,curPos1)) { ; parse classes
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
			While (r12 := RegExMatch(curDocTextNoStr,"mi)" oCallTip.classEnd,match12,curPos12)) {
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
			
			obj := Map("type","Class","desc",className,"classBody",Trim(classBody," `t`r`n"),"extends",extends,"index",GetLineNum(match.Pos(0)),"parent",parent)
			classBody := PruneFirstLast(classBody) ; prune first and last lines
			memberList := Map()
			classBodyNoStr := StringOutline(classBody)
			
			curPos14 := 1
			While (r14 := RegExMatch(classBodyNoStr,"mi)" oCallTip.classStart,match14,curPos14)) { ; try to get sub classes
				curPos14 := match14.Pos(0)
				
				curPos13 := curPos14
				While (r13 := RegExMatch(classBodyNoStr,"mi)" oCallTip.classEnd,match13,curPos13)) {
					innerClassBody := SubStr(classBodyNoStr,curPos14,match13.Pos(0)+match13.Len(0)-curPos14)
					curPos13 := match13.Pos(0) + match13.Len(0)
					w := StrReplace(StrReplace(innerClassBody,"{","{",lB),"}","}",rB)
					
					If (lB = rB) {
						innerClassBody := SubStr(classBody,curPos14,match13.Pos(0)+match13.Len(0)-curPos14)
						
						GetClasses(innerClassBody,className)
						blankReplace := RegExReplace(innerClassBody,".*"," ")
						classBody := StrReplace(classBody,innerClassBody,blankReplace) ; blank out the sub class after parsing it
						classBodyNoStr := StringOutline(classBody)
						; curPos1 := curPos13
						
						Break
					}
				}
			}
			
			curPos2 := 1
			While (r2 := RegExMatch(classBodyNoStr,"mi)" oCallTip.classMethodStart,match2,curPos2)) { ; parse methods - multi-line
				isStatic := match2.Value(1) ? true : false
				memberName := match2.Value(2)
				memberParams := match2.Value(3)
				curPos2 := match2.Pos(0)
				
				curPos5 := curPos2
				While (r5 := RegExMatch(classBodyNoStr,"mi)" oCallTip.ClassMethodEnd,match5,curPos5)) {
					methBody := SubStr(classBodyNoStr,curPos2,match5.Pos(0)+match5.Len(0)-curPos2) ; this needs to be fixed, should be parsing the "NoStr" var
					curPos5 := match5.Pos(0) + match5.Len(0)
					w := StrReplace(StrReplace(methBody,"{","{",lB),"}","}",rB)
					
					If (lB = rB) {
						methBody := SubStr(classBody,curPos2,match5.Pos(0)+match5.Len(0)-curPos2)
						curPos2 := curPos5
						showStyle := CheckShowMode(methBody,"method - " memberName)
						
						If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
							memberList[memberName] := Map("name",memberName,"params",memberParams,"type","method","body",Trim(methBody,"`r`n"),"static",isStatic)
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
			While (r4 := RegExMatch(classBodyNoStr,"mi)" oCallTip.classPropertyStart,match4,curPos4)) {
				If (r15 := RegExMatch(match4.Value(0),"mi)" oCallTip.classStart))
					continue
				
				isStatic := match4.Value(1) ? true : false
				memberName := match4.Value(2)
				memberParams := match4.Value(3)
				curPos4 := match4.Pos(0)
				
				curPos6 := curPos4
				While (r6 := RegExMatch(classBodyNoStr,"mi)" oCallTip.ClassPropertyEnd,match6,curPos6)) {
					propBody := SubStr(classBodyNoStr,curPos4,match6.Pos(0)+match6.Len(0)-curPos4)
					curPos6 := match6.Pos(0)
					w := StrReplace(StrReplace(propBody,"{","{",lB),"}","}",rB)
					
					If (lB = rB) {
						propBody := SubStr(classBody,curPos4,match6.Pos(0)+match6.Len(0)-curPos4)
						curPos4 := curPos6
						showStyle := CheckShowMode(propBody,"property - " memberName)
						
						If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
							memberList[memberName] := Map("name",memberName,"params",memberParams,"type","property","body",Trim(propBody,"`r`n"),"static",isStatic)
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
			While (r7 := RegExMatch(StringOutline(classBody,false),"im)" oCallTip.classMethodOneLine,match7,curPos7)) {
				isStatic := match7.Value(1) ? true : false
				memberName := match7.Value(2)
				memberParams := match7.Value(3)
				showStyle := CheckShowMode(match7.Value(0))
				curPos7 := match7.Pos(0) + match7.Len(0)
				
				If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
					memberList[memberName] := Map("name",memberName,"params",memberParams,"type","method","body",Trim(match7.Value(0),"`r`n"),"static",isStatic)
			}
			
			curPos8 := 1
			While (r8 := RegExMatch(StringOutline(classBody,false),"mi)" oCallTip.classPropertyOneLine,match8,curPos8)) {
				isStatic := match8.Value(1) ? true : false
				memberName := match8.Value(2)
				memberParams := match8.Value(3)
				showStyle := CheckShowMode(match8.Value(0))
				curPos8 := match8.Pos(0) + match8.Len(0)
				
				If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
					memberList[memberName] := Map("name",memberName,"params",memberParams,"type","property","body",Trim(match8.Value(0),"`r`n"),"static",isStatic)
			}
		
			classBodyRemain := classBody
			
			For memName, memObj in memberList {
				memBody := memObj["body"]
				classBodyRemain := StrReplace(classBodyRemain,memBody)
			}
			
			Loop Parse classBodyRemain, "`n", "`r"
			{
				curLine := Trim(A_LoopField," `t")
				isStatic := (InStr(curLine,"Static ")) ? true : false
				
				curPos11 := 1
				While (r11 := RegExMatch(curLine,oCallTip.classSmallPropExt,match11,curPos11)) {
					memberName := match11.Value(2)
					showStyle := Trim(match11.Value(3)," `t;")
					
					If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
						memberList[memberName] := Map("name",memberName,"params","","type","property","body","","static",isStatic)
					
					curPos11 := match11.Pos(1) + match11.Len(1)
				}
			}
			
			obj["members"] := memberList
			classList[className] := obj
		}
		
		funcBody := "", funcName := "", match := "", params := "", returnObj := "", className := ""
	}
	
	curDocText := "", obj := ""
	return classList
}

ScanClasses(curDocText) { ; scan doc for class instances
	For className, obj in ClassesList {
		curPos := 1 ;??? oCallTip.classInstance is most likely not robust enough   ;ZZZ - do you mean not accounting for #$@ in class names?
		While (RegExMatch(curDocText,"mi)" StrReplace(oCallTip.classInstance,"{Class}",className),match,curPos)) {
			instName := match.Value(1)
			params := match.Value(2)
			curPos := match.Pos(0) + match.Len(0)
			ClassesList[instName] := Map("type","Instance","name",instName,"params",params,"class",className,"index",GetLineNum(match.Pos(1)))
		}
	}
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
		includesSearch := oCallTip.includes
		
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

findFile(sInFile) {
	result := ""
	Loop Files sInFile ".*" ; this gets <Lib> references
	{
		If (A_LoopFileName) {
			result := A_LoopFileFullPath
			break
		}
	}
	
	If (!result)
		result := !FileExist(sInFile) ? "" : sInFile ;ZZZ - simplified
	
	return result
}
