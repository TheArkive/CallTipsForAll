; Contains functions for parsing the active window, language files, and/or includes
; in order to load data for call tips.

; ==================================================
; Determines parent window based on user settings.
; ==================================================
editorCtl(method:="") {
	curExe := Settings["ProgExe"], curClassNN := Settings["ProgClassNN"], ClassNNcomp := curClassNN "1"
	
	If (!oCallTip.progHwnd Or method = "") {
		winList := WinGetList("ahk_exe " curExe)
		
		Loop winList.Length {
			curHwnd := winList[A_Index]
			ctlArr := WinGetControls("ahk_id " curHwnd)
			
			Loop ctlArr.Length {
				If (ClassNNcomp = ctlArr[A_Index]) {
					found := true, hwndComp := curHwnd
					Break
				}
			}
		}
	} Else If (method = "click") {
		MouseGetPos  x,y,hWnd, ClassNN
		If (!InStr(ClassNN,curClassNN))
			return {ctlClassNN: "", progHwnd: 0, ctlHwnd: 0}
		Else
			ClassNNcomp := ClassNN, found := true, hwndComp := hWnd
	}
	
	If (!found)
		return {ctlClassNN: "", progHwnd: 0, ctlHwnd: 0}
	
	retVal := {ctlClassNN: ClassNNcomp, progHwnd: hwndComp}
	ctlHwnd := ControlGetHwnd(ClassNNcomp,"ahk_id " hwndComp)
	retval.progTitle := WinGetTitle("ahk_id " hwndComp)
	retVal.ctlHwnd := ctlHwnd
	
	return retVal
}

; ==================================================
; keyword list - These are also generated when functions, methods, and properties are
;                enumerated from List_*.txt files.  This will be expanded for an auto
;                complete function/hotkey.  KW_*.txt files are optional, and don't serve
;                much purpose yet.
; ==================================================

LoadKeywordsList() {
	KeywordList := Map(), srcFiles := oCallTip.srcFiles
	Loop Files srcFiles "Keywords\KW_*.txt"
	{
		curText := FileRead(A_LoopFileFullPath)
		Loop Parse curText, "`n", "`r"
			KeywordList[A_LoopField] := "keyword"
	}
}
; ==================================================
; Create function and command list for call tips / and classes?
; ==================================================

LoadFunctionsList() {
	FunctionList := Map(), srcFiles := oCallTip.srcFiles
	Loop Files srcFiles "\Other\List_*.txt" ; functions and commands are combined into one list => FunctionList[]
	{
		a := StrSplit(A_LoopFileName,"_") ; comma (,) separator
		fileName := A_LoopFileFullPath, curFileType := StrReplace(a[2],".txt")
		
		curList := FileRead(fileName)
		curList := Trim(curList,"`r`n") entryEnd
		curPos := 1, subStrEnd := 1
		len := StrLen(curList)

		While (curPos <= len And subStrEnd) {
			subStrEnd := InStr(curList,entryEnd,false,curPos)
			curLen := subStrEnd - curPos
			curSubStr := SubStr(curList,curPos,curLen)
			
			If (curFileType = "Regex") {
				Loop Parse curSubStr, "`n", "`r"
				{
					If (A_Index = 1)
						oCallTip.funcStart := Trim(SubStr(A_LoopField,16))
					Else If (A_Index = 2)
						oCallTip.funcEnd := Trim(SubStr(A_LoopField,14))
					Else if (A_Index = 3)
						oCallTip.funcReturn := Trim(SubStr(A_LoopField,17))
					Else If (A_Index = 4)
						oCallTip.classStart := Trim(SubStr(A_LoopField,13))
					Else If (A_Index = 5)
						oCallTip.classEnd := Trim(SubStr(A_LoopField,11))
					Else If (A_Index = 6)
						oCallTip.classMethod := Trim(SubStr(A_LoopField,9))
					Else If (A_Index = 7)
						oCallTip.classProperty := Trim(SubStr(A_LoopField,11))
					Else If (A_Index = 8)
						oCallTip.classInstance := Trim(SubStr(A_LoopField,16))
					Else If (A_Index = 9)
						oCallTip.includes := Trim(SubStr(A_LoopField,11))
				}
			} Else {
				funcName := "", funcHelpLink := "", funcDescArr := Array()
				funcArr := StrSplit(curSubStr,Chr(96)), i := funcArr.Length
				
				Loop i {
					t := Trim(funcArr[A_Index],"`r`n")
					If (A_Index = 1) {
						Loop Parse t, "`n", "`r"
						{
							If (A_Index = 1)
								funcType := A_LoopField
							Else If (A_Index = 2)
								funcName := A_LoopField
						}
					} Else If (A_Index > 1 And A_Index < i) {
						funcDescArr.Push(t)
					} Else If (A_Index = i) {
						funcHelpLink := t
					}
				}
				
				curObj := Map()
				curObj["desc"] := funcDescArr
				curObj["type"] := funcType ; "function" or "command"
				curObj["helpLink"] := funcHelpLink
				FunctionList[funcName] := curObj
				KeywordList[funcName] := "function"
			}
			curPos := subStrEnd + StrLen(entryEnd), curObj := ""
		}
	}
}

; FunctionList Structure
; ========================
;	FunctionList
;
;		funcName / curObj
;
;			desc / descArr
;			type / funcTypeStr
;			helpLink / helpLinkStr


; msgbox "Begin:   " funcBeginStr "`r`nEnd:   " funcEndStr
; ==================================================
; for debug only
; ==================================================
; For funcName, obj in FunctionList {
	; type := obj["type"]
	; desc := obj["desc"]
	
	; textList .= funcName " / " type "`r`n" desc "`r`n`r`n`r`n"
; }
; clipboard := textList
; msgbox textList

; ==================================================
; generate custom function list - taken care of in ReloadElements()
; ==================================================
; hCtl := editorCtlHwnd(hEditorWin,cClassNN,EventType)
; curDocText := ControlGetText(cClassNN,"ahk_id " hEditorWin)
; curDocArr := StrSplit(curDocText,"`n","`r")
; CustomFunctions := GetCustomFunctions(curDocArr)

; ==================================================
; Create Object Index by type for call tips
; ==================================================

LoadMethPropList() {
	MethPropList := Map(), srcFiles := oCallTip.srcFiles
	Loop Files srcFiles "\Objects\*.txt"
	{
		a := StrSplit(A_LoopFileName,"_")
		fnType := a.Has(2) ? a[2] : A_LoopFileName, fnType := StrReplace(fnType,".txt","")
		
		curList := FileRead(A_LoopFileFullPath)
		curList := Trim(curList,"`r`n") entryEnd
		objList := "", objListPre := "", lineSplit := "", curObjType := ""
		curPos := 1, curHelpLink := "", propText := "", methText := ""
		len := StrLen(curList)
		
		curObj := Map(), propList := Map(), methList := Map()

		While (curPos <= len) {
			subStrEnd := InStr(curList,entryEnd,false,curPos)
			curLen := subStrEnd - curPos
			curSubStr := SubStr(curList,curPos,curLen)
			
			curDesc := "", curMemType := "", curMem := ""
			If (A_Index = 1) {
				objMatchArr := StrSplit(curSubStr,Chr(96))
				i := objMatchArr.Length
				
				objDescArr := Array(), methPropArr := Array(), curHelpLink := ""
				Loop i {
					t := Trim(objMatchArr[A_Index],"`r`n")
					If (A_Index = 1) {
						objListPre := t, objMatchText .= objListPre "`r`n"
					} Else If (A_Index > 1 And A_Index < i) {
						objDescArr.Push(t)
					} Else If (A_Index = i) {
						curHelpLink := t
					}
				}
				
				Loop Parse objListPre, "`n", "`r" ; create list of defined objTypes
				{
					lineSplit := StrSplit(A_LoopField," "), curObjType := lineSplit[2]
					objList .= curObjType "`r`n"
				}
				objList := Sort(objList,"U")
				objList := Trim(objList,"`r`n")
			} Else {
				memMap := Map(), memDescArr := Array(), memHelpLink := ""
				methPropArr := StrSplit(curSubStr,Chr(96)), i := methPropArr.Length
				Loop i {
					t := Trim(methPropArr[A_Index],"`r`n")
					If (A_Index = 1) {
						Loop Parse t, "`n", "`r"
						{
							If (A_Index = 1)
								curMemType := A_LoopField
							Else If (A_Index = 2)
								curMem := A_LoopField
						}
					} Else if (A_Index > 1 And A_Index < i) {
						memDescArr.Push(t)
					} Else If (A_Index = i) {
						memHelpLink := t
					}
				}
				
				memMap["desc"] := memDescArr
				memMap["helpLink"] := memHelpLink
				
				curMemType := StrLower(curMemType)  
				KeywordList[curMem] := curMemType
				If (curMemType = "Method")
					methList[curMem] := memMap
				Else
					propList[curMem] := memMap
			}
		
			curPos := subStrEnd + StrLen(entryEnd)
		}
		
		methTitle := "Methods: " ; create first desc as method/property list
		For methName in methList
			methText .= "." methName ", "
		
		methText := Trim(methText," ,`r`n")
		If (methText)
			methText := methTitle methText
		
		propTitle := "Properties: "
		For propName in propList
			propText .= "." propName ", "
		
		propText := Trim(propText," ,`r`n")
		If (propText)
			propText := propTitle propText
		
		firstDesc := Trim("[ObjectTypeName]`r`n`r`n" methText,"`r`n")
		firstDesc := Trim(firstDesc "`r`n`r`n" propText,"`r`n")
		
		curObj["method"] := methList, curObj["property"] := propList
		curObj["helpLink"] := curHelpLink
		
		Loop Parse objList, "`n", "`r" ; append methods/properties to all defined obj types
		{
			curFirstDesc := StrReplace(firstDesc,"[ObjectTypeName]",A_LoopField)
			objDescArr.InsertAt(1,curFirstDesc)
			curObj["desc"] := objDescArr
			MethPropList[A_LoopField] := curObj
		}
		curObj := "", methList := "", propList := ""
	}
	
	return objMatchText
}
; MethPropList Structure
; ==========================
;	MethPropList
;
;		objType / curObj
;
;			method / methObj
;				helpLink / helpLinkStr
;				desc     / descArr
;
;			prop / propObj
;				helpLink / helpLinkStr
;				desc     / descArr
;
;			helpLink / helpLinkStr
;			desc / descArr

; ==================================================
; for debug only
; ==================================================
; For objType in MethPropList { ; list obj types
	; testList .= objType "`r`n"
; }
; MsgBox testList

; ==================================================
; for debug only
; ==================================================
; objType := "ArrayObject" ; for debug only
; testObj := MethPropList[objType]
; mList := testObj["method"]

; testList := objType "`r`n`r`n" ; specific object
; For curMeth, curDesc in mList {
	; testList .= "Memeber: " curMeth "`r`n" curDesc "`r`n`r`n"
; }
; MsgBox testList

; ==================================================
; Generates a heirarchical list of object match strings to be executed in a specific order.
; Execution is done by CreateObjList()
; Heirarchy:    List
;                   Level / LevelObj
;                       Label / LabelObj
;                           Member: regex  (string - the regex string)
;                           Member: type   (string - object type)
;                           Member: direct (boolean - indicates if {substitution} in regex is required)
; ==================================================
LoadObjectCreateList(objMatchText) {
	ObjectCreateList := Map()
	objMatchText := Sort(objMatchText)
	objMatchText := Trim(objMatchText,"`r`n")
	curLevel := 0, prevLevel := 0, curLevelObj := Map()

	Loop Parse objMatchText, "`r", "`n"
	{
		curObj := Map()
		result := RegExMatch(A_LoopField,"([0-9]) ([\w]+) ([\w]+) (.*)",match)
		
		If (IsObject(match) And match.Count() = 4) {
			curLevel := match.Value(1), curObjType := match.Value(2)
			curLabel := match.Value(3), regex := match.Value(4)
			isDirect := InStr(regex,"{") ? 0 : 1 ; reorganize with level sub object
			
			If (curLevel != prevLevel)
				ObjectCreateList[prevLevel] := curLevelObj, curLevelObj := Map()
			
			curObj := Map()
			curObj["regex"] := regex, curObj["type"] := curObjType, curObj["direct"] := isDirect
			curLevelObj[curLabel] := curObj
			
			prevLevel := curLevel
		} Else {
			msg := "Line:`r`n    " A_LoopField "`r`n`r`nObject File - improper format.`r`n`r`nLine Format:`r`n    Level ObjType Label RegExMatchString"
			MsgBox msg
			ExitApp
		}
		curObj := ""
	}
	ObjectCreateList[prevLevel] := curLevelObj, curLevelObj := ""
}

; ==================================================
; for debug only
; ==================================================
; msgbox "isOb: " IsObject(ObjectCreateList) " / " ObjectCreateList.Count

; For level, lvlObj in ObjectCreateList { ; for debug only
	; For label, labelObj in lvlObj {
		; regex := labelObj["regex"]
		; type := labelObj["type"]
		; direct := labelObj["direct"]
		; testList .= level " / " label " / " type "`r`n" regex "`r`n`r`n"
	; }
; }
; msgbox testList



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

; ==================================================
; for debug only
; ==================================================
; For objName, obj in ObjectList { ; for debug only
	; label := type := obj["type"], obj["label"], match := obj["match"]
	; testList .= objName " / " label " / " type "`r`n" match "`r`n`r`n"
; }
; msgbox "Final:`r`n`r`n" testList

; ==================================================
; end init =========================================
; ==================================================

; ================================================================
; Generates a list of objects in the document as defined by the user.
; This will NOT do class objects (yet).
; If an object in the code document is assigned to a member of an object,
; and that member is a keyword (built-in method or property) then that entry
; will not be added to this list.
; ================================================================
CreateObjList(curDocText) { ; v2 - loops full text in one chunk, hopefully uses less CPU
	oList := Map()
	
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
								objName := objNameArr[2]
							
							If (!oList.Has(objName))
								obj := Map()
							Else
								obj := oList[objName]
							
							objMatch := (c = 2) ? match.Value(2) : match.Value(1)
							typeObj["label"] := label
							typeObj["match"] := objMatch
							If (type)
								obj[type] := typeObj
							
							quit := false
							If (KeywordList.Has(objName)) ; omit in ObjectList if objName is a keyword
								quit := true
							
							If (!quit) ; If (!oList.Has(objName))
								oList[objName] := obj
							
							curPos := match.Pos(c) + match.Len(c)
						} Else
							curPos := StrLen(curDocText)
					}
				} Else { ; perform {substitution} before doing regex match and adding to ObjectList
					r1 := RegExMatch(regex,"\{(.*?)\}",match), listType := match.Value(1)
					
					For curObjName, curLblObj in oList {
						For curType, curTypeObj in curLblObj {
							If (curType = listType) {
								newRegex := StrReplace(regex,"{" listType "}",curObjName)
								
								While (result := RegExMatch(curDocText,"i)" newRegex,match,curPos)) {
									c := match.Count()
									If (IsObject(match) And c = 2) {
										typeObj := Map(), objName := match.Value(1) ; obj := Map()
										objNameArr := StrSplit(objName,".")
										If (objNameArr.Length > 1)
											objName := objNameArr[2]
										
										If (!oList.Has(objName))
											obj := Map()
										Else
											obj := oList[objName]
										
										objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
										typeObj["label"] := label
										typeObj["match"] := objMatch
										If (type)
											obj[type] := typeObj
										
										quit := false
										If (KeywordList.Has(objName)) ; omit in ObjectList if objName is a keyword
											quit := true
										
										If (!quit) ; If (!oList.Has(objName))
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
					objName := objNameArr[2]
				
				objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
				
				quit := false
				If (KeywordList.Has(objName)) ; omit in ObjectList if objName is a keyword
					quit := true
				
				If (!quit) ; If (!oList.Has(objName))
					oList[objName] := curObjType
				
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
GetCustomFunctions(curDocText) {
	funcList := Map(), curPos1 := 1
	While (result := RegExMatch(curDocText,"mi)" oCallTip.funcStart,match,curPos1)) { ; funcBeginStr is defined in List_Functions.txt
		If (IsObject(match) And match.Count()) {
			curPos1 := match.Pos(2) + match.Len(2)
			r2 := RegExMatch(curDocText,"i)" oCallTip.funcEnd,match2,curPos1)
			funcName := match.Value(1)
			params := match.Value(2)
			body := Trim(SubStr(curDocText,curPos1,match2.Pos(1)-curPos1+1)," `t`r`n")
			
			if (funcName != "") {
				funcBody := curDocLine
				obj := Map("type","CustomFunction","desc",funcName params,"funcBody",body)
				funcList[funcName] := obj
			}
			
			returnObj := Map(), curPos2 := 1
			While (r3 := RegExMatch(body,"mi)" oCallTip.funcReturn,match3,curPos2)) {
				If (match3.Count()) {
					cMatch := match3.Value(1)
					returnObj[cMatch] := match3.Pos(1)
				}
				curPos2 := match3.Pos(1) + match3.Len(1)
			}
			
			If (returnObj.Count > 0)
				obj["return"] := returnObj ; attach returnObj list of "return" lines if exist
		}
		
		
		funcBody := "", funcName := "", match := "", params := "", returnObj := ""
	}
	
	curDocText := "", obj := ""
	return funcList
}

GetClasses(curDocText) {
	; msgbox oCallTip.classStart "`r`n`r`n" oCallTIp.classEnd
	classList := Map(), curPos1 := 1
	While (result := RegExMatch(curDocText,"mi)" oCallTip.classStart,match,curPos1)) { ; funcBeginStr is defined in List_Functions.txt
		; msgbox "in"
		If (IsObject(match) And match.Count()) {
			If (match.Value(1) != "") {
				className := match.Value(1), extends := ""
				curPos1 := match.Pos(1) + match.Len(1)
			} Else {
				className := match.Value(2), extends := match.Value(3)
				curPos1 := match.Pos(3) + match.Len(3)
			}
			; msgbox "className: " className
			
			r2 := RegExMatch(curDocText,"i)" oCallTip.classEnd,match2,curPos1)
			classBody := Trim(SubStr(curDocText,curPos1,match2.Pos(1)-curPos1+1)," `t`r`n")
			
			memberList := Map(), curPos2 := 1, curPos3 := 1
			While (r3 := RegExMatch(classBody,"mi)" oCallTip.classMethod,match3,curPos2)) {
				; msgbox match3.Value(1) " / " match3.Value(2)
				memberName := match3.Value(1)
				memberParams := match3.Value(2)
				curPos2 := match3.Pos(2) + match3.Len(2)
				memberList[memberName] := Map("name",memberName,"params",memberParams,"type","method")
			}
			
			While (r4 := RegExMatch(classBody,"mi)" oCallTip.classProperty,match4,curPos3)) {
				memberName := match4.Value(1)
				memberParams := match4.Value(2)
				
				curPos3 := match4.Pos(1) + match4.Len(1)
				curPos3 := match4.Value(2) != "" ? match4.Pos(2) + match4.Len(2) : curPos3
				
				memberList[memberName] := Map("name",memberName,"params",memberParams,"type","property")
				; msgbox match4.Value(1) " / " match4.Value(2) " / " curPos3
			}
			
			if (className != "") {
				obj := Map("type","Class","desc",className,"classBody",classBody,"extends",extends)
				obj["members"] := memberList
				classList[className] := obj
			}
			
			; For k, v in memberList
				; fullList .= k " / " v["params"] " / " v["type"] "`r`n"
			; msgbox fullList
		}
		
		funcBody := "", funcName := "", match := "", params := "", returnObj := ""
	}
	
	; msgbox classList.Count
	
	curDocText := "", obj := ""
	return classList
}

ScanClasses(curDocArr) { ; scan doc for class instances
	Loop curDocArr.Length {
		curLine := curDocArr[A_Index]
		For className, obj in ClassesList {
			; msgbox className "`r`n`r`n" curLine "`r`n`r`n" oCallTip.classInstance
			If (RegExMatch(curLine,"i)" StrReplace(oCallTip.classInstance,"{Class}",className),match)) {
				; msgbox match.Value(1) " / " match.Value(2)
				instName := match.Value(1)
				params := match.Value(2)
				ClassesList[instName] := Map("type","Instance","name",instName,"params",params,"class",className)
			}
		}
	}
}