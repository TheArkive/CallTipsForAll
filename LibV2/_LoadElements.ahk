; Contains functions for parsing the active window, language files, and/or includes
; in order to load data for call tips.

; ================================================================
; Reads current document and caret position and refreshes ObjectList, FunctionList,
; and CustomFunctions.
; ================================================================

ReParseText() {
	If (!oCallTip.progHwnd Or !oCallTip.ctlHwnd)
		return
	
	curHwnd := oCallTip.progHwnd
	
	curMon := GetMonitorData()
	
	fontFace := Settings["fontFace"], fontSize := Settings["fontSize"]
	fontColor := Settings["fontColor"], bgColor := Settings["bgColor"]
	
	g := GuiCreate("-Caption AlwaysOnTop +Owner" curHwnd)
	g.SetFont("s" fontSize,fontFace)
	g.BackColor := bgColor
	ctl := g.AddText("+c" fontColor " x5 y5","Loading Objects / Custom Functions...")
	; xVal := curMon.Cx - (g.ClientPos.w/2)
	xVal := curMon.left
	strShow := "y0 x" xVal " NA NoActivate"
	g.Show(strShow)
	
	; dims := g.Pos
	; y := SysGet(79) - dims.h
	; x := SysGet(78) - dims.w
	; g.Show("x" x " y" y " NA NoActivate") ; move to lower right
	
	ReloadElements()
	g.Destroy()
}

ReloadElements() {
	srcFiles := A_ScriptDir "\Languages\" Settings["ActiveLanguage"]
	oCallTip.srcFiles := srcFiles
	cClassNN := oCallTip.ctlClassNN, hEditorWin := oCallTip.progHwnd
	
	Loop Files srcFiles "\*.chm"
		If (A_Index = 1)
			oCallTip.helpFile := A_LoopFileFullPath
	
	LoadKeywordsList()
	LoadFunctionsList()
	
	curDocText := ControlGetText(cClassNN,"ahk_id " hEditorWin)
	curDocArr := StrSplit(curDocText,"`n","`r")
	
	baseFile := Settings["BaseFile"]
	If (FileExist(baseFile)) {
		fileList := GetIncludes()
		Loop fileList.Length
			curDocText2 .= FileRead(fileList[A_Index]) "`r`n`r`n" ; load all includes into one var
		
		If (curDocText2) {
			curDocText := curDocText2, curDocText2 := ""
			curDocArr := StrSplit(curDocText,"`n","`r")
		}
	}
	
	CustomFunctions := GetCustomFunctions(curDocText)
	ClassesList := GetClasses(curDocText)
	ScanClasses(curDocArr)

	objMatchText := LoadMethPropList()
	LoadObjectCreateList(objMatchText)

	ObjectList := CreateObjList(curDocText)
	curDocArr := "", curDocText := ""
}

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
			If (!ctlArr)
				Break
			
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
			found := false
		Else
			ClassNNcomp := ClassNN, found := true, hwndComp := hWnd
	}
	
	If (!found) {
		oCallTip.ctlHwnd := 0
		oCallTip.ctlClassNN := ""
		oCallTip.progHwnd := 0
		oCallTip.progTitle := ""
	} Else {
		ctlHwnd := ControlGetHwnd(ClassNNcomp,"ahk_id " hwndComp)
		oCallTip.ctlHwnd := ctlHwnd
		oCallTip.ctlClassNN := ClassNNcomp
		oCallTip.progHwnd := hwndComp
		oCallTip.progTitle := WinGetTitle("ahk_id " hwndComp)
	}
	; retVal := {ctlClassNN: ClassNNcomp, progHwnd: hwndComp}
	; ctlHwnd := ControlGetHwnd(ClassNNcomp,"ahk_id " hwndComp)
	; retval.progTitle := WinGetTitle("ahk_id " hwndComp)
	; retVal.ctlHwnd := ctlHwnd
	
	; return retVal
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
						oCallTip.classMethodStart := Trim(SubStr(A_LoopField,14))
					Else If (A_Index = 7)
						oCallTip.classMethodEnd := Trim(SubStr(A_LoopField,12))
					Else If (A_Index = 8)
						oCallTip.classMethodOneLine := Trim(SubStr(A_LoopField,16))
					Else If (A_Index = 9)
						oCallTip.classPropertyStart := Trim(SubStr(A_LoopField,16))
					Else If (A_Index = 10)
						oCallTip.classPropertyEnd := Trim(SubStr(A_LoopField,14))
					Else If (A_Index = 11)
						oCallTip.classPropertyOneLine := Trim(SubStr(A_LoopField,18))
					Else If (A_Index = 12)
						oCallTip.classSmallPropExt := Trim(SubStr(A_LoopField,15))
					Else If (A_Index = 13)
						oCallTip.classSmallPropInt := Trim(SubStr(A_LoopField,15))
					Else If (A_Index = 14)
						oCallTip.classInstance := Trim(SubStr(A_LoopField,16))
					Else If (A_Index = 15)
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
	curDocTextNoStr := StringOutline(curDocText)
	classList := Map(), curPos1 := 1
	While (result := RegExMatch(curDocTextNoStr,"mi)" oCallTip.classStart,match,curPos1)) { ; parse classes
		If (IsObject(match) And match.Count()) {
			; If (match.Value(1) != "")
				; className := match.Value(1), extends := ""
			; Else
				; className := match.Value(2), extends := match.Value(3)
			
			className := match.Value(1)
			extends := match.Value(3)
			classShowStyle := Trim(match.Value(4)," `t;")
			; msgbox className " / " extends " / " showStyle
			
			r2 := RegExMatch(curDocTextNoStr,"mi)" oCallTip.classEnd,match2,match.Pos(0)) ; curPos1
			curPos1 := match2.Pos(0) + match2.Len(0)
			classBody := SubStr(curDocText,match.Pos(0),match2.Pos(0)+match2.Len(0)-match.Pos(0))
			classBodyNoStr := StringOutline(classBody)
			
			memberList := Map(), curPos2 := 1
			While (r3 := RegExMatch(classBodyNoStr,"mi)" oCallTip.classMethodStart,match3,curPos2)) { ; parse methods - multi-line
				If (A_Index = 1) {
					startPos := 1 + match.Len(0), endPos := match3.Pos(0)
					init := Trim(SubStr(classBody,startPos,endPos-startPos)," `t`r`n")
					SmallPropExt := oCallTip.classSmallPropExt
					
					Loop Parse init, "`n", "`r"
					{
						curLine := Trim(A_LoopField," `t")
						isStatic := (InStr(curLine,"Static ")) ? true : false
						
						curPos8 := 1
						While (r11 := RegExMatch(curLine,oCallTip.classSmallPropExt,match11,curPos8)) {
							memberName := match11.Value(2)
							showStyle := Trim(match11.Value(3)," `t;")
							
							If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
								memberList[memberName] := Map("name",memberName,"params","","type","property","body","","static",isStatic)
							; msgbox "memName: " memberName " / " showStyle "`r`n`r`n" match11.Value(0)
							
							curPos8 := match11.Pos(1) + match11.Len(1)
						}
					}
				}
				
				isStatic := match3.Value(1) ? true : false
				memberName := match3.Value(2)
				memberParams := match3.Value(3)
				showStyle := Trim(match3.Value(4)," `t;")
				curPos2 := match3.Pos(0)
				
				; msgbox memberName " / " memberParams " / " showStyle
				
				While (r5 := RegExMatch(classBodyNoStr,"mi)" oCallTip.ClassMethodEnd,match5,curPos2)) {
					methBody .= SubStr(classBody,curPos2,match5.Pos(0)+match5.Len(0)-curPos2)
					curPos2 := match5.Pos(0) + match5.Len(0)
					w := StrReplace(StrReplace(methBody,"{","{",lB),"}","}",rB)
					
					If (lB = rB) {
						If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
							memberList[memberName] := Map("name",memberName,"params",memberParams,"type","method","body",Trim(methBody,"`r`n"),"static",isStatic)
						methBodyNoStr := StringOutline(methBody)
						
						curPos6 := 1
						While (r9 := RegExMatch(methBodyNoStr,"mi)" oCallTip.classSmallPropInt,match9,curPos6)) {
							tinyProp := match9.Value(1)
							showStyle := Trim(match9.Value(2)," `t;")
							curPos6 := match9.Pos(1) + match9.Len(1)
							
							If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
								memberList[tinyProp] := Map("name",tinyProp,"params","","type","property","body",match9.Value(0),"static",false)
						}
						
						methBody := ""
						break
					}
				}
			}
			
			curPos3 := 1
			While (r4 := RegExMatch(classBodyNoStr,"mi)" oCallTip.classPropertyStart,match4,curPos3)) {
				isStatic := match4.Value(1) ? true : false
				memberName := match4.Value(2)
				memberParams := match4.Value(3)
				showStyle := Trim(match4.Value(4)," `t;")
				curPos3 := match4.Pos(0)
				
				While (r6 := RegExMatch(classBodyNoStr,"mi)" oCallTip.ClassPropertyEnd,match6,curPos3)) {
					propBody .= SubStr(classBody,curPos3,match6.Pos(0)+match6.Len(0)-curPos3)
					curPos3 := match6.Pos(0) + match6.Len(0)
					w := StrReplace(StrReplace(propBody,"{","{",lB),"}","}",rB)
					
					If (lB = rB) {
						If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
							memberList[memberName] := Map("name",memberName,"params",memberParams,"type","property","body",Trim(propBody,"`r`n"),"static",isStatic)
						propBody := ""
						
						curPos7 := 1
						While (r10 := RegExMatch(methBodyNoStr,"mi)" oCallTip.classSmallPropInt,match10,curPos7)) {
							tinyProp := match10.Value(1)
							showStyle := Trim(match10.Value(2)," `t;")
							curPos7 := match10.Pos(0) + match10.Len(0)
							
							If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
								memberList[tinyProp] := Map("name",tinyProp,"params","","type","property","body",match10.Value(0),"static",false)
						}
						
						break
					}
				}
			}
			
			curPos4 := 1
			While (r7 := RegExMatch(classBodyNoStr,"mi)" oCallTip.classMethodOneLine,match7,curPos4)) {
				isStatic := match7.Value(1) ? true : false
				memberName := match7.Value(2)
				memberParams := match7.Value(3)
				showStyle := Trim(match7.Value(4)," `t;")
				curPos4 := match7.Pos(0) + match7.Len(0)
				
				If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
					memberList[memberName] := Map("name",memberName,"params",memberParams,"type","method","body",Trim(match7.Value(0),"`r`n"),"static",isStatic)
			}
			
			curPos5 := 1
			While (r8 := RegExMatch(classBodyNoStr,"mi)" oCallTip.classPropertyOneLine,match8,curPos5)) {
				isStatic := match8.Value(1) ? true : false
				memberName := match8.Value(2)
				memberParams := match8.Value(3)
				showStyle := Trim(match8.Value(4)," `t;")
				curPos5 := match8.Pos(0) + match8.Len(0)
				
				If ((classShowStyle = "show" And showStyle = "show") Or (!classShowStyle And showStyle != "hide"))
					memberList[memberName] := Map("name",memberName,"params",memberParams,"type","property","body",Trim(match8.Value(0),"`r`n"),"static",isStatic)
			}
			
			if (className != "") {
				obj := Map("type","Class","desc",className,"classBody",Trim(classBody," `t`r`n"),"extends",extends)
				obj["members"] := memberList
				classList[className] := obj
			}
		}
		
		funcBody := "", funcName := "", match := "", params := "", returnObj := ""
	}
	
	curDocText := "", obj := ""
	return classList
}

ScanClasses(curDocArr) { ; scan doc for class instances
	Loop curDocArr.Length {
		curLine := curDocArr[A_Index]
		For className, obj in ClassesList {
			; msgbox className "`r`n`r`n" curLine "`r`n`r`n" oCallTip.classInstance
			If (RegExMatch(curLine,"mi)" StrReplace(oCallTip.classInstance,"{Class}",className),match)) {
				; msgbox match.Value(1) " / " match.Value(2)
				instName := match.Value(1)
				params := match.Value(2)
				ClassesList[instName] := Map("type","Instance","name",instName,"params",params,"class",className)
			}
		}
	}
}

GetIncludes() {
	baseFile := Settings["BaseFile"]
	curDocText := FileRead(baseFile)
	curDocArr := StrSplit(curDocText,"`n","`r")
	
	SplitPath baseFile, , baseFolderP
	curBaseFolder := baseFolderP
	
	includes := oCallTip.includes, includeArr := Array()
	Loop curDocArr.Length {
		curLine := curDocArr[A_Index]
		If (RegExMatch(curLine,"mi)" includes,match))
			includeArr.Push(match.Value(1))
	}
	curDocArr := "" ; free memory
	
	; Loop includeArr.Length
		; firstList .= includeArr[A_Index] "`r`n`r`n"
	; msgbox "baseFolder: " baseFolderP "`r`n`r`n" firstList
	
	FinalArr := Array(), FinalArr.Push(baseFile)
	Loop includeArr.Length {
		curInc := includeArr[A_Index], curInc := Trim(RegExReplace(curInc,"i)(^#Include(Again)?|\*i|" Chr(34) ")",""))
		curInclude := RegExReplace(curInc,"<|>","")
		curInclude := StrReplace(curInclude,"%A_ScriptDir%",baseFolderP)
		
		If (SubStr(curInclude,1,3) = "..\") { ; processing includes starting with ..\
			repInclude := SubStr(curInclude,4)
			baseArr := StrSplit(baseFolderP,"\")
			c := baseArr.Length - 1
			Loop c
				fullPath .= baseArr[A_Index] "\"
			fullPath := Trim(fullPath,"\")
			If (FileExist(fullPath "\" repInclude)) {
				FinalArr.Push(fullPath "\" repInclude)
				continue
			}
		}
		
		stdLib := (curInc = curInclude Or InStr(FileExist(curInclude),"D")) ? false : true
		includeExist := FileExist(curInclude)
		isDir := InStr(includeExist,"D") ? true : false
		isFile := includeExist ? true : false
		
		If (stdLib) { ; check stdLib locations
			f1 := baseFolderP "\Lib\" curInclude, f1 := findFile(f1)
			If (f1) {
				FinalArr.Push(f1)
				continue
			}
			f2 := A_MyDocuments "\AutoHotkey\Lib\" curInclude, f2 := findFile(f2)
			If (f2) {
				FinalArr.Push(f2)
				continue
			}
			f3 := RegRead("HKEY_LOCAL_MACHINE\SOFTWARE\AutoHotkey","InstallDir") "\Lib\" curInclude, f3 := findFile(f3)
			If (f3) {
				FinalArr.Push(f3)
				continue
			}
		}
		
		If (isDir) {
			curBaseFolder := curInclude
			continue
		}
		
		If (isFile) {
			FinalArr.Push(curInclude)
			continue
		}
		
		f4 := findFile(curBaseFolder "\" curInclude)
		If (f4) {
			FinalArr.Push(f4)
			continue
		}
	}
	
	; Loop finalArr.Length
		; finalList .= finalArr[A_Index] "`r`n`r`n"
	; msgbox finalList
	
	return finalArr
}

findFile(sInFile) {
	result := ""
	Loop Files sInFile ".*" ; do this for <libraries> without extension
	{
		If (A_LoopFileName) {
			result := A_LoopFileFullPath
			break
		}
	}
	
	If (!result And FileExist(sInFile)) {
		result := sInFile
		
		; Loop Files sInFile ; do this for libraries including extension
		; {
			; If (A_LoopFileName) {
				; result := A_LoopFileFullPath
				; break
			; }
		; }
	}
	
	return result
}