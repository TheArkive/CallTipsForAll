; Contains functions for parsing the active window, language files, and/or includes
; in order to load data for call tips.

; ================================================================
; Reads current document and caret position and refreshes ObjectList, FunctionList,
; and CustomFunctions.
; ================================================================

ReParseText() {
	If (!oCallTip.progHwnd Or !oCallTip.ctlHwnd)
		return
	
	;show a gui that elements are loaded
	g := Gui.New("-Caption AlwaysOnTop +Owner" oCallTip.progHwnd)
	g.SetFont("s" Settings["fontSize"], Settings["fontFace"])
	g.BackColor := Settings["bgColor"]
	ctl := g.AddText("+c" Settings["fontColor"] " x5 y5", "Loading Objects / Custom Functions...")
	curMon := GetMonitorData()
	g.Show("y0 x" curMon.left " NA NoActivate")
	
	ReloadElements()
	g.Destroy()
}

ReloadElements() {
	DocumentMap := Array() ; this should be reset every reload, even if no baseFile
	srcFiles := oCallTip.srcFiles
	
	Loop Files srcFiles "\*.chm"
		If (A_Index = 1)
			oCallTip.helpFile := A_LoopFileFullPath
	
	curDocText := ControlGetText(oCallTip.ctlHwnd) ;get text from current doc in editor
	
	If (FileExist(Settings["BaseFile"])) { ;or use content of base file and all its includes instead
		pos := 0, tmp := ""
		For i, File in GetIncludes() {
			curFileText := FileRead(File), curFileLen := StrLen(curFileText)
			curFileArr := StrSplit(curFileText,"`n","`r")
			
			For lineNum, lineText in curFileArr { ; create DocumentMap so regex pos can correspond to a file and line number
				lineLen := StrLen(lineText)
				start := (lineLen > 0) ? pos++ : pos
				end := (lineLen > 0) ? pos + (lineLen - 1) : pos
				pos := end ; sync pos
				DocumentMap.Push(Map("fileName",File, "lineNum",lineNum, "start",start, "end",end, "length",lineLen,"text",lineText))
				pos += 2 ; add length of CRLF
			}
			
			tmp .= curFileText "`r`n`r`n" ; load all includes into one var
			pos += 4 ; add length of CRLF x 2
		}
		
		curDocText := tmp ? tmp : curDocText
		; A_Clipboard := curDocText
		
		; For i, obj in DocumentMap ; quick test of DocumentMap
			; testStr .= obj["fileName"] "`r`nLine: " obj["lineNum"] " / " obj["start"] " / " obj["end"] "`r`n`r`n"
		
		; msgbox testStr
	}
	
	oCallTip.docTextNoStr := StringOutline(curDocText) ; this should only be done once per load/reload (for full text + includes)
	oCallTip.docText := curDocText
	
	CustomFunctions := GetCustomFunctions(curDocText)
	ClassesList := GetClasses(curDocText)
	ScanClasses(curDocText)
	ObjectList := CreateObjList(curDocText)
}

; ==================================================
; Determines parent window based on user settings.
; ==================================================
; 
GetEditorHwnd() {
	If (!oCallTip.ctlConfirmed) { ; should only be run at script start, generally... cont
		winList := WinGetList("ahk_exe " Settings["ProgExe"])
		
		Loop winList.Length { ;ZZZ - you didn't askf or this, but this is to filter out the FIND window, or any other child window if active in text editor
			if (oCallTip.ctlConfirmed)
				break ;ZZZ - no need to keep looking after control is found
			
			curHwnd := winList[A_Index]
			ctlArr := WinGetControls("ahk_id " curHwnd)
			If (!ctlArr)
				Break
			
			ClassNNcomp := Settings["ProgClassNN"] . 1
			Loop ctlArr.Length {
				If (ClassNNcomp = ctlArr[A_Index]) {
					ctlHwnd := ControlGetHwnd(ClassNNcomp,"ahk_id " curHwnd)
					oCallTip.ctlHwnd := ctlHwnd
					oCallTip.progHwnd := curHwnd
					oCallTip.progPID := WinGetPID("ahk_id " curHwnd)
					oCallTip.progTitle := WinGetTitle("ahk_id " curHwnd)
					oCallTip.ctlConfirmed := true
					
					Break  ;??? - this will only break the first loop, hence it will find other instances of the editor and will use the last one found.
				} ;ZZZ - i tried addressing a proper loop break above
			}
		}
	} 
	If (!oCallTip.ctlConfirmed And Settings["ProgExe"] And Settings["ProgClassNN"])
		MsgBox "Editor control not found!"
}
  
CheckMouseLocation() {
	MouseGetPos x,y,hWnd, ctlHwndCheck, 2
  
	If (IsObject(SettingsGUI) And SettingsGUI.hwnd = hwnd)
		oCallTip.ctlActive := true
	Else If (IsObject(callTipGui) And callTipGui.hwnd = hwnd)
		oCallTip.ctlActive := true
	Else If (oCallTip.progHwnd != hWnd Or ctlHwndCheck != oCallTip.ctlHwnd)
		oCallTip.ctlActive := false
	Else
		oCallTip.ctlActive := true
	
	If (!oCallTip.ctlActive) {
		IH.Stop()
		SetupInputHook(false)
	}
}

; ==================================================
; keyword list - These are also generated when functions, methods, and properties are
;                enumerated from List_*.txt files.  This will be expanded for an auto
;                complete function/hotkey.  KW_*.txt files are optional, and don't serve
;                much purpose yet.
; ==================================================

LoadKeywordsList() {
	KeywordList := Map() ; , KeywordList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	Loop Files oCallTip.srcFiles "\Keywords\KW_*.txt"
	{
		curText := FileRead(A_LoopFileFullPath)
		Loop Parse curText, "`n", "`r"
			KeywordList[A_LoopField] := "keyword"
	}
	KeywordList.Has("") ? KeywordList.Delete("") : ""
}
; ==================================================
; Create function and command list for call tips / and classes?
; ==================================================

LoadFunctionsList() {
	FunctionList := Map() ; , FunctionList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	Loop Files oCallTip.srcFiles "\Other\List_*.txt" ; functions and commands are combined into one list => FunctionList[]
	{
		a := StrSplit(A_LoopFileName,"_") ; comma (,) separator
		fileName := A_LoopFileFullPath, curFileType := StrReplace(a[2],".txt")
		
		curList := FileRead(fileName)
		curList := Trim(curList,"`r`n") entryEnd

		If (curFileType = "Regex") {
			Loop Parse curList, "`n", "`r"
			{
				Switch A_Index	;AAA - LOL wasn't thinking of that at the time, but yah you are right!
				{                     ;??? this could be further simplified if the lines in the regex file start with the right property name
					case 1:  oCallTip.funcStart            := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 2:  oCallTip.funcEnd              := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 3:  oCallTip.funcReturn           := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 4:  oCallTip.classStart           := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 5:  oCallTip.classEnd             := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 6:  oCallTip.classMethodStart     := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 7:  oCallTip.classMethodEnd       := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 8:  oCallTip.classMethodOneLine   := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 9:  oCallTip.classPropertyStart   := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 10: oCallTip.classPropertyEnd     := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 11: oCallTip.classPropertyOneLine := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 12: oCallTip.classSmallPropExt    := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 13: oCallTip.classSmallPropInt    := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 14: oCallTip.classInstance        := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 15: oCallTip.includes             := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
					case 16: oCallTip.lineComment          := Trim(SubStr(A_LoopField,1+InStr(A_LoopField,":")))
				}
			}
			Continue ;jump to next file
		}

		curPos := 1, subStrEnd := 1
		len := StrLen(curList)

		While (curPos <= len And subStrEnd) {
			subStrEnd := InStr(curList,entryEnd,false,curPos)   ;find position of next entryEnd which is 2 consecutive empty lines
			curLen := subStrEnd - curPos
			curSubStr := SubStr(curList,curPos,curLen)
			curPos := subStrEnd + StrLen(entryEnd)              ;prepare for next iteration
      
			funcArr := StrSplit(curSubStr,Chr(96))

			curObj := Map()
			DescArr := Array()
			For i, funcDef in funcArr
			{
				textblock := Trim(funcDef,"`r`n")
				If (i = 1) {
					tmp := StrSplit(textblock,"`n", "`r")
					curObj["type"] := tmp[1]  ; "function" or "command" ... mostly
					funcName := tmp[2]
				} Else {
					LastBlock := textblock
					(A_Index < funcArr.Length) ? DescArr.Push(LastBlock) : ""
				}
			}		
			curObj["desc"]     := DescArr
			curObj["helpLink"] := LastBlock

			FunctionList[funcName] := curObj
			KeywordList[funcName] := "function"
			curObj := ""
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
	MethPropList := Map() ; , MethPropList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	srcFiles := oCallTip.srcFiles, objMatchText := ""
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
					If (A_Index = 1)
						objListPre := t, objMatchText .= objListPre "`r`n"
					Else If (A_Index > 1 And A_Index < i)
						objDescArr.Push(t)
					Else If (A_Index = i)
						curHelpLink := t
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
; Generates a hierarchical list of object match strings to be executed in a specific order.
; Execution is done by CreateObjList()
; Hierarchy:    List
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
	funcList := Map(), funcList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	curPos1 := 1
	
	While (result := RegExMatch(curDocTextNoStr,"mi)" oCallTip.funcStart,match,curPos1)) {
		funcName := "", bodyText := ""
		If (IsObject(match) And match.Count()) {
			curPos1 := match.Pos(0)
			funcName := match.Value(1)
			; params := RegExReplace(match.Value(2),"`t|`r|`n","")
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

GetIncludes() { ;ZZZ - in general i need to treat libraries properly, as you said they shouldn't have to be #INCLUDED, but...
	baseFile := Settings["BaseFile"] ;ZZZ - this still has to be dependent on having a "BaseFile" defined of course.
	curDocText := FileRead(baseFile) ;ZZZ - So there are several changes that need to happen here, but I'll address the other smaller ones first.
	curDocArr := StrSplit(curDocText,"`n","`r") ;ZZZ - I think i can do more good with fixing the function parser first, that one is more clear in my head.
	
	SplitPath baseFile, , baseFolderP
	curBaseFolder := baseFolderP
	
	includes := oCallTip.includes, includeArr := Array() ;ZZZ - includes regex are not being loaded, gotta check that out now before fixing GetCustomFunctions()
	Loop curDocArr.Length {
		curLine := curDocArr[A_Index]
		If (r1 := RegExMatch(curLine,"mi)" includes,match)) {
			; msgbox "curLine: " curLine "`r`nregex: " includes "`r`ncurMatch: " match.Value(0)
			includeArr.Push(match.Value(1))
		}
	}
	curDocArr := "" ; free memory
	
	FinalArr := Array(), FinalArr.Push(baseFile)
	Loop includeArr.Length {
		curInc := includeArr[A_Index]
		curInc := Trim(RegExReplace(curInc,"i)(^#Include(Again)?|\*i|" Chr(34) ")",""))
		curInclude := RegExReplace(curInc,"<|>","")
		curInclude := StrReplace(curInclude,"%A_ScriptDir%",baseFolderP) ;??? all other built-in variables are not handled, why? I have a small function for it in ahk project manager (ReplaceVars())    ;ZZZ - we should include your function!
		
		includeExist := FileExist(curInclude)  ;??? I'm not sure if this captures a pure/relative dir name , e.g. "#INCLUDE LibV2"
		isDir := InStr(includeExist,"D") ? true : false ;ZZZ - relative dir #INCLUDEs are handled with f4 below.
		isFile := (includeExist And !InStr(includeExist,"D")) ? true : false
		
		If (isDir) {
			curBaseFolder := curInclude
			continue
		}
		
		If (isFile) {
			FinalArr.Push(curInclude)
			continue
		}
		
		f4 := findFile(curBaseFolder "\" curInclude) ;ZZZ - should handle all relative located files.
		If (f4) {
			FinalArr.Push(f4)
			continue
		}
	}
	
	f1 := baseFolderP "\Lib\*" ;??? - simplified handling of 3 lib locations   ;ZZZ - yes finally simplified!
	Loop Files f1
		FinalArr.Push(A_LoopFileFullPath)
	
	f2 := A_MyDocuments "\AutoHotkey\Lib\*"
	Loop Files f2
		FinalArr.Push(A_LoopFileFullPath)
	
	f3 := RegRead("HKEY_LOCAL_MACHINE\SOFTWARE\AutoHotkey","InstallDir") "\Lib\*"
	Loop Files f3
		FinalArr.Push(A_LoopFileFullPath)
	
	return finalArr
}

; ReplaceVars(v, OutFileName, CurrentOutDir){
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
