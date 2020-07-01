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
			KeywordList[funcName] := curObj["type"] ; fill proper type instead of all "function"
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
				; KeywordList[curMem] := curMemType
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
