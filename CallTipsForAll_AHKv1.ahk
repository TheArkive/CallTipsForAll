; === comment out if using this script as a library and these are already determined. ===
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================

FileEncoding, UTF-8

Global callTipGui, curIndex, helpFile, fullDescArr, hCtl, hEditorWin, cClassNN, CallTipHwnd

Global fontFace, fontSize, fontColor, bgColor, srcFiles

Global truncateTextChars, AutoCompleteLength, useTooltip, maxLines
Global loadCallTipOnClick, callTipSelectable
Global closeTipOnLButton, closeTipOnClick, closeTipOnFocusChange

Global curPhrase, curPhraseObj, curPhraseType, parentObj
Global funcBeginStr, funcEndStr
Global ObjectCreateList
Global ObjectList, MethPropList, FunctionList, CustomFunctions, KeywordList

srcFiles := A_ScriptDir "\Languages\AHK1"
Loop Files, %srcFiles%\*.chm
	If (A_Index = 1)
		helpFile := A_LoopFileFullPath

fontFace := "Courier New"
fontSize := 10
fontColor := "Yellow"
bgColor := "202020"

; ==== settings ========================================================================
; Some of these settings don't play well together.  Just think about what you enable.
; For example if you enable loadCallTipOnClick and closeTipOnClick or closeTipOnLButton,
; then the call tip won't load on click.  Tooltips may not follow the closeTipOnLButton,
; or closeTipOnClick settings.
; ======================================================================================

truncateTextChars := 80

AutoCompleteLength := 3			; Auto-Complete won't trigger until X chars have been typed

useTooltip := false				; ignores fontFace, fontSize, and callTipSelectable

maxLines := 20

loadCallTipOnClick := false		; loads call tip on click, if clicked keyword is recognized
callTipSelectable := false		; make call tip text selectable, ignores closeTipOnLButton

closeTipOnLButton := false		; close call tip on L click anywhere
closeTipOnClick := false		; close call tip when clicked on
closeTipOnFocusChange := true	; close call tip when text editor loses focus

; ======================================================================================
; Script Vars
; ======================================================================================

Global entryEnd
entryEnd := "`r`n`r`n`r`n"

; ======================================================================================
; Tray Menu
; ======================================================================================
; Menu, subMenuAutoGen, Add, Generate Commands, iconMenu

Menu, Tray, NoStandard
Menu, Tray, Add, ReWrap Text (CTL + ALT + W), iconMenu
Menu, Tray, Add, UnWrap Text (CTL + ALT + U), iconMenu
; Menu, Tray, Add, AHK Auto-Gen, :subMenuAutoGen
Menu, Tray, Add, Reload, iconMenu
Menu, Tray, Add, Exit, iconMenu

iconMenu(ItemName, ItemPos, MenuObj) {
	If (ItemName = "Generate Commands") {
		AutoCmdFunc()
	} Else If (ItemName = "ReWrap Text (CTL + ALT + W)") {
		clipboard := truncateTxt(truncateTextChars)
	} Else If (ItemName = "UnWrap Text (CTL + ALT + U)") {
		clipboard := unwrapText()
	} Else If (ItemName = "Reload")
		Reload
	Else If (ItemName = "Exit")
		ExitApp
}

truncateTxt(x) {
	If (!StrLen(clipboard))
		return ""
	
	inText := StrReplace(clipboard,"`r`n",Chr(1))
	a := StrSplit(inText,Chr(1))
	
	Loop % a.Length() {
		curLine := a[A_Index]
		
		If (curLine) {
			len := StrLen(curLine), s := 1
			While (s <= len) {
				curChunk := LTrim(SubStr(curLine,s,x))
				
				y := 0
				lastChar := SubStr(curChunk,-1)
				nextChar := SubStr(curLine,s+x)
				
				nectCharStat := false, lastCharStat := false
				If nextChar is not space
					nextCharStat := true
				If lastChar is not space
					lastCharStat := true
				
				If ((nextCharStat) And (lastCharStat) And s <= len) {
					y := 1
					While(lastChar := SubStr(curChunk,y*-1,1)) {
						lastCharStat := false
						If lastChar is not space
							lastCharStat := true
						If (lastCharStat)
							y += 1
						Else
							Break
					}
				}
				
				lastChunk := Trim(SubStr(curLine,s,x-y))			
				newText .= lastChunk "`r`n"
				
				s := s + x - y
			}
		} Else
			newText .= "`r`n"
	}
	
	return Trim(newText,"`r`n")
}

unwrapText() {
	If (!StrLen(clipboard))
		return ""
	
	inText := StrReplace(clipboard,"`r`n`r`n",Chr(1))
	a := StrSplit(inText,Chr(1))
	
	Loop % a.Length()
		newText .= StrReplace(a[A_Index],"`r`n"," ") "`r`n`r`n"
	
	return Trim(newText,"`r`n")
}

; ======================================================================================
; input hook - for processing during user input - i prefer hotkey to invoke call tip.
; ======================================================================================

IH := InputHook("V I1","","") ; options , endKeys , matchList
IH.OnKeyDown := Func("keyPress"), IH.KeyOpt("{All}","N"), IH.Start()

keyPress(iHook, VK, SC) { ; InputHookObject
	curKey := "sc" Format("{:X}",SC)
	
	Try {
		state := GetKeyState(curKey,"P")
	} Catch e {
		msgbox "State: " state "`r`n... is invalid.`r`n`r`n" e
	}
	
	if (state)
		SetTimer, LoadAutoComplete, -500
	
	iHook.Stop()
	IH := InputHook("V I1","","")
	
	IH.OnKeyDown := Func("keyPress"), IH.KeyOpt("{All}","N"), IH.Start()
}

; ==================================================
; keyword list - These are also generated when functions, methods, and properties are
;                enumerated from List_*.txt files.  This will be expanded for an auto
;                complete function/hotkey.  KW_*.txt files are optional, and don't serve
;                much purpose yet.
; ==================================================

KeywordList := Object()
Loop Files srcFiles "Keywords\KW_*.txt"
{
	FileRead, curText, %A_LoopFileFullPath%
	Loop Parse, curText, `r, `n
		KeywordList[A_LoopField] := "keyword"
}

; ==================================================
; Create function and command list for call tips
; ==================================================

FunctionList := Object()

Loop Files, %srcFiles%\Other\List_*.txt ; functions and commands are combined into one list => FunctionList[]
{
	a := StrSplit(A_LoopFileName,"_") ; comma (,) separator
	fileName := A_LoopFileFullPath, curFileType := StrReplace(a[2],".txt")
	version := a.Has(3) ? a[3] : 0
	
	FileRead, curList, %fileName%
	curList := Trim(curList,"`r`n") entryEnd
	curPos := 1, subStrEnd := 1
	len := StrLen(curList)

	While (curPos <= len And subStrEnd) {
		subStrEnd := InStr(curList,entryEnd,false,curPos)
		curLen := subStrEnd - curPos
		curSubStr := SubStr(curList,curPos,curLen)
		
		If (curFileType = "BeginEnd") {
			Loop Parse, curSubStr, `r, `n
			{
				If (A_Index = 1)
					funcBeginStr := Trim(SubStr(A_LoopField,7))
				Else If (A_Index = 2)
					funcEndStr := Trim(SubStr(A_LoopField,5))
			}
		} Else {
			funcName := "", funcDesc := "", bigListVer := ""
			
			Loop Parse, curSubStr, `r, `n
			{
				If (A_Index = 1)
					funcType := A_LoopField
				Else If (A_Index = 2)
					funcName := A_LoopField
				Else If (A_Index = 3)
					funcDesc := A_LoopField
				Else If (A_Index >=	4)
					funcDesc .= "`r`n" A_LoopField
			}
			
			curObj := {}
			curObj["desc"] := funcDesc
			curObj["type"] := funcType ; "function" or "command"
			FunctionList[funcName] := curObj
			KeywordList[funcName] := "function"
		}
		curPos := subStrEnd + StrLen(entryEnd), curObj := ""
	}
}

; msgbox "Begin:   " funcBeginStr "`r`nEnd:   " funcEndStr

; ==================================================
; for debug only
; ==================================================
; For funcName, obj in FunctionList {
	; type := obj["type"]
	; desc := obj["desc"]
	
	; textList .= "Name: " funcName " / " type "`r`n" desc "`r`n`r`n`r`n"
; }
; clipboard := textList
; msgbox % textList

; ==================================================
; generate custom function list
; ==================================================
hCtl := editorCtlHwnd(hEditorWin,eType,cClassNN)
ControlGetText, curDocText, %cClassNN%, ahk_id %hEditorWin%
curDocArr := StrSplit(curDocText,"`n","`r")
CustomFunctions := GetCustomFunctions(curDocArr)

; ==================================================
; Create Object Index by type for call tips
; ==================================================
MethPropList := Object()
Loop Files, %srcFiles%\Objects\*.txt
{
	a := StrSplit(A_LoopFileName,"_")
	fnType := a.HasKey(2) ? a[2] : A_LoopFileName, fnType := StrReplace(fnType,".txt","")
	
	FileRead, curList, %A_LoopFileFullPath%
	curList := Trim(curList,"`r`n") entryEnd
	objList := "", objListPre := "", lineSplit := "", curObjType := ""
	curPos := 1, curHelpLink := ""
	len := StrLen(curList)
	
	curObj := Object(), propList := Object(), methList := Object()

	While (curPos <= len) {
		subStrEnd := InStr(curList,entryEnd,false,curPos)
		curLen := subStrEnd - curPos
		curSubStr := SubStr(curList,curPos,curLen)
		
		curDesc := "", curMemType := "", curMem := ""
		If (A_Index = 1) {
			objMatchArr := StrSplit(curSubStr,Chr(96))
			objListPre := Trim(objMatchArr[1],"`r`n")
			objMatchText .= objListPre "`r`n" ; Trim(objMatchArr[1],"`r`n")
			curHelpLink := objMatchArr.HasKey(2) ? Trim(objMatchArr[2],"`r`n") : ""
			curHelpLink := Trim(curHelpLink,"`r`n")
			
			Loop Parse, objListPre, `r, `n ; create list of defined objTypes
			{
				lineSplit := StrSplit(A_LoopField," "), curObjType := lineSplit[2]
				objList .= curObjType "`r`n"
			}
			Sort, objList, U
			objList := Trim(objList,"`r`n")
		} Else {
			Loop Parse, curSubStr, `r, `n
			{
				If (A_Index = 1)
					curMemType := A_LoopField
				Else If (A_Index = 2)
					curMem := A_LoopField
				Else
					curDesc .= A_LoopField "`r`n"
			}
			
			StringLower, curMemType, curMemType
			KeywordList[curMem] := curMemType
			If (curMemType = "Method")
				methList[curMem] := curDesc
			Else
				propList[curMem] := curDesc
		}
	
		curPos := subStrEnd + StrLen(entryEnd)
	}
	
	curObj["method"] := methList, curObj["property"] := propList
	curObj["helpLink"] := curHelpLink
	
	Loop Parse, objList, `r, `n ; append methods/properties to all defined obj types
		MethPropList[A_LoopField] := curObj
	
	curObj := "", methList := "", propList := ""
}

; ==================================================
; for debug only
; ==================================================
; For objType in MethPropList { ; list obj types
	; testList .= objType "`r`n"
; }
; MsgBox % testList

; ==================================================
; for debug only
; ==================================================
; objType := "Object" ; for debug only
; testObj := MethPropList[objType]
; mList := testObj["method"]

; testList := objType "`r`n`r`n" ; specific object
; For curMeth, curDesc in mList {
	; testList .= "Memeber: " curMeth "`r`n" curDesc "`r`n`r`n"
; }
; MsgBox % testList

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
ObjectCreateList := Object()
Sort, objMatchText, objMatchText
objMatchText := Trim(objMatchText,"`r`n")
curLevel := 0, prevLevel := 0, curLevelObj := Object()

Loop Parse, objMatchText, `r, `n
{
	curObj := Object()
	result := RegExMatch(A_LoopField,"O)([0-9]) ([\w]+) ([\w]+) (.*)",match)
	
	If (IsObject(match) And match.Count() = 4) {
		curLevel := match.Value(1), curObjType := match.Value(2)
		curLabel := match.Value(3), regex := match.Value(4)
		isDirect := InStr(regex,"{") ? 0 : 1 ; reorganize with level sub object
		
		If (curLevel != prevLevel) {
			ObjectCreateList[prevLevel] := curLevelObj, curLevelObj := Object()
		}
		
		curObj := Object()
		curObj["regex"] := regex, curObj["type"] := curObjType, curObj["direct"] := isDirect
		curLevelObj[curLabel] := curObj
		
		prevLevel := curLevel
	} Else {
		msg := "Line:`r`n    " A_LoopField "`r`n`r`nObject File - improper format.`r`n`r`nLine Format:`r`n    Level ObjType Label RegExMatchString"
		MsgBox % msg
		ExitApp
	}
	curObj := ""
}
ObjectCreateList[prevLevel] := curLevelObj, curLevelObj := ""

; msgbox % "ObjectCreateList count: " ObjectCreateList.Count()

; ==================================================
; for debug only
; ==================================================
; For level, lvlObj in ObjectCreateList { ; for debug only
	; For label, labelObj in lvlObj {
		; regex := labelObj["regex"]
		; type := labelObj["type"]
		; direct := labelObj["direct"]
		; testList .= level " / " label " / " type "`r`n" regex "`r`n`r`n"
	; }
; }
; msgbox % testList



; ============================================================
; Parse document and gereate list of object names and other data.
; ============================================================
ObjectList := CreateObjList(curDocArr)
curDocArr := "", curDocText := "" ; these will be recreated on demand

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

; ======================================================================================
; Load Call Tip
; ======================================================================================
LoadCallTip() { ; curPhrase, curPhraseType ---> globals
	If (curPhrase = "")
		return
	
	fullDesc := ""
	phraseRedir := ""
	
	If (curPhraseType = "function")
		phraseRedir := "function"
	Else If (curPhraseType = "CustomFunction")
		phraseRedir := "function"
	Else if (curPhraseType = "Flow")
		phraseRedir := "function"
	Else If (curPhraseType = "Command")
		phraseRedir := "function"
	
	if (phraseRedir = "function") {
		For funcName, obj in FunctionList {
			If (funcName = curPhrase) {
				fullDesc := obj["desc"]
				break
			}
		}
		
		If (!fullDesc) {
			For funcName, obj in CustomFunctions {
				If (funcName = curPhrase) {
					fullDesc := obj["desc"]
					break
				}
			}
		}
		
		If (!fullDesc)
			return
	} Else If (curPhraseType = "object") {
		obj := ObjectList.HasKey(curPhrase) ? ObjectList[curPhrase] : ""
		If (!obj)
			return
		
		objType := obj["type"]
		obj := MethPropList.HasKey(objType) ? MethPropList[objType] : ""
		If (IsObject(obj))
			methObj := obj["method"], propObj := obj["property"]
		
		methTitle := "Methods: "
		For methName in methObj {
			If (!Mod(A_Index,8))
				methList .= "." methName "`r`n"
			Else
				methList .= "." methName ", "
		}
		methList := Trim(methList," ,`r`n")
		If (methList)
			methList := methTitle methList
		
		propTitle := "Properties: "
		For propName in propObj {
			If (!Mod(A_Index,8))
				propList .= "." propName "`r`n"
			Else
				propList .= "." propName ", "
		}
		propList := Trim(propList," ,`r`n")
		If (propList)
			propList := propTitle propList
		
		fullDesc := Trim(objType "`r`n`r`n" methList,"`r`n")
		fullDesc := Trim(fullDesc "`r`n`r`n" propList,"`r`n")
	} Else If (curPhraseType = "method" Or curPhraseType = "property") {
		obj := ObjectList.HasKey(parentObj) ? ObjectList[parentObj] : ""
		If (!obj)
			return
		
		objType := obj["type"]
		obj := MethPropList[objType]
		methObj := obj[curPhraseType]
		
		StringUpper, curPhraseType, curPhraseType, T
		curText := ""
		For methPropName, desc in methObj {
			
			If (methPropName = curPhrase) {
				curMethPropName := methPropName
				curText := methObj[methPropName]
				Break
			}
		}
		
		If (curText)
			fullDesc := objType " " titleHeader ":`r`n`r`n" curText
		fullDesc := StrReplace(fullDesc,"[MemberName]",curMethPropName)
	} Else ; unrecognized keyword, so return
		return
	
	If (!curIndex) {
		fullDescArr := StrSplit(fullDesc,Chr(96))
		fullDesc := fullDescArr[1]
	} Else {
		If (curIndex = fullDescArr.Length())
			return
	}
	
	fullDesc := StrReplace(fullDesc,"`r`n/","`r`n`r`n")
	fullDesc := StrReplace(fullDesc,"''","``")
	fullDesc := Trim(fullDesc," `t`r`n")
	Loop Parse, fullDesc, `r, `n ; count lines in desc
		tL := A_Index
	
	If (tL > maxLines) ; set max lines
		tL := maxLines, vscroll := ""
	Else
		vscroll := "-VScroll"
	
	If (!fullDesc)
		return
	Else
		dims := GetMsgDimensions(fullDesc,fontFace,fontSize)
	
	CoordMode Caret, Screen
	outX := A_CaretX, outY := A_CaretY
	
	If (useTooltip) {
		outX += -70, outY += -25
		Tooltip %fullDesc%, %outX%, %outY%
	} Else {
		outX += -11, outY += 20
		ctlHwnd := editorCtlHwnd(hEditorWin,eType,cClassNN)
		
		Gui, CallTip:New, -Border AlwaysOnTop +Owner%hEditorWin% +HwndCallTipHwnd
		Gui, Font, s%fontSize%, %fontFace%
		
		If (!callTipSelectable) {
			Gui, CallTip:Add, Text, +c%fontColor% ggui_click r%tL% x5 y5 AltSubmit, %fullDesc%
			GuiControlGet, dCtl, Pos, Static1
			Gui, Color, %bgColor%
		} Else {
			Gui, CallTip:Add, Edit, +c%fontColor% r%tL% %vscroll% -E0x200 x5 y5 ReadOnly, %fullDesc%
			GuiControlGet, dCtl, Pos, Edit1
			Gui, Color, %bgColor%
		}
		
		posTest := OutX + dCtlW
		
		SysGet, temp1, 78
		If (posTest > temp1) {
			offset := posTest - temp1
			OutX := OutX - offset - 20
		}
		
		h := dCtlH + 10, w := dCtlW + 10
		Gui, CallTip:Show, x%OutX% y%outY% h%h% w%w% NA NoActivate
	}
}

gui_click(CtrlHwnd, GuiEvent, EventInfo) {
	If (closeTipOnClick) {
		Gui, CallTip:Destroy
		callTipGui := "", curIndex := "", fullDescArr := "", CallTipHwnd := ""
	} Else {
		If (curPhraseType != "object") {
			link := Trim(fullDescArr[fullDescArr.Length()],"`r`n")
		} Else {
			obj := ObjectList.HasKey(curPhrase) ? ObjectList[curPhrase] : ""
			If (IsObject(obj)) {
				objType := obj["type"], obj := MethPropList[objType]
				link := obj["helpLink"]
			}
		}
		
		If (link And FileExist(helpFile)) {
			helpFile := StrReplace(helpFile," ","%20")
			cmd := "hh.exe mk:@MSITStore:" helpFile "::" Trim(link,"`r`n")
			Run, %cmd%
			Gui, CallTip:Destroy
			callTipGui := "", curIndex := "", fullDescArr := "", CallTipHwnd := ""
		}
	}
}

; ======================================================================================
; Load auto-complete
; ======================================================================================
LoadAutoComplete() {
	ProcInput() ; update cursor postion and curPhrase in global variables
	
	If (StrLen(curPhrase) >= AutoCompleteLength) { ; not done yet!
	
	}
}


; ================================================================
; Reads current line / cursor position and determines current top-level function.
; Not currently used in any meaningful way.
; ================================================================
GetTopLevelFunc(sInput, curCol, ByRef funcStart, ByRef funcEnd) {
	result := RegExMatch(sInput,"O)([\w\.]+\(.*?\))",match)
	curFunc := "", foundIt := 0
	
	Loop { ; isolate top-level function
		If (IsObject(match)) {
			curMatch := match.value(1), curFunc := curMatch
			curLen := match.Len(1), funcStart := match.pos(1)
			
			w := StrReplace(curMatch,"(","(",LPar), w := StrReplace(curMatch,")",")",RPar)
			
			i := 1
			While (RPar != LPar) {
				curFunc := SubStr(sInput,funcStart,curLen + i)
				w := StrReplace(curFunc,"(","(",LPar), w := StrReplace(curFunc,")",")",RPar)
				i++
			}
			
			funcEnd := funcStart + StrLen(curFunc) - 1
			
			If (curCol >= funcStart And curCol <= funcEnd) {
				foundIt := 1
				break
			}
			
			result := RegExMatch(sInput,"O)([\w\.]+\(.*?\))",match,funcStart + StrLen(curFunc) - 1)
			if (!result)
				break
		} Else
			break
	}
	
	If (foundIt)
		return curFunc
	Else
		return ""
}

; ================================================================
; Replaces nested functions with ~~~~ ... meant to avoid confusion when using regex.
; sInput is usually a single one-line function.
; Not currently in use.
; ================================================================
FuncParamOutline(sInput,ByRef funcName) {
	funcName := RegExMatch(sInput,"O)^([\w\.]*)",match), funcName := match.Value(1)
	paramStrStart := StrLen(funcName) + 2, paramStr := SubStr(sInput,paramStrStart,-1)
	result := RegExMatch(paramStr,"O)([\w\.]+\(.*?\))",match) ; similar to above
	
	Loop { ; blank out expressions in top-level function parameters
		If (IsObject(match)) {
			curMatch := match.value(1), curChunk := curMatch
			curLen := match.Len(1), exprStart := match.pos(1)
			
			w := StrReplace(curMatch,"(","(",LPar), w := StrReplace(curMatch,")",")",RPar)
			
			i := 1
			While (RPar != LPar) {
				curChunk := SubStr(paramStr,exprStart,curLen + i), newCurLen := curLen + i
				w := StrReplace(curChunk,"(","(",LPar), w := StrReplace(curChunk,")",")",RPar)
				i++
			}
			
			repStr := ""
			Loop newCurLen
				repStr .= "~"
			
			paramStr := StrReplace(paramStr,curChunk,repStr,,1)
			result := RegExMatch(paramStr,"O)([\w\.]+\(.*?\))",match)
			if (!result)
				break
		} Else
			break
	}
	
	return paramStr
}

; ================================================================
; Not currently used.
; Can be used to map out the parameters within a function, and to determine the
; caret position's current parameter.  Needs work, processing nested functions
; doesn't happen yet.
; ================================================================
paramData(lineText,curCol,funcName,funcStart,paramStr) {
	curParamNum := 0
	paramList := Object()
	paramArr := StrSplit(paramStr,Chr(44))
	paramBaseLine := funcStart + StrLen(funcName) + 1
	paramStart := paramBaseLine
	
	Loop paramArr.Length() {
		curParam := paramArr[A_Index], curParamLen := StrLen(curParam)
		curParamText := SubStr(lineText,paramStart,curParamLen)
		
		paramEnd := paramStart + curParamLen - 1
		If (curCol >= paramStart And curCol <= paramEnd)
			curParamNum := A_Index
		
		paramObj := Object()
		paramObj["text"] := curParamText
		paramObj["start"] := paramStart
		paramObj["end"] := paramStart + curParamLen - 1
		paramList["n" A_Index] := paramObj
		paramList["total"] := A_Index
		paramList["current"] := curParamNum
		
		paramStart += curParamLen + 1
	}
	paramObj := ""
	
	return paramList
}

; ================================================================
; Replaces a "strings" in a single line (usually the current line) with *******.
; Helps with determining function boundaries and creating object lists.  This also
; assists the script in identifying elements properly, since RegEx is used.
; ================================================================
StringOutline(sInput) {
	curLineNoStr := sInput
	While (result := RegExMatch(curLineNoStr,"O)(" Chr(34) ".*?" Chr(34) ")",match)) {
		repStr := ""
		If (IsObject(match)) {
			Loop % match.Len(1)
				repStr .= "*"
			curLineNoStr := StrReplace(curLineNoStr,match.Value(1),repStr,,1)
			match := ""
		}
	}
	
	return curLineNoStr
}

; ================================================================
; generates the current "phrase.Obj" with dots (.) where the caret is.
; ================================================================
getCurPhraseObj(curLineNoStr,curCol,ByRef curPhraseStartOut) {
	Lhalf := SubStr(curLineNoStr,1,curCol-1), Rhalf := SubStr(curLineNoStr,curCol) ; split line at curCol
	p1 := RegExMatch(Lhalf,"O)([#?\w\.]+)$",match), p1 := IsObject(match) ? Trim(match.Value(1)," `t") : ""
	p2 := RegExMatch(Rhalf,"O)^(#?[\w\.]+)",match2), p2 := IsObject(match2) ? Trim(match2.Value(1)," `t") : ""
	curPhraseOut := p1 p2 ? p1 p2 : ""
	curPhraseStartOut := (p1 or p2) ? (p1 ? match.Pos(1) : curCol) : 0
	return curPhraseOut
}

; ================================================================
; Generates the current "phrase" without dots (.)
; ================================================================
getCurPhrase(curLineNoStr, curCol, ByRef curPhraseStartOut) {
	Lhalf := SubStr(curLineNoStr,1,curCol-1), Rhalf := SubStr(curLineNoStr,curCol) ; split line at curCol
	p1 := RegExMatch(Lhalf,"O)(#?[\w]+)$",match), p1 := IsObject(match) ? Trim(match.Value(1)," `t") : ""
	p2 := RegExMatch(Rhalf,"O)^(#?[\w]+)",match2), p2 := IsObject(match2) ? Trim(match2.Value(1)," `t") : ""
	curPhraseOut := p1 p2 ? p1 p2 : ""
	curPhraseStartOut := (p1 or p2) ? (p1 ? match.Pos(1) : curCol) : 0
	return curPhraseOut
}

; ================================================================
; retrieves the parent object from phraseObj.  Usually phraseObj comes from getCurPhraseObj().
; ================================================================
GetParentObj(phraseObj, ByRef methProp, funcName := "", curTopFunc := "") {
	If (!phraseObj)
		return ""
	
	aStr := StrSplit(phraseObj,".")
	Loop % aStr.Length() {
		curBit := aStr[A_Index]
		fullPhrase .= curBit "."
		If (curBit = curPhrase)
			Break
	}
	fullPhrase := Trim(fullPhrase,".")
	
	If (!phraseObj And !funcName)
		return ""
	
	aStr := StrSplit(fullPhrase,".")
	If (!IsObject(aStr) Or aStr.Length() = 0)
		return ""
	
	pOn := aStr.Length() - 1
	sObj := aStr.HasKey(pOn) ? aStr[pOn] : ""
	
	If (ObjectList.HasKey(sObj))
		return sObj
	Else If (ObjectList.HasKey(curPhrase))
		return curPhrase
	Else
		return ""
}

; ================================================================
; Generates a list of objects in the document as defined by the user.
; This will NOT do class objects (yet).
; If an object in the code document is assigned to a member of an object,
; and that member is a keyword (built-in method or property) then that entry
; will not be added to this list.
; ================================================================
CreateObjList(curDocArr) {
	oList := Object()
	
	For level, lvlObj in ObjectCreateList {
		For label, lblObj in lvlObj {
			i := 1, type := lblObj["type"], regex := lblObj["regex"], direct := lblObj["direct"]
			
			count1 := curDocArr.Length()
			Loop %count1% {
				curLine := curDocArr[A_Index], i := A_Index
				curLine := StringOutline(curLine)
				curPos := 1
				
				If (direct) {
					While (result := RegExMatch(curLine,"O)" regex,match,curPos)) {
						
						c := match.Count()
						If (IsObject(match) And c >= 1) {
							obj := Object(), objName := match.Value(1)
							objNameArr := StrSplit(objName,".")
							If (objNameArr.Length() > 1)
								objName := objNameArr[2]
							
							objMatch := (c = 2) ? match.Value(2) : match.Value(1)
							
							obj["type"] := type
							obj["label"] := label
							obj["line"] := i
							obj["match"] := objMatch
							
							quit := false
							If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
								quit := true
							
							If (!oList.HasKey(objName) And !quit)
								oList[objName] := obj
							
							curPos := match.Pos(c) + match.Len(c)
						} Else
							curPos := StrLen(curLine)
					}
				} Else { ; perform {substitution} before doing regex match and adding to ObjectList
					r1 := RegExMatch(regex,"O)\{(.*?)\}",match), listType := match.Value(1)
					
					For curObjName, curLblObj in oList {
						curType := curLblObj["type"]
						If (curType = listType) {
							newRegex := StrReplace(regex,"{" listType "}",curObjName)
							
							While (result := RegExMatch(curLine,"O)" newRegex,match,curPos)) {
								c := match.Count()
								If (IsObject(match) And c = 2) {
									obj := Object(), objName := match.Value(1)
									objNameArr := StrSplit(objName,".")
									If (objNameArr.Length() > 1)
										objName := objNameArr[2]
								
									objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
									obj["type"] := type
									obj["label"] := label
									obj["line"] := i
									obj["match"] := objMatch
									
									quit := false
									If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
										quit := true
									
									If (!oList.HasKey(objName))
										oList[objName] := obj
									
									curPos := match.Pos(c) + match.Len(c)
								} Else
									curPos := StrLen(curLine)
							} ; end while
						}
					} ; end for
				}
			} ; end loop
		} ; end for
	} ; end for
	
	Loop % curDocArr.Length() { ; parse document one more time for custom functions returning objects
		curLine := curDocArr[A_Index], i := A_Index
		curLine := StringOutline(curLine)
		
		For custFuncName, curLblObj in CustomFunctions {
			newRegex := "^[ \t]*([\w\.]+)[ \t]*:=[ \t]*({CustomFunction}\x28)"
			newRegex := StrReplace(newRegex,"{CustomFunction}",custFuncName)
			curRetStr := "", curObjType := ""
			
			If (curLblObj.HasKey("return")) {
				returnObj := curLblObj["return"]
				For retStr, L in returnObj { ; loop through return entries in Func
					For curObjName, curLblObj in oList { ; loop through oList to find objMatch
						If (retStr = curObjName) {
							curRetStr := retStr ; return var in matched function
							curObjType := curLblObj["type"] ; obj type to save
							Break ; break on match
						}
					} ; end for
					
					If (curRetStr)
						Break
				} ; end for
			}
			
			curPos := 1
			While (result := RegExMatch(curLine,"O)" newRegex,match,curPos)) {
				c := match.Count()
				If (IsObject(match) And match.Count() = 2 and curRetStr) { ; if match add obj to list
					obj := Object(), objName := match.Value(1)
					objNameArr := StrSplit(objName,".")
					If (objNameArr.Length() > 1)
						objName := objNameArr[2]
					
					objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
					obj["type"] := curObjType
					obj["label"] := "objByFunc"
					obj["line"] := i
					obj["match"] := objMatch
					
					quit := false
					If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
						quit := true
					
					If (!oList.HasKey(objName))
						oList[objName] := obj
					
					curPos := match.Pos(c) + match.Len(c)
				} Else
					curPos := StrLen(curLine)
			} ; end while
		} ; end for
	} ; end loop
	
	return oList
}

; ================================================================
; Creates a list of user defined functions.  The call tip only shows FuncName(params...) and
; it is not currently possible to add extra help in the call tip for custom functions.
; ================================================================
GetCustomFunctions(curDocArr) {
	funcList := Object()
	Loop % curDocArr.Length() {
		curDocLine := curDocArr[A_Index]
		
		result := RegExMatch(curDocLine,"O)" funcBeginStr,match) ; funcBeginStr is defined in List_Functions.txt
		If (IsObject(match) And match.Count()) {
			funcName := match.Value(1)
			params := match.Value(2)
			
			if (funcName != "") {
				funcBody := curDocLine
				obj := Object()
				obj["type"] := "CustomFunction"
				obj["desc"] := funcName params
				obj["line"] := A_Index
				funcList[funcName] := obj
			}
		} Else If (funcBody)
			funcBody .= "`r`n" curDocLine
		
		If (RegExMatch(curDocLine,"O)" funcEndStr) And funcName) { ; funcEndStr is defined in List_Functions.txt
			obj["funcBody"] := funcBody
			
			returnObj := Object()
			funcBodyArr := StrSplit(funcBody,"`r","`n")
			Loop % funcBodyArr.Length() { ; compile function body and search for "return" lines
				fLine := funcBodyArr[A_Index]
				If (RegExMatch(fLine,"O)return[ \t]+([\w]+)",match)) {
					If (match.Count()) {
						cMatch := match.Value(1)
						returnObj[cMatch] := match.Pos(1)
					}
				}
			}
			
			If (returnObj.Count() > 0)
				obj["return"] := returnObj ; attach returnObj list of "return" lines if exist
			
			funcList[funcName] := obj
			funcBody := "", funcName := "", match := "", params := "", returnObj := ""
		}
	}
	
	curDocArr := "", obj := ""
	return funcList
}

; ================================================================
; Reads current document and caret position and refreshes ObjectList, FunctionList,
; and CustomFunctions.
; ================================================================
ProcInput() {
	curPhrase := "", parentObjType := "", curPhraseType := ""
	hCtl := editorCtlHwnd(hEditorWin,eType,cClassNN)
	
	If (!hCtl) ; if supported text editor inactive, return
		return
	
	ControlGetText, curDocText, %cClassNN%, ahk_id %hEditorWin%
	If (!curDocText)
		return
	
	curDocArr := StrSplit(curDocText,"`n","`r")
	CustomFunctions := GetCustomFunctions(curDocArr)
	ObjectList := CreateObjList(curDocArr)
	
	ControlGet, curCol, CurrentCol,,, ahk_id %hCtl%
	ControlGet, curLine, CurrentLine,,, ahk_id %hCtl%
	
	If (!curLine)
		return
	If (!curDocArr.HasKey(curLine))
		return
	
	curLineText := curDocArr[curLine]
	
	curLineNoStr := StringOutline(curLineText) ; blank out strings with "****"
	curPhrase := getCurPhrase(curLineNoStr,curCol,curPhraseStart) ; curPhraseStart: ByRef
	curPhraseObj := getCurPhraseObj(curLineNoStr,curCol,curPhraseObjStart)
	parentObj := GetParentObj(curPhraseObj,curMethProp)
	parentObjType := (parentObj And ObjectList.HasKey(parentObj)) ? ObjectList[parentObj]["type"] : ""
	
	; topFunc := GetTopLevelFunc(curLineNoStr,curCol,funcStart,funcEnd) ; funcStart, funcEnd: ByRef - not currently used
	; funcText := topFunc ? SubStr(curLineText,funcStart,StrLen(topFunc)) : ""
	
	; === primary data bits ===
	; curPhrase, curPhraseStart, funcName
	; curPhraseObj, curPhraseObjStart, parentObj, parentObjType
	; curParamNum, curParamText
	; arrays: CustomFunctions, ObjectList, MethPropList, paramList
	
	For curFuncName, obj in FunctionList {
		If (curFuncName = curPhrase) {
			curPhraseType := obj["type"]
			Break
		}
	}
	
	If (!curPhraseType) {
		For curFuncName, obj in CustomFunctions {
			If (curFuncName = curPhrase) {
				curPhraseType := obj["type"]
				Break
			}
		}
	}
	
	If (!curPhraseType) {
		For curObjName in ObjectList {
			If (curObjName = curPhrase) {
				curPhraseType := "object"
				Break
			}
		}
	}
	
	If (!curPhraseType And MethPropList.HasKey(parentObjType)) {
		methList := MethPropList[parentObjType]["method"]
		For methName in methList {
			If (methName = curPhrase) {
				curPhraseType := "method"
				Break
			}
		}
	}
	
	If (!curPhraseType And MethPropList.HasKey(parentObjType)) {
		propList := MethPropList[parentObjType]["property"]
		For propName in propList {
			If (propName = curPhrase) {
				curPhraseType := "property"
				Break
			}
		}
	}
	
	; If (curPhrase) { ; for debugging while allowing user input - requires InputHook up top
		; x := A_CaretX, y := A_CaretY
		; Tooltip % "curPhrase :" curPhrase ": / curPhraseObj: " curPhraseObj " / funcName: " funcName " / curMethProp: " curMethProp "`r`nparentObj: " parentObj " / parentObjType: " parentObjType "`r`nphraseType: " curPhraseType "`r`nfuncText: " funcText, x, (y+30)
	; } Else
		; ToolTip
}

editorCtlHwnd(ByRef eHwnd, ByRef cType, ByRef classNN) {
	cType := "", eHwnd := "", classNN := "", hSci := 0
	If (eHwnd := WinActive("ahk_exe notepad.exe")) {
		ControlGet, hSci, Hwnd,, Edit1, ahk_class Notepad ; hSci
		cType := "edit", classNN := "Edit1"
	} Else If (eHwnd := WinActive("ahk_exe notepad++.exe")) {
		ControlGet, hSci, Hwnd,, Scintilla1, ahk_exe notepad++.exe ; hSci
		cType := "scintilla", classNN := "Scintilla1"
	}
	
	return hSci
}

; ===========================================
; posted by Algumist - gets line text from scintilla control
; no longer needed
; https://www.autohotkey.com/boards/viewtopic.php?p=241297#p241297
; ============================================

; SCI_LINELENGTH(Line,hSci) {
	; len := hSci ? DllCall("SendMessage", "Ptr", hSci, "UInt", 2350 , "Int", Line-1, "Int", 0) : 0
	; return len
; }

; SCI_GETLINE(Line,hSci) {
	; If !Line
		; Return ""
	
    ; PID := WinGetPID("ahk_id " hSci)
    ; If !(hProc := DllCall("OpenProcess", "UInt", 0x438, "Int", False, "UInt", PID, "Ptr"))
        ; Return

    ; Len := SCI_LINELENGTH(Line,hSci)
    ; Address := DllCall("VirtualAllocEx", "Ptr", hProc, "Ptr", 0, "UPtr", Len, "UInt", 0x1000, "UInt", 4, "Ptr")

    ; Len := DllCall("SendMessage", "Ptr", hSci, "UInt", 2153 , "Int", Line-1, "Ptr", Address) ; orig = Line - 1
    ; VarSetCapacity(LineText, Len, 0)
    ; DllCall("ReadProcessMemory", "Ptr", hProc, "Ptr", Address, "Ptr", &LineText, "UPtr", Len, "Ptr", 0)
    ; LineText := StrGet(&LineText, "UTF-8")

    ; DllCall("VirtualFreeEx", "Ptr", hProc, "Ptr", Address, "UPtr", 0, "UInt", 0x8000) ; MEM_RELEASE
    ; DllCall("CloseHandle", "Ptr", hProc)

    ; Return LineText
; }

; ================================================================
; scan help docs - auto generate
; ================================================================

AutoCmdFunc() {
	FileSelectFolder, curFolder,,,Select COMMANDS folder:
	If (curFolder = "")
		return
	
	Loop Files curFolder "\*.htm"
	{
		curFile := A_LoopFileName, doSkip := false, multiSyntax := false, multiSyntaxArr := Array()
		If (curFile = "Block.htm" Or curFile = "GuiControls.htm")
			doSkip := true
		If (curFile = "ListView.htm" Or curFile = "TreeView.htm")
			doSkip := true
		If (curFile = "Math.htm")
			multiSyntax := true
		
		If (!doSkip) {
			curTitle := "" curSyntax := "", fullSyntax := "", match := ""
			FileRead, curText, %A_LoopFileFullPath%
			curType := "Command"
			
			curTitleMatch1 := "O)<h1>(.*?)</h1>"
			result := RegExMatch(curText,curTitleMatch1,match)
			If (IsObject(match) And match.Count()) {
				curTitle := match.Value(1)
				If (curTitle = "{...} (block)")
					curTitle := "Block"
			}
			curTitle := Trim(RegExReplace(curTitle,"<span class=" Chr(34) "ver" Chr(34) ">.*?</span>",""))
			curTitle := StrReplace(curTitle,"("), curTitle := StrReplace(curTitle,")")
			
			curMatch1 := "Os)<pre class=" Chr(34) "Syntax" Chr(34) "[^>]*>(.*?)\Q</pre>\E", curPos := 1
			While (result := RegExMatch(curText,curMatch1,match,curPos)) {
				If (IsObject(match) And match.Count()) {
					curSyntax := match.Value(1)
					curSyntax := RegExReplace(curSyntax,"s)(<span class=" Chr(34) "optional" Chr(34) "[^>]*>)(.*?)(\Q</span>\E)","[$2]")
					curSyntax := RegExReplace(curSyntax,"s)<span class=" Chr(34) "func" Chr(34) "[^>]*>(.*?)\Q</span>\E","$1")
					curSyntax := RegExReplace(curSyntax,"s)<i>(.*?)\Q</i>\E","$1")
					curSyntax := RegExReplace(curSyntax,"s)<a href[^>]*>(.*?)\Q</a>\E","$1")
					curSyntax := RegExReplace(curSyntax,"<span class=" Chr(34) "ver" Chr(34) ">.*?\Q</span>\E","")
					curSyntax := RegExReplace(curSyntax,"<em>.*?\Q</em>\E","")
					
					If (!multiSyntax)
						fullSyntax .= Trim(curSyntax,"`r`n") "`r`n"
					Else
						multiSyntaxArr.Push(curSyntax)
					
					curPos := match.Pos(1) + match.Len(1)
				}
			}
			
			If (curTitle and (fullSyntax Or multiSyntaxArr.Length())) {
				curTitle := RegExReplace(curTitle,"<em>(.*?)\Q</em>\E","$1")
				curTitleArr := StrSplit(curTitle,"/")
				curHelpLink := "/docs/commands/" A_LoopFileName
				
				If (!multiSyntaxArr.Length()) {
					If (InStr(fullSyntax,"(") And InStr(fullSyntax,")"))
						curType := "Function"
					Loop curTitleArr.Length() {
						curEntry := curType "`r`n" Trim(curTitleArr[A_Index]) "`r`n" Trim(fullSyntax,"`r`n") "`r`n" Chr(96) "`r`n" curHelpLink
						fullList .= curEntry "`r`n`r`n`r`n"
					}
				} Else {
					Loop multiSyntaxArr.Length() {
						curLine := multiSyntaxArr[A_Index]
						If (InStr(curLine,"(") And InStr(curLine,")"))
							curType := "Function"
						result := RegExMatch(curLine,"[\w]+ := ([\w]+)\(",match)
						If (IsObject(match) And match.Count()) {
							curTitle := match.Value(1)
							curEntry := curType "`r`n" Trim(curTitle) "`r`n" Trim(curLine,"`r`n") "`r`n" Chr(96) "`r`n" curHelpLink "#" curTitle
							fullList .= curEntry "`r`n`r`n`r`n"
						}
					}
				}
			}
		}
	}
	
	destFile := A_Desktop "\List_Commands.txt"
	FileDelete %destFile%
	FileAppend %fullList%, %destFile%
	MsgBox % "Commands list complete."
	Run % "notepad.exe " Chr(34) destFile Chr(34)
}



; ======================================================================================
; get text dimensions given a font face and size, also get line height
; all dimensions and line height are measured in pixels
; ======================================================================================
GetMsgDimensions(sString,sFaceName,nHeight,maxW := 0) {
	Loop Parse, sString, `n, `r
	{
		line1 := A_LoopField
		break
	}
	
	;;;;;;; ==================================================== get line height
	Gui, New
	Gui, Font, s%nHeight%, %sFaceName%
	Gui, Add, Text, , %line1%
	GuiControlGet, curCtl, Pos, Static1
	
	lineH := curCtlH
	Gui, Destroy
	;;;;;;; ====================================================
	
	maxH := 0 ; maxW is already set
	fullLen := StrLen(sString)
	curPos := 1, len := 65535
	
	While (curPos <= fullLen) {
		tinyStr := SubStr(sString,curPos,len)
		
		Gui, New
		Gui, Font, s%nHeight%, %sFaceName%
		If (!maxW)
			Gui, Add, Text,, %tinyStr% ; curCtl
		Else
			Gui, Add, Text, w%maxW%, %tinyStr% ; curCtl
		GuiControlGet, curCtl, Pos, Static1
		
		ctlSize := {}
		ctlSize.w := curCtlW, ctlSize.h := curCtlH
		ctlSize.x := curCtlX, ctlSize.y := curCtlY
		If (!maxW)
			maxW := ctlSize.w > maxW ? ctlSize.w : maxW
		
		maxH += ctlSize.h
		Gui, Destroy
		
		curPos += len
	}
	
	retVal := {}
	retVal.w := maxW, retVal.h := maxH, retVal.lineHeight := lineH, retVal.x := ctlSize.x, retVal.y := ctlSize.y
	
	guiObj := "", curCtl := "", ctlSize := ""
	return retVal
}


; ================================================================
; hotkeys - global
; ================================================================
^+Space:: ; load call tip
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		ProcInput()
		LoadCallTip()
	}
return

^!Space:: ; load auto-complete list
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		msgbox "yee haw!"
	}
return

^+F12:: ; close CallTipsForAll
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		MsgBox "Closing Call Tips For All!"
		ExitApp
	}
Return

~ESC:: ; close call tip window
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		
	}
	
	If (useToolTip)
		Tooltip
	Else {
		Gui, CallTip:Destroy
		callTipGui := "", curIndex := "", fullDescArr := "", CallTipHwnd := ""
	}
return

Up::
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		If (curIndex) {
			If (curIndex > 1) {
				curIndex--
				Gui, CallTip:Destroy
				callTipGui := ""
				LoadCallTip()
			} Else SendInput {Up}
		} Else
			SendInput {Up}
	} Else SendInput {Up}
return

Down::
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		If (curIndex) {
			If (curIndex = 1 And fullDescArr.Length > 2) {
				curIndex++
				Gui, CallTip:Destroy
				callTipGui := ""
				LoadCallTip()
			} Else SendInput {Down}
		} Else
			SendInput {Down}
	} Else SendInput {Down}
return

~LButton::
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		hCtl := editorCtlHwnd(hEditorWin,cType,cClassNN)
		MouseGetPos,,,winHwnd
		
		If (loadCallTipOnClick And (winHwnd != CallTipHwnd)) {
			If (useToolTip)
				ToolTip
			Else {
				Gui, CallTip:Destroy
				callTipGui := "", curIndex := "", fullDescArr := "", CallTipHwnd := ""
			}
			
			ProcInput()
			LoadCallTip()
		}
	} Else If (closeTipOnFocusChange) { ; close call tip when text editor loses focus
		If (CallTipHwnd And WinActive("A") = CallTipHwnd)
			return
		If (useToolTip)
			ToolTip
		Else {
			Gui, CallTip:Destroy
			callTipGui := "", curIndex := "", fullDescArr := "", CallTipHwnd := ""
		}
	}
	
	If (closeTipOnLButton) { ; closeTipOnLButton if enabled
		If (useToolTip)
			ToolTip
		Else If (IsObject(callTipGui)) {
			Gui, CallTip:Destroy
			callTipGui := "", curIndex := "", fullDescArr := "", CallTipHwnd := ""
		}
	}
return

^!w::
	newText := truncateTxt(truncateTextChars)
	If (newText)
		clipboard := newText
return

^!u::
	newText := unwrapText()
	If (newText)
		clipboard := newText
return



F11:: ; list custom functions, commands, and objects - for debugging List_*.txt files only
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		testList := ""
		For curName, obj in CustomFunctions {
			desc := obj["desc"]
			testList .= curName " / " desc "`r`n`r`n"
		}
			
		msgbox % "Custom Functions:`r`n`r`n" testList
		
		testList := ""
		For objName, obj in ObjectList {
			type := obj["type"], label := obj["label"], match := obj["match"]
			testList .= objName " / " label " / " type "`r`n" match "`r`n`r`n"
		}
		msgbox % "ObjectList:`r`n`r`n" testList
	}
return

F10:: ; list functions - for debugging List_*.txt files only
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		testList := ""
		For curName, obj in FunctionList {
			if (curName = "msgbox") {
				desc := obj["desc"]
				testList .= curName "`r`n" desc "`r`n`r`n"
			}
		}
			
		msgbox % "Functions:`r`n`r`n" testList
	}
return




; ==================================================
; hotkeys - #If not working so well???
; ==================================================
; #If WinActive("ahk_exe notepad++.exe") ; WinActive("ahk_exe notepad.exe") Or 

