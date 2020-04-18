; === comment out if using this script as a library and these are already determined. ===
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================

FileEncoding, UTF-8

Global callTipGui, curIndex, helpFile, fullDescArr, hCtl, hEditorWin, cClassNN, CallTipHwnd

Global fontFace, fontSize, fontColor, bgColor, srcFiles

Global truncateTextChars, AutoCompleteLength, useTooltip, maxLines, maxWidth
Global loadCallTipOnClick, callTipSelectable
Global closeTipOnLButton, closeTipOnClick, closeTipOnFocusChange

Global curPhrase, curPhraseObj, curPhraseType, parentObj, parentObjType
Global funcBeginStr, funcEndStr
Global ObjectCreateList
Global ObjectList, MethPropList, FunctionList, CustomFunctions, KeywordList

; ======================================================================================
; ==== user settings ===================================================================
; ======================================================================================
srcFiles := A_ScriptDir "\Languages\AHK1"
Loop Files, %srcFiles%\*.chm
	If (A_Index = 1)
		helpFile := A_LoopFileFullPath

fontFace := "Courier New"
fontSize := 10
fontColor := "Yellow"
bgColor := "202020"

; ==== gui behavior settings ===========================================================
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
SysGet, maxWidth, 78
maxWidth := (maxWidth * 0.75)

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
	
	state := GetKeyState(curKey,"P")
	
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
	
	FileRead, curList, %fileName%
	curList := Trim(curList,"`r`n") entryEnd
	curPos := 1, subStrEnd := 1
	len := StrLen(curList)

	While (curPos <= len And subStrEnd) {
		subStrEnd := InStr(curList,entryEnd,false,curPos)
		curLen := subStrEnd - curPos
		curSubStr := SubStr(curList,curPos,curLen)
		
		If (curFileType = "BeginEnd") {
			Loop Parse, curSubStr, `n, `r
			{
				If (A_Index = 1)
					funcBeginStr := Trim(SubStr(A_LoopField,7))
				Else If (A_Index = 2)
					funcEndStr := Trim(SubStr(A_LoopField,5))
			}
		} Else {
			funcName := "", funcHelpLink := "", funcDescArr := Array()
			funcArr := StrSplit(curSubStr,Chr(96)), i := funcArr.Length()
			
			Loop % i {
				t := Trim(funcArr[A_Index],"`r`n")
				If (A_Index = 1) {
					Loop Parse, t, `n, `r
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
			
			curObj := Object()
			curObj["desc"] := funcDescArr
			curObj["type"] := funcType ; "function" or "command"
			curObj["helpLink"] := funcHelpLink
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
	curPos := 1, curHelpLink := "", propText := "", methText := ""
	len := StrLen(curList)
	
	curObj := Object(), propList := Object(), methList := Object()

	While (curPos <= len) {
		subStrEnd := InStr(curList,entryEnd,false,curPos)
		curLen := subStrEnd - curPos
		curSubStr := SubStr(curList,curPos,curLen)
		
		curDesc := "", curMemType := "", curMem := ""
		If (A_Index = 1) {
			objMatchArr := StrSplit(curSubStr,Chr(96))
			i := objMatchArr.Length()
			
			objDescArr := Array(), methPropArr := Array(), curHelpLink := ""
			Loop % i {
				t := Trim(objMatchArr[A_Index],"`r`n")
				If (A_Index = 1) {
					objListPre := t, objMatchText .= objListPre "`r`n"
				} Else If (A_Index > 1 And A_Index < i) {
					objDescArr.Push(t)
				} Else If (A_Index = i) {
					curHelpLink := t
				}
			}
			
			Loop Parse, objListPre, `n, `r ; create list of defined objTypes
			{
				lineSplit := StrSplit(A_LoopField," "), curObjType := lineSplit[2]
				objList .= curObjType "`r`n"
			}
			Sort, objList, U
			objList := Trim(objList,"`r`n")
		} Else {
			memMap := Object(), memDescArr := Array(), memHelpLink := ""
			methPropArr := StrSplit(curSubStr,Chr(96)), i := methPropArr.Length()
			Loop % i {
				t := Trim(methPropArr[A_Index],"`r`n")
				If (A_Index = 1) {
					Loop Parse, t, `n, `r
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
			
			StringLower, curMemType, curMemType
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
	
	Loop Parse, objList, `n, `r ; append methods/properties to all defined obj types
	{
		curFirstDesc := StrReplace(firstDesc,"[ObjectTypeName]",A_LoopField)
		objDescArr.InsertAt(1,curFirstDesc)
		curObj["desc"] := objDescArr
		MethPropList[A_LoopField] := curObj
	}
	curObj := "", methList := "", propList := ""
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
		
		If (curLevel != prevLevel)
			ObjectCreateList[prevLevel] := curLevelObj, curLevelObj := Object()
		
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
ObjectList := CreateObjList(curDocText)
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
	
	If (!curIndex)
		curIndex := 1
	fullDesc := "", phraseRedir := "", helpLink := "", descArr := Array()
	
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
				descArr := obj["desc"] ; desc array
				helpLink := obj["helpLink"]
				break
			}
		}
		
		If (!descArr.Length()) {
			For funcName, obj in CustomFunctions {
				If (funcName = curPhrase) {
					descStr := obj["desc"]
					descArr.Push(descStr) ; make array from str
					break
				}
			}
		}
		
		If (!descArr.Length())
			return
		Else {
			fullDescArr := Object()
			For index, desc in descArr {
				curDescObj := Object()
				curDescObj["desc"] := desc
				curDescObj["helpLink"] := helpLink
				fullDescArr[index] := curDescObj
			}
			curDescObj := "", descArr := ""
		}
	} Else If (curPhraseType = "object") {
		obj := ObjectList.HasKey(curPhrase) ? ObjectList[curPhrase] : ""
		If (!obj)
			return
		
		fullDescArr := Object(), i := 1
		For objType, typeObj in obj {
			listObj := MethPropList.HasKey(objType) ? MethPropList[objType] : ""
			
			If (listObj) {
				descArr := listObj["desc"]
				helpLink := listObj["helpLink"]
				
				For index, desc in descArr {
					curObj := Object()
					curObj["desc"] := desc, curObj["helpLink"] := helpLink
					fullDescArr[i] := curObj
					i++
				}
			}
		}
		curObj := "", listObj := "", obj := ""
	} Else If (curPhraseType = "method" Or curPhraseType = "property") {
		obj := ObjectList.HasKey(parentObj) ? ObjectList[parentObj] : ""
		If (!obj)
			return
		
		fullDescArr := Object(), i := 1
		For objType, typeObj in obj {
			listObj := MethPropList.HasKey(objType) ? MethPropList[objType] : ""
			memObj := listObj[curPhraseType]
			
			For methPropName, descObj in memObj {
				If (methPropName = curPhrase) {
					descArr := descObj["desc"]
					helpLink := descObj["helpLink"]
					helpLink := StrReplace(helpLink,"[MemberName]",methPropName)
					
					For index, desc in descArr {
						StringUpper, titleHeader, curPhraseType, T
						desc := objType " " titleHeader ":`r`n`r`n" desc
						curObj := Object()
						curObj["helpLink"] := helpLink, curObj["desc"] := desc
						fullDescArr[i] := curObj
						i++
					}
					
					Break
				}
			}
		}
		curObj := "", listObj := "", memObj := "", descObj := ""
	} Else ; unrecognized keyword, so return
		return
	
	fullDescArrObj := fullDescArr[curIndex]
	fullDesc := fullDescArrObj["desc"]
	
	Loop Parse, fullDesc, `n, `r
		newFullDesc .= "`r`n" RegExReplace(A_LoopField,"^/","`r`n")
	fullDesc := Trim(newFullDesc,"`r`n")
	
	If (!fullDesc)
		return
	Else
		dims := GetMsgDimensions(fullDesc,fontFace,fontSize)
	
	Loop Parse, fullDesc, `n, `r ; count lines in desc
		tL := A_Index
	
	If (tL > maxLines) ; set max lines
		tL := maxLines, vscroll := ""
	Else
		vscroll := "-VScroll"
	
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
		SysGet, maxW, 78
		maxW := (maxW < dims.w) ? "w" maxWidth : ""
		
		If (!callTipSelectable) {
			Gui, CallTip:Add, Text, %maxW% +c%fontColor% ggui_click r%tL% x5 y5 AltSubmit, %fullDesc%
			GuiControlGet, dCtl, Pos, Static1
			Gui, Color, %bgColor%
		} Else {
			Gui, CallTip:Add, Edit, %maxW% +c%fontColor% r%tL% %vscroll% -E0x200 x5 y5 ReadOnly, %fullDesc%
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
	fullDescArrObj := fullDescArr[curIndex]
	fullDesc := fullDescArrObj["desc"]
	link := fullDescArrObj["helpLink"]
	
	If (InStr(link,"/") = 1 And FileExist(helpFile)) {
		helpFile := StrReplace(helpFile," ","%20")
		cmd := "hh.exe mk:@MSITStore:" helpFile "::" Trim(link,"`r`n")
		Run %cmd%
		Gui, CallTip:Destroy
		callTipGui := "", curIndex := "", fullDescArr := ""
	}
	
	If (callTipGui And closeTipOnClick) {
		Gui, CallTip:Destroy
		callTipGui := "", curIndex := "", fullDescArr := ""
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
	curLineNoStr := RegExReplace(sInput,Chr(96) Chr(34) "|" "\\" Chr(34),"**")			; blank out literal `" first
	While (result := RegExMatch(curLineNoStr,"O)(" Chr(34) ".*?" Chr(34) ")",match)) {	; which helps to properly match strings
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
CreateObjList(curDocText) { ; v2 - hopefully uses less cpu
	oList := Object()
	
	For level, lvlObj in ObjectCreateList {
		For label, lblObj in lvlObj {
			i := 1, type := lblObj["type"], regex := lblObj["regex"], direct := lblObj["direct"]
			
				curPos := 1
				
				; Msgbox % level " / " label " / " type "`r`n`r`n" regex
				
				If (direct) {
					While(result := RegExMatch(curDocText,"O)" regex,match,curPos)) {
						c := match.Count()
						If (IsObject(match) And c >= 1) {
							typeObj := Object(), objName := match.Value(1) ; obj := Object()
							objNameArr := StrSplit(objName,".")
							If (objNameArr.Length() > 1)
								objName := objNameArr[2]
							
							If (!oList.HasKey(objName)) {
								obj := Object()
							} Else {
								obj := oList[objName]
							}
							
							objMatch := (c = 2) ? match.Value(2) : match.Value(1)
							typeObj["label"] := label
							typeObj["match"] := objMatch
							If (type)
								obj[type] := typeObj
							
							quit := false
							If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
								quit := true
							
							If (!quit) ; If (!oList.HasKey(objName))
								oList[objName] := obj
							
							curPos := match.Pos(c) + match.Len(c)
						} Else
							curPos := StrLen(curDocText)
					}
				} Else { ; perform {substitution} before doing regex match and adding to ObjectList
					r1 := RegExMatch(regex,"O)\{(.*?)\}",match), listType := match.Value(1)
					
					For curObjName, curLblObj in oList {
						For curType, curTypeObj in curLblObj {
							If (curType = listType) {
								newRegex := StrReplace(regex,"{" listType "}",curObjName)
								
								While (result := RegExMatch(curDocText,"O)" newRegex,match,curPos)) {
									c := match.Count()
									If (IsObject(match) And c = 2) {
										typeObj := Object(), objName := match.Value(1) ; obj := Object()
										objNameArr := StrSplit(objName,".")
										If (objNameArr.Length() > 1)
											objName := objNameArr[2]
										
										If (!oList.HasKey(objName))
											obj := Object()
										Else
											obj := oList[objName]
										
										objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
										typeObj["label"] := label
										typeObj["match"] := objMatch
										If (type)
											obj[type] := typeObj
										
										quit := false
										If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
											quit := true
										
										If (!quit) ; If (!oList.HasKey(objName))
											oList[objName] := obj
										
										curPos := match.Pos(c) + match.Len(c)
									} Else
										curPos := StrLen(curDocText)
								} ; end while
							}
						}
					} ; end for
				}
			
		} ; end for
	} ; end for
	
	For custFuncName, curLblObj in CustomFunctions {
		curRetStr := "", curObjType := "", newRegex := "", doMatch := false
		
		If (curLblObj.HasKey("return")) {
			returnObj := curLblObj["return"]
			For retStr, L in returnObj { ; loop through return entries in Func
				For curObjName, curLblObj in oList { ; loop through oList to find objMatch
					If (retStr = curObjName) {
						doMatch := true
						newRegex := "([\w\.]+)[ \t]*:=[ \t]*({CustomFunction}\x28)"
						newRegex := StrReplace(newRegex,"{CustomFunction}",custFuncName)
						
						curRetStr := retStr ; return var in matched function
						curObjType := curLblObj ; curLblObj["type"] ; obj type to save
						Break ; break on match
					}
				} ; end for
				
				If (curRetStr)
					Break
			} ; end for
		}
		
		curPos := 1
		While (result := RegExMatch(curDocText,"O)" newRegex,match,curPos) And doMatch) {
			c := match.Count()
			If (IsObject(match) And match.Count() = 2 and curRetStr) { ; if match add obj to list
				obj := Object(), objName := match.Value(1)
				objNameArr := StrSplit(objName,".")
				If (objNameArr.Length() > 1)
					objName := objNameArr[2]
				
				objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
				
				quit := false
				If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
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


; CreateObjList(curDocArr) { ; v1 - lags
	; oList := Object()
	
	; For level, lvlObj in ObjectCreateList {
		; For label, lblObj in lvlObj {
			; i := 1, type := lblObj["type"], regex := lblObj["regex"], direct := lblObj["direct"]
			
			; count1 := curDocArr.Length()
			; Loop %count1% {
				; curLine := curDocArr[A_Index], i := A_Index
				; curLine := StringOutline(curLine)
				; curPos := 1
				
				; If (direct) {
					; While (result := RegExMatch(curLine,"O)" regex,match,curPos)) {
						
						; c := match.Count()
						; If (IsObject(match) And c >= 1) {
							; obj := Object(), objName := match.Value(1)
							; objNameArr := StrSplit(objName,".")
							; If (objNameArr.Length() > 1)
								; objName := objNameArr[2]
							
							; objMatch := (c = 2) ? match.Value(2) : match.Value(1)
							
							; obj["type"] := type
							; obj["label"] := label
							; obj["line"] := i
							; obj["match"] := objMatch
							
							; quit := false
							; If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
								; quit := true
							
							; If (!oList.HasKey(objName) And !quit)
								; oList[objName] := obj
							
							; curPos := match.Pos(c) + match.Len(c)
						; } Else
							; curPos := StrLen(curLine)
					; }
				; } Else { ; perform {substitution} before doing regex match and adding to ObjectList
					; r1 := RegExMatch(regex,"O)\{(.*?)\}",match), listType := match.Value(1)
					
					; For curObjName, curLblObj in oList {
						; curType := curLblObj["type"]
						; If (curType = listType) {
							; newRegex := StrReplace(regex,"{" listType "}",curObjName)
							
							; While (result := RegExMatch(curLine,"O)" newRegex,match,curPos)) {
								; c := match.Count()
								; If (IsObject(match) And c = 2) {
									; obj := Object(), objName := match.Value(1)
									; objNameArr := StrSplit(objName,".")
									; If (objNameArr.Length() > 1)
										; objName := objNameArr[2]
								
									; objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
									; obj["type"] := type
									; obj["label"] := label
									; obj["line"] := i
									; obj["match"] := objMatch
									
									; quit := false
									; If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
										; quit := true
									
									; If (!oList.HasKey(objName))
										; oList[objName] := obj
									
									; curPos := match.Pos(c) + match.Len(c)
								; } Else
									; curPos := StrLen(curLine)
							; } ; end while
						; }
					; } ; end for
				; }
			; } ; end loop
		; } ; end for
	; } ; end for
	
	; Loop % curDocArr.Length() { ; parse document one more time for custom functions returning objects
		; curLine := curDocArr[A_Index], i := A_Index
		; curLine := StringOutline(curLine)
		
		; For custFuncName, curLblObj in CustomFunctions {
			; newRegex := "^[ \t]*([\w\.]+)[ \t]*:=[ \t]*({CustomFunction}\x28)"
			; newRegex := StrReplace(newRegex,"{CustomFunction}",custFuncName)
			; curRetStr := "", curObjType := ""
			
			; If (curLblObj.HasKey("return")) {
				; returnObj := curLblObj["return"]
				; For retStr, L in returnObj { ; loop through return entries in Func
					; For curObjName, curLblObj in oList { ; loop through oList to find objMatch
						; If (retStr = curObjName) {
							; curRetStr := retStr ; return var in matched function
							; curObjType := curLblObj["type"] ; obj type to save
							; Break ; break on match
						; }
					; } ; end for
					
					; If (curRetStr)
						; Break
				; } ; end for
			; }
			
			; curPos := 1
			; While (result := RegExMatch(curLine,"O)" newRegex,match,curPos)) {
				; c := match.Count()
				; If (IsObject(match) And match.Count() = 2 and curRetStr) { ; if match add obj to list
					; obj := Object(), objName := match.Value(1)
					; objNameArr := StrSplit(objName,".")
					; If (objNameArr.Length() > 1)
						; objName := objNameArr[2]
					
					; objMatch := (match.Count() = 2) ? match.Value(2) : match.Value(1)
					; obj["type"] := curObjType
					; obj["label"] := "objByFunc"
					; obj["line"] := i
					; obj["match"] := objMatch
					
					; quit := false
					; If (KeywordList.HasKey(objName)) ; omit in ObjectList if objName is a keyword
						; quit := true
					
					; If (!oList.HasKey(objName))
						; oList[objName] := obj
					
					; curPos := match.Pos(c) + match.Len(c)
				; } Else
					; curPos := StrLen(curLine)
			; } ; end while
		; } ; end for
	; } ; end loop
	
	; return oList
; }

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
	ObjectList := CreateObjList(curDocText)
	
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
	
	parentObjTypeList := ObjectList.HasKey(parentObj) ? ObjectList[parentObj] : Object()
	
	; parentObjType := (parentObj And ObjectList.HasKey(parentObj)) ? ObjectList[parentObj]["type"] : ""
	
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
	
	If (!curPhraseType And parentObjTypeList.Count()) {
		For objType in parentObjTypeList {
			methList := MethPropList[objType]["method"]
			For methName in methList { ; MethPropList.count
				If (methName = curPhrase) {
					curPhraseType := "method"
					parentObjType := objType
					Break
				}
			}
		}
	}
	
	If (!curPhraseType And parentObjTypeList.Count()) {
		For objType in parentObjTypeList {
			propList := MethPropList[objType]["property"]
			For propName in propList {
				If (propName = curPhrase) {
					curPhraseType := "property"
					parentObjType := objType
					Break
				}
			}
		}
	}
	
	; If (curPhrase) { ; for debugging while allowing user input - requires InputHook up top
		; x := A_CaretX, y := A_CaretY
		; Tooltip % "curPhrase :" curPhrase ": / curPhraseObj: " curPhraseObj " / funcName: " funcName " / curMethProp: " curMethProp "`r`nparentObj: " parentObj " / parentObjType: " parentObjType "`r`nphraseType: " curPhraseType "`r`nfuncText: " funcText, x, (y+30)
	; } Else
		; ToolTip
}

debugToolTip() {
	ProcInput()
	If (curPhrase) { ; for debugging while allowing user input - requires InputHook up top
		x := A_CaretX, y := A_CaretY, y+30
		Tooltip % "curPhrase :" curPhrase ": / curPhraseObj: " curPhraseObj
		      . "`r`nfuncName: " funcName " / funcText: " funcText
		      . "`r`nparentObj: " parentObj " / parentObjType: " parentObjType 
			  . "`r`nphraseType: " curPhraseType, %x%, %y%
	} Else
		ToolTip
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
						curEntry := curType "`r`n" Trim(curTitleArr[A_Index]) "`r`n" Chr(96) Trim(fullSyntax,"`r`n") "`r`n" Chr(96) curHelpLink
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
							curEntry := curType "`r`n" Trim(curTitle) "`r`n" Chr(96) Trim(curLine,"`r`n") "`r`n" Chr(96) curHelpLink "#" curTitle
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
	; If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		
	; }
	If (useToolTip)
		Tooltip
	Else {
		Gui, CallTip:Destroy
		callTipGui := "", curIndex := "", fullDescArr := "", CallTipHwnd := ""
	}
return

Up::
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		i := fullDescArr ? fullDescArr.Count() : 0
		If (curIndex And (curIndex-1 != 0)) {
			curIndex--
			Gui, CallTip:Destroy
			LoadCallTip()
		} Else SendInput {Up}
	} Else SendInput {Up}
return

Down::
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		i := fullDescArr ? fullDescArr.Count() : 0
		If (curIndex And (curIndex+1) <= i) {
			curIndex++
			Gui, CallTip:Destroy
			LoadCallTip()
		} Else SendInput {Down}
	} Else SendInput {Down}
return

~LButton::
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		; debugToolTip()
		
		hCtl := editorCtlHwnd(hEditorWin,cType,cClassNN)
		
		MouseGetPos,,,winHwnd
		doSkip := false
		If (IsObject(callTipGui)) {
			if (winHwnd = callTipGui.hwnd)
				doSkip := true
		}
		
		If (loadCallTipOnClick And (winHwnd != CallTipHwnd)) {
			If (useToolTip)
				ToolTip
			Else If (IsObject(callTipGui)) {
				Gui, CallTip:Destroy
				callTipGui := "", curIndex := "", fullDescArr := ""
			}
			
			ProcInput()
			LoadCallTip()
		}
	} Else If (closeTipOnFocusChange) { ; close call tip when text editor loses focus
		If (useToolTip)
			ToolTip
		Else {
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
			For curType, obj2 in obj {
				type := curType, label := obj2["label"], match := obj2["match"]
				testList .= objName " / " label " / " type "`r`n" match "`r`n`r`n"
			}
		}
		msgbox % ObjectList.Count() "`r`nObjectList:`r`n`r`n" testList
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

