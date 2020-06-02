; AHK v1
; === comment out if using this script as a library and these are already determined. ===
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================

#INCLUDE LibV1\_Jxon_v1.ahk
#INCLUDE LibV1\_Font_Picker_Dialog_v1.ahk
#INCLUDE LibV1\_Color_Picker_Dialog_v1.ahk

FileEncoding, UTF-8

Global SettingsGUI, AutoCompleteGUI, callTipGui
Global curIndex, helpFile, fullDescArr, hCtl, hEditorWin, cClassNN

Global srcFiles ; fontFace fontSize, fontColor, bgColor

Global WrapTextChars, AutoCompleteLength, useTooltip, maxLines, maxWidth
Global callTipSelectable

Global oCallTips

Global curPhrase, curPhraseObj, curPhraseType, parentObj, parentObjType
Global funcBeginStr, funcEndStr

Global ObjectCreateList
Global ObjectList, MethPropList, FunctionList, CustomFunctions, KeywordList

; ======================================================================================
; ==== user settings ===================================================================
; ======================================================================================
; fontFace := "Courier New"
; fontSize := 10
; fontColor := "Yellow"
; bgColor := "202020"

; ==== gui behavior settings ===========================================================
; Some of these settings don't play well together.  Just think about what you enable.
; For example if you enable loadCallTipOnClick and closeTipOnClick or closeTipOnLButton,
; then the call tip won't load on click.  Tooltips may not follow the closeTipOnLButton,
; or closeTipOnClick settings.
; ======================================================================================

WrapTextChars := 80

AutoCompleteLength := 3			; Auto-Complete won't trigger until X chars have been typed

useTooltip := false				; ignores fontFace, fontSize, and callTipSelectable

maxLines := 20

; CallTipSelectable := true		; make call tip text selectable, ignores closeTipOnLButton

closeTipOnLButton := false		; close call tip on L click anywhere
closeTipOnClick := false		; close call tip when clicked on
; ======================================================================================
; Script Vars
; ======================================================================================

Global entryEnd, Settings, exeList
entryEnd := "`r`n`r`n`r`n"
FileRead, settingsText, Settings.txt
Settings := Jxon_Load(settingsText)
; exeList := Settings["exeList"]

ReParseText() ; initial loading of functions, custom functions, objects

; ======================================================================================
; Tray Menu
; ======================================================================================
; Menu, subMenuAutoGen, Add, Generate Commands, iconMenu

Menu, Tray, NoStandard
Menu, Tray, Add, Settings, iconMenu
Menu, Tray, Add, ReWrap Text (CTL + ALT + W), iconMenu
Menu, Tray, Add, UnWrap Text (CTL + ALT + U), iconMenu
; Menu, Tray, Add, AHK Auto-Gen, :subMenuAutoGen
Menu, Tray, Add, Reload, iconMenu
Menu, Tray, Add, Exit, iconMenu

iconMenu(ItemName, ItemPos, MenuName) {
	If (ItemName = "Settings") {
		SettingsGUI()
	} Else If (ItemName = "Generate Commands") {
		AutoCmdFunc()
	} Else If (ItemName = "ReWrap Text (CTL + ALT + W)") {
		clipboard := WrapText(WrapTextChars)
	} Else If (ItemName = "UnWrap Text (CTL + ALT + U)") {
		clipboard := unwrapText()
	} Else If (ItemName = "Reload")
		Reload
	Else If (ItemName = "Exit")
		ExitApp
}

WrapText(x) {
	hCtl := editorCtlHwnd(hEditorWin,cClassNN,EventType)
	
	If (!StrLen(clipboard))
		return ""
	
	inText := clipboard
	endCRLF := false
	If (result := RegExMatch(inText,"(.*[\r\n]+$)",match))
		endCRLF := true
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
				nextChar := SubStr(curLine,s+x,1)
				
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
	
	newText := Trim(newText,"`r`n")
	
	If (endCRLF)
		return newText "`r`n"
	return newText
}

unwrapText() {
	If (!StrLen(clipboard))
		return ""
	
	inText := clipboard
	endCRLF := false
	If (result := RegExMatch(inText,"(.*[\r\n]+$)",match))
		endCRLF := true
	inText := StrReplace(clipboard,"`r`n`r`n",Chr(1))
	a := StrSplit(inText,Chr(1))
	
	Loop % a.Length() {
		tempText := a[A_Index]
		tempText := StrReplace(tempText,".`r`n",".  ")
		tempText := StrReplace(tempText,"`r`n"," ")
		
		newText .= tempText "`r`n`r`n"
	}
	newText := Trim(newText,"`r`n")
	newText := RegExReplace(newText,"\.  $",".")
	
	If (endCRLF)
		return newText "`r`n"
	Else return newText
}

; ======================================================================================
; input hook - for processing during user input - i prefer hotkey to invoke call tip.
; ======================================================================================

IH := InputHook("V I1","","") ; options , endKeys , matchList
IH.OnKeyDown := Func("keyPress"), IH.KeyOpt("{All}","N"), IH.Start()

keyPress(iHook, VK, SC) { ; InputHookObject
	curKey := "sc" Format("{:X}",SC)
	
	If (curKey != "sc118" And curKey != "sc117") { ; laptop close / open
		state := 0
		Try {
			state := GetKeyState(curKey,"P")
		} Catch e {
			msgbox % "State: " state "`r`n... is invalid.`r`n`r`n" curKey
		}
		
		if (state) {
			SetTimer LoadAutoComplete, -100
			SetTimer ReParseText, -500
		}
		
		iHook.Stop()
		IH := InputHook("V I1","","")
		
		IH.OnKeyDown := Func("keyPress"), IH.KeyOpt("{All}","N"), IH.Start()
	}
}

; ==================================================
; keyword list - These are also generated when functions, methods, and properties are
;                enumerated from List_*.txt files.  This will be expanded for an auto
;                complete function/hotkey.  KW_*.txt files are optional, and don't serve
;                much purpose yet.
; ==================================================

LoadKeywordsList() {
	KeywordList := Object()
	Loop Files srcFiles "Keywords\KW_*.txt"
	{
		FileRead, curText, %A_LoopFileFullPath%
		Loop Parse, curText, `n, `r
			KeywordList[A_LoopField] := "keyword"
	}
}
; ==================================================
; Create function and command list for call tips
; ==================================================

LoadFunctionsList() {
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
	
	; textList .= "Name: " funcName " / " type "`r`n" desc "`r`n`r`n`r`n"
; }
; clipboard := textList
; msgbox % textList

; ==================================================
; generate custom function list
; ==================================================
; hCtl := editorCtlHwnd(hEditorWin,cClassNN,EventType)
; ControlGetText, curDocText, %cClassNN%, ahk_id %hEditorWin%
; curDocArr := StrSplit(curDocText,"`n","`r")
; CustomFunctions := GetCustomFunctions(curDocArr)

; ==================================================
; Create Object Index by type for call tips
; ==================================================
LoadMethPropList() {
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
LoadObjectCreateList(objMatchText) {
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
}

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

; ======================================================================================
; Load Call Tip
; ======================================================================================
LoadCallTip() { ; curPhrase, curPhraseType ---> globals
	fontFace := Settings["fontFace"]
	fontSize := Settings["fontSize"]
	fontColor := Format("{:X}",Settings["fontColor"])
	bgColor := Format("{:X}",Settings["bgColor"])
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
						desc := objType " " titleHeader ":`r`n`r`n" StrReplace(desc,"[MemberName]",methPropName)
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
	
	curMon := GetMonitorData(outX,outY) ; get actvie monitor dims and L/R/T/B
	dims := GetTextDims(fullDesc,fontFace,fontSize,maxWidth)
	
	If (dims.w > curMon.w)
		dims := GetTextDims(fullDesc,fontFace,fontSize,curMon.w * 0.75)
		
	vscroll := "-VScroll"
	
	CoordMode Caret, Screen
	outX := A_CaretX, outY := A_CaretY
	
	If (useTooltip) {
		outX += -70, outY += -25
		Tooltip %fullDesc%, %outX%, %outY%
	} Else {
		outX += -11, outY += 20
		
		If (callTipGui)
			Gui, CallTip:Destroy
		
		Gui, CallTip:New, % "-Border AlwaysOnTop +Owner " hEditorWin " +HwndcallTipGui"
		Gui, Color, %bgColor%
		Gui, Font, s%fontSize%, %fontFace%
		
		If (!Settings["CallTipSelectable"]) {
			Gui, CallTip:Add, Text, % "w" dims.w " h" dims.h " +c" fontColor " ggui_click x5 y5 AltSubmit", %fullDesc%
			GuiControlGet, dCtl, Pos, Static1
		} Else {
			SysGet, scrollW, 2
			Gui, CallTip:Add, Edit, % "w" (dims.w+(scrollW*2)) " h" dims.h " x5 y5 +Background" bgColor " " vscroll "-E0x200 x5 y5 ReadOnly", %fullDesc%
			GuiControlGet, dCtl, Pos, Edit1
		}
		
		posTest := OutX + dCtlW
		
		If (posTest > curMon.right) {
			offset := posTest - curMon.right
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
	
	KeywordFilter := Object()
	If (StrLen(curPhrase) >= AutoCompleteLength) { ; not done yet!
		For kw, type in KeywordList {
			If (InStr(kw,curPhrase))
				KeywordFilter[kw] := type
		}
		; msgbox KeywordFilter.Count
		
	}
}

AutoCompleteGUI(KeywordFilter) {
	If (AutoCompleteGUI)
		AutoCompleteGUI.Destroy()
	
	
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
				
				If (direct) {
					While(result := RegExMatch(curDocText,"O)" regex,match,curPos)) {
						c := match.Count()
						If (IsObject(match) And c >= 1) {
							typeObj := Object(), objName := match.Value(1) ; obj := Object()
							objNameArr := StrSplit(objName,".")
							If (objNameArr.Length() > 1)
								objName := objNameArr[2]
							
							If (!oList.HasKey(objName))
								obj := Object()
							Else
								obj := oList[objName]
							
							
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
						} ; end for
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
						curObjType := curLblObj
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

ReParseText() {
	hCtl := editorCtlHwnd(hEditorWin,cClassNN,EventType)
	If (cClassNN != "scintilla1" And cClassNN != "Edit1")
		return
		
	If (WinActive("ahk_id " hEditorWin)) {
		fontFace := Settings["fontFace"]
		fontSize := Settings["fontSize"]
		fontColor := Format("{:X}",Settings["fontColor"])
		bgColor := Format("{:X}",Settings["bgColor"])
		
		Gui, Loading:New, -Caption AlwaysOnTop +Owner%hEditorWin%
		Gui, Loading:Font, s%fontSize%, %fontFace%
		Gui, Loading:Color, %bgColor%
		Gui, Loading:Add, Text, x5 y5, Loading Objects / Custom Functions...
		GUi, Loading:Show, y0 NA NoActivate
		
		; dims := g.Pos
		; y := SysGet(79) - dims.h
		; x := SysGet(78) - dims.w
		; g.Show("x" x " y" y " NA NoActivate") ; move to lower right
		
		ReloadElements()
		Gui Loading:Destroy
	}
}

ReloadElements() {
	srcFiles := A_ScriptDir "\Languages\" Settings["ActiveLanguage"]
	Loop Files srcFiles "\*.chm"
		If (A_Index = 1)
			helpFile := A_LoopFileFullPath
	
	LoadKeywordsList()
	LoadFunctionsList()
	
	ControlGetText, curDocText, %cClassNN%, % "ahk_id " hEditorWin
	curDocArr := StrSplit(curDocText,"`n","`r")
	CustomFunctions := GetCustomFunctions(curDocArr)

	objMatchText := LoadMethPropList()
	LoadObjectCreateList(objMatchText)

	ObjectList := CreateObjList(curDocText)
	curDocArr := "", curDocText := ""
}

ProcInput() {
	curPhrase := "", parentObjType := "", curPhraseType := ""
	ControlGetText, curDocText, %cClassNN%, % "ahk_id " hEditorWin
	curDocArr := StrSplit(curDocText,"`n","`r")
	
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
}

debugToolTip() {
	ProcInput()
	If (curPhrase) { ; for debugging while allowing user input - requires InputHook up top
		x := A_CaretX, y := A_CaretY
		Tooltip % "curPhrase :" curPhrase ": / curPhraseObj: " curPhraseObj
		      . "`r`nfuncName: " funcName " / funcText: " funcText
		      . "`r`nparentObj: " parentObj " / parentObjType: " parentObjType 
			  . "`r`nphraseType: " curPhraseType, %x%, % (y+30)
	} Else
		ToolTip
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





; ================================================================
; Settings GUI
; ================================================================

SettingsGUI() {
	Global
	Gui, Settings:New, AlwaysOnTop +Labelgui_ +HwndSettingsGUI, CallTipsForAll v2
	
	
	ActiveLanguage := Settings["ActiveLanguage"]
	
	If (callTipGui) {
		Gui, CallTip:Destroy
		callTipGui := "", curIndex := "", fullDescArr := ""
	}
	
	langList := ""
	Loop, Files, %A_ScriptDir%\Languages\*, D
		langList .= (A_LoopFileName = ActiveLanguage) ? A_LoopFileName "||" : A_LoopFileName "|"
	
	langList := (SubStr(langList,-1) = "||") ? langList : Trim(langList,"|")
	
	Gui, Settings:Add, Text, x5 y10, Language:
	Gui, Settings:Add, DropDownList, x+2 yp-4 w100 vPickLang ggui_change_events, %langList%
	
	Gui, Settings:Add, Checkbox, xm y+8 vLoadCallTipOnClick ggui_change_events, Load call tip on click
	GuiControl, Settings:, LoadCallTipOnClick, % Settings["LoadCallTipOnClick"]
	
	Gui, Settings:Add, Checkbox, x+8 vCloseTipOnFocusChange ggui_change_events, CLose call tip on focus change
	GuiControl, Settings:, CloseTipOnFocusChange, % Settings["CloseTipOnFocusChange"]
	
	Gui, Settings:Add, Checkbox, xm y+8 vDebugToolTip ggui_change_events, Show Debug Tooltip
	GuiControl, Settings:, DebugToolTip, % Settings["DebugToolTip"]
	
	Gui, Settings:Add, Checkbox, x+8 vCallTipSelectable ggui_change_events, Selectable call tips
	GuiControl, Settings:, CallTipSelectable, % Settings["CallTipSelectable"]
	
	Gui, Settings:Add, Button, vPickFont xm y+8 ggui_change_events, Select Font
	GuiControlGet, ctlpos, Settings:Pos, PickFont
	textW := ctlposW
	
	Gui, Settings:Add, Button, vPickFontColor x+0 ggui_change_events, Select Font Color
	GuiControlGet, ctlpos, Settings:Pos, PickFontColor
	textW += ctlposW
	
	Gui, Settings:Add, Button, vPickBgColor x+0 ggui_change_events, Select Background Color
	GuiControlGet, ctlpos, Settings:Pos, PickBgColor
	textW += ctlposW
	
	Gui, Settings:Add, Edit, % "vFontDemo xm y+8 w" textW " h50 -E0x200 -VScroll", Call Tip Test ; ReadOnly
	
	SetFontDemo()
	Gui, Settings:Show
}

gui_change_events(CtrlHwnd, GuiEvent, EventInfo) {
	GuiControlGet, ctlName, Settings:Name, %CtrlHwnd%
	GuiControlGet, ctlValue, , %CtrlHwnd%
	
	if (ctlName = "PickLang")
		Settings["ActiveLanguage"] := ctlValue
	else if (ctlName = "LoadCallTipOnClick")
		Settings["LoadCallTipOnClick"] := ctlValue
	else if (ctlName = "CloseTipOnFocusChange")
		Settings["CloseTipOnFocusChange"] := ctlValue
	else if (ctlName = "DebugToolTip")
		Settings["DebugToolTip"] := ctlValue
	else if (ctlName = "CallTipSelectable")
		Settings["CallTipSelectable"] := ctlValue
	Else If (ctlName = "PickFont") {
		fName := Settings["fontFace"]
		fSize := Settings["fontSize"]
		fontObj := Object("name",fName,"size",fSize) ; ,"color",0xFF0000
		fontObj := FontSelect(fontObj,SettingsGUI,0)
		If (fontObj) {
			Settings["fontFace"] := fontObj["name"]
			Settings["fontSize"] := fontObj["size"]
			SetFontDemo()
		}
	} Else if (ctlName = "PickFontColor") {
		fontColor := Settings["fontColor"]
		fontColor := ColorSelect(fontColor,SettingsGUI)
		If (fontColor > -1) {
			Settings["fontColor"] := fontColor
			SetFontDemo()
		}
	} Else if (ctlName = "PickBgColor") {
		bgColor := Settings["bgColor"]
		bgColor := ColorSelect(bgColor,SettingsGUI)
		If (bgColor > -1) {
			Settings["bgColor"] := bgColor
			SetFontDemo()
		}
	}
}

SetFontDemo() {
	fontFace := Settings["fontFace"]
	fontSize := Settings["fontSize"]
	fontColor := Format("{:X}",Settings["fontColor"])
	bgColor := Format("{:X}",Settings["bgColor"])
	
	Gui, Settings:Default
	Gui, Font, s%fontSize% c%fontColor%, %fontFace%
	GuiControl, Font, FontDemo
	GUi, Color,, %bgColor%
}

gui_close(hwnd) {
	ReParseText()
	
	settingsText := Jxon_Dump(Settings,4)
	FileDelete Settings.txt
	FileAppend %settingsText%, Settings.txt
	Gui, Settings:Destroy
	SettingsGUI := ""
}

gui_escape(hwnd) {
	ReParseText()
	
	settingsText := Jxon_Dump(Settings,4)
	FileDelete Settings.txt
	FileAppend %settingsText%, Settings.txt
	Gui, Settings:Destroy
	SettingsGUI := ""
}

editorCtlHwnd(ByRef progHwnd, ByRef classNN, ByRef eType) {
	MouseGetPos x,y,clickWinHwnd, clickCtlClassNN
	ctlFound := false, startTicks := A_TickCount
	
	; msgbox clickCtlClassNN " / " cClassNN
	
	; If (IsObject(callTipGui) and clickWinHwnd = callTipGui.hwnd) {
		; classNN := cClassNN, ctlHwnd := hCtl, progHwnd := hEditorWin, eType := -3
	; } Else If (IsObject(SettingsGUI) and clickWinHwnd = SettingsGUI.hwnd) {
		; classNN := cClassNN, ctlHwnd := hCtl, progHwnd := hEditorWin, eType := -4
	; } Else If (!clickCtlClassNN)
		; classNN := "", ctlHwnd := 0, progHwnd := 0, eType := -2
	; Else If (hEditorWin = clickWinHwnd And InStr(clickCtlClassNN,"SysTreeView"))
		; classNN := cClassNN, ctlHwnd := hCtl, progHwnd := hEditorWin, eType := 3
	; Else If (cClassNN != clickCtlClassNN) { ; look for editor / tab change
		; ctlFound := true
		; While (!newCtlHwnd) {
			; newCtlHwnd := ControlGetHwnd("scintilla1","A")
			; newCtlHwnd := newCtlHwnd ? newCtlHwnd : ControlGetHwnd("Edit1","A")
			; nextClassNN := ControlGetClassNN(newCtlHwnd,"A")
			
			; If (nextClassNN = "scintilla1" Or nextClassNN = "Edit1" And nextClassNN = clickCtlClassNN) {
				; classNN := nextClassNN
				; ctlHwnd := newCtlHwnd
				; progHwnd := WinActive("A")
				; eType := 1
				; break
			; } Else If (nextClassNN = "scintilla1" Or nextClassNN = "Edit1") {
				; classNN := nextClassNN
				; ctlHwnd := newCtlHwnd
				; progHwnd := WinActive("A")
				; eType := 2
				; break
			; } Else
				; newCtlHwnd := ""
			
			; Sleep 100
			; diff := A_TickCount - startTicks
			; If (diff > 1000) {
				; ctlFound := false
				; ctlHwnd := 0, progHwnd := WinActive("A"), classNN := "", eType := -1
				; break
			; }
		; }
	; } Else ; reuse global vars if click is in same win / ctrl
		; classNN := cClassNN, ctlHwnd := hCtl, progHwnd := hEditorWin, eType := 0
	classNN := clickCtlClassNN
	progHwnd := clickWinHwnd
	ControlGet, ctlHwnd, Hwnd,, %classNN%, % "ahk_id " clickWinHwnd
	eType := 0
	
	return ctlHwnd
}

closeCallTip(eType) {
	doClose := (eType = -2 or eType = -1 or eType = 1 or eType = 2) ? true : false
	If (doClose And Settings["CloseTipOnFocusChange"]) {
		If (useToolTip)
			ToolTip
		Else If (callTipGui) {
			Gui, CallTip:Destroy
			callTipGui := "", curIndex := "", fullDescArr := ""
		}
	}
}

LeftClickCheck() {
	hCtl := editorCtlHwnd(hEditorWin,cClassNN,EventType)
	
	closeCallTip(EventType)
	
	If (EventType = 1 Or EventType = 2) ; new control found, so reload
		ReParseText()
	Else If (EventType = -1 Or EventType = -2) {
		Sleep 100
		hCtl := editorCtlHwnd(hEditorWin,cClassNN,EventType)
		
		If (EventType >= 1)
			ReParseText()
	}
	
	return EventType
}

DisplayCallTip() {
	If (cClassNN != "scintilla1" And cClassNN != "Edit1")
		return
	
	If (useToolTip)						; close call tip (tooltip version)
		ToolTip
	Else If (IsObject(callTipGui)) {	; close call tip (gui version)
		Gui, CallTip:Destroy
		callTipGui := "", curIndex := "", fullDescArr := ""
	}
	
	If (Settings["DebugToolTip"])	; use debug tool tip if enabled
		debugToolTip()
	Else {				; load call tip
		ProcInput()
		LoadCallTip()
	}
}

; ======================================================================
; modified from Fnt_Library v3 posted by jballi
; https://www.autohotkey.com/boards/viewtopic.php?f=6&t=4379
; original function(s) = Fnt_CalculateSize() / Fnt_GetAverageCharWidth()
; ======================================================================
GetTextDims(r_Text, sFaceName, nHeight,maxWidth:=0) {
	Static Dummy57788508, DEFAULT_GUI_FONT:=17, HWND_DESKTOP:=0, MAXINT:=0x7FFFFFFF, OBJ_FONT:=6, SIZE
	
	hDC := DllCall("GetDC", "Ptr", HWND_DESKTOP) ; "UInt" or "Ptr" ?
	devCaps := DllCall("GetDeviceCaps", "Uint", hDC, "int", 90)
	
	nHeight := -DllCall("MulDiv", "int", nHeight, "int", devCaps, "int", 72)
	
	bBold := False, bItalic := False, bUnderline := False, bStrikeOut := False, nCharSet := 0
	
	hFont := DllCall("CreateFont", "int", nHeight, "int", 0 ; get specified font handle
	               , "int", 0, "int", 0, "int", 400 + 300 * bBold
				   , "Uint", bItalic, "Uint", bUnderline, "Uint"
				   , bStrikeOut, "Uint", nCharSet, "Uint", 0, "Uint"
				   , 0, "Uint", 0, "Uint", 0, "str", sFaceName)
	
	hFont := !hFont ? DllCall("GetStockObject","Int",DEFAULT_GUI_FONT) : hFont ; load default font if invalid
	
    l_LeftMargin:=0, l_RightMargin:=0, l_TabLength:=0, r_Width:=0, r_Height:=0
	l_Width := (!maxWidth) ? MAXINT : maxWidth
	l_DTFormat := 0x400|0x10 ; DT_CALCRECT (0x400) / DT_WORDBREAK (0x10)
	
    VarSetCapacity(DRAWTEXTPARAMS,20,0) ;-- Create and populate DRAWTEXTPARAMS structure
    NumPut(20,           DRAWTEXTPARAMS,0,"UInt")       ;-- cbSize
    NumPut(l_TabLength,  DRAWTEXTPARAMS,4,"Int")        ;-- iTabLength
    NumPut(l_LeftMargin, DRAWTEXTPARAMS,8,"Int")        ;-- iLeftMargin
    NumPut(l_RightMargin,DRAWTEXTPARAMS,12,"Int")       ;-- iRightMargin
	
    VarSetCapacity(RECT,16,0) ;-- Create and populate the RECT structure
    NumPut(l_Width,RECT,8,"Int")                        ;-- right
	
    old_hFont:=DllCall("SelectObject","Ptr",hDC,"Ptr",hFont)
	
	VarSetCapacity(SIZE,8,0) ;-- Initialize
	testW := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ; taken from Fnt_GetAverageCharWidth()
	RC := DllCall("GetTextExtentPoint32","Ptr",hDC,"Str",testW,"Int",StrLen(testW),"Ptr",&SIZE)
	RC := RC ? NumGet(SIZE,0,"Int") : 0
	avgCharWidth := Floor((RC/26+1)/2)
	avgCharHeight := NumGet(SIZE,4,"Int")
	
    VarSetCapacity(l_Text,VarSetCapacity(r_Text)+16,0)
	l_Text:=r_Text ;-- Create a buffer + 16 bytes
	
    DllCall("DrawTextEx"
        ,"Ptr",hDC                                      ;-- hdc [in]
        ,"Str",l_Text                                   ;-- lpchText [in, out]
        ,"Int",-1                                       ;-- cchText [in]
        ,"Ptr",&RECT                                    ;-- lprc [in, out]
        ,"UInt",l_DTFormat                              ;-- dwDTFormat [in]
        ,"Ptr",&DRAWTEXTPARAMS)                         ;-- lpDTParams [in]
	
    DllCall("SelectObject","Ptr",hDC,"Ptr",old_hFont)
	DllCall("ReleaseDC","Ptr",HWND_DESKTOP,"Ptr",hDC) ; avoid memory leak
	
    NumPut(r_Width:=NumGet(RECT,8,"Int"),SIZE,0,"Int") ; get txt rect W
    NumPut(r_Height:=NumGet(RECT,12,"Int"),SIZE,4,"Int") ; get txt rect H
	
	retVal := {}, retVal.h := r_Height, retVal.w := r_Width
	retVal.avgW := avgCharWidth, retVal.avgH := avgCharHeight, retVal.addr := &SIZE
	
	return retVal
}

; ===========================================================================
; created by TheArkive
; Usage: Specify X/Y coords to get info on which monitor that point is on,
;        and the bounds of that monitor.  If no X/Y is specified then the
;        current mouse X/Y coords are used.
; ===========================================================================
GetMonitorData(x:="", y:="") {
	CoordMode Mouse, Screen ; CoordMode Mouse, Screen ; AHK v1
	If (x = "" Or y = "")
		MouseGetPos x, y
	actMon := 0
	
	SysGet, monCount, MonitorCount ; SysGet, monCount, MonitorCount ; AHK v1
	Loop % monCount { ; Loop % monCount { ; AHK v1
		SysGet, m, Monitor, %A_Index% ; SysGet, m, Monitor, A_Index ; AHK v1
		
		If (mLeft = "" And mTop = "" And mRight = "" And mBottom = "")
			Continue
		
		If (x >= (mLeft) And x <= (mRight-1) And y >= mTop And y <= (mBottom-1)) {
			monList := {}, monList.left := mLeft, monList.right := mRight
			monList.top := mTop, monList.bottom := mBottom, monList.active := A_Index
			monList.x := x, monList.y := y
			monList.Cx := ((mRight - mLeft) / 2) + mLeft
			monList.Cy := ((mBottom - mTop) / 2) + mTop
			monList.w := mRight - mLeft, monList.h := mBottom - mTop
			Break
		}
	}
	
	return monList
}

MultiClickDetect(ThisKey, delay:=300, CycleLimit:=0) {	; ThisKey = A_ThisHotKey, or whatever value you pass
    Global MultiClickCount, MultiClickKey			; delay (ms) = expected delay between "clicks"
    ct := MultiClickTickCount()
    
    If ((ct > delay And ct <> "" And ct <> 0) Or (ThisKey <> MultiClickKey And MultiClickKey <> ""))
        MultiClickCount := 0
    Else If (MultiClickCount >= CycleLimit And CycleLimit > 0) ; resets MultiClickCount to 1 on CycleLimit+1
        MultiClickCount := 0 ; useful for firing multiple double/triple/etc clicks without a pause between.
    
    MultiClickKey := ThisKey, MultiClickCount++
    return MultiClickCount
}

MultiClickTickCount() { ; returns the number of ticks (ms) since the last button event (any button)
    Global MultiClickTicksPrev
    CurTicks := A_TickCount
    
    If (MultiClickTicksPrev = "")
        diff := 0, MultiClickTicksPrev := A_TickCount
    Else
        diff := A_TickCount - MultiClickTicksPrev, MultiClickTicksPrev := A_TickCount
    
    return diff
}


; ================================================================
; hotkeys - global
; ================================================================
^+Space:: ; load call tip
	DisplayCallTip()
	; ProcInput()
	; LoadCallTip()
return

^!Space:: ; load auto-complete list
	If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		msgbox "yee haw!"
	}
return

^+F12:: ; close CallTipsForAll
	MsgBox "Closing Call Tips For All!"
	ExitApp
Return

~ESC:: ; close call tip window
	If (callTipGui) {
		Gui, CallTip:Destroy
		callTipGui := "", curIndex := "", fullDescArr := ""
	}
	
	Tooltip
return

Up::
	i := fullDescArr ? fullDescArr.Count() : 0
	If (curIndex And (curIndex-1 != 0)) {
		curIndex--
		Gui, CallTip:Destroy
		callTipGui := ""
		LoadCallTip()
	} Else SendInput {Up}
return

Down::
	i := fullDescArr ? fullDescArr.Count() : 0
	If (curIndex And (curIndex+1) <= i) {
		curIndex++
		Gui, CallTip:Destroy
		callTipGui := ""
		LoadCallTip()
	} Else SendInput {Down}
return

~LButton::
	c := MultiClickDetect("LButton")
	
	hCtl := editorCtlHwnd(hEditorWin,cClassNN,EventType)
	
	If ((cClassNN = "scintilla1" Or "Edit1") And EventType = 0 And c = 1) { ; load calltip
		If (!IsObject(ObjectList))
			ReParseText()
		If (Settings["LoadCallTipOnClick"]) ; load call tip on click if enabled
			DisplayCallTip()
	} Else If (Settings["CloseTipOnFocusChange"] And c = 1) {
		If (callTipGui) {
			Gui, CallTip:Destroy
			callTipGui := "", curIndex := "", fullDescArr := ""
		}
	} Else If (c = 2) {
		If (callTipGui) {
			Gui, CallTip:Destroy
			callTipGui := "", curIndex := "", fullDescArr := ""
			ReParseText()
		}
	}
return

^!w::
	newText := WrapText(WrapTextChars)
	If (newText)
		clipboard := newText
return

^!u::
	newText := unwrapText()
	If (newText)
		clipboard := newText
return



F11:: ; list custom functions, commands, and objects - for debugging List_*.txt files only
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
return

F10:: ; list functions - for debugging List_*.txt files only
	; testList := ""
	; For curName, obj in FunctionList {
		; if (curName = "msgbox") {
			; desc := obj["desc"]
			; testList .= curName "`r`n" desc "`r`n`r`n"
		; }
	; }
		
	; msgbox % "Functions:`r`n`r`n" testList
	
	For level, lvlObj in ObjectCreateList { ; for debug only
		For label, labelObj in lvlObj {
			regex := labelObj["regex"]
			type := labelObj["type"]
			direct := labelObj["direct"]
			testList .= level " / " label " / " type "`r`n" regex "`r`n`r`n"
		}
	}

	msgbox % testList
return




; ==================================================
; hotkeys - #If not working so well???
; ==================================================
; #If WinActive("ahk_exe notepad++.exe") ; WinActive("ahk_exe notepad.exe") Or 

