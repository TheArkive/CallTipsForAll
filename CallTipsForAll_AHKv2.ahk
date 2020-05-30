; AHK v2
; === comment out if using this script as a library and these are already determined. ===
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================

#INCLUDE <_Jxon_v2>
#INCLUDE <_Font_Picker_Dialog_v2>
#INCLUDE <_Color_Picker_Dialog_v2>
#INCLUDE <TheArkive_Debug>

FileEncoding "UTF-8"

Global SettingsGUI, AutoCompleteGUI, callTipGui
Global WrapTextChars, AutoCompleteLength, useTooltip, maxLines ; , maxWidth
Global callTipSelectable

Global oCallTip
oCallTip := {}
oCallTip.curIndex := "", oCallTip.fullDescArr := "", oCallTip.helpFile := ""
oCallTip.ctlClassNN := "", oCallTip.ctlHwnd := 0, oCallTip.progHwnd := 0
oCallTip.curPhrase := ""

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
; For example if you enable LoadCallTipOnClick and closeTipOnClick or closeTipOnLButton,
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
settingsText := FileRead("Settings.txt")
Settings := Jxon_Load(settingsText) ; MapObject

hCtl := editorCtl()
If (hCtl.progHwnd) {
	oCallTip.ctlHwnd := hCtl.ctlHwnd
	oCallTip.ctlClassNN := hCtl.ctlClassNN
	oCallTip.progHwnd := hCtl.progHwnd
	ReParseText() ; initial loading of functions, custom functions, objects
}

; ======================================================================================
; Tray Menu
; ======================================================================================
subMenuAutoGen := MenuCreate()
subMenuAutoGen.Add("Generate Commands","iconMenu")

trayMenu := A_TrayMenu
trayMenu.Delete()
trayMenu.Add("Settings","iconMenu")
trayMenu.Add()
trayMenu.Add("ReWrap Text (CTL + ALT + W)","iconMenu")
trayMenu.Add("UnWrap Text (CTL + ALT + U)","iconMenu")
trayMenu.Add("AHK Auto-Gen",subMenuAutoGen)
trayMenu.Add("Reload","iconMenu")
trayMenu.Add("Exit","iconMenu")

iconMenu(ItemName, ItemPos, MenuObj) { ; MenuObject
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
	If (!StrLen(clipboard))
		return ""
	
	inText := clipboard
	endCRLF := false
	If (result := RegExMatch(inText,"(.*[\r\n]+$)",match))
		endCRLF := true
	
	inText := StrReplace(clipboard,"`r`n",Chr(1))
	a := StrSplit(inText,Chr(1))
	
	Loop a.Length {
		curLine := a[A_Index]
		
		If (curLine) {
			len := StrLen(curLine), s := 1
			While (s <= len) {
				curChunk := LTrim(SubStr(curLine,s,x))
				
				y := 0
				lastChar := SubStr(curChunk,-1)
				nextChar := SubStr(curLine,s+x,1)
				
				nextCharStat := false, lastCharStat := false
				If !(nextChar is "space")
					nextCharStat := true
				If !(lastChar is not "space")
					lastCharStat := true
				
				If ((nextCharStat) And (lastCharStat) And s <= len) {
					y := 1
					While(lastChar := SubStr(curChunk,y*-1,1)) {
						lastCharStat := false
						If !(lastChar is "space")
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
	
	Loop a.Length {
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

; IH := InputHook("V I1","","") ; options , endKeys , matchList
; IH.OnKeyDown := Func("keyPress"), IH.KeyOpt("{All}","N"), IH.Start()

; keyPress(iHook, VK, SC) { ; InputHookObject
	; curKey := "sc" Format("{:X}",SC)
	
	; If (curKey != "sc118" And curKey != "sc117") { ; laptop close / open
		; state := 0
		; Try {
			; state := GetKeyState(curKey,"P")
		; } Catch e {
			; msgbox "State: " state "`r`n... is invalid.`r`n`r`n" curKey
		; }
		
		; if (state) {
			; SetTimer "LoadAutoComplete", -100
			; SetTimer "ReParseText", -500
		; }
		
		; iHook.Stop()
		; IH := InputHook("V I1","","")
		
		; IH.OnKeyDown := Func("keyPress"), IH.KeyOpt("{All}","N"), IH.Start()
	; }
; }

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
; Create function and command list for call tips
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
			
			If (curFileType = "BeginEnd") {
				Loop Parse curSubStr, "`n", "`r"
				{
					If (A_Index = 1)
						oCallTip.funcBeginStr := Trim(SubStr(A_LoopField,7))
					Else If (A_Index = 2)
						oCallTip.funcEndStr := Trim(SubStr(A_LoopField,5))
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

; ======================================================================================
; Load Call Tip
; ======================================================================================
LoadCallTip() { ; curPhrase, curPhraseType ---> globals
	fontFace := Settings["fontFace"]
	fontSize := Settings["fontSize"]
	fontColor := Settings["fontColor"]
	bgColor := Settings["bgColor"]
	
	curPhrase := oCallTip.curPhrase ; Global curPhrase, curPhraseObj, curPhraseType, parentObj, parentObjType
	curPhraseObj := oCallTip.curPhraseObj
	curPhraseType := oCallTip.curPhraseType
	parentObj := oCallTip.parentObj
	parentObjType := oCallTip.parentObjType
	
	If (curPhrase = "")
		return
	
	If (!oCallTip.curIndex)
		oCallTip.curIndex := 1
	curIndex := oCallTip.curIndex
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
		
		If (!descArr.Length) {
			For funcName, obj in CustomFunctions {
				If (funcName = curPhrase) {
					descStr := obj["desc"]
					descArr.Push(descStr) ; make array from str
					break
				}
			}
		}
		
		If (!descArr.Length)
			return
		Else {
			fullDescArr := Map()
			For index, desc in descArr {
				curDescObj := Map()
				curDescObj["desc"] := desc
				curDescObj["helpLink"] := helpLink
				fullDescArr[index] := curDescObj
			}
			curDescObj := "", descArr := ""
		}
	} Else If (curPhraseType = "object") {
		obj := ObjectList.Has(curPhrase) ? ObjectList[curPhrase] : ""
		If (!obj)
			return
		
		fullDescArr := Map(), i := 1
		For objType, typeObj in obj {
			listObj := MethPropList.Has(objType) ? MethPropList[objType] : ""
			
			If (listObj) {
				descArr := listObj["desc"]
				helpLink := listObj["helpLink"]
				
				For index, desc in descArr {
					curObj := Map()
					curObj["desc"] := desc, curObj["helpLink"] := helpLink
					fullDescArr[i] := curObj
					i++
				}
			}
		}
		curObj := "", listObj := "", obj := ""
	} Else If (curPhraseType = "method" Or curPhraseType = "property") {
		obj := ObjectList.Has(parentObj) ? ObjectList[parentObj] : ""
		If (!obj)
			return
		
		fullDescArr := Map(), i := 1
		For objType, typeObj in obj {
			listObj := MethPropList.Has(objType) ? MethPropList[objType] : ""
			memObj := listObj[curPhraseType]
			
			For methPropName, descObj in memObj {
				If (methPropName = curPhrase) {
					descArr := descObj["desc"]
					helpLink := descObj["helpLink"]
					helpLink := StrReplace(helpLink,"[MemberName]",methPropName)
					
					For index, desc in descArr {
						titleHeader := StrUpper(curPhraseType,"T")
						desc := objType " " titleHeader ":`r`n`r`n" StrReplace(desc,"[MemberName]",methPropName)
						curObj := Map()
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
	
	oCallTip.fullDescArr := fullDescArr
	fullDescArrObj := fullDescArr[curIndex]
	fullDesc := fullDescArrObj["desc"]
	
	Loop Parse fullDesc, "`n", "`r"
		newFullDesc .= "`r`n" RegExReplace(A_LoopField,"^/","`r`n")
	fullDesc := Trim(newFullDesc,"`r`n")
	
	If (!fullDesc)
		return
	
	curMon := GetMonitorData(outX,outY) ; get actvie monitor dims and L/R/T/B
	dims := GetTextDims(fullDesc,fontFace,fontSize,maxWidth)
	
	If (dims.w > curMon.w)
		dims := GetTextDims(fullDesc,fontFace,fontSize,curMon.w * 0.75)
	
	vscroll := "-VScroll"
	
	CoordMode "Caret", "Screen"
	CaretGetPos(outX, outY)
	
	If (useTooltip) {
		outX += -70, outY += -25
		Tooltip fullDesc, outX, outY
	} Else {
		outX += -11, outY += 20
		
		If (callTipGui)
			callTipGui.Destroy()
		
		callTipGui := GuiCreate("-Border AlwaysOnTop +Owner" hEditorWin) ; Splitpath drive
		callTipGui.BackColor := bgColor
		callTipGui.SetFont("s" fontSize " c" fontColor,fontFace)
		
		If (!Settings["CallTipSelectable"]) {
			ctl := callTipGui.Add("Text","w" dims.w " h" dims.h " x5 y5",fullDesc) ; " +c" fontColor 
		} Else {
			scrollW := SysGet(2)
			ctl := callTipGui.Add("Edit","w" (dims.w+(scrollW*2)) " h" dims.h " x5 y5 +Background" bgColor " " vscroll " -E0x200 ReadOnly",fullDesc)
		}
		
		If (!Settings["CallTipSelectable"])
			ctl.OnEvent("Click","gui_click")
		
		dCtl := ctl.pos
		posTest := OutX + dCtl.w
		
		If (posTest > curMon.right) {
			offset := posTest - curMon.right
			OutX := OutX - offset - 20
		}
		
		h := dCtl.h + 10, w := dCtl.w + 10
		callTipGui.Show("x" OutX " y" outY " h" h " w" w " NA NoActivate")
	}
}

gui_click(ctlObj,info) {
	curIndex := oCallTip.curIndex
	fullDescArrObj := oCallTip.fullDescArr[curIndex]
	fullDesc := fullDescArrObj["desc"]
	link := fullDescArrObj["helpLink"]
	helpFile := oCallTip.helpFile
	
	If (InStr(link,"/") = 1 And FileExist(helpFile)) {
		helpFile := StrReplace(helpFile," ","%20")
		cmd := "hh.exe mk:@MSITStore:" helpFile "::" Trim(link,"`r`n")
		Run cmd			  
		callTipGui.Destroy()
		callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
	}
	
	If (callTipGui And closeTipOnClick) {
		callTipGui.Destroy()
		callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
	}
}

; ======================================================================================
; Load auto-complete
; ======================================================================================
LoadAutoComplete() {
	ProcInput() ; update cursor postion and curPhrase in global variables
	
	curPhrase := oCallTip.curPhrase ; Global curPhrase, curPhraseObj, curPhraseType, parentObj, parentObjType
	
	KeywordFilter := Map()
	If (StrLen(curPhrase) >= AutoCompleteLength) { ; not done yet!
		For kw, type in KeywordList {
			If (InStr(kw,curPhrase))
				KeywordFilter[kw] := type
		}
		; msgbox KeywordFilter.Count
		
	}
}

AutoCompleteGUI(KeywordFilter) {
	If (IsObject(AutoCompleteGUI))
		AutoCompleteGUI.Destroy()
	
	
}


; ================================================================
; Reads current line / cursor position and determines current top-level function.
; Not currently used in any meaningful way.
; ================================================================
GetTopLevelFunc(sInput, curCol, ByRef funcStart, ByRef funcEnd) {
	result := RegExMatch(sInput,"([\w\.]+\(.*?\))",match)
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
			
			result := RegExMatch(sInput,"([\w\.]+\(.*?\))",match,funcStart + StrLen(curFunc) - 1)
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
	funcName := RegExMatch(sInput,"^([\w\.]*)",match), funcName := match.Value(1)
	paramStrStart := StrLen(funcName) + 2, paramStr := SubStr(sInput,paramStrStart,-1)
	result := RegExMatch(paramStr,"([\w\.]+\(.*?\))",match) ; similar to above
	
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
			result := RegExMatch(paramStr,"([\w\.]+\(.*?\))",match)
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
	paramList := Map()
	paramArr := StrSplit(paramStr,Chr(44))
	paramBaseLine := funcStart + StrLen(funcName) + 1
	paramStart := paramBaseLine
	
	Loop paramArr.Length {
		curParam := paramArr[A_Index], curParamLen := StrLen(curParam)
		curParamText := SubStr(lineText,paramStart,curParamLen)
		
		paramEnd := paramStart + curParamLen - 1
		If (curCol >= paramStart And curCol <= paramEnd)
			curParamNum := A_Index
		
		paramObj := Map()
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
	While (result := RegExMatch(curLineNoStr,"(" Chr(34) ".*?" Chr(34) ")",match)) {	; which helps properly match strings
		repStr := ""
		If (IsObject(match)) {
			Loop match.Len(1)
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
	p1 := RegExMatch(Lhalf,"([#?\w\.]+)$",match), p1 := IsObject(match) ? Trim(match.Value(1)," `t") : ""
	p2 := RegExMatch(Rhalf,"^(#?[\w\.]+)",match2), p2 := IsObject(match2) ? Trim(match2.Value(1)," `t") : ""
	curPhraseOut := p1 p2 ? p1 p2 : ""
	curPhraseStartOut := (p1 or p2) ? (p1 ? match.Pos(1) : curCol) : 0
	return curPhraseOut
}

; ================================================================
; Generates the current "phrase" without dots (.)
; ================================================================
getCurPhrase(curLineNoStr, curCol, ByRef curPhraseStartOut) {
	Lhalf := SubStr(curLineNoStr,1,curCol-1), Rhalf := SubStr(curLineNoStr,curCol) ; split line at curCol
	p1 := RegExMatch(Lhalf,"(#?[\w]+)$",match), p1 := IsObject(match) ? Trim(match.Value(1)," `t") : ""
	p2 := RegExMatch(Rhalf,"^(#?[\w]+)",match2), p2 := IsObject(match2) ? Trim(match2.Value(1)," `t") : ""
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
	
	curPhrase := oCallTip.curPhrase ; Global curPhrase, curPhraseObj, curPhraseType, parentObj, parentObjType
	
	aStr := StrSplit(phraseObj,".")
	Loop aStr.Length {
		curBit := aStr[A_Index]
		fullPhrase .= curBit "."
		If (curBit = curPhrase)
			Break
	}
	fullPhrase := Trim(fullPhrase,".")
	
	If (!phraseObj And !funcName)
		return ""
	
	aStr := StrSplit(fullPhrase,".")
	If (!IsObject(aStr) Or aStr.Length = 0)
		return ""
	
	pOn := aStr.Length - 1
	sObj := aStr.Has(pOn) ? aStr[pOn] : ""
	
	If (ObjectList.Has(sObj))
		return sObj
	Else If (ObjectList.Has(curPhrase))
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
CreateObjList(curDocText) { ; v2 - loops full text in one chunk, hopefully uses less CPU
	oList := Map()
	
	For level, lvlObj in ObjectCreateList {
		For label, lblObj in lvlObj {
			i := 1, type := lblObj["type"], regex := lblObj["regex"], direct := lblObj["direct"]
			
				curPos := 1
				
				If (direct) {
					While(result := RegExMatch(curDocText,regex,match,curPos)) {
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
								
								While (result := RegExMatch(curDocText,newRegex,match,curPos)) {
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
		While (result := RegExMatch(curDocText,newRegex,match,curPos) And doMatch) {
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
GetCustomFunctions(curDocArr) {
	funcList := Map()
	Loop curDocArr.Length {
		curDocLine := curDocArr[A_Index]
		
		result := RegExMatch(curDocLine,oCallTip.funcBeginStr,match) ; funcBeginStr is defined in List_Functions.txt
		If (IsObject(match) And match.Count()) {
			funcName := match.Value(1)
			params := match.Value(2)
			if (funcName != "") {
				funcBody := curDocLine
				obj := Map()
				obj["type"] := "CustomFunction"
				obj["desc"] := funcName params
				obj["line"] := A_Index
				funcList[funcName] := obj
			}
		} Else If (funcBody)
			funcBody .= "`r`n" curDocLine
		
		If (RegExMatch(curDocLine,oCallTip.funcEndStr) And funcName) { ; funcEndStr is defined in List_Functions.txt
			obj["funcBody"] := funcBody
			
			returnObj := Map()
			funcBodyArr := StrSplit(funcBody,"`r","`n")
			Loop funcBodyArr.Length { ; compile function body and search for "return" lines
				fLine := funcBodyArr[A_Index]
				If (RegExMatch(fLine,"return[ \t]+([\w]+)",match)) {
					If (match.Count()) {
						cMatch := match.Value(1)
						returnObj[cMatch] := match.Pos(1)
					}
				}
			}
			
			If (returnObj.Count > 0)
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
	; msgbox curDocText
	curDocArr := StrSplit(curDocText,"`n","`r")
	CustomFunctions := GetCustomFunctions(curDocArr)

	objMatchText := LoadMethPropList()
	LoadObjectCreateList(objMatchText)

	ObjectList := CreateObjList(curDocText)
	curDocArr := "", curDocText := ""
}

ProcInput() {
	If (!IsObject(ObjectList) Or !IsObject(FunctionList) Or !IsObject(CustomFunctions))
		return
	
	cClassNN := oCallTip.ctlClassNN, hEditorWin := oCallTip.progHwnd, hCtl := oCallTip.ctlHwnd
	
	curDocText := ControlGetText(cClassNN,"ahk_id " hEditorWin)
	curDocArr := StrSplit(curDocText,"`n","`r")
	
	curCol := ControlGetCurrentCol(hCtl) ; global
	curLine := ControlGetCurrentLine(hCtl) ; global
	
	If (!curLine)
		return
	If (!curDocArr.Has(curLine))
		return
	
	curLineText := curDocArr[curLine]
	curLineNoStr := StringOutline(curLineText) ; blank out strings with "****"
	
	curPhrase := getCurPhrase(curLineNoStr,curCol,curPhraseStart) ; curPhraseStart: ByRef
	oCallTip.curPhrase := curPhrase
	
	curPhraseObj := getCurPhraseObj(curLineNoStr,curCol,curPhraseObjStart)
	parentObj := GetParentObj(curPhraseObj,curMethProp) ; curMethProp: ByRef
	
	oCallTip.curPhraseObj := curPhraseObj
	oCallTip.parentObj := parentObj
	oCallTip.curPhraseType := ""
	oCallTip.parentObjType := ""
	
	parentObjTypeList := ObjectList.Has(parentObj) ? ObjectList[parentObj] : Map()
	
	; parentObjType := (parentObj And ObjectList.Has(parentObj)) ? ObjectList[parentObj]["type"] : ""
	
	; topFunc := GetTopLevelFunc(curLineNoStr,curCol,funcStart,funcEnd) ; funcStart, funcEnd: ByRef - not currently used
	; funcText := topFunc ? SubStr(curLineText,funcStart,StrLen(topFunc)) : ""
	
	; === primary data bits ===
	; curPhrase, curPhraseStart, funcName
	; curPhraseObj, curPhraseObjStart, parentObj, parentObjType
	; curParamNum, curParamText
	; arrays: CustomFunctions, ObjectList, MethPropList, paramList
	
	For curFuncName, obj in FunctionList {
		If (curFuncName = curPhrase) {
			curPhraseType := obj["type"], oCallTip.curPhraseType := curPhraseType
			Break
		}
	}
	
	If (!curPhraseType) {
		For curFuncName, obj in CustomFunctions {
			If (curFuncName = curPhrase) {
				curPhraseType := obj["type"], oCallTip.curPhraseType := curPhraseType
				Break
			}
		}
	}
	
	If (!curPhraseType) {
		For curObjName in ObjectList {
			If (curObjName = curPhrase) {
				curPhraseType := "object", oCallTip.curPhraseType := curPhraseType
				Break
			}
		}
	}
	
	If (!curPhraseType And parentObjTypeList.Count) {
		For objType in parentObjTypeList {
			methList := MethPropList[objType]["method"]
			For methName in methList { ; MethPropList.count
				If (methName = curPhrase) {
					curPhraseType := "method", oCallTip.curPhraseType := curPhraseType
					parentObjType := objType, oCallTip.parentObjType := parentObjType
					Break
				}
			}
		}
	}
	
	If (!curPhraseType And parentObjTypeList.Count) {
		For objType in parentObjTypeList {
			propList := MethPropList[objType]["property"]
			For propName in propList {
				If (propName = curPhrase) {
					curPhraseType := "property", oCallTip.curPhraseType := curPhraseType
					parentObjType := objType, oCallTip.parentObjType := parentObjType
					Break
				}
			}
		}
	}
}

debugToolTip() {
	curPhrase := oCallTip.curPhrase ; Global curPhrase, curPhraseObj, curPhraseType, parentObj, parentObjType
	curPhraseObj := oCallTip.curPhraseObj
	curPhraseType := oCallTip.curPhraseType
	parentObj := oCallTip.parentObj
	parentObjType := oCallTip.parentObjType
	
	ProcInput()
	If (curPhrase) { ; for debugging while allowing user input - requires InputHook up top
		CaretGetPos(x,y)
		Tooltip "curPhrase :" curPhrase ": / curPhraseObj: " curPhraseObj
		      . "`r`nfuncName: " funcName " / funcText: " funcText
		      . "`r`nparentObj: " parentObj " / parentObjType: " parentObjType 
			  . "`r`nphraseType: " curPhraseType, x, (y+30)
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
	curFolder := DirSelect("","","Select COMMANDS folder:")
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
			curText := FileRead(A_LoopFileFullPath)
			curType := "Command"
			
			curTitleMatch1 := "<h1>(.*?)</h1>"
			result := RegExMatch(curText,curTitleMatch1,match)
			If (IsObject(match) And match.Count()) {
				curTitle := match.Value(1)
				If (curTitle = "{...} (block)")
					curTitle := "Block"
			}
			curTitle := Trim(RegExReplace(curTitle,"<span class=`"ver`">.*?</span>",""))
			curTitle := StrReplace(curTitle,"("), curTitle := StrReplace(curTitle,")")
			
			curMatch1 := "s)<pre class=`"Syntax`"[^>]*>(.*?)\Q</pre>\E", curPos := 1
			While (result := RegExMatch(curText,curMatch1,match,curPos)) {
				If (IsObject(match) And match.Count()) {
					curSyntax := match.Value(1)
					curSyntax := RegExReplace(curSyntax,"s)(<span class=`"optional`"[^>]*>)(.*?)(\Q</span>\E)","[$2]")
					curSyntax := RegExReplace(curSyntax,"s)<span class=`"func`"[^>]*>(.*?)\Q</span>\E","$1")
					curSyntax := RegExReplace(curSyntax,"s)<i>(.*?)\Q</i>\E","$1")
					curSyntax := RegExReplace(curSyntax,"s)<a href[^>]*>(.*?)\Q</a>\E","$1")
					curSyntax := RegExReplace(curSyntax,"<span class=`"ver`">.*?\Q</span>\E","")
					curSyntax := RegExReplace(curSyntax,"<em>.*?\Q</em>\E","")
					
					If (!multiSyntax)
						fullSyntax .= Trim(curSyntax,"`r`n") "`r`n"
					Else
						multiSyntaxArr.Push(curSyntax)
					
					curPos := match.Pos(1) + match.Len(1)
				}
			}
			
			If (curTitle and (fullSyntax Or multiSyntaxArr.Length)) {
				curTitle := RegExReplace(curTitle,"<em>(.*?)\Q</em>\E","$1")
				curTitleArr := StrSplit(curTitle,"/")
				curHelpLink := "/docs/commands/" A_LoopFileName
				
				If (!multiSyntaxArr.Length) {
					If (InStr(fullSyntax,"(") And InStr(fullSyntax,")"))
						curType := "Function"
					Loop curTitleArr.Length {
						curEntry := curType "`r`n" Trim(curTitleArr[A_Index]) "`r`n" Chr(96) Trim(fullSyntax,"`r`n") "`r`n" Chr(96) curHelpLink
						fullList .= curEntry "`r`n`r`n`r`n"
					}
				} Else {
					Loop multiSyntaxArr.Length {
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
	FileDelete destFile
	FileAppend fullList, destFile
	MsgBox "Commands list complete."
	Run "notepad.exe " Chr(34) destFile Chr(34)
}

; ================================================================
; Settings GUI
; ================================================================

SettingsGUI() {
	SettingsGUI := GuiCreate("AlwaysOnTop","CallTipsForAll v2")
	SettingsGUI.OnEvent("Close","gui_close")
	SettingsGUI.OnEvent("escape","gui_close")
	ActiveLanguage := Settings["ActiveLanguage"]
	
	If (IsObject(callTipGui)) {
		callTipGui.Destroy()
		callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
	}
	
	Loop Files A_ScriptDir "\Languages\*", "D"
		langList .= (A_LoopFileName = ActiveLanguage) ? A_LoopFileName "||" : A_LoopFileName "|"
	
	langList := (SubStr(langList,-2) = "||") ? langList : Trim(langList,"|")
	
	SettingsGUI.AddText("x5 y10","Language:")
	SettingsGUI.AddDropDownList("x+2 yp-4 w100 vPickLang",langList).OnEvent("change","gui_change_events")
	
	ctl := SettingsGUI.AddCheckbox("xm y+8 vLoadCallTipOnClick","Load call tip on double-click")
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["LoadCallTipOnClick"]
	
	ctl := SettingsGUI.AddCheckbox("x+8 vCloseTipOnFocusChange Section","Close call tip on focus change")
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["CloseTipOnFocusChange"]
	
	ctl := SettingsGUI.AddCheckbox("xm y+8 vDebugToolTip","Show Debug Tooltip")
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["DebugToolTip"]
	
	ctl := SettingsGUI.AddCheckbox("xs yp vCallTipSelectable","Selectable call tips")
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["CallTipSelectable"]
	
	ctl := SettingsGUI.AddButton("vPickFont xm y+8","Select Font")
	ctl.OnEvent("click","gui_change_events")
	textW := ctl.pos.w
	
	ctl := SettingsGUI.AddButton("vPickFontColor x+0","Select Font Color")
	ctl.OnEvent("click","gui_change_events")
	textW += ctl.pos.w
	
	ctl := SettingsGUI.AddButton("vPickBgColor x+0","Select Background Color")
	ctl.OnEvent("click","gui_change_events")
	
	textW += ctl.pos.w + 30
	
	SettingsGUI.AddEdit("vFontDemo xm y+8 w330 h50 ReadOnly -E0x200 -VScroll","Call Tip Test")
	
	SettingsGUI.AddText("xm y+12","Editor EXE:")
	SettingsGUI.AddEdit("vProgExe xm y+2 w330")
	
	SettingsGUI.AddText("xm y+8","Editor Control ClassNN - no number:")
	SettingsGUI.AddEdit("vProgClassNN xm y+2 w330")
	
	SettingsGUI["ProgExe"].Value := Settings["ProgExe"]
	SettingsGUI["ProgClassNN"].Value := Settings["ProgClassNN"]
	
	SetFontDemo()
	SettingsGUI.Show()
}

gui_change_events(ctl, Info) { ; GuiControlObject
	if (ctl.Name = "PickLang")
		Settings["ActiveLanguage"] := ctl.Text
	else if (ctl.Name = "LoadCallTipOnClick")
		Settings["LoadCallTipOnClick"] := ctl.Value
	else if (ctl.Name = "CloseTipOnFocusChange")
		Settings["CloseTipOnFocusChange"] := ctl.Value
	else if (ctl.Name = "DebugToolTip")
		Settings["DebugToolTip"] := ctl.Value
	else if (ctl.Name = "CallTipSelectable")
		Settings["CallTipSelectable"] := ctl.Value
	Else If (ctl.Name = "PickFont") {
		fName := Settings["fontFace"]
		fSize := Settings["fontSize"]
		fontObj := Map("name",fName,"size",fSize) ; ,"color",0xFF0000
		fontObj := FontSelect(fontObj,ctl.gui.hwnd,0)
		If (fontObj) {
			Settings["fontFace"] := fontObj["name"]
			Settings["fontSize"] := fontObj["size"]
			SetFontDemo()
		}
	} Else If (ctl.Name = "PickFontColor") {
		fontColor := Settings["fontColor"]
		fontColor := ColorSelect(fontColor,ctl.gui.hwnd)
		If (fontColor > -1) {
			Settings["fontColor"] := fontColor
			SetFontDemo()
		}
	} Else If (ctl.Name = "PickBgColor") {
		bgColor := Settings["bgColor"]
		bgColor := ColorSelect(bgColor,ctl.gui.hwnd)
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
	
	SettingsGUI["FontDemo"].SetFont("s" fontSize " c" fontColor,fontFace)
	SettingsGUI["FontDemo"].Opt("+Background" bgColor)
}

gui_close(guiObj) {
	SetTimer "ReParseText", -500
	Settings["ProgExe"] := guiObj["ProgExe"].value
	
	settingsText := Jxon_Dump(Settings,4)
	FileDelete "Settings.txt"
	FileAppend settingsText, "Settings.txt"
	SettingsGUI.Destroy(), SettingsGUI := ""
}

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
	
	; msgbox "found: " found " / " ClassNNcomp " / " hwndComp
	
	If (!found)
		return {ctlClassNN: "", progHwnd: 0, ctlHwnd: 0}
	
	retVal := {ctlClassNN: ClassNNcomp, progHwnd: hwndComp}
	ctlHwnd := ControlGetHwnd(ClassNNcomp,"ahk_id " hwndComp)
	retVal.ctlHwnd := ctlHwnd
	
	return retVal
}

closeCallTip() {
	If (useToolTip)						; close call tip (tooltip version)
		ToolTip
	Else If (IsObject(callTipGui)) {
		callTipGui.Destroy()
		callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
		WinActivate "ahk_id " oCallTip.progHwnd
	}
}

ClickCheck(curKey) {
	hCtl := editorCtl("click")
	If (!hCtl.progHwnd Or !hCtl.ctlHwnd) {
		closeCallTip()
		return
	}
	
	c := MultiClickDetect(curKey)
	
	oCallTip.ctlHwnd := hCtl.ctlHwnd
	oCallTip.ctlClassNN := hCtl.ctlClassNN
	oCallTip.progHwnd := hCtl.progHwnd
	
	If (curKey = "~LButton") {
		If (c = 1 and Settings["CloseTipOnFocusChange"] And !hCtl.progHwnd)
			closeCallTip()
		
		If (c = 2 And Settings["LoadCallTipOnClick"])
			DisplayCallTip()
	}
}

DisplayCallTip() {
	cClassNN := oCallTip.ctlClassNN
	
	If (cClassNN != "scintilla1" And cClassNN != "Edit1")
		return
	
	closeCallTip()
	
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
	
    DRAWTEXTPARAMS := BufferAlloc(20,0) ;-- Create and populate DRAWTEXTPARAMS structure
	NumPut "UInt", 20, "Int", l_TabLength, "Int", l_LeftMargin, "Int", l_RightMargin, DRAWTEXTPARAMS
	
	RECT := BufferAlloc(16,0)
	NumPut "Int", l_Width, RECT, 8 ;-- right
	
    old_hFont := DllCall("SelectObject","Ptr",hDC,"Ptr",hFont)
	
	SIZE := BufferAlloc(8,0) ;-- Initialize
	testW := "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ; taken from Fnt_GetAverageCharWidth()
	RC := DllCall("GetTextExtentPoint32","Ptr",hDC,"Str",testW,"Int",StrLen(testW),"Ptr",SIZE.Ptr)
	RC := RC ? NumGet(SIZE,0,"Int") : 0
	avgCharWidth := Floor((RC/26+1)/2)
	avgCharHeight := NumGet(SIZE,4,"Int")
	
	strBufSize := StrPut(r_Text)
	l_Text := BufferAlloc(strBufSize+16,0)
	StrPut r_Text, l_Text ; , (l_Text.Size+1) ; not specifying size
	
    DllCall("DrawTextEx"
        ,"Ptr",hDC                                      ;-- hdc [in]
        ,"Ptr",l_Text.Ptr ; orig type = "Str"           ;-- lpchText [in, out]
        ,"Int",-1                                       ;-- cchText [in]
        ,"Ptr",RECT.Ptr                                 ;-- lprc [in, out]
        ,"UInt",l_DTFormat                              ;-- dwDTFormat [in]
        ,"Ptr",DRAWTEXTPARAMS.Ptr)                      ;-- lpDTParams [in]
	
    DllCall("SelectObject","Ptr",hDC,"Ptr",old_hFont)
	DllCall("ReleaseDC","Ptr",HWND_DESKTOP,"Ptr",hDC) ; avoid memory leak
	
	r_Width := NumGet(RECT,8,"Int")
	r_Height := NumGet(RECT,12,"Int")
	NumPut "Int", r_Width, "Int", r_Height, SIZE ; write H/W to SIZE rect structure
	
	retVal := {}, retVal.h := r_Height, retVal.w := r_Width
	retVal.avgW := avgCharWidth, retVal.avgH := avgCharHeight, retVal.addr := SIZE.Ptr
	
	return retVal
}

; ===========================================================================
; created by TheArkive
; Usage: Specify X/Y coords to get info on which monitor that point is on,
;        and the bounds of that monitor.  If no X/Y is specified then the
;        current mouse X/Y coords are used.
; ===========================================================================
GetMonitorData(x:="", y:="") {
	CoordMode "Mouse", "Screen" ; CoordMode Mouse, Screen ; AHK v1
	If (x = "" Or y = "")
		MouseGetPos x, y
	actMon := 0
	
	monCount := MonitorGetCount() ; SysGet, monCount, MonitorCount ; AHK v1
	Loop monCount { ; Loop % monCount { ; AHK v1
		MonitorGet(A_Index,mLeft,mTop,mRight,mBottom) ; SysGet, m, Monitor, A_Index ; AHK v1
		
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
    
	MultiClickCount := (MultiClickCount = "") ? 0 : MultiClickCount
    If ((ct > delay And ct != "" And ct != 0) Or (ThisKey != MultiClickKey And MultiClickKey != ""))
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
; hotkeys - global ; 
; ================================================================

^Space::ReParseText()

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

~ESC::closeCallTip()

Up::
	i := oCallTip.fullDescArr ? oCallTip.fullDescArr.Count : 0
	If (oCallTip.curIndex And (oCallTip.curIndex-1 != 0)) {
		oCallTip.curIndex--
		callTipGui.Destroy()
		callTipGui := ""
		LoadCallTip()
	} Else SendInput "{Up}"
return

Down::
	i := oCallTip.fullDescArr ? oCallTip.fullDescArr.Count : 0
	If (oCallTip.curIndex And (oCallTip.curIndex+1) <= i) {
		oCallTip.curIndex++
		callTipGui.Destroy()
		callTipGui := ""
		LoadCallTip()
	} Else SendInput "{Down}"
return

~LButton::ClickCheck(A_ThisHotkey)
~MButton::closeCallTip()

; ^LButton::ClickCheck(A_ThisHotkey)
	
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
		
	msgbox "Custom Functions:`r`n`r`n" testList
	
	testList := ""
	For objName, obj in ObjectList {
		For curType, obj2 in obj {
			type := curType, label := obj2["label"], match := obj2["match"]
			testList .= objName " / " label " / " type "`r`n" match "`r`n`r`n"
		}
	}
	msgbox ObjectList.Count "`r`nObjectList:`r`n`r`n" testList
return

F10:: ; list functions - for debugging List_*.txt files only
	; testList := ""
	; For curName, obj in FunctionList {
		; if (curName = "msgbox") {
			; desc := obj["desc"]
			; testList .= curName "`r`n" desc "`r`n`r`n"
		; }
	; }
		
	; msgbox "Functions:`r`n`r`n" testList
	
	For level, lvlObj in ObjectCreateList { ; for debug only
		For label, labelObj in lvlObj {
			regex := labelObj["regex"]
			type := labelObj["type"]
			direct := labelObj["direct"]
			testList .= level " / " label " / " type "`r`n" regex "`r`n`r`n"
		}
	}
	msgbox testList
return




; ==================================================
; hotkeys - #If not working so well???
; ==================================================
; #If WinActive("ahk_exe notepad++.exe") ; WinActive("ahk_exe notepad.exe") Or 

