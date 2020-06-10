; AHK v2
; === comment out if using this script as a library and these are already determined. ===
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================

FileEncoding "UTF-8"

class oCallTip {
	Static srcFiles := "", c := ""
	Static captureKeys := "abcdefghijklmnopqrstuvwxyz1234567890.,-=[]{Space}{Backspace}{Up}{Down}{Left}{Right}{Enter}{Tab}"
	
	; properties for call display and help link
	Static curIndex := "", fullDescArr := Map(), helpFile := ""
	
	; properties for parent window and editor control
	Static ctlHwnd := 0, progHwnd := 0, progTitle := "", ctlActive := false, ctlConfirmed := false
	
	; properties for text elements near the caret
	Static curPhrase := "", curPhraseObj := "", parentObj := "", curPhraseType := "", parentObjType := ""
	
	; properties for regex matches
	Static funcStart := "", funcEnd := "", funcReturn := "", classStart := "", classEnd := "", includes := ""
	Static classMethodStart := "", classMethodEnd := "", classMethodOneLine := ""
	Static classPropertyStart := "", classPropertyEnd := "", classPropertyOneLine := ""
	Static classSmallPropExt := "", classSmallPropInt := "", classInstance := ""
}

Global SettingsGUI, AutoCompleteGUI, callTipGui

Global ClassesList, ObjectCreateList, ObjectList ; internal list for keeping track of objects and obj types
Global MethPropList, UserObjMethProp, FunctionList, CustomFunctions, KeywordList ; internal lists for indicated types
Global Settings ; user settings object
Global entryEnd := "`r`n`r`n`r`n" ; used to determine expected separation between elements in language files

; eventually move these to settings GUI
Global WrapTextChars := 80
Global AutoCompleteLength := 0			; Auto-Complete won't trigger until X chars have been typed
Global useTooltip := false				; ignores fontFace, fontSize, and callTipSelectable
Global maxLines := 20

; OnMessage 0x0100, "WM_KEYDOWN" ; WM_KEYDOWN control

IH := InputHook("V I1","","") ; options , endKeys , matchList
IH.OnKeyDown := Func("keyPress")
IH.KeyOpt(oCallTip.captureKeys,"N") ; don't capture all keys, that causes problems
IH.Start()

Settings := ReadSettingsFromFile() ; Map
AddTrayMenu() ; modify tray menu

If (!Settings["ProgClassNN"] Or !Settings["ProgExe"]) ; prompt user for important settings (if these are blank)
	SettingsGUI()

editorCtl() ; check active window and populate oCalLTip properties
If (oCallTip.progHwnd)
	ReParseText() ; initial loading of functions, custom functions, objects, if matching window found

return ; end of auto execute section

; ================================================================
; INCLUDES
; ================================================================

#INCLUDE LibV2\_Jxon_v2.ahk
#INCLUDE LibV2\_Font_Picker_Dialog_v2.ahk
#INCLUDE LibV2\_Color_Picker_Dialog_v2.ahk

#INCLUDE LibV2\_LoadElements.ahk
#INCLUDE LibV2\_ProcInfo.ahk
#INCLUDE LibV2\_gui.ahk
#INCLUDE LibV2\_Init_and_Support_Funcs.ahk
#INCLUDE *i LibV2\TheArkive_Debug.ahk
; #INCLUDE LibV2\_scintilla_class.ahk

; ================================================================
; hotkeys - global ; 
; ================================================================

^Space::ClickCheck(A_ThisHotkey) ; ReParseText() ; ClickCheck(A_ThisHotkey)

^+Space::DisplayCallTip() ; ClickCheck("LButton")

^!Space::LoadAutoComplete()

^+F12:: ; close CallTipsForAll
	MsgBox "Closing Call Tips For All!"
	ExitApp
Return

~ESC::
	closeCallTip()
	closeAutoComplete()
return

Up:: ; scroll when multiple records are available for call tips
	i := oCallTip.fullDescArr ? oCallTip.fullDescArr.Count : 0
	If (oCallTip.curIndex And (oCallTip.curIndex-1 != 0)) {
		oCallTip.curIndex--
		callTipGui.Destroy()
		callTipGui := ""
		LoadCallTip()
	} Else If (IsObject(AutoCompleteGUI)) {
		If (!AutoCompleteGUI["KwList"].Focused)
			AutoCompleteGUI["KwList"].Focus()
		SendInput "{Up}"
	} Else SendInput "{Up}"
return

Down:: ; scroll when multiple records are available for call tips
	i := oCallTip.fullDescArr ? oCallTip.fullDescArr.Count : 0
	If (oCallTip.curIndex And (oCallTip.curIndex+1) <= i) {
		oCallTip.curIndex++
		callTipGui.Destroy()
		callTipGui := ""
		LoadCallTip()
	} Else If (IsObject(AutoCompleteGUI)) {
		; msgbox AutoCompleteGUI["KwList"].Focused control
		If (!AutoCompleteGUI["KwList"].Focused)
			AutoCompleteGUI["KwList"].Focus()
		SendInput "{Down}"
	} Else SendInput "{Down}"
return

~LButton::ClickCheck(A_ThisHotkey)
~MButton::
	closeCallTip()
	closeAutoComplete()
return

^!w:: ; hotkey to wrap text in clipboard
	newText := WrapText(WrapTextChars)
	If (newText)
		clipboard := newText
return

^!u:: ; hotkey to unwrap text in clipboard
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
	
	testList := ""
	For className, obj in ClassesList
		testList .= className " / " obj["type"] "`r`n"
	MsgBox "Classes loaded:`r`n`r`n" testList
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

; F9::
	; ControlEditPaste "test1234", oCallTip.ctlHwnd
	
	; oCallTip.test := ["a","b","c"] ; control
	; oCallTip.test2 := {d:"d1",e:"e2",f:"f3"}
	; crazyTest := Jxon_Dump2(oCallTip,4)
	; clipboard := crazyTest
	; msgbox crazyTest
	
	; newTest := Jxon_Load2(crazyTest)
	; testAgain := Jxon_Dump2(newText,4)
	; clipboard := testAgain
	; msgbox testAgain
; return

; ======================================================================================
; input hook - for processing during user input - i prefer hotkey to invoke reload.
; ======================================================================================
keyPress(iHook, VK, SC) { ; InputHookObject
	If (IsObject(CallTipGUI) Or GetKeyState("Ctrl"))
		return
	
	acON := false ; auto-complete GUI active
	curPhrase := oCallTip.curPhrase, ctl := ""
	
	If (IsObject(AutoCompleteGUI)) {
		Try ctl := AutoCompleteGUI["KwList"] ; AutoCompleteGUI may be in the process of closing
		If (ctl)
			acON := true 
	}
	
	If (!acOn And !Settings["AutoComplete"])
		return
	
	If (vk = 9) {
		If (acON) {
			SendInput "{Backspace}"
			ctl.Focus(), ctl.Value := 1
		}
	} Else If (vk = 13) { ; enter key
		If (acON and !ctl.Focused)
			closeAutoComplete()
		Else If (acON and ctl.Focused) {
			kwSel := ctl.Text
			kwSel := RegExReplace(kwSel,"\.|\(f\)|\(m\)","")
			
			If (InStr(kwSel,curPhrase) = 1 Or !curPhrase) {
				remainder := StrReplace(kwSel,curPhrase,"",,1)
				Loop Parse remainder
					ControlEditPaste A_LoopField, oCallTip.ctlHwnd ; not ideal but works
			}
			closeAutoComplete()
		}
	} Else If ((VK = 37 or VK = 39) And oCallTip.ctlActive) { ; left and right arrows
		If (acON) ; make sure auto-complete GUI is visible
			closeAutoComplete()
	} Else
		SetTimer "LoadAutoComplete", -100
	
	iHook.Stop()
	
	IH := InputHook("V I1","","")
	IH.OnKeyDown := Func("keyPress")
	IH.KeyOpt(oCallTip.captureKeys,"N") ; don't capture all keys, that causes problems
	IH.Start()
}

; ======================================================================================
; Functions related to hotkey events
; ======================================================================================
LoadAutoComplete() {
	KeywordFilter := ""
	editorCtl("")
	If (!oCallTip.ctlActive Or !KeywordList.Count) {
		closeAutoComplete()
		return
	}
	
	ProcInput() ; update cursor postion and curPhrase in global variables
	
	curPhrase := oCallTip.curPhrase
	curPhraseObj := oCallTip.curPhraseObj
	parentObj := oCallTip.parentObj
	parentObjType := oCallTip.parentObjType
	curPhraseType := oCallTip.curPhraseType
	
	If (StrLen(curPhrase) >= AutoCompleteLength) {
		KeywordFilter := Map()
		If (StrLen(curPhrase) >= AutoCompleteLength) {
			KeywordFilter := KwSearchDeep(curPhrase, parentObj)
			
			If (KeywordFilter.Count)
				LoadAutoCompleteGUI(KeywordFilter)
			Else
				closeAutoComplete()
		} Else
			closeAutoComplete()
	} Else
		closeAutoComplete()
}

KwSearchDeep(curPhrase, parentObj) {
	resultList := Map()
	If (curPhrase And !parentObj) {
		For objName in ObjectList {
			If (inStr(objName,curPhrase))
				resultList[objName] := "User Object"
		}
		
		For className, obj in ClassesList {
			If (inStr(className,curPhrase))
				resultList[classname] := "User " obj["type"]
		}
		
		If (!resultList.Count) {
			For kw, kwType in KeywordList { ; normal keyword look
				If (InStr(kw,curPhrase))
					resultList[kw] := kwType
			}
		}
	} Else If (parentObj) {
		LoopList := ["ObjectList","ClassesList"]
		foundObj := false, curObj := "", thisList := 0
		
		Loop 2 {
			If (A_Index = 1) {
				thisList := "ObjectList"
				curList := ObjectList
			} Else If (A_Index = 2) {
				thisList := "ClassesList"
				curList := ClassesList
			}
			
			For objName in curList {
				If (parentObj = objName) {
					curObj := objName
					foundObj := true
					Break
				}
			}
			If (foundObj)
				Break
		}
		
		If (curObj) {
			curObject := curList[curObj]
			If (thisList = "ObjectList") {
				For curType in curObject {
					curMethPropList := MethPropList[curType]
					curMethList := curMethPropList["method"]
					For methName in curMethList
						If (inStr(methName,curPhrase))
							resultList[methName] := curType " Method"
					
					curPropList := curMethPropList["property"]
					For propName in curPropList
						If (inStr(methName,curPhrase))
							resultList[propName] := curType " Property"
				}
			} Else If (thisList = "ClassesList") {
				classObj := ClassesList[curObj]
				memList := classObj["members"]
				For memName, obj in memList
					resultList[memName] := obj["type"]
			}
		}
	}
	
	return resultList
}

debugToolTip() {
	ProcInput()
	
	curPhrase := oCallTip.curPhrase
	curPhraseObj := oCallTip.curPhraseObj
	curPhraseType := oCallTip.curPhraseType
	parentObj := oCallTip.parentObj
	parentObjType := oCallTip.parentObjType
	
	If (curPhrase) { ; for debugging while allowing user input - requires InputHook up top
		CaretGetPos(x,y)
		Tooltip "curPhrase :" curPhrase ": / curPhraseObj: " curPhraseObj
		      . "`r`nfuncName: " funcName " / funcText: " funcText
		      . "`r`nparentObj: " parentObj " / parentObjType: " parentObjType 
			  . "`r`nphraseType: " curPhraseType, x, (y+30)
	} Else
		ToolTip
}

ClickCheck(curKey) {
	editorCtl("click")
	If (!oCallTip.ctlActive) {
		closeCallTip()
		return
	}
	
	c := MultiClickDetect(curKey)
	
	If (curKey = "~LButton" And c = 2 And Settings["LoadCallTipOnClick"]) {
		If (!oCallTip.ctlActive)
			return
		closeAutoComplete()
		DisplayCallTip()
	} Else If (curKey = "^Space") {
		oCallTip.c := c
		SetTimer "CallTipsReload", -300
	}
}

CallTipsReload() {
	If (oCallTip.c = 1) {
		ReParseText()
	} Else If (oCallTip.c = 2) {
		QuickReloadGUI()
	}
}

DisplayCallTip() {
	If (!oCallTip.ctlActive)
		return
	
	closeCallTip()
	ProcInput() ; process data near caret to determin call tip contents
	
	If (Settings["DebugToolTip"])	; use debug tool tip if enabled
		debugToolTip()
	Else
		LoadCallTip()
}

closeCallTip() {
	If (Settings["DebugToolTip"])						; close call tip (tooltip version)
		ToolTip
	If (IsObject(callTipGui)) {
		callTipGui.Destroy()
		callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
		WinActivate "ahk_id " oCallTip.progHwnd
	}
}

closeAutoComplete() {
	If (IsObject(AutoCompleteGUI)) {
		AutoCompleteGUI.Destroy()
		AutoCompleteGUI := ""
		If (oCallTip.ctlActive)
			WinActivate "ahk_id " oCallTip.progHwnd
	}
}
