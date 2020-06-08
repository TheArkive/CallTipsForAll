; AHK v2
; === comment out if using this script as a library and these are already determined. ===
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================

FileEncoding "UTF-8"

Global SettingsGUI, AutoCompleteGUI, callTipGui
Global oCallTip ; main global object containing most settings
oCallTip := {curIndex: "", fullDescArr: "", helpFile: "", ctlClassNN: "", ctlHwnd: 0, progHwnd: 0, curPhrase: ""}

Global ObjectCreateList, ObjectList ; internal list for keeping track of objects and obj types
Global MethPropList, FunctionList, CustomFunctions, KeywordList, ClassesList ; internal lists for indicated types
Global Settings ; user settings object
Global entryEnd := "`r`n`r`n`r`n" ; used to determine expected separation between elements in language files

; eventually move these to settings GUI
Global WrapTextChars := 80
Global AutoCompleteLength := 3			; Auto-Complete won't trigger until X chars have been typed
Global useTooltip := false				; ignores fontFace, fontSize, and callTipSelectable
Global maxLines := 20

Settings := ReadSettingsFromFile() ; load user settings
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

^!Space:: ; load auto-complete list
	; If (WinActive("ahk_exe notepad++.exe") Or WinActive("ahk_exe notepad.exe")) {
		; msgbox "yee haw!"
	; }
return

^+F12:: ; close CallTipsForAll
	MsgBox "Closing Call Tips For All!"
	ExitApp
Return

~ESC::closeCallTip()

Up:: ; scroll when multiple records are available for call tips
	i := oCallTip.fullDescArr ? oCallTip.fullDescArr.Count : 0
	If (oCallTip.curIndex And (oCallTip.curIndex-1 != 0)) {
		oCallTip.curIndex--
		callTipGui.Destroy()
		callTipGui := ""
		LoadCallTip()
	} Else SendInput "{Up}"
return

Down:: ; scroll when multiple records are available for call tips
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

; ======================================================================================
; input hook - for processing during user input - i prefer hotkey to invoke reload.
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

; ======================================================================================
; Functions related to hotkey events
; ======================================================================================
LoadAutoComplete() {
	ProcInput() ; update cursor postion and curPhrase in global variables
	
	curPhrase := oCallTip.curPhrase
	
	KeywordFilter := Map()
	If (StrLen(curPhrase) >= AutoCompleteLength) { ; not done yet!
		For kw, type in KeywordList {
			If (InStr(kw,curPhrase))
				KeywordFilter[kw] := type
		}
		; msgbox KeywordFilter.Count
		
	}
}

debugToolTip() {
	curPhrase := oCallTip.curPhrase
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
	editorCtl("click")
	If (!oCallTip.progHwnd Or !oCallTip.ctlHwnd) {
		closeCallTip()
		return
	}
	
	c := MultiClickDetect(curKey)
	
	If (curKey = "~LButton" And c = 2 And Settings["LoadCallTipOnClick"]) {
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
	If (oCallTip.ctlClassNN != "scintilla1" And oCallTip.ctlClassNN != "Edit1")
		return
	
	closeCallTip()
	ProcInput() ; process data near caret to determin call tip contents
	
	If (Settings["DebugToolTip"])	; use debug tool tip if enabled
		debugToolTip()
	Else
		LoadCallTip()
}



