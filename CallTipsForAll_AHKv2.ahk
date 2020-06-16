
; AHK v2
; === comment out if using this script as a library and these are already determined. ===
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================

FileEncoding "UTF-8"

Global SettingsGUI, AutoCompleteGUI, callTipGui

Global ClassesList, ObjectCreateList, ObjectList ; internal list for keeping track of objects and obj types
Global MethPropList, UserObjMethProp, FunctionList, CustomFunctions, KeywordList ; internal lists for indicated types
Global Settings ; user settings object
Global entryEnd := "`r`n`r`n`r`n" ; used to determine expected separation between elements in language files

; eventually move these to settings GUI
Global WrapTextChars := 80
Global AutoCompleteLength := 0			; this actually needs to be zero for actual intuitive auto-complete... 
Global useTooltip := false				; ignores fontFace, fontSize, and callTipSelectable
Global maxLines := 20

; OnMessage 0x0100, "WM_KEYDOWN" ; WM_KEYDOWN control

SetupInputHook()

Settings := ReadSettingsFromFile() ; Map
AddTrayMenu() ; modify tray menu

If (!Settings["ProgClassNN"] Or !Settings["ProgExe"]) ; prompt user for important settings (if these are blank)
	SettingsGUILoad()

GetEditorHwnd() ; checks Settings["ProgExe"] windows and populate oCallTip properties
If (oCallTip.progHwnd) {  ; initial loading of functions, custom functions, objects, if matching window found
	FullReload() ; conditional for language elements
}

return ; end of auto execute section

class oCallTip {
	Static srcFiles := "", c := ""
	Static captureKeys := "abcdefghijklmnopqrstuvwxyz1234567890.,-=[]{Space}{Backspace}{Up}{Down}{Left}{Right}{Enter}{Tab}"
	Static docTextNoStr := "" ;ZZZ - do this to store blanked out strings
	Static docTextNoComments := "" ;ZZZ - do this to prune comments
	
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
#INCLUDE LibV2\TheArkive_Debug.ahk
#INCLUDE LibV2\_scintilla_class_ext.ahk

; ================================================================
; hotkeys - global ; 
; ================================================================

^Space::ClickCheck(A_ThisHotkey) ; ReParseText() ; ClickCheck(A_ThisHotkey)

^+Space::DisplayCallTip() ; ClickCheck("LButton")

^!Space::LoadAutoComplete(true) ; force Auto-Complete gui to show

^+F12:: ; close CallTipsForAll
{
	MsgBox "Closing Call Tips For All!"
	ExitApp
}

~ESC::
{
	closeCallTip()
	closeAutoComplete()
}

Up:: ; scroll when multiple records are available for call tips
{
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
}

Down:: ; scroll when multiple records are available for call tips
{
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
}

~LButton::ClickCheck(A_ThisHotkey)
~MButton::
{
	closeCallTip()
	closeAutoComplete()
}

^!w:: ; hotkey to wrap text in clipboard
{
	newText := WrapText(WrapTextChars)
	If (newText)
		clipboard := newText
}

^!u:: ; hotkey to unwrap text in clipboard
{
	newText := unwrapText()
	If (newText)
		clipboard := newText
}

F11:: ; list custom functions, commands, and objects - for debugging List_*.txt files only
{
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
}

F10:: ; list functions - for debugging List_*.txt files only
{
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
}

F9::
{
	oCallTip.srcFiles := A_ScriptDir "\Languages\" Settings["ActiveLanguage"] ;ZZZ - this needs to be here for proper functionality
	LoadKeywordsList()
	LoadFunctionsList()
	
	objMatchText := LoadMethPropList()
	LoadObjectCreateList(objMatchText)
	
	ReParseText()
}

; ======================================================================================
; input hook - for processing during user input - i prefer hotkey to invoke reload.
; ======================================================================================
FullReload() {
	If (!oCallTip.progHwnd) ;ZZZ - mostly only applies to first run
		GetEditorHwnd() ;ZZZ - this function now also puts text editor PID into oCallTip
	
	If (oCallTip.ctlHwnd) {
		; df := SendMessage(ScintillaExt.SCI_GETDIRECTFUNCTION, 0, 0, oCallTip.ctlHwnd)
		; dp := SendMessage(ScintillaExt.SCI_GETDIRECTPOINTER, 0, 0, oCallTip.ctlHwnd)
		
		; ScintillaExt.df := df
		; ScintillaExt.dp := dp
		
		ScintillaExt.pid := oCallTip.progPID
	}
	
	oCallTip.srcFiles := A_ScriptDir "\Languages\" Settings["ActiveLanguage"] ;ZZZ - this needs to be here for proper functionality
	
	If (!KeywordList)		;ZZZ - only load these if they don't already exist
		LoadKeywordsList()	;ZZZ - this is good for first run, or initial startup
	If (!FunctionsList)
		LoadFunctionsList()
	If (!MethPropList) {
		objMatchText := LoadMethPropList()
		LoadObjectCreateList(objMatchText)
	}
	
	ReParseText()
}

SetupInputHook() {
	IH := InputHook("V I1","","") ; options , endKeys , matchList
	IH.OnKeyDown := Func("keyPress")
	IH.KeyOpt(oCallTip.captureKeys,"N") ; don't capture all keys, that causes problems
	IH.Start()
}

keyPress(iHook,VK,SC) { ; InputHookObject
	If (IsObject(CallTipGUI) Or GetKeyState("Ctrl") Or IsObject(SettingsGUI))
		return
	
	ctl := "", acON := false ; auto-complete GUI active
	
	If (IsObject(AutoCompleteGUI)) {
		Try ctl := AutoCompleteGUI["KwList"] ; AutoCompleteGUI may be in the process of closing
		acON := (ctl) ? true : acON
	}
	
	If (!Settings["AutoComplete"])
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
			
			curPhrase := oCallTip.curPhrase
			parentObj := oCallTip.parentObj
			If (InStr(kwSel,curPhrase) = 1 Or !curPhrase) {
				hwnd := oCallTip.ctlHwnd
				
				If (kwSel) {
					curPos := ScintillaExt.SendMsg("SCI_GETCURRENTPOS",0,0,hwnd)
					newPos := curPos - StrLen(curPhrase)
					r1 := ScintillaExt.SendMsg("SCI_SETANCHOR",newPos,0,hwnd)
					r2 := ScintillaExt.SendMsg("SCI_REPLACESEL",0,kwSel,hwnd)
				}
			}
			closeAutoComplete()
			
			t := GetPhraseType(kwSel,parentObj)
			If (t = "command" Or t = "Function" Or t = "Object" Or t = "method" Or t = "property")
				DisplayCallTip()
			
			return ; try to suppress carriage return
		}
	} Else If ((VK = 37 or VK = 39) And oCallTip.ctlActive) { ; left and right arrows
		If (acON) ; make sure auto-complete GUI is visible
			closeAutoComplete()
	} Else
		SetTimer "LoadAutoComplete", -10
	
	iHook.Stop()
	
	SetupInputHook()
}

GetPhraseType(inPhrase,parentObj := "") { ; oCallTip.curPhraseType is not populated when using auto-complete, so we get curPhrase type this way
	If (KeywordList.Has(inPhrase))
		return KeywordList[inPhrase]
	Else If (ObjectList.Has(inPhrase))
		return "object"
	Else If (ClassesList.Has(inPhrase))
		return ClassesList[inPhrase]["type"]
	Else If (MethPropList.Has(parentObj)) {
		methList := MethPropList[parentObj]["method"]
		propList := MethPropList[parentObj]["property"]
		
		If methList.Has(inPhrase)
			return "method"
		Else If propList.Has(inPhrase)
			return "property"
	}
}

; ======================================================================================
; Functions related to hotkey events
; ======================================================================================
LoadAutoComplete(force:=false) { ; use force:=true for manual invocation (mostly)
	KeywordFilter := ""
	If (!oCallTip.progHwnd) ; this should be conditional
		GetEditorHwnd()
	
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
			
			If (KeywordFilter.Count = 1)
				For kw in KeywordFilter
					testWord := RegExreplace(kw,"\(m\)|\(f\)|\.","")
			
			If (force Or (parentObj And !curPhrase)) { ; for manual invocation, or after typing "object."
				LoadAutoCompleteGUI(KeywordFilter)
			} Else {
				If (KeywordFilter.Count And curPhrase != testWord) ;ZZZ - if user manually finishes typing word, close AutoComplete
					LoadAutoCompleteGUI(KeywordFilter)
				Else
					closeAutoComplete()
			}
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
						If (inStr(propName,curPhrase))
							resultList[propName] := curType " Property"
				}
			} Else If (thisList = "ClassesList") {
				classObj := ClassesList[curObj]
				memList := classObj["members"]
				For memName, obj in memList
					If (InStr(memName,curPhrase))
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
	CheckMouseLocation()
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