; AHK v2
; === comment out if using this script as a library and these are already determined. ===
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================


FileEncoding "UTF-8"

Global SettingsGUI, AutoCompleteGUI, callTipGui

Global oCallTip := {curIndex: "", fullDescArr: "", helpFile: ""
                  , ctlClassNN: "", ctlHwnd: 0, progHwnd: 0, curPhrase: ""}

Global ObjectCreateList
Global ObjectList, MethPropList, FunctionList, CustomFunctions, KeywordList, ClassesList

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

Global WrapTextChars := 80

Global AutoCompleteLength := 3			; Auto-Complete won't trigger until X chars have been typed

Global useTooltip := false				; ignores fontFace, fontSize, and callTipSelectable

Global maxLines := 20

closeTipOnLButton := false		; close call tip on L click anywhere
closeTipOnClick := false		; close call tip when clicked on

; ======================================================================================
; Script Vars
; ======================================================================================

Global entryEnd := "`r`n`r`n`r`n", Settings

If (FileExist("Settings.txt.blank") And !FileExist("Settings.txt")) ; load default settings on first run
	FileMove "Settings.txt.blank", "Settings.txt"

Settings := Jxon_Load(FileRead("Settings.txt")) ; MapObject

editorCtl() ; check active window and populate oCalLTip properties
If (oCallTip.progHwnd) {
	; oCallTip.ctlHwnd := hCtl.ctlHwnd
	; oCallTip.ctlClassNN := hCtl.ctlClassNN
	; oCallTip.progHwnd := hCtl.progHwnd
	; oCallTip.progTitle := hCtl.progTitle
	ReParseText() ; initial loading of functions, custom functions, objects
}

If (!Settings["ProgClassNN"] Or !Settings["ProgExe"])
	SettingsGUI()

; ======================================================================================
; Tray Menu
; ======================================================================================

trayMenu := A_TrayMenu
trayMenu.Delete()
trayMenu.Add("Settings","iconMenu")
trayMenu.Add()
trayMenu.Add("ReWrap Text (CTL + ALT + W)","iconMenu")
trayMenu.Add("UnWrap Text (CTL + ALT + U)","iconMenu")
trayMenu.Add("Reload","iconMenu")
trayMenu.Add("Exit","iconMenu")

iconMenu(ItemName, ItemPos, MenuObj) { ; MenuObject
	If (ItemName = "Settings") {
		SettingsGUI()
	} Else If (ItemName = "ReWrap Text (CTL + ALT + W)") {
		clipboard := WrapText(WrapTextChars)
	} Else If (ItemName = "UnWrap Text (CTL + ALT + U)") {
		clipboard := unwrapText()
	} Else If (ItemName = "Reload")
		Reload
	Else If (ItemName = "Exit")
		ExitApp
}

Return ;end of Autoexec Section

#INCLUDE LibV2\_Jxon_v2.ahk
#INCLUDE LibV2\_Font_Picker_Dialog_v2.ahk
#INCLUDE LibV2\_Color_Picker_Dialog_v2.ahk

#INCLUDE LibV2\_LoadElements.ahk
#INCLUDE LibV2\_ProcInfo.ahk
#INCLUDE LibV2\_gui.ahk
#INCLUDE *i LibV2\TheArkive_Debug.ahk


; ======================================================================================
; just a quick little tool to wrap text -- text must be copied to clipboard first
; ======================================================================================
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

; ======================================================================================
; just a little tool to unwrap text copied to clipboard - removes CRLFs
; ======================================================================================

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

AutoCompleteGUI(KeywordFilter) {
	If (IsObject(AutoCompleteGUI))
		AutoCompleteGUI.Destroy()
	
	
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
		SetTimer "MakeItEasy", -300
	}
}

MakeItEasy() {
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

; ======================================================================================
; Detects a multi-click event, given a specified delay and specified key, between clicks
; ======================================================================================
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




; ==================================================
; hotkeys - #If not working so well???
; ==================================================
; #If WinActive("ahk_exe notepad++.exe") ; WinActive("ahk_exe notepad.exe") Or 


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

; AutoCmdFunc() {
	; curFolder := DirSelect("","","Select COMMANDS folder:")
	; If (curFolder = "")
		; return
	
	; Loop Files curFolder "\*.htm"
	; {
		; curFile := A_LoopFileName, doSkip := false, multiSyntax := false, multiSyntaxArr := Array()
		; If (curFile = "Block.htm" Or curFile = "GuiControls.htm")
			; doSkip := true
		; If (curFile = "ListView.htm" Or curFile = "TreeView.htm")
			; doSkip := true
		; If (curFile = "Math.htm")
			; multiSyntax := true
		
		; If (!doSkip) {
			; curTitle := "" curSyntax := "", fullSyntax := "", match := ""
			; curText := FileRead(A_LoopFileFullPath)
			; curType := "Command"
			
			; curTitleMatch1 := "<h1>(.*?)</h1>"
			; result := RegExMatch(curText,curTitleMatch1,match)
			; If (IsObject(match) And match.Count()) {
				; curTitle := match.Value(1)
				; If (curTitle = "{...} (block)")
					; curTitle := "Block"
			; }
			; curTitle := Trim(RegExReplace(curTitle,"<span class=`"ver`">.*?</span>",""))
			; curTitle := StrReplace(curTitle,"("), curTitle := StrReplace(curTitle,")")
			
			; curMatch1 := "s)<pre class=`"Syntax`"[^>]*>(.*?)\Q</pre>\E", curPos := 1
			; While (result := RegExMatch(curText,curMatch1,match,curPos)) {
				; If (IsObject(match) And match.Count()) {
					; curSyntax := match.Value(1)
					; curSyntax := RegExReplace(curSyntax,"s)(<span class=`"optional`"[^>]*>)(.*?)(\Q</span>\E)","[$2]")
					; curSyntax := RegExReplace(curSyntax,"s)<span class=`"func`"[^>]*>(.*?)\Q</span>\E","$1")
					; curSyntax := RegExReplace(curSyntax,"s)<i>(.*?)\Q</i>\E","$1")
					; curSyntax := RegExReplace(curSyntax,"s)<a href[^>]*>(.*?)\Q</a>\E","$1")
					; curSyntax := RegExReplace(curSyntax,"<span class=`"ver`">.*?\Q</span>\E","")
					; curSyntax := RegExReplace(curSyntax,"<em>.*?\Q</em>\E","")
					
					; If (!multiSyntax)
						; fullSyntax .= Trim(curSyntax,"`r`n") "`r`n"
					; Else
						; multiSyntaxArr.Push(curSyntax)
					
					; curPos := match.Pos(1) + match.Len(1)
				; }
			; }
			
			; If (curTitle and (fullSyntax Or multiSyntaxArr.Length)) {
				; curTitle := RegExReplace(curTitle,"<em>(.*?)\Q</em>\E","$1")
				; curTitleArr := StrSplit(curTitle,"/")
				; curHelpLink := "/docs/commands/" A_LoopFileName
				
				; If (!multiSyntaxArr.Length) {
					; If (InStr(fullSyntax,"(") And InStr(fullSyntax,")"))
						; curType := "Function"
					; Loop curTitleArr.Length {
						; curEntry := curType "`r`n" Trim(curTitleArr[A_Index]) "`r`n" Chr(96) Trim(fullSyntax,"`r`n") "`r`n" Chr(96) curHelpLink
						; fullList .= curEntry "`r`n`r`n`r`n"
					; }
				; } Else {
					; Loop multiSyntaxArr.Length {
						; curLine := multiSyntaxArr[A_Index]
						; If (InStr(curLine,"(") And InStr(curLine,")"))
							; curType := "Function"
						; result := RegExMatch(curLine,"[\w]+ := ([\w]+)\(",match)
						; If (IsObject(match) And match.Count()) {
							; curTitle := match.Value(1)
							; curEntry := curType "`r`n" Trim(curTitle) "`r`n" Chr(96) Trim(curLine,"`r`n") "`r`n" Chr(96) curHelpLink "#" curTitle
							; fullList .= curEntry "`r`n`r`n`r`n"
						; }
					; }
				; }
			; }
		; }
	; }
	
	; destFile := A_Desktop "\List_Commands.txt"
	; FileDelete destFile
	; FileAppend fullList, destFile
	; MsgBox "Commands list complete."
	; Run "notepad.exe " Chr(34) destFile Chr(34)
; }