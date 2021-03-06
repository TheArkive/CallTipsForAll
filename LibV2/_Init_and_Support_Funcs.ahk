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

; ==================================================
; Determines parent window based on user settings.
; ==================================================
; 
CheckMouseLocation() {
	MouseGetPos x,y,hWnd, ctlHwndCheck, 2
    ctlHwndCheck := (ctlHwndCheck = "") ? 0 : ctlHwndCheck
    oCallTip.clickCtlHwnd := ctlHwndCheck
	
	ctlClassNnCheck := ""
	Try ctlClassNnCheck := ControlGetClassNN(ctlHwndCheck) ; try to get ClassNN
	
	If (oCallTip.progHwnd = hWnd And ctlClassNnCheck != "" And InStr(ctlClassNnCheck,Settings["ProgClassNN"])) {
		oCallTip.ctlHwnd := ctlHwndCheck ; update ctl hwnd if match
		ScintillaExt.ctlHwnd := ctlHwndCheck
	}
	
  	If (IsObject(SettingsGUI) And SettingsGUI.hwnd = hwnd)
		oCallTip.ctlActive := false
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

ReadSettingsFromFile() {
	SettingsFile := "Settings.txt"
	If (FileExist("Settings.txt.blank") And !FileExist(SettingsFile)) ; load default settings on first run
		FileMove "Settings.txt.blank", SettingsFile
	
	return Jxon_Load(FileRead(SettingsFile)) ; Map
}

AddTrayMenu() {
	; ======================================================================================
	; Tray Menu
	; ======================================================================================
	; trayMenu := A_TrayMenu
    
    ; Debug.Msg(Type(A_TrayMenu) " - " Type(trayMenu))
    
	A_TrayMenu.Delete()
	A_TrayMenu.Add("Settings","iconMenu")
	A_TrayMenu.Add()
	A_TrayMenu.Add("ReWrap Text (CTL + ALT + W)","iconMenu")
	A_TrayMenu.Add("UnWrap Text (CTL + ALT + U)","iconMenu")
	
	A_TrayMenu.Add("Reload Items (Full)","iconMenu")
	A_TrayMenu.Add("Reload Script","iconMenu")
	A_TrayMenu.Add("Exit","iconMenu")

	iconMenu(ItemName, ItemPos, MenuObj) { ; MenuObject
		If (ItemName = "Settings") {
			SettingsGUILoad()
		; } Else If (ItemName = "Generate Commands") {
			; AutoCmdFunc()
		} Else If (ItemName = "ReWrap Text (CTL + ALT + W)") {
			clipboard := WrapText(WrapTextChars)
		} Else If (ItemName = "UnWrap Text (CTL + ALT + U)") {
			clipboard := unwrapText()
		} Else If (ItemName = "Reload Script")
			Reload
		Else If (ItemName = "Reload Items (Full)")
			FullReload()
		Else If (ItemName = "Exit")
			ExitApp
	}
}

; ======================================================================================
; just a quick little tool to wrap text -- text must be copied to clipboard first
; ======================================================================================
WrapText(x) {
	If (!StrLen(A_Clipboard))
		return ""
	
	inText := A_Clipboard
	endCRLF := false
	If (result := RegExMatch(inText,"(.*[\r\n]+$)",match))
		endCRLF := true
	
	inText := StrReplace(A_Clipboard,"`r`n",Chr(1))
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
	If (!StrLen(A_Clipboard))
		return ""
	
	inText := A_Clipboard
	endCRLF := false
	If (result := RegExMatch(inText,"(.*[\r\n]+$)",match))
		endCRLF := true
	
	inText := StrReplace(A_Clipboard,"`r`n`r`n",Chr(1))
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
;        returns a map with data of that monitor or nothing on error
; ===========================================================================
GetMonitorData(x:="", y:="") {
	CoordMode "Mouse", "Screen" ; CoordMode Mouse, Screen ; AHK v1
	If (x = "" Or y = "")
		MouseGetPos x, y
	
	monCount := MonitorGetCount() ; SysGet, monCount, MonitorCount ; AHK v1
	Loop monCount {
		MonitorGet(A_Index,mLeft,mTop,mRight,mBottom) ; SysGet, m, Monitor, A_Index ; AHK v1
		
		If (mLeft = "" And mTop = "" And mRight = "" And mBottom = "")
			Continue
		
		If (x >= (mLeft) And x <= (mRight-1) And y >= mTop And y <= (mBottom-1)) {
			return MonData := {left: mLeft, right: mRight
			                 , top: mTop, bottom: mBottom
			                 , w: mRight - mLeft, h: mBottom - mTop
			                 , x: x, y: y, active: A_Index
			                 , Cx: (mRight - mLeft) / 2 + mLeft
			                 , Cy: (mBottom - mTop) / 2 + mTop }
		}
	}
}

; ======================================================================================
; Detects a multi-click event, given a specified delay and specified key, between clicks
; ======================================================================================
class MultiClick {
	Static Count := "", Key := "", TicksPrev := ""
	
	Static Detect(ThisKey, delay:=300, CycleLimit:=0) {	; ThisKey = A_ThisHotKey, or whatever value you pass
		ct := this.TickDiff()
		
		this.Count := (this.Count = "") ? 0 : this.Count
		If ((ct > delay And ct != "" And ct != 0) Or (ThisKey != this.Key And this.Key != ""))
			this.Count := 0
		Else If (this.Count >= CycleLimit And CycleLimit > 0) ; resets MultiClickCount to 1 on CycleLimit+1
			this.Count := 0 ; useful for firing multiple double/triple/etc clicks without a pause between.
		
		this.Key := ThisKey, this.Count++
		return this.Count
	}
	Static TickDiff() { ; returns the number of ticks (ms) since the last button event (any button)
		CurTicks := A_TickCount
		
		If (this.TicksPrev = "")
			diff := 0, this.TicksPrev := A_TickCount
		Else
			diff := A_TickCount - this.TicksPrev, this.TicksPrev := A_TickCount
		
		return diff
	}
}