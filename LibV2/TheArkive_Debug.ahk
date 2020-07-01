Global ArkDebugObj := "", ArkDebugWinHwnd := "", ArkDebugOutputHwnd := ""

DebugMsg(str,sOrder:="top",TimeStamp:=true) {
    ArkDebugCheck() ; check if debug window open
    
    If (TimeStamp) ; append timestamp + str
        str := "[" A_Hour ":" A_Min ":" A_Sec "] " str "`r`n"
	
	AppendTxt(ArkDebugOutputHwnd,StrPtr(str))
}

ArkDebugCheck() {
	If (!IsSet(ArkDebugWinHwnd))
		ArkDebugOpen()
	Else If (!WinExist("ahk_id " ArkDebugWinHwnd))
        ArkDebugOpen()
}

ArkDebugOpen() {
	ArkDebugObj := Gui.New("+Resize +AlwaysOnTop","TheArkyTekt Debug Window"), ArkDebugWinHwnd := ArkDebugObj.Hwnd
	ArkDebugObj.OnEvent("size", "ArkDebugGuiSize"), ArkDebugObj.OnEvent("close","ArkDebugClose")
	
	ArkDebugObj.SetFont("s11","Courier New")
	ctl := ArkDebugObj.Add("Button","x5 y5 Section","Copy to Clipboard").OnEvent("Click","ArkDebugCopyClip")
	ctl := ArkDebugObj.Add("Button","yp x+5","Clear Window").OnEvent("Click","ArkDebugClear")
	
	ctl := ArkDebugObj.Add("Edit","vArkDebugOutput xs y+0 w700 h500 Multi ReadOnly")
	ArkDebugOutputHwnd := ctl.hwnd, ctl := ""
	ArkDebugObj.Show("NA NoActivate")
}

ArkDebugCopyClip(oCtl,*) { ; button
	ctl := GuiCtrlFromHwnd(ArkDebugOutputHwnd)
	clipboard := ctl.value
}

ArkDebugClear(oCtl,*) { ; button
	ctl := GuiCtrlFromHwnd(ArkDebugOutputHwnd), ctl.value := ""
}

ArkDebugGuiSize(obj, MinMax, Width, Height) {
	w := Width - 10, h := Height - 10 - 40, ctl := GuiCtrlFromHwnd(ArkDebugOutputHwnd), ctl.GetPos(x,y,w,h)
	ctl.Move(x,y,w,h)
}

ArkDebugClose(oGui,*) {
	ArkDebugWinHwnd := ""
}

AppendTxt(hEdit, ptrText, loc:="bottom") {
    charLen := SendMessage(0x000E, 0, 0,, "ahk_id " hEdit)						;WM_GETTEXTLENGTH
	If (loc = "bottom")
		SendMessage 0x00B1, charLen, charLen,, "ahk_id " hEdit	;EM_SETSEL
	Else If (loc = "top")
		SendMessage 0x00B1, 0, 0,, "ahk_id " hEdit
    SendMessage 0x00C2, False, ptrText,, "ahk_id " hEdit			;EM_REPLACESEL
}