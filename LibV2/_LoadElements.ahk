; Contains functions for parsing the active window, language files, and/or includes
; in order to load data for call tips.

; ================================================================
; Reads current document and caret position and refreshes ObjectList, FunctionList,
; and CustomFunctions.
; ================================================================

ReParseText() {
	If (!oCallTip.progHwnd Or !oCallTip.ctlHwnd)
		return
	
	;show a gui that elements are loaded
	g := Gui.New("-Caption AlwaysOnTop +Owner" oCallTip.progHwnd)
	g.SetFont("s" Settings["fontSize"], Settings["fontFace"])
	g.BackColor := Settings["bgColor"]
	ctl := g.AddText("+c" Settings["fontColor"] " x5 y5", "Loading Objects / Custom Functions...")
	curMon := GetMonitorData()
	g.Show("y0 x" curMon.left " NA NoActivate")
	
	ReloadElements()
	g.Destroy()
}

ReloadElements() {
	DocumentMap := Array() ; this should be reset every reload, even if no baseFile
	srcFiles := oCallTip.srcFiles
	
	Loop Files srcFiles "\*.chm"
		If (A_Index = 1)
			oCallTip.helpFile := A_LoopFileFullPath
	
	; curDocText := ControlGetText(oCallTip.ctlHwnd) ;get text from current doc in editor
	
	; ============ old parser ==================
	; oCallTip.docTextNoStr := StringOutline(curDocText) ; this should only be done once per load/reload (for full text + includes)
	; oCallTip.docText := curDocText
	
	; CustomFunctions := GetCustomFunctions(curDocText)
	; ClassesList := GetClasses(curDocText)
	; ScanClasses(curDocText)
	; ObjectList := CreateObjList(curDocText)
	
	; ====== new parser ======
	baseFile := Settings["BaseFile"]
	If (baseFile)
		ahk_parser_launcher(baseFile)
	
}