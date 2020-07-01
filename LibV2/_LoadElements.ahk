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
	IncludesList := Array()
	srcFiles := oCallTip.srcFiles
	
	Loop Files srcFiles "\*.chm"
		If (A_Index = 1)
			oCallTip.helpFile := A_LoopFileFullPath
	
	curDocText := ControlGetText(oCallTip.ctlHwnd) ;get text from current doc in editor
	
	If (FileExist(Settings["BaseFile"])) { ;or use content of base file and all its includes instead
		GetIncludes([Settings["BaseFile"]]) ; populate IncludesList
		GetLibs()
		pos := 0, tmp := ""
		
		; msgbox "IncludesList: " IncludesList.Length
		
		For i, File in IncludesList { ; IncludesList is global
			curFileText := FileRead(File), curFileLen := StrLen(curFileText)
			curFileArr := StrSplit(curFileText,"`n","`r")
			
			For lineNum, lineText in curFileArr { ; create DocumentMap so regex pos can correspond to a file and line number
				lineLen := StrLen(lineText)
				start := (lineLen > 0) ? pos++ : pos
				end := (lineLen > 0) ? pos + (lineLen - 1) : pos
				pos := end ; sync pos
				DocumentMap.Push(Map("fileName",File, "lineNum",lineNum, "start",start, "end",end, "length",lineLen,"text",lineText))
				pos += 2 ; add length of CRLF
			}
			
			tmp .= curFileText "`r`n`r`n" ; load all includes into one var
			pos += 4 ; add length of CRLF x 2
		}
		
		curDocText := tmp ? tmp : curDocText
		; A_Clipboard := curDocText
		
		; For i, obj in DocumentMap ; quick test of DocumentMap
			; testStr .= obj["fileName"] "`r`nLine: " obj["lineNum"] " / " obj["start"] " / " obj["end"] "`r`n`r`n"
		
		; msgbox testStr
	}
	; msgbox "done parsing includes"
	
	oCallTip.docTextNoStr := StringOutline(curDocText) ; this should only be done once per load/reload (for full text + includes)
	oCallTip.docText := curDocText
	
	CustomFunctions := GetCustomFunctions(curDocText)
	; msgbox "in1"
	ClassesList := GetClasses(curDocText)
	; msgbox "in2"
	ScanClasses(curDocText)
	; msgbox "in3"
	ObjectList := CreateObjList(curDocText)
	; msgbox "done parsing"
}