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