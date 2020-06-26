; Contains functions for processing data pertaining to where the caret is in the text editor
; and what "phrase" the caret is near.

; Not all of these functions are used yet.  Particularly, the functions that determine if the
; caret is in a function, and also which parameter the caret is in.  These may be used later
; for more fancy call tips.

; ================================================================
; Replaces a "strings" in a single line (usually the current line) with *******.
; Helps with determining function boundaries and creating object lists.  This also
; assists the script in identifying elements properly, since RegEx is used.
; ================================================================
StringOutline(sInput,pruneComments := true) {
	curLineNoStr := RegExReplace(sInput,Chr(96) Chr(34),"**")			; blank out literal `" first
	While (result := RegExMatch(curLineNoStr,"(" Chr(34) ".*?" Chr(34) ")",match)) {	; which helps properly match strings
		repStr := ""
		If (IsObject(match)) {
			repStr := StrRepeat("*",match.Len(1))
			curLineNoStr := StrReplace(curLineNoStr,match.Value(1),repStr,,1)
			match := ""
		}
	}
	
	If (pruneComments) {
		While (r := RegExMatch(curLineNoStr,"(;.*)",match)) { ;ZZZ - this doesn't work ... need to replace comments with spaces to keep POS accurate
			If (IsObject(match)) {
				repStr := StrRepeat(" ",match.Len(0)) ;ZZZ - this will be slightly less annoying than below
				curLineNoStr := StrReplace(curLineNoStr,match.Value(0),repStr,,1)
			}
		}
	}
	
	; curLineNoStr := RegExReplace(curLineNoStr,"\\" Chr(34),"**") ;ZZZ - hopefully don't need these
	; curLineNoStr := RegExReplace(curLineNoStr,"\" Chr(34),"*")
	
	return curLineNoStr
}

StrRepeat(str,num) {
	result := ""
	Loop num
		result .= str
	return result
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
	; msgbox "curLine:`r`n`t" curLineNoStr
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
	
	curPhrase := oCallTip.curPhrase
	
	fullPhrase := ""
	aStr := StrSplit(phraseObj,".")
	
	Loop (aStr.Length-1) {
		curBit := aStr[A_Index]
		fullPhrase .= curBit "."
	}
	fullPhrase := Trim(fullPhrase,".")
	
	If (!phraseObj And !funcName)
		return ""
	
	If (fullPhrase)
		return fullPhrase
	
	; aStr := StrSplit(fullPhrase,".")
	; If (!IsObject(aStr) Or aStr.Length = 0)
		; return ""
	
	; pOn := aStr.Length - 1
	; sObj := aStr.Has(pOn) ? aStr[pOn] : ""
	
	; If (ObjectList.Has(sObj))
		; return sObj
	; Else If (ObjectList.Has(curPhrase))
		; return curPhrase
	; Else If (sObj)
		; return sObj
	; Else
		; return ""
}

ProcInput() {
	If (!IsObject(ObjectList) Or !IsObject(FunctionList) Or !IsObject(CustomFunctions))
		return
	
	; hCtl := ControlGetFocus("ahk_id " oCallTip.progHwnd) ; active ctl hwnd
	; curClassNN := ControlGetClassNN(hCtl) ; active ctl ClassNN
	
	hCtl := oCalLTip.ctlHwnd
	ctlClassNN := Settings["ProgClassNN"]
	
	If (ctlClassNN = "edit") { ; specific for edit control (notepad.exe)
		oCallTip.ctlHwnd := hCtl ; update control hwnd
		curLine := ControlGetCurrentLine(hCtl) ; global
		curCol := ControlGetCurrentCol(hCtl) ; global
		curLineText := ControlGetLine(curLine,hCtl)
		oCallTip.lineNum := curLine, oCallTip.lineText := curLineText
		curLineNoStr := StringOutline(curLineText) ; blank out strings with "****"
	} Else If (ctlClassNN = "scintilla") { ; specific for scintilla control
		oCallTip.ctlHwnd := hCtl ; update control hwnd
		curPos := ScintillaExt.SendMsg("SCI_GETCURRENTPOS",0,0,hCtl)
		curLine := ScintillaExt.SendMsg("SCI_LINEFROMPOSITION",curPos.dll,0,hCtl)
		scintBufferLen := ScintillaExt.SendMsg("SCI_LINELENGTH",curLine.dll,0,hCtl)
		curLineText := ScintillaExt.SendMsg("SCI_GETCURLINE",scintBufferLen.dll,"",hCtl,scintBufferLen.dll)
		
		If (scintBufferLen.dll = curLineText.dll) ; this should catch last char when typing at the END of the document
			curLineText := ScintillaExt.SendMsg("SCI_GETCURLINE",scintBufferLen.dll+1,"",hCtl,scintBufferLen.dll+1)
		
		; DebugMsg("scintBufferLen: " scintBufferLen.dll " / pos: " curLineText.dll " / curLineText: " curLineText.str)
		
		curCol := curLineText.dll + 1
		
		oCallTip.scintBufferLen := scintBufferLen.dll
		oCallTip.lineText := curLineText.str
		oCallTip.lineNum := curLine.dll, oCallTip.lineText := curLineText.str
		curLineNoStr := StringOutline(curLineText.str) ; blank out strings with "****"
	}
	
	oCallTip.colNum := curCol
	
	curPhrase := getCurPhrase(curLineNoStr,curCol,curPhraseStart) ; curPhraseStart: ByRef
	oCallTip.curPhrase := curPhrase
	
	curPhraseObj := getCurPhraseObj(curLineNoStr,curCol,curPhraseObjStart)
	parentObj := GetParentObj(curPhraseObj,curMethProp) ; curMethProp: ByRef
	
	oCallTip.curPhraseObj := curPhraseObj
	oCallTip.parentObj := parentObj
	oCallTip.curPhraseType := "", curPhraseType := ""
	oCallTip.parentObjType := "", parentObjType := ""
	
	parentObjTypeList := Map()
	For objName in ObjectList {
		If (parentObj = objName) {
			parentObjTypeList := ObjectList[objName]["types"], parentObj := objName ; correct case on parentObj
			Break
		}
	}
	
	; parentObjTypeList := ObjectList.Has(parentObj) ? ObjectList[parentObj]["types"] : Map()
	
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
	
	If (!curPhraseType And ClassesList.Count) {
		For instName, obj in ClassesList {
			If (instName = curPhrase) { ; catch classname and instances
				curPhraseType := obj["type"], oCallTip.curPhraseType := curPhraseType
				Break
			}
		}
	}
	
	If (!curPhraseType And ClassesList.Count) { ; class methods and properties
		For instName, obj in ClassesList {
			If (instName = parentObj) {
				If (obj["type"] = "Instance")
					obj := ClassesList[obj["class"]]
				
				memList := obj["members"]
				For memName, memObj in memList {
					If (memName = curPhrase Or memName = "__" curPhrase) {
						curPhraseType := "class-" memObj["type"], oCallTip.curPhraseType := curPhraseType
						break
					}
				}
			}
		}
	}
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