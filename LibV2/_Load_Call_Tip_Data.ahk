; ==================================================
; Functions here load basic call tip data
; For objects, only data the will match objects is loaded.  Actually identifying functions within source code
; is done by the parser.
; ==================================================


; ==================================================
; keyword list - Load pre-defined keywords.  Basically anything that is NOT a function, command, class, object,
;                or anything else that is not already defined in lang files.
; ==================================================
;    Structure
;
;		obj
;			[elementName] = type_string (usually set to "keyword" by default

LoadKeywordsList() {
	KeywordList := Map(), KeywordList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	Loop Files oCallTip.srcFiles "\Keywords\KW_*.txt"
	{
		curText := FileRead(A_LoopFileFullPath)
		Loop Parse curText, "`n", "`r"
		{
			e := InStr(A_LoopField,"=")
			If (!e)
				KeywordList[A_LoopField] := "keyword"
			Else
				KeywordList[SubStr(A_LoopField,1,e-1)] := SubStr(A_LoopField,e+1)
		}
	}
	KeywordList.Has("") ? KeywordList.Delete("") : ""
}

; ==================================================
; Loads any call tip data that is 1-to-1, in other words, not nested like objects.
; Mostly this is functions, commands, directives, flow control words, etc.
; Yes, this function name is a slight misnomer.  It loads more than just functions.
; ==================================================
; FunctionList Structure
; ========================
;	FunctionList
;
;		obj
;			[elementName] - name of func, cmd, etc.  Defined in lang files.
;				["desc"] - help data for call tips, defined in lang files
;				["type"] - type of element, function, command, flow, directive, etc... Defined in lang files
;				["helpLink"] - help link for element, defined in lang files
; ==================================================
LoadFunctionsList() {
	FunctionList := Map(), FunctionList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	Loop Files oCallTip.srcFiles "\Other\List_*.txt" ; functions and commands are combined into one list => FunctionList[]
	{
		a := StrSplit(A_LoopFileName,"_") ; comma (,) separator
		fileName := A_LoopFileFullPath, curFileType := StrReplace(a[2],".txt")
		
		curList := FileRead(fileName)
		curList := Trim(curList,"`r`n") entryEnd

		If (curFileType = "Regex")
			Continue

		curPos := 1, subStrEnd := 1
		len := StrLen(curList)

		While (curPos <= len And subStrEnd) {
			subStrEnd := InStr(curList,entryEnd,false,curPos)   ;find position of next entryEnd which is 2 consecutive empty lines
			curLen := subStrEnd - curPos
			curSubStr := SubStr(curList,curPos,curLen)
			curPos := subStrEnd + StrLen(entryEnd)              ;prepare for next iteration
      
			funcArr := StrSplit(curSubStr,Chr(96))

			curObj := Map()
			DescArr := Array()
			For i, funcDef in funcArr
			{
				textblock := Trim(funcDef,"`r`n")
				If (i = 1) {
					tmp := StrSplit(textblock,"`n", "`r")
					curObj["type"] := tmp[1]  ; "function" or "command" ... mostly
					funcName := tmp[2]
				} Else {
					LastBlock := textblock
					(A_Index < funcArr.Length) ? DescArr.Push(LastBlock) : ""
				}
			}		
			curObj["desc"]     := DescArr
			curObj["helpLink"] := LastBlock

			FunctionList[funcName] := curObj
			KeywordList[funcName] := curObj["type"] ; fill proper type instead of all "function"
			curObj := ""
		}
	}
}

; ==================================================
; Create Object list, indexed by object type.  All nested members are included.
; ==================================================
;    Structure:
;
;		obj
;			["method"] - method list
;				["desc"] - help data as defined in lang files
;				["helpLink"] - help link for method
;			["property"] - property list
;				["desc"] - help data as defined in lang files
;				["helpLink"] - help link for property
;			["helpLink"] - link to help info
;			["desc"] - summarized list of methods, properties
; ==================================================

LoadMethPropList() {
	MethPropList := Map(), MethPropList.CaseSense := 0 ;ZZZ - keeping CaseSense on so we can correct case on auto-complete
	srcFiles := oCallTip.srcFiles, objMatchText := ""
	Loop Files srcFiles "\Objects\*.txt"
	{
		a := StrSplit(A_LoopFileName,"_")
		fnType := a.Has(2) ? a[2] : A_LoopFileName, fnType := StrReplace(fnType,".txt","")
		
		curList := FileRead(A_LoopFileFullPath)
		curList := Trim(curList,"`r`n") entryEnd
		objList := "", objListPre := "", lineSplit := "", curObjType := ""
		curPos := 1, curHelpLink := "", propText := "", methText := ""
		len := StrLen(curList)
		
		curObj := Map(), propList := Map(), methList := Map()

		While (curPos <= len) {
			subStrEnd := InStr(curList,entryEnd,false,curPos)
			curLen := subStrEnd - curPos
			curSubStr := SubStr(curList,curPos,curLen)
			
			curDesc := "", curMemType := "", curMem := ""
			If (A_Index = 1) {
				objMatchArr := StrSplit(curSubStr,Chr(96))
				i := objMatchArr.Length
				
				objDescArr := Array(), methPropArr := Array(), curHelpLink := ""
				Loop i {
					t := Trim(objMatchArr[A_Index],"`r`n")
					If (A_Index = 1)
						objListPre := t, objMatchText .= objListPre "`r`n"
					Else If (A_Index = 2)
						moreObj := t
					Else If (A_Index > 2 And A_Index < i)
						objDescArr.Push(t)
					Else If (A_Index = i)
						curHelpLink := t
				}
				
				Loop Parse objListPre, "`n", "`r" ; create list of defined objTypes
				{
					lineSplit := StrSplit(A_LoopField," "), curObjType := lineSplit[2]
					objList .= curObjType "`r`n"
				}
				objList := Sort(objList,"U")
				objList := Trim(objList,"`r`n")
			} Else {
				memMap := Map(), memDescArr := Array(), memHelpLink := ""
				methPropArr := StrSplit(curSubStr,Chr(96)), i := methPropArr.Length
				Loop i {
					t := Trim(methPropArr[A_Index],"`r`n")
					If (A_Index = 1) {
						Loop Parse t, "`n", "`r"
						{
							If (A_Index = 1)
								curMemType := A_LoopField
							Else If (A_Index = 2)
								curMem := A_LoopField
						}
					} Else if (A_Index > 1 And A_Index < i) {
						memDescArr.Push(t)
					} Else If (A_Index = i) {
						memHelpLink := t
					}
				}
				
				memMap["desc"] := memDescArr
				memMap["helpLink"] := memHelpLink
				
				curMemType := StrLower(curMemType)  
				; KeywordList[curMem] := curMemType
				If (curMemType = "Method")
					methList[curMem] := memMap
				Else
					propList[curMem] := memMap
			}
		
			curPos := subStrEnd + StrLen(entryEnd)
		}
		
		methTitle := "Methods: " ; create first desc as method/property list
		For methName in methList
			methText .= "." methName ", "
		
		methText := Trim(methText," ,`r`n")
		If (methText)
			methText := methTitle methText
		
		propTitle := "Properties: "
		For propName in propList
			propText .= "." propName ", "
		
		propText := Trim(propText," ,`r`n")
		If (propText)
			propText := propTitle propText
		
		firstDesc := Trim("[ObjectTypeName]`r`n`r`n" methText,"`r`n")
		firstDesc := Trim(firstDesc "`r`n`r`n" propText,"`r`n")
		
		curObj["method"] := methList, curObj["property"] := propList
		curObj["helpLink"] := curHelpLink, curObj["moreObj"] := moreObj
		
		Loop Parse objList, "`n", "`r" ; append methods/properties to all defined obj types
		{
			curFirstDesc := StrReplace(firstDesc,"[ObjectTypeName]",A_LoopField)
			objDescArr.InsertAt(1,curFirstDesc)
			curObj["desc"] := objDescArr
			MethPropList[A_LoopField] := curObj
		}
		curObj := "", methList := "", propList := ""
	}
	
	return objMatchText
}

; ==================================================
; Generates a hierarchical list of object match strings to be executed in a specific order.
; Execution is done by CreateObjList()
; Hierarchy:    obj
;                   [N] - number or level, 0 is parsed first
;                       [Label] - must be unique, this is a name for the record that defines level and regex search criteria, defined in lang files
;                           ["regex"] - the regex string
;                           ["type"] - object type string
;                           ["direct"] - boolean - indicates if {substitution_obj_name} in regex is required
; ==================================================
LoadObjectCreateList(objMatchText) {
	ObjectCreateList := Map()
	objMatchText := Sort(objMatchText)
	objMatchText := Trim(objMatchText,"`r`n")
	curLevel := 0, prevLevel := 0, curLevelObj := Map()

	Loop Parse objMatchText, "`r", "`n"
	{
		curObj := Map()
		result := RegExMatch(A_LoopField,"([0-9]) ([\w]+) ([\w]+) (.*)",match)
		
		If (IsObject(match) And match.Count() = 4) {
			curLevel := match.Value(1), curObjType := match.Value(2)
			curLabel := match.Value(3), regex := match.Value(4)
			isDirect := InStr(regex,"{") ? 0 : 1 ; reorganize with level sub object
			
			If (curLevel != prevLevel)
				ObjectCreateList[prevLevel] := curLevelObj, curLevelObj := Map()
			
			curObj := Map()
			curObj["regex"] := regex, curObj["type"] := curObjType, curObj["direct"] := isDirect
			curLevelObj[curLabel] := curObj
			
			prevLevel := curLevel
		} Else {
			msg := "Line:`r`n    " A_LoopField "`r`n`r`nObject File - improper format.`r`n`r`nLine Format:`r`n    Level ObjType Label RegExMatchString"
			MsgBox msg
			ExitApp
		}
		curObj := ""
	}
	ObjectCreateList[prevLevel] := curLevelObj, curLevelObj := ""
}
