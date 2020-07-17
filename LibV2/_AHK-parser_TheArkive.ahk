; ahk parser by TheArkive

ahk_parser_launcher(baseFile:="") {
    curTicks := A_TickCount
    
    If (baseFile)
        IncludesList := [], GetIncludes([baseFile]), GetLibs()
    
    ahk_parser_thearkive()
    
    ParseObjectsInstances() ; identify objects and instances
    
    finalTick := A_TickCount - curTicks
    mins := Integer(Integer(finalTick / 1000) / 60)
    secs := finalTick/1000 - (mins * 60)
    
    Settings["lastParse"] := mins " mins " secs " secs"
}

GetIncludes(inputIncludes) { ; input should start with [baseFile]
    newIncludes := []
    Loop inputIncludes.Length {
        curFile := inputIncludes[A_Index]
        
        If (IncludeCheckDupe(curFile))
            continue
        Else
            IncludesList.Push(curFile)
        
        curDocText := FileRead(curFile)
        curDocArr := StrSplit(curDocText,"`n","`r")
        
        SplitPath curFile, outFile, baseFolderP
        curBaseFolder := baseFolderP
        includesSearch := "^(#Include(Again)?[ \t]+.*)"
        
        Loop curDocArr.Length {
            curLine := curDocArr[A_Index]
            
            If (!r1 := RegExMatch(curLine,"mi)" includesSearch,match))
                continue
            
            curInc := Trim(RegExReplace(match.Value(1),"i)(^#Include(Again)?|\*i|" Chr(34) ")",""))
            curInclude := RegExReplace(curInc,"<|>","")
            
            If (curInc != curInclude) ; include is library file and will automatically be scanned
                continue
            
            curInclude := ReplaceVars(curInclude,curBaseFolder)
            
            finalFile := (FileExist(curBaseFolder "\" curInclude)) ? curBaseFolder "\" curInclude : FileExist(curInclude) ? curInclude : ""
            incType := FileExist(finalFile)
            
            If (InStr(incType,"D"))
                curBaseFilder := finalFile ; change baseFolder if dir
            Else If (incType)
                newIncludes.Push(finalFile) ; add to new includes
        }
    }
    
    If (newIncludes.Length)
        GetIncludes(newIncludes)    
}

IncludeCheckDupe(inFile) {
    dupe := false
    For i, curInc in IncludesList { ; check for duplicate includes
        If (inFile = curInc) {
            dupe := true
            Break
        }
    }
    
    return dupe
}

GetLibs() {
    If (IncludesList.Length) {
        baseFile := IncludesList[1]
        SplitPath baseFile,, baseFolder
        
        f1 := baseFolder "\Lib\*" ;??? - simplified handling of 3 lib locations   ;ZZZ - yes finally simplified!
        Loop Files f1
        {
            If (!IncludeCheckDupe(A_LoopFileFullPath))
                IncludesList.Push(A_LoopFileFullPath)
        }
        
        f2 := A_MyDocuments "\AutoHotkey\Lib\*"
        Loop Files f2
        {
            If (!IncludeCheckDupe(A_LoopFileFullPath))
                IncludesList.Push(A_LoopFileFullPath)
        }
        
        f3 := ""
        Try f3 := RegRead("HKEY_LOCAL_MACHINE\SOFTWARE\AutoHotkey","InstallDir") "\Lib\*"
        If (f3) {
            Loop Files f3
            {
                If (!IncludeCheckDupe(A_LoopFileFullPath))
                    IncludesList.Push(A_LoopFileFullPath)
            }
        }
    }
}

ReplaceVars(v, CurrentOutDir){ ; made by toralf ; orig params:  v, OutFileName, CurrentOutDir
  AHKVars := Map("%A_AhkPath%",      A_AhkPath
            , "%A_AhkVersion%",      A_AhkVersion
            , "%A_ComputerName%",    A_ComputerName
            , "%A_ComSpec%",         A_ComSpec
            , "%A_Desktop%",         A_Desktop
            , "%A_DesktopCommon%",   A_DesktopCommon
            , "%A_IsCompiled%",      A_IsCompiled
            ; , "%A_IsUnicode%",       A_IsUnicode
            , "%A_MyDocuments%",     A_MyDocuments
            , "%A_ProgramFiles%",    A_ProgramFiles
            , "%A_Programs%",        A_Programs
            , "%A_ProgramsCommon%",  A_ProgramsCommon
            , "%A_PtrSize%",         A_PtrSize
            ; , "%A_ScriptFullPath%":    CurrentOutDir "\" OutFileName ; less used for includes?
            ; , "%A_LineFile%":          CurrentOutDir "\" OutFileName
            , "%A_ScriptDir%",         CurrentOutDir
            ; , "%A_ScriptName%",        OutFileName
            , "%A_Space%",           A_Space
            , "%A_StartMenu%",       A_StartMenu
            , "%A_StartMenuCommon%", A_StartMenuCommon
            , "%A_Startup%",         A_Startup
            , "%A_StartupCommon%",   A_StartupCommon
            , "%A_Tab%",             A_Tab
            , "%A_Temp%",            A_Temp
            , "%A_UserName%",        A_UserName
            , "%A_WinDir%",          A_WinDir )

  For var,value in AHKVars
    v := StrReplace(v, var, value) 
  Return v  
}

ahk_parser_thearkive() { ; () = \x28 \x29 ... [] = \x5B \x5D ... {} = \x7B \x7D
    q := Chr(34), c := "", f := ""
    
    CustomFunctions := Map(), CustomFunctions.CaseSense := 0
    ClassesList := Map(), ClassesList.CaseSense := 0
    ObjectList := Map(), ObjectList.CaseSense := 0
    VariablesList := Array()
    
    oCallTip.newParseFunction := 1
    
    For i, includeFile in IncludesList {
        curDocText := FileRead(includeFile)
        docArr := StrSplit(curDocText,"`n","`r")
        
        i := 1, totLines := docArr.Length
        lastVarStatus := ""
        While (i <= totLines) {
            curType := ""
            iStart := 0 ; start of multi-line statement
            
            oCallTip.LPar := 0, oCallTip.RPar := 0 ; reset brace count
            oCallTip.LBrace := 0, oCallTip.RBrace := 0
            oCallTip.LCBrace := 0, oCallTip.RCBrace := 0
            
            multi := false
            curLine := docArr[i]
            curLineNoStr := StringOutline(curLine) ; docArrNoStr[i]
            
            If (Trim(curLine," `t") = "") { ; blank line so skip
                i++
                Continue
            }
            
            If (RegExMatch(curLine,"^[ \t]*\;")) { ; comment so skip
                i++
                Continue
            }
            
            If (RegExMatch(curLineNoStr,"^[ \t]*/\*")) {
                curChunk := curLine
                i++, curLine := docArr[i], curChunk .= "`r`n" curLine
                While (!RegExMatch(curLine,"^[ \t]*\*/"))
                    i++, curLine := docArr[i], curChunk .= "`r`n" curLine
                
                ; Debug.Msg(curChunk)
                i++
                Continue ; just skipping the multi-line comment block
            }
            
            If (docArr.Has(i+1))
                curChunk := curLine "`r`n" docArr[i+1], curChunkNoStr := curLineNoStr "`r`n" StringOutline(docArr[i+1])
            Else
                curChunk := curLine, curChunkNoStr := curLineNoStr
            
            If (RegExMatch(curLine,"i)^[ \t]*([#!\^\+\&\<\>\*\~\$]*[\w_ ]+)\:\:",match)) { ; hotkeys - not bothering to parse body of hotkey
                hotkeyName := match.Value(1)
                HotkeyList[hotkeyName] := Map("lineNum",i,"fileName",includeFile,"hotkeyName",hotkeyName)
                i++
                Continue
            } Else If (RegExMatch(curChunkNoStr,"mi)class ([\w_]+)( extends ([\w_\.]+))?[ \t\r\n]*\{")) { ; classes
                iStart := i
                encl := enclosureCheck(curChunkNoStr)
                oCallTip.newParseClass := 1 ; enable static collection of classes while parsing
                curType := "class"
                lastVarStatus := ""
                i++
            } Else If (RegExMatch(curChunkNoStr,"m)^[ \t]*([\w_]+)\x28")) { ; functions
                iStart := i
                encl := enclosureCheck(curChunkNoStr)
                curType := "function"
                lastVarStatus := ""
                i++
            } Else { ; NOT classes or functions ... usually not enclosed at all...
                iStart := i
                encl := enclosureCheck(curChunkNoStr)
                curLineNoStr := RTrim(curLineNoStr," `t")
                
                If (RegExMatch(curLine,"i)^[ \t]*Global"))
                    lastVarStatus := "Global"
                Else If (RegExMatch(curLine,"i)^[ \t]*Static"))
                    lastVarStatus := "Static"
                Else If (RegExMatch(curLineNoStr,"i)RegExMatch\x28[^\,]*\,[^\,]*\,[ \t]*([\w_]+)[ \t]*\x29",match)) {
                    ObjectList[match.Value(1)] := "RegExMatchObject"
                }
                
                i++
                
                varList := getVar(curLine,includeFile,i,lastVarStatus) ; collect vars outside functions / classes
                If (!varList.Length)
                    lastVarStatus := ""
                
                For i, obj in varList {
                    KeywordList[obj["varName"]] := "UserVar"
                    obj["elementName"] := "", VariablesList.Push(obj) ; vars NOT in a function / class / etc...
                }
                
                Continue
            }
            
            tempVarList := []
            While (encl.exist And !encl.even) { ; compiles a multi-line statment
                i++
                If (!docArr.Has(i))
                    Break
                nextLine := docArr[i]
                nextLineNoStr := StringOutline(docArr[i])
                
                multi := true
                encl := enclosureCheck(nextLineNoStr)
                curChunk .= "`r`n" nextLine ; concatenate chunk
                curChunkNoStr .= "`r`n" nextLineNoStr
            }
            
            If (curType = "class") {
                c := GetClasses(curChunk,includeFile,iStart) ; don't push 
            } Else If (curType = "function") {
                f := GetCustomFunction(curChunk,includeFile,iStart) ; not a big performance hit
                If (f And !CustomFunctions.Has(f["funcName"])) {
                    For i, obj in tempVarList {
                        KeywordList[obj["varName"]] := "UserVar"
                        obj["elementName"] := f["funcName"], VariablesList.Push(obj)
                    }
                    
                    CustomFunctions[f["funcName"]] := f ; add function to call tip data
                }
            }
            
            i++
        }
    }
    
    If (c) { ; save classes to global var
        For k, obj in c
            ClassesList[k] := obj
    }
}

enclosureCheck(sInput) { ; checks for even numbers of enclosures () [] {}, return total:0 means even
    noStr := StringOutline(sInput)
    
    p := 0, b := 0, c := 0
    noStr := StrReplace(StrReplace(noStr,"(","(",,LP),")",")",,RP)
    noStr := StrReplace(StrReplace(noStr,"[","[",,LB),"]","]",,RB)
    noStr := StrReplace(StrReplace(noStr,"{","{",,LC),"}","}",,RC)
    
    oCallTip.LPar += LP
    oCallTip.RPar += RP
    oCallTip.LBrace += LB
    oCallTip.RBrace += RB
    oCallTip.LCBrace += LC
    oCallTip.RCBrace += RC
    
    p := Abs(oCallTip.LPar - oCallTip.RPar)
    b := Abs(oCallTip.LBrace - oCallTip.RBrace)
    c := Abs(oCallTip.LCBrace - oCallTip.RCBrace)
    
    r2 := p + b + c
    
    even := (r2) ? false : true
    pRes := (oCallTip.LPar or oCallTip.RPar) ? true : false ; parenthesis exist?
    bRes := (oCallTip.LBrace or oCallTip.RBrace) ? true : false ; brackets exist?
    cRes := (oCallTip.LCBrace or oCallTip.RCBrace) ? true : false ; curly braces exist?
    
    exist := (!pRes And !bRes And !cRes) ? false : true
    
    p := {exist:pRes, total:p} ; result = exist? true/false   total = if 0 then even
    b := {exist:bRes, total:b}
    c := {exist:cRes, total:c}
    
    return {p:p, b:b, c:c, even:even, exist:exist} ; if enclosures exist, are they even, and how many of each
}

GetCustomFunction(curDocText, fileName, lineNum) { ;ZZZ - this should work better
    obj := "", noStr := StringOutline(curDocText)
    curRegex := "^[ \t]*([\w]+)\x28"
    
    If (r1 := RegExMatch(noStr,"mi)" curRegex, match)) {
        curPos1 := match.Pos(0)
        fn := match.Value(1)
        If (fn = "" Or fn = "If" Or fn = "While" Or fn = "For" Or fn = "Else")
            return "" ; not a function
        
        curPos2 := curPos1
        While (r2 := InStr(noStr, ")",, curPos2)) {
            curPos2 := r2
            funcStart := SubStr(noStr,curPos1,curPos2-curPos1+1)
            w := StrReplace(StrReplace(funcStart,"(","(",,LP),")",")",,RP)
            
            If (LP = RP) {
                funcStart := SubStr(curDocText,curPos1,curPos2-curPos1+1)
                funcStart := RegExReplace(RegExReplace(funcStart,"\r|\n",""),"[ \t]+","")
                r3 := RegExMatch(funcStart,"(\x28.*\x29)",match3)
                params := match3.Value(0)
                
                If (!RegExMatch(curDocText,"[ \t\r\n]*\{",match4,curPos2+1))
                    return "" ; not a function
                
                Break ; fn, funcStart, params, fileName, lineNum
            }
        }
    } Else
        return "" ; not a function
    
    curPos4 := 1, retObj := Array()
    While (r4 := RegExMatch(curDocText, "i)[ \t]*return[ \t]+(.*)", match4, curPos4)) {
        curPos4 := match4.Pos(0) + match4.Len(0)
        retObj.Push(match4.Value(1))
    }
    
    obj := Map("type","CustomFunction","desc",fn params,"funcName",fn,"params",params,"file",fileName,"lineNum",lineNum,"return",retObj)
    
    return obj
}

; CheckShowMode(strBody,helper:="") { ; checks class, methods, properties for "; show" or "; hide" comment
    ; showStyle := ""
    ; arr := StrSplit(strBody,"`n","`r"), firstLine := StringOutline(arr[1],false) ; blank out strings, but not comments
    ; r := RegExMatch(firstLine,"\;(.*)",match)
    
    ; If (IsObject(match)) {
        ; showStyle := Trim(match.Value(1)," `t;")
        ; showStyle := (InStr(showStyle,"hide") = 1) ? "hide" : (InStr(showStyle,"show") = 1) ? "show" : ""
    ; }
    
    ; return showStyle
; }

PruneFirstLast(strBody) {
    testArr := StrSplit(strBody,"`n","`r"), output := ""
    If (testArr.Length <= 2)
        return strBody ; if 2 lines or less, don't process
    
    Loop testArr.Length {
        If (A_Index > 1 And A_Index < testArr.Length)
            output .= testArr[A_Index] "`r`n"
    }
    return output
}

GetClasses(curDocText, fileName, lineNum, parent := "") {
    curDocTextNoStr := StringOutline(curDocText)
    static classList := Map()
    
    If (oCallTip.newParseFunction) {
        classList := Map()
        oCallTip.newParseFunction := 0
    }
    
    If (!classList.Count)
        classList.CaseSense := 0
    
    curPos1 := 1
    result := RegExMatch(curDocTextNoStr,"mi)^[ \t]*class[ \t]+([\w\_]+)([ \t]*extends[ \t]*([\w\_\.]+))?[ \t\r\n]*\{",match)
    
    innerClassBody := ""
    className := match.Value(1)
    
    curPos1 := match.Pos(0) + match.Len(0)
    extends := match.Value(3)
    
    curDocText := PruneFirstLast(curDocText) ; prune first and last lines
    txtArr := StrSplit(curDocText,"`n","`r")
    
    obj := Map("type","Class","desc",className,"extends",extends,"parent",parent,"fileName",fileName,"lineNum",lineNum,"lines",txtArr.Length)
    memberList := Map()
    
    lastPropStatus := false
    i := 1
    
    While (i <= txtArr.Length) {
        curLine := txtArr[i]
        curLineNoStr := StringOutline(curLine)
        nextLine := "", nextLineNoStr := ""
        
        If (txtArr.Has(i+1)) {
            nextLine := txtArr[i+1]
            nextLineNoStr := StringOutline(nextLine)
        }
        
        curType := "", iStart := 0
        
        oCallTip.LPar := 0, oCallTip.RPar := 0 ; reset brace count
        oCallTip.LBrace := 0, oCallTip.RBrace := 0
        oCallTip.LCBrace := 0, oCallTip.RCBrace := 0
        
        If (txtArr.Has(i+1)) {
            curChunk := curLine "`r`n" nextLine
            curChunkNoStr := curLineNoStr "`r`n" nextLineNoStr
        } Else
            curChunk := curLine, curChunkNoStr := curLineNoStr
        
        If (Trim(curLine," `t") = "") { ; blank line
            i++
            Continue
        }
        
        If (RegExMatch(curLine,"^[ \t]*\;")) { ; comment so skip
            i++
            Continue
        }
        
        If (RegExMatch(curLineNoStr,"^[ \t]*/\*")) {
            curChunk := curLine
            i++, curLine := txtArr[i], curChunk .= "`r`n" curLine
            While (!RegExMatch(curLine,"^[ \t]*\*/"))
                i++, curLine := txtArr[i], curChunk .= "`r`n" curLine
            
            ; Debug.Msg(curChunk)
            i++
            Continue ; just skipping the multi-line comment block
        }
        
        encl := {exist:false, even:true}
        
       
        If (RegExMatch(curChunkNoStr,"mi)^[ \t]*class[ \t]+([\w_]+)([ \t]*extends[ \t]*([\w_\.]+))?[ \t\r\n]*\{")) { ; sub class
            iStart := i
            i++
            encl := enclosureCheck(curChunkNoStr)
            curType := "subclass"
        } Else If (RegExMatch(curChunkNoStr,"i)^[ \t]*(Static[ \t]+)?[\w_]+[ \t]*\=\>")) { ; short property no params
            iStart := i
            i++
            encl := enclosureCheck(curChunkNoStr)
            curType := "property-short"
        } Else If (RegExMatch(curChunkNoStr,"i)^[ \t]*(Static[ \t]*)?[\w_]+\x28")) { ; methods
            iStart := i
            i++
            encl := enclosureCheck(curChunkNoStr)
            curType := "method"
        } Else If (RegExMatch(curChunkNoStr,"i)^[ \t]*(Static[ \t]*)?[\w_]+(\x5B|[ \t\r\n]+\x7B)",match)) { ; properties
            iStart := i
            i++
            encl := enclosureCheck(curChunkNoStr)
            curType := "property"
            
            ; Debug.Msg("encls:    " encl.exist " / " encl.even "`r`n" encl.p.exist "/" encl.p.total " || " encl.b.exist "/" encl.b.total " || " encl.p.exist "/" encl.p.total)
        } Else { ; properties set on _init
            iStart := i
            curChunk := curLine
            curChunkNoStr := curLineNoStr
            ; i++ ; operate on curLine only, so i++ happens at end of loop
            encl := enclosureCheck(curLineNoStr)
            curType := "other"
        }
        
        While ((encl.exist And !encl.even)) { ; compiles a multi-line statment
            i++
            
            If (!txtArr.Has(i))
                Break
            
            nextLine := txtArr[i]
            nextLineNoStr := StringOutline(nextLine)
            
            multi := true
            encl := enclosureCheck(nextLineNoStr)
            curChunk .= "`r`n" nextLine ; concatenate chunk
            curChunkNoStr .= "`r`n" nextLineNoStr
        }
        
        ; If (curType = "other")
            Debug.Msg(encl.exist " / " encl.even "`r`n" curChunk)
        
        oCallTip.LPar := 0, oCallTip.RPar := 0 ; reset brace count
        oCallTip.LBrace := 0, oCallTip.RBrace := 0
        oCallTip.LCBrace := 0, oCallTip.RCBrace := 0
        
        If (curType = "subclass") {
            GetClasses(curChunk, fileName, lineNum+iStart, className)
            i++
        } Else If (curType = "method") {
            shortMeth := false
            opener := SubStr(curChunk,1,InStr(curChunkNoStr,")"))
            opener := RegExReplace(opener,"`r|`n|[ ]{2,}|`t","")
            RegExMatch(opener,"i)(Static[ \t]+)?([\w\_\.]+)(\x28.*\x29)",match3)
            
            isStatic := (Trim(match3.Value(1)," `t") = "static") ? true : false
            memberName := match3.Value(2), memberParams := match3.Value(3)
            
            If (!InStr(curChunkNoStr,"{") And !InStr(curChunkNoStr,"=>")) {
                While (txtArr.Has(i+1)) {
                    i++
                    nextLine := txtArr[i]
                    nextLineNoStr := StringOutline(nextLine)
                    curChunk .= "`r`n" nextLine
                    curChunkNoStr .= "`r`n" nextLineNoStr
                    If (InStr(nextLine,"{"))
                        Break
                }
            } Else
                shortMeth := true
            
            encl := enclosureCheck(curChunkNoStr)
            While (encl.exist And !encl.even) {
                i++
                nextLine := txtArr[i]
                nextLineNoStr := StringOutline(nextLine)
                curChunk .= "`r`n" nextLine
                curChunkNoStr .= "`r`n" nextLineNoStr
                encl := enclosureCheck(nextLineNoStr)
            }
            
            memberList[memberName] := Map("name",memberName,"params",memberParams,"type",curType,"static",isStatic ; ,"body",curChunk
                                         ,"lineNum",lineNum+iStart,"fileName",fileName)
        } Else If (curType = "property") {
            If (RegExMatch(curChunk,"i)(Static[ \t]*)?([\w\_]+)[ \t]*\x5B")) {
                shortProp := false
                opener := SubStr(curChunk,1,InStr(curChunkNoStr,"]"))
                opener := RegExReplace(opener,"`r|`n|[ ]{2,}|`t","")
                RegExMatch(opener,"i)(Static[ \t]+)?([\w\_]+)(\x5B.*\x5D)",match3)
                memberParams := match3.Value(3)
                
                If (!InStr(curChunkNoStr,"{") And !InStr(curChunkNoStr,"=>")) {
                    While (txtArr.Has(i+1)) {
                        i++
                        nextLine := txtArr[i]
                        nextLineNoStr := StringOutline(nextLine)
                        curChunk .= "`r`n" nextLine
                        curChunkNoStr .= "`r`n" nextLineNoStr
                        If (InStr(nextLine,"{"))
                            Break
                    }
                } Else
                    shortProp := true
                
                encl := enclosureCheck(curChunkNoStr)
                While (encl.exist And !encl.even) {
                    i++
                    nextLine := txtArr[i]
                    nextLineNoStr := StringOutline(nextLine)
                    curChunk .= "`r`n" nextLine
                    curChunkNoStr .= "`r`n" nextLineNoStr
                    encl := enclosureCheck(nextLineNoStr)
                }
            } Else If (RegExMatch(curChunkNoStr,"i)[ \t]*(Static[ \t]*)?([\w\_]+)[ \t\r\n]*(\x7B|\=\>)",match3)) {
                opener := SubStr(curChunk,1,InStr(curChunkNoStr,"{")-1)
                opener := RegExReplace(opener,"`r|`n|[ ]{2,}|`t","")
                memberParams := ""
            }
            
            isStatic := (Trim(match3.Value(1)," `t") = "static") ? true : false
            memberName := match3.Value(2)
            
            memberList[memberName] := Map("name",memberName,"params",memberParams,"type",curType,"static",isStatic ; ,"body",curChunk
                                         ,"lineNum",lineNum+iStart,"fileName",fileName,"value","")
        } Else If (curType = "property-short") {
            RegExMatch(curChunkNoStr,"i)[ \t]*(Static[ \t]+)?([\w\_]+)",match3)
            isStatic := (Trim(match3.Value(1)," `t") = "static") ? true : false
            memberName := match3.Value(2)
            memberParams := ""
            curType := "property"
            
            memberList[memberName] := Map("name",memberName,"params",memberParams,"type",curType,"static",isStatic ; ,"body",curChunk
                                         ,"lineNum",lineNum+iStart,"fileName",fileName,"value","")
        } Else { ; short init properties
            ; Debug.Msg(curChunk)
            
            curLine := Trim(curLine," `t")
            curLineNoStr := Trim(curLineNoStr," `t")
            
            If (InStr(curLine,Chr(44)) = 1)
                isStatic := lastPropStatus
            Else If (InStr(Trim(curChunk),"Static") = 1)
                isStatic := true, lastPropStatus := true
            
            varList := getVar(curChunk,fileName,lineNum+iStart,lastPropStatus)
            
            For i, obj in varList {
                memberList[obj["varName"]] := Map("name",obj["varName"]
                                             ,"params",""
                                             ,"value",obj["varValue"]
                                             ,"type","property"
                                             ,"static",(obj["status"] = "static" ? true : false)
                                             ,"body",""
                                             ,"lineNum",lineNum+iStart,"fileName",fileName)
            }
        }
        
        obj["members"] := memberList
        classList[className] := obj
        
        i++
    }
    
    return classList
}

; GetCustFuncParams(funcBody) {
    ; r := RegExMatch(funcBody,"mi)^[ \t]*(Static[ \t]+)?([\w\.]+)(\x28.*\x29)([ \t]*\;.*)?[ \t\r\n]*\{",match)
    ; If (match.HasMethod("Value"))
        ; return match.Value(3)
    ; Else
        ; return ""
; }

; GetPropParams(paramBody) {
    ; r := RegExMatch(paramBody,"mi)^[ \t]*(Static[ \t]+)?([\w\.]+)(\[.*?\])?([ \t]*\;.*)?[ \t\r\n]*\{",match)
    ; If (match.HasMethod("Value"))
        ; return match.Value(3)
    ; Else
        ; return ""
; }

getVar(stringText,fileName,lineNum,lastStatus) {
    stringText := RegExReplace(stringText,"`r|`n|[ ]{2,}|`t","")
    
    status := "", noStr := StringOutline(stringText)
    If (RegExMatch(stringText,"^[ \t]*Global"))
        status := "Global"
    Else If (RegExMatch(stringText,"^[ \t]*Static"))
        status := "Static"
    Else If (lastStatus And RegExMatch(stringText,"^[ \t]*\,"))
        status := lastStatus
    
    stringText := RegExReplace(stringText,"(^[ \t]*Global[ \t]+|^[ \t]*Static[ \t]+)","")
    noStr := RegExReplace(noStr,"(^[ \t]*Global[ \t]+|^[ \t]*Static[ \t]+)","")
    comment := StringOutline(stringText,false), comment := SubStr(comment,InStr(comment,";"))
    stringText := RTrim(stringText," `t,")
    noStr := RTrim(noStr," `t,")
    stringText := SubStr(stringText,1,StrLen(noStr))
    
    varList := Array()
    varArr := StrSplit(noStr,",")
    
    If (status) {
        append := 0
        For i, txt in varArr {
            txt := Trim(txt," `t")
            If (!txt)
                Continue
            
            If (!append) {
                realLine := Trim(SubStr(stringText,1,StrLen(txt)))
                testLine := Trim(SubStr(noStr,1,StrLen(txt)))
            } Else {
                realLine .= "," Trim(SubStr(stringText,1,StrLen(txt)))
                testLine .= "," Trim(SubStr(noStr,1,StrLen(txt)))
            }
            
            p := 0, b := 0, c := 0
            testLine := StrReplace(StrReplace(testLine,"(","(",,LP),")",")",,RP)
            testLine := StrReplace(StrReplace(testLine,"[","[",,LB),"]","]",,RB)
            testLine := StrReplace(StrReplace(testLine,"{","{",,LC),"}","}",,RC)
            p := LP - RP, b := LB - RB, c := LC - RC
            
            If (p Or b Or c) {
                append++
            }  Else {
                append := 0
            
                If (splitter := InStr(realLine,":=")) {
                    varName := Trim(SubStr(realLine,1,splitter-1))
                    varValue := Trim(SubStr(realLine,splitter)," `t:=")
                } Else
                    varName := Trim(realLine," `t"), varValue := ""
                
                varList.Push(Map("varName",varName,"varValue",varValue,"fileName",fileName,"lineNum",lineNum,"status",status,"comment",comment))
            }
            
            stringText := SubStr(stringText,StrLen(txt)+1)
            stringText := Trim(stringText," ,")
            
            noStr := SubStr(noStr,StrLen(txt)+1)
            noStr := Trim(noStr," ,")
        }
    } Else {
        noStr := RTrim(noStr," `t")
        curPos := 1
        rg := "(([\w\_\.]+)[ \t]*\:\=)"
        lastPos := 0, lastLen := 0, lastVar := "", lastValue := "", t := 0
        While (r := RegExMatch(noStr,rg,match,curPos)) {
            t := A_Index
            If (lastPos = 0) {
                lastVar := match.Value(2)
                lastPos := match.Pos(0)
                lastLen := match.Len(0)
                lastValue := match.Value(0)
            } Else {
                varName := lastVar
                startPos := lastPos + lastLen
                curLen := match.Pos(0) - lastPos - lastLen
                varValue := Trim(SubStr(stringText,startPos,curLen)," `t")
                
                varList.Push(Map("varName",varName,"varValue",varValue,"fileName",fileName,"lineNum",lineNum,"status",status,"comment",comment))
                lastVar := match.Value(2), lastPos := match.Pos(0), lastLen := match.Len(0)
            }
            curPos := match.Pos(0) + match.Len(0)
        }
        
        If (t > 0) {
            varName := lastVar
            startPos := lastPos + lastLen
            varValue := Trim(SubStr(stringText,startPos)," `t")
            
            varList.Push(Map("varName",varName,"varValue",varValue,"fileName",fileName,"lineNum",lineNum,"status",status,"comment",comment))
        }
    }
    If (varList.Has(""))
        varList.Delete("")
    
    return varList
}

ParseObjectsInstances() { ; skip "ByCallback" labels... regex for "ByDerived" and "ByComment" labels - case insensitive!!!
    round1 := ObjectCreateList.Has("0") ? ObjectCreateList["0"] : Map()
    round2 := ObjectCreateList.Has("1") ? ObjectCreateList["1"] : Map()
    
    For i, obj in VariablesList {
        nextVar := false
        varValue := obj["varValue"], varName := obj["varName"], comment := obj["comment"]
        If (!varValue)
            Continue
        
        If (Type(varValue) = "Array" And !varValue.Length)
            Continue
        
        If (Type(varValue) = "Map" And !varValue.Count)
            Continue
        
        For label, obj2 in round1 { ; round 1, level 0
            labelSuffix := SubStr(label,-9)
            
            If (labelSuffix = "ByDerived" Or labelSuffix = "ByComment") {
                If (RegExMatch(varValue,"i)^" obj2["regex"]) Or RegExMatch(comment,"i)^" obj2["regex"])) {
                    ObjectList[VarName] := obj2["type"]
                    nextVar := true
                    Continue
                }
            } Else {
                If (InStr(varValue,obj2["regex"]) = 1) {
                    ObjectList[varName] := obj2["type"]
                    nextVar := true
                    Continue
                }
            }
        }
        
        If (nextVar)
            Continue
        
        If (RegExMatch(varValue,"i)New[ \t]+([\w\.]+)",match)) { ; AHK v1 class instance
            className := match.Value(1)
            ClassesList[varName] := Map("type","Instance","class",className,"extends","","parent","")
            Continue
        }
        
        If (RegExMatch(varValue,"i)([\w\.]+)\.New\(",match)) { ; AHK v2 class instance
            className := match.Value(1)
            ClassesList[varName] := Map("type","Instance","class",className,"extends","","parent","")
            Continue
        }
        
        For label, obj3 in round2 { ; round 2, level 1 - derived objects
            labelSuffix := SubStr(label,-9)
            labelRegex := obj3["regex"]
            
            For objName, objType in ObjectList {
                If (!InStr(labelRegex,"{" objType "}")) {
                    nextVar := True
                    Continue
                }
                
                If (labelSuffix != "ByDerived") {
                    nextVar := True
                    Continue
                }
                
                curRegex := StrReplace(labelRegex,"{" objType "}",objName)
                If (RegExMatch(varValue,"i)^" curRegex)) {
                    ObjectList[VarName] := obj3["type"]
                    nextVar := true
                    Continue
                }
            }
            
            If (nextVar)
                Continue
        }
        
        If (nextVar)
            Continue
    }
    
    For kw, kwType in KeywordList { ; load pre-defined objects
        If (InStr(kwType,"Object"))
            ObjectList[kw] := kwType
    }
}