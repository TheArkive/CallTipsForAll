; AHK v2
; === comment out if using this script as a library and these are already determined. ===
SendMode "Input"  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir A_ScriptDir  ; Ensures a consistent starting directory.
; =======================================================================================

FileEncoding "UTF-8"

Global SettingsGUI := "", AutoCompleteGUI := "", callTipGui := "" ; GuiObject

Global ClassesList := Map(), CustomFunctions := Map(), ObjectList := Map()    ; internal lists - by parsing
Global IncludesList := [], VariablesList := Array(), HotkeyList := Map()

Global ObjectCreateList := Map(), MethPropList := Map()                        ; internal lists - by lang files
Global KeywordFilter := Map(), KeywordList := Map(), FunctionList := Map()

Global Settings := Map() ; user settings object
Global IH := "" ; inputHook must be global or dupes will be made if user holds down a key
Global entryEnd := "`r`n`r`n`r`n" ; used to determine expected separation between elements in language files

; eventually move these to settings GUI
Global WrapTextChars := 80
Global useTooltip := false                ; ignores fontFace, fontSize, and callTipSelectable

SetupInputHook(false) ; suppress enter - true/false

Settings := ReadSettingsFromFile()

AddTrayMenu() ; modify tray menu

If (!Settings["ProgClassNN"] Or !Settings["ProgExe"]) ; prompt user for important settings (if these are blank)
    SettingsGUILoad()

GetEditorHwnd() ; checks Settings["ProgExe"] windows and populate oCallTip properties
SetHotkeys()
If (oCallTip.progHwnd)  ; initial loading of functions, custom functions, objects, if matching window found
    FullReload()

return ; end of auto execute section

class oCallTip { ; testing
    Static srcFiles := "", c := ""
    Static captureKeys := "abcdefghijklmnopqrstuvwxyz1234567890.,-=/'\[]{{}{}}{Space}{Backspace}{Up}{Down}{Left}{Right}{Escape}~"
    Static colNum := 0, lineNum := 0, lineText := ""
    Static scintBufferLen := 0, suppressEnter := false
    Static newParseClass := 0, newParseFunction := 0
    
    ; properties for call display and help link
    Static curIndex := "", fullDescArr := Map(), helpFile := ""
    
    ; properties for parent window and editor control
    Static ctlHwnd := 0, progHwnd := 0, progTitle := "", ctlActive := false, ctlConfirmed := false, clickCtlHwnd := 0
    
    ; properties for text elements near the caret
    Static curPhrase := "", curPhraseObj := "", parentObj := "", curPhraseType := "", parentObjType := ""
    
    ; store various GUI hwnd values?
    Static AutoCompleteHwnd := 0
    
    ; properties for enclosure matching
    Static LPar := 0, RPar := 0, LBrace := 0, RBrace := 0, LCBrace := 0, RCBrace := 0
    
    
    
    ; property1[a, b] { ; show
    
    ; }
    ; property2[c
                ; , d] ; show
    ; {
    
    ; }
    ; property3[] {
    
    ; }
    ; property4 ; show
    ; {
    
    ; }
    ; property5 => a + b ; show
    
    ; property6[a
                ; ,b] => c + d
    
    ; method1(a,b) { ; show
    
    ; }
    ; method2(c,d) ; show
    ; {
    
    ; }
    ; method3() {
    
    ; }
    ; method4() ; show
    ; {
    
    ; }
    ; method5() => a * b ; show
    
    ; method6(a
            ; ,b) => c * d
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
#INCLUDE LibV2\_scintilla_class_ext.ahk
#INCLUDE LibV2\_Load_Call_Tip_Data.ahk

#INCLUDE LibV2\_AHK-parser_TheArkive.ahk

#INCLUDE LibV2\TheArkive_Debug.ahk



; ================================================================
; hotkeys - global ; 
; ================================================================

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
        If (!AutoCompleteGUI["KwList"].Focused)
            AutoCompleteGUI["KwList"].Focus()
        SendInput "{Down}"
    } Else SendInput "{Down}"
}

~LButton::ClickCheck(A_ThisHotkey)
~MButton::
{
    closeCallTip(), closeAutoComplete()
    oCallTip.suppressEnter := false, IH.Stop(), SetupInputHook(false)
}

; ^!w:: ; hotkey to wrap text in clipboard
; {
    ; newText := WrapText(WrapTextChars)
    ; If (newText)
        ; clipboard := newText
; }

; ^!u:: ; hotkey to unwrap text in clipboard
; {
    ; newText := unwrapText()
    ; If (newText)
        ; clipboard := newText
; }

F10::{
    ; NPPMSG := WM_USER + 1000 /// WM_USER = 0x0400 / 1024
    ; RUNCOMMAND_USER := WM_USER + 3000 /// 4024
    
    ; NPPM_GETCURRENTBUFFERID
    ; NPPM_GETCURRENTDIRECTORY
    ; NPPM_GETFILENAME
    
    ; ?
    ; NPPM_GETCURRENTDOCINDEX
    r := ScintillaExt.SendMsg("SCI_GETCODEPAGE")
    msgbox r.dll
}

F11:: ; list custom functions, commands, and objects - for debugging List_*.txt files only
{
    A_Clipboard := Jxon_dump(HotkeyList,4)
    MsgBox "check HotkeyList: " HotkeyList.Count "`r`nPaste object structure into a text document."
    
    A_Clipboard := Jxon_dump(VariablesList,4)
    Msgbox "check VarList: " VariablesList.Length "`r`nPaste object structure into a text document."
    
    A_Clipboard := Jxon_Dump(FunctionList,4)
    msgbox "check FunctionList: " FunctionList.Count "`r`nPaste object structure into a text document."
    
    A_Clipboard := Jxon_dump(CustomFunctions,4)
    msgbox "check CustomFunctions: " CustomFunctions.Count "`r`nPaste object structure into a text document."
    
    A_Clipboard := Jxon_dump(MethPropList,4)
    msgbox "check MethPropList: " MethPropList.Count "`r`nPaste object structure into a text document."
    
    A_Clipboard := Jxon_dump(ObjectList,4)
    msgbox "check ObjectList: " ObjectList.Count "`r`nPaste object structure into a text document."
    
    A_Clipboard := Jxon_dump(ClassesList,4)
    msgbox "check ClassesList: " ClassesList.Count "`r`nPaste object structure into a text document."
    
    A_Clipboard := Jxon_dump(IncludesList,4)
    msgbox "check IncludesList: " IncludesList.Length "`r`nPaste object structure into a text document."
    
    A_Clipboard := Jxon_dump(ObjectCreateList,4)
    msgbox "check ObjectCreateList: " ObjectCreateList.Count "`r`nPaste object structure into a text document."
}

; F9::
; {
    ; oCallTip.srcFiles := A_ScriptDir "\Languages\" Settings["ActiveLanguage"] ;ZZZ - this needs to be here for proper functionality
    ; LoadKeywordsList()
    ; LoadFunctionsList()
    
    ; objMatchText := LoadMethPropList()
    ; LoadObjectCreateList(objMatchText)
    
    ; ReParseText()
; }

; ======================================================================================
; input hook - for processing during user input - i prefer hotkey to invoke reload.
; ======================================================================================
FullReload()
{
    If (!oCallTip.progHwnd) ;ZZZ - mostly only applies to first run
        GetEditorHwnd() ;ZZZ - this function now also puts text editor PID into oCallTip
    
    If (oCallTip.ctlHwnd) {
        ScintillaExt.pid := oCallTip.progPID ; need PID for sending messages to edit control in some cases
        ScintillaExt.ctlHwnd := oCallTip.ctlHwnd
    }
    
    oCallTip.srcFiles := A_ScriptDir "\Languages\" Settings["ActiveLanguage"] ;ZZZ - this needs to be here for proper functionality
    
    LoadKeywordsList() ; reload defined language elements
    LoadFunctionsList()
    objMatchText := LoadMethPropList()
    LoadObjectCreateList(objMatchText)
    
    ReParseText() ; reload user defined elements
}

SetupInputHook(suppressEnter)
{
    IH := InputHook("V I1","","") ; options , endKeys , matchList
    IH.OnKeyDown := Func("keyPress")
    IH.KeyOpt(oCallTip.captureKeys,"N") ; don't capture all keys, that causes problems
    
    If (suppressEnter)
        IH.KeyOpt("{Enter}{Tab}","N S")
    Else
        IH.KeyOpt("{Enter}{Tab}","N")
    
    IH.Start()
}

keyPress(iHook,VK,SC) { ; InputHook ;ZZZ - significant changes here...
    a := 0, b := 0, c := 0
    Try a := WinActive("ahk_id " oCallTip.progHwnd) ; check if editor control is active
    Try b := AutoCompleteGUI.HasProp("hwnd") ? WinActive("ahk_id " AutoCompleteGUI.hwnd) : 0 ; check if auto-complete is active
    Try c := SettingsGUI.HasProp("hwnd") ? WinActive("ahk_id " SettingsGUI.hwnd) : 0
    
    If (GetKeyState("Alt") Or GetKeyState("Ctrl") Or (!a And !b) Or c)    ; if alt-tab editor control is inactive
        oCallTip.ctlActive := false
    Else If (a Or b)                                 ; if a > 0 then editor control is active
        oCallTip.ctlActive := true
    
    If (oCallTip.ctlActive) { ; populate or clear KeywordFilter based on .ctlActive
        hCtl := ControlGetFocus("ahk_id " oCallTip.progHwnd) ; active ctl hwnd
        
        curClassNN := ""
        Try curClassNN := ControlGetClassNN(hCtl) ; active ctl ClassNN
        ctlClassNN := Settings["ProgClassNN"]
        
        If (InStr(curClassNN,ctlClassNN))
            oCallTip.ctlHwnd := hCtl, ScintillaExt.ctlHwnd := hCtl
        
        ProcInput()
        curPhrase := oCallTip.curPhrase, parentObj := oCallTip.parentObj
        KeywordFilter := KwSearchDeep(curPhrase, parentObj)
    } Else {
        KeywordFilter.Clear()
        oCallTip.suppressEnter := false
        IH.Stop(), SetupInputHook(oCallTip.suppressEnter) ; no need to continue processing
        return
    }
    
    If (KeywordFilter.Count) ; set {Enter} and {Tab} suppression
        oCallTip.suppressEnter := true
    Else                    ; disable {Enter}/{Tab} suppression if KeywordFilter is empty
        oCallTip.suppressEnter := false, closeAutoComplete()
    
    If (IsObject(AutoCompleteGUI)) {
        oCallTip.suppressEnter := true, ctl := ""
        Try ctl := AutoCompleteGUI["KwList"] ; AutoCompleteGUI may be in the process of closing
        acON := (ctl) ? true : false
    } Else
        acON := false, ctl := ""
    
    If (vk = 27) { ; escape key
        closeCallTip()
        closeAutoComplete()
        oCallTip.suppressEnter := false
    } Else If (vk = 8 And !KeywordFilter.Count) { ; backspace key
        oCallTip.suppressEnter := false
        closeAutoComplete()
    } Else If (vk = 9) { ; tab key
        If (acON) {
            ctl.Focus()
            
            If (!ctl.Value Or ctl.Value = KeywordFilter.Count)
                ctl.Value := 1 ; select first row if no selection or not active
            Else If (ctl.Value < KeywordFilter.Count)
                ctl.Value++ ; increment selection on subsequent key presses
        }
    } Else If (vk = 13) { ; enter key
        If (acON and !ctl.Focused) {
            closeAutoComplete()
        } Else If (acON and ctl.Focused) {
            kwSel := ctl.Text
            kwSel := RegExReplace(kwSel,"\.|\(f\)|\(m\)|\(o\)|\(c\)","")
            
            hwnd := oCallTip.ctlHwnd
            ScintillaExt.ctlHwnd := hwnd
            
            ctlClassNN := Settings["ProgClassNN"]
            
            If (kwSel) {
                If (ctlClassNN = "edit") { ; edit specific, ie. notepad.exe
                    sPos := BufferAlloc(4,0)
                    SendMessage 176, sPos.ptr, 0, hwnd ; EM_GETSEL - used this way it gets the carat position
                    curPos := NumGet(sPos,"UInt"), newPos := curPos - StrLen(curPhrase)
                    SendMessage 177, newPos, curPos, hwnd ; EM_SETSEL
                    EditPaste kwSel, hwnd
                } Else If (ctlClassNN = "scintilla") { ; scintilla specific, ie. notepad++.exe
                    curPos := ScintillaExt.SendMsg("SCI_GETCURRENTPOS")
                    newPos := curPos.dll - StrLen(curPhrase)
                    
                    r1 := ScintillaExt.SendMsg("SCI_SETANCHOR",newPos)
                    r2 := ScintillaExt.SendMsg("SCI_REPLACESEL",0,kwSel)
                }
                closeAutoComplete()
                
                t := KeywordFilter.Has(kwSel) ? KeywordFilter[kwSel] : "" ; get keyword type
                If (t != "property") ; simplify - do NOT show call tip on ENTER if type = ...
                    DisplayCallTip()
                
                KeywordFilter.Clear()
            }
        }
        
        oCallTip.suppressEnter := false
    } Else If ((VK = 37 or VK = 39) And oCallTip.ctlActive) { ; left and right arrows
        oCallTip.suppressEnter := false
        If (acON) ; make sure auto-complete GUI is visible
            closeAutoComplete()
    } Else { ; skip autocomplete if...
        If (IsObject(CallTipGUI) Or GetKeyState("Ctrl") Or !Settings["AutoComplete"] Or IsObject(SettingsGUI)) {
            oCallTip.suppressEnter := false
            return
        }
                SetTimer "LoadAutoComplete", -30 ; ... otherwise load auto-complete
    }
    
    ; If (oCallTip.ctlActive)
        ; debugmsg("suppress: " ocallTIp.suppressEnter " / kw: " KeywordFilter.Count " / curPhrase: " curPhrase " / hwnd: " a)
    
    IH.Stop() ; global InputHook prevents duplicate IH objects on rapid key entry, ie. holding a key down.
    SetupInputHook(oCallTip.suppressEnter)
}

; ======================================================================================
; Functions related to hotkey events
; ======================================================================================
AutoComp(ThisKey:="") {
    LoadAutoComplete(true) ; force ProcInput() and populating of KeywordFilter
}

ReloadGui(ThisKey:="") {
    ClickCheck("Reload")
}

LoadAutoComplete(force:=false) { ; use force:=true for manual invocation (mostly)
    If (!oCallTip.progHwnd) ; this should be conditional
        GetEditorHwnd()
    
    If (!oCallTip.ctlActive Or !KeywordList.Count) {
        closeAutoComplete()
        oCallTip.suppressEnter := false
        return
    }
    
    If (force) ; this is normally done on keyPress() callback
        ProcInput()
    
    curPhrase := oCallTip.curPhrase
    curPhraseObj := oCallTip.curPhraseObj
    parentObj := oCallTip.parentObj
    parentObjType := oCallTip.parentObjType
    curPhraseType := oCallTip.curPhraseType
    
    If (force) ; this is normally done on keyPress() callback
        KeywordFilter := KwSearchDeep(curPhrase, parentObj)
    
    testWord := ""
    If (KeywordFilter.Count = 1)
        For kw in KeywordFilter
            testWord := RegExReplace(kw,"\(m\)|\(f\)|\.","")
    
    If ((KeywordFilter.Count And parentObj And !curPhrase) Or force) { ; for manual invocation, or after typing "object."
        oCallTip.suppressEnter := true, IH.Stop(), SetupInputHook(true)
        LoadAutoCompleteGUI(KeywordFilter)
    } Else {
        If (KeywordFilter.Count And curPhrase != testWord) { ;ZZZ - normally .Count will be > 1
            oCallTip.suppressEnter := true
            LoadAutoCompleteGUI(KeywordFilter)
        } Else {                                ;ZZZ - if user manually finishes typing word, close AutoComplete
            closeAutoComplete(), oCallTip.suppressEnter := false, IH.Stop(), SetupInputHook(false)
            KeywordFilter.Clear
        }
    }
}

KwSearchDeep(curPhrase, parentObj) {
    resultList := Map()
    If (curPhrase And !parentObj) {
        If (ObjectList) {
            For objName in ObjectList {
                If (inStr(objName,curPhrase))
                    resultList[objName] := "User Object"
            }
        }
        
        If (ClassesList) {
            For className, obj in ClassesList {
                If (inStr(className,curPhrase))
                    resultList[classname] := "User " obj["type"]
            }
        }
        
        If (CustomFunctions) {
            For funcName, obj in CustomFunctions {
                If (InStr(funcName,curPhrase))
                    resultList[funcName] := "User Function"
            }
        }
        
        ; If (!resultList.Count) {
        For kw, kwType in KeywordList { ; normal keyword look
            If (InStr(kw,curPhrase))
                resultList[kw] := kwType
        }
        ; }
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
            If (thisList = "ObjectList") {
                curType := ObjectList[curObj]
                curMethPropList := MethPropList[curType]
                curMethList := curMethPropList["method"]
                For methName in curMethList {
                    If (!curPhrase)
                        resultList[methName] := "Method"
                    Else If (InStr(methName,curPhrase))
                        resultList[methName] := "Method"
                }
                
                curPropList := curMethPropList["property"]
                For propName in curPropList {
                    If (!curPhrase)
                        resultList[propName] := "Property"
                    Else If (inStr(propName,curPhrase))
                        resultList[propName] := "Property"
                }
            } Else If (thisList = "ClassesList") {
                resultList := kwParseClass(curObj,resultList,curPhrase,parentObj)
            }
        } ; end obj/class searching
    }
    
    return resultList
}

kwParseClass(curObj, resultList, curPhrase, parentObj) {
    classObj := ClassesList[curObj]
    classType := classObj["type"], className := curObj
    classObj := (classObj["type"] = "Instance") ? ClassesList[classObj["class"]] : classObj
    extends := classObj["extends"], parent := classObj["parent"]
    
    memList := classObj["members"]
    For memName, obj in memList {
        params := obj["params"], curType := obj["type"], isStatic := obj["static"]
        
        If (classType := "class" And isStatic) {
            curType := (curType = "property" And params) ? "property-params" : curType
            If (!curPhrase)
                resultList[memName] := curType
            Else If (InStr(memName,curPhrase))
                resultList[memName] := curType
        } Else If (classType := "instance" And !isStatic) {
            curType := (curType = "property" And params) ? "property-params" : curType
            If (!curPhrase)
                resultList[memName] := curType
            Else If (InStr(memName,curPhrase))
                resultList[memName] := curType
        }
    }
    
    extends := StrReplace(StrReplace(extends,className ".",""),"." className,"")
    If (extends And ClassesList.Has(extends)) {
        resultList := kwParseClass(extends, resultList, curPhrase, parentObj)
    }
    
    return resultList
}

debugToolTip() {
    ProcInput()
    
    funcName := "", funcText := ""
    curPhrase := oCallTip.curPhrase
    curPhraseObj := oCallTip.curPhraseObj
    curPhraseType := oCallTip.curPhraseType
    parentObj := oCallTip.parentObj
    parentObjType := oCallTip.parentObjType
    scintBufferLen := oCallTip.scintBufferLen
    lineText := oCallTip.lineText
    
    lineNum := oCallTip.lineNum
    colNum := oCalltip.colNum
    
    CaretGetPos(x,y)
    Tooltip "curPhrase :" curPhrase ": / curPhraseObj: " curPhraseObj
          . "`r`nfuncName: " funcName " / funcText: " funcText
          . "`r`nparentObj: " parentObj " / parentObjType: " parentObjType 
          . "`r`nphraseType: " curPhraseType
          . "`r`nLine: " lineNum " / Col: " colNum
          . "`r`nLineText: " lineTExt
          . "`r`nscintBufferLen: " scintBufferLen
          , x, (y+30) 
}

CloseScript(ThisKey:="") {
    Msgbox "Closing CallTipsForAll!"
    ExitApp
}

ClickCheck(curKey) {
    CheckMouseLocation()
    oCallTip.suppressEnter := false, IH.Stop(), SetupInputHook(false)
    
    If (!oCallTip.ctlActive) {
        closeCallTip()
        return
    }
    
    If (oCallTip.clickCtlHwnd != oCallTip.AutoCompleteHwnd)
        closeAutoComplete()
    
    MultiClick.Detect(curKey)
    c := MultiClick.Count
    
    If (curKey = "~LButton" And c = 2 And Settings["LoadCallTipOnClick"]) {
        If (!oCallTip.ctlActive)
            return
        
        DisplayCallTip()
    } Else If (curKey = "Reload") {
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

DisplayCallTip(ThisKey:="") {
    closeAutoComplete()
    If (!oCallTip.ctlActive)
        return
    
    closeCallTip()
    ProcInput() ; process data near caret to determin call tip contents
    
    If (Settings["DebugToolTip"])    ; use debug tool tip if enabled
        debugToolTip()
    Else
        LoadCallTip()
}

closeCallTip() {
    If (Settings["DebugToolTip"])                        ; close call tip (tooltip version)
        ToolTip
    If (IsObject(callTipGui)) {
        callTipGui.Destroy()
        callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
        WinActivate "ahk_id " oCallTip.progHwnd
    }
}

closeAutoComplete() {
    If (WinExist("ahk_id " oCallTip.AutoCompleteHwnd)) {
        oCallTip.AutoCompleteHwnd := 0
        AutoCompleteGUI.Destroy()
        AutoCompleteGUI := ""
        If (oCallTip.ctlActive)
            WinActivate "ahk_id " oCallTip.progHwnd
    }
}