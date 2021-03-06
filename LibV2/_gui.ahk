; Contains functions that invoke GUI elements, and GuiControl event callbacks, including any support functions used.
; 
; GUIs: CallTip, Settings

; ======================================================================================
; Load Call Tip
; ======================================================================================

LoadCallTip() { ; curPhrase, curPhraseType ---> globals
    fontFace := Settings["fontFace"]
    fontSize := Settings["fontSize"]
    fontColor := Settings["fontColor"]
    bgColor := Settings["bgColor"]
    
    curPhrase := oCallTip.curPhrase ; Global curPhrase, curPhraseObj, curPhraseType, parentObj, parentObjType
    curPhraseObj := oCallTip.curPhraseObj
    curPhraseType := oCallTip.curPhraseType
    parentObj := oCallTip.parentObj
    parentObjType := oCallTip.parentObjType
    
    If (curPhrase = "")
        return
    
    If (!oCallTip.curIndex)
        oCallTip.curIndex := 1
    curIndex := oCallTip.curIndex
    fullDesc := "", phraseRedir := "", helpLink := "", descArr := Array()
    
    If (curPhraseType = "function")
        phraseRedir := "function"
    Else If (curPhraseType = "CustomFunction")
        phraseRedir := "function"
    Else if (curPhraseType = "Flow")
        phraseRedir := "function"
    Else If (curPhraseType = "Command")
        phraseRedir := "function"
    
    if (phraseRedir = "function") {
        For funcName, obj in FunctionList {
            If (funcName = curPhrase) { ; Or funcName = curPhraseObj ??
                descArr := obj["desc"] ; desc array
                helpLink := obj["helpLink"]
                break
            }
        }
        
        If (!descArr.Length) {
            For funcName, obj in CustomFunctions {
                If (funcName = curPhrase) {
                    params := RegExReplace(obj["params"]," |`t","")
                    If ((params = "()" Or params = "") And Settings["HideNoParamFuncs"])
                        return
                    
                    descStr := obj["desc"]
                    descArr.Push(descStr) ; make array from str
                    break
                }
            }
        }
        
        If (!descArr.Length)
            return
        Else {
            fullDescArr := Map()
            For index, desc in descArr {
                curDescObj := Map()
                curDescObj["desc"] := desc
                curDescObj["helpLink"] := helpLink
                fullDescArr[index] := curDescObj
            }
            curDescObj := "", descArr := ""
        }
    } Else If (curPhraseType = "object") {
        found := false
        For objName, curObjType in ObjectList { ; find element and correct case
            If (objName = curPhrase) {
                objType := curObjType
                found := true
                Break
            }
        }
        
        If (!found)
            return
        
        fullDescArr := Map(), i := 1
        listObj := MethPropList.Has(objType) ? MethPropList[objType] : ""
        
        If (listObj) {
            descArr := listObj["desc"]
            helpLink := listObj["helpLink"]
            
            For index, desc in descArr {
                curObj := Map()
                curObj["desc"] := desc, curObj["helpLink"] := helpLink
                fullDescArr[i] := curObj
                i++
            }
            
            addon := listObj["moreObj"]
            addon := StrSplit(addon,",")
            For j, objType in addon {
                descArr := MethPropList[objType]["desc"]
                For index, desc in descArr {
                    curObj := Map()
                    curObj["desc"] := desc, curObj["helpLink"] := helpLink
                    fullDescArr[i] := curObj
                    i++
                }
            }
        }
        curObj := "", listObj := "", obj := ""
    } Else If (curPhraseType = "method" Or curPhraseType = "property") {
        found := false
        For objName, curObjType in ObjectList { ; find element and correct case
            If (objName = parentObj) {
                objType := curObjType
                found := true
                Break
            }
        }
        
        If (!found)
            return
        
        fullDescArr := Map(), i := 1
        listObj := MethPropList.Has(objType) ? MethPropList[objType] : ""
        memObj := listObj[curPhraseType]
        
        For methPropName, descObj in memObj {
            If (methPropName = curPhrase) {
                descArr := descObj["desc"]
                helpLink := descObj["helpLink"]
                helpLink := StrReplace(helpLink,"[MemberName]",methPropName)
                
                For index, desc in descArr {
                    titleHeader := StrUpper(curPhraseType,"T")
                    desc := objType " " titleHeader ":`r`n`r`n" StrReplace(desc,"[MemberName]",methPropName)
                    curObj := Map()
                    curObj["helpLink"] := helpLink, curObj["desc"] := desc
                    fullDescArr[i] := curObj
                    i++
                }
                
                Break
            }
        }
        
        curObj := "", listObj := "", memObj := "", descObj := ""
    } Else If (curPhraseType = "Class" or curPhraseType = "Instance") {
        For clName, cObj in ClassesList {
            If (clName = curPhrase) {
                className := clName ; correct case for lookup
                extends := cObj["extends"] ? " extends " cObj["extends"] : ""
                classParent := cObj["parent"]
                break
            }
        }
        
        If (!className)
            return
        
        obj := ClassesList[className] ; obj := ClassesList.Has(className) ? ClassesList[className] : ""
        If (!obj)
            return
        
        If (curPhraseType = "Instance") {
            className := obj["class"]
            obj := ClassesList[className]
            If (!obj)
                return
        }
        
        subClasses := ""
        For clName, cObj in ClassesList {
            parent := cObj.Has("parent") ? cObj["parent"] : ""
            subClasses .= (parent = className) ? clName ", " : ""
        }
        subClasses := Trim(subClasses,", ")
        
        m := 0, p := 0
        listObj := obj["members"]
        fullDescArr := Map(), i := 1, propList := "", methList := ""
        
        For memName, memObj in listObj {
            memType := memObj["type"]
            stat := memObj["static"] ? " (S)" : ""
            If (memType = "method") {
                m++
                If (m <= 20)
                    methList .= "." memName stat ", "
                Else If (m = 21)
                    methList .= "..."
                
            } Else if (memType = "property" Or memType = "propertySmall") {
                p++
                If (p <= 20)
                    propList .= "." memName stat ", "
                Else If (p = 21)
                    propList .= "..."
            }
        }
        methList := Trim(methList,", "), propList := Trim(propList,", ")
        
        tempDesc := className " Class" (curPhraseType = "Instance" ? " Instance" : "") extends "`r`n" ; ":`r`n`r`n"
        tempDesc .= !subClasses ? "" : "    Sub Classes: " subClasses "`r`n"
        tempDesc .= !classParent ? "" : "    Parent: " classParent "`r`n"
        tempDesc .= "`r`n"
        tempDesc .= methList ? "Methods (" m "): " methList "`r`n`r`n" : ""
        tempDesc .= propList ? "Properties (" p "): " propList : ""
        tempDesc := Trim(tempDesc," `t`r`n")
        curObj := Map("desc",tempDesc,"helpLink","")
        fullDescArr[1] := curObj
    } Else If (curPhraseType = "class-method" or curPhraseType = "class-property") {
        subType := StrReplace(curPhraseType,"class-",""), fullDescArr := Map()
        
        For clName in ClassesList {
            If (clName = parentObj) {
                className := clName ; correct case for lookup
                break
            }
        }
        
        If (!className)
            return
        
        parObjType := ClassesList[className]["type"]
        parObjName := (parObjType = "Instance") ? ClassesList[className]["class"] : className
        memList := ClassesList[parObjName]["members"]
        
        For memName, memObj in memList {
            memMatch := (memName = curPhrase) ? memName : (memName = "__" curPhrase) ? "__" curPhrase : ""
            If (memMatch) {
                params := memObj["params"]
                tempDesc := parObjName " Class:`r`n`r`n" parentObj "." memName params
                curObj := Map("desc",tempDesc,"helpLink","")
                fullDescArr[1] := curObj
                break
            }
        }
    } Else ; unrecognized keyword, so return
        return
    
    oCallTip.fullDescArr := fullDescArr
    fullDescArrObj := fullDescArr[curIndex]
    fullDesc := fullDescArrObj["desc"]
    newFullDesc := ""
    
    Loop Parse fullDesc, "`n", "`r"
        newFullDesc .= "`r`n" RegExReplace(A_LoopField,"^/","`r`n")
    fullDesc := Trim(newFullDesc,"`r`n")
    
    If (!fullDesc)
        return
    
    curMon := GetMonitorData() ; was using > outX,outY ; get actvie monitor dims and L/R/T/B
    dims := GetTextDims(fullDesc,fontFace,fontSize,curMon.w * 0.75) ; no maxWidth, get full dims
    
    vscroll := "-VScroll"
    
    CoordMode "Caret", "Screen"
    CaretGetPos(outX, outY)
    
    If (useTooltip) {
        outX += -70, outY += -25
        Tooltip fullDesc, outX, outY
    } Else {
        outX += -11, outY += dims.avgH
        
        If (callTipGui)
            callTipGui.Destroy()
        
        callTipGui := Gui.New("-DPIScale -Border AlwaysOnTop +Owner" oCallTip.progHwnd) ; Splitpath drive
        callTipGui.BackColor := bgColor
        callTipGui.SetFont("s" fontSize " c" fontColor,fontFace)
        
        If (!Settings["CallTipSelectable"]) {
            ctl := callTipGui.Add("Text","w" dims.w " h" dims.h " x5 y5",fullDesc) ; " +c" fontColor 
        } Else {
            scrollW := SysGet(2)
            ctl := callTipGui.Add("Edit","w" (dims.w+(scrollW*2)) " h" dims.h " x5 y5 +Background" bgColor " " vscroll " -E0x200 ReadOnly",fullDesc)
        }
        
        If (!Settings["CallTipSelectable"])
            ctl.OnEvent("Click","gui_click")
        
        dCtl := "", dCtlw := "", dCtlh := ""
        dCtl := ctl.GetPos(,,dCtlw,dCtlh)
        posTest := OutX + dCtlw
        
        If (posTest > curMon.right) {
            offset := posTest - curMon.right
            OutX := OutX - offset - 20
        }
        
        h := dCtlh + 10, w := dCtlw + 10
        callTipGui.Show("x" OutX " y" outY " h" h " w" w " NA NoActivate")
    }
}

gui_click(ctlObj,info) {
    curIndex := oCallTip.curIndex
    fullDescArrObj := oCallTip.fullDescArr[curIndex] ; map
    fullDesc := fullDescArrObj["desc"]
    link := fullDescArrObj["helpLink"]
    helpFile := oCallTip.helpFile
    
    If (InStr(link,"/") = 1 And FileExist(helpFile)) {
        helpFile := StrReplace(helpFile," ","%20")
        cmd := "hh.exe mk:@MSITStore:" helpFile "::" Trim(link,"`r`n")
        Run cmd,, "maximize"
        callTipGui.Destroy()
        callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
    }
    
    If (callTipGui) { ; closeTipOnClick - this shouldn't be an issue...
        callTipGui.Destroy()
        callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
    }
}

; ================================================================
; Settings GUI
; ================================================================

SettingsGUILoad() {
    KillAllHotkeys()
    closeCallTip()
    closeAutoComplete()
    
    SettingsGUI := Gui.New("+OwnDialogs","CallTipsForAll v2")
    SettingsGUI.OnEvent("Close","gui_close")
    SettingsGUI.OnEvent("escape","gui_close")
    ActiveLanguage := Settings["ActiveLanguage"]
    
    If (IsObject(callTipGui)) {
        callTipGui.Destroy()
        callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
    }
    
    langList := [], choose := 1
    Loop Files A_ScriptDir "\Languages\*", "D"
    {
        langList.Push(A_LoopFileName)
        choose := (A_LoopFileName = ActiveLanguage) ? A_Index : choose
    }
    
    SettingsGUI.Add("Text","x5 y10","Language:")
    SettingsGUI.Add("DropDownList","x+2 yp-4 w100 vActiveLanguage Choose" choose,langList).OnEvent("change","gui_change_events")
    
    ctl := SettingsGUI.Add("Checkbox","xm y+8 vLoadCallTipOnClick","Load call tip on double-click") ; ListView
    ctl.OnEvent("click","gui_change_events")
    ctl.Value := Settings["LoadCallTipOnClick"]
    
    ctl := SettingsGUI.Add("Checkbox","x+8 vAutoComplete Section","Auto-Complete while typing")
    ctl.OnEvent("click","gui_change_events")
    ctl.Value := Settings["AutoComplete"]
    
    ; ctl := SettingsGUI.Add("Checkbox","xm y+8 vAutoComplete","Auto-Complete while typing")
    ; ctl.OnEvent("click","gui_change_events")
    ; ctl.Value := Settings["AutoComplete"]
    
    ; ctl := SettingsGUI.Add("Checkbox","x+8 vCloseTipOnFocusChange Section","Close call tip on focus change")
    ; ctl.OnEvent("click","gui_change_events")
    ; ctl.Value := Settings["CloseTipOnFocusChange"]
    
    ctl := SettingsGUI.Add("Checkbox","xm y+8 vDebugToolTip","Show Debug Tooltip")
    ctl.OnEvent("click","gui_change_events")
    ctl.Value := Settings["DebugToolTip"]
    
    ctl := SettingsGUI.Add("Checkbox","xs yp vCallTipSelectable Disabled","Selectable call tips")
    ctl.OnEvent("click","gui_change_events")
    ctl.Value := Settings["CallTipSelectable"]
    
    ctl := SettingsGUI.Add("Checkbox","xm y+8 vHideNoParamFuncs","Omit user functions without params from call tips")
    ctl.OnEvent("click","gui_change_events")
    ctl.Value := Settings["HideNoParamFuncs"]
    
    ctl := SettingsGUI.Add("Button","vPickFont xm y+8","Select Font")
    ctl.OnEvent("click","gui_change_events")
    
    ctl := SettingsGUI.Add("Button","vPickFontColor x+0","Select Font Color")
    ctl.OnEvent("click","gui_change_events")
    
    ctl := SettingsGUI.Add("Button","vPickBgColor x+0","Select Background Color")
    ctl.OnEvent("click","gui_change_events")
    
    SettingsGUI.Add("Edit","vFontDemo xm y+8 w330 h50 ReadOnly -E0x200 -VScroll","Call Tip Test")
    
    SettingsGUI.Add("Text","xm y+12","Text Editor EXE:")
    SettingsGUI.Add("Edit","vProgExe xm y+2 w330")
    SettingsGUI["ProgExe"].Value := Settings["ProgExe"]
    
    SettingsGUI.Add("Text","xm y+8","Editor Control ClassNN - without number (ie. `"edit`" not `"edit1`"):")
    SettingsGUI.Add("Edit","vProgClassNN xm y+2 w330")
    SettingsGUI["ProgClassNN"].Value := Settings["ProgClassNN"]
    
    SettingsGUI.Add("Text","xm y+8","Base File (only needed to process includes):")
    SettingsGUI.Add("Edit","vBaseFile xm y+2 w290 r1",Settings["BaseFile"])
    SettingsGUI.Add("Button","vSelBaseFile x+0 w20","...").OnEvent("click","gui_change_events")
    SettingsGUI.Add("Button","vClearBaseFile x+0 w20","X").OnEvent("click","gui_change_events")
    
    w := 130
    SettingsGUI.Add("Text","xm y+8","Call-Tip Hotkey:")
    SettingsGUI.Add("Hotkey","vCallTipInvoke xm y+2 w" w,Settings["CallTipInvoke"]).OnEvent("change","gui_hotkey_events") ; Settings["CallTipInvoke"]
    
    SettingsGUI.Add("Text","xs yp-15","Auto-Complete Hotkey:")
    SettingsGUI.Add("Hotkey","vAutoCompleteInvoke xs y+2 w" w,Settings["AutoCompleteInvoke"]).OnEvent("change","gui_hotkey_events") ; Settings["AutoCompleteInvoke"]
    
    SettingsGUI.Add("Text","xm y+8","Reload/Base File Hotkey:`r`n(double-tap for Base File)")
    SettingsGUI.Add("Hotkey","vReloadInvoke xm y+2 w" w,Settings["ReloadInvoke"]).OnEvent("change","gui_hotkey_events") ; Settings["ReloadInvoke"]
    
    SettingsGUI.Add("Text","xs yp-28","`r`nClose CallTipsForAll Hotkey:")
    SettingsGUI.Add("Hotkey","vCloseInvoke xs y+2 w" w,Settings["CloseInvoke"]).OnEvent("change","gui_hotkey_events") ; Settings["CloseInvoke"]
    
    SettingsGUI.Add("Text","xm y+8","Last parse took:`r`n     " (Settings.Has("lastParse") ? Settings["lastParse"] : ""))
    
    SetFontDemo()
    SettingsGUI.Show()
}

gui_hotkey_events(ctl, info) {
    result := ""
    ctrl := GetKeyState("Ctrl")        ; fix key combos with {Space}
    alt := GetKeyState("Alt")        ; fix key combos with {Space}
    shift := GetKeyState("Shift")    ; fix key combos with {Space}
    If ((ctrl Or alt Or shift) And GetKeyState("Space"))
        result := (ctrl ? "^" : "") (alt ? "!" : "") (shift ? "+" : "") "Space"
    
    result ? ctl.Value := result : ""
}

gui_change_events(ctl, Info) { ; GuiControlObject
    ; if (ctl.Name = "ActiveLanguage")
        ; Settings["ActiveLanguage"] := ctl.Text
    if (ctl.Name = "LoadCallTipOnClick")
        Settings["LoadCallTipOnClick"] := ctl.Value
    else if (ctl.Name = "CloseTipOnFocusChange")
        Settings["CloseTipOnFocusChange"] := ctl.Value
    else if (ctl.Name = "DebugToolTip")
        Settings["DebugToolTip"] := ctl.Value
    Else If (ctl.Name = "AutoComplete")
        Settings["AutoComplete"] := ctl.Value
    Else If (ctl.Name = "HideNoParamFuncs")
        Settings["HideNoParamFuncs"] := ctl.Value
    If (ctl.Name = "PickFont") {
        fName := Settings["fontFace"]
        fSize := Settings["fontSize"]
        fontObj := Map("name",fName,"size",fSize) ; ,"color",0xFF0000
        fontObj := FontSelect(fontObj,ctl.gui.hwnd,0)
        If (fontObj) {
            Settings["fontFace"] := fontObj["name"]
            Settings["fontSize"] := fontObj["size"]
            SetFontDemo()
        }
    } Else If (ctl.Name = "PickFontColor") {
        fontColor := Settings["fontColor"]
        fontColor := ColorSelect(fontColor,ctl.gui.hwnd)
        If (fontColor > -1) {
            Settings["fontColor"] := fontColor
            SetFontDemo()
        }
    } Else If (ctl.Name = "PickBgColor") {
        bgColor := Settings["bgColor"]
        bgColor := ColorSelect(bgColor,ctl.gui.hwnd)
        If (bgColor > -1) {
            Settings["bgColor"] := bgColor
            SetFontDemo()
        }
    } Else If (ctl.Name = "SelBaseFile") {
        lastFile := Settings["BaseFile"], startDir := ""
        If (FileExist(lastFile))
            SplitPath lastFile, , startDir
        
        selFile := FileSelect("",startDir,"Select Base File:")
        ctl.gui["BaseFile"].value := selFile ? selFile : ""
    } Else If (ctl.Name = "ClearBaseFile")
        ctl.gui["BaseFile"].value := ""
}

SetFontDemo() {
    fontFace := Settings["fontFace"]
    fontSize := Settings["fontSize"]
    fontColor := Format("{:X}",Settings["fontColor"])
    bgColor := Format("{:X}",Settings["bgColor"])
    
    SettingsGUI["FontDemo"].SetFont("s" fontSize " c" fontColor,fontFace)
    SettingsGUI["FontDemo"].Opt("+Background" bgColor)
}

KillAllHotkeys() {
    CallTipInvoke := Settings["CallTipInvoke"]
    AutoCompleteInvoke := Settings["AutoCompleteInvoke"]
    ReloadInvoke := Settings["ReloadInvoke"]
    CloseInvoke := Settings["CloseInvoke"]
    
    HotIfWinActive "ahk_id " oCallTip.progHwnd
    If (CallTipInvoke)
        Hotkey CallTipInvoke, "Off"
    If (AutoCompleteInvoke)
        Hotkey AutoCompleteInvoke, "Off"
    If (ReloadInvoke)
        Hotkey ReloadInvoke, "Off"
    If (CloseInvoke)
        Hotkey CloseInvoke, "Off"
}

SetHotkeys() {
    CallTipInvoke := Settings["CallTipInvoke"]
    AutoCompleteInvoke := Settings["AutoCompleteInvoke"]
    ReloadInvoke := Settings["ReloadInvoke"]
    CloseInvoke := Settings["CloseInvoke"]
    
    ; DebugMsg("progHwnd: " oCallTip.progHwnd)
    
    WinActivate "ahk_id " oCallTip.progHwnd
    HotIfWinActive "ahk_id " oCallTip.progHwnd
    
    If (CallTipInvoke) {
        ; DebugMsg("CallTipInvoke - " CallTipInvoke)
        Hotkey CallTipInvoke, "DisplayCallTip"
        Hotkey CallTipInvoke, "On"
    }
    
    If (AutoCompleteInvoke) {
        ; DebugMsg("AutoCompleteInvoke - " AutoCompleteInvoke)
        Hotkey AutoCompleteInvoke, "AutoComp"
        Hotkey AutoCompleteInvoke, "On"
    }
    
    If (ReloadInvoke) {
        ; DebugMsg("ReloadInvoke - " ReloadInvoke)
        Hotkey ReloadInvoke, "ReloadGui"
        Hotkey ReloadInvoke, "On"
    }
    
    If (CloseInvoke) {
        ; DebugMsg("CloseInvoke - " CloseInvoke)
        Hotkey CloseInvoke, "CloseScript"
        Hotkey CloseInvoke, "on"
    }
    
}

gui_close(guiObj) {
    newLang := guiObj["ActiveLanguage"].Text
    newExe := guiObj["ProgExe"].value
    newClassNN := guiObj["ProgClassNN"].value
    newBaseFile := guiObj["BaseFile"].value
    
    doReload := false
    If (newLang != Settings["ActiveLanguage"] Or newExe != Settings["ProgExe"] Or newClassNN != Settings["ProgClassNN"] Or newBaseFile != Settings["BaseFile"])
        doReload := true
    
    Settings["ProgExe"] := guiObj["ProgExe"].value
    Settings["ProgClassNN"] := guiObj["ProgClassNN"].value
    Settings["BaseFile"] := guiObj["BaseFile"].value
    Settings["ActiveLanguage"] := guiObj["ActiveLanguage"].Text
    
    Settings["CallTipInvoke"] := guiObj["CallTipInvoke"].Value
    Settings["AutoCompleteInvoke"] := guiObj["AutoCompleteInvoke"].Value
    Settings["ReloadInvoke"] := guiObj["ReloadInvoke"].Value
    Settings["CloseInvoke"] := guiObj["CloseInvoke"].Value
    
    SetHotkeys()
    
    settingsText := Jxon_Dump(Settings,4)
    FileDelete "Settings.txt"
    FileAppend settingsText, "Settings.txt"
    SettingsGUI.Destroy(), SettingsGUI := ""
    
    If (doReload)
        SetTimer "FullReload", -1
}

; ================================================================
; Base File Quick Select
; ================================================================

QuickReloadGUI() {
    m := GetMonitorData()
    
    g := Gui.New("+OwnDialogs","Base File Quick Reload")
    g.OnEvent("close","quick_reload_close")
    g.OnEvent("escape","quick_reload_close")
    
    g.Add("Text","xm y8","Base File:")
    g.Add("Edit","vBaseFile x+2 yp-4 w400 ReadOnly r1",Settings["BaseFile"])
    g.Add("Button","vPickBaseFile x+0","...").OnEvent("click","quick_reload_gui")
    g.Add("Button","vClearBaseFile x+0","X").OnEvent("click","quick_reload_gui")
    g.Add("Button","vReload x+10 +Default","Reload").OnEvent("click","quick_reload_gui")
    
    g.Show("Hide")
    
    w := "", h := ""
    g.GetPos(,,w,h)
    showDims := "x" (m.Cx - (w/2)) " y" (m.Cy - (h/2))
    g.Show(showDims)
}

quick_reload_gui(ctl, info) {
    If (ctl.Name = "PickBaseFile") {
        newFile := FileSelect("",Settings["BaseFile"],"Select Base File:")
        If (newFile)
            ctl.gui["BaseFile"].value := newFile
    } Else If (ctl.Name = "ClearBaseFile")
        ctl.gui["BaseFile"].value := ""
    
    Settings["BaseFile"] := ctl.gui["BaseFile"].value
    
    settingsText := Jxon_Dump(Settings,4)
    FileDelete "Settings.txt"
    FileAppend settingsText, "Settings.txt"
    
    ctl.gui.Destroy()
    ReParseText() ; reload user defined elements
}

quick_reload_close(g) {
    g.Destroy()
}

; ================================================================
; Auto-Complete
; ================================================================
LoadAutoCompleteGUI(KeywordFilter) {
    wa := false ; win is visible?
    If (IsObject(AutoCompleteGUI) And WinExist("ahk_id " oCallTip.AutoCompleteHwnd))
        wa := true
    
    dispList := [], endList := [], curPhrase := oCallTip.curPhrase, kwBlock := ""
    For kw, kwType in KeywordFilter {
        addon := "", prefix := ""
        
        If (inStr(kwType,"Function"))
            addon := "(f)"
        Else If (InStr(kwType,"Class"))
            addon := "(c)"
        Else If (InStr(kwType,"Object"))
            addon := "(o)"
        Else If (inStr(kwType,"Method"))
            addon := "(m)"
        
        If (InStr(kwType,"property") Or InStr(kwType,"method"))
            prefix := "."
        
        kwBlock .= prefix kw addon "`r`n"
        
        If (!curPhrase Or InStr(kw,curPhrase) != 1)
            endList.Push(prefix kw addon)
        Else If (InStr(kw,curPhrase) = 1)
            dispList.Push(prefix kw addon)
    }
    
    fontFace := Settings["fontFace"]
    fontSize := Settings["fontSize"]
    kwDims := GetTextDims(kwBlock,fontFace,fontSize)
    
    w := kwDims.w + (kwDims.avgW * 4) ; add VScroll width
    maxR := (KeywordFilter.Count > 10) ? 10 : KeywordFilter.Count
    h := (kwDims.avgH * maxR) + 8
    
    CoordMode "Caret", "Screen"
    CaretGetPos(outX, outY)
    
    If (!wa) { ; auto-complete not visible
        AutoCompleteGUI := ""
        fontColor := Settings["fontColor"]
        bgColor := Settings["bgColor"]
        hEditorWin := oCallTip.progHwnd
        
        AutoCompleteGUI := Gui.New("-DPIScale -Border AlwaysOnTop +Owner" hEditorWin)
        oCallTip.AutoCompleteHwnd := AutoCompleteGUI.hwnd
        AutoCompleteGUI.BackColor := bgColor
        AutoCompleteGUI.SetFont("s" fontSize " c" fontColor,fontFace)
        
        ctl := AutoCompleteGUI.Add("ListBox","vKwList x0 y0 w" w " h" h " +Background" bgColor) ; " r" maxR
        ctl.Add(dispList), ctl.Add(endList)
        ctl.GetPos(,,,h)
    
        AutoCompleteGUI.Show("x" outX " y" (outY + kwDims.avgH) " w" w " h" h " hide")
        AutoCompleteGUI.Show("h" h " NA NoActivate")
    } Else {
        If (Type(outY) = "String") ; caret pos couldn't be retrieved
            return
        
        AutoCompleteGui.Move(outX,(outY + kwDims.avgH),w,h)
        
        ctl := AutoCompleteGui["KwList"]
        ctl.Opt("-Redraw")
        ctl.Move(,,w,h)
        ctl.Delete()
        ctl.Add(dispList), ctl.Add(endList)
        ctl.Opt("+Redraw")
    }
}

