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
		For objName, objobj in ObjectList { ; find element and correct case
			If (objName = curPhrase) {
				obj := objobj["types"]
				found := true
				Break
			}
		}
		
		If (!found)
			return
		
		fullDescArr := Map(), i := 1
		For objType, typeObj in obj {
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
			}
		}
		curObj := "", listObj := "", obj := ""
	} Else If (curPhraseType = "method" Or curPhraseType = "property") {
		found := false
		For objName, objobj in ObjectList { ; find element and correct case
			If (objName = parentObj) {
				obj := objobj["types"]
				found := true
				Break
			}
		}
		
		If (!found)
			return
		
		fullDescArr := Map(), i := 1
		For objType, typeObj in obj {
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
			parent := cObj["parent"]
			subClasses .= (cObj["parent"] = className) ? clName ", " : ""
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
		
		callTipGui := Gui.New("-Border AlwaysOnTop +Owner" oCallTip.progHwnd) ; Splitpath drive
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
	fullDescArrObj := oCallTip.fullDescArr[curIndex]
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
	
	If (callTipGui And closeTipOnClick) {
		callTipGui.Destroy()
		callTipGui := "", oCallTip.curIndex := "", oCallTip.fullDescArr := ""
	}
}

; ================================================================
; Settings GUI
; ================================================================

SettingsGUILoad() {
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
	SettingsGUI.Add("DropDownList","x+2 yp-4 w100 vPickLang Choose" choose,langList).OnEvent("change","gui_change_events")
	
	ctl := SettingsGUI.Add("Checkbox","xm y+8 vLoadCallTipOnClick","Load call tip on double-click") ; ListView
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["LoadCallTipOnClick"]
	
	ctl := SettingsGUI.Add("Checkbox","x+8 vCloseTipOnFocusChange Section","Close call tip on focus change")
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["CloseTipOnFocusChange"]
	
	ctl := SettingsGUI.Add("Checkbox","xm y+8 vDebugToolTip","Show Debug Tooltip")
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["DebugToolTip"]
	
	ctl := SettingsGUI.Add("Checkbox","xs yp vCallTipSelectable","Selectable call tips")
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["CallTipSelectable"]
	
	ctl := SettingsGUI.Add("Checkbox","xm y+8 vAutoComplete","Auto-Complete while typing")
	ctl.OnEvent("click","gui_change_events")
	ctl.Value := Settings["AutoComplete"]
	
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
	
	SetFontDemo()
	SettingsGUI.Show()
}

gui_change_events(ctl, Info) { ; GuiControlObject
	if (ctl.Name = "PickLang")
		Settings["ActiveLanguage"] := ctl.Text
	else if (ctl.Name = "LoadCallTipOnClick")
		Settings["LoadCallTipOnClick"] := ctl.Value
	else if (ctl.Name = "CloseTipOnFocusChange")
		Settings["CloseTipOnFocusChange"] := ctl.Value
	else if (ctl.Name = "DebugToolTip")
		Settings["DebugToolTip"] := ctl.Value
	else if (ctl.Name = "CallTipSelectable")
		Settings["CallTipSelectable"] := ctl.Value
	Else If (ctl.Name = "AutoComplete")
		Settings["AutoComplete"] := ctl.Value
	Else If (ctl.Name = "PickFont") {
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

gui_close(guiObj) {
	Settings["ProgExe"] := guiObj["ProgExe"].value
	Settings["ProgClassNN"] := guiObj["ProgClassNN"].value
	Settings["BaseFile"] := guiObj["BaseFile"].value
	
	settingsText := Jxon_Dump(Settings,4)
	FileDelete "Settings.txt"
	FileAppend settingsText, "Settings.txt"
	SettingsGUI.Destroy(), SettingsGUI := ""
	
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
	
	g.Show("Hide")
	
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
	
	ReParseText()
	ctl.gui.Destroy()
}

quick_reload_close(g) {
	g.Destroy()
}

; ================================================================
; Auto-Complete
; ================================================================
LoadAutoCompleteGUI(KeywordFilter) {
	If (IsObject(AutoCompleteGUI))
		AutoCompleteGUI.Destroy(), AutoCompleteGUI := ""
	
	dispList := [], endList := [], curPhrase := oCallTip.curPhrase, kwBlock := ""
	For kw, kwType in KeywordFilter {
		addon := "", prefix := ""
		If (inStr(kwType,"method"))
			addon := "(m)"
		Else If (inStr(kwType,"function"))
			addon := "(f)"
		
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
	fontColor := Settings["fontColor"]
	bgColor := Settings["bgColor"]
	hEditorWin := oCallTip.progHwnd
	
	kwDims := GetTextDims(kwBlock,fontFace,fontSize)
	
	w := kwDims.w + (kwDims.avgW * 4) ; add VScroll width
	maxR := (KeywordFilter.Count > 10) ? 10 : KeywordFilter.Count
	
	CoordMode "Caret", "Screen"
	CaretGetPos(outX, outY)
	
	AutoCompleteGUI := Gui.New("-Border AlwaysOnTop +Owner" hEditorWin)
	AutoCompleteGUI.BackColor := bgColor
	AutoCompleteGUI.SetFont("s" fontSize " c" fontColor,fontFace)
	
	If (AutoCompleteGUI)
		ctl := AutoCompleteGUI.Add("ListBox","vKwList x0 y0 w" w " r" maxR " +Background" bgColor)
	If (AutoCompleteGUI)
		ctl.Add(dispList), ctl.Add(endList)
	
	If (AutoCompleteGUI)
		ctl.GetPos(,,,h)
	
	If (AutoCompleteGUI) {
		AutoCompleteGUI.Show("x" outX " y" (outY + kwDims.avgH) " w" w " h" h " hide")
		AutoCompleteGUI.Show("h" h " NA NoActivate")
	}
}
