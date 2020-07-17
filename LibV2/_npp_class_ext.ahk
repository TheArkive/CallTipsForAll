; AHK v2
; =================================================================================
; EXAMPLES
; =================================================================================
winHwnd := WinExist("ahk_exe notepad++.exe")
winPid := WinGetPid("ahk_id " winHwnd)

NppExt.pid := winPid
NppExt.hwnd := winHwnd
; =================================================================================
; ABOVE TEXT MUST BE UNCOMMENTED FOR BELOW EXAMPLES
; =================================================================================

#INCLUDE TheArkive_Debug.ahk

; result := NppExt.SendMsg("NPPM_GETFILENAME")
result := NppExt.SendMsg("NPPM_GETFULLCURRENTPATH")
; result := NppExt.SendMsg("NPPM_GETFULLPATHFROMBUFFERID")
msgbox result.str

; myBuf := BufferAlloc(260,0)
; dllRes := DllCall("SendMessage", "Ptr", this.hwnd, "UInt", curMsg, "UInt", bufSize, "Ptr", proc.bufAddr)




; ==========================================================================================================================
; based on Scintilla class made by kczx3: https://github.com/kczx3/Scintilla
; and based on scintilla wrapper functions provided by toralf: https://www.autohotkey.com/boards/viewtopic.php?p=336240#p336240
; ==========================================================================================================================
;
; This class aims to extract basic info from Notepad++ such as current file, list of open files, etc.
;
; ==========================================================================================================================
; === set these first ===
; ==========================================================================================================================
; NppExt.pid := pid
; NppExt.hwnd := progHwnd
;
; ==========================================================================================================================
; === then send call commands / messages ===
; ==========================================================================================================================
; returnObj := ScintillaExt.SendMsg("SCI_msg...", wParam := 0, lParam := 0)
;
;		returnObj := {dll:number, str:string}
;		- if output is a number from the SCI message, it's in the .dll member (returnObj.dll)
;		- if output is a string, it's in the .str member (returnObj.str)
;
; General usage cases:
;
;		1) wParam is an integer, and lParam is not used (set to zero by default).
;
;		2) wParam and lParam are both integers.  wParam may not be used, in which case it is zero.
;
;		3) wParam is an integer and lParam is a string.
;
;			a) wParam is usually a position, or buffer length, then lParam will be a string.
;
;			b) If you are "putting" a string into the scintilla control then pass it "quoted" directly into lParam.
;
;			c) If you are extracting a string value from the scintilla control, then pass a blank string to lParam.
;
; For specific usage cases that deviate from the scintilla help docs, see examples above.
; 
; ==========================================================================================================================
; ==========================================================================================================================
class NppExt {
	static df := 0, dp := 0, pid := 0, hwnd := 0
	
	Static WM_USER := 1024, NPPMSG := this.WM_USER + 1000, RUNCOMMAND_USER := this.WM_USER + 3000, MAIN_VIEW := 0, SUB_VIEW := 1, MAX_PATH := 260
	
	Static ALL_OPEN_FILES := 0, PRIMARY_VIEW := 1, SECOND_VIEW := 2, MODELESSDIALOGADD := 0, MODELESSDIALOGREMOVE := 1
	
	Static VAR_NOT_RECOGNIZED := 0, FULL_CURRENT_PATH := 1, CURRENT_DIRECTORY := 2, FILE_NAME := 3, NAME_PART := 4, EXT_PART := 5, CURRENT_WORD := 6, NPP_DIRECTORY := 7, CURRENT_LINE := 8, CURRENT_COLUMN := 9, NPP_FULL_FILE_PATH := 10, GETFILENAMEATCURSOR := 11
	
    Static NPPM_GETCURRENTSCINTILLA := this.NPPMSG + 4      , NPPM_GETCURRENTLANGTYPE := this.NPPMSG + 5        , NPPM_SETCURRENTLANGTYPE := this.NPPMSG + 6
         , NPPM_GETNBOPENFILES      := this.NPPMSG + 7      , NPPM_GETOPENFILENAMES   := this.NPPMSG + 8        , NPPM_GETNBSESSIONFILES  := this.NPPMSG + 13
         , NPPM_GETSESSIONFILES     := this.NPPMSG + 14     , NPPM_SAVESESSION        := this.NPPMSG + 15       , NPPM_SAVECURRENTSESSION := this.NPPMSG + 16
         , NPPM_GETOPENFILENAMESPRIMARY := this.NPPMSG + 17 , NPPM_GETOPENFILENAMESSECOND := this.NPPMSG + 18
         , NPPM_CREATESCINTILLAHANDLE := this.NPPMSG + 20   , NPPM_DESTROYSCINTILLAHANDLE := this.NPPMSG + 21   , NPPM_GETNBUSERLANG := this.NPPMSG + 22

    Static NPPM_GETCURRENTDOCINDEX  := this.NPPMSG + 23

    ; Static NPPM_SETSTATUSBAR := this.NPPMSG + 24, STATUSBAR_DOC_TYPE := 0, STATUSBAR_DOC_SIZE := 1, STATUSBAR_CUR_POS := 2, STATUSBAR_EOF_FORMAT := 3, STATUSBAR_UNICODE_TYPE := 4, STATUSBAR_TYPING_MODE := 5
	
	Static NPPM_GETMENUHANDLE := this.NPPMSG + 25, NPPPLUGINMENU := 0, NPPMAINMENU := 1
	
	Static NPPM_ENCODESCI := this.NPPMSG+26, NPPM_DECODESCI := this.NPPMSG+27, NPPM_ACTIVATEDOC := this.NPPMSG+28, NPPM_LAUNCHFINDINFILESDLG := this.NPPMSG+29, NPPM_DMMSHOW := this.NPPMSG+30
	
	Static NPPM_DMMHIDE := this.NPPMSG+31, NPPM_DMMUPDATEDISPINFO := this.NPPMSG+32, NPPM_DMMREGASDCKDLG := this.NPPMSG+33, NPPM_LOADSESSION := this.NPPMSG+34, NPPM_DMMVIEWOTHERTAB := this.NPPMSG+35, NPPM_RELOADFILE := this.NPPMSG+36, NPPM_SWITCHTOFILE := this.NPPMSG+37, NPPM_SAVECURRENTFILE := this.NPPMSG+38, NPPM_SAVEALLFILES := this.NPPMSG+39
	
	Static NPPM_SETMENUITEMCHECK := this.NPPMSG+40, NPPM_ADDTOOLBARICON := this.NPPMSG+41, NPPM_GETWINDOWSVERSION := this.NPPMSG+42, NPPM_DMMGETPLUGINHWNDBYNAME := this.NPPMSG+43, NPPM_MAKECURRENTBUFFERDIRTY := this.NPPMSG+44, NPPM_GETENABLETHEMETEXTUREFUNC := this.NPPMSG+45, NPPM_GETPLUGINSCONFIGDIR := this.NPPMSG+46, NPPM_MSGTOPLUGIN := this.NPPMSG+47, NPPM_MENUCOMMAND := this.NPPMSG+48, NPPM_TRIGGERTABBARCONTEXTMENU := this.NPPMSG+49, NPPM_GETNPPVERSION := this.NPPMSG+50
	
	Static NPPM_HIDETABBAR := this.NPPMSG+51, NPPM_ISTABBARHIDDEN := this.NPPMSG+52, NPPM_GETPOSFROMBUFFERID := this.NPPMSG+57, NPPM_GETFULLPATHFROMBUFFERID := this.NPPMSG+58, NPPM_GETBUFFERIDFROMPOS := this.NPPMSG+59, NPPM_GETCURRENTBUFFERID := this.NPPMSG+60, NPPM_RELOADBUFFERID := this.NPPMSG+61, NPPM_GETBUFFERLANGTYPE := this.NPPMSG+64, NPPM_SETBUFFERLANGTYPE := this.NPPMSG+65, NPPM_GETBUFFERENCODING := this.NPPMSG+66, NPPM_SETBUFFERENCODING := this.NPPMSG+67, NPPM_GETBUFFERFORMAT := this.NPPMSG+68, NPPM_SETBUFFERFORMAT := this.NPPMSG+69, NPPM_HIDETOOLBAR := this.NPPMSG+70, NPPM_ISTOOLBARHIDDEN := this.NPPMSG+71, NPPM_HIDEMENU := this.NPPMSG+72, NPPM_ISMENUHIDDEN := this.NPPMSG+73, NPPM_HIDESTATUSBAR := this.NPPMSG+74, NPPM_ISSTATUSBARHIDDEN := this.NPPMSG+75
	
	Static NPPM_GETSHORTCUTBYCMDID := this.NPPMSG+76, NPPM_DOOPEN := this.NPPMSG+77, NPPM_SAVECURRENTFILEAS := this.NPPMSG+78, NPPM_GETCURRENTNATIVELANGENCODING := this.NPPMSG+79, NPPM_ALLOCATESUPPORTED := this.NPPMSG+80, NPPM_ALLOCATECMDID := this.NPPMSG+81, NPPM_ALLOCATEMARKER := this.NPPMSG+82, NPPM_GETLANGUAGENAME := this.NPPMSG+83, NPPM_GETLANGUAGEDESC := this.NPPMSG+84, NPPM_SHOWDOCSWITCHER := this.NPPMSG+85, NPPM_ISDOCSWITCHERSHOWN := this.NPPMSG+86, NPPM_GETAPPDATAPLUGINSALLOWED := this.NPPMSG+87, NPPM_GETCURRENTVIEW := this.NPPMSG+88, NPPM_DOCSWITCHERDISABLECOLUMN := this.NPPMSG+89, NPPM_GETEDITORDEFAULTFOREGROUNDCOLOR := this.NPPMSG+90, NPPM_GETEDITORDEFAULTBACKGROUNDCOLOR := this.NPPMSG+91, NPPM_SETSMOOTHFONT := this.NPPMSG+92, NPPM_SETEDITORBORDEREDGE := this.NPPMSG+93, NPPM_SAVEFILE := this.NPPMSG+94, NPPM_DISABLEAUTOUPDATE := this.NPPMSG+95, NPPM_REMOVESHORTCUTBYCMDID := this.NPPMSG+96, NPPM_GETPLUGINHOMEPATH := this.NPPMSG+97
	
	Static NPPM_GETFULLCURRENTPATH := this.RUNCOMMAND_USER + this.FULL_CURRENT_PATH
	Static NPPM_GETCURRENTDIRECTORY := this.RUNCOMMAND_USER + this.CURRENT_DIRECTORY
	Static NPPM_GETFILENAME := this.RUNCOMMAND_USER + this.FILE_NAME
	Static NPPM_GETNAMEPART := this.RUNCOMMAND_USER + this.NAME_PART
	Static NPPM_GETEXTPART := this.RUNCOMMAND_USER + this.EXT_PART
	Static NPPM_GETCURRENTWORD := this.RUNCOMMAND_USER + this.CURRENT_WORD
	Static NPPM_GETNPPDIRECTORY := this.RUNCOMMAND_USER + this.NPP_DIRECTORY
	Static NPPM_GETFILENAMEATCURSOR := this.RUNCOMMAND_USER + this.GETFILENAMEATCURSOR
	
	Static NPPM_GETCURRENTLINE := this.RUNCOMMAND_USER + this.CURRENT_LINE
	Static NPPM_GETCURRENTCOLUMN := this.RUNCOMMAND_USER + this.CURRENT_COLUMN
	Static NPPM_GETNPPFULLFILEPATH := this.RUNCOMMAND_USER + this.NPP_FULL_FILE_PATH
	
	Static NPPN_FIRST := 1000 ; notifications
	Static NPPN_READY := 1001, NPPN_TBMODIFICATION := 1002, NPPN_FILEBEFORECLOSE := 1003, NPPN_FILEOPENED := 1004, NPPN_FILECLOSED := 1005, NPPN_FILEBEFOREOPEN := 1006, NPPN_FILEBEFORESAVE := 1007, NPPN_FILESAVED := 1008, NPPN_SHUTDOWN := 1009, NPPN_BUFFERACTIVATED := 1010, NPPN_LANGCHANGED := 1011, NPPN_WORDSTYLESUPDATED := 1012, NPPN_SHORTCUTREMAPPED := 1013, NPPN_FILEBEFORELOAD := 1014, NPPN_FILELOADFAILED := 1015, NPPN_READONLYCHANGED := 1016, NPPN_DOCORDERCHANGED := 1017, NPPN_SNAPSHOTDIRTYFILELOADED := 1018, NPPN_BEFORESHUTDOWN := 1019, NPPN_CANCELSHUTDOWN := 1020, NPPN_FILEBEFORERENAME := 1021, NPPN_FILERENAMECANCEL := 1022, NPPN_FILERENAMED := 1023, NPPN_FILEBEFOREDELETE := 1024, NPPN_FILEDELETEFAILED := 1025, NPPN_FILEDELETED := 1026
	
	Static SendMsg(msgName,wParam:=0,lParam:=0) {	; for read str:  make ext buffer, SendMsg, ReadProcMem
		winHwnd := this.hwnd									; for write str: make ext buffer, WriteProcMem, SendMsg
		curMsg := this.HasOwnProp(msgName) ? this.%msgName% : 0
		
		If (!curMsg)
			return {dll:0, str:""}
		Else If (msgName = "NPPM_GETFULLCURRENTPATH") {
			result := this.ReadProc(this.MAX_PATH*2, curMsg)
			return result
		} Else If (msgName = "NPPM_GETFILENAME") {
			result := this.ReadProc(this.MAX_PATH*2, curMsg)
			return result
		} Else If (msgName = "NPPM_GETOPENFILENAMES") { ; not implemented yet
			return {dll:0, str:""}
		} Else If (msgName = "NPPM_GETFULLPATHFROMBUFFERID") { ; not implemented yet
			; bufSize := DllCall("SendMessage", "Ptr", this.hwnd, "UInt", curMsg , "Int", wParam, "Int", lParam)
			; Debug.Msg("bufSize: " bufSize)
			return {dll:0, str:""}
		} Else
			return this.EasyMsg(curMsg, wParam, lParam)
	}
	
	Static EasyMsg(curMsg, wParam:=0, lParam:=0) {
		r := DllCall("SendMessage", "Ptr", this.hwnd, "UInt", curMsg , "Int", wParam, "Int", lParam)
		return {dll:r, str:""}
	}
	
	Static OpenProc(bufSize) {
		hProc := DllCall("OpenProcess", "UInt", 0x438, "Int", False, "UInt", this.pid, "Ptr") ; get proccess handle
		If (!hProc)
			return {bufAddr:0, hProc:0}
		
		bufAddr := DllCall("VirtualAllocEx", "Ptr", hProc, "Ptr", 0, "UInt", bufSize, "UInt", 0x1000, "UInt", 4, "Ptr") ; allocate memory in process
		If (!bufAddr)
			return {bufAddr:0, hProc:0}
		
		return {bufAddr:bufAddr, hProc:hProc}
	}
	
	Static WriteProc(curMsg, wParam:="", bufText:="") { ; lParam is bufText
		bufSize := StrPut(bufText)
		extBuf := BufferAlloc(bufSize,0) ; create external buffer for read/write into scintilla control
		if (bufText != "")
			StrPut(bufText, extBuf.ptr, "UTF-16")
		proc := this.OpenProc(bufSize) ; VarSetCapacity(written,4,0)
		
		wParam := (wParam = "") ? 0 : wParam
		
		r := DllCall("kernel32\WriteProcessMemory", "Ptr", proc.hProc, "Ptr", proc.bufAddr, "Ptr", extBuf.ptr, "UInt", bufSize, "Ptr", 0)
		
		extBuf := ""
		If (!r)
			return {dll:0, str:""}
		
		dllRes := DllCall("SendMessage", "Ptr", this.hwnd, "UInt", curMsg, "UInt", wParam, "Ptr", proc.bufAddr)
		this.CloseProc(proc.hProc, proc.bufAddr)
		
		return {dll:dllRes, str:""} ; this is a test
	}
	
	Static ReadProc(bufSize, curMsg, wParam:="", lParam:="") {
		extBuf := BufferAlloc(bufSize,0) ; create external buffer for read/write into scintilla control
		debug.msg("bufSize: " extBuf.Size)
		proc := this.OpenProc(bufSize)
		
		If (wParam != "")
			dllRes := DllCall("SendMessage", "Ptr", this.hwnd, "UInt", curMsg, "UInt", wParam, "Ptr", proc.bufAddr)
		Else
			dllRes := DllCall("SendMessage", "Ptr", this.hwnd, "UInt", curMsg, "UInt", bufSize, "Ptr", proc.bufAddr)
		
		r := DllCall("ReadProcessMemory", "Ptr", proc.hProc, "Ptr", proc.bufAddr, "Ptr", extBuf.ptr, "UInt", bufSize, "Ptr", 0)
		If (!r)
			return {dll:0, str:""}
		
		retVal := StrGet(extBuf,"UTF-16")
		
		extBuf := ""
		this.CloseProc(proc.hProc, proc.bufAddr)
		
		return {dll:dllRes, str:retVal}
	}
	
	Static CloseProc(hProc, bufAddr) {
		DllCall("VirtualFreeEx", "Ptr", hProc, "Ptr", bufAddr, "UPtr", 0, "UInt", 0x8000) ; MEM_RELEASE
		DllCall("CloseHandle", "Ptr", hProc)
	}
}
; asdf