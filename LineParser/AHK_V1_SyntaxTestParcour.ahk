
global SuperGlobalVar := [1,2]
     , SecondSuperGlobalVar := "test;test2"

#Include *i   Dummy.file  ;comment

Hotkey, Backspace, test

;>>> Test Area for code outside of functions or labels -----------------------------------------------------------------
MsgBox, 4, Test,  ; Comment.
; Comment.
( Join(:)) LTrim  ;Comment.                ;currently doesn't work with 'Join:' (is reported as bug now)
     ; This is not a comment; it is literal. Include the word Comments in the line above to make it a comment.
#Include   Dummy.file  
     No___FuncDefInContinuation(){
        This is not a function definition because it is in a continuation section
     }
), 1   ; Comment.
Return

#Include *i   Dummy.file  ;comment

;>>> PSPad functions to populate its code explorer ---------------------------------------------------------------------
;the following three functions try to mimik PSPad functions, but the real code or function is unknown
AddBaseNode(Tree, Caption, Image) {
   Return TV_Add(Caption, 0, "Sort")
}ParseAndAddNode(Tree, ParentID, Image, Line, Needle, Replacement, LineNumber)  {
   If RegExMatch(Line, Needle)
      Return TV_Add(RegExReplace(Line, Needle, Replacement, , 1) " (Line " LineNumber ")", ParentID)
   Return 0
}SortOrDeleteNode(TreeNode, Delete) {
   If !TV_GetChild(TreeNode) && (Delete)
      TV_Delete(TreeNode)
}

;>>> Internal Helper Functions -----------------------------------------------------------------------------------------

_D(Title := "Debug", Vars*){      ;small helper to Debug
  local Var,Value,Text := ""
  For Var,Value in Vars           ;call it like this
    Text .= Var ": " Value "`n"   ;>>> _D("end of line", {i:i, Line:Line, FuncBlockLevel:FuncBlockLevel}*)
  MsgBox, 0, %Title%, %Text%
}

;>>> Gui ---------------------------------------------------------------------------------------------------------------
BuildGui:
  Gui, Add, TreeView, w400 r50 hwndHTV
  FillTreeView(A_ScriptFullPath)
  Gui, Show, , ParseAHK - Code Explorer - Drop your AHK files here
  #Include *i   Dummy.file  ;comment
Return
Test:
Esc::
GuiClose:
ExitApp
GuiDropFiles:       ;only take first file
  Loop, parse, A_GuiEvent, `n
  {   FillTreeView(A_LoopField)
      Break
  }
Return
FillTreeView(File){
  global HTV
       , OnlyGlobalVarForTest := {1:"test","te,st":","}
  GuiControl, -Redraw, %HTV%
  TV_Delete()
  FileRead, Script, %File%
  Lines := StrSplit(Script, "`n", "`r")
  ParseAHK(Lines, HTV, DocComment := ">>>")
  GuiControl, +Redraw, %HTV%
  TV_Modify(TV_GetNext(), "VisFirst")
  #Include *i   Dummy.file  ;comment
}

;>>> Test Section for different Syntax =================================================================================

;comments
/*
  No___FuncDefInBlockComment(){
    global NoGlobalVar
         , NoSecondGlobalVar
  }
  #Include  Dummy.file 
*/ FuncDefOnLastLineOfBlockComment(){
}

;hotstrings
; // Find the hotstring's final double-colon by considering escape sequences from left to right.
; // This is necessary for to handles cases such as the following:
; // ::abc```::::Replacement String
; // The above hotstring translates literally into "abc`::".
::abc```::::Replacement String
  ::afk::away from keyboard
::btw::by the way

;labels
MySub:
   MyFuncA()
Return

(:                    ;>>> if this label is after the '(' hotkey, AHK throws error  ==> Duplicate label.
  MsgBox, This is a label %A_ThisLabel%
Return

F1:: MsgBox, "Help me"

;hotkeys
(::
  MsgBox, This is a Hotkey %A_ThisHotkey%
  GoSub, (
  If (alt = 1) {
    GoSub, LabelAroundFuncDef
  }Else{

  }
Return

;hotstring & DLLCall
::(::
  MsgBox, This is a HotString %A_ThisLabel% or %A_ThisHotkey%
  Result := Trim(DllCall("DllFile\Function"
                             , Type1, Arg1
                             + text, Type2
                             := (x = 1 )
                             ? "test"
                             : "case" , Arg2, "Cdecl ReturnType")
                             . "TrimExtension not DllCall")
Return

;function with labels
;Q: should the function def be nested underneath the label? similar to lables within function bodies
;   a non empty line (D := 2) is required between label and function def, otherwise error ==> A label must not point to a function.
;   because of that and the items below I do not think it should be nested
;      a) it is too rare and doesn't have any benefit (I see).
;      b) labels within functions are privat but func defs inside labels are public
;      c) goto/gosub label will not trigger the function unless the function is also called
LabelAroundFuncDef:
  D := 2
  MyFuncA() {
     MyFuncB(A, B)
     LabelInsideFunction:
     MsgBox, %A% - %B%
  }
Return

;function with multiline and label
MyFuncB(ByRef A
       , ByRef B)
;Test for comment between func def and {
{ LabelWithoutBrace:
   A := B := 10
   HotKey,#c,MySub
   HotKey !c,MySub
}

;class
Class MyClass
{
   InstanceVar := 5 + 1
   static ClassVar := 10 + 2

   Class SubClass {
      SubClassMethod() {
      }
   }
   MyClassMethod1() {
   }
   Prop {
   }
   MyCLassMethod2() {
   }MyCLassMethod3() {             ;>>> currently not allowed if previous block is a property
   }Prop2[]
   {
   }
   MyCLassMethod6() {
   }Class SubClass2 {              ;>>> currently not allowed if previous block is a property
      SubClassMethod2() {
      }
   }
   MyClassMethod4(){
   }
   get   {
   }
   set {
   }
}Class MyClass2
{  Prop2 [ a][  b ] [  c ]{
}  }

;class extended with OTB
class ExtendClass Extends MyClass {
   ExtendClassMethod(A
                   , B)
   {
   }
}

;function after class with multiline and OTB
MyMultilineFunc(ByRef A
              , ByRef B){
   A := B := 10
}

;unicode function name
PöhseFunkßion()
{
}

SubTezt:
  if yy
  { testinnerbrace:    ;>>> do not show currently, because {} are only processed currently inside class and function definitions

  } testafterbrace:    ;>>> do not show currently, because {} are only processed currently inside class and function definitions
return

Functest(){

} testAfter:           ;>>> this shows up, because the } gets removed (it belongs to the function)
    MsgBox, {
return