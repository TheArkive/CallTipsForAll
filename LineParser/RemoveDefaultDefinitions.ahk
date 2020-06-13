/*
Target of this function is to get the "real" parameters of a function definition.

Even when literal strings are removed from the line, splitting by comma is not an option, because arrays and objects can have comma in them.

Approach: robustly remove the default values of the function parameters.
*/

; Text = Byref   param1   :=    { 1:"Test" , "Key {2": "" }  ,   param2*  ,   Param3   :=   [ {"t,e[a]s:t{b}":  {1:"t,e[a]s:t{b}","t,e[a]s:t{b}": [4 ] }}]
Text = param1 := { 1:"Test" , "Key {2": "" } , Param3 := [ {"t,e[a]s:t{b}": {1:"t,e[a]s:t{b}","t,e[a]s:t{b}": [4 ] }}]
MsgBox % "Test RemoveDefaultDefinitions:`n`n>" Text "<`n`n>" RemoveDefaultDefinitions(Text) "<"
ExitApp

/*
;input 
>param1 := { 1:"Test" , "Key {2": "" } , Param3 := [ {"t,e[a]s:t{b}": {1:"t,e[a]s:t{b}","t,e[a]s:t{b}": [4 ] }}]<
;this is the current result
>param1 .. --------------------------- , Param3 .. [ {--------------: ----------------------------------------}]<
;this is what I would like to achieve
>param1 .. --------------------------- , Param3 .. -------------------------------------------------------------<
*/

RemoveDefaultDefinitions(Line){
  static chars := [{"open":Chr(34),"close":Chr(34)}        ; quote character
                  ; ,{"open":"{",    "close":"}"}
                  ,{"open":"\[",    "close":"]"}   
                  ,{"open":"\{",    "close":"}"}
                  ,{"open":":",    "close":"="}]   
  ;replace 
  ;  "quoted strings"
  ;  {objects}
  ;  [Arrays]
  ;  with dots and dashes to keep length of line constant, and that other character positions do not change
  
  CleanLine := Line
  For k,q in chars
  {
    ;Replace quoted literal strings             1) replace two consecutive quotes with dots
    ; CleanLine := StrReplace(CleanLine, q.open . q.close , "..")
    CleanLine := RemoveQuotedStrings(CleanLine)
    MsgBox %CleanLine%
    ; param1 := { 1:"Test" , "Key {2": .. } , Param3 := [ {"t,e[a]s:t{b}": {1:"t,e[a]s:t{b}","t,e[a]s:t{b}": [4 ] }}]
    ; param1 := { 1:------ , --------: .. } , Param3 := [ {--------------: {1:--------------,--------------: [4 ] }}]


    Pos := 1                                 ;  2) replace ungreedy strings in quotes with dashes
    Needle := q.open . "[^" q.open "]*?" . q.close
    ; Needle := "\s*?" q.open . "[^" q.open "]*?" . q.close
    ; Needle := ":=\s*?" q.open . ".*?" . q.close
    While Pos := RegExMatch(CleanLine, Needle, QuotedString, Pos){
      ; MsgBox % q.open "`n" CleanLine
      ReplaceString =
      Loop, % StrLen(QuotedString)
         ReplaceString .= "-"
      CleanLine := RegExReplace(CleanLine, Needle, ReplaceString, Count, 1, Pos)
    }
  }
  Return CleanLine
}
