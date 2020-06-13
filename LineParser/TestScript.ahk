Return    ;comment this line for testing
; with the above Return active it is the end of the auto-Exec Section.


a := 1,               b := "te,st", c := 1.2, d := "", e:=Mod(10,(100-10)*10), f:={h:[a,a,b,b],i:[c,d]}, g:=[e,e,e],h:=RTrim(Chr(34) "-" i := Mod(ceil(a),round(c, 0)), OmitChar := "1"), t := s ? u : w
; Msgbox %a%
    a  = 1,               b := "te,st", c := 1.2, d := "", e:=Mod(10,(100-10)*10), f:={h:[a,a,b,b],i:[c,d]}, g:=[e,e,e],h:=RTrim(Chr(34) "-" i := Mod(ceil(a),round(c, 0)), OmitChar := "1"), t := s ? u : w
; Msgbox %a%
(a = 1) ? a:=2 : a:=4, b := "te,st", c := 1.2, d := "", e:=Mod(10,(100-10)*10), f:={h:[a,a,b,b],i:[c,d]}, g:=[e,e,e],h:=RTrim(Chr(34) "-" i := Mod(ceil(a),round(c, 0)), OmitChar := "1"), t := s ? u : w
; Msgbox %a%
;       123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
;                1         2         3         4         5         6         7         8         9         0         1         2         3         4         5         6         7         8
Line1 = a := 1, b := "te,st", c := 1.2, d := "", e:=Mod(10,(100-10)*10), f:={h:[a,a,b,b],i:[c,d]}, g:=[e,e,e],h:=RTrim(Chr(34) "-" i := Mod(ceil(a),round(c, 0)), OmitChar := "1"), t := s ? u : w
Line2 = a  = 1, b := "te,st", c := 1.2, d := "", e:=Mod(10,(100-10)*10), f:={h:[a,a,b,b],i:[c,d]}, g:=[e,e,e],h:=RTrim(Chr(34) "-" i := Mod(ceil(a),round(c, 0)), OmitChar := "1"), t := s ? u : w
Line3 = (a = 1) ? a:=2 : a:=4, b := "te,st", c := 1.2, d := "", e:=Mod(10,(100-10)*10), f:={h:[a,a,b,b],i:[c,d]}, g:=[e,e,e],h:=RTrim(Chr(34) "-" i := Mod(ceil(a),round(c, 0)), OmitChar := "1"), t := s ? u : w
ot(testfunc(Line1))
ot(testfunc(Line2))
ot(testfunc(Line3))
MsgBox wait testfunc(Line)
ExitApp

testfunc(Line){
  static VarLegacyAssignRE := "
               ( Join LTrim Comment
                    OS)(*UCP)                 ;Study and Unicode (for \s and \w)
                    ^\s*                      ;optionally whitespace at start of lien
                    (?P<VarName>[\w#$@]+)     ;a variable name
                    \s*                       ;optionally whitespace
                    =                         ;legacy assignment operator
              )"
         VarExprAssignRE := "
               ( Join LTrim Comment
                    OS)(*UCP)                 ;Study and Unicode (for \s and \w)
                    (?P<VarName>[\w#$@]+)     ;a variable name
                    \s*                       ;optionally whitespace
                    :=                        ;expression assignment operator
              )"
  
  If RegExMatch(Line, VarLegacyAssignRE, Match)
    Return [{Name: Match.VarName, Position: Match.Pos, Type: "Legacy"}]
  CleanLine := RemoveQuotedStrings(Line)
  Pos := 1
  Vars := []
  While (Pos := RegExMatch(CleanLine, VarExprAssignRE, Match, Pos)) {
    Vars.push( {Name: Match.VarName, Position: Match.Pos, Type: "Expression"} )
    Pos := Pos + Match.Len
  }
  Return Vars
}

/*

  If (InStr(CleanLine, ":=") And InStr(CleanLine, ",")){
    rb := cb := sb := 0
    Loop Parse, CleanLine
    {
      Switch A_LoopField 
      {
        Case "(": cb++
        Case ")": cb--
        Case "{": rb++
        Case "}": rb--
        Case "[": sb++
        Case "]": sb--
        Case ":": colnum := A_Index
        Case "=": 
        Case ",":
      }
    }
  }


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
*/
