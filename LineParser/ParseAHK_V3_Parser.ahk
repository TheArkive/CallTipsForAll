;>>> Parse AHK to detect ...
;       definitions of classes (including nested), methods, properties, functions, labels, hotkeys, hotstrings
;       function parameters
;       variables that get assigned something
;       lines on which variables are defined as global
;       lines on which DllCalls are used
;       comments that start with a DocComment string
;       files to include
; ================================================================================================
/*
Known Issues
- Return statements in multi-line statements are not recognized. (most likely the last of the statements, but AHK doesn't care)
- HotStrings: escaped characters are not escaped in code explorer
- DllCall: takes string up to the last ) in the line, not necessary the correct one.         
- Vars :   global variables in multi-line statements are not split due to comma in objects or arrays
           
Potential Enhancements 
- detect Return in oneline statements
  
- detect scope of variables|methods|properties (super-global, global, local, static)

- detect variable names in commands, e.g. on Gui,Add the hwndVar/gVar/vVar etc
 - brute force: scan for all text with length > 3 and remove keywords
  
- detect 'correct' indentation level (curly braces block do work in function bodies, but oneliner after Loop/For/If etc are missing or outside of function bodies)
  to later be able to define different styles not only indentation level needs to be known but also if the line starts with an opening or closing brace

- detect 'documentation' e.g. of functions in comments (needs special keywords or format, similar to doxygen etc.) for call tips

- detect if a variable points to an object (associative/linear array, class, file, COM, function and inputhook) to inherit it's methods and properties

- refactor function (currently ~580 lines)
  - reduce comments to minimum or store them somewhere else
  - try to oursource code in new functions
  
- add push() method to LineInfo parallel to set()

- scan include files directly where they are included in the code, to have the lines correctly WithinName (which will be rare, but correct)
  then FileName or FileContent should be the incoming parameter to ParseAHK() with detection on which is provided.
  
- callMAP or dependencyMap: 
  detect within label/function which label or function is called/gosub/goto 
  later a map could be created from it (currently I do not know how to find roots except for AutoExec, but maybe with brute force to count how often a label/function was called, and then the roots are without a direct call e.g. hotkeys, hotstrings, autoexec)
  if it is the identical name it is a recursive label/function.
 
*/

ParseAHK(FileContent, SearchRE := "", DocComment := "") {
  ; internal vars with default values
  ; local enforces assume global; this is usually not needed, since all variables should be local anyway. But this is how I can check with ListVars in main script which variables have not been initialized. In the final script local could be removed
  local InCommentSection := False             ; true if within a comment section '/* ... */'
      , ContinuationBuffer := ""              ; buffer to collect continuation lines
      , ContinuationBufferLineNum := 0        ; buffer for first line number of continuation lines
      , PureCode := []

  ;>>> define RegEx Needles
      , DocCommRE :="
              ( Join LTrim Comment       ; DocComment allows comments to show up in Code Explorer as Notes
                    OiS)(*UCP)                 ;case insensitive (for the DocComment string), Study and Unicode (for \s and DocComment)
                    (^|.*\s);                  ;a ';' either at start of line or with some code and a whitespace in front, thus it is an AHK comment
                    \s*                        ;optional whitespace
                    \Q" DocComment "\E         ;the literal DocComment string
                    \s*                        ;optional whitespace
                    (.*)                       ;$2 the documentation string
              )"
      , ContinuationBlock2RE :="
              ( Comment LTrim Join         ; for Continuation Block Method 2
                    OiS)(*UCP)                 ;case insensitive (for the option texts), Study and Unicode (for \s and JoinString)
                    ^\(                        ;a '(' at start of line
                    (
                        \s*                         ;optional white space(s)
                        (
                          (Join)                       ;the option Join (Match3)
                          (\S{0,15})                   ;and an optional joinstring (up to 15 characters long) (Match4)
                          |(LTrim)                     ;or the option LTrim (Match5)
                          |(L|R)Trim0                  ;or the option LTrim0 and RTrim0 (Match6)
                          |(C\S*)                      ;or the option Comments (a string that starts with a 'C') (Match7)
                          |(Q\S*)                      ;or the option Quotes (for AHK v2; a string that starts with a 'Q')
                          |(`%)                        ;or the option %
                          |(,)                         ;or the option ,
                          |(``))                       ;or the option `
                    `)*                        ;the above items are optional and can exits multiple times ( the brace needs to be backticked to not close the continuation section)
                    (?!                        ;and the following items are not to exist to the right
                        .*?                        ;any optional text (ungreedy)
                        (
                          \)                          ;and a ')'
                          |:$))                       ;or a ':' at the end of line
              )"
      , ContinuationOperatorsRE :="
              (LTrim Join Comment
                    iS)(*UCP)                 ;case insensitive (for 'and' and 'or'), Study and Unicode (for \w)
                    ^(                        ;at the start of line
                       (and|or)[^\w#@$]|      ;either "And" or "Or" followed by non word character
                       &|,|-(?!-)|!|~|/|<|>|=|:|    ;or operators that do not need to be escaped, but no --
                       \\|\.|\*|\?|\+(?!\+)|\||\^)  ;or these operators (that must be preceded by a backslash to be seen as literal), but no ++
              )"
      , LabelRE :="
              ( Join LTrim Comment
                    OS)(*UCP)                 ;Study and Unicode (for \s)
                    ^([^\s,`:]+)              ;$1 some characters at the start of the line (but no whitespace, comma, backtick or colon)
                    :$                        ;a ':' at the end of line
              )"
      , HotStringRE :="
              ( Comment Join LTrim
                    OS)                        ;Study
                    ^:                         ;a ':' at start of line
                    .*?                        ;options (ungreedy)
                    :                          ;a ':'
                    (.+)                       ;$1 the hotstring
                    ::                         ;two ':'s
                    (.*)                       ;$2 rest of line
                    $                          ;end of line
              )"
      , HotKeyRE :="
              ( LTrim Join Comment
                    OS)                        ;Study
                    ^                          ;at start of line
                    (.+)                       ;$1 the hotkey at least one character (but including whitespace)
                    ::                         ;two ':'s
                    (.*)                       ;$2 rest of line
                    $                          ;end of line
              )"

      ;local variable without initialization
      , ContiBlock2Settings, AllowComments, AllowTrimLeft, AllowTrimRight, JoinString
      , Lines, FuncName, Match, i, Line, LineNoCo, LineNoLi, TotalNumberOfLine, LineOrig, Params, PhysicalLineNum, Type
      , Block, Vars, Data

  ;>>> Begin to parse FileContent line by line to find the pure code
  ; search for SearchRE and DocComment
  ; remove semicolon comments  ( ;) and commentSections (/*..*/)
  ; concatenate Continuation Lines Method 1 and Method 2
  ; remove empty lines and indentation
  ; store pure code
  Lines := StrSplit(FileContent, "`n", "`r")
  TotalNumberOfLine := Lines.MaxIndex()
  For PhysicalLineNum, LineOrig In Lines {
    LineInfo.Line(PhysicalLineNum, { _LineOrig: LineOrig                                            ;the original line 
                                   , _LineTrim: Line := Trim(LineOrig)                              ;remove leading/trailing whitespaces
                                   , _LineNoCo: LineNoCo := LineInfo.Comment(PhysicalLineNum, Line) ;line without comment
                                   , _LineNoLi: LineNoLi := RemoveQuotedStrings(LineNoCo) })        ;line without literal strings
    
    ;search for SearchRE
    If RegExMatch(Line, SearchRE)
      LineInfo.Search(PhysicalLineNum, SearchRE, {Line: Line})

    If (DocComment <> "")     ;extract DocComment regardless of where it is (even inside a comment or comment section)
      If RegExMatch(Line, DocCommRE, Match)
        LineInfo.DocComment(PhysicalLineNum, DocComment, {Line: Match.2})

    ;when InContinuationBlock2 empty lines matter and maybe even comments and block comments
    ;hence, this has to be done before comments are stripped off and empty lines are skipped
    If isObject(ContiBlock2Settings) {
      If (SubStr(LineNoCo, 1, 1) = ")"){             ;it's the end of the continuation section
        ContiBlock2Settings := ""
        LineInfo.PopWithin(PhysicalLineNum)
        LineNoCo := SubStr(LineNoCo, 2)                  ;remove ) from line
      }
      
      LineInfo.Line(PhysicalLineNum, ContiBlock2Settings)

      ;when still in continuation section trim or strip the original line
      If isObject(ContiBlock2Settings) {
        If (Line and ContiBlock2Settings.AllowComments)    ;check if comments are allowed literally, if yes, remove semicolon comments when line is not already empty
          If ! (LineOrig := RemoveComments(LineOrig))        ;can not use LineNoCo, because it doesn't have original indentation (LineNoCo is trimmed)
            Continue                                           ;when line is now empty, skip it
        If ContiBlock2Settings.AllowTrimLeft
          LineOrig := LTrim(LineOrig)
        If ContiBlock2Settings.AllowTrimRight
          LineOrig := RTrim(LineOrig)
        ;in continuation section concatenate the line with the JoinString,
        ContinuationBuffer .= ContiBlock2Settings.JoinString . LineOrig
      } Else
        ;otherwise the code after the ) will be concatenated without any string
        ContinuationBuffer .= LineNoCo       ;line is stripped and trimmed before the ) got removed

      Continue                                   ;go to next line
    }

    ;>>> Remove all comments and skip empty lines ----------------------------------------------------------------------
    ;Skip comment section
    ;the /* and */ symbols comment out an entire section, but only if the symbols appear at the beginning of a line
    ;code after the */ is not part of the comment section
    If (InCommentSection) {
      LineInfo.Line(PhysicalLineNum, { CommentSection: InCommentSection })
      If (SubStr(Line, 1, 2) = "*/"){
        InCommentSection := False
        Line := Trim(SubStr(Line, 3))   ;remove the /* from the beginning of the line and continue checking
        LineInfo.PopWithin( PhysicalLineNum)
        LineInfo.Line(PhysicalLineNum, { CommentSection: InCommentSection, LineAfter: Line })
      }Else
        Continue                        ;discard this line, it is in a Comment Section
    }Else If (! isObject(ContiBlock2Settings) AND SubStr(Line, 1, 2) = "/*") {
      InCommentSection := True
      LineInfo.CommentSection(PhysicalLineNum, "/*", { CommentSection: InCommentSection } )
      Continue
    }

   ;Remove any comment and skip empty lines (If not the last line)
    If ((!Line := RemoveComments(Line)) AND PhysicalLineNum <> TotalNumberOfLine){
      LineInfo.Line(PhysicalLineNum, { BlankOrComment: True })
      Continue
    }


    ;>>> Collect continuation lines ==================================================================================

    ;>>> Check for Labels,HotStrings,HotKeys And/Or Start collecting continuation lines
    ;Label names are not case sensitive, and may consist of any characters other than space, tab, comma and the escape character (`). No other code can be written on the same line.
    ;they are allowed inside of functions definitions, thus they are detected here but will be used later (detected again)
    If (RegExMatch(Line, LabelRE) Or RegExMatch(Line, HotStringRE) Or RegExMatch(Line, HotKeyRE) Or ContinuationBuffer = "") {
      ;this is not a continuation line. It is a label or the first line to be put into the buffer.

      ;Labels checked here to avoid false positives in checks below
      ;Since labels are allowed within a function body, they might be preceded with a brace,
      ;e.g. when a label is the first line after a multi-line function definition without OTB
;       FuncDef()
;       { Label:    ;this label would not be caught correctly here
;
;       }
      ;or when it is following immediately a {} block  (within a function body or outside)
;       If(  )
;       {
;
;       } Label:    ;this label would not be caught correctly here

      ;thus do nothing just now, it will be caught later
      ;within functions first the {} blocks have to be analyzed and the braces trimmed off,
      ;then the line has to be scanned again, but re-scanning can not start before the swap of lines
      ; (due to check for continuation) since this might screw up the order of lines
      ;thus, everything that is relevant within the function body has to be processed after the swap of lines

      ;in case there is no line in buffer, get next one
      ;this could happen when a label was detected very early, e.g. on the very first line in file.
      If (ContinuationBuffer = ""){
        ContinuationBuffer := Line
        ContinuationBufferLineNum := PhysicalLineNum
        If (PhysicalLineNum <> TotalNumberOfLine)
          Continue                    ;go to next line, but not in case of last line
      }

    ;>>> Collect continuation section Method 2
    ;used to merge a large number of lines; can also be used with any command or expression (assignment)
    ;in most cases there shouldn't be any valuable info for the code explorer on any of these lines (including any code on last line after ")")
    ;but these lines may contain var assignments or class or function calls
    }Else If (RegExMatch(Line, ContinuationBlock2RE, Match)) {  ;it's the start of the continuation section when it starts with (
                                                                ;but doesn't have a ), exception is after Join; it could be an expressions like (x.y)[z]()
                                                                ;and doesn't have a : at it's start or end, exception is after Join; it could be a label, hotkey or hotstring
      JoinString := Match.Value(3) ? "" : "`n"                        ;JoinString is by default `n, when Join is present it is 'no space'
      JoinString := Match.Value(4) ? Match.Value(4) : JoinString      ;when a string is given right after Join, it is used instead
      AllowTrimLeft  :=  Match.Value(5) ? True : False                ;with LTrim all spaces and tabs at the beginning of each line are omitted
      AllowTrimRight := !Match.Value(6) ? True : False                ;with RTrim0 omission of spaces and tabs from the end of each line is turned off
      AllowComments  :=  Match.Value(7) ? True : False                ;a string starting with C allows semicolon comments inside the continuation section (they are removed from the code) but not /*..*/ (these lines stay in the code)
      
      ContiBlock2Settings := {ContiBlock2: True
                            , AllowTrimLeft: AllowTrimLeft
                            , AllowTrimRight: AllowTrimRight
                            , AllowComments: AllowComments
                            , JoinString: JoinString }
      
      LineInfo.ContiBlock2(PhysicalLineNum, "(", ContiBlock2Settings )
      Continue                   ;go to next line     ;other parameters are ignored, because they currently do not matter for code explorer, e.g. % or , or ` or )

    ;>>> Collect continuation lines Method 1
    ;A line that starts with the following operators is automatically merged with the line directly above it
    ;operators: "and", "or", ||, &&, a comma or a period and all other expression operators except ++ and --
    ;AHK ignores also ::, but since hotstrings are caught above I see no need in this script
    }Else If (RegExMatch(Line, ContinuationOperatorsRE)){
      ContinuationBuffer .= " " Line   ;merge lines with a space
      LineInfo.Line( PhysicalLineNum, {ContiBlock1: True, Line: Line} )
      If (PhysicalLineNum <> TotalNumberOfLine)
        Continue                       ;go to next line, but not in case of last line
    }
    
    ;>>> Store buffered line and buffer current line
    PureCode.push( { Line: ContinuationBufferLineNum, Code: ContinuationBuffer } )
    ContinuationBuffer := Line  
    ContinuationBufferLineNum := PhysicalLineNum
  }

    local HotKeyCommandRE :="
              ( LTrim Join Comment
                    OiS)(*UCP)                 ;case insensitive (for the Hotkey texts), Study and Unicode (for \s)
                    ^Hotkey                    ;the text 'Hotkey' at start of line
                    (\s*,\s*|\s+)              ;a comma or space
                    (?!If)                     ;not the text 'If'
                    (.+?)                      ;$2 the hotkey
                    \s*                        ;spaces
                    ,                          ;a comma
                    .*                         ;rest of line
                    $                          ;end of line
              )"
      , DllCallRE :="
              ( Join LTrim Comment
                    Oi)                       ;case insensitive (for 'DllCall')
                    .*                        ;some code
                    (                         ;$1
                       DllCall                ;the text 'DllCall'
                       \(                     ;a '('
                       .*                     ;some code         ;            if i would make this ungreedy the first ) would be taken, not necessary the right one.
                       \))                    ;closing bracket   ;>>> to fix: takes the last ) in the line, not necessary the right one.
              )"
      , ClassRE :="
              ( Join LTrim Comment
                    OiS)(*UCP)                ;case insensitive (for \s, \w and 'Class'), Study and Unicode (for \s and \w)
                    ^Class                    ;the text 'Class' at the start of line
                    \s+                       ;at least one whitespace
                    ([\w#$@]+)                ;$1 one or more characters (A-Za-z0-9_) or #, $, @  (all allowed characters in variable names)
                    .*                        ;rest of the line
              )"
      , FunctionRE :="
              ( Join LTrim Comment            ;$1 the whole line will be a match
                    OS)(*UCP)                 ;Study and Unicode (for \w)
                    ^                         ;at the start of line
                    ([\w$#@]+)                ;$1 one or more characters (A-Za-z0-9_) or #, $, @  (all allowed characters in variable names)
                    \(                        ;a '('
              )"
      , PropertyRE :="
              ( Join LTrim Comment            ;$1 the whole line will be a match
                    OS)(*UCP)                 ;Study and Unicode (for \w)
                    ^                         ;at the start of line
                    ([\w$#@]+)                ;$1 one or more characters (A-Za-z0-9_) or #, $, @  (all allowed characters in variable names)
                    (\[.*\])?                 ;$2 optional parameters
                    (\s*{)?$                  ;optionally whitespace with OTB at end of line
              )"
      , OTBCommandsRE :="
              ( Join LTrim Comment
                    iS)(*UCP)^(                                           ;case insensitive and at start of line
                    If((Not)?Exist|MsgBox)?|                              ; either If|If[Not]Exit|IfMsgBox
                    If((Not)?(Equal|InString)|(Greater|Less)(OrEqual)?)|  ; or If[not]Equal|If[not]Equal|If[Greater|Less][OrEqual]
                    IfWin(Not)?(Active|Exist)|                            ; or IfWin[Not][Active|Exist]
                    Else|Try|Catch|Finally|                               ; or
                    Loop|While|For|Switch)                                ; or
                    [,\s({]                                               ; followed by an ',' space or '(' (for expression) or '{' (in case of loop until)
              )"
      , OneLineStatementRE :="
              ( Join LTrim Comment
                    iS)(*UCP)^(                                           ;case insensitive and at start of line
                    If((Not)?Exist|MsgBox)?|                              ; either If|If[Not]Exit|IfMsgBox
                    If((Not)?(Equal|InString)|(Greater|Less)(OrEqual)?)|  ; or If[not]Equal|If[not]Equal|If[Greater|Less][OrEqual]
                    IfWin(Not)?(Active|Exist)|                            ; or IfWin[Not][Active|Exist]
                    Else|Catch|                                           ; or
                    Loop|While|For)                                       ; or
                    [,\s(]                                                ; followed by an ',' space or '(' (for expression) 
              )"
      , IncludeRE :="
              ( Join LTrim Comment
                    OiS)(*UCP)                ;case insensitive, Study and Unicode (for \s)
                    ^#Include                 ;the text '#Include' at the start of line
                    \s+                       ;at least one whitespace
                    (\*i\s)?                  ;maybe the option "*i" and at least a single whitespace
                    \s*                       ;potentially more whitespace
                    (?P<File>.*)              ;rest of the line
              )"
      , ReturnRE :=" 
              ( Join LTrim Comment
                    OiS)(*UCP)                ;case insensitive, Study and Unicode (for \s)
                    ^Return                   ;the text 'return' at the start of line
                    \s*                       ;optionally whitespace
                    (.*)                      ;rest of the line
              )"
      , GlobalVarsRE :=" 
              ( Join LTrim Comment
                    OiS)(*UCP)                ;case insensitive, Study and Unicode (for \s)
                    ^global                   ;the text 'global' at the start of line
                    \s+                       ;at least one whitespace
                    (.*)                      ;rest of the line
              )"
      ;distinguish super globals and globals?        
      
      , VarScopeRE :=" 
              ( Join LTrim Comment
                    OiS)(*UCP)                ;case insensitive, Study and Unicode (for \s)
                    ^                         ;at the start of line
                    (static|global|local)     ;one of the 3 words
                    \s                        ;minimum one whitespace
              )"
      , VarAssumeScopeRE :=" 
              ( Join LTrim Comment
                    OiS)                      ;Study
                    ^                         ;at the start of line
                    (static|global|local)     ;one of the 3 words
                    $                         ;end of line
              )"

  ;>>>> Parse the pure code 
  For i, Data in PureCode {
    Line := Data.Code
    LineNoLi := RemoveQuotedStrings(Line)
    PhysicalLineNum := Data.Line

    ;>>> #Include ----------------------------------------------------------------------------------------------------
    If RegExMatch(Line, IncludeRE, Match){
        LineInfo.Include(PhysicalLineNum, Match.File )          
        Continue                                   
    }

    ;>>> Open block counter curled braces ------------------------------------------------------------------
    ;Process braces at the start of a line
    ;the concept with InStr() was taken from CoCo's ListClasses script (line 52; http://ahkscript.org/boards/viewtopic.php?p=43349#p42793)
    While (i := InStr("}}{", SubStr(Line, 1, 1)) ) {          ;i will be 1 or 3 depending on which brace is found
      If (i = 1)
        LineInfo.PopWithin(PhysicalLineNum)
      Else
        LineInfo.Brace(PhysicalLineNum, "{", { Brace: True } )

      If !(Line := LTrim(SubStr(Line, 2), " `t")){    ;trim off first char which is a brace and remove whitespace
        LineInfo.Line(PhysicalLineNum, {OnlyBraces: True })
        Continue, 2             ;when no more braces and line is empty go to next line
      }
    }


    ;>>> Class definitions ===========================================================================================
    ;a Class definition starts with the keyword "class"
    ;Class definitions can contain variable declarations, method and property definitions, Meta-Functions and nested class definitions
    ;they are not allowed inside a function definition
    
    If (RegExMatch(Line, ClassRE, Match)) {
      LineInfo.Class(PhysicalLineNum, Match.1, {} )
      If (SubStr(Line, 0) = "{")      ;check OTB
        LineInfo.Brace(PhysicalLineNum, "{", { OTB: True } )
      Continue
    }

    ;>>> Functions / methods / meta functions / class properties =====================================================
    ;function/method/metaFunction definitions are only allowed outside of function/method definitions (outside or inside of class) and on base level of a class
;     func()      ;case 1
;     {
;     }
;
;     func(){     ;case 2 OTB
;     }
;
;     class classname
;     {
;        MethodOrMetaFunction()    ;case 3
;        {
;        }
;
;        MethodOrMetaFunction(){   ;case 4 OTB
;        }
;     }

    ;>>> Check for a new function/method/metaFunction definition
    If (RegExMatch(Line, FunctionRE, FuncName)) {    ;potential function definition or call without return value, let's check the end of line or next not empty line
      If ( (SubStr(Line, 0) = ")" AND SubStr(PureCode[i+1, "Code"], 1, 1) = "{")   ;case 1 & 3: function definition with { on next line
        OR (SubStr(Line, 0) = "{" )) {                                          ;case 2 & 4: function definition with OTB
        Type := LineInfo.isNested() ? "Method" : "Function"
        Params := GetParameterOfFunctionDef(Line)
        LineInfo.Line(PhysicalLineNum, {Type: Type, NumParams: Params.Length(), Params: Params })
        LineInfo[Type](PhysicalLineNum, FuncName.1, {NumParams: Params.Length(), Params: Params} )
        If (SubStr(Line, 0) = "{")       ;check again for OTB
          LineInfo.Brace(PhysicalLineNum, "{", { OTB: True } )
        Continue
      }
    }

      ;class property definitions are only allowed on base level of a class
      ;they can only have Get and Set functions
;       class classname
;       {
;          prop      ;case 1
;          {
;             get {
;                 return ...
;             }
;             set {
;                 return ... := value
;             }
;          }
;
;          prop{     ;case 2
;          }
;
;          prop[]    ;case 3
;          {
;          }
;
;          prop[]{   ;case 4
;          }
;       }
    ;we are not in another function definition and in a class and at the base level of a class

    ;>>> Check for a new class property definition
    If (RegExMatch(Line, PropertyRE, FuncName) And LineInfo.getNest(0).Type = "Class") {    ;potential a property definition, let's check the end of line or next not empty line
        ; If (SubStr(FuncName.0, 0) = "["                                                   ;???  regex got improved, check might not be needed anymore
            ; AND ((SubStr(Line, 0) = "]" AND SubStr(ContinuationBuffer, 1, 1) = "{") OR SubStr(Line, 0) = "{")   ;case 3 & 4
            ; Or SubStr(Line, 0) = "{" ){       ;case 1 & 2
          LineInfo.Line(PhysicalLineNum, {Type: "Property" })
          LineInfo.Property(PhysicalLineNum, FuncName.1, {} )
          If (SubStr(Line, 0) = "{")       ;check again for OTB
            LineInfo.Brace(PhysicalLineNum, "{", { OTB: True } )
          Continue
    }

    ;>>> Search for braces at the end of a line (OTB)
    ;end of line is a { with some text before it
    ;class, functions/methods and Properties have been checked for OTB previously
    ;need to check if the line is a command that supports OTB
    ;this avoids false positives like "Msgbox, {" or "StringReplace, Out, In, Search, {"
    ;it is still not 100% robust but close, e.g. "IfEqual, var, {" would still be a false positive.
    If (SubStr(Line, 0) = "{" And RegExMatch(Line, OTBCommandsRE)){
      LineInfo.Brace(PhysicalLineNum, "{", { OTB: True } )
      Continue
    }

    ;>>> Search for one line statement commands without a curled brace following on next line
    If (RegExMatch(Line, OneLineStatementRE, Match) AND SubStr(PureCode[i + 1, "Code"], 1, 1) <> "{" )
      LineInfo.Line(PhysicalLineNum + 1, { OneLineStatement: True } )
    

    ;>>> Search for Return at start of a line
    ;    for classes no check at end of the line is done, because only definitions are allowed inside a class definition,
    ;    no flow commands that would allow OTB
    If (RegExMatch(Line, ReturnRE, Match)){
      ;we found a Return, this can be
      ;  the end of one or several label, hotkey or hotstring Blocks or end of autoexec section
      ;     then it should end that block
      ;  on a one line e.g. If statement
      ;     then it should not end the current block
      ;  inside of barces (e.g. return of a function)
      ;     then it should not end the current block
      Switch LineInfo.GetWithin( i := 0).Type
      {
        Case "AutoExec", "Label", "HotKey", "HotString":
          LineInfo.PopWithin(PhysicalLineNum)
        Case "Brace":
          While ( isObject(Block := LineInfo.GetWithin(++i)) ){
            Type := Block.Type
            If Type in Function,Method,Property
            {
              LineInfo.Line(PhysicalLineNum, {Return: True, Value: Match.1, For: Block.Name " (" Block.Type ")"})
              If StrLen(Match.1)
                LineInfo.FuncReturn(Block.Type, Block.Line, {Line: PhysicalLineNum, Value: Match.1})
              Break
            }
          }
      }
    }

    ;>>> GlobalVars ----------------------------------------------------------------------------------------------------
    If InStr(Line, "global")                 ;>>> this line can potentially be removed the RegExMatch should be enough, check speed before and after removal: Check 2020-06: only minimal effect, maybe even positive effect to keep line
      If RegExMatch(Line, GlobalVarsRE, Match)
        LineInfo.Global(PhysicalLineNum, {vars: Match.1})  ;>>> problem to split into vars due to comma in objects and arrays

    ;>>> DllCalls ----------------------------------------------------------------------------------------------------
    ;>>> who has a need for showing DllCall in code explorer?
    If InStr(Line, "DllCall")                ;>>> this line can potentially be removed the RefgExMatch should be enough, check speed before and after removal
      If RegExMatch(Line, DllCallRE, Match)
        LineInfo.DllCall(PhysicalLineNum, {Call: Match.1})

    ;>>> Variables ----------------------------------------------------------------------------------------
    
    ;outside of functions "global" makes a variable super-global
      ;local and static are not allowed outside of functions
    ;within a function body 
      ;without any word all vars are assume-local 
      ;if first line is just "local" it is Force-local mode 
      ;if first line is "local var" var is local and it is Assume-GLOBAL mode
      ;if first line is just "local" and second line is just "static" it is Force-local mode with Assume-static mode
      ;if first line is just "static" it is Assume-static mode
      ;if first line is just "global" it is Assume-global mode
      ;with "global var" that var is global
      ;with "local var" that var is local
      ;with "static var" that var is static
    ;parameters or functions are local
          
    If RegExMatch(Line, VarScopeRE, Match) {
      Switch Match.Value(1)
      {
        Case "global":
        Case "local":
        Case "static":
      }
    }
    
    ;Get Variable Names when something gets assigned to them
    ;function parameters with default values will only be captured as function parameters
    Vars := GetVarAssignments(Line)
    If (i := Vars.Length())
      LineInfo.Var(PhysicalLineNum, Vars)

    ;>>> HotStrings & HotKeys ----------------------------------------------------------------------------------------
    ;HotStrings & HotKeys are not allowed inside of functions or classes or on the same line as the } of a {} block
    ;but AHK takes care of it, so I will not worry and assume that the code provided is valid AHK code.
    ;potential HotKeys and HotStrings contain a double colon
    ;in case there is code right of the double colon it is either a HotKey with implicit Return or a auto-replace HotString.
    ;without code to the right they start a block to the next Return statement
    If (InStr(LineNoLi, "::")){
      If RegExMatch(Line, HotStringRE, Match){
        LineInfo.HotString(PhysicalLineNum, Match.1, {} )
        If StrLen(Match.2)
          LineInfo.PopWithin(PhysicalLineNum)
        Continue                                   ;>>> to fix: escaped characters are not escaped in code explorer
      }
      If RegExMatch(Line, HotKeyRE, Match){
        LineInfo.HotKey(PhysicalLineNum, Match.1, {} )
        If StrLen(Match.2)
          LineInfo.PopWithin(PhysicalLineNum)
        Continue
      }
    }
    
    ;>>> Labels ----------------------------------------------------------------------------------------------------
    ;Label names are not case sensitive, and may consist of any characters other than space, tab, comma and the escape character (`).
    ;they are allowed inside of functions definitions, in this case they are 'belonging' to the function definitions
    ;since all the block braces are trimmed off, the remaining of the line is the label
    ; tn := isObject(tnCurrentFuncDef) ? tnCurrentFuncDef : oResult.Labels
    If RegExMatch(Line, LabelRE, Match){
      LineInfo.Label(PhysicalLineNum, Match.1, {} )
      Continue
    }

    ;>>> Hotkey Command
    If RegExMatch(Line, HotKeyCommandRE, Match){
      LineInfo.Hotkey(PhysicalLineNum, Match.2, {Type: "HotKeyCommand"})
      Continue
    }

    ;every line that makes it through to this point is a 'normal' command
    ;in the case of the code explorer nothing needs to be done
   }
  Return LineInfo.getAll()
}

;>>> Internal Helper Functions -----------------------------------------------------------------------------------------
RemoveComments(Line){
  If !(Pos := InStr(Trim(Line), ";"))
    Return Line             ; no quote character in this line

  If (Pos = 1)
    Return                  ; whole line is pure comment

  ;remove comments (first clean line of quotes strings)
  ; If (Pos := RegExMatch(this.RemoveQuotedStrings(Line), "\s+;.*$"))    ;AHK interpreter removes everything with " ;" even when within     var := "string with ; semi colon"     AHK will detect a comment
  If (Pos := RegExMatch(Line, "\s+;.*$"))
    Line := SubStr(Line, 1, Pos - 1)

  Return Line
}

RemoveQuotedStrings(Line){
  ;the concept how to remove quoted strings was taken from CoCo's ListClasses script (line 77; http://ahkscript.org/boards/viewtopic.php?p=43349#p42793)
  ;replace quoted strings with dots and dashes to keep length of line constant, and that other character positions do not change
  static q := Chr(34)       ; quote character

  ;Replace quoted literal strings             1) replace two consecutive quotes with dots
  CleanLine := StrReplace(Line, q . q, "..")

  Pos := 1                                 ;  2) replace ungreedy strings in quotes with dashes
  Needle := q . ".*?" . q
  While Pos := RegExMatch(CleanLine, Needle, QuotedString, Pos){
    ReplaceString =
    Loop, % StrLen(QuotedString)
       ReplaceString .= "-"
    CleanLine := RegExReplace(CleanLine, Needle, ReplaceString, Count, 1, Pos)
  }
  Return CleanLine
}

GetParameterOfFunctionDef(Line){
  static ParamsStringRE := "
               ( Join LTrim Comment
                    OS)(*UCP)                 ;Study and Unicode (for \d, \D, \s, \S, \w, \W, \b and \B)
                    ^[\w#$@]+                 ;at the start of line the function name
                    \(                        ;a opening brace
                    (.*?)                     ;$1 the parameter string
                    \)                        ;a closing brace
                    \s?                       ;optionally whitespace
                    \{?$                      ;optionally an opening curled brace
              )"
       , ParamRE:="
              ( Join LTrim Comment
                    OiS)(*UCP)                ;case insensitive (for 'ByRef'), Study and Unicode (for \d, \D, \s, \S, \w, \W, \b and \B)
                    ^(?P<ByRef>ByRef)?        ;at the start of string optionally the word ByRef
                    \s*                       ;optionally whitespace
                    (?P<ParamName>[\w#$@]+)   ;one ore more characters (A-Za-z0-9_) or #, $, @  (all allowed characters in variable names)
                    (?P<Variadic>\*)?         ;optionally a * for a variadic function
                    \s*(?:=|:=)?\s*           ;optionally whitespace, = or := and whitespace
                    (?P<DefaultValue>.*)      ;optionally a default value
              )"

  ParamsString := RegExReplace(Line, ParamsStringRE, "$1")
  CleanSting := RemoveQuotedStrings(ParamsString)
  Pos := 1
  Params := []
  While (Pos < StrLen(CleanSting) + 1) {
    If !(NextPos := InStr(CleanSting, ",", , Pos))
      NextPos := StrLen(CleanSting) + 1
    If RegExMatch(Trim(SubStr(ParamsString, Pos, NextPos - Pos)), ParamRE, Match) 
      Params.push( {ByRef: Match.ByRef, ParamName: Match.ParamName, Variadic: Match.Variadic, DefaultValue: Match.DefaultValue, Scope: "Local"} )
    Pos := NextPos + 1
  }
  Return Params
}

GetVarAssignments(Line){
  static VarLegacyAssignRE := "
               ( Join LTrim Comment
                    OS)(*UCP)                 ;Study and Unicode (for \s and \w)
                    ^\s*                      ;optionally whitespace at start of lien
                    (?P<VarName>[\w#$@]+)     ;a variable name
                    \s*                       ;optionally whitespace
                    =                         ;legacy assignment operator
              )"
       , VarExprAssignRE := "
               ( Join LTrim Comment
                    OS)(*UCP)                 ;Study and Unicode (for \s and \w)
                    (?P<VarName>[\w#$@]+)     ;a variable name
                    \s*                       ;optionally whitespace
                    (:|\+|-|\*|/|//|\.|\||&|\^|>>|<<)=             ;expression assignment operator
              )"
  ;Note: the docs state on Comma (multi-statement) that "[v1.0.46.01+]: When a comma is followed immediately by a variable and an equal sign, that equal sign is automatically treated as an assignment (:=). For example, all of the following are assignments: x:=1, y=2, a=b=c. New scripts should not rely on this behavior as it may change."
  ;these assignments are not captured by this function.
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
