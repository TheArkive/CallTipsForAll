; LineInfo stores info per file per line
; when info is set it either gets added or if key exists overwritten

/*
.File(filename)                ;set filename (until next filename is set)
.File(filename, WithinStack)   ;set filename with WithinStack (until next filename is set)
.set(type, line#, StringOrObject) ;set info for Type and line of set file
.has(type, line#, keys*)       ;returns True/False if type on line has the key(s) of set file
.has(type, line#)              ;returns True/False if type has line info for set file
.has(type)                     ;returns True/False if type info has been set for set file
.getAll()                      ;return all info 
.get()                         ;return all info of set file
.get(type)                     ;return all info of type of set file
.get(type, line#)              ;return all info of type of line of set file
.get(type, line#, keys*)       ;return one value for one key or an object of {key: value, ...} for multiple keys of type of line of set file
.deleteAll()                   ;delete all info 
.delete()                      ;delete all info of set file
.delete(type)                  ;delete all info of type of set file
.delete(type, line#)           ;delete all info of type of line of set file
.delete(type, line#, keys*)    ;delete one or more keys of a type of a line of set file

.Var(line#, StringOrObject)          ;store an array of vars 
.DllCall(line#, StringOrObject)      ;store properties of DllCall 
.Include(line#, StringOrObject)      ;store properties of #Include
.Global(line#, StringOrObject)       ;store lines that start with global
.Line(line#, StringOrObject)         ;store properties of a line

.SetItem(Type, line#, Name, Props)           ;helper function to store properties by type
.Search(line#, SearchRE, properties)         ;store properties of SearchRE 
.DocComment(line#, DocComment, properties)   ;store a DocComment and it's properties 

.SetWithin(Type, line#, Name)      ;add Name to stack of within
.PopWithin(line#)                  ;remove Block from stack of within
.StoreWithin(line#)                ;stores Within info on all line in Block
.GetWithin(Level)                  ;get Block Info (Level 0 is head, Level 1 the one below, etc)
.GetWithinStack()                  ;get Stack as a string

.Hotkey(line#, Name, properties)     ;store a Hotkey and it's properties 
.HotString(line#, Name, properties)  ;store a HotString and it's properties
.Brace(line#, Name, StringOrObject)          ;set within and stores info to line
.CommentSection(line#, Name, StringOrObject) ;set within and stores info to line
.ContiBlock2(line#, Name, StringOrObject)    ;set within and stores info to line

.SetNesting(Type, line#, Name, Props) ;store current nest
.isNested()                           ;returns level of nesting, "" for no nesting
.getNest(Level)                       ;get Nest Info (Level 0 is head, Level 1 the one below, etc)
.PopNesting(line#)                    ;remove Nest from stack of Nesting

                   Properties are one object with keys; {key1: info, ... } 
.Function(line#, Name, properties)   ;store a function and it's properties 
.Property(line#, Name, properties)   ;store a property and it's properties 
.Method(line#, Name, properties)     ;store a method and it's properties 
.Class(line#, Name, properties)      ;store a class and it's properties 
.Label(line#, Name, properties)      ;store a Label and it's properties 
.FuncReturn(Type, line#, Props)      ;store the return values of a function/method

.Comment(line#, code)                ;removes comments from code, stores the comment internally and returns code without comment
.GuessDocComment()                   ;returns an object of best guess DocComments in the stored comments 
.RemoveComments(LineOfCode)          ;remove comments from line of code
.RemoveQuotedStrings(LineOfCode)     ;remove literal strings from line of code
*/

Class LineInfo {

  File(filename, WithinStack){
    old := this._File

    this.Info[ this._File := filename ] := {}
    this.Comments[ this._File ] := {}
    
    If !isObject(WithinStack)
      WithinStack := [ {Type: "AutoExec", Line: 1, Name: "AutoExec"} ]
    this.WithinStack[ this._File ] := WithinStack
    this.NestingStack := []
    return old
  }

  set(Type, Line, StringOrObject){
    If !IsObject(StringOrObject)
      Return this.Info[ this._File, Type, Line] := StringOrObject
    For key, value in StringOrObject {
      If isObject(value) {  ;>>> make this more robust by going as deep as possible into object.
        For k, v in value
          this.Info[ this._File, Type, Line, key, k ] := v
     } Else
        this.Info[ this._File, Type, Line, key ] := value
      i := A_Index
    }
    Return i
  }
  
  has(Type, line, keys*){
    Exist := true
    If keys.MaxIndex() {
      For i, key in keys
        If isObject(key)
          For j, k in key
            Exist := this.Info[ this._File, Type, line ].Haskey( k ) ? Exist : False
        Else
          Exist := this.Info[ this._File, Type, line ].Haskey( key ) ? Exist : False
      Return Exist
    } 
    If line
        Return this.Info[ this._File, Type ].Haskey( line )
    Return this.Info[ this._File ].Haskey( Type )
  }
  
  get(Type, line, keys*){
    If keys.MaxIndex() {
      If (keys.MaxIndex() = 1 AND !isObject(keys[1]))
        Return this.Info[ this._File, Type, line, keys[1] ]
      obj := {}
      For i, key in keys
        If isObject(key)
          For j, k in key
            obj[key] := this.Info[ this._File, Type, line, k ]
        Else
          obj[key] := this.Info[ this._File, Type, line, key ]
      Return obj
    } Else If line {
      Return this.Info[ this._File, Type, line ]
    } Else If Type {
      Return this.Info[ this._File, Type ]
    } Else {
      Return this.Info[ this._File ]
    }
  }
  
  getAll(){
    Return this.Info
  }
  
  delete(Type, line, keys*){            ;currently not used at all
    If keys.MaxIndex() {
      If (keys.MaxIndex() = 1 AND !isObject(keys[1]))
        Return this.Info[ this._File, Type, line ].Delete( keys[1] )
      For i, key in keys {
        If isObject(key)
          For j, k in key
            obj[key] := this.Info[ this._File, Type, line ].Delete( k )
        Else
          this.Info[ this._File, Type, line ].Delete( key )
        i := A_Index
      }
      Return i
    } Else If line {
      Return this.Info[ this._File, Type ].Delete( line )
    } Else If Type {
      Return this.Info[ this._File ].Delete( Type )
    } 
    this.Comments.Delete( this.File )
    this.WithinStack.Delete( this._File )
    this.NestingStack := []
    Return this.Info.Delete( this._File )
  }
  
  deleteAll(){
    this.Comments := {}
    this.WithinStack := {}
    this.NestingStack := []
    Return this.Info := {}
  }


  Var(line, StringOrObject){
    Return this.Set("Var", line, StringOrObject)
  }
  DllCall(line, StringOrObject){
    Return this.Set("DllCall", line, StringOrObject)
  }
  Include(line, StringOrObject){
    Return this.Set("Include", line, StringOrObject)
  }
  Global(line, StringOrObject){
    Return this.Set("Global", line, StringOrObject)
  }
  Line(line, StringOrObject){
    Return this.Set("Line", line, StringOrObject)
  }


/*
differences between SetItem, Within and Nesting:
"Nesting" is only for Function, Classes, Methods and Properties (and Labels, Return within), it is purely to nest these at the correct place
"Within" is keeping track of all braces and indentations as well as documenting the WithIn properties for each line (from set to pop)
"SetItem" is documenting the provided properties for a line 
*/


  SetItem(Type, line, Name, Props){
    this.Line(line, {(Type): True})
    If StrLen(Name)
      Props["Name"]:= Name
    Return this.set(Type, line, Props)
  }
  
  Search(line, Name, Props){
    Return this.SetItem("Search", line, Name, Props)
  }
  DocComment(line, Name, Props){
    Return this.SetItem("DocComment", line, Name, Props)
  }

;####### Within

  SetWithin(Type, line, Name, Props){
    Return this.WithinStack[ this._File ].push( {Type: Type, Line: Line, Name: Name} )
  }
  PopWithin(line){
    this.StoreWithin(line)
    Block := this.WithinStack[ this._File ].pop()
    NextType := this.GetWithin(0).Type
    Switch Block.Type
    {
      Case "Brace":    ;it's a curled brace block, if it belongs to a Function,Class,Method,Property, they should be closed as well
        While (NextType = "Label") {     ;a function can have labels within, they end with the Brace that closes the function. 
                                         ;The current code might not be robust (doesn't check if function really ends with this brace) but seems ok for now. 
                                         ;      Otherwise a GoTo or GoSub would be jumping into a curled brace Block, which would be pretty bad coding.
          this.StoreWithin(line)
          Block := this.WithinStack[ this._File ].pop()
          this.PopNesting(line)
          NextType := this.GetWithin(0).Type
        }
        If NextType in Function,Class,Method,Property 
        {
          this.StoreWithin(line)
          Block := this.WithinStack[ this._File ].pop()
          this.PopNesting(line)
        }
      Case "Label", "HotKey", "HotString":   ;If was a Return, check if it also closes other "Labels"
        While (NextType = "Label" Or NextType = "HotKey" Or NextType = "HotString") {
          this.StoreWithin(line)
          Block := this.WithinStack[ this._File ].pop()
          NextType := this.GetWithin(0).Type
        }
    }
    Return Block
  }
  StoreWithin(line){
    Block := this.GetWithin(0)
    Stack := this.GetWithinStack()
    Loop, % line - Block.Line + 1
    {
      LineID := A_Index + Block.Line - 1
      If !this.has("Line", LineID, "WithinType")  ;only if line doesn't have the info yet, all others are nested within current block
        this.Line( LineID, {WithinType: Block.Type ,WithinName: Block.Name ,WithinStack: Stack} )  
    }
  }
  GetWithin(Level){
    Return this.WithinStack[ this._File, this.WithinStack[this._File].MaxIndex() - Level ]
  }
  GetWithinStack(){
    For i, Data in this.WithinStack[ this._File ] {
      tmpstr .= (Data.Name ? Data.Name : Data.Type) "\"
    }
    Return Trim(tmpstr, "\")
  }
  

  Hotkey(line, Name, Props){
    this.SetItem("Hotkey", line, Name, Props)
    If (Props.Type = "HotKeyCommand")
      Return
    Return this.SetWithin("Hotkey", line, Name, Props)
  }
  HotString(line, Name, Props){
    this.SetItem("HotString", line, Name, Props)
    Return this.SetWithin("HotString", line, Name, Props)
  }

  Brace(line, Name, StringOrObject){
    this.Line(line, StringOrObject)
    Return this.SetWithin("Brace", line, Name, {})
  }
  CommentSection(line, Name, StringOrObject){
    this.Line(line, StringOrObject)
    Return this.SetWithin("CommentSection", line, Name, {})
  }
  ContiBlock2(line, Name, StringOrObject){
    this.Line(line, StringOrObject)
    Return this.SetWithin("ContiBlock2", line, Name, {})
  }


;####### Nesting

  SetNesting(Type, line, Name, Props){
    this.SetWithin(Type, line, Name, Props)
    If this.isNested() {
      
      ;these 3 lines are identical to SetItem, but then Props is stored at a different node
      this.Line(line, {(Type): True})
      If StrLen(Name)
        Props["Name"]:= Name
        
      Nest := this.getNest(0)
      Nest.Pointer[ Type, line ] := Props
      this.NestingStack.push( {Type: Type, line: line, Name: Name, Pointer: Nest.Pointer[ Type, line ] } )
    } Else {
      this.SetItem(Type, line, Name, Props)
      this.NestingStack.push( {Type: Type, line: line, Name: Name, Pointer: this.Info[ this._File, Type, line ] } )
    }
    Return 
  }
  isNested(){
    Return this.NestingStack.MaxIndex()
  }
  getNest(Level){
    Return this.NestingStack[ this.Nesting.MaxIndex() - Level ]
  }
  PopNesting(line){
    Nest := this.NestingStack.pop()
    Nest.Pointer["LineEndOfBody"] := line
    Return Nest
  }

  Function(line, Name, Props){
    Return this.SetNesting("Function", line, Name, Props)
  }
  Property(line, Name, Props){
    Return this.SetNesting("Property", line, Name, Props)
  }
  Method(line, Name, Props){
    Return this.SetNesting("Method", line, Name, Props)
  }
  Class(line, Name, Props){
    Return this.SetNesting("Class", line, Name, Props)
  }
  Label(line, Name, Props){
    Return this.SetNesting("Label", line, Name, Props)
  }
  
  FuncReturn(Type, line, Props){       
    If this.isNested() {
      Pointer := this.getNest(0).Pointer
    } Else {
      Pointer := this.Info[ this._File, Type, Line ]
    }
    If Pointer.HasKey("ReturnValue")
      Pointer["ReturnValue"].push( Props )
    Else
      Pointer["ReturnValue"] := [ Props ]
    Return 
  }
  
;####### Comments
  
  GuessDocComment(){
    tmpobj := {}
    For file, FileData in this.Comments {
      For line, CommentData in FileData {
        For i, Comment in CommentData {
          If RegExMatch(Comment, "[^\s]*", Match){
            If tmpobj.HasKey(Match)
              tmpobj[Match].push( { File: file, Line: line, Comment: Comment} )
            Else
              tmpobj[Match] := [ { File: file, Line: line, Comment: Comment } ]
          }
        }
      }
    }
    DocComments := {}
    For DC, DCData in tmpobj {
      If ( StrLen(DC) > 2 AND ! StrReplace(DC, SubStr(DC, 1, 1)) )  ;minimum 3 chars and all identical
        DocComments[DC] := DCData
      Else If ( RegExMatch(DC, "^[^a-zA-Z0-9]*$") )                 ;completely no word character 
        DocComments[DC] := DCData
      Else If ( DCData.MaxIndex() > 4 AND StrLen(DC) > 4  )         ;minimum 5 chars, more then 4 hits and all identical
        DocComments[DC] := DCData
    }
    Return DocComments
  }
  
  Comment(line, code){
    StrippedCode := this.RemoveComments(code)
    Comment := LTrim(StrReplace(code, StrippedCode,,1), " `t;")
    If StrLen(Comment){
      If !isObject( this.Comments[ this.File, line ] )
        this.Comments[ this.File , line ] := [ Comment ]
      Else  
        this.Comments[ this.File , line ].push( Comment )
    }
    Return StrippedCode
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

}
