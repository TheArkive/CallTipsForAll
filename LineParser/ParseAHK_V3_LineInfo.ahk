; LineInfo stores info per file per line
; when info is set it either gets added or if key exists overwritten

/*
LineInfo.File(filename)                ;set filename (until next filename is set)
LineInfo.File(filename, WithinStack)   ;set filename with WithinStack (until next filename is set)
LineInfo.has(type, line#, keys*)       ;returns True/False if type on line has the key(s) of set file
LineInfo.has(type, line#)              ;returns True/False if type has line info for set file
LineInfo.has(type)                     ;returns True/False if type info has been set for set file
LineInfo.set(type, line#, StringOrObject) ;set info for Type and line of set file
LineInfo.getAll()                      ;return all info 
LineInfo.get()                         ;return all info of set file
LineInfo.get(type)                     ;return all info of type of set file
LineInfo.get(type, line#)              ;return all info of type of line of set file
LineInfo.get(type, line#, keys*)       ;return one value for one key or an object of {key: value, ...} for multiple keys of type of line of set file
LineInfo.deleteAll()                   ;delete all info 
LineInfo.delete()                      ;delete all info of set file
LineInfo.delete(type)                  ;delete all info of type of set file
LineInfo.delete(type, line#)           ;delete all info of type of line of set file
LineInfo.delete(type, line#, keys*)    ;delete one or more keys of a type of a line of set file

                           Properties are one object with keys; {key1: info, ... } 
LineInfo.Function(line#, Name, properties)   ;store a function and it's properties 
LineInfo.Property(line#, Name, properties)   ;store a function and it's properties 
LineInfo.Class(line#, Name, properties)      ;store a class and it's properties 
LineInfo.Label(line#, Name, properties)      ;store a Label and it's properties 
LineInfo.Hotkey(line#, Name, properties)     ;store a Hotkey and it's properties 
LineInfo.HotString(line#, Name, properties)  ;store a HotString and it's properties
LineInfo.Search(line#, SearchRE, properties) ;store properties of SearchRE 

LineInfo.Var(line#, StringOrObject)          ;store an array of vars 
LineInfo.DllCall(line#, StringOrObject)      ;store properties of DllCall 
LineInfo.Include(line#, StringOrObject)      ;store properties of #Include
LineInfo.Global(line#, StringOrObject)       ;store lines that start with global
LineInfo.Line(line#, StringOrObject)         ;store properties of a line
 
LineInfo.SetWithin(Type, line#, Name)      ;add Name to stack of within
LineInfo.PopWithin(line#)                  ;remove Name from stack of within
LineInfo.StoreWithin(line#)                ;stores Within info on all line in Block
LineInfo.GetWithin(Level)                  ;get Block Info (Level 0 is head, Level 1 the one below, etc)
LineInfo.GetWithinStack()                  ;get Stack as a string

LineInfo.Comment(line#, code)                ;removes comments from code, stores the comment internally and returns code without comment
LineInfo.DocComment(line#, DocComment, properties) ;store a DocComment and it's properties 
LineInfo.GuessDocComment()                   ;returns an object of best guess DocComments in the stored comments 
*/

Class LineInfo {

  File(filename, WithinStack){
    old := this._File

    this.Info[ this._File := filename ] := {}
    this.Comments[ this._File ] := {}
    
    If !isObject(WithinStack)
      WithinStack := [ {Type: "AutoExec", Line: 1, Name: "AutoExec"} ]
    this.WithinStack[ this._File ] := WithinStack

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
  
  delete(Type, line, keys*){
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
    Return this.Info.Delete( this._File )
}
  
  deleteAll(){
    this.Comments := {}
    this.WithinStack := {}
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


  SetItem(Type, line, Name, Props){
    If StrLen(Name)
      Props["Name"]:= Name
    this.Line(line, {(Type): True})
    Return this.set(Type, line, Props)
  }
  
  Function(line, Name, Props){
    Return this.SetItem("Function", line, Name, Props)
  }
  FuncReturn(Type, line, Props){
    If this.has(Type, Line, "ReturnValue")
      this.Info[ this._File, Type, Line, "ReturnValue"].push( Props )
    Else
      this.Info[ this._File, Type, Line, "ReturnValue"] := [ Props ]
    Return 
  }
  
  Property(line, Name, Props){
    Return this.SetItem("Property", line, Name, Props)
  }
  Method(line, Name, Props){
    Return this.SetItem("Method", line, Name, Props)
  }
  Class(line, Name, Props){
    Return this.SetItem("Class", line, Name, Props)
  }
  Label(line, Name, Props){
    Return this.SetItem("Label", line, Name, Props)
  }
  Hotkey(line, Name, Props){
    Return this.SetItem("Hotkey", line, Name, Props)
  }
  HotString(line, Name, Props){
    Return this.SetItem("HotString", line, Name, Props)
  }
  Search(line, Name, Props){
    Return this.SetItem("Search", line, Name, Props)
  }
  DocComment(line, Name, Props){
    Return this.SetItem("DocComment", line, Name, Props)
  }
  
  SetWithin(Type, line, Name){
    Return this.WithinStack[ this._File ].push( {Type: Type, Line: Line, Name: Name} )
  }
  PopWithin(line){
    prev := this.GetWithinStack()
    this.StoreWithin(line)
    Block := this.WithinStack[ this._File ].pop()
    Type := LineInfo.GetWithin(0).Type
    If Type in Function,Class,Method,Property
      Block := this.WithinStack[ this._File ].pop()
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
