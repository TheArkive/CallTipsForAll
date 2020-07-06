# CallTipsForAll
External call tip solution for multiple text editors.  Currently Scintilla and edit controls are supported.  However, in Notepad, double-click select behaves differently.  So you may need to make more use of the keyboard to invoke call tips, rather than double-click to invoke them.

Check [Language_CallTip_Design_Help.txt](./Language_CallTip_Design_Help.txt) for help on how to design call tips for another language.

## Languages Supported
* AutoHotkey v1.1.32
* AutoHotkey v2.0-a115
* AutoHotkey_H v2.110-H001

## Basic Usage

Run script and use hotkeys to invoke / close call tips.

### Hotkeys
```
CTRL+SHIFT+Space or Double-click   =   invoke Call-Tip
             ESC or Middle-click   =   close Call-Tip and Auto-Complete
                      CTRL+Space   =   reload functions / objects / classes
         (double-tap) CTRL+Space   =   call dialog to change base file
                  CTRL+ALT+Space   =   invoke Auto-Complete window manually
```

If you selected a Base File in the Settings window, or when you use the hotkey to reload, then only SAVED file data is loaded.  Click on call tips to invoke the CHM and link directly to the corresponding page.

### Classes - special note

You may not need this, but just in case:

The default mode for classes is to show all methods and properties parsed.  You can selectively hide these methods / properties by adding a `; hide` comment to the end of the line.

You can reverse this mode by adding a `; show` comment after the first line that starts the class (the opening brace `{` can be on the next line).  This will hide all elements except those that have a `; show` comment.

All comments should be on the first line of the element, even if the opening brace `{` is on the next line.

Example:

```
; show mode hides all methods properties, except those marked by "; show"
class myClass { ; show
    var1 := 1, var2 := 2
    Static var3 := 3, var4 := 4 ; show
	
    __New() {
		...
    }
    method1(x,y) { ; show
		...
    }
    method2(a,b) ; show
	{
		...
    }
}



; the brace can be on the next line, the "; show" comment should be on the first line of the class
class myClass ; show
{
	...
}


; hide mode is the default, it shows all methods / properties except those marked by "; hide"
class myClass
{
var1 := 1, var2 := 2
    Static var3 := 3, var4 := 4 ; hide
	
    __New() {
		
    }
    method1(x,y) { ; hide
		...
    }
    method2(a,b)
	{
		...
    }
}
```

## To-Do List
* Add more languages ... eventually ...
* improve parser to better handle variables set to an expression as a continuation section

## AutoHotkey objects - special cases

Some matches require a "cheat code" to get the call tips working.  Here they are:

AHK v2 (this exists for AHK v1 also, though none of the v2 elements are supported of course)
* Use comment to identify objects:
```
; Array
; Buffer
; File
; BoundFunc
; Function
; GuiControl
; Gui
; InputHook
; Listview
; Map
; Menu
; Object
; RegExMatch
; StatusBar
; TreeView
```

This is a workaround for being able to recognize a particular var as an object even if it isn't being set by a pre-defined pattern in the language files.

## Features

Best bits:
* Auto-Hotkey commands, functions, objects are mostly complete and display calltips
* #INCLUDEs are processed, unless the Base File is omitted in user settings, then current window is processed
* User defined functions are parsed so you can quickly reference the parameters in call tips
* User defined classes are defined with a full list of properties and methods
* User class methods and properties can be filtered with `; hide` and `; show` comments, see above

Quick Access to help file topics:
* Most call tips for an element of the AHK language links to a help page in the corresponding CHM help file
* Click call tips to bring up an associated help page

Language file format:
* Language file format allows for making call tips for more languages than just AHK
* In `Languages\{lang}\Other` define how functions and classes are formatted with regex

Misc:
* Call tip records can contain multipul pages, navigate these pages with `Up` and `Down` keys when call tip is visible.
* Easily invoke and close call tips with keyboard and/or mouse
* Quickly change "base file" (same as a "project file") with double-tap of a hotkey you specify in Settings
* Access user settings from tray icon menu

## For Best Results...
* Start a supported text editor first, then start this script.  This shouldn't be necessary, but if you experience issues, close the text editor and the script, then start the text editor, then the script.
* Don't reuse a variable name as a different type or object than previously used
* Be sure to use some indenting on the body of your functions and classes
* don't try to create an instance of a class with the same name as the class
    * Example:  `msgbox2 := msgbox2.New(...)   <-- do NOT do this`
    * In AHK v2 attempting to do this i think throws an error
    * I think you can do this in AHK v1, but it is highly NOT recommended
* Set your settings first and then try to invoke call tips on your document

## Contributors
From the AHK forums:
* robodesign - he helps me think when my brain is tied in knotts
* toralf - suggested a better structure, and working on a better parser to incorporate more debug info

## Languages -- status

AHK v1 and v2 call tips have been created, and are quite comprehensive, but not 100% complete.  AHK v2 is currently the most complete.  At a minimum basic commands and built-in functions should be complete for AHK v1 and v2.

For AHK v1:
Most of what remains for AHK v1 is objects (mostly arrays) resulting from functions and/or commands.

For AHK v2:  Done as far as I can tell.  Let me know if I missed something.

AutoHotkey CHM help files are included in this release, but you can of course replace them with your local copy if desired.

If you find any info missing that should be added in the call tips please add an issue or contact me ont he forum and I'll try to address it as soon as possible.

