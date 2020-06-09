# CallTipsForAll
External call tip solution for multiple text editors.  Currently Scintilla and edit controls are supported.

Check [Language_CallTip_Design_Help.txt](./Language_CallTip_Design_Help.txt) for help on how to design call tips for another language.

## Languages Supported
* AutoHotkey v1.1.32
* AutoHotkey v2.0-a108

## Basic Usage

Run script and use hotkeys to invoke / close call tips.

### Hotkeys
```
CTRL+SHIFT+Space or Double-click   =   invoke call tip
             ESC or Middle-click   =   close call tip
                      CTRL+Space   =   reload functions / objects / classes
         (double-tap) CTRL+Space   =   call dialog to change base file
```

NOTE: If you selected a Base File in the Settings window, then only SAVED file data is loaded.

Click on call tips to invoke the CHM and link directly to the corresponding page.

You can disable double-click to open call tip in Settings window from tray menu.

### Classes - special note

The default mode for classes is to show all methods and properties parsed.  You can selectively hide these methods / properties by adding a `; hide` comment to the end of the line.

You can reverse this mode by adding a `; show` comment after the brace `{` that starts a class.  This will hide all elements except those that have a `; show` comment.

All comments should be on the first line of the element, after a brace `{` if applicable.

Example:

```
class myClass { ; show
    var1 := 1, var2 := 2
    Static var3 := 3, var4 := 4 ; show
	
    __New() {
		
    }
    method1(x,y) { ; show
	
    }
    method2(a,b) {
	
    }
}
```

In the above example, only the properties `var3 and var4` will show, and only `method1()` will show.

## Current Status
Adding code to parse the script for properties / methods defined in user objects (not classes).

* Currently only user object type and name is added to a list.  Only user classes actually parse methods and properties to list them.  Adding this ability isn't hard, but adds another parsing of the full document (including #INCLUDEs)

Current parsing pattern is as follows:

* 1st parse for #INCLUDEs (if enabled in user settings) - then concatenate all documents
* 2nd parse for user defined functions
* 3rd parse for user classes, properties, and methods
* 4th parse for instances of classes
* 5th parse for user defined objects
* ... this will add a 6th parse to be able to list props/methods attached to user objects

I feel like this isn't ideal, but at least all this happens in under a second, and I'm frequently finding ways to add to the flexibility without sacrificing accuracy.  The primary down-side now is that when parsing user objects, all "data members" will be identified as "properties" or just "data members", because I'm not yet sure how one adds methods to an object that is not a class.

Nested user objects aren't tracked very well.  Currently when an object is assigned to be nested inside another object, the script actually reads the last element in a nested object structure, and simply stores the "object name" as the last nested element, instead of the full `obj.obj.member` match.  I'll probably try to fix this first before I go on to actually attempt to store lists of properties of user created objects.  Needless to say, tracking this kind of nesting and being accurate with each sub-object's type and list of properties is not going to be easy.

## To-Do List
* Parse the script for `.properties` defined in user objects (not classes)
* Add auto-complete support for keywords and object methods / properties
* Add more languages ... but that will take a while
* Enable full customization of hotkeys to invoke / close call tips
* Index line numbers for custom functions, classes, and class instances

## AutoHotkey objects - special cases

Some matches require a "cheat code" to get the call tips working.  Here they are:

AHK v2 (this is planned for AHK v1 also)
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

This is a workaround for being able ot use call tips on objects that are extracted from user created objects, which is just a convention created by the user, and not an actual creation of the object as documented by the language.

## Features

Best bits:
* Auto-Hotkey commands, functions, objects are mostly complete and display calltips
* #INCLUDEs are processed, unless omitted in user settings, then current window is processed
* User defined functions are parsed so you can quickly reference the parameters in call tips
* User defined classes are defined with a full list of properties and methods
* User class methods and properties can be filtered with `; hide` and `; show` comments, see above

Access to help files:
* Most call tips for an element of the AHK language links to a help page in the corresponding CHM help file
* Click call tips to bring up an associated help page

Language file format:
* Language file format allows for making call tips for more languages than just AHK
* In `Languages\{lang}\Other` define how functions and classes are formatted with regex

Misc:
* Call tip records can contain several pages, navigate these pages with `Up` and `Down` keys when call tip is visible.
* Easily invoke and close call tips with keyboard and mouse
* Quickly change "base file" (same as a "project file") with double-tap CTRL+Space
* Access user settings from tray icon menu

## For Best Results...
* Besure to use the `Static` prefix in your classes to make sure that only the properties and methods that should be shown are displayed in the call tips
* Don't reuse a variable name as a different type or object than previously used
* Be sure to use some indenting on the body of your functions and classes
* don't try to create an instance of a class with the same name as the class
    * Example:  `msgbox2 := msgbox2.New(...)   <-- do NOT do this`
    * In AHK v2 attempting to do this i think throws an error
    * I think you can do this in AHK v1, but it is highly NOT recommended
* Set your settings first and then try to invoke call tips on your document
* Make use of `; show` and `; hide` comments in your classes so that only the methods / properties that are meant to be used by the user are shown in call tips.  See above in Basic Usage.

## Contributors
From the AHK forums:
* robodesign - he helps me think when my brain is tied in knotts
* toralf - suggested a better structure, since the code was getting messy

## Languages -- status

AHK v1 and v2 call tips have been created, and are quite comprehensive, but not 100% complete.  AHK v2 is currently the most complete.  At a minimum basic commands and built-in functions should be complete for AHK v1 and v2.

For AHK v1:
Most of what remains for AHK v1 is objects (mostly arrays) resulting from functions and/or commands.

For AHK v2:
Little things like the `Pos` and `ClientPos` object related to GUI objects are unfinished in AHK v2, and a few more instances of built-in functions returning arrays needs to be added as well.

AutoHotkey CHM help files are included in this release, but you can of course replace them with your local copy if desired.

If you find any info missing that should be added in the call tips please add an issue and I'll try to address it as soon as possible.

