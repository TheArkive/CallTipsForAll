# CallTipsForAll
External call tip solution for multiple text editors.  Currently Scintilla and edit controls are supported.

Check [Language_CallTip_Design_Help.txt](./Language_CallTip_Design_Help.txt) for help on how to design call tips for another language.

AHK v1 and v2 call tips have been created, and are quite comprehensive, but not 100% complete.  AHK v2 is currently the most complete, howver basic commands and built-in functions should be complete for AHK v1 and v2.  Most of what remains is objects (mostly arrays) resulting from functions and/or commands.

AutoHotkey CHM help files are included in this release, but you can of course replace them with your local copy if desired.

If you find any info missing that should be added in the call tips please add an issue and I'll try to address it as soon as possible.

## Basic Usage

Run script and use hotkeys to invoke call tips.

### Hotkeys
```
CTRL+SHIFT+Space or Double-click   =   invoke call tip
             ESC or Middle-click   =   close call tip

Click on call tips to invoke the CHM and link directly to the corresponding page.
```
### User Settings
Use Settings window from the system tray to change user settings.  Enter the main EXE of the program and the ClassNN of the control preferred for use (minus the number, ie. "scintilla", not "scintilla1").

## To-Do List
* Add auto-complete support for keywords and object methods / properties
* Support for user classes
* enable #INCLUDE processing for custom functions and objects
* Add more languages ... but that will take a while

## AutoHotkey language - special cases

Some matches require a "cheat code" to get the call tips working.  Here they are:

AHK v2
* Use comment to identify objects:
```; Array
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
; TreeView```

This is a workaround for being able ot use call tips on objects that are extracted from user created objects, which is just a convention created by the user, and not an actual creation of the object as documented by the language.

## For Best Results...
* Don't reuse a variable name as a different type or object than previously used
