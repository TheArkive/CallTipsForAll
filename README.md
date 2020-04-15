# CallTipsForAll
External call tip solution for multiple text editors.  Currently Notepad and Notepad++ are supported.

Check [Language_CallTip_Design_Help.txt](./Language_CallTip_Design_Help.txt) for help on how to design call tips for another language.

Currentlly only AHK v2 call tips have been created.  AHK v1 call tips will be available soon.

AutoHotkey CHM help files are included in this release, but you can of course replace them with your local copy if desired.

If you find any info missing that should be added in the call tips please add an issue and I'll try to address it.  Keep in mind certain things are more tricky, like trying to regocnize ListView and TreeView objects in AHK v2.

## Basic Usage

Run script and use hotkeys to invoke call tips.

### Hotkeys
```
CTRL + SHIFT + Space   =   invoke call tip
ESC                    =   close call tip

Click on call tips to invoke the CHM and link directly to the corresponding page.
```
### User Settings
For now, change user settings in the script.  GUI coming later.

## Latest updates
* AutoHotkey v1 and v2 call tips are ready!  Call Tips should be around 90% complete on both.
* AHK v1 version of the script is now ready.

## To-Do List
* Add auto-complete as CTRL + ALT + Space or automatic
* Add GUI to save user settings and easily switch languages
* Improve support for `var := expr, var := expr`
* enable #INCLUDE processing for custom functions and objects
* parse classes
* Add ListView / TreeView object definitions
* Add more languages ... but that will take a while

## AutoHotkey language - special cases

Some cases require a "cheat code" to get the call tips working.  Here they are:

AHK v1 and v2
* InputHook Object from callback, example:
```
InputHook.OnKeyDown := Func("myFunc")
myFunc(iHook, VK, SC) { ... } ; InputHookObject

iHook is detected as an InputHook object.
```

AHK v2
* Menu object from label/function
```
trayMenu := A_TrayMenu
trayMenu.Add("menu item","iconMenu")
iconMenu(ItemName, ItemPos, MenuObj) { ... } ; MenuObject

MenuObj is detected as a Menu object.
```

AHK v2 (in progress for next update)
* ListView object from `GuiObj["item"]`
```
LvObj := GuiObj["item"] ; ListViewObject

LvObj is detected as a ListView object
```

AHK v2 (in progress for next update)
* TreeView object from `GuiObj["item"]`
```
TvObj := GuiObj["item"] ; TreeViewObject

TvObj is detected as a TreeView object
```

## For Best Results...
* Try not to use literal quotes within strings
* Don't reuse a variable name if it is a diffeernt type/object
