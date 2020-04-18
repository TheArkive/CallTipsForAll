# CallTipsForAll
External call tip solution for multiple text editors.  Currently Notepad and Notepad++ are supported.

Check [Language_CallTip_Design_Help.txt](./Language_CallTip_Design_Help.txt) for help on how to design call tips for another language.

Currentlly only AHK v2 call tips have been created.  AHK v1 call tips will be available soon.

AutoHotkey CHM help files are included in this release, but you can of course replace them with your local copy if desired.

If you find any info missing that should be added in the call tips please add an issue and I'll try to address it.  Keep in mind certain things are more tricky, but I'll try my best.

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
* Reworked parsing objects, now more efficient and less lag
* Now, keywords can have multiple call tips, use UP/DOWN arrows to scroll
* reworked the call tip file format, now user can define multiple custom call tips per keyword

* AutoHotkey v1 and v2 call tips are ready!  Call Tips should be around 90% complete on both.
* AHK v1 version of the script is now ready.

## To-Do List
* Attempt to detect literal ``` `" and `\"` ``` in strings
* Add auto-complete as CTRL + ALT + Space or automatic
* Add GUI to save user settings and easily switch languages
* enable #INCLUDE processing for custom functions and objects
* parse classes
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

AHK v2 Language Definition Update
* All defined objects in the call tip language files can now be detected by comments.  Examples:
```
MapObj := UnknownObj["UnknownItem"] ; MapObject
    ; MapObj is detected as a MapObject object

Array := KnownObject["UnknownItem"] ; ArrayObject
    ; Array is detected as an ArrayObject
```

This is a workaround for being able ot use call tips on objects that are extracted from user created objects, which is just a convention created by the user, and not an actual creation of the object as documented by the language.

## For Best Results...
* Don't reuse a variable name as a different type or object than previously used
