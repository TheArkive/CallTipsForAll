# CallTipsForAll
External call tip solution for multiple text editors.  Currently Notepad and Notepad++ are supported.

Check [Language_CallTip_Design_Help.txt](./Language_CallTip_Design_Help.txt) for help on how to design call tips for another language.

Currentlly only AHK v2 call tips have been created.  AHK v1 call tips will be available soon.

AutoHotkey CHM help files are included in this release, but you can of course replace them with your local copy if desired.

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

## To-Do List

* make AHKv1 compatible version
* Add auto-complete as CTRL + ALT + Space or automatic
* Add GUI to save user settings and easily switch languages
* Improve support for `var := expr, var := expr`
* enable #INCLUDE processing for custom functions and objects
* parse classes
* Add ListView / TreeView object definitions
* Add more languages ... but that will take a while
