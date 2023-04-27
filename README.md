# rtbinobj
Windows tool to convert binary files To DOS Object (OMF) format. Creates byte exact files as Turbo Pascal 

BINOBJ and Turbo C BGIOBJ. Can be used with Turbo Pascal, QuickPascal, Freepascal 8086,Turbo C, QuickC, and 

Open Watcom compilers. Allows you to embed images/audio or any binary file to your exe and access at runtime 

using extern variable. The output format is strictly for DOS OBJ format. Includes GUI and console versions

In addtion to supporting the same output format (byte exact) you can also now add 

"public size name" to access size of linked file at runtime. Example for Turbo Pascal in Examples directory.


rtbinobjgui.exe is the GUI version - just click and select your file, Save As to convert to OBJ.

rtbinobj.exe is the console version you run in the command line (cmd.exe)

rtbinobj infile outfile public_name

eg. rtbinobj image.xgf image.obj MYIMAGE

link from Turbo Pascal/freepascal 8086 with $L directive

procedure MYIMAGE; external;

{$L image.obj}
 
![](https://github.com/retronick2020/rtbinobj/wiki/rtbinobjgui.png)
![](https://github.com/retronick2020/rtbinobj/wiki/rtbinobj.png) 
