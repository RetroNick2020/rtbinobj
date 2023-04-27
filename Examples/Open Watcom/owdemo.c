/* ************************************************************ */
/* owdemo.c _putimage link to exe demo For Open Watcom C        */
/*                                                              */
/* OW.XGF was created by Exporting image as _putimage file      */
/* from Raster Master. rtbinobj was used to convert image to obj*/
/* format. This allows us to link image to exe without creating */
/* c arrays.                                                    */
/*                                                              */
/* RTBGIOBJ ow.xgf ow.obj _ow                                   */
/* wcl owdemo.c ow.obj                                          */
/* wcl386 /ldos4g owdemo.c ow.obj                               */
/* or 2 step version                                            */
/* wcl -c owdemo.c                                              */
/* wlink name owdemo.exe file {owdemo.obj ow.obj}               */
/* ************************************************************ */

#include <graph.h>
#include <conio.h>


extern char far ow;    //far keyword very important - works in all modes 16/32 bit
extern unsigned long far owsize;  //change to int to long for 32bit OBJ's

void main()
{
  _setvideomode(_VRES16COLOR);  
  _putimage(150,150,&ow,_GPSET); // & required to display image correctly

  getch();
  _setvideomode(_DEFAULTMODE);
  printf("%u",owsize); //unsigned int
  //printf("%d",owsize); //unsigned long
}  


