Program TestOBJ;
 uses graph;


procedure myimage; external;     (*public name in RTBINOBJ      *)
procedure myimagesize; external; (*public size name in RTBINOBJ *)
{$L myimage.obj}   (* In File was image.xgf - exported from Raste Master *)
                   (* as Turbo Pascal - PutImage File                    *)
                   (* name used ater clicking save as button             *)

function IntToStr(I: Longint): String;
var
  S: string[11];
begin
  Str(I, S);
  IntToStr := S;
end;

var
 gd,gm : integer;
 size  : ^word;
 image : ^pointer; (*can be pointer to anything as well*)

begin
 gd:=EGA;
 gm:=EGAHI;
 initgraph(gd,gm,'c:\tp7\bgi');
 image:=@myimage;
 putimage(150,10,image^,normalput);
 (* or type cast to pointer without requiring a seperate variable *)
 putimage(10,10,POINTER(@myimage)^,normalput);

 size:=@myimagesize;
 outtextxy(10,40,'BINOBJ is '+IntToStr(size^)+' bytes');
 (* or type cast using WORD(POINTER(@myimagesize)^)  *)
 outtextxy(10,60,'BINOBJ is '+IntToStr(WORD(POINTER(@myimagesize)^))+' bytes');

 readln;
 closegraph;
end.