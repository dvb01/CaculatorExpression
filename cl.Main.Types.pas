unit cl.Main.Types;

interface
uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics;

  type
    ColorConvert = class
      Const
      HLSMAX = 240;
      RGBMAX = 255;
     // UNDEFINED = 160;
      class procedure HLStoRGB(H,L,S:integer;var R:integer;var G:integer;var B:integer );static;
      class function  HLSToColor(H,L,S:integer):TColor;static;
      class function  RandomColorLS(L,S:byte):TColor;static;
    end;

implementation



Class procedure ColorConvert.HLStoRGB(H,L,S:integer;var R:integer;var G:integer;var B:integer );
Var
Magic1,Magic2 : single;
      function HueToRGB(n1,n2,hue : single) : single;
      begin
              if (hue < 0) then      hue := hue+HLSMAX;
              if (hue > HLSMAX) then hue:=hue -HLSMAX;

              if      (hue < (HLSMAX/6)) then      result:= ( n1 + (((n2-n1)*hue+(HLSMAX/12))/(HLSMAX/6)) )
              else if (hue < (HLSMAX/2)) then      result:=n2
              else if (hue < ((HLSMAX*2)/3)) then  result:= ( n1 + (((n2-n1)*(((HLSMAX*2)/3)-hue)+(HLSMAX/12))/(HLSMAX/6)))
              else                                 result:= ( n1 );
      end;
begin
      if (S = 0) then
      begin
          B:=round( (L*RGBMAX)/HLSMAX ); R:=B; G:=B;
      end
      else
      begin
          if (L <= (HLSMAX/2)) then Magic2 := (L*(HLSMAX + S) + (HLSMAX/2))/HLSMAX
          else                      Magic2 := L + S - ((L*S)  + (HLSMAX/2))/HLSMAX;

          Magic1 := 2*L-Magic2;

          R := round( (HueToRGB(Magic1,Magic2,H+(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX );
          G := round( (HueToRGB(Magic1,Magic2,H)*RGBMAX + (HLSMAX/2)) / HLSMAX );
          B := round( (HueToRGB(Magic1,Magic2,H-(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX );
      end;

      if R<0 then R:=0; if R>RGBMAX then R:=RGBMAX;
      if G<0 then G:=0; if G>RGBMAX then G:=RGBMAX;
      if B<0 then B:=0; if B>RGBMAX then B:=RGBMAX;
end;

class function ColorConvert.HLSToColor(H,L,S:integer):TColor;
var R,G,B:integer;
begin
    HLStoRGB(H,L,S,R,G,B);
    Result:=RGB(R,G,B);
end;

class function  ColorConvert.RandomColorLS(L,S:byte):TColor;
begin
   Result:=HLSToColor(random(256),L,S);
end;

end.
