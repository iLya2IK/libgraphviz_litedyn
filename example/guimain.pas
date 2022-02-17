unit guimain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, OGLGraphvizWrapper;

type

  { TForm1 }

  TForm1 = class(TForm)
    GroupBox1 : TGroupBox;
    DotFileDescr : TMemo;
    Image1 : TImage;
    ScrollBox1 : TScrollBox;
    ToolBar1 : TToolBar;
    ToolButton1 : TToolButton;
    procedure FormCreate(Sender : TObject);
    procedure FormDeactivate(Sender : TObject);
    procedure ToolButton1Click(Sender : TObject);
  private
    Cntx : TGVContext;
  public

  end;

var
  Form1 : TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ToolButton1Click(Sender : TObject);
var
  Str : TGVRenderStream;
  G : IGVCntxGraph;
  bmp : TBitmap;
begin
  if not TGVContext.IsGVLibsLoaded then Exit;
  bmp := TBitmap.Create;
  try
    G := Cntx.NewGraph(DotFileDescr.Lines);
    G.Layout(Cntx, 'dot');
    Str := G.RenderMemory('bmp');
    try
      G.Close;
      bmp.LoadFromStream(Str);
    finally
      Str.Free;
    end;

    Image1.Picture.Assign(bmp);
    Image1.Width := bmp.Width;
    Image1.Height := bmp.Height;
  finally
    bmp.free;
  end;
end;

procedure TForm1.FormCreate(Sender : TObject);
begin
  if TGVContext.GVLibsLoad then
  begin
    Cntx := TGVContext.Create;
  end else
    Cntx := nil;
end;

procedure TForm1.FormDeactivate(Sender : TObject);
begin
  if Assigned(Cntx) then
    Cntx.Free;
  TGVContext.GVLibsUnLoad;
end;

end.

