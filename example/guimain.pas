unit guimain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, OGLGraphvizWrapper, OGLGraphvizObjectSeeker;

type

  { TForm1 }

  TForm1 = class(TForm)
    GroupBox1 : TGroupBox;
    DotFileDescr : TMemo;
    Image1 : TImage;
    Label1 : TLabel;
    Memo1 : TMemo;
    Panel1 : TPanel;
    ScrollBox1 : TScrollBox;
    ToolBar1 : TToolBar;
    ToolButton1 : TToolButton;
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure Image1MouseDown(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer);
    procedure ToolButton1Click(Sender : TObject);
  private
    Cntx : TGVContext;
    PlainG : TPTGraph;
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
  if Assigned(PlainG) then PlainG.Free;
  bmp := TBitmap.Create;
  try
    G := Cntx.NewGraph(DotFileDescr.Lines);
    G.Layout(Cntx, 'dot');

    Str := G.RenderMemory('bmp');
    try
      bmp.LoadFromStream(Str);
    finally
      Str.Free;
    end;

    Str := G.RenderMemory('plain');
    try
      try
        PlainG := TPlainTextParser.ParseStream(Str,
                                               G.GetAttrDef('dpi','96').AsFloat);
      except
        on E : EPlainParser do
        begin
           Memo1.Lines.Add(E.Message);
           PlainG := nil;
        end;
      end;
    finally
      Str.Free;
    end;
    G.Close;

    Image1.Picture.Assign(bmp);
    Image1.Width := bmp.Width;
    Image1.Height := bmp.Height;
  finally
    bmp.free;
  end;
end;

procedure TForm1.FormCreate(Sender : TObject);
begin
  if TGVContext.GVLibsLoadDefault then
  begin
    Cntx := TGVContext.Create;
  end else
    Cntx := nil;
end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
  if Assigned(Cntx) then
    Cntx.Free;
  if Assigned(PlainG) then
    PlainG.Free;

  TGVContext.GVLibsUnLoad;
end;

procedure TForm1.Image1MouseDown(Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer);
var N : TPTNode;
begin
  if Assigned(PlainG) then
  begin
    N := PlainG.FindNodeAtCanvasPt(Point(X, Y));
    Memo1.Lines.BeginUpdate;
    Memo1.Lines.Clear;
    if assigned(N) then
    begin
      Memo1.Lines.Add('Name : ' + N.Name);
      Memo1.Lines.Add('Label : ' + N.NodeLabel);
      Memo1.Lines.Add('Style : ' + N.Style);
      Memo1.Lines.Add('Shape : ' + N.Shape);
      Memo1.Lines.Add('Color : ' + N.Color);
    end;
    Memo1.Lines.EndUpdate;
  end;
end;

end.

