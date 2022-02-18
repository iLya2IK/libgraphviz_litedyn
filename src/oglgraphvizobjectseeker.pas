{
 OGLGraphvizObjectSeeker:
   Finding and extracting object data from Graphviz rendering

   Copyright (c) 2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit OGLGraphvizObjectSeeker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLGraphvizWrapper;

{
Plain-text syntax:

  graph scale width height
  node name x y width height label style shape color fillcolor
  edge tail head n x1 y1 .. xn yn [label xl yl] style color
  stop
}

type
  TPTLineKind = (ptlUnknown, ptlGraph, ptlNode, ptlEdge, ptlStop);

  TPTSplineCoord = record
    x, y : Double;
  end;

  TPTPoint = TPTSplineCoord;

  PPTSplineCoordsArray = ^TPTSplineCoord;

  TPTGraph = class;

  { TPTTokenLine }

  TPTTokenLine = class(TStringList)
  private
    FKind : TPTLineKind;
    FLineNumber : Integer;
    function GetFltValue(Index : Integer) : Double;
    function GetIntValue(Index : Integer) : Integer;
    function GetStrValue(Index : Integer) : String;
  public
    constructor Create(aLineNumber : Integer);
    property Kind : TPTLineKind read FKind;
    property LineNumber : Integer read FLineNumber;
    property AsStr[Index : Integer] : String read GetStrValue;
    property AsInt[Index : Integer] : Integer read GetIntValue;
    property AsFloat[Index : Integer] : Double read GetFltValue;
  end;

  TPTRecord = class
  public
    class function CheckComplete(aLine : TPTTokenLine) : Boolean; virtual; abstract;
  end;

  { TPTNode }

  TPTNode = class(TPTRecord)
  private
    FOwner : TPTGraph;
    FX, FY, FW, FH : Double;
    FName, FLabel : String;
    FStyle, FShape : String;
    FPColor, FFColor : String;
    FNextNode : TPTNode;
  public
    class function CheckComplete(aLine : TPTTokenLine) : Boolean; override;

    constructor Create(aLine : TPTTokenLine; aGraph : TPTGraph);
    destructor Destroy; override;
    function IsPtInNode(pt : TPTPoint) : Boolean;
    property NextNode : TPTNode read FNextNode;
    property Name : string read FName;
    property NodeLabel : string read FLabel;
    property Style : string read FStyle;
    property Shape : string read FShape;
    property Color : string read FPColor;
    property FillColor : string read FFColor;
    property X : Double read FX;
    property Y : Double read FY;
    property W : Double read FW;
    property H : Double read FH;
  end;

  { TPTEdge }

  TPTEdge = class(TPTRecord)
  private
    FOwner : TPTGraph;
    FTail, FHead : TPTNode;
    FLabel : String;
    FStyle, FColor : String;
    FlX, FlY : Double;
    FArray : PPTSplineCoordsArray;
    FArrayLen : Integer;
    FNextEdge : TPTEdge;
    function GetLabelPt : TPTPoint;
    function GetSplinePt(index : integer) : TPTPoint;
  public
    class function CheckComplete(aLine : TPTTokenLine) : Boolean; override;

    constructor Create(aLine : TPTTokenLine; aGraph : TPTGraph);
    destructor Destroy; override;
    property NextEdge : TPTEdge read FNextEdge;
    property Tail : TPTNode read FTail;
    property Head : TPTNode read FHead;
    property Count : Integer read FArrayLen;
    property Spline[index : integer] : TPTPoint read GetSplinePt;
    property Style : String read FStyle;
    property Color : String read FColor;
    property EdgeLabel : String read FLabel;
    property LabelX : Double read FlX;
    property LabelY : Double read FlY;
    property LabelPt : TPTPoint read GetLabelPt;
  end;

  { TPTGraph }

  TPTGraph = class(TPTRecord)
  private
    FScale, FW, FH : Double;
    FDotWidth, FDotHeight : integer;
    FDpi : Double;
    FFirstNode, FLastNode : TPTNode;
    FFirstEdge, FLastEdge : TPTEdge;
  public
    class function CheckComplete(aLine : TPTTokenLine) : Boolean; override;

    constructor Create(const aLine : TPTTokenLine; aDpi : Double);
    destructor Destroy; override;
    function FindNodeByName(const aName : string) : TPTNode;
    function FindNodeAtPt(ipt : TPTPoint) : TPTNode;
    function FindNodeAtCanvasPt(pt : TPoint) : TPTNode;
    function CanvasPtToGraph(pt : TPoint) : TPTPoint;
    procedure AddNode(aNode : TPTNode);
    procedure AddEdge(aEdge : TPTEdge);
  end;

  { TPlainTextParser }

  TPlainTextParser = class
  private
    class function DoParseLine(const aLine : String; aLineNumber : Integer
      ) : TPTTokenLine;
  public
    class function ParseStrList(aLines : TStrings; aDpi : Double) : TPTGraph;
    class function ParseStream(aLines : TStream; aDpi : Double) : TPTGraph;
  end;

  { EPlainParser }

  EPlainParser = class(Exception)
  public
    constructor Create(const aMsg : String; aLineNumber : Integer); overload;
    constructor Create(const aMsg : String; aLineNumber, aToken : Integer); overload;
  end;

implementation

const
  esParsingError     = 'Parsing error "%s" at line :%d';
  esParsingExError   = 'Parsing error "%s" at line :%d token # %d';
  esWrongStatement   = 'Parsing error "wrong statement ''%s''" at line :%d';
  esWrongFormat      = 'wrong format';
  esGraphExpected    = 'graph token expected';
  esWrongGraphFormat = 'wrong format for graph';
  esWrongNodeFormat  = 'wrong format for node';
  esWrongEdgeFormat  = 'wrong format for edge';
  esTokenExpected    = 'token expected';
  esFloatExpected    = 'float number expected';
  esIntExpected      = 'integer number expected';

function UTF8CodepointSz(chr : PAnsiChar) : Byte; inline;
begin
  case chr^ of
    #0..#191   : Result := 1;
    #192..#223 : Result := 2;
    #224..#239 : Result := 3;
    #240..#247 : Result := 4;
    else Result := 1;
  end;
end;

{ EPlainParser }

constructor EPlainParser.Create(const aMsg : String; aLineNumber : Integer);
begin
  inherited CreateFmt(esParsingError, [aMsg, aLineNumber]);
end;

constructor EPlainParser.Create(const aMsg : String; aLineNumber,
  aToken : Integer);
begin
  inherited CreateFmt(esParsingExError, [aMsg, aLineNumber, aToken]);
end;

{ TPTTokenLine }

function TPTTokenLine.GetFltValue(Index : Integer) : Double;
begin
  if Index < Count then
  begin
    if not TryStrToFloat(Self[index], Result, TGVContext.GraphvizFormatSettings) then
      raise EPlainParser.Create(esFloatExpected, FLineNumber, Index);
  end else
    raise EPlainParser.Create(esTokenExpected, FLineNumber, Index);
end;

function TPTTokenLine.GetIntValue(Index : Integer) : Integer;
begin
  if Index < Count then
  begin
    if not TryStrToInt(Self[index], Result) then
      raise EPlainParser.Create(esIntExpected, FLineNumber, Index);
  end else
    raise EPlainParser.Create(esTokenExpected, FLineNumber, Index);
end;

function TPTTokenLine.GetStrValue(Index : Integer) : String;
begin
  if Index < Count then
  begin
    Result := Self[index];
  end else
    raise EPlainParser.Create(esTokenExpected, FLineNumber, Index);
end;

constructor TPTTokenLine.Create(aLineNumber : Integer);
begin
  inherited Create;

  FLineNumber := aLineNumber;
end;

{ TPTNode }

class function TPTNode.CheckComplete(aLine : TPTTokenLine) : Boolean;
begin
  Result := aLine.Count >= 11;
end;

constructor TPTNode.Create(aLine : TPTTokenLine; aGraph : TPTGraph);
begin
  FOwner := aGraph;

  FName  := aLine.AsStr[1];
  FX     := aLine.AsFloat[2];
  FY     := aLine.AsFloat[3];
  FW     := aLine.AsFloat[4];
  FH     := aLine.AsFloat[5];
  FLabel := aLine.AsStr[6];
  FStyle := aLine.AsStr[7];
  FShape := aLine.AsStr[8];
  FPColor:= aLine.AsStr[9];
  FFColor:= aLine.AsStr[10];
end;

destructor TPTNode.Destroy;
begin
  if Assigned(FNextNode) then FreeAndNil(FNextNode);
  inherited Destroy;
end;

function TPTNode.IsPtInNode(pt : TPTPoint) : Boolean;
begin
  if (pt.x >= (FX - FW * 0.5)) and (pt.y >= (FY - FH * 0.5)) and
     (pt.x <= (FX + FW * 0.5)) and (pt.y <= (FY + FH * 0.5)) then
  begin
    //todo : do specific calcs for 'shape'
    Result := true;
  end else
    Result := false;
end;

{ TPTGraph }

class function TPTGraph.CheckComplete(aLine : TPTTokenLine) : Boolean;
begin
  Result := aLine.Count >= 4;
end;

constructor TPTGraph.Create(const aLine : TPTTokenLine; aDpi : Double);
begin
  FFirstNode := nil;
  FFirstEdge := nil;
  FLastEdge := nil;
  FLastNode := nil;
  FDpi := aDpi;

  FScale := aLine.AsFloat[1];
  FW     := aLine.AsFloat[2];
  FH     := aLine.AsFloat[3];
  FDotWidth  := Trunc(FW * FDpi);
  FDotHeight := Trunc(FH * FDpi);
end;

destructor TPTGraph.Destroy;
begin
  if Assigned(FFirstNode) then FreeAndNil(FFirstNode);
  if Assigned(FFirstEdge) then FreeAndNil(FFirstEdge);
  inherited Destroy;
end;

function TPTGraph.FindNodeByName(const aName : string) : TPTNode;
begin
  Result := FFirstNode;
  while Assigned(Result) do
  begin
    if SameStr(Result.Name, aName) then
      break;
    Result := Result.FNextNode;
  end;
end;

function TPTGraph.FindNodeAtPt(ipt : TPTPoint) : TPTNode;
begin
  Result := FFirstNode;
  while Assigned(Result) do
  begin
    if Result.IsPtInNode(ipt) then
      break;
    Result := Result.FNextNode;
  end;
end;

function TPTGraph.FindNodeAtCanvasPt(pt : TPoint) : TPTNode;
begin
  Result := FindNodeAtPt(CanvasPtToGraph(pt));
end;

function TPTGraph.CanvasPtToGraph(pt : TPoint) : TPTPoint;
begin
  Result.x := double(pt.X) / FDpi;
  Result.y := double(FDotHeight - pt.Y) / FDpi;
end;

procedure TPTGraph.AddNode(aNode : TPTNode);
begin
  if Assigned(FLastNode) then
  begin
    FLastNode.FNextNode := aNode;
    FLastNode := aNode;
  end else
  begin
    FFirstNode := aNode;
    FLastNode := aNode;
  end;
end;

procedure TPTGraph.AddEdge(aEdge : TPTEdge);
begin
  if Assigned(FLastEdge) then
  begin
    FLastEdge.FNextEdge := aEdge;
    FLastEdge := aEdge;
  end else
  begin
    FFirstEdge := aEdge;
    FLastEdge := aEdge;
  end;
end;

{ TPTEdge }

function TPTEdge.GetSplinePt(index : integer) : TPTPoint;
begin
  Result := FArray[index];
end;

class function TPTEdge.CheckComplete(aLine : TPTTokenLine) : Boolean;
var p, l : integer;
begin
  if aLine.Count > 4 then
  begin
    l := aLine.AsInt[3];
    p := 4 + 2 * l;
    if (aLine.Count - p) > 2 then
      Inc(p, 3);
    Inc(p, 2);
    Result := aLine.Count >= p;
  end else
    Result := false;
end;

function TPTEdge.GetLabelPt : TPTPoint;
begin
  Result.x := FlX;
  Result.y := FlY;
end;

constructor TPTEdge.Create(aLine : TPTTokenLine; aGraph : TPTGraph);
var i, p : integer;
begin
  FOwner := aGraph;

  FTail := aGraph.FindNodeByName(aLine.AsStr[1]);
  FHead := aGraph.FindNodeByName(aLine.AsStr[2]);
  FArrayLen := aLine.AsInt[3];
  p := 4;
  if FArrayLen > 0 then
  begin
    FArray := GetMem(SizeOf(TPTPoint) * FArrayLen);
    for i := 0 to FArrayLen-1 do
    begin
      FArray[i].x := aLine.AsFloat[p];
      FArray[i].y := aLine.AsFloat[p+1];
      Inc(p, 2);
    end;
  end else FArray := nil;
  if (aLine.Count - p) > 2 then
  begin
    FLabel := aLine.AsStr[p];
    FlX := aLine.AsFloat[p+1];
    FlY := aLine.AsFloat[p+2];
    Inc(p, 3);
  end;
  FStyle := aLine.AsStr[p];
  FColor := aLine.AsStr[p+1];
end;

destructor TPTEdge.Destroy;
begin
  if Assigned(FArray) then
    FreeMem(FArray);
  if Assigned(FNextEdge) then FreeAndNil(FNextEdge);
  inherited Destroy;
end;

{ TPlainTextParser }

class function TPlainTextParser.DoParseLine(const aLine : String;
  aLineNumber : Integer) : TPTTokenLine;
var l, len, tp : Integer;
    chr : PAnsiChar;
    Token : PAnsiChar;
    isEsc, InStrExpr, canadd : Boolean;
begin
  Result := TPTTokenLine.Create(aLineNumber);

  try
    len := Length(aLine);
    if len < 4 then
      raise EPlainParser.Create(esWrongFormat, aLineNumber)
    else
    begin
      isEsc := false; InStrExpr := false;
      chr := @(aLine[1]);
      Token := GetMem(len + 1);
      try
        FillByte(Token^, len + 1, 0);
        tp := 0;
        while len > 0 do
        begin
          l := UTF8CodepointSz(chr);

          canadd := true;
          if l = 1 then
          case chr^ of
          ' ':
            if not InStrExpr then
            begin
              Result.Add(StrPas(Token));
              FillByte(Token^, tp, 0);
              tp := 0;
              canadd := false;
            end;
          '"':
            if (not isEsc) and InStrExpr then
            begin
              InStrExpr := false;
              canadd := false;
            end else begin
              InStrExpr := true;
              canadd := isEsc;
            end;
          '\':
            if (not isEsc) and InStrExpr then
            begin
              isEsc := true;
              canadd := false;
            end;
          end;

          if canadd then
          begin
            System.Move(chr^, Token[tp], l);
            Inc(tp, l);
            isEsc := false;
          end;

          Inc(chr, l);
          Dec(len, l);
        end;
        if tp > 0 then
          Result.Add(StrPas(Token));

      finally
        Freemem(Token);
      end;

      if SameStr(Result.AsStr[0], 'edge') then
        Result.FKind := ptlEdge else
      if SameStr(Result.AsStr[0], 'node') then
        Result.FKind := ptlNode else
      if SameStr(Result.AsStr[0], 'graph') then
        Result.FKind := ptlGraph else
      if SameStr(Result.AsStr[0], 'stop') then
        Result.FKind := ptlStop else
        raise EPlainParser.CreateFmt(esWrongStatement, [Result.AsStr[0], aLineNumber]);
    end;
  except
    on E : EPlainParser do
    begin
      if assigned(Result) then FreeAndNil(Result);
      raise;
    end;
  end;
end;

class function TPlainTextParser.ParseStrList(aLines : TStrings; aDpi : Double
  ) : TPTGraph;
var
  i : Integer;
  L : TPTTokenLine;
begin
  Result := nil;
  try
    for i := 0 to aLines.Count-1 do
    begin
      L := DoParseLine(aLines[i], i);
      if Assigned(L) then
      begin
        try
          case L.Kind of
            ptlGraph : begin
              if TPTGraph.CheckComplete(L) then
                Result := TPTGraph.Create(L, aDpi) else
                raise EPlainParser.Create(esWrongGraphFormat, i);
            end;
            ptlNode  : begin
              if Assigned(Result) then
              begin
                if TPTNode.CheckComplete(L) then
                  Result.AddNode(TPTNode.Create(L, Result)) else
                  raise EPlainParser.Create(esWrongNodeFormat, i);
              end else
                raise EPlainParser.Create(esGraphExpected, i);
            end;
            ptlEdge  : begin
              if Assigned(Result) then
              begin
                if TPTEdge.CheckComplete(L) then
                  Result.AddEdge(TPTEdge.Create(L, Result)) else
                  raise EPlainParser.Create(esWrongEdgeFormat, i);
              end else
                raise EPlainParser.Create(esGraphExpected, i);
            end;
            ptlStop  : Break;
          end;
        finally
          L.Free;
        end;
      end;
    end;
  except
    on E : EPlainParser do
    begin
      if assigned(Result) then FreeAndNil(Result);
      raise;
    end;
  end;
end;

class function TPlainTextParser.ParseStream(aLines : TStream; aDpi : Double
  ) : TPTGraph;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromStream(aLines);
    Result :=ParseStrList(SL, aDpi);
  finally
    SL.Free;
  end;
end;

end.

