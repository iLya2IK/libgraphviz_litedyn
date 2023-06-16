{
 OGLGraphvizWrapper:
   Wrapper for Graphviz library

   Copyright (c) 2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit OGLGraphvizWrapper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, libGraphviz_dynlite;

type
  TGVOption = (gvoDirected, gvoStrict, gvoNoLoop,
               gvoMainGraph, gvoFlatLock, gvoNoWrite,
               gvoHasAttrs, gvoHasCmpnd);

  TGVOptions = set of TGVOption;

  TGVAttrId = Pointer;

  TGVId = libGraphviz_dynlite.IDTYPE;

  TGVContext = class;

  IGVGraph = interface;
  IGVCntxGraph = interface;

  IGVAttr = interface(IUnknown)
   ['{bf3f0402-cf04-4bbe-b7db-e32ffdfc920c}']
   function AsString  : String;
   function AsFloat   : Double;
   function AsInteger : Integer;
  end;

  IGVObject = interface(IUnknown)
   ['{b79ff0b0-9f9e-4a1b-b6f9-cca5578e1df3}']
   function Obj : Pointer;
   function Name : String;
   function Kind : TAgObjKind;
   function Root : IGVGraph;
   function GetAttrId(const atrName : String) : TGVAttrId;
   function GetAttr(const atrName : String) : IGVAttr;
   function GetAttrDef(const atrName, def : String) : IGVAttr;
   function GetExAttr(atrId : TGVAttrId) : IGVAttr;
   function GetExAttrDef(atrId : TGVAttrId; const def : String) : IGVAttr;
   function SetAttr(const atrName, value : String) : IGVObject;
   function SetAttrSafe(const atrName, value, def : String) : IGVObject;
   function SetExAttr(atrId : TGVAttrId; const value : String) : IGVObject;
  end;

  IGVNode = interface(IGVObject)
   ['{158909d2-87e5-43e0-901f-2385956da85f}']
   function Node : Pointer;
  end;

  IGVEdge = interface(IGVObject)
   ['{2a98dfc7-0bcb-407c-9720-e21fbc6e65a4}']
   function Edge : Pointer;
  end;

  IGVIterator = interface(IUnknown)
   ['{37b102af-dc9f-4b93-beb2-d701d6c58294}']
   function HasNext : Boolean;
   function Next : IGVObject;
   procedure Reset;
   function Count : Integer;
  end;

  IGVGraph = interface(IGVObject)
   ['{881d8904-aa7e-4d13-9c8d-2838511e5493}']
   function  Graph : Pointer;
   function  Parent : IGVGraph;
   function  Contains(O : IGVObject) : Boolean;

   function  Attr(akind: TAgObjKind; const aName, aValue : String) : TGVAttrId;
   function  GraphAttr(const aName, aValue : String) : TGVAttrId;
   function  NodeAttr(const aName, aValue : String) : TGVAttrId;
   function  EdgeAttr(const aName, aValue : String) : TGVAttrId;

   procedure DeleteObject(aNode : IGVObject);

   function  NumOfNodes : Integer;
   function  FindNode(const aName : String) : IGVNode; overload;
   function  FindNode(aId : TGVId) : IGVNode; overload;
   function  Node(const aName : String) : IGVNode; overload;
   function  Node(aId : TGVId) : IGVNode; overload;
   function  Node : IGVNode; overload;
   function  NodeDegree(N : IGVNode; InEdges, OutEdges : Boolean) : Integer;
   function  NodeUniqEdges(N : IGVNode; InEdges, OutEdges : Boolean) : Integer;
   function  GetNodeIterator : IGVIterator;
   procedure DeleteNode(aNode : IGVNode);

   function  NumOfEdges : Integer;
   function  FindEdge(N1, N2: IGVNode; const aName : String) : IGVEdge; overload;
   function  FindEdge(N1, N2: IGVNode; aId : TGVId) : IGVEdge; overload;
   function  Edge(N1, N2 : IGVNode; const aName : String) : IGVEdge; overload;
   function  Edge(N1, N2 : IGVNode; aId : TGVId) : IGVEdge; overload;
   function  Edge(N1, N2 : IGVNode) : IGVEdge; overload;
   function  GetEdgeIterator(N : IGVNode) : IGVIterator;
   function  GetInEdgeIterator(N : IGVNode) : IGVIterator;
   function  GetOutEdgeIterator(N : IGVNode) : IGVIterator;
   procedure DeleteEdge(aEdge : IGVEdge);

   function  NumOfSubgraphs : Integer;
   function  FindSubgraph(const aName : String) : IGVGraph; overload;
   function  FindSubgraph(aId : TGVId) : IGVGraph; overload;
   function  Subgraph(const aName : String) : IGVGraph; overload;
   function  Subgraph(aId : TGVId) : IGVGraph; overload;
   function  Subgraph() : IGVGraph; overload;
   function  GetSubgraphIterator : IGVIterator;
   procedure DeleteSubgraph(aSub : IGVGraph);

   procedure Close;
  end;

  { TGVAttr }

  TGVAttr = class(TInterfacedObject, IGVAttr)
  private
    FValue : String;
  public
    constructor Create(const aValue : String);
    function AsString  : String;
    function AsFloat   : Double;
    function AsInteger : Integer;
  end;

  { TGVRenderStream }

  TGVRenderStream = class(TCustomMemoryStream)
  public
    destructor Destroy; override;
  end;

  IGVCntxGraph = interface(IGVGraph)
   ['{e8f2d8ec-743b-4b2a-bf99-f58af63f8124}']

   procedure Layout(aCntx: TGVContext; const engine: String);
   procedure RenderFilename(const format, filename : String);
   procedure RenderStream(const format : String; outs : TStream);
   function  RenderMemory(const format : String) : TGVRenderStream;
   procedure Render(const format : String);
   procedure FreeLayout;

   function  Context : TGVContext;
  end;

  { TGVIterator }

  TGVIterator = class(TInterfacedObject, IGVIterator)
  private
    FNext : Pointer;
    FKind : TAgObjKind;
    FOwner : pAgraph_t;
    FPar  : Pointer;
  public
    constructor Create(aKind : TAgObjKind; aOwner, aPar : Pointer);
    function HasNext : Boolean;
    function Next : IGVObject;
    procedure Reset;
    function Count : Integer;
  end;

  { TGVObject }

  TGVObject = class(TInterfacedObject, IGVObject)
  private
    FObj : Pointer;
  public
    constructor Create(aObj : Pointer);

    function Obj : Pointer;
    function Name : String;
    function Kind : TAgObjKind;
    function Root : IGVGraph;
    function GetAttrId(const atrName : String) : TGVAttrId;
    function GetAttr(const atrName : String) : IGVAttr;
    function GetAttrDef(const atrName, def : String) : IGVAttr;
    function GetExAttr(atrId : TGVAttrId) : IGVAttr;
    function GetExAttrDef(atrId : TGVAttrId; const def : String) : IGVAttr;
    function SetAttr(const atrName, value : String) : IGVObject;
    function SetAttrSafe(const atrName, value, def : String) : IGVObject;
    function SetExAttr(atrId : TGVAttrId; const value : String) : IGVObject;
  end;

  { TGVNode }

  TGVNode = class(TGVObject, IGVNode)
  public
    constructor Create(G : IGVGraph; const aName : String;
      createnew : Boolean = true); overload;
    constructor Create(G : IGVGraph; aId : TGVId;
      createnew : Boolean = true); overload;
    function Node : Pointer;
  end;

  { TGVEdge }

  TGVEdge = class(TGVObject, IGVEdge)
  public
    constructor Create(G : IGVGraph; N1, N2 : IGVNode; const aName : String;
      createnew : Boolean = true); overload;
    constructor Create(G : IGVGraph; N1, N2 : IGVNode; aId : TGVId;
      createnew : Boolean = true); overload;
    function Edge : Pointer;
  end;

  { TGVGraph }

  TGVGraph = class(TGVObject, IGVGraph)
  private
    procedure InitFromString(const S : String);
    procedure InternalInit; virtual;
  public
    constructor Create(const aName : String; aOptions : TGVOptions);
    constructor Create(const aDotCode : String);
    constructor Create(aDotCode : TStrings);
    constructor Create(aObj : Pointer);

    function  Graph : Pointer;
    function  Parent : IGVGraph;
    function  Contains(O : IGVObject) : Boolean;

    function  Attr(akind: TAgObjKind; const aName, aValue : String) : TGVAttrId;
    function  GraphAttr(const aName, aValue : String) : TGVAttrId;
    function  NodeAttr(const aName, aValue : String) : TGVAttrId;
    function  EdgeAttr(const aName, aValue : String) : TGVAttrId;

    procedure DeleteObject(aNode : IGVObject);

    function  NumOfNodes : Integer;
    function  FindNode(const aName : String) : IGVNode; overload;
    function  FindNode(aId : TGVId) : IGVNode; overload;
    function  Node(const aName : String) : IGVNode; overload;
    function  Node(aId : TGVId) : IGVNode; overload;
    function  Node : IGVNode; overload;
    function  NodeDegree(N : IGVNode; InEdges, OutEdges : Boolean) : Integer;
    function  NodeUniqEdges(N : IGVNode; InEdges, OutEdges : Boolean) : Integer;
    function  GetNodeIterator : IGVIterator;
    procedure DeleteNode(aNode : IGVNode);

    function  NumOfEdges : Integer;
    function  FindEdge(N1, N2: IGVNode; const aName : String) : IGVEdge; overload;
    function  FindEdge(N1, N2: IGVNode; aId : TGVId) : IGVEdge; overload;
    function  Edge(N1, N2 : IGVNode; const aName : String) : IGVEdge; overload;
    function  Edge(N1, N2 : IGVNode; aId : TGVId) : IGVEdge; overload;
    function  Edge(N1, N2 : IGVNode) : IGVEdge; overload;
    function  GetEdgeIterator(N : IGVNode) : IGVIterator;
    function  GetInEdgeIterator(N : IGVNode) : IGVIterator;
    function  GetOutEdgeIterator(N : IGVNode) : IGVIterator;
    procedure DeleteEdge(aEdge : IGVEdge);

    function  NumOfSubgraphs : Integer;
    function  FindSubgraph(const aName : String) : IGVGraph; overload;
    function  FindSubgraph(aId : TGVId) : IGVGraph; overload;
    function  Subgraph(const aName : String) : IGVGraph; overload;
    function  Subgraph(aId : TGVId) : IGVGraph; overload;
    function  Subgraph() : IGVGraph; overload;
    function  GetSubgraphIterator : IGVIterator;
    procedure DeleteSubgraph(aSub : IGVGraph);

    procedure Close;
  end;

  { TGVCntxGraph }

  TGVCntxGraph = class(TGVGraph, IGVCntxGraph)
  private
    FCntx  : TGVContext;
    procedure InternalInit; override;
  public
    procedure Layout(aCntx: TGVContext; const engine: String);
    procedure RenderFilename(const format, filename : String);
    procedure RenderStream(const format : String; outs : TStream);
    function  RenderMemory(const format : String) : TGVRenderStream;
    procedure Render(const format : String);
    procedure FreeLayout;

    procedure Close;

    function  Context : TGVContext;
  end;

  { TGVContext }

  TGVContext = class
  private
    FContext : pGVC_t;
  public
    constructor Create;
    destructor Destroy; override;

    function NewGraph(const aName : String; aOptions : TGVOptions) : IGVCntxGraph;
    function NewGraph(const aDotCode : String) : IGVCntxGraph;
    function NewGraph(aDotCode : TStrings) : IGVCntxGraph;

    class function GVLibsLoadDefault : Boolean;
    class function GVLibsLoad(const CGraphLibLoc, GVLibLoc : String) : Boolean;
    class function IsGVLibsLoaded : Boolean;
    class function GVLibsUnLoad : Boolean;
    class function GraphvizFormatSettings : TFormatSettings;
  end;

implementation

var vFS : TFormatSettings;

{ TGVAttr }

constructor TGVAttr.Create(const aValue : String);
begin
  FValue := aValue;
end;

function TGVAttr.AsString : String;
begin
  Result := FValue;
end;

function TGVAttr.AsFloat : Double;
begin
  Result := StrToFloat(FValue, TGVContext.GraphvizFormatSettings);
end;

function TGVAttr.AsInteger : Integer;
begin
  Result := StrToInt(FValue);
end;

{ TGVRenderStream }

destructor TGVRenderStream.Destroy;
begin
  if Assigned(Memory) then
    gvFreeRenderData(Memory);
  inherited Destroy;
end;

{ TGVIterator }

constructor TGVIterator.Create(aKind: TAgObjKind; aOwner, aPar: Pointer);
begin
  FOwner := aOwner;
  FPar := aPar;
  FKind := aKind;
  Reset;
end;

function TGVIterator.HasNext: Boolean;
begin
  Result := Assigned(FNext);
end;

function TGVIterator.Next: IGVObject;
begin
  case FKind of
    agkGraph :
      Result := TGVGraph.Create(FNext) as IGVGraph;
    agkNode :
      Result := TGVNode.Create(FNext) as IGVNode;
    agkEdge, agkInEdge, agkOutEdge :
      Result := TGVEdge.Create(FNext) as IGVEdge;
  else
    Result := nil;
  end;

  case FKind of
    agkGraph   : FNext := agnxtsubg(FNext);
    agkNode    : FNext := agnxtnode(FOwner, FNext);
    agkEdge    : FNext := agnxtedge(FOwner, FNext, FPar);
    agkInEdge  : FNext := agnxtin(FOwner, FNext);
    agkOutEdge : FNext := agnxtout(FOwner, FNext);
  else
    FNext := nil;
  end;
end;

procedure TGVIterator.Reset;
begin
  case FKind of
    agkGraph   : FNext := agfstsubg(FOwner);
    agkNode    : FNext := agfstnode(FOwner);
    agkEdge    : FNext := agfstedge(FOwner, FPar);
    agkInEdge  : FNext := agfstin(FOwner, FPar);
    agkOutEdge : FNext := agfstout(FOwner, FPar);
  else
    FNext := nil;
  end;
end;

function TGVIterator.Count: Integer;
begin
  case FKind of
    agkGraph   : Result := agnsubg(FOwner);
    agkNode    : Result := agnnodes(FOwner);
    agkEdge    : Result := -1;
    agkInEdge  : Result := -1;
    agkOutEdge : Result := -1;
  else
    Result := -1;
  end;
end;

{ TGVCntxGraph }

procedure TGVCntxGraph.InternalInit;
begin
  FCntx := nil;
end;

procedure TGVCntxGraph.Layout(aCntx : TGVContext; const engine: String);
begin
  if Assigned(FCntx) and (aCntx <> FCntx) then
    FreeLayout;
  FCntx := aCntx;
  gvLayout(FCntx.FContext, Graph, engine);
end;

procedure TGVCntxGraph.RenderFilename(const format, filename: String);
begin
  if Assigned(FCntx) then
    gvRenderFilename(FCntx.FContext, Graph, format, filename);
end;

procedure TGVCntxGraph.RenderStream(const format: String; outs: TStream);
var
  buf : PAnsiChar = nil;
  sz : Integer = 0;
begin
  if Assigned(FCntx) then begin
    gvRenderData(FCntx.FContext, Graph, format, @buf, @sz);
    outs.WriteBuffer(buf^, sz);
    gvFreeRenderData(buf);
  end;
end;

procedure TGVCntxGraph.Render(const format: String);
begin
  if Assigned(FCntx) then
    gvRender(FCntx.FContext, Graph, format);
end;

function TGVCntxGraph.RenderMemory(const format : String) : TGVRenderStream;
var
  buf : PAnsiChar = nil;
  sz : Integer = 0;
begin
  Result := TGVRenderStream.Create;
  if Assigned(FCntx) then begin
    gvRenderData(FCntx.FContext, Graph, format, @buf, @sz);
    Result.SetPointer(buf, sz);
    Result.Position := 0;
  end;
end;

procedure TGVCntxGraph.FreeLayout;
begin
  if Assigned(FCntx) then
  begin
    gvFreeLayout(FCntx.FContext, Graph);
    FCntx := nil;
  end;
end;

procedure TGVCntxGraph.Close;
begin
  FreeLayout;
  inherited Close;
end;

function TGVCntxGraph.Context: TGVContext;
begin
  Result := FCntx;
end;

{ TGVEdge }

constructor TGVEdge.Create(G: IGVGraph; N1, N2: IGVNode; const aName: String;
  createnew : Boolean);
begin
  FObj := agedge(G.Graph, N1.Node, N2.Node, aName, createnew);
end;

constructor TGVEdge.Create(G: IGVGraph; N1, N2: IGVNode; aId: TGVId;
  createnew : Boolean);
begin
  FObj := agidedge(G.Graph, N1.Node, N2.Node, aId, createnew);
end;

function TGVEdge.Edge: Pointer;
begin
  Result := FObj;
end;

{ TGVNode }

constructor TGVNode.Create(G: IGVGraph; const aName: String; createnew : Boolean);
begin
  FObj := agnode(G.Graph, aName, createnew);
end;

constructor TGVNode.Create(G: IGVGraph; aId: TGVId; createnew : Boolean);
begin
  FObj := agidnode(G.Graph, aId, createnew);
end;

function TGVNode.Node: Pointer;
begin
  Result := FObj;
end;

{ TGVObject }

constructor TGVObject.Create(aObj: Pointer);
begin
  FObj := aObj;
end;

function TGVObject.Obj: Pointer;
begin
  Result := FObj;
end;

function TGVObject.Name: String;
begin
  Result := agnameof(FObj);
end;

function TGVObject.Kind: TAgObjKind;
begin
  Result := agobjkind(FObj);
end;

function TGVObject.Root: IGVGraph;
begin
  Result := TGVGraph.Create(agroot(FObj)) as IGVGraph;
end;

function TGVObject.GetAttrId(const atrName: String): TGVAttrId;
begin
  Result := TGVAttrId(agattrsym(FObj, atrName));
end;

function TGVObject.GetAttr(const atrName: String): IGVAttr;
begin
  Result := TGVAttr.Create(agget(FObj, atrName)) as IGVAttr;
end;

function TGVObject.GetAttrDef(const atrName, def : String) : IGVAttr;
begin
  Result := GetExAttrDef(GetAttrId(atrName), def);
end;

function TGVObject.GetExAttr(atrId: TGVAttrId): IGVAttr;
begin
  Result := TGVAttr.Create(agxget(FObj, pAgsym_t(atrId))) as IGVAttr;
end;

function TGVObject.GetExAttrDef(atrId : TGVAttrId; const def : String) : IGVAttr;
begin
  if Assigned(atrId) then
    Result := GetExAttr(atrId) else
    Result := TGVAttr.Create(def) as IGVAttr;
end;

function TGVObject.SetAttr(const atrName, value: String): IGVObject;
begin
  agset(FObj, atrName, value);
  Result := Self as IGVObject;
end;

function TGVObject.SetAttrSafe(const atrName, value, def : String) : IGVObject;
begin
  agsafeset(FObj, atrName, value, def);
  Result := Self as IGVObject;
end;

function TGVObject.SetExAttr(atrId: TGVAttrId; const value: String): IGVObject;
begin
  agxset(FObj, pAgsym_t(atrId), value);
  Result := Self as IGVObject;
end;

{ TGVGraph }

procedure TGVGraph.InitFromString(const S: String);
begin
  FObj := agmemread(S);

  InternalInit;
end;

procedure TGVGraph.InternalInit;
begin
  // do nothing
end;

constructor TGVGraph.Create(const aName: String; aOptions: TGVOptions);
var
  opts : Agdesc_s;
begin
  opts := 0;
  if gvoDirected in aOptions then opts := opts or bm_Agdesc_s_directed;
  if gvoStrict in aOptions then   opts := opts or bm_Agdesc_s_strict;
  if gvoNoLoop in aOptions then   opts := opts or bm_Agdesc_s_no_loop;
  if gvoMainGraph in aOptions then opts := opts or bm_Agdesc_s_maingraph;
  if gvoFlatLock in aOptions then opts := opts or bm_Agdesc_s_flatlock;
  if gvoNoWrite in aOptions then  opts := opts or bm_Agdesc_s_no_write;
  if gvoHasAttrs in aOptions then opts := opts or bm_Agdesc_s_has_attrs;
  if gvoHasCmpnd in aOptions then opts := opts or bm_Agdesc_s_has_cmpnd;

  FObj := agopen(aName, opts, nil);

  InternalInit;
end;

constructor TGVGraph.Create(const aDotCode: String);
begin
  InitFromString(aDotCode);
end;

constructor TGVGraph.Create(aDotCode: TStrings);
begin
  InitFromString(aDotCode.Text);
end;

constructor TGVGraph.Create(aObj: Pointer);
begin
  FObj:= aObj;
  InternalInit;
end;

function TGVGraph.Attr(akind: TAgObjKind; const aName, aValue: String
  ): TGVAttrId;
begin
  Result := agattr(Graph, akind, aName, aValue);
end;

function TGVGraph.GraphAttr(const aName, aValue: String): TGVAttrId;
begin
  Result := Attr(agkGraph, aName, aValue);
end;

function TGVGraph.NodeAttr(const aName, aValue: String): TGVAttrId;
begin
  Result := Attr(agkNode, aName, aValue);
end;

function TGVGraph.EdgeAttr(const aName, aValue: String): TGVAttrId;
begin
  Result := Attr(agkEdge, aName, aValue);
end;

procedure TGVGraph.DeleteObject(aNode: IGVObject);
begin
  agdelete(Graph, aNode.Obj);
end;

function TGVGraph.NumOfNodes: Integer;
begin
  Result := agnnodes(Graph);
end;

function TGVGraph.FindNode(const aName: String): IGVNode;
begin
  Result := TGVNode.Create(Self, aName, false) as IGVNode;
end;

function TGVGraph.FindNode(aId: TGVId): IGVNode;
begin
  Result := TGVNode.Create(Self, aId, false) as IGVNode;
end;

function TGVGraph.Node(const aName: String): IGVNode;
begin
  Result := TGVNode.Create(Self, aName) as IGVNode;
end;

function TGVGraph.Node(aId: TGVId): IGVNode;
begin
  Result := TGVNode.Create(Self, aId) as IGVNode;
end;

function TGVGraph.Node: IGVNode;
begin
  Result := TGVNode.Create(Self, '') as IGVNode;
end;

function TGVGraph.NodeDegree(N: IGVNode; InEdges, OutEdges: Boolean): Integer;
begin
  Result := agdegree(Graph, N.Node, InEdges, OutEdges);
end;

function TGVGraph.NodeUniqEdges(N: IGVNode; InEdges, OutEdges: Boolean
  ): Integer;
begin
  Result := agcountuniqedges(Graph, N.Node, InEdges, OutEdges);
end;

function TGVGraph.GetNodeIterator : IGVIterator;
begin
  Result := TGVIterator.Create(agkNode, Graph, nil) as IGVIterator;
end;

procedure TGVGraph.DeleteNode(aNode: IGVNode);
begin
  agdelnode(Graph, aNode.Node);
end;

function TGVGraph.NumOfEdges: Integer;
begin
  Result := agnedges(Graph);
end;

function TGVGraph.FindEdge(N1, N2: IGVNode; const aName: String): IGVEdge;
begin
  Result := TGVEdge.Create(Self, N1, N2, aName, false) as IGVEdge;
end;

function TGVGraph.FindEdge(N1, N2: IGVNode; aId: TGVId): IGVEdge;
begin
  Result := TGVEdge.Create(Self, N1, N2, aId, false) as IGVEdge;
end;

function TGVGraph.Edge(N1, N2: IGVNode; const aName: String): IGVEdge;
begin
  Result := TGVEdge.Create(Self, N1, N2, aName) as IGVEdge;
end;

function TGVGraph.Edge(N1, N2: IGVNode; aId: TGVId): IGVEdge;
begin
  Result := TGVEdge.Create(Self, N1, N2, aId) as IGVEdge;
end;

function TGVGraph.Edge(N1, N2: IGVNode): IGVEdge;
begin
  Result := TGVEdge.Create(Self, N1, N2, '') as IGVEdge;
end;

function TGVGraph.GetEdgeIterator(N : IGVNode) : IGVIterator;
begin
  Result := TGVIterator.Create(agkEdge, Graph, N.Node) as IGVIterator;
end;

function TGVGraph.GetInEdgeIterator(N : IGVNode) : IGVIterator;
begin
  Result := TGVIterator.Create(agkInEdge, Graph, N.Node) as IGVIterator;
end;

function TGVGraph.GetOutEdgeIterator(N : IGVNode) : IGVIterator;
begin
  Result := TGVIterator.Create(agkOutEdge, Graph, N.Node) as IGVIterator;
end;

procedure TGVGraph.DeleteEdge(aEdge: IGVEdge);
begin
  agdeledge(Graph, aEdge.Edge);
end;

function TGVGraph.NumOfSubgraphs: Integer;
begin
  Result := agnsubg(Graph);
end;

function TGVGraph.FindSubgraph(const aName: String): IGVGraph;
begin
  Result := TGVGraph.Create(agsubg(Graph, aName, false)) as IGVGraph;
end;

function TGVGraph.FindSubgraph(aId: TGVId): IGVGraph;
begin
  Result := TGVGraph.Create(agidsubg(Graph, aId, false)) as IGVGraph;
end;

function TGVGraph.Subgraph(const aName: String): IGVGraph;
begin
  Result := TGVGraph.Create(agsubg(Graph, aName, true)) as IGVGraph;
end;

function TGVGraph.Subgraph(aId: TGVId): IGVGraph;
begin
  Result := TGVGraph.Create(agidsubg(Graph, aId, true)) as IGVGraph;
end;

function TGVGraph.Subgraph() : IGVGraph;
begin
  Result := Subgraph('');
end;

function TGVGraph.GetSubgraphIterator : IGVIterator;
begin
  Result := TGVIterator.Create(agkGraph, Graph, nil) as IGVIterator;
end;

procedure TGVGraph.DeleteSubgraph(aSub: IGVGraph);
begin
  agdelsubg(Graph, aSub.Graph);
end;

function TGVGraph.Parent: IGVGraph;
begin
  Result := TGVGraph.Create(agparent(Self.Graph)) as IGVGraph;
end;

function TGVGraph.Contains(O: IGVObject): Boolean;
begin
  Result := agcontains(Graph, O.Obj) <> 0;
end;

procedure TGVGraph.Close;
begin
  if Assigned(Graph) then
    agclose(Graph);

  FObj := nil;
end;

function TGVGraph.Graph: Pointer;
begin
  Result := FObj;
end;

{ TGVContext }

constructor TGVContext.Create;
begin
  FContext := gvContext();
end;

destructor TGVContext.Destroy;
begin
  gvFreeContext(FContext);
  inherited Destroy;
end;

function TGVContext.NewGraph(const aName: String; aOptions: TGVOptions
  ): IGVCntxGraph;
begin
  Result := TGVCntxGraph.Create(aName, aOptions) as IGVCntxGraph;
end;

function TGVContext.NewGraph(const aDotCode: String): IGVCntxGraph;
begin
  Result := TGVCntxGraph.Create(aDotCode) as IGVCntxGraph;
end;

function TGVContext.NewGraph(aDotCode: TStrings): IGVCntxGraph;
begin
  Result := TGVCntxGraph.Create(aDotCode) as IGVCntxGraph;
end;

class function TGVContext.GVLibsLoadDefault: Boolean;
begin
  Result := InitGVIZInterface(GVDLL);
end;

class function TGVContext.GVLibsLoad(const CGraphLibLoc, GVLibLoc: String
  ): Boolean;
begin
  Result := InitGVIZInterface([CGraphLibLoc, GVLibLoc]);
end;

class function TGVContext.IsGVLibsLoaded : Boolean;
begin
  Result := IsGVIZloaded;
end;

class function TGVContext.GVLibsUnLoad : Boolean;
begin
  Result := DestroyGVIZInterface;
end;

class function TGVContext.GraphvizFormatSettings : TFormatSettings;
begin
  Result := vFS;
end;

initialization
  vFS := DefaultFormatSettings;
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := #0;

end.

