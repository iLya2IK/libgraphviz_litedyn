(******************************************************************************)
(*                            libGraphviz_dynlite                             *)
(*                 free pascal wrapper around Graphviz library                *)
(*                           https://graphviz.org/                            *)
(*                                                                            *)
(* Copyright (c) 2022 Ilya Medvedkov                                          *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the  GNU Lesser General Public License  as published by *)
(* the Free Software Foundation; either version 3 of the License (LGPL v3).   *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.                                          *)
(* See the GNU Lesser General Public License for more details.                *)
(*                                                                            *)
(* A copy of the GNU Lesser General Public License is available on the World  *)
(* Wide Web at <https://www.gnu.org/licenses/lgpl-3.0.html>.                  *)
(*                                                                            *)
(******************************************************************************)

unit libGraphviz_dynlite;

{$mode objfpc}{$H+}

{$packrecords c}

interface

uses dynlibs, SysUtils;

const
{$if defined(UNIX) and not defined(darwin)}
  GVDLL: Array [0..1] of string = ('libcgraph.so', 'libgvc.so');
{$ELSE}
{$ifdef WINDOWS}
  GVDLL: Array [0..1] of string = ('cgraph.dll', 'gvc.dll');
{$endif}
{$endif}


type
  pGVC_t = Pointer;
  pAgraph_t = Pointer;
  pAgnode_t = Pointer;
  pAgedge_t = Pointer;
  pAgsubnode_t = Pointer;
  pAgsym_t = Pointer;

  pAgdisc_t = Pointer;

  IDTYPE = QWord;

  { graph descriptor  }
  Agdesc_s = byte;

  TAgObjKind = (agkGraph, agkNode, agkEdge, agkInEdge, agkOutEdge);

const
  bm_Agdesc_s_directed = $1;   { if edges are asymmetric  }
  bm_Agdesc_s_strict = $2;     { if multi-edges forbidden  }
  bm_Agdesc_s_no_loop = $4;    { if no loops  }
  bm_Agdesc_s_maingraph = $8;  { if this is the top level graph  }
  bm_Agdesc_s_flatlock = $10;  { if sets are flattened into lists in cdt  }
  bm_Agdesc_s_no_write = $20;  { if a temporary subgraph  }
  bm_Agdesc_s_has_attrs = $40; { if string attr tables should be initialized  }
  bm_Agdesc_s_has_cmpnd = $80; { if may contain collapsed nodes  }


//Base Interface
(*  set up a graphviz context - and init graph - retaining old API *)
function gvContext(): pGVC_t;
(* Compute a layout using a specified engine *)
function gvLayout(gvc: pGVC_t; g: pAgraph_t; const engine: string): integer;
(* Clean up graphviz context *)
function gvFreeContext(gvc: pGVC_t): integer;
(* Free memory allocated and pointed to by *result in gvRenderData *)
procedure gvFreeRenderData(Data: pansichar);
(* Render layout in a specified format to a malloc'ed string *)
function gvRenderData(gvc: pGVC_t; g: pAgraph_t; const format: string;
  char: PPAnsiChar; length: PCardinal): integer;
(* Render layout in a specified format to an open FILE *)
function gvRender(gvc: pGVC_t; g: pAgraph_t; const format: string): integer;

(* Render layout in a specified format to a file with the given name *)
function gvRenderFilename(gvc: pGVC_t; g: pAgraph_t; const format: string;
  const filename: string): integer;
(* Clean up layout data structures - layouts are not nestable (yet) *)
function gvFreeLayout(gvc: pGVC_t; g: pAgraph_t): integer;

//graphs
(* Create new graph from string *)
function agmemread(const cp: string): pAgraph_t;
(* Close graph *)
function agclose(g: pAgraph_t): integer;
function agdesc_directed(var a: Agdesc_s): byte;
procedure agdesc_set_directed(var a: Agdesc_s; __directed: byte);
function agdesc_strict(var a: Agdesc_s): byte;
procedure agdesc_set_strict(var a: Agdesc_s; __strict: byte);
function agdesc_no_loop(var a: Agdesc_s): byte;
procedure agdesc_set_no_loop(var a: Agdesc_s; __no_loop: byte);
function agdesc_maingraph(var a: Agdesc_s): byte;
procedure agdesc_set_maingraph(var a: Agdesc_s; __maingraph: byte);
function agdesc_flatlock(var a: Agdesc_s): byte;
procedure agdesc_set_flatlock(var a: Agdesc_s; __flatlock: byte);
function agdesc_no_write(var a: Agdesc_s): byte;
procedure agdesc_set_no_write(var a: Agdesc_s; __no_write: byte);
function agdesc_has_attrs(var a: Agdesc_s): byte;
procedure agdesc_set_has_attrs(var a: Agdesc_s; __has_attrs: byte);
function agdesc_has_cmpnd(var a: Agdesc_s): byte;
procedure agdesc_set_has_cmpnd(var a: Agdesc_s; __has_cmpnd: byte);
(* NB agdisc not supported *)
(* Create new epmty graph *)
function agopen(const Name: string; opt: Agdesc_s; adisc: pAgdisc_t): pAgraph_t;

function agnode(g: pAgraph_t; const Name: string; createflag: boolean): pAgnode_t;
function agidnode(g: pAgraph_t; id: IDTYPE; createflag: boolean): pAgnode_t;
function agsubnode(g: pAgraph_t; n: pAgnode_t; createflag: boolean): pAgnode_t;
function agfstnode(g: pAgraph_t): pAgnode_t;
function agnxtnode(g: pAgraph_t; n: pAgnode_t): pAgnode_t;
function aglstnode(g: pAgraph_t): pAgnode_t;
function agprvnode(g: pAgraph_t; n: pAgnode_t): pAgnode_t;

function agsubrep(g: pAgraph_t; n: pAgnode_t): pAgsubnode_t;
function agnodebefore(u, v: pAgnode_t): integer;

function agedge(g: pAgraph_t; t: pAgnode_t; h: pAgnode_t;
  const Name: string; createflag: boolean): pAgedge_t;
function agidedge(g: pAgraph_t; t: pAgnode_t; h: pAgnode_t;
  id: IDTYPE; createflag: boolean): pAgedge_t;
function agsubedge(g: pAgraph_t; e: pAgedge_t; createflag: boolean): pAgedge_t;
function agfstin(g: pAgraph_t; n: pAgnode_t): pAgedge_t;
function agnxtin(g: pAgraph_t; e: pAgedge_t): pAgedge_t;
function agfstout(g: pAgraph_t; n: pAgnode_t): pAgedge_t;
function agnxtout(g: pAgraph_t; e: pAgedge_t): pAgedge_t;
function agfstedge(g: pAgraph_t; n: pAgnode_t): pAgedge_t;
function agnxtedge(g: pAgraph_t; e: pAgedge_t; n: pAgnode_t): pAgedge_t;

function agsubg(g: pAgraph_t; const Name: string; createflag: boolean): pAgraph_t;
function agidsubg(g: pAgraph_t; id: IDTYPE; createflag: boolean): pAgraph_t;
function agfstsubg(g: pAgraph_t): pAgraph_t;
function agnxtsubg(subg: pAgraph_t): pAgraph_t;
function agparent(g: pAgraph_t): pAgraph_t;

function agattr(g: pAgraph_t; kind: TAgObjKind; const Name: string;
  const Value: string): pAgsym_t;
function agattrsym(obj: pointer; const Name: string): pAgsym_t;
function agnxtattr(g: pAgraph_t; kind: TAgObjKind; attr: pAgsym_t): pAgsym_t;
function agcopyattr(oldobj: pointer; newobj: pointer): integer;
function agget(obj: pointer; Name: string): string;
function agxget(obj: pointer; sym: pAgsym_t): string;
function agset(obj: pointer; const Name: string; const Value: string): integer;
function agxset(obj: pointer; sym: pAgsym_t; const Value: string): integer;
function agsafeset(obj: pointer; const Name: string; const Value: string;
  const def: string): integer;

function agraphof(obj: pointer): pAgraph_t;
function agroot(obj: pointer): pAgraph_t;
function agcontains(g: pAgraph_t; obj: pointer): integer;
function agnameof(obj: pointer): string;
function agrelabel_node(n: pAgnode_t; const newname: string): integer;
function agdelete(g: pAgraph_t; obj: pointer): integer;
function agdelsubg(g, sub: pAgraph_t): integer;
function agdelnode(g: pAgraph_t; arg_n: pAgnode_t): integer;
function agdeledge(g: pAgraph_t; arg_e: pAgedge_t): integer;
function agobjkind(obj: pointer): TAgObjKind;

function agnnodes(g: pAgraph_t): integer;
function agnedges(g: pAgraph_t): integer;
function agnsubg(g: pAgraph_t): integer;
function agdegree(g: pAgraph_t; n: pAgnode_t; inv, outv: Boolean): integer;
function agcountuniqedges(g: pAgraph_t; n: pAgnode_t; inv, outv: Boolean): integer;

function IsGVIZloaded: boolean;
function InitGVIZInterface(const aLibs : array of String): boolean;
function DestroyGVIZInterface: boolean;

implementation

var
  GVIZloaded: boolean = False;

resourcestring
  SFailedToLoadGraphviz = 'Failed to load Graphviz library';

type
  //Base Interface
  p_gvContext = function(): pGVC_t; cdecl;
  p_gvLayout = function(gvc: pGVC_t; g: pAgraph_t;
    const engine: pansichar): integer; cdecl;
  p_gvFreeContext = function(gvc: pGVC_t): integer; cdecl;
  p_gvFreeRenderData = procedure(Data: pansichar); cdecl;
  p_gvRenderData = function(gvc: pGVC_t; g: pAgraph_t; const format: pansichar;
    char: PPAnsiChar; length: PCardinal): integer; cdecl;
  p_gvRenderFilename = function(gvc: pGVC_t; g: pAgraph_t;
    const format: pansichar; const filename: pansichar)
    : integer;
    cdecl;
  p_gvRender = function(gvc: pGVC_t; g: pAgraph_t;
    const format: pansichar; rt : pointer) : integer; cdecl;
  p_gvFreeLayout = function(gvc: pGVC_t; g: pAgraph_t): integer; cdecl;

  //graphs
  p_agmemread = function(const cp: pansichar): pAgraph_t; cdecl;
  p_agclose = function(g: pAgraph_t): integer; cdecl;
  p_agopen = function(const cp: pansichar; opt: Agdesc_s;
    adisc: pAgdisc_t): pAgraph_t; cdecl;

  //nodes
  p_agnode = function(g: pAgraph_t; Name: pansichar;
    createflag: integer): pAgnode_t; cdecl;
  p_agidnode = function(g: pAgraph_t; id: IDTYPE;
    createflag: integer): pAgnode_t; cdecl;
  p_agsubnode = function(g: pAgraph_t; n: pAgnode_t;
    createflag: integer): pAgnode_t; cdecl;
  p_agfstnode = function(g: pAgraph_t): pAgnode_t; cdecl;
  p_agnxtnode = function(g: pAgraph_t; n: pAgnode_t): pAgnode_t; cdecl;
  p_aglstnode = function(g: pAgraph_t): pAgnode_t; cdecl;
  p_agprvnode = function(g: pAgraph_t; n: pAgnode_t): pAgnode_t; cdecl;

  p_agsubrep = function(g: pAgraph_t; n: pAgnode_t): pAgsubnode_t; cdecl;
  p_agnodebefore = function(u, v: pAgnode_t): integer; cdecl;

  // edges
  p_agedge = function(g: pAgraph_t; t: pAgnode_t; h: pAgnode_t;
    Name: pansichar; icreateflag: integer): pAgedge_t; cdecl;
  p_agidedge = function(g: pAgraph_t; t: pAgnode_t; h: pAgnode_t;
    id: IDTYPE; createflag: integer): pAgedge_t; cdecl;
  p_agsubedge = function(g: pAgraph_t; e: pAgedge_t;
    createflag: integer): pAgedge_t; cdecl;
  p_agfstin = function(g: pAgraph_t; n: pAgnode_t): pAgedge_t; cdecl;
  p_agnxtin = function(g: pAgraph_t; e: pAgedge_t): pAgedge_t; cdecl;
  p_agfstout = function(g: pAgraph_t; n: pAgnode_t): pAgedge_t; cdecl;
  p_agnxtout = function(g: pAgraph_t; e: pAgedge_t): pAgedge_t; cdecl;
  p_agfstedge = function(g: pAgraph_t; n: pAgnode_t): pAgedge_t; cdecl;
  p_agnxtedge = function(g: pAgraph_t; e: pAgedge_t; n: pAgnode_t): pAgedge_t; cdecl;

  // definitions for subgraphs
  p_agsubg = function(g: pAgraph_t; Name: pansichar;
    createflag: integer): pAgraph_t; cdecl;
  p_agidsubg = function(g: pAgraph_t; id: IDTYPE;
    createflag: integer): pAgraph_t; cdecl;
  p_agfstsubg = function(g: pAgraph_t): pAgraph_t; cdecl;
  p_agnxtsubg = function(subg: pAgraph_t): pAgraph_t; cdecl;
  p_agparent = function(g: pAgraph_t): pAgraph_t; cdecl;

  // String attributes
  p_agattr = function(g: pAgraph_t; kind: integer; Name: pansichar;
    const Value: pansichar): pAgsym_t; cdecl;
  p_agattrsym = function(obj: pointer; Name: pansichar): pAgsym_t; cdecl;
  p_agnxtattr = function(g: pAgraph_t; kind: integer;
    attr: pAgsym_t): pAgsym_t; cdecl;
  p_agcopyattr = function(oldobj: pointer; newobj: pointer): integer; cdecl;
  p_agget = function(obj: pointer; Name: pansichar): pansichar; cdecl;
  p_agxget = function(obj: pointer; sym: pAgsym_t): pansichar; cdecl;
  p_agset = function(obj: pointer; Name: pansichar;
    const Value: pansichar): integer; cdecl;
  p_agxset = function(obj: pointer; sym: pAgsym_t;
    const Value: pansichar): integer; cdecl;
  p_agsafeset = function(obj: pointer; Name: pansichar; const Value: pansichar;
    const def: pansichar): integer; cdecl;

  // generic
  p_agraphof = function(obj: pointer): pAgraph_t; cdecl;
  p_agroot = function(obj: pointer): pAgraph_t; cdecl;
  p_agcontains = function(g: pAgraph_t; obj: pointer): integer; cdecl;
  p_agnameof = function(obj: pointer): pansichar; cdecl;
  p_agrelabel_node = function(n: pAgnode_t; newname: pansichar): integer; cdecl;
  p_agdelete = function(g: pAgraph_t; obj: pointer): integer; cdecl;
  p_agdelsubg = function(g, sub: pAgraph_t): integer; cdecl;
  p_agdelnode = function(g: pAgraph_t; arg_n: pAgnode_t): integer; cdecl;
  p_agdeledge = function(g: pAgraph_t; arg_e: pAgedge_t): integer; cdecl;
  p_agobjkind = function(obj: pointer): integer; cdecl;

  // set cardinality
  p_agnnodes = function(g: pAgraph_t): integer; cdecl;
  p_agnedges = function(g: pAgraph_t): integer; cdecl;
  p_agnsubg = function(g: pAgraph_t): integer; cdecl;
  p_agdegree = function(g: pAgraph_t; n: pAgnode_t;
    inv, outv: integer): integer; cdecl;
  p_agcountuniqedges = function(g: pAgraph_t; n: pAgnode_t;
    inv, outv: integer): integer; cdecl;

{
/* Internal (non-string) Attributes */
CGRAPH_API void *agbindrec(void *obj, const char *name, unsigned int recsize,
           int move_to_front);
CGRAPH_API Agrec_t *aggetrec(void *obj, const char *name, int move_to_front);
CGRAPH_API int agdelrec(void *obj, const char *name);
CGRAPH_API void aginit(g : pAgraph_t; int kind, const char *rec_name,
                       int rec_size, int move_to_front);
CGRAPH_API void agclean(g : pAgraph_t; int kind, char *rec_name);
}

var
  _gvContext: p_gvContext = nil;
  _gvLayout: p_gvLayout = nil;
  _gvFreeContext: p_gvFreeContext = nil;
  _gvFreeRenderData: p_gvFreeRenderData = nil;
  _gvRenderData: p_gvRenderData = nil;
  _gvRenderFilename: p_gvRenderFilename = nil;
  _gvRender: p_gvRender = nil;
  _gvFreeLayout: p_gvFreeLayout = nil;

  _agmemread: p_agmemread = nil;
  _agclose: p_agclose = nil;
  _agopen: p_agopen = nil;

  _agnode: p_agnode = nil;
  _agidnode: p_agidnode = nil;
  _agsubnode: p_agsubnode = nil;
  _agfstnode: p_agfstnode = nil;
  _agnxtnode: p_agnxtnode = nil;
  _aglstnode: p_aglstnode = nil;
  _agprvnode: p_agprvnode = nil;

  _agsubrep: p_agsubrep = nil;
  _agnodebefore: p_agnodebefore = nil;

  _agedge: p_agedge = nil;
  _agidedge: p_agidedge = nil;
  _agsubedge: p_agsubedge = nil;
  _agfstin: p_agfstin = nil;
  _agnxtin: p_agnxtin = nil;
  _agfstout: p_agfstout = nil;
  _agnxtout: p_agnxtout = nil;
  _agfstedge: p_agfstedge = nil;
  _agnxtedge: p_agnxtedge = nil;

  _agsubg: p_agsubg = nil;
  _agidsubg: p_agidsubg = nil;
  _agfstsubg: p_agfstsubg = nil;
  _agnxtsubg: p_agnxtsubg = nil;
  _agparent: p_agparent = nil;

  _agattr: p_agattr = nil;
  _agattrsym: p_agattrsym = nil;
  _agnxtattr: p_agnxtattr = nil;
  _agcopyattr: p_agcopyattr = nil;
  _agget: p_agget = nil;
  _agxget: p_agxget = nil;
  _agset: p_agset = nil;
  _agxset: p_agxset = nil;
  _agsafeset: p_agsafeset = nil;

  _agraphof: p_agraphof = nil;
  _agroot: p_agroot = nil;
  _agcontains: p_agcontains = nil;
  _agnameof: p_agnameof = nil;
  _agrelabel_node: p_agrelabel_node = nil;
  _agdelete: p_agdelete = nil;
  _agdelsubg: p_agdelsubg = nil;
  _agdelnode: p_agdelnode = nil;
  _agdeledge: p_agdeledge = nil;
  _agobjkind: p_agobjkind = nil;

  _agnnodes: p_agnnodes = nil;
  _agnedges: p_agnedges = nil;
  _agnsubg: p_agnsubg = nil;
  _agdegree: p_agdegree = nil;
  _agcountuniqedges: p_agcountuniqedges = nil;


const
  bp_Agdesc_s_directed = 0;
  bp_Agdesc_s_strict = 1;
  bp_Agdesc_s_no_loop = 2;
  bp_Agdesc_s_maingraph = 3;
  bp_Agdesc_s_flatlock = 4;
  bp_Agdesc_s_no_write = 5;
  bp_Agdesc_s_has_attrs = 6;
  bp_Agdesc_s_has_cmpnd = 7;

  cAGRAPH = 0;   { can't exceed 2 bits. see Agtag_t. }
  cAGNODE = 1;
  cAGOUTEDGE = 2;
  cAGINEDGE = 3;   { (1 << 1) indicates an edge tag.   }
  cAGEDGE = cAGOUTEDGE; { synonym in object kind args }

  _AGCFLAG: array [boolean] of integer = (0, 1);
  _AGCKIND: array [TAgObjKind] of integer = (cAGRAPH, cAGNODE, cAGEDGE, cAGINEDGE, cAGOUTEDGE);

var
  GVLib: Array of HModule;

{$IFNDEF WINDOWS}
{ Try to load all library versions until you find or run out }
procedure LoadLibUnix(const aLibs : Array of String);
var i : integer;
begin
  for i := 0 to High(aLibs) do
  begin
    GVLib[i] := LoadLibrary(aLibs[i]);
  end;
end;

{$ELSE WINDOWS}
procedure LoadLibsWin(const aLibs : Array of String);
var i : integer;
begin
  for i := 0 to High(aLibs) do
  begin
    GVLib[i] := LoadLibrary(aLibs[i]);
  end;
end;

{$ENDIF WINDOWS}

function gvContext(): pGVC_t;
begin
  if Assigned(_gvContext) then
    Result := _gvContext()
  else
    Result := nil;
end;

function gvLayout(gvc: pGVC_t; g: pAgraph_t; const engine: string): integer;
begin
  if Assigned(_gvLayout) then
    Result := _gvLayout(gvc, g, pansichar(engine))
  else
    Result := -1;
end;

function gvFreeContext(gvc: pGVC_t): integer;
begin
  if Assigned(_gvFreeContext) then
    Result := _gvFreeContext(gvc)
  else
    Result := -1;
end;

procedure gvFreeRenderData(Data: pansichar);
begin
  if Assigned(_gvFreeRenderData) then
    _gvFreeRenderData(Data);
end;

function gvRenderData(gvc: pGVC_t; g: pAgraph_t; const format: string;
  char: PPAnsiChar; length: PCardinal): integer;
begin
  if Assigned(_gvRenderData) then
    Result := _gvRenderData(gvc, g, pansichar(format), char, length)
  else
    Result := -1;
end;

function gvRender(gvc: pGVC_t; g: pAgraph_t; const format: string): integer;
begin
  if Assigned(_gvRender) then
    Result := _gvRender(gvc, g, pansichar(format), nil)
  else
    Result := -1;
end;

function gvRenderFilename(gvc: pGVC_t; g: pAgraph_t; const format: string;
  const filename: string): integer;
begin
  if Assigned(_gvRenderFilename) then
    Result := _gvRenderFilename(gvc, g, pansichar(format), pansichar(filename))
  else
    Result := -1;
end;

function gvFreeLayout(gvc: pGVC_t; g: pAgraph_t): integer;
begin
  if Assigned(_gvFreeLayout) then
    Result := _gvFreeLayout(gvc, g)
  else
    Result := -1;
end;

function agmemread(const cp: string): pAgraph_t;
begin
  if Assigned(_agmemread) then
    Result := _agmemread(pansichar(cp))
  else
    Result := nil;
end;

function agopen(const Name: string; opt: Agdesc_s; adisc: pAgdisc_t): pAgraph_t;
begin
  if Assigned(_agopen) then
    Result := _agopen(pansichar(Name), opt, adisc)
  else
    Result := nil;
end;

function agnode(g: pAgraph_t; const Name: string; createflag: boolean): pAgnode_t;
begin
  if Assigned(_agnode) then
    Result := _agnode(g, pansichar(Name), _AGCFLAG[createflag])
  else
    Result := nil;
end;

function agidnode(g: pAgraph_t; id: IDTYPE; createflag: boolean): pAgnode_t;
begin
  if Assigned(_agidnode) then
    Result := _agidnode(g, id, _AGCFLAG[createflag])
  else
    Result := nil;
end;

function agsubnode(g: pAgraph_t; n: pAgnode_t; createflag: boolean): pAgnode_t;
begin
  if Assigned(_agsubnode) then
    Result := _agsubnode(g, n, _AGCFLAG[createflag])
  else
    Result := nil;
end;

function agfstnode(g: pAgraph_t): pAgnode_t;
begin
  if Assigned(_agfstnode) then
    Result := _agfstnode(g)
  else
    Result := nil;
end;

function agnxtnode(g: pAgraph_t; n: pAgnode_t): pAgnode_t;
begin
  if Assigned(_agnxtnode) then
    Result := _agnxtnode(g, n)
  else
    Result := nil;
end;

function aglstnode(g: pAgraph_t): pAgnode_t;
begin
  if Assigned(_aglstnode) then
    Result := _aglstnode(g)
  else
    Result := nil;
end;

function agprvnode(g: pAgraph_t; n: pAgnode_t): pAgnode_t;
begin
  if Assigned(_agprvnode) then
    Result := _agprvnode(g, n)
  else
    Result := nil;
end;

function agsubrep(g: pAgraph_t; n: pAgnode_t): pAgsubnode_t;
begin
  if Assigned(_agsubrep) then
    Result := _agsubrep(g, n)
  else
    Result := nil;
end;

function agnodebefore(u, v: pAgnode_t): integer;
begin
  if Assigned(_agnodebefore) then
    Result := _agnodebefore(u, v)
  else
    Result := 0;
end;

function agclose(g: pAgraph_t): integer;
begin
  if Assigned(_agclose) then
    Result := _agclose(g)
  else
    Result := -1;
end;

function agedge(g: pAgraph_t; t: pAgnode_t; h: pAgnode_t; const Name: string;
  createflag: boolean): pAgedge_t;
begin
  if Assigned(_agedge) then
    Result := _agedge(g, t, h, pansichar(Name), _AGCFLAG[createflag])
  else
    Result := nil;
end;

function agidedge(g: pAgraph_t; t: pAgnode_t; h: pAgnode_t; id: IDTYPE;
  createflag: boolean): pAgedge_t;
begin
  if Assigned(_agidedge) then
    Result := _agidedge(g, t, h, id, _AGCFLAG[createflag])
  else
    Result := nil;
end;

function agsubedge(g: pAgraph_t; e: pAgedge_t; createflag: boolean): pAgedge_t;
begin
  if Assigned(_agsubedge) then
    Result := _agsubedge(g, e, _AGCFLAG[createflag])
  else
    Result := nil;
end;

function agfstin(g: pAgraph_t; n: pAgnode_t): pAgedge_t;
begin
  if Assigned(_agfstin) then
    Result := _agfstin(g, n)
  else
    Result := nil;
end;

function agnxtin(g: pAgraph_t; e: pAgedge_t): pAgedge_t;
begin
  if Assigned(_agnxtin) then
    Result := _agnxtin(g, e)
  else
    Result := nil;
end;

function agfstout(g: pAgraph_t; n: pAgnode_t): pAgedge_t;
begin
  if Assigned(_agfstout) then
    Result := _agfstout(g, n)
  else
    Result := nil;
end;

function agnxtout(g: pAgraph_t; e: pAgedge_t): pAgedge_t;
begin
  if Assigned(_agnxtout) then
    Result := _agnxtout(g, e)
  else
    Result := nil;
end;

function agfstedge(g: pAgraph_t; n: pAgnode_t): pAgedge_t;
begin
  if Assigned(_agfstedge) then
    Result := _agfstedge(g, n)
  else
    Result := nil;
end;

function agnxtedge(g: pAgraph_t; e: pAgedge_t; n: pAgnode_t): pAgedge_t;
begin
  if Assigned(_agnxtedge) then
    Result := _agnxtedge(g, e, n)
  else
    Result := nil;
end;

function agsubg(g: pAgraph_t; const Name: string; createflag: boolean): pAgraph_t;
begin
  if Assigned(_agsubg) then
    Result := _agsubg(g, pansichar(Name), _AGCFLAG[createflag])
  else
    Result := nil;
end;

function agidsubg(g: pAgraph_t; id: IDTYPE; createflag: boolean): pAgraph_t;
begin
  if Assigned(_agidsubg) then
    Result := _agidsubg(g, id, _AGCFLAG[createflag])
  else
    Result := nil;
end;

function agfstsubg(g: pAgraph_t): pAgraph_t;
begin
  if Assigned(_agfstsubg) then
    Result := _agfstsubg(g)
  else
    Result := nil;
end;

function agnxtsubg(subg: pAgraph_t): pAgraph_t;
begin
  if Assigned(_agnxtsubg) then
    Result := _agnxtsubg(subg)
  else
    Result := nil;
end;

function agparent(g: pAgraph_t): pAgraph_t;
begin
  if Assigned(_agparent) then
    Result := _agparent(g)
  else
    Result := nil;
end;

function agattr(g: pAgraph_t; kind: TAgObjKind; const Name: string;
  const Value: string): pAgsym_t;
begin
  if Assigned(_agattr) then
    Result := _agattr(g, _AGCKIND[kind], pansichar(Name), pansichar(Value))
  else
    Result := nil;
end;

function agattrsym(obj: pointer; const Name: string): pAgsym_t;
begin
  if Assigned(_agattrsym) then
    Result := _agattrsym(obj, pansichar(Name))
  else
    Result := nil;
end;

function agnxtattr(g: pAgraph_t; kind: TAgObjKind; attr: pAgsym_t): pAgsym_t;
begin
  if Assigned(_agnxtattr) then
    Result := _agnxtattr(g, _AGCKIND[kind], attr)
  else
    Result := nil;
end;

function agcopyattr(oldobj: pointer; newobj: pointer): integer;
begin
  if Assigned(_agcopyattr) then
    Result := _agcopyattr(oldobj, newobj)
  else
    Result := -1;
end;

function agget(obj: pointer; Name: string): string;
begin
  if Assigned(_agget) then
    Result := StrPas(_agget(obj, pansichar(Name)))
  else
    Result := '';
end;

function agxget(obj: pointer; sym: pAgsym_t): string;
begin
  if Assigned(_agxget) then
    Result := StrPas(_agxget(obj, sym))
  else
    Result := '';
end;

function agset(obj: pointer; const Name: string; const Value: string): integer;
begin
  if Assigned(_agset) then
    Result := _agset(obj, pansichar(Name), pansichar(Value))
  else
    Result := -1;
end;

function agxset(obj: pointer; sym: pAgsym_t; const Value: string): integer;
begin
  if Assigned(_agxset) then
    Result := _agxset(obj, sym, pansichar(Value))
  else
    Result := -1;
end;

function agsafeset(obj: pointer; const Name: string; const Value: string;
  const def: string): integer;
begin
  if Assigned(_agsafeset) then
    Result := _agsafeset(obj, pansichar(Name), pansichar(Value), pansichar(def))
  else
    Result := -1;
end;

function agraphof(obj: pointer): pAgraph_t;
begin
  if Assigned(_agraphof) then
    Result := _agraphof(obj)
  else
    Result := nil;
end;

function agroot(obj: pointer): pAgraph_t;
begin
  if Assigned(_agroot) then
    Result := _agroot(obj)
  else
    Result := nil;
end;

function agcontains(g: pAgraph_t; obj: pointer): integer;
begin
  if Assigned(_agcontains) then
    Result := _agcontains(g, obj)
  else
    Result := -1;
end;

function agnameof(obj: pointer): string;
begin
  if Assigned(_agnameof) then
    Result := StrPas(_agnameof(obj))
  else
    Result := '';
end;

function agrelabel_node(n: pAgnode_t; const newname: string): integer;
begin
  if Assigned(_agrelabel_node) then
    Result := _agrelabel_node(n, pansichar(newname))
  else
    Result := -1;
end;

function agdelete(g: pAgraph_t; obj: pointer): integer;
begin
  if Assigned(_agdelete) then
    Result := _agdelete(g, obj)
  else
    Result := -1;
end;

function agdelsubg(g, sub: pAgraph_t): integer;
begin
  if Assigned(_agdelsubg) then
    Result := _agdelsubg(g, sub)
  else
    Result := -1;
end;

function agdelnode(g: pAgraph_t; arg_n: pAgnode_t): integer;
begin
  if Assigned(_agdelnode) then
    Result := _agdelnode(g, arg_n)
  else
    Result := -1;
end;

function agdeledge(g: pAgraph_t; arg_e: pAgedge_t): integer;
begin
  if Assigned(_agdeledge) then
    Result := _agdeledge(g, arg_e)
  else
    Result := -1;
end;

function agobjkind(obj: pointer): TAgObjKind;
begin
  if Assigned(_agobjkind) then
    Result := TAgObjKind(_agobjkind(obj))
  else
    Result := agkInEdge;
end;

function agnnodes(g: pAgraph_t): integer;
begin
  if Assigned(_agnnodes) then
    Result := _agnnodes(g)
  else
    Result := 0;
end;

function agnedges(g: pAgraph_t): integer;
begin
  if Assigned(_agnedges) then
    Result := _agnedges(g)
  else
    Result := 0;
end;

function agnsubg(g: pAgraph_t): integer;
begin
  if Assigned(_agnsubg) then
    Result := _agnsubg(g)
  else
    Result := 0;
end;

function agdegree(g: pAgraph_t; n: pAgnode_t; inv, outv: Boolean): integer;
begin
  if Assigned(_agdegree) then
    Result := _agdegree(g, n, _AGCFLAG[inv], _AGCFLAG[outv])
  else
    Result := -1;
end;

function agcountuniqedges(g: pAgraph_t; n: pAgnode_t; inv, outv: Boolean
  ): integer;
begin
  if Assigned(_agcountuniqedges) then
    Result := _agcountuniqedges(g, n, _AGCFLAG[inv], _AGCFLAG[outv])
  else
    Result := 0;
end;

function IsGVIZloaded: boolean;
begin
  Result := GVIZloaded;
end;

procedure UnloadLibraries;
var i : integer;
begin
  GVIZloaded := False;
  for i := 0 to High(GVLib) do
  if GVLib[i] <> NilHandle then
  begin
    FreeLibrary(GVLib[i]);
    GVLib[i] := NilHandle;
  end;
end;

function LoadLibraries(const aLibs : Array of String): boolean;
var i : integer;
begin
  SetLength(GVLib, Length(aLibs));
  Result := False;
  {$IFDEF WINDOWS}
  LoadLibsWin(aLibs);
  {$ELSE}
  LoadLibUnix(aLibs);
  {$ENDIF}
  for i := 0 to High(aLibs) do
  if GVLib[i] <> NilHandle then
     Result := true;
end;

function GetProcAddr(const module: Array of HModule; const ProcName: string): Pointer;
var i : integer;
begin
  for i := Low(module) to High(module) do
  if module[i] <> NilHandle then
  begin
    Result := GetProcAddress(module[i], PChar(ProcName));
    if Assigned(Result) then Exit;
  end;
end;

procedure LoadGVIZEntryPoints;
begin
  _gvContext := p_gvContext(GetProcAddr(GVLib, 'gvContext'));
  _gvLayout := p_gvLayout(GetProcAddr(GVLib, 'gvLayout'));
  _gvFreeContext := p_gvFreeContext(GetProcAddr(GVLib, 'gvFreeContext'));
  _gvFreeRenderData := p_gvFreeRenderData(GetProcAddr(GVLib, 'gvFreeRenderData'));
  _gvRenderData := p_gvRenderData(GetProcAddr(GVLib, 'gvRenderData'));
  _gvRenderFilename := p_gvRenderFilename(GetProcAddr(GVLib, 'gvRenderFilename'));
  _gvRender := p_gvRender(GetProcAddr(GVLib, 'gvRender'));
  _gvFreeLayout := p_gvFreeLayout(GetProcAddr(GVLib, 'gvFreeLayout'));

  _agmemread := p_agmemread(GetProcAddr(GVLib, 'agmemread'));
  _agclose := p_agclose(GetProcAddr(GVLib, 'agclose'));
  _agopen := p_agopen(GetProcAddr(GVLib, 'agopen'));

  _agnode := p_agnode(GetProcAddr(GVLib, 'agnode'));
  _agidnode := p_agidnode(GetProcAddr(GVLib, 'agidnode'));
  _agsubnode := p_agsubnode(GetProcAddr(GVLib, 'agsubnode'));
  _agfstnode := p_agfstnode(GetProcAddr(GVLib, 'agfstnode'));
  _agnxtnode := p_agnxtnode(GetProcAddr(GVLib, 'agnxtnode'));
  _aglstnode := p_aglstnode(GetProcAddr(GVLib, 'aglstnode'));
  _agprvnode := p_agprvnode(GetProcAddr(GVLib, 'agprvnode'));

  _agsubrep := p_agsubrep(GetProcAddr(GVLib, 'agsubrep'));
  _agnodebefore := p_agnodebefore(GetProcAddr(GVLib, 'agnodebefore'));

  _agedge := p_agedge(GetProcAddr(GVLib, 'agedge'));
  _agidedge := p_agidedge(GetProcAddr(GVLib, 'agidedge'));
  _agsubedge := p_agsubedge(GetProcAddr(GVLib, 'agsubedge'));
  _agfstin := p_agfstin(GetProcAddr(GVLib, 'agfstin'));
  _agnxtin := p_agnxtin(GetProcAddr(GVLib, 'agnxtin'));
  _agfstout := p_agfstout(GetProcAddr(GVLib, 'agfstout'));
  _agnxtout := p_agnxtout(GetProcAddr(GVLib, 'agnxtout'));
  _agfstedge := p_agfstedge(GetProcAddr(GVLib, 'agfstedge'));
  _agnxtedge := p_agnxtedge(GetProcAddr(GVLib, 'agnxtedge'));

  _agsubg := p_agsubg(GetProcAddr(GVLib, 'agsubg'));
  _agidsubg := p_agidsubg(GetProcAddr(GVLib, 'agidsubg'));
  _agfstsubg := p_agfstsubg(GetProcAddr(GVLib, 'agfstsubg'));
  _agnxtsubg := p_agnxtsubg(GetProcAddr(GVLib, 'agnxtsubg'));
  _agparent := p_agparent(GetProcAddr(GVLib, 'agparent'));

  _agattr := p_agattr(GetProcAddr(GVLib, 'agattr'));
  _agattrsym := p_agattrsym(GetProcAddr(GVLib, 'agattrsym'));
  _agnxtattr := p_agnxtattr(GetProcAddr(GVLib, 'agnxtattr'));
  _agcopyattr := p_agcopyattr(GetProcAddr(GVLib, 'agcopyattr'));
  _agget := p_agget(GetProcAddr(GVLib, 'agget'));
  _agxget := p_agxget(GetProcAddr(GVLib, 'agxget'));
  _agset := p_agset(GetProcAddr(GVLib, 'agset'));
  _agxset := p_agxset(GetProcAddr(GVLib, 'agxset'));
  _agsafeset := p_agsafeset(GetProcAddr(GVLib, 'agsafeset'));

  _agraphof := p_agraphof(GetProcAddr(GVLib, 'agraphof'));
  _agroot := p_agroot(GetProcAddr(GVLib, 'agroot'));
  _agcontains := p_agcontains(GetProcAddr(GVLib, 'agcontains'));
  _agnameof := p_agnameof(GetProcAddr(GVLib, 'agnameof'));
  _agrelabel_node := p_agrelabel_node(GetProcAddr(GVLib, 'agrelabel_node'));
  _agdelete := p_agdelete(GetProcAddr(GVLib, 'agdelete'));
  _agdelsubg := p_agdelsubg(GetProcAddr(GVLib, 'agdelsubg'));
  _agdelnode := p_agdelnode(GetProcAddr(GVLib, 'agdelnode'));
  _agdeledge := p_agdeledge(GetProcAddr(GVLib, 'agdeledge'));
  _agobjkind := p_agobjkind(GetProcAddr(GVLib, 'agobjkind'));

  _agnnodes := p_agnnodes(GetProcAddr(GVLib, 'agnnodes'));
  _agnedges := p_agnedges(GetProcAddr(GVLib, 'agnedges'));
  _agnsubg := p_agnsubg(GetProcAddr(GVLib, 'agnsubg'));
  _agdegree := p_agdegree(GetProcAddr(GVLib, 'agdegree'));
  _agcountuniqedges := p_agcountuniqedges(GetProcAddr(GVLib, 'agcountuniqedges'));
end;

procedure ClearGVIZEntryPoints;
begin
  _gvContext := nil;
  _gvLayout := nil;
  _gvFreeContext := nil;
  _gvFreeRenderData := nil;
  _gvRenderData := nil;
  _gvRender := nil;
  _gvRenderFilename := nil;
  _gvFreeLayout := nil;

  _agmemread := nil;
  _agclose := nil;
  _agopen := nil;

  _agnode := nil;
  _agidnode := nil;
  _agsubnode := nil;
  _agfstnode := nil;
  _agnxtnode := nil;
  _aglstnode := nil;
  _agprvnode := nil;

  _agsubrep := nil;
  _agnodebefore := nil;

  _agedge := nil;
  _agidedge := nil;
  _agsubedge := nil;
  _agfstin := nil;
  _agnxtin := nil;
  _agfstout := nil;
  _agnxtout := nil;
  _agfstedge := nil;
  _agnxtedge := nil;

  _agsubg := nil;
  _agidsubg := nil;
  _agfstsubg := nil;
  _agnxtsubg := nil;
  _agparent := nil;

  _agattr := nil;
  _agattrsym := nil;
  _agnxtattr := nil;
  _agcopyattr := nil;
  _agget := nil;
  _agxget := nil;
  _agset := nil;
  _agxset := nil;
  _agsafeset := nil;

  _agraphof := nil;
  _agroot := nil;
  _agcontains := nil;
  _agnameof := nil;
  _agrelabel_node := nil;
  _agdelete := nil;
  _agdelsubg := nil;
  _agdelnode := nil;
  _agdeledge := nil;
  _agobjkind := nil;

  _agnnodes := nil;
  _agnedges := nil;
  _agnsubg := nil;
  _agdegree := nil;
  _agcountuniqedges := nil;

end;

function InitGVIZInterface(const aLibs : array of String): boolean;
begin
  Result := IsGVIZloaded;
  if Result then
    exit;
  Result := LoadLibraries(aLibs);
  if not Result then
  begin
    UnloadLibraries;
    Exit;
  end;
  LoadGVIZEntryPoints;
  GVIZloaded := True;
  Result := True;
end;

function DestroyGVIZInterface: boolean;
begin
  Result := not IsGVIZloaded;
  if Result then
    exit;
  ClearGVIZEntryPoints;
  UnloadLibraries;
  Result := True;
end;

function agdesc_directed(var a: Agdesc_s): byte;
begin
  Result := (a and bm_Agdesc_s_directed) shr bp_Agdesc_s_directed;
end;

procedure agdesc_set_directed(var a: Agdesc_s; __directed: byte);
begin
  a := a or ((__directed shl bp_Agdesc_s_directed) and bm_Agdesc_s_directed);
end;

function agdesc_strict(var a: Agdesc_s): byte;
begin
  Result := (a and bm_Agdesc_s_strict) shr bp_Agdesc_s_strict;
end;

procedure agdesc_set_strict(var a: Agdesc_s; __strict: byte);
begin
  a := a or ((__strict shl bp_Agdesc_s_strict) and bm_Agdesc_s_strict);
end;

function agdesc_no_loop(var a: Agdesc_s): byte;
begin
  Result := (a and bm_Agdesc_s_no_loop) shr bp_Agdesc_s_no_loop;
end;

procedure agdesc_set_no_loop(var a: Agdesc_s; __no_loop: byte);
begin
  a := a or ((__no_loop shl bp_Agdesc_s_no_loop) and bm_Agdesc_s_no_loop);
end;

function agdesc_maingraph(var a: Agdesc_s): byte;
begin
  Result := (a and bm_Agdesc_s_maingraph) shr bp_Agdesc_s_maingraph;
end;

procedure agdesc_set_maingraph(var a: Agdesc_s; __maingraph: byte);
begin
  a := a or ((__maingraph shl bp_Agdesc_s_maingraph) and bm_Agdesc_s_maingraph);
end;

function agdesc_flatlock(var a: Agdesc_s): byte;
begin
  Result := (a and bm_Agdesc_s_flatlock) shr bp_Agdesc_s_flatlock;
end;

procedure agdesc_set_flatlock(var a: Agdesc_s; __flatlock: byte);
begin
  a := a or ((__flatlock shl bp_Agdesc_s_flatlock) and bm_Agdesc_s_flatlock);
end;

function agdesc_no_write(var a: Agdesc_s): byte;
begin
  Result := (a and bm_Agdesc_s_no_write) shr bp_Agdesc_s_no_write;
end;

procedure agdesc_set_no_write(var a: Agdesc_s; __no_write: byte);
begin
  a := a or ((__no_write shl bp_Agdesc_s_no_write) and bm_Agdesc_s_no_write);
end;

function agdesc_has_attrs(var a: Agdesc_s): byte;
begin
  Result := (a and bm_Agdesc_s_has_attrs) shr bp_Agdesc_s_has_attrs;
end;

procedure agdesc_set_has_attrs(var a: Agdesc_s; __has_attrs: byte);
begin
  a := a or ((__has_attrs shl bp_Agdesc_s_has_attrs) and bm_Agdesc_s_has_attrs);
end;

function agdesc_has_cmpnd(var a: Agdesc_s): byte;
begin
  Result := (a and bm_Agdesc_s_has_cmpnd) shr bp_Agdesc_s_has_cmpnd;
end;

procedure agdesc_set_has_cmpnd(var a: Agdesc_s; __has_cmpnd: byte);
begin
  a := a or ((__has_cmpnd shl bp_Agdesc_s_has_cmpnd) and bm_Agdesc_s_has_cmpnd);
end;

end.
