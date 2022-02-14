{
 simpletest
   Testing program for Graphviz library wrapper

   Copyright (c) 2022 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program simpletest;

uses Classes, libGraphviz_dynlite, OGLGraphvizWrapper;

const cLabel = 'label';

var
  // native
  gvc : pGVC_t = nil;
  G : pAgraph_t = nil;
  N : Array [0..8] of pAgnode_t;
  Ed : pAgedge_t;
  sym : pAgsym_t;

  // wrapper
  Cntx : TGVContext;
  Gv  : IGVCntxGraph;
  Nv : Array [0..8] of IGVNode;
  Ev : IGVEdge;
  Av : TGVAttrId;
  NI, EI : IGVIterator;
  FS : TFileStream;
begin
  if InitGVIZInterface then // initalize gvc and cgraph libs
  begin
    //step 1. drawing a graph using native tools by parsing a dot file.
    gvc := gvContext();

    //uploading the description of the dot file.
    G := agmemread('digraph finite_state_machine {'+
	'rankdir=LR;'+
	'size="8,5";'+
	'node [shape = doublecircle]; 0 3 4 8;'+
	'node [shape = circle];'+
	'0 -> 2 [label = "SS(B)"];'+
	'0 -> 1 [label = "SS(S)"];'+
	'1 -> 3 [label = "S($end)"];'+
	'2 -> 6 [label = "SS(b)"];'+
	'2 -> 5 [label = "SS(a)"];'+
	'2 -> 4 [label = "S(A)"];'+
	'5 -> 7 [label = "S(b)"];'+
	'5 -> 5 [label = "S(a)"];'+
	'6 -> 6 [label = "S(b)"];'+
	'6 -> 5 [label = "S(a)"];'+
	'7 -> 8 [label = "S(b)"];'+
	'7 -> 5 [label = "S(a)"];'+
	'8 -> 6 [label = "S(b)"];'+
	'8 -> 5 [label = "S(a)"];}');
    gvLayout(gvc, G, 'dot');
    gvRenderFilename(gvc, G, 'svg', 'out1.svg');
    gvFreeLayout(gvc, G);
    agclose(G);

    //step 2. we do the same manually.
    //        the graph is constructed by native cgraph functions.
    G := agopen('finite_state_machine', bm_Agdesc_s_directed, nil);
    agattr(G, agkGraph, 'rankdir', 'LR');
    agattr(G, agkGraph, 'size', '8, 5');
    agattr(G, agkNode,  'shape', 'doublecircle');
    N[0] := agnode(G, '0', true);
    N[3] := agnode(G, '3', true);
    N[4] := agnode(G, '4', true);
    N[8] := agnode(G, '8', true);
    agattr(G, agkNode,  'shape', 'circle');
    N[2] := agnode(G, '2', true);
    N[1] := agnode(G, '1', true);
    N[6] := agnode(G, '6', true);
    N[5] := agnode(G, '5', true);
    N[7] := agnode(G, '7', true);

    sym := agattr(g,agkEdge,cLabel,'');
    Ed := agedge(G, N[0], N[2], '', true);
    //Assuming an attribute already exists for some object,
    //its value can be obtained or set using its string name
    agset(Ed, cLabel, 'SS(B)');
    Ed := agedge(G, N[0], N[1], '', true);
    //If an attribute will be referenced often,
    //it is faster to use its descriptor as an index, as shown here
    agxset(Ed, sym, 'SS(S)');
    Ed := agedge(G, N[1], N[3], '', true);
    agxset(Ed, sym, 'S($end)');
    Ed := agedge(G, N[2], N[6], '', true);
    agxset(Ed, sym, 'SS(b)');
    Ed := agedge(G, N[2], N[5], '', true);
    agxset(Ed, sym, 'SS(a)');
    Ed := agedge(G, N[2], N[4], '', true);
    agxset(Ed, sym, 'S(A)');
    Ed := agedge(G, N[5], N[7], '', true);
    agxset(Ed, sym, 'S(b)');
    Ed := agedge(G, N[5], N[5], '', true);
    agxset(Ed, sym, 'S(a)');
    Ed := agedge(G, N[6], N[6], '', true);
    agxset(Ed, sym, 'S(b)');
    Ed := agedge(G, N[6], N[5], '', true);
    agxset(Ed, sym, 'S(a)');
    Ed := agedge(G, N[7], N[8], '', true);
    agxset(Ed, sym, 'S(b)');
    Ed := agedge(G, N[7], N[5], '', true);
    agxset(Ed, sym, 'S(a)');
    Ed := agedge(G, N[8], N[6], '', true);
    agxset(Ed, sym, 'S(b)');
    Ed := agedge(G, N[8], N[5], '', true);
    agxset(Ed, sym, 'S(a)');

    gvLayout(gvc, G, 'dot');
    gvRenderFilename(gvc, G, 'svg', 'out2.svg');
    gvFreeLayout(gvc, G);
    agclose(G);
    gvFreeContext(gvc);

    //step 3. do the same with the wrapper
    Cntx := TGVContext.Create;
    try
      Gv := Cntx.NewGraph('finite_state_machine', [gvoDirected]);
      try
        Gv.GraphAttr('rankdir', 'LR');
        Gv.GraphAttr('size', '8, 5');
        Gv.NodeAttr('shape', 'doublecircle');
        Nv[0] := Gv.Node('0');
        Nv[3] := Gv.Node('3');
        Nv[4] := Gv.Node('4');
        Nv[8] := Gv.Node('8');
        Gv.NodeAttr('shape', 'circle');
        Nv[2] := Gv.Node('2');
        Nv[1] := Gv.Node('1');
        Nv[6] := Gv.Node('6');
        Nv[5] := Gv.Node('5');
        Nv[7] := Gv.Node('7');

        Av := Gv.EdgeAttr(cLabel, '');
        Ev := Gv.Edge(Gv.FindNode('0'), Gv.FindNode('2'));
        Ev.SetExAttr(Av, 'SS(B)');
        Gv.Edge(Nv[0], Nv[1]).SetExAttr(Av, 'SS(S)');
        Gv.Edge(Nv[1], Nv[3]).SetExAttr(Av, 'S($end)');
        Gv.Edge(Nv[2], Nv[6]).SetExAttr(Av, 'SS(b)');
        Gv.Edge(Nv[2], Nv[5]).SetExAttr(Av, 'SS(a)');
        Gv.Edge(Nv[2], Nv[4]).SetExAttr(Av, 'S(A)');
        Gv.Edge(Nv[5], Nv[7]).SetExAttr(Av, 'S(b)');
        Gv.Edge(Nv[5], Nv[5]).SetExAttr(Av, 'S(a)');
        Gv.Edge(Nv[6], Nv[6]).SetExAttr(Av, 'S(b)');
        Gv.Edge(Nv[6], Nv[5]).SetExAttr(Av, 'S(a)');
        Gv.Edge(Nv[7], Nv[8]).SetExAttr(Av, 'S(b)');
        Gv.Edge(Nv[7], Nv[5]).SetExAttr(Av, 'S(a)');
        Gv.Edge(Nv[8], Nv[6]).SetExAttr(Av, 'S(b)');
        Gv.Edge(Nv[8], Nv[5]).SetExAttr(Av, 'S(a)');

        Gv.Layout(Cntx, 'dot');
        Gv.RenderFilename('svg', 'out3.svg');

        NI := Gv.GetNodeIterator;
        while NI.HasNext do
        begin
          Nv[0] := NI.Next as IGVNode;
          WriteLn('Node ', Nv[0].Name);
          EI := Gv.GetEdgeIterator(Nv[0]);
          while EI.HasNext do
          begin
            Ev := EI.Next as IGVEdge;
            WriteLn('  Edge ', Ev.GetExAttr(Av));
          end;
        end;

        //step 3.1. example of drawing a graph into a stream.
        FS := TFileStream.Create('out4.bmp', fmCreate or fmOpenWrite);
        try
          Gv.RenderStream('bmp', FS);
        finally
          FS.Free;
        end;

      finally
        Gv.Close;
      end;
    finally
      Cntx.Free;
    end;


    WriteLn('OK');
    DestroyGVIZInterface;
  end else begin
    WriteLn('Cant initialize Graphviz');
  end;
  Readln;
end.

