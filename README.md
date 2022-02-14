libGraphViz_dynlite

It is free pascal bindings and wrapper around cGraph and GVC libraries (Graphviz). Graphviz is open source graph visualization software. Graph visualization is a way of representing structural information as diagrams of abstract graphs and networks. It has important applications in networking, bioinformatics, software engineering, database and web design, machine learning, and in visual interfaces for other technical domains. This wrapper and bindings are written for dynamically linking with libraries cGraph and GVC.

 
### Requirements

* [Free Pascal Compiler](http://freepascal.org)
* [Lazarus IDE](http://www.lazarus.freepascal.org/) (optional)
* [Graphviz dynamic link libraries](https://graphviz.org/download/)


### Additional reading

* [Cgraph Tutorial](https://graphviz.org/pdf/cgraph.pdf)
* [Using Graphviz as a Library](https://graphviz.org/pdf/libguide.pdf)


### Installation

Get the sources and add the *source* directory to the project search path. For the Lazarus development environment, you can install the libgraphviz_ilya2ik.lpk package for your project. For FPC add the *source* directory to the *fpc.cfg* file.


#### Usage example

```pascal

(* Create and draw finite state machine example     *)
(* <https://graphviz.org/Gallery/directed/fsm.html> *)

program simpletest;

uses Classes, OGLGraphvizWrapper;

const cLabel = 'label';

var
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

		// place the graph in a contextual layout to initialize 
		// drawing interfaces using a specific engine (dot)
        Gv.Layout(Cntx, 'dot');		

		// iterating through all nodes and edges of the graph
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

		// draw the graph in an SVG file
        Gv.RenderFilename('svg', 'out3.svg');
		
        // example of drawing the graph into a stream
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

    DestroyGVIZInterface;
  end else begin
    WriteLn('Cant initialize Graphviz');
  end;
  Readln;
end.

```