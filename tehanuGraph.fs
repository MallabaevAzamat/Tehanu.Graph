// Дополнительные сведения о F# см. на http://fsharp.org
// Дополнительную справку см. в проекте "Учебник по F#".

open Tehanu.FSharp
open Tehanu.Core
open QuickGraph
open QuickGraph.Collections
open QuickGraph.Graphviz
open QuickGraph.Graphviz.Dot

type Tree with
    member this.ToGraph(graph: AdjacencyGraph<string, Edge<string>>, parent: string, count_joints: int): int =
        match this with
        | Atom a ->
            ignore <| graph.AddVertex(a)
            ignore <| graph.AddEdge(new Edge<string>(parent, a))
            count_joints
        | Pair (l, r) ->
            let name = "parent #" + string count_joints
            ignore <| graph.AddVertex(name)
            ignore <| graph.AddEdge(new Edge<string>(parent, name))
            (!r).ToGraph(graph, name, (!l).ToGraph(graph, name, count_joints + 1))
        | Error e ->
            let name = "error: " + e
            ignore <| graph.AddVertex(name)
            ignore <| graph.AddEdge(new Edge<string>(parent, name))
            count_joints
    member this.ToGraph(): AdjacencyGraph<string, Edge<string>> =
        let uG = new AdjacencyGraph<string, Edge<string>>()
        ignore <| uG.AddVertex("root")
        ignore <| this.ToGraph(uG, "root", 0)
        uG

    member this.ToDot(): string =
        let g = this.ToGraph()

        let s = new QuickGraph.Algorithms.TopologicalSort.TopologicalSortAlgorithm<string, Edge<string>>(g)
        s.Compute();

        let a = new GraphvizAlgorithm<string, Edge<string>>(g)
        let f = fun (s:obj) (e:FormatVertexEventArgs<string>) ->
            e.VertexFormatter.Label <- if (e.Vertex.StartsWith("parent #")) then "" else e.Vertex
            e.VertexFormatter.Style <- if (e.Vertex.StartsWith("parent #")) then GraphvizVertexStyle.Solid else if (e.Vertex.StartsWith("``") && e.Vertex.EndsWith("``")) then GraphvizVertexStyle.Diagonals else GraphvizVertexStyle.Bold
        let ff = new FormatVertexEventHandler<string>(f)
        a.add_FormatVertex ff
        let f =
            fun (s:obj) (e:FormatEdgeEventArgs<string, Edge<string>>) ->
                ()//e.EdgeFormatter.Head.Label <- e.Edge.Source
                //e.EdgeFormatter.Tail.Label <- e.Edge.Target

        let ff = new FormatEdgeAction<string, Edge<string>>(f)

        a.add_FormatEdge ff
        a.Generate()

[<EntryPoint>]
let main (argv : string[]) =
    if argv.Length = 2 then
        let modul = Tehanu.FSharp.toTree (argv.[0] + argv.[1]) (System.IO.File.ReadAllText(argv.[1] + ".fs"))
        System.IO.File.WriteAllText(argv.[1] + ".lst", modul.ToString())
        System.IO.File.WriteAllText(argv.[1] + ".dot", modul.ToDot())
    else
        printfn @"
        Please, inputs argumnets with format:
        Tehanu.QuickGraph Unix-path Name
        "
    0 // возвращение целочисленного кода выхода
