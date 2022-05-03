(* disjoint-set data dtructure from: https://github.com/ayberkt/sml-union-find/blob/master/src/union_find.sml *)

local
    val elements : int vector ref = ref #[];
    val L = ref 0;

    fun parse file =
        let
            fun readInt input = 
                Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

            val inStream = TextIO.openIn file
            val N = readInt inStream + 1
            val M = readInt inStream

            fun readInts 0 map = map
                | readInts i map = readInts (i - 1) ((readInt inStream, readInt inStream, readInt inStream) :: map)
        in
            (N, readInts M [])
        end;

    fun mergesort [] = []
        | mergesort [(a, b, c)] = [(a, b, c)]
        | mergesort (x) = 
            let 
                fun halve nil = (nil, nil)
                    | halve [(a, b, c)] = ([(a, b, c)], nil)
                    | halve ((a, b, c)::(d, e, f)::cs) = 
                        let
                            val (x,y) = halve cs
                        in
                            ((a, b, c)::x, (d, e, f)::y)
                        end
                
                val (x, y) = halve x

                fun merge ([], []) = []
                    | merge (fs, []) = fs
                    | merge ([], gs) = gs
                    | merge ((a, b, c)::fs, (d, e, f)::gs) =
                        if (#3(a, b, c) < #3(d, e, f)) then (a, b, c) :: merge(fs, (d, e, f)::gs) else (d, e, f) :: merge ((a, b, c)::fs, gs) 
            in merge (mergesort x, mergesort y)
            end;

    fun repeat x 0 = []
        | repeat x n = x :: repeat x (n-1);

    fun add n =
        let
            val xs = Vector.fromList (repeat ~1 n)
        in
            elements := Vector.concat [xs, !elements]
        end;

    fun find elem =
        let
            val v = Vector.sub (!elements, elem)
        in
            if v < 0 then elem 
            else
                let
                    val x = find(v)
                    val _ = elements := Vector.update(!elements, elem, v)
                in
                    v
                end
        end;

    fun union a b =
        let
            val aRoot = find(a)
            val bRoot = find(b)
            val aCount = Vector.sub (!elements, aRoot)
            val bCount = Vector.sub (!elements, bRoot)
            val newSize = aCount + bCount
        in
            if ~aCount < ~bCount
            then
                let
                    val _ = elements := Vector.update (!elements, aRoot, bRoot)
                    val _ = elements := Vector.update (!elements, bRoot, newSize)
                in
                    ()
                end
            else
                let
                    val _ = elements := Vector.update (!elements, bRoot, aRoot)
                    val _ = elements := Vector.update (!elements, aRoot, newSize)
                in
                    ()
                end
        end;

    fun mst [] = ()
        | mst ((v, u, w) :: map) =
            if find(v) <> find(u) then (union v u; L := w; mst map) else mst map;

in
    fun min_fill file = 
        let 
            val map =
                let
                    val input = parse file
                in
                    (#1(input), mergesort(#2(input)))
                end
        in
            (add (#1(map)); mst (#2(map)); print (Int.toString(!L) ^ "\n"))
        end
end