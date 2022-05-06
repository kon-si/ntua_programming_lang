(* disjoint-set data dtructure from: https://github.com/ayberkt/sml-union-find/blob/master/src/union_find.sml *)

    val elements : int vector ref = ref #[];
    val max = ref 0;

    fun parse file =
        let
        fun readInt input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

        val inStream = TextIO.openIn file
        val N = readInt inStream + 1
        val M = readInt inStream

        fun readInts 0 arr = arr
        | readInts i arr = readInts (i - 1) ((readInt inStream, readInt inStream, readInt inStream) :: arr)
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
                    val x=find(v)
                    val _ =elements:=Vector.update(!elements, elem, v)
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

    fun gas_tank [] = ()
        | gas_tank ((a, b, c) :: arr) =
        if find(a)<>find(b) then (max:=c;union a b;gas_tank arr) else gas_tank arr;


    fun min_fill file = 
        let 
        val input = parse file
        val arr = mergesort(#2(input))
        val N = #1(input)
        in
        (add N; gas_tank arr; print (Int.toString(!max) ^ "\n"))
        end
