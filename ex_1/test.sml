 val L = ref 0;

    fun parse file =
        let
            fun readInt input = 
                Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

            val inStream = TextIO.openIn file
            val N = readInt inStream
            val M = readInt inStream

            fun readInts 0 map = map
                | readInts i map = readInts (i - 1) ((readInt inStream, readInt inStream, readInt inStream) :: map)
        in
            (readInts M [])
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


        fun find parent u =
            let
                val i = u - 1
            in 
                if Array.sub(parent, i) = u then u
                else
                    let
                        val _ = Array.update(parent, i, (find parent (Array.sub(parent, i))))
                    in
                        Array.sub(parent, i)
                    end
            end;

        fun union parent u v =
            let 
                val u_root = find parent u
                val v_root = find parent v
            in 
                if u_root <> u andalso v_root = v then Array.update(parent, (u_root-1), v_root) else Array.update(parent, (v_root-1), u_root) 
            end;

            fun union parent u v =
            let 
                val u_root = find parent u
                val v_root = find parent v
            in 
                if u_root <> u then Array.update(parent, (v-1), u_root)
                else
                    (if v_root <> v then Array.update(parent, (u-1), v_root) else Array.update(parent, (v-1), u_root))
            end;

 fun mst [] = ()
        | mst ((v, u, w) :: map) =
            let 
                 
                val v_root = find parent v;
                val u_root = find parent u;
                val parent =
                    (let
                        fun incr x = x + 1;
                    in
                        Array.tabulate (6, incr)
                    end)
            in
                if (v_root) <> (u_root) then (union parent v u; L := w; mst map) else mst map
            end;


    fun min_fill file = 
        let 
            val map =
                let
                    val input = parse file
                in
                    mergesort input
                end

            
        in
            (mst map; print (Int.toString(!L) ^ "\n"))
        end;


        fun test nil =
            let
                val parent =
                    let
                        fun incr x = x + 1;
                    in
                        Array.tabulate (6, incr)
                    end
            in
                (union parent 5 6; 
                union parent 2 1; 
                union parent 1 5; 
                union parent 4 2; 
                union parent 3 2; 
                if find parent 2 <> find parent 5 
                (* andalso find parent 3 = find parent 5 
                andalso find parent 2 = find parent 5 
                andalso find parent 7 = find parent 2 
                andalso find parent 4 <> find parent 2  *)
                then print "yes\n" else print "no\n")
            end;