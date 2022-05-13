local
    fun parse file =
        let
            fun next_String input = (TextIO.inputAll input) 
            val stream = TextIO.openIn file
        in
            explode(next_String stream)
        end

    fun rotate (n, []) = []
        | rotate (n, (c::msg)) =
            let 
                val ascii = Char.ord(c)
            in
                if ascii > 96 andalso ascii < 123 then Char.chr(((ascii-97+n) mod 26)+97)::(rotate (n, msg))
                else if ascii > 64 andalso ascii < 91 then Char.chr(((ascii-65+n) mod 26)+65)::(rotate (n, msg))
                else c::(rotate (n, msg))
            end

    val f = [Math.ln 0.08167, Math.ln 0.01492, Math.ln 0.02782, Math.ln 0.04253, Math.ln 0.12702, Math.ln 0.02228, Math.ln 0.02015, Math.ln 0.06094, Math.ln 0.06966, Math.ln 0.00153, Math.ln 0.00772, Math.ln 0.04025, Math.ln 0.02406, Math.ln 0.06749, Math.ln 0.07507, Math.ln 0.01929, Math.ln 0.00095, Math.ln 0.05987, Math.ln 0.06327, Math.ln 0.09056, Math.ln 0.02758, Math.ln 0.00978, Math.ln 0.02360, Math.ln 0.00150, Math.ln 0.01974, Math.ln 0.00074]
    fun entropy [] = 0.0
        | entropy  msg =
            let
                val f_n = Array.array(26, 0.0)
                fun divide c x = x/c; 

                fun frequency []  = (0.0, f_n)
                    | frequency (c::msg) =
                        let
                            val ascii = Char.ord(c)
                        in
                            if ascii > 96 andalso ascii < 123 then 
                                let
                                    val _ =  Array.update(f_n, (ascii-97), (Array.sub(f_n, ascii-97)+1.0))
                                in
                                    (#1(frequency msg)+1.0, f_n)
                                end
                            else if ascii > 64 andalso ascii < 91 then
                                let
                                    val _ =  Array.update(f_n, (ascii-65), (Array.sub(f_n, ascii-65)+1.0))
                                in
                                    (#1(frequency msg)+1.0, f_n)
                                end
                            else (#1(frequency msg), f_n)
                        end

                val freq = frequency msg
                val count = #1(freq)
                val freq = #2(freq)
                val _ = Array.modify (divide count) freq 

                fun mul arr [] i = arr
                    | mul arr (x::f) i = 
                        let
                            val _ = Array.update(arr, i, (Array.sub(arr, i)*x:real))
                        in
                            mul arr f (i+1)
                        end

                val h_n = mul freq f 0
            in
                ~(Array.foldl op+ 0.0 h_n)
            end
in
    fun decrypt file =
        let
            val msg = parse file

            fun min_N msg 26 = (26, 10000.0)
                | min_N msg i =
                let
                    val decrypted = rotate (i, msg)
                    val en = entropy decrypted
                    val prev = (min_N msg (i+1))
                    val prev_en = #2(prev)
                in 
                    if prev_en < en then prev else (i, en)
                end

            val N = #1(min_N msg 1)
            val msg = rotate(N, msg)

            fun print_arr [] = print "\n"
                | print_arr (c::msg) = 
                    let 
                        val _ = 
                            if c = #"\n" then print "\n"
                            else if c = #"\"" then print "\""
                            else if c = #"\\" then print "\\"
                            else print (Char.toString c)
                    in
                        print_arr msg
                    end
        in
            print_arr msg
        end
end