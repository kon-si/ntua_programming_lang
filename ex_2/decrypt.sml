

fun parse file =
        let
            fun read_string input = TextIO.inputLine input
            val instream = TextIO.openIn file
        in
            valOf(read_string instream)
        end



(*Char.ord(#"c"); char to ascii
String.sub(it, 0); traverse string*)
