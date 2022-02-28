module program
open System
open EEExtensions

type Register = Regist of int

type Phase = | Phase1 | Phase2

type Op = 
    | Imm8 of int 
    | RegOp of (Register * int)
    | SymImm8 of string

type SymTable = 
    { Table: Map<string,int>; Phase : Phase}

        member this.Lookup name =
            match this.Phase, Map.tryFind name this.Table with
            | _, Some n -> 
                Ok n
            | Phase1, None -> 
                Ok 0
            | Phase2, None   -> 
                Error $"Can't find symbol '{name}' in symbol table"

        member this.AddSymbol name value =
            if Map.containsKey name this.Table then
                Error $"Duplicate definition of symbol '{name}'"
            else
                Ok {this with Table = Map.add name value this.Table }

        static member Initial = {Table = Map.empty; Phase = Phase1}
    

type IWord = 
    {
        Dat: uint32}
        static member JmpCode = uint32 0xC000
        static member AluOpcField n = uint32 (n <<< 12)
        static member JmpOpcField n = uint32 (n <<< 9)
        static member JmpInvBit b = uint32 <| if b then (1 <<< 8) else 0
        static member Imm8Bit b = uint32 <| if b then (1 <<< 8) else 0
        static member RaField n = uint32 (n <<< 9)
        static member RbField n = uint32 (n <<< 5)
        static member RbImms5 b imms5 = IWord.RbField b + uint32 (imms5 &&& 0x1f)
        static member Imm8Field n = uint32 ( n &&& 0xFF)
        static member MemOp n = uint32 0x8000u + (uint32 n <<< 12)

type Token = 
    | ALUOP of int 
    | JMPOP of int * bool 
    | MEMOP of int
    | DCW
    | Imm of int 
    | Reg of Register
    | Symbol of string
    | Hash
    | LBra
    | RBra
    | ErrorTok of string
    | Comment of string



type Line = 
    {
        Label: string option
        Word: Result<uint32,string> option
        LineNo: int
        Address: uint32
        Table: SymTable
        Phase: Phase
        Comment: string
    }
        static member First =
            {
                Label=None
                Word = None
                LineNo = 1
                Address = 0u
                Table = SymTable.Initial
                Phase = Phase1
                Comment = ""
            }





let opMap: Map<string,Token> = 
    Map.ofList [
        "R0", Reg (Regist 0)
        "R1", Reg (Regist 1)
        "R2", Reg (Regist 2)
        "R3", Reg (Regist 3)
        "R4", Reg (Regist 4)
        "R5", Reg (Regist 5)
        "R6", Reg (Regist 6)
        "R7", Reg (Regist 7)
        "MOV", ALUOP 0
        "ADD", ALUOP 1
        "SUB", ALUOP 2
        "ADC", ALUOP 3
        "SBC", ALUOP 4
        "AND", ALUOP 5
        "XOR", ALUOP 6
        "LSR", ALUOP 7
        "LDR", MEMOP 0
        "STR", MEMOP 1
        "CMP", ALUOP 13
        "JMP", JMPOP (0,false)
        "EXT", JMPOP (0,true)
        "JNE", JMPOP (1,false)
        "JEQ", JMPOP (1,true)
        "JCS", JMPOP (2,false)
        "JCC", JMPOP (2,true)
        "JMI", JMPOP (3,false)
        "JPL", JMPOP (3,true)
        "JGE", JMPOP (4,false)
        "JLT", JMPOP (4,true)
        "JGT", JMPOP (5,false)
        "JLE", JMPOP (5,true)
        "JHI", JMPOP (6,false)
        "JLS", JMPOP (6,true)
        "JSR", JMPOP (7,false)
        "RET", JMPOP (7,true)
        "#", Hash
        "[", LBra
        "]", RBra
        "DCW", DCW
        ]


let toksToString tokL =
    let tokToS (tok:Token) =
        match tok, Map.tryPick (fun key value -> if value = tok then Some key else None) opMap with
        | _, Some key-> key
        | Symbol s, _ -> s
        | Comment "",_ -> ""
        | Comment comment, _-> "// " + comment
        | Imm n,_ -> $"#{n}"
        | _ -> sprintf "'%A'" tok
    tokL
    |> List.map tokToS
    |> String.concat  " "

/// split the input line, remove white space, return list of strings as tokens
let tokenize (s:string) =
    let s' = s.Replace("#"," # ").Replace(","," , ").Replace("["," [ ").Replace("]"," ] ")
    let sL =
        s'.Split([|"//"|],StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
    let s, comment =
        match sL with
        | [] -> "", Comment ""
        | [commentText] when String.startsWith "//" s' ->
            "", Comment commentText 
            
        | line :: comment -> line, Comment ((String.concat "//" comment).Trim())
    s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map (fun s -> s.ToUpper())
    |> List.filter (fun s -> s <> "," && s <> "")
    |> List.map (fun s -> 
        match Map.tryFind s opMap with
        | Some tok -> tok
        | None when Seq.contains (s.Chars 0) "0123456789-" ->
            try Some(int32 s) with | e -> None
            |> (function | None -> ErrorTok $"can't parse {s} as an integer"                 
                         | Some n -> Imm n)
        | None when (Char.IsLetter (s.Chars 0))-> Symbol s
        | None -> ErrorTok $"'{s}' is not recognised")
    |> (fun toks -> toks @ [comment])


let makeOp isJmp (Regist a) (op:Op) =
    let ra = IWord.RaField a
    fun (symTab:SymTable) ->
        match op with
        | Imm8 n when n <= 255 && n >= -128 -> 
            Ok <| ra + IWord.Imm8Field n + IWord.Imm8Bit (not isJmp)
        | Imm8 n -> Error $"Immediate operand {n} is not in the allowed range 255 .. -128"
        | SymImm8 s ->
            symTab.Lookup s
            |> Result.map (fun n -> ra + IWord.Imm8Field n + IWord.Imm8Bit (not isJmp))
        | RegOp( r,n) when isJmp ->
            Error $"Jump instruction is not allowed register operand '{r}+{n}'"
        | RegOp(_,n) when n > 15 || n < -16 ->
            Error $"Imms5 number {n} is not in range -16 .. +15"
        | RegOp(Regist r,n) ->
            Ok <| ra + IWord.RbImms5 r n

let makeAluOp ra n op =
    fun symTab ->
        makeOp false ra op symTab
        |> Result.map (fun w -> w + IWord.AluOpcField n)

let makeMemOp ra n op =
    fun symTab ->
        makeOp false ra op symTab
        |> Result.map (fun w -> w + IWord.MemOp n)




let makeJmpOp inv ra n op =
    fun symTab ->
        makeOp true ra op symTab
        |> Result.map (fun w -> 
            w + IWord.JmpOpcField n + IWord.JmpInvBit inv + IWord.JmpCode)
    
            
let (|ParseOpInner|_|) toks =
    match toks with
    | Symbol s :: rest -> 
        Some (Ok (SymImm8 s), rest)
    | Hash :: Imm n :: rest| Imm n :: rest -> 
        Some (Ok (Imm8 n), rest)
    | Reg r :: Hash :: Imm n :: rest | Reg r :: Imm n :: rest -> 
        Some (Ok (RegOp(r, n)), rest)
    | Reg r :: rest -> 
        Some (Ok (RegOp(r, 0)), rest)
    | toks -> Some (Error $"'{toksToString toks}' found when operand expected",[]) // nothing else matches.

let (|ParseComment|_|) comments =
    match comments with
    | [ Comment c ] -> Some (Ok c)
    | toks -> Some (Error $"'{toksToString toks}' found when comment or end-of-line expected")

let makeParse (op:Result<Op,string>) (comment: Result<string,string>) =
    match op,comment with
    | Error op, _ -> Some (Error op,"")
    | _, Error comment -> Some (Error comment,"")
    | Ok op, Ok comment -> Some (Ok op,comment)

let (|ParseOp|_|) useBrackets toks =
    match useBrackets, toks with
    | true, LBra :: ParseOpInner (op,RBra :: ParseComment c) -> 
        makeParse op c
    | _, ParseOpInner( op, ParseComment c) -> 
        makeParse op c
    | _ -> 
        failwithf $"What? Can't parse {(useBrackets, toks)}"

// Tokenizes, then parses, a line of text
let rec parseUnlabelled (line: Line) (tokL: Token list) : Line =
    //printfn $"Parsing {line.Address}:'{toksToString tokL}'"
    let nl = {line with LineNo = line.LineNo + 1}
    let wordOf wordGen ra n op =
        match op with
        | Ok op', comment -> 
            {nl with Word = Some (wordGen ra n op' line.Table); Comment = comment}
        | Error s, _->
            {nl with Word = Some (Error s)}
    let lineError s = {nl with Word = Some <| Error s}
    let error = List.tryPick (function | ErrorTok s -> Some s | _ -> None) tokL
    let tokString = toksToString tokL
    match error, tokL with
    | Some s, rest -> 
        lineError $"Line {line.LineNo}: Token error: '{s}'", rest
    | _, [] -> 
        {nl with Word = None}, []
    |_, [Comment s] ->
        {nl with Word = None}, [Comment s]
    | _, ALUOP n :: Reg ra :: ParseOp false (op) ->
        wordOf makeAluOp ra n op,[]
    | _, JMPOP (n,inv) :: ParseOp false (op) ->
        wordOf (makeJmpOp inv) (Regist 0) n op, []
    | _, MEMOP n :: Reg ra :: ParseOp true (op) ->
        wordOf makeMemOp ra n op, []
    | _, DCW :: Imm n :: rest ->
        {nl with Word = Some (Ok (uint32 n))}, []
    | _ when line.Label.IsSome ->
        lineError $"Error in '{tokString}' after symbol '{line.Label.Value}', \
                    perhaps this is a mis-spelled opcode mnemonic?",[]
    | _  -> 
        lineError $"Unexpected parse error: '{tokString}'"   , []
    |> (fun (line, rest) -> 
        match line, rest with
        | {Word = Some (Error msg)}, _ -> 
            line
        | _, [Comment comment] ->
            {line with Comment = comment}
        | _, [] ->
            {line with Comment = ""}
        | _, rest -> 
            {line with Word = Some (Error (toksToString rest))})
    |> (fun line' -> 
            let usesMemory = line.Label = None && line.Word <> None
            {line' with Address = line'.Address + if usesMemory then 1u else 0u})

let parse (line: Line) (tokL: Token list) : Line =
    let lineError s = {line with Word = Some <| Error s; LineNo = line.LineNo + 1}
    match tokL with
    | Symbol s :: tokL' ->
         let table = 
             match line.Phase with
             | Phase1 -> line.Table.AddSymbol s (int line.Address)
             | Phase2 -> Ok line.Table
         match table with
         |  Error _ -> 
             lineError  $"Duplicate label: '{s}'"
         | Ok table' ->
             parseUnlabelled {line with Table = table'; Label = Some s} tokL'
    | _ ->
        parseUnlabelled {line with Label = None} tokL

/// runs the assembler RAPL
let doLoop() =
    let rec parseLine (line:Line) : Unit =
            printf ">>"
            let txt = Console.ReadLine() 
            match String.trim txt with
            | "q" -> 
                ()
            | txt ->
                String.trim txt
                |> tokenize
                |> parse line
                |> (fun line ->
                    match line.Label, line.Word with
                    | lab, Some(Ok w) ->
                        printfn "Label = %A Machine Code: 0x%04x 0b%016B" lab w w
                    | lab, None ->
                        printfn $"no output, Label = {lab}"
                    | _, Some (Error mess) ->
                        printfn $"Error: {mess}"
                    parseLine line)
    parseLine Line.First
 

let parseLines (txtL: string list) =
    let incAddress (line:Line) = {line with Address = line.Address + 1u}
    let errorLine (line:Line) (msg:string) =
        $"Line no {line.LineNo}: %s{msg}"

    let getErrors (lines: Line list) =
        lines
        |> List.collect (function | {Word=Some (Error msg )} as line -> 
                                        [errorLine line msg]  
                                  | _ -> [])

    let parseFolder (line,outs) tokL =
            let line = {parse line tokL with Label = None}
            line, outs @ [line]

    let tokLines = 
        txtL
        |> List.map tokenize

    let firstPass =
        let folder line toks =
            parse {line with Label = None} toks
        (Line.First, tokLines)
        ||> List.scan folder



    match getErrors firstPass with
    | [] -> 
        let init = {Line.First with 
                        Table = (List.last firstPass).Table; 
                        Address = 0u
                        Phase = Phase2}
        ((init,[]), tokLines)
        ||> List.fold parseFolder
        |> (fun (line, outs) ->
            match getErrors outs with
            | [] ->  
                outs
                |> List.filter (fun line -> line.Word <> None)
                |> Ok
            | lst -> Error lst)
    | lst -> Error lst

let assembler (path:string) =
    let formatAssembly (lines: Line list) =
        lines
        |> List.map (fun line -> 
            let num = line.Address 
            let word = match line.Word with | Some (Ok n) -> n | _ -> 0u
            sprintf "%s" $"0x%02x{num} 0x%04x{word}")
    

    let ext = IO.Path.GetExtension path
    let dir = IO.Path.GetDirectoryName path
    match ext.ToUpper() with
    | ".TXT" ->
        IO.File.ReadAllLines path
        |> Array.toList
        |> parseLines
        |> function | Error lst -> 
                        printfn $"Assembly errors in file '{path}':"
                        printfn "%s" (String.concat "\n" lst)
                        ()
                    | Ok lst ->
                        let pathOut = 
                            IO.Path.ChangeExtension(path, "ram")
                        let output = 
                            formatAssembly lst
                            |> List.toArray
                        IO.File.WriteAllLines(pathOut, output)
                        printfn $"Successful assembly of '{path}'"
                        printfn $"{output.Length} lines written to '{pathOut}'"
                        
    | s -> 
        printfn $"EEP1asm is watching {dir}, noted a file extension {ext} \
                    only files with extension 'txt' will be assembled"



let watch (path:string) =
    let dir =
        match IO.Directory.Exists path with
        | true -> path
        | false -> IO.Path.GetDirectoryName path
    let files =
        IO.Directory.EnumerateFiles dir
        |> Seq.toList
        |> List.filter (fun path ->
            (IO.Path.GetExtension path).ToUpper() = ".TXT")
    printfn $"Watching '{dir}'"

    let processFile (args: IO.FileSystemEventArgs) =
        let path = args.FullPath
        let ext = (IO.Path.GetExtension path).ToUpper()
        if ext = ".TXT" then
            System.Threading.Thread.Sleep 50
            assembler args.FullPath
    files
    |> List.iter assembler
    let fileSystemWatcher = new IO.FileSystemWatcher()   
    fileSystemWatcher.Path <- dir
    fileSystemWatcher.NotifyFilter <- IO.NotifyFilters.LastWrite
    fileSystemWatcher.EnableRaisingEvents <- true
    fileSystemWatcher.IncludeSubdirectories <- false
    fileSystemWatcher.Changed.Add processFile
    fileSystemWatcher.Created.Add processFile
    while true do ()
        
 
[<EntryPoint>]
let main argv = 
    match argv with
    | [|pathToWatch|] ->
        watch pathToWatch
    | _ -> watch "."
    0





    



