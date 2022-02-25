module program
open System
open EEExtensions

type Register = Regist of int

type Phase = | Phase1 | Phase2

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
        static member JmpCode = uint32 0xF000
        static member AluOpcField n = uint32 (n <<< 12)
        static member JmpOpcField n = uint32 (n <<< 9)
        static member JmpInvBit b = uint32 <| if b then (1 <<< 8) else 0
        static member Imm8Bit b = uint32 <| if b then (1 <<< 8) else 0
        static member RaField n = uint32 (n <<< 9)
        static member RbField n = uint32 (n <<< 5)
        static member RbImms5 b imms5 = uint32 <| b <<< 5 + (imms5 &&& 0x1f)
        static member Imm8Field n = uint32 ( n &&& 0xFF)

type Token = 
    | ALUOP of int 
    | JMPOP of int * bool 
    | MEMOP of int
    | Imm of int 
    | Reg of Register
    | Symbol of string
    | Hash
    | LBra
    | RBra
    | ErrorTok of string



type Line = 
    {
        Label: string option
        Word: Result<uint32,string> option
        LineNo: int
        Address: uint32
        Table: SymTable
        Phase: Phase
    }
        static member First =
            {
                Label=None
                Word = None
                LineNo = 1
                Address = 0u
                Table = SymTable.Initial
                Phase = Phase1
            }



type Op = 
    | Imm8 of int 
    | RegOp of (Register * int)
    | SymImm8 of string

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
        ]




/// split the input line, remove white space, return list of strings as tokens
let tokenize (s:string) =
    let s' = s.Replace("#"," # ").Replace(","," , ").Replace("["," [ ").Replace("]"," ] ")
    let sL =
        s'.Split([|"//"|],StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
    let s =
        match sL with
        | [] -> ""
        | line :: comment -> line
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
        | None -> ErrorTok $"'{s} is not recognised")


let makeOp isJmp (op:Op) =
    fun (symTab:SymTable) ->
        match op with
        | Imm8 n when n <= 255 && n >= -128 -> 
            Ok <| IWord.Imm8Field n
        | Imm8 n -> Error $"Immediate operand {n} is not in the allowed range 255 .. -128"
        | SymImm8 s ->
            symTab.Lookup s
            |> Result.map (fun n -> IWord.Imm8Field n + IWord.Imm8Bit (not isJmp))
        | RegOp( r,n) when isJmp ->
            Error $"Jump instruction is not allowed register operand '{r}+{n}'"
        | RegOp(_,n) when n > 15 || n < -16 ->
            Error $"Imms8 number {n} is not in range -16 .. +16"
        | RegOp(Regist r,n) ->
            Ok <| IWord.RbImms5 r n

let makeAluOp n op =
    fun symTab ->
        makeOp false op symTab
        |> Result.map (fun w -> w + IWord.AluOpcField n)



let makeJmpOp n inv op =
    fun symTab ->
        makeOp false op symTab
        |> Result.map (fun w -> 
            w + IWord.JmpOpcField n + IWord.JmpInvBit inv + IWord.JmpCode)
    
            
let rec (|ParseOp|_|) toks =
    match toks with
    | [Symbol s] -> 
        Some (Ok (SymImm8 s))
    | [Hash; Imm n] | [Imm n]-> 
        Some (Ok (Imm8 n))
    | [Reg r; Hash; Imm n] | [Reg r; Imm n] -> 
        Some (Ok (RegOp(r, n)))
    | [Reg r] -> 
        Some (Ok (RegOp(r, 0)))
    | _ -> None // nothing else matches.

// Tokenizes, then parses, a line of text
let rec parse (line: Line) (tokL: Token list) : Line =
    let nl = {line with LineNo = line.LineNo + 1}
    let lineError s = {nl with Word = Some <| Error s}
    let error = List.tryPick (function | ErrorTok s -> Some s | _ -> None) tokL
    match error, tokL with
    | Some s, _ -> lineError $"Line {line.LineNo}: {s}"
    | _, [] -> {nl with Word = None}
    | _, Symbol s :: rest ->
        match line.Label, line.Table.AddSymbol s (int line.Address) with
        | Some s', _ -> 
            lineError  $"Two labels: '{s}' and '{s'} are not allowed on one line"
        | _, Error s -> lineError s
        | _, Ok table ->
            parse {line with Table = table; Label = Some s} rest
    | _, ALUOP n :: ParseOp (Ok op) ->
        {nl with Word = Some (makeAluOp n op line.Table)}
    | _, JMPOP (n,inv) :: ParseOp (Ok op) ->
        {nl with Word = Some (makeJmpOp n inv op line.Table)}
    |_ -> lineError $"Unexpected parse error{tokL}"       

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
    let errorLine (line:Line) (msg:string) =
        $"Line no {line.LineNo}: %s{msg}"

    let getErrors (lines: Line list) =
        lines
        |> List.collect (function | {Word=Some (Error msg )} as line -> 
                                        [errorLine line msg]  
                                  | _ -> [])

    let parseFolder (line,outs) tokL =
            let line = parse line tokL
            line, outs @ [line]

    let tokLines = 
        txtL
        |> List.map tokenize

    let firstPass =
        (Line.First, tokLines)
        ||> List.scan parse

    match getErrors firstPass with
    | [] -> 
        let init = {Line.First with Table = (List.last firstPass).Table}
        ((init,[]), tokLines)
        ||> List.fold parseFolder
        |> (fun (line, outs) ->
            match getErrors outs with
            | [] ->  Ok outs
            | lst -> Error lst)
    | lst -> Error lst

let assembler (path:string) =
    let formatAssembly (lines: Line list) =
        lines
        |> List.map (fun line -> 
            let num = line.Address
            let word = match line.Word with | Some (Ok n) -> n | _ -> 0u
            sprintf "%s" $"%02x{num}: %04x{word}")
        |> String.concat "\n"
    let ext = IO.Path.GetExtension path
    let dir = IO.Path.GetDirectoryName
    match ext with
    | "txt" ->
        IO.File.ReadAllLines path
        |> Array.toList
        |> parseLines
        |> function | Error lst -> 
                        printfn "Assembly Errors in file {path}:"
                        printfn "%s" (String.concat "\n" lst)
                        ()
                    | Ok lst ->
                        printfn "%s" (formatAssembly lst)
                        ()
                        
    | s -> 
        printfn $"EEP1asm is watching {dir}, noted a file extension {ext} only files with extension 'txt' will be assembled"



let watch path =
    let processFile (args: IO.FileSystemEventArgs) =
        printfn "%s" args.FullPath
    let fileSystemWatcher = new IO.FileSystemWatcher()   
    fileSystemWatcher.Path <- path
    fileSystemWatcher.NotifyFilter <- IO.NotifyFilters.LastWrite
    fileSystemWatcher.EnableRaisingEvents <- true
    fileSystemWatcher.IncludeSubdirectories <- true
    fileSystemWatcher.Changed.Add processFile
    fileSystemWatcher.Created.Add processFile
    ()
        
 
[<EntryPoint>]
let main argv = 
    let d = IO.Directory.GetCurrentDirectory()
    printfn "%s" d
    let watchDir = d + "\\..\\..\\..\\..\\.."
    let files = IO.Directory.EnumerateFiles watchDir |> Seq.toList
    printfn "%A" files

    printfn "%A" argv
    0





    



