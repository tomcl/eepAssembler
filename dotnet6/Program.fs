module program
open System
open EEExtensions

type CPU = EEP0 | EEP1

/// split the input line, remove white space, return list of strings as tokens
let tokenize (s:string) =
    let s' = s.Replace("#"," # ").Replace(","," , ").Replace("["," [ ").Replace("]"," ] ")
    s'.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map (fun s -> s.ToUpper())

/// The b register number field bit offset
let regBFieldOffset cpu =
    if cpu = EEP1 then 5 else 8

/// The a register number field bit offset
let regAFieldOffset cpu = 
    if cpu = EEP1 then 9 else 10 

/// The bit that signifies the immediate form of operand
let immediateOpOffset cpu =
    if cpu = EEP1 then 8 else 12

let eep1RegOps = ["MOV",0; "ADD",0x1000; "SUB",0x2000; "ADC",0x3000; "SBC",0x4000; "AND",0x5000; "XOR",0x6000; 
                    "LSL",0x7000; "LDR", 0x8000; "STR",0xA000]
let eep0RegOps = ["MOV",0; "ADD",0x2000; "SUB",0x4000; "ADC",0x6000; "LDR", 0x8000; "STR",0xA000]


let regOps cpu = 
    match cpu with
    | EEP1 -> eep1RegOps
    | EEP0 -> eep0RegOps
    |> Map.ofList

let eep0jumps = ["JMP",0xC000; "JNE",0xD000; "JCS",0xE000; "JMI",0xF000]
let eep1jumps =
    let makeCode opc n = (opc <<< 9) + (n <<< 8) + 0xC000
    [
        "JMP","XXX"
        "JNE","JEQ"
        "JCS","JCC"
        "JMI","JPL"
        "JGE","JLT"
        "JGT","JLE"
        "JHI","JLS"
        "JSR","RET"
    ]
    |> List.mapi (fun i (xx,nxx) -> [xx,makeCode i 0; nxx, makeCode i 1])
    |> List.concat
    |> Map.ofList

let jumps cpu = if cpu = EEP1 then eep1jumps else Map.ofList eep0jumps
let eep0regs =  ["R0",0; "R1",1; "R2",2; "R3",3]
let eep1regs =  ["R4",4; "R5",5; "R6",6; "R7",7]
let regs cpu = 
    (if cpu = EEP1 then eep0regs @ eep1regs else eep0regs)
    |> Map.ofList


/// match a register (Rx)
let (|RegMatch|_|) cpu ra = Map.tryFind ra (regs cpu)

/// Single bit in position n
let bit n = 1 <<< n




//------------------------------------------------------------------------//
//------------------------ Very simple disassembler-----------------------//
//------------------------------------------------------------------------//

type Field = {
     StartBit: int
     Size: int
     Name: string
}

let makeFields sL =
    let makeF (start, fields) (name,size) = (start+size, fields @ [{Name=name; Size=size; StartBit = start-size}])
    ((15,[]), sL)
    ||> List.fold makeF
    |> snd

    

let printNextField msBitNum (f:Field) : Result<string,string> =
    let fMSBitNum = f.StartBit + f.Size
    let printData f = sprintf $" {f.Name}:I({fMSBitNum}:{f.StartBit})"
    let printDontCares a b = 
        " " + ([a..b] |> List.map (fun _ -> "X") |> String.concat "")
    if msBitNum < fMSBitNum then
        Error $"Can't print {f} from MS Bit = {msBitNum}"
    else
        Ok ((printDontCares msBitNum fMSBitNum) + printData f)
    

let rec printFields (iWord: uint32) (mSBitNum: int) (fL: Field list) =
    match fL with
    | [] when mSBitNum = 0 -> 
        ""
    | [] -> 
        sprintf $"Can't print {fL} with MS bit = {mSBitNum}"
    | f :: fL' -> 
        match printNextField mSBitNum f with
        | Error eMess -> eMess
        | Ok mess -> mess + printFields iWord f.StartBit fL'



let dissAssembleFixed (iWord: uint32) (mSBit: int) ( spec: (string * int) list) =
    spec
    |> makeFields
    |> printFields iWord 15



/// Top-level function to run assembler: assembles EEP0 and EEP1.
/// Recursively calls itself if given command to go to the "other" CPU.
let rec runAssembler (cpu: CPU) =
    let thisCPU = cpu.ToString()
    let switchTo = if cpu = EEP0 then "EEP1" else "EEP0"
    printfn $"{switchTo} - switches to {switchTo} assembler"
    printfn "q - quits"
    printfn $"type lines of {thisCPU} assembler:"
    


   //------------------------------------------------------------------------//
   //------------------------------ Assembler--------------------------------//
   //------------------------------------------------------------------------//


    /// return the code (correctly aligned) for Rb
    let makeRegOp n = n <<< regBFieldOffset cpu

    /// return the code (correctly aligned) for an immediate Op
    /// The caller must check that n is in the correct range for Imm8 or Imms5
    let makeImmOp n = bit (immediateOpOffset cpu) + n
  

    /// Parse a number in hex,binary or decimal
    let parseImm isImms5 (s:string) = 
        let (loLimit,hiLimit) = if isImms5 then (-16,15) else (-128,256)
        try Some(int32 s) with | e -> None
        |> (function | None -> Error $"can't parse {s} as an integer"                 
                     | Some n -> Ok n)
        |> Result.bind (fun n -> 
             if n >= 0 && n < hiLimit then 
                Ok (makeImmOp n) 
             elif n < 0 then
                Ok (makeImmOp (n &&& (-loLimit*2 - 1)))
             else Error $"Invalid Imm8 operand {n} - must be in range {loLimit}..{hiLimit}")


    /// Parse the Rb, #imms8 part (only for EEP1).
    /// b - the register number already passed.
    /// imms5 - the string to parse as an imms5
    let parseRegBAndImms5 b imms5 =
        if cpu <> EEP1 then Error "Detected a register + Imms5 format, this is only valid for EEP1"
        else 
            parseImm true imms5
            |> Result.map (fun c -> (b <<< regBFieldOffset cpu) + c)

    /// Parse the 'Op' part of the assembler
    let rec parseOp (op: string list) =
        match op with
        | "[" :: op' -> 
            // remove the [ op ] brackets, for optional LDR, STR syntax
            // should really only allow these for LDR,STR, they are allowed for anything!
            parseOp op'[0..op'.Length-2]
        | [ RegMatch cpu b ]-> 
            /// It must be either Rb (EEP0 and EEP1)
            Ok (makeRegOp b)
        | [ RegMatch cpu b ; ","; c]
        | [ RegMatch cpu b ; ","; "#"; c] ->
            parseRegBAndImms5 b c
        | ["#" ; c ] ->
            parseImm false c
        | [c] -> parseImm false c
        | _ -> 
            let cs = String.concat " " op
            Error "Can't parse '{cs}' as '#N' or 'Ra'"

    // Tokenizes, then parses, a line of text, 
    // printing the assembly or an error message
    let parse (s:string) =
        let tokens = tokenize s
        match tokens with
        | [] -> Error ""
        | opc :: RegMatch cpu ra :: "," :: op ->
            match Map.tryFind opc (regOps cpu) with
            | None -> Error $"Expecting: register or memory opcode, not {opc}"
            | Some regOpc -> 
                parseOp op
                |> Result.map (fun op ->regOpc + (ra <<< regAFieldOffset cpu) + op )
        | opc :: op ->
            match Map.tryFind opc (jumps cpu) with
            | None -> Error $"Expecting: jump opcode, not {opc}"
            | Some jumpOpc -> 
                parseOp op
                |> Result.map (fun op -> jumpOpc + op )
        

    /// runs the assembler RAPL
    let doLoop () =
        let mutable running = true
        while running do
                printf ">>"
                let line = Console.ReadLine() 
                match String.trim line with
                | "q" -> 
                    running <- false
                | line when line = switchTo ->
                    runAssembler (if switchTo = "EEP1" then EEP1 else EEP0)
                | line ->
                    String.trim line
                    |> parse
                    |> (function | Ok n -> printfn "Machine Code: 0x%04x 0b%016B" n n
                                 | Error mess -> printfn $"Error in '{line}\n{mess}")
    doLoop ()

runAssembler EEP0

