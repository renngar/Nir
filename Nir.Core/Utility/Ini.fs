module Nir.Utility.INI

open FParsec

////////////////////////////////////////////////////////////////////////
/// INI Domain Model
////////////////////////////////////////////////////////////////////////
type IniPropertyValue = string

/// An INI file property including its name, value and any preceding comments
type Property =
    { Comments: string list
      Property: string
      Value: IniPropertyValue }

/// An INI file section including its name, properties and any preceding comments
type Section =
    { Comments: string list
      Section: string
      Properties: Property list }

/// An INI file including its sections with their properties and any trailing comments
type Ini =
    { FileName: string
      Sections: Section list
      TrailingComments: string list }

/// An INI parser
module Parser =
#if DEBUG
    // Useful for debugging parser combinators
    let (<!>) (p: Parser<_, _>) label: Parser<_, _> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply
#endif

    ////////////////////////////////////////////////////////////////////////
    /// INI Parsing Logic
    ////////////////////////////////////////////////////////////////////////
    type PropertyLine =
        { Name: string
          Value: string }

    type INI =
        | IniComment of string
        | IniSection of string
        | IniProperty of PropertyLine


    // Parse a sequence of any whitespace
    let ws = spaces

    // Parse intraline whitespace
    let lineWs = manyChars (anyOf " \t")

    /// parse a string throwing away any trailing spaces or tabs
    let strLineWs s = pstring s .>> lineWs

    /// `strExcept exception` parses a sequence of *zero* or more characters that do not appear in `exceptions`.
    let strExcept exceptions = manyChars (noneOf exceptions) .>> lineWs

    /// `str1Except exceptions` parses a sequence of *one* or more characters that do not appear in `exceptions`.
    let str1Except exceptions = many1Chars (noneOf exceptions) .>> lineWs

    /// parse a comment line beginning with `;`
    let lineComment = pchar ';' >>. restOfLine true |>> IniComment

    /// parses a section name, not the whole section header
    let sectionName =
        str1Except "] \t\n\r\000"

    /// parses a section header
    let sectionHeader =
        sectionName
        |> between (strLineWs "[") (strLineWs "]")
        .>> optional skipNewline
        |>> IniSection

    // Cannot start with a square, open bracket or a semicolon.  That would
    // be a section header or comment.
    let propertyChar1 =
        noneOf ";[\n\r\000="

    let private trimEnd (s: string) = s.TrimEnd()

    // Should not end with whitespace.  Trimming it is easier than
    let propertyCharRest =
        strExcept "\n\r\000="
        |>> trimEnd

    let propertyName = propertyChar1 .>>. propertyCharRest .>> lineWs |>> fun (i, r) -> (string i) + r

    let propertyValue = restOfLine true |>> trimEnd
    let assignment = strLineWs "="

    let propertyLine =
        propertyName .>> assignment .>>. propertyValue |>> fun (n, v) ->
            IniProperty
                { Name = n
                  Value = v }

    let line = lineComment <|> sectionHeader <|> propertyLine .>> ws

    let iniFile = ws >>. many line

    let convertToTree ini =
        let mutable comments = []

        let mutable section =
            { Comments = []
              Section = ""
              Properties = [] }

        let mutable sections = []

        let useComments() =
            let cs = List.rev (comments)
            comments <- []
            cs

        let finishPreviousSection() =
            match section with
            | sec when sec.Comments.IsEmpty && sec.Properties.IsEmpty -> ()
            | _ -> sections <- { section with Properties = List.rev (section.Properties) } :: sections

        for line in ini do
            match line with
            | IniComment c -> comments <- c :: comments
            | IniSection s ->
                finishPreviousSection()
                section <-
                    { Comments = useComments()
                      Section = s
                      Properties = [] }

            | IniProperty { Name = n; Value = v } ->
                section <-
                    { section with
                          Properties =
                              { Comments = useComments()
                                Property = n
                                Value = v }
                              :: section.Properties }
        finishPreviousSection()
        { FileName = ""
          Sections = List.rev (sections)
          TrailingComments = List.rev (comments) }

////////////////////////////////////////////////////////////////////////
/// INI Convenience Functions
////////////////////////////////////////////////////////////////////////
open Parser

/// `parseIni s` parses the INI content and converts it into a internal
/// `Ini` data model.
let parseIni (s: string): Ini =
    match run iniFile s with
    | Failure(msg, _, _) -> failwith msg
    | Success(ini, _, _) -> convertToTree ini

/// `parseIniFile fileName` loads and parses the .ini file into an internal
/// `Ini` data model.
let parseIniFile (fileName: string): Ini =
    try
        use sr = new System.IO.StreamReader(fileName)
        sr.ReadToEnd()
    with :? System.IO.FileNotFoundException -> ""
    |> parseIni
    |> fun ini -> { ini with FileName = fileName }

/// Returns the named `section` of the `ini`
let section section ini: Section * Ini =
    let s = ini.Sections |> List.tryFind (fun s -> s.Section = section)
    match s with
    | Some s' -> s', ini
    | None ->
        { Comments = []
          Section = section
          Properties = [] }, ini

/// Returns the named `property` within the given `section`, which may
/// be looked up using the `section` function.
let property property (section, ini): Property * Ini =
    let p = section.Properties |> List.tryFind (fun p -> p.Property = property)
    match p with
    | Some p' -> p', ini
    | None ->
        { Comments = []
          Property = property
          Value = "" }, ini

/// Return the value of `property`
let propertyValue (property: Property, ini: Ini): IniPropertyValue * Ini = property.Value, ini
