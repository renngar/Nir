[<AutoOpen>]
module Nir.Utility.INI.Implementation

open System.IO
open FParsec
open Nir.Extensions
open Nir.Parsing
open Nir.Utility.INI.Types

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
    type PropertyLine = { Name: string; Value: string }

    type INI =
        | IniComment of string
        | IniSection of SectionName
        | IniProperty of PropertyLine


    // Parse a sequence of any whitespace
    let private ws = spaces

    /// parse a string throwing away any trailing spaces or tabs
    let private strLineWs s = pstring s .>> lineWs

    /// `strExcept exception` parses a sequence of *zero* or more characters that do not appear in `exceptions`.
    let private strExcept exceptions = manyChars (noneOf exceptions) .>> lineWs

    /// parse a comment line beginning with `;`
    let private lineComment =
        pchar ';' >>. restOfLine true |>> IniComment

    /// parses a section header
    let sectionHeader =
        SectionName.Parser
        |> between (strLineWs "[") (strLineWs "]")
        .>> optional skipNewline
        |>> IniSection

    // Cannot start with a square, open bracket or a semicolon.  That would
    // be a section header or comment.
    let private propertyChar1 = noneOf ";[\n\r\000="

    let private trimEnd (s: string) = s.TrimEnd()

    // Should not end with whitespace.  Trimming it is easier than
    let private propertyCharRest = strExcept "\n\r\000=" |>> trimEnd

    let propertyName =
        propertyChar1
        .>>. propertyCharRest
        .>> lineWs
        |>> fun (i, r) -> (string i) + r

    let propertyValue = restOfLine true |>> trimEnd
    let assignment = strLineWs "="

    let propertyLine =
        propertyName
        .>> assignment
        .>>. propertyValue
        |>> fun (n, v) -> IniProperty { Name = n; Value = v }

    let private line =
        lineComment
        <|> sectionHeader
        <|> propertyLine
        .>> ws

    let iniFile = ws >>. many line

    let internal convertToTree ini =
        let mutable comments = []

        let mutable section = None

        let mutable sections = []

        let useComments () =
            let cs = List.rev (comments)
            comments <- []
            cs

        let finishPreviousSection () =
            match section with
            | None -> ()
            | Some sec when sec.Comments.IsEmpty && sec.Properties.IsEmpty -> ()
            | Some sec ->
                sections <-
                    { sec with
                          Properties = List.rev (sec.Properties) }
                    :: sections

        for line in ini do
            match line with
            | IniComment c -> comments <- c :: comments
            | IniSection s ->
                finishPreviousSection ()

                section <-
                    Some
                        { Comments = useComments ()
                          Section = s
                          Properties = [] }

            | IniProperty { Name = n; Value = v } ->
                section <-
                    Option.map (fun section ->
                        { section with
                              Properties =
                                  { Comments = useComments ()
                                    Property = n
                                    Value = v }
                                  :: section.Properties }) section

        finishPreviousSection ()

        let sections' = List.rev (sections)

        { Sorted = sections' |> List.isSortedBy (fun s -> s.Section)
          FileName = ""
          Sections = sections'
          TrailingComments = List.rev (comments) }

////////////////////////////////////////////////////////////////////////
/// INI Convenience Functions
////////////////////////////////////////////////////////////////////////
open Parser

/// `parseIni s` parses the INI content and converts it into a internal
/// `Ini` data model.
let private parseIni (s: string): Ini =
    match run iniFile s with
    | Failure (msg, _, _) -> failwith msg
    | Success (ini, _, _) -> convertToTree ini

/// `parseIniFile fileName` loads and parses the .ini file into an internal
/// `Ini` data model.
let parseIniFile (fileName: string): Ini =
    try
        use sr = new StreamReader(fileName)
        sr.ReadToEnd()
    with :? FileNotFoundException -> ""
    |> parseIni
    |> fun ini -> { ini with FileName = fileName }

/// `saveIni` saves the Ini into its .ini file
let saveIni (ini: Ini) =
    let writeComments (sw: StreamWriter) (comments: string list) =
        for comment in comments do
            sw.WriteLine(comment)

    use sw = new StreamWriter(ini.FileName)
    let write format = fprintfn sw format

    ini.Sections
    |> if ini.Sorted then List.sortBy (fun s -> s.Section) else id
    |> Seq.iteri (fun i section ->
        if i > 0 then write "" // blank line between sections
        writeComments sw section.Comments
        write "[%s]" section.Section.Value

        for property in section.Properties do
            writeComments sw property.Comments
            write "%s=%s" property.Property property.Value)

    writeComments sw ini.TrailingComments
    ini

/// Try to fined the named section in the ini
let private trySection (section: SectionName) ini: Section option =
    ini.Sections
    |> List.tryFind (fun s -> s.Section = section)

let private newSection (section: SectionName) properties =
    { Comments = []
      Section = section
      Properties = properties }

/// Returns the named `section` of the `ini`
let section (section: SectionName) (ini: Ini): Section =
    match trySection section ini with
    | Some s -> s
    | None -> newSection section []

/// Try to find the named property in the list of properties
let tryProperty property properties: Property option =
    List.tryFind (fun s -> s.Property = property) properties

/// Try to find the named property in the list of properties and return its value
let tryPropertyValue property properties: IniPropertyValue option =
    tryProperty property properties
    |> Option.map (fun { Value = v } -> v)

/// Find `property` in `properties` and split its value using all `delimiters`.
///
/// Returns the split value as an array of string, if found. Otherwise `Array.empty` is returned.
let getPropertyValues property properties delimiters =
    let rec multiSplit (delimiters: string list) (ss: string []): string [] =
        match delimiters with
        | [] -> ss
        | [ d ] -> Array.collect (fun (s: string) -> s.Split(d)) ss
        | d :: ds -> multiSplit ds (multiSplit [ d ] ss)

    tryPropertyValue property properties
    |> Option.map (Array.create 1 >> multiSplit delimiters)
    |> Option.defaultValue Array.empty

let private newProperty name value: Property =
    { Comments = []
      Property = name
      Value = value }

/// Returns the named `property` within the given `section`, which may
/// be looked up using the `section` function.
let property property section =
    match tryProperty property section.Properties with
    | Some p -> p
    | None -> newProperty property ""

/// Return the value of `property`
let propertyValue (property: Property): IniPropertyValue = property.Value

let setProperty properties propertyName value: Properties =
    match tryProperty propertyName properties with
    | None -> List.append properties [ newProperty propertyName value ]
    | Some _ -> List.map (fun p -> if p.Property = propertyName then { p with Value = value } else p) properties

let setPropertyList properties propertyName delimiter values: Properties =
    String.concat delimiter values
    |> setProperty properties propertyName

let private updateSection sectionName propertyUpdater =
    List.map (fun s ->
        if s.Section = sectionName then
            { s with
                  Properties = propertyUpdater s }
        else
            s)

let getIniProperty ini sectionName propertyName: IniPropertyValue =
    ini
    |> section sectionName
    |> property propertyName
    |> propertyValue

let private setSectionProperties ini (sectionName: SectionName) properties: Ini =
    { ini with
          Sections =
              match trySection sectionName ini with
              | None -> List.append ini.Sections [ newSection sectionName properties ]
              | Some _ -> updateSection sectionName (fun _ -> properties) ini.Sections }

let setIniProperty ini sectionName propertyName value: Ini =
    { ini with
          Sections =
              match trySection sectionName ini with
              | None -> List.append ini.Sections [ newSection sectionName [ newProperty propertyName value ] ]
              | Some _ -> updateSection sectionName (fun s -> setProperty s.Properties propertyName value) ini.Sections }
