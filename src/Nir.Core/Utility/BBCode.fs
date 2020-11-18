namespace Nir.Utility

open System.Text

// Supports the tags found on Nexus
//
//    https://wiki.nexusmods.com/index.php/Formating_and_BBCode_in_Descriptions
//
// I may support the forums and tags described on bbcode.org later:
//
//    https://forums.nexusmods.com/index.php?app=forums&module=extras&section=legends&do=bbcode
//    https://www.bbcode.org/reference.php
module BBCode =
    open FParsec
    open Nir.Parsing

    module Parser =
        type Tag =
            { Tag: string // TODO Make this a union type
              Value: string
              Content: Element list }

        and Element =
            | Text of string
            | Tag of Tag

        type State =
            { // Links may not be nested, but other tags are allowed
              InLink: bool
              ListDepth: int
              InListElement: bool }
            static member Default =
                { InLink = false
                  ListDepth = 0
                  InListElement = false }

        let private isTagChar = isWordChar

        // Derived from http://www.quanttec.com/fparsec/users-guide/parsing-with-user-state.html
        let elements, elementsR = createParserForwardedToRef ()

        let str s = pstring s

        let tagOpenBegin tag =
            str ("[" + tag)
            >>? nextCharSatisfiesNot isTagChar
            <?> "[" + tag + "] tag"

        let tagOpen tag = tagOpenBegin tag >>. str "]"
        let tagClose tag = str ("[/" + tag + "]")

        let tag (t: string) (p: Parser<Element list, 'a>) (f: Tag -> 'b): Parser<'b, 'a> =
            between
                (tagOpen t)
                (tagClose t)
                (p
                 |>> (fun x -> { Tag = t; Value = ""; Content = x } |> f))

        /// Defines a basic tag that takes no values and has no special rules
        let basicTag s = tag s elements Tag

        /// Defines a tag that can have extra content in the open tag.
        ///
        /// The extra material may be values, properties, styles, etc.
        let tagWithAttributes (t: string) (openTagContent: Parser<string, State>) allowedContent: Parser<Element, State> =
            tagOpenBegin t
            >>. openTagContent
            .>> str "]"
            .>>. allowedContent
            .>> tagClose t
            |>> (fun (v, c) -> { Tag = t; Value = v; Content = c } |> Tag)

        let equals = pchar '='

        /// Defines a tag that must have a value
        let valueTag t value allowedContent =
            tagWithAttributes t (equals >>. value) allowedContent

        /// Defines a tag that has an optional value
        let optionalValueTag t value content =
            let optionalValue =
                opt (equals >>. value)
                |>> (function
                | Some v -> v
                | None -> "")

            tagWithAttributes t optionalValue content

        let text: Parser<Element, State> = (many1Satisfy (isNoneOf "[]") |>> Text)

        let alignment s =
            choice
                [ str "left"
                  str "center"
                  str "right" ]
                s

        let img =
            optionalValueTag "img" alignment elements

        let private trim (s: string) = s.Trim()

        let maybeQuoted p = (p <|> between quote quote p) |>> trim

        let fontName =
            maybeQuoted (many1Chars (wordChar <|> aSpace))

        let font = valueTag "font" fontName elements

        let hexColor s =
            s
            |> (pchar '#'
                .>>. many1Chars hex
                |>> (fun (c, s) -> string c + s))

        let colorValue s =
            choice [ hexColor; many1Chars wordChar ] s

        let color = valueTag "color" colorValue elements

        // Adapted from the FParsec "Parsing with user state" example.
        //
        // http://www.quanttec.com/fparsec/users-guide/parsing-with-user-state.html
        let nonNestedTag tag pAttributesAndClose pBody f isInTag setInTag setNotInTag =
            tagOpenBegin tag
            >>. ((fun stream ->
                     if not (isInTag stream.UserState) then
                         stream.UserState <- setInTag stream.UserState
                         Reply(())
                     else // generate error at start of tag
                         stream.Skip(-tag.Length - 1)
                         Reply(FatalError, messageError ("Nested [" + tag + "] tags are not allowed.")))
                 >>. pipe2 pAttributesAndClose pBody f
                 .>> (tagClose tag >>. updateUserState setNotInTag))

        let urlChars s = noneOf "\" \t\n\\[\\]" s
        let urlAddress s = maybeQuoted (many1Chars urlChars) s

        let url =
            nonNestedTag
                "url"
                (equals >>. urlAddress .>> pchar ']')
                elements
                (fun url content ->
                    { Tag = "url"
                      Value = url
                      Content = content }
                    |> Tag)
                (fun us -> us.InLink)
                (fun us -> { us with InLink = true })
                (fun us -> { us with InLink = false })

        let standaloneTag tag =
            tagOpen tag
            |>> fun t -> { Tag = t; Value = ""; Content = [] } |> Tag

        let line = standaloneTag "line"

        let item =
            let canStartItem =
                userStateSatisfies (fun us -> us.ListDepth > 0 && not us.InListElement)

            canStartItem
            >>. ws
            >>. tagOpen "*"
            >>. updateUserState (fun us -> { us with InListElement = true })
            >>. elements
            .>> updateUserState (fun us -> { us with InListElement = false })
            |>> (fun x -> { Tag = "*"; Value = ""; Content = x } |> Tag)

        let listType = anyOf "1AaIi" |>> string
        let items = many item

        let list =
            let enterList =
                updateUserState (fun us -> { us with ListDepth = us.ListDepth + 1 })

            let exitList =
                updateUserState (fun us -> { us with ListDepth = us.ListDepth - 1 })

            optionalValueTag "list" listType
            <| between enterList exitList items

        let element =
            choice [ text
                     yield!
                         List.map
                             basicTag
                             [ "b"
                               "u"
                               "i"
                               "s"
                               "center"
                               "right"
                               "heading"
                               "quote"
                               "spoiler"
                               "youtube" ]
                     img
                     font
                     color
                     url
                     line
                     list
                     item ]

        do elementsR := many element
        let document = elements .>> eof

    /// Naively strips BBCode from a string
    ///
    /// Currently it does nothing intelligent about [quote], [line], [list], [*], or any other tag.
    let strip (str: string) =
        let sb = StringBuilder()
        let append (str: string) = sb.Append str |> ignore

        let rec stripList (es: Parser.Element list) =
            es
            |> Seq.iter (function
                | Parser.Text s -> append s
                | Parser.Tag tag -> stripList tag.Content)

        runParserOnString Parser.document Parser.State.Default "" str
        |> function
        | ParserResult.Success (es, _, _) -> stripList es
        | ParserResult.Failure _ -> append str

        sb.ToString()
