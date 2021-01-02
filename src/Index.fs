module Index

open Elmish
open Feliz
open Browser
type EscapeOptions =
    {
        EscapeForMarkdown: bool
        DisplayInTranslatedBox: bool
    }
type State =
    {
        Input: string * float
        TranslatedText: string
        /// n to k that the letter is translated
        Frequency: int * int
        Table: string * Transliterate.Table
        EscapeOptions: EscapeOptions
    }

type Msg =
    | SetInput of string * scrollHeight:float
    | SetFrequency of int * int
    | SetTable of string * Transliterate.Table
    | SetEscapeOptions of EscapeOptions

let init () =
    let st =
        {
            Table = Transliterate.tables.[0]
            Input = "", 0.0
            TranslatedText = ""
            Frequency = 1, 5
            EscapeOptions =
                {
                    EscapeForMarkdown = true
                    DisplayInTranslatedBox = false
                }
        }
    st, Cmd.none

let update (msg: Msg) (state: State) =
    let table = snd state.Table
    let transliterate table (n, k) escapeOptions text =
        let fn =
            if escapeOptions.EscapeForMarkdown
               && escapeOptions.DisplayInTranslatedBox then
                Transliterate.markdownEscape
            else id
        Transliterate.transliterate table (n, k) text
        |> fn

    match msg with
    | SetInput (text, scrollHeight) ->
        { state with
            TranslatedText =
                let n, k = state.Frequency
                transliterate table (n, k) state.EscapeOptions text
            Input = text, scrollHeight }, Cmd.none
    | SetFrequency(n, k) ->
        { state with
            Frequency = n, k
            TranslatedText =
                state.Input
                |> fst
                |> transliterate table (n, k) state.EscapeOptions
        }, Cmd.none
    | SetTable(name, table) ->
        { state with
            Table = name, table
            TranslatedText =
                let n, k = state.Frequency
                state.Input
                |> fst
                |> transliterate table (n, k) state.EscapeOptions
        }, Cmd.none
    | SetEscapeOptions escapeOptions ->
        { state with
            TranslatedText =
                let n, k = state.Frequency
                state.Input
                |> fst
                |> transliterate table (n, k) escapeOptions
            EscapeOptions = escapeOptions
        }, Cmd.none
open Zanaptak.TypedCssClasses
open Fable.Core

type Icon = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css", Naming.PascalCase>
type Bulma = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.1/css/bulma.min.css", Naming.PascalCase>

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Fulma.Extensions.Wikiki


let frequencySelect (state : State) (dispatch : Msg -> unit) =
    let frequencyList =
        [|
            "крайне редко 1/5"
            "редко 2/5"
            "чаще 3/5"
            "часто 4/5"
            "всегда 5/5"
        |]

    Dropdown.dropdown [
        Dropdown.IsHoverable
        // Dropdown.IsUp
    ] [
        Dropdown.trigger [] [
            Button.button [
                Button.OnClick (fun e ->
                    ()
                )
            ] [
                span []
                    [ str frequencyList.[fst state.Frequency - 1] ]
                Icon.icon [ Icon.Size IsSmall ]
                    [ Fa.i [ Fa.Solid.AngleDown ]
                        []
                    ]
            ]
        ]
        let currentFreqIdx = fst state.Frequency - 1

        Dropdown.menu [
            // Props [
            //     Style [
            //         CSSProp.Display DisplayOptions.Block
            //     ]
            // ]
        ] [
            frequencyList
            |> Array.mapi (fun i name ->
               Dropdown.Item.a [
                   if i = currentFreqIdx then
                       Dropdown.Item.IsActive true
                   Dropdown.Item.Option.Props [
                       OnClick (fun e ->
                           SetFrequency(i + 1, frequencyList.Length)
                           |> dispatch
                       )
                   ]
               ] [
                   str name
               ]
            )
            |> List.ofArray
            |> Dropdown.content []
        ]
    ]

let tableSelect (state : State) (dispatch : Msg -> unit) =
    let frequencyList = Transliterate.tables

    Dropdown.dropdown [
        Dropdown.IsHoverable
        // Dropdown.IsUp
    ] [
        Dropdown.trigger [] [
            Button.button [
                Button.OnClick (fun e ->
                    ()
                )
            ] [
                span []
                    [ str (fst state.Table) ]
                Icon.icon [ Icon.Size IsSmall ]
                    [ Fa.i [ Fa.Solid.AngleDown ]
                        []
                    ]
            ]
        ]
        let currentFreqIdx = fst state.Table

        Dropdown.menu [
            // Props [
            //     Style [
            //         CSSProp.Display DisplayOptions.Block
            //     ]
            // ]
        ] [
            frequencyList
            |> Array.map (fun (name, table) ->
               Dropdown.Item.a [
                   if name = currentFreqIdx then
                       Dropdown.Item.IsActive true
                   Dropdown.Item.Option.Props [
                       OnClick (fun e ->
                           SetTable(name, table)
                           |> dispatch
                       )
                   ]
               ] [
                   str name
               ]
            )
            |> List.ofArray
            |> Dropdown.content []
        ]
    ]


let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [] [
        Column.column [
        ] [
            tableSelect state dispatch
            frequencySelect state dispatch
        ]
        Columns.columns [] [
            Column.column [
            ] [
                Columns.columns [] [
                    Column.column [] [
                        Field.div [ Field.IsGrouped ] [
                            Control.p [ Control.IsExpanded ] [
                                Textarea.textarea [
                                    Textarea.Placeholder "Text for transliteration"
                                    Textarea.HasFixedSize
                                    Textarea.Value (fst state.Input)
                                    Textarea.OnChange (fun x ->
                                        let y = x.currentTarget :?> Types.HTMLTextAreaElement
                                        if y.scrollHeight <> snd state.Input then
                                            y.setAttribute ("style", "height: auto;") // overflow-y: hidden;
                                            y.setAttribute ("style", sprintf "height: %fpx" y.scrollHeight)

                                        SetInput (x.Value, y.scrollHeight) |> dispatch
                                    )
                                ] []
                            ]
                        ]
                    ]
                ]
            ]
            Column.column [] [
                Columns.columns [] [
                    Column.column [] [
                        Content.content [] [
                            Field.div [ Field.IsGrouped ] [
                                Control.p [ Control.IsExpanded ] [
                                    Textarea.textarea [
                                        Textarea.Placeholder "Transliteration result"
                                        Textarea.HasFixedSize
                                        Textarea.IsReadOnly true
                                        Textarea.Value (state.TranslatedText)

                                        Textarea.Props [
                                            Style [
                                                CSSProp.Height (snd state.Input)
                                            ]
                                        ]
                                    ] []
                                ]
                            ]
                        ]
                    ]
                ]
                Columns.columns [ Columns.IsCentered ] [
                    Column.column [] [
                        match Browser.Navigator.navigator.clipboard with
                        | Some clipboard ->
                            Button.button [
                                Button.OnClick (fun _ ->
                                    let text =
                                        if state.EscapeOptions.EscapeForMarkdown then
                                            if state.EscapeOptions.DisplayInTranslatedBox then
                                                state.TranslatedText
                                            else
                                                Transliterate.markdownEscape state.TranslatedText
                                        else
                                            state.TranslatedText
                                    clipboard.writeText text
                                    |> ignore

                                )
                            ] [
                                Fa.span [ Fa.Solid.Clipboard; Fa.FixedWidth] []
                            ]
                        | None -> ()
                    ]
                    Column.column [] [
                        div [ ClassName "block" ] [
                            Checkradio.checkbox  [
                                Checkradio.Id "checkradio-1"

                                Checkradio.Checked state.EscapeOptions.EscapeForMarkdown
                                Checkradio.OnChange (fun e ->
                                    {
                                        state.EscapeOptions with
                                            EscapeForMarkdown =
                                                not state.EscapeOptions.EscapeForMarkdown
                                    }
                                    |> SetEscapeOptions
                                    |> dispatch
                                )
                            ] [
                                str "Экранировать для Discord "
                            ]
                            Checkradio.checkbox  [
                                Checkradio.Id "checkradio-2"
                                Checkradio.Disabled (not state.EscapeOptions.EscapeForMarkdown)
                                Checkradio.Checked state.EscapeOptions.DisplayInTranslatedBox
                                if state.EscapeOptions.EscapeForMarkdown then
                                    Checkradio.OnChange (fun e ->
                                        {
                                            state.EscapeOptions with
                                                DisplayInTranslatedBox =
                                                    not state.EscapeOptions.DisplayInTranslatedBox
                                        }
                                        |> SetEscapeOptions
                                        |> dispatch
                                    )
                            ] [
                                str "Отображать итог "
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let navBrand =
    Navbar.Brand.div [] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/fable.ico"
                Alt "Logo"
            ]
        ]
    ]

let view (state : State) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.IsFullHeight
    ] [
        Hero.head [] [
            Navbar.navbar [] [
                Container.container [] [
                    // navBrand
                ]
            ]
        ]

        Hero.body [] [
            Container.container [] [
                Column.column [
                    // Column.Width (Screen.All, Column.Is6)
                    // Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "Transliterator" ]
                    containerBox state dispatch
                ]
            ]
        ]
    ]
