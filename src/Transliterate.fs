module Transliterate

type Table = Map<char, string []>
let convertTable xs =
    xs
    |> List.groupBy fst
    |> List.map (fun (k, xs) -> k, xs |> List.map snd |> Array.ofList)
    |> Map.ofList

let tableChars =
    [
        'в', "y"
        'ы', "b|"
        'е', "3"
        'э', "-)"
        'т', "+"
        'х', "><"
        'и', "i"
        'а', "(L"
        'а', @"/-\"
        'г', "|^"
        'з', "'/_"
        'и', "{/}"
        'ч', "`-|"
        'у', @"\|/"
        'х', "}{"
        'и', "[/]"
        'ё', "È"
        'т', "7"
        'о', "<>"
        'а', "@"
        'е', "[-"
        'м', "{V}"
        'р', "|°"
        'г', "|-"
        'ю', "|~O"
        'п', "/7"
        'р', "|7"
        'б', "Ƌ"
        'с', "©"
        'е', "F"
        'ж', "|-|-|"
        'ж', ">|<"
        'т', "«|»"
        'т', "-|-"
        'т', "~|~"
        'а', "4"
        'н', "!-!"
        'и', "V"
        'о', "0"
        'д', @"_/\_"
        'л', "J|"
        'к', "1<"
        'з', "z"
        'ш', "III"
        'ф', "ƒ"
        'о', "()"
        'в', "j3"
        'в', "|3"
        'а', @"/\"
    ]
    |> convertTable

let tableGost =
    [
        'Є', "EH"
        'І', "I"
        'і', "i"
        '№', "#"
        'є', "eh"
        'А', "A"
        'Б', "B"
        'В', "V"
        'Г', "G"
        'Д', "D"
        'Е', "E"
        'Ё', "JO"
        'Ж', "ZH"
        'З', "Z"
        'И', "I"
        'Й', "JJ"
        'К', "K"
        'Л', "L"
        'М', "M"
        'Н', "N"
        'О', "O"
        'П', "P"
        'Р', "R"
        'С', "S"
        'Т', "T"
        'У', "U"
        'Ф', "F"
        'Х', "KH"
        'Ц', "C"
        'Ч', "CH"
        'Ш', "SH"
        'Щ', "SHH"
        'Ъ', "'"
        'Ы', "Y"
        'Ь', ""
        'Э', "EH"
        'Ю', "YU"
        'Я', "YA"
        'а', "a"
        'б', "b"
        'в', "v"
        'г', "g"
        'д', "d"
        'е', "e"
        'ё', "jo"
        'ж', "zh"
        'з', "z"
        'и', "i"
        'й', "jj"
        'к', "k"
        'л', "l"
        'м', "m"
        'н', "n"
        'о', "o"
        'п', "p"
        'р', "r"
        'с', "s"
        'т', "t"
        'у', "u"
        'ф', "f"
        'х', "kh"
        'ц', "c"
        'ч', "ch"
        'ш', "sh"
        'щ', "shh"
        'ъ', ""
        'ы', "y"
        'ь', ""
        'э', "eh"
        'ю', "yu"
        'я', "ya"
        '«', ""
        '»', ""
        '—', "-"
    ]
    |> convertTable

let tableIso =
    [
        'Є', "YE"
        'І', "I"
        'Ѓ', "G"
        'і', "i"
        '№', "#"
        'є', "ye"
        'ѓ', "g"
        'А', "A"
        'Б', "B"
        'В', "V"
        'Г', "G"
        'Д', "D"
        'Е', "E"
        'Ё', "YO"
        'Ж', "ZH"
        'З', "Z"
        'И', "I"
        'Й', "J"
        'К', "K"
        'Л', "L"
        'М', "M"
        'Н', "N"
        'О', "O"
        'П', "P"
        'Р', "R"
        'С', "S"
        'Т', "T"
        'У', "U"
        'Ф', "F"
        'Х', "X"
        'Ц', "C"
        'Ч', "CH"
        'Ш', "SH"
        'Щ', "SHH"
        'Ъ', "'"
        'Ы', "Y"
        'Ь', ""
        'Э', "E"
        'Ю', "YU"
        'Я', "YA"
        'а', "a"
        'б', "b"
        'в', "v"
        'г', "g"
        'д', "d"
        'е', "e"
        'ё', "yo"
        'ж', "zh"
        'з', "z"
        'и', "i"
        'й', "j"
        'к', "k"
        'л', "l"
        'м', "m"
        'н', "n"
        'о', "o"
        'п', "p"
        'р', "r"
        'с', "s"
        'т', "t"
        'у', "u"
        'ф', "f"
        'х', "x"
        'ц', "c"
        'ч', "ch"
        'ш', "sh"
        'щ', "shh"
        'ъ', ""
        'ы', "y"
        'ь', ""
        'э', "e"
        'ю', "yu"
        'я', "ya"
        '«', ""
        '»', ""
        '—', "-"
    ]
    |> convertTable

let tableCustom =
    [
        'а', "4"
        'б', "6"
        'в', "8"
        'г', "r"
        'д', "|)"
        'е', "3"
        'ж', "]|["
        'з', "3"
        'и', "|/|"
        'й', "|/|"
        'к', "|<"
        'л', "J|"
        'м', @"/\/\"
        'н', "|-|"
        'о', "0"
        'п', "||"
        'р', "|>"
        'с', "["
        'т', "7"
        'y', "`/"
        'ф', "qp"
        'х', "*"
        'ц', "LL"
        'ч', "`-|"
        'ш', "LL|"
        'ъ', "`Ь"
        'ы', "b|"
        'я', "9|"
    ]
    |> convertTable

let tableGreek =
    [
        "α" , 'а'
        "β" , 'б'
        // "γ" , 'г'
        "δ" , 'д'
        "ε" , 'э'
        "ξ" , 'е'
        // "ει", 'и'
        "ζ" , 'з'

        // "ι" , 'и'
        "κ" , 'к'
        "λ" , 'л'
        // "μ" , 'м'
        // "ν" , 'н'
        // "ου", 'у'
        "π" , 'п'
        "ρ" , 'р'
        "ς" , 'с'

        // "θ" , 'т'
        "τ" , 'т'

        "υ" , 'и'
        // "υ" , 'ю'
        // "υ" , 'в'
        "φ" , 'ф'
        "χ" , 'х'
    ]
    |> List.map (fun (x, y) -> y, x)
    |> convertTable

let tables =
    [|
        "Из VtM", tableChars
        "Греческая", tableGreek
        "???", tableCustom
        "По ГОСТУ", tableGost
        "По ISO", tableIso
    |]

let r = System.Random()

let markdownEscape =
    String.collect (function
        | '\\' -> @"\\"
        | '_' -> @"\_"
        | '*' -> @"\*"
        | '~' -> @"\~"
        // | '<' -> @"\<" // эта штука исключительно Discord'а. Не знаю, надо ли экранировать
        | c -> string c
    )

let transliterate (table:Table) (n, k) =
    let fn =
        if n < k then
            fun c ->
                match Map.tryFind c table with
                | Some (xs:_ []) ->
                    if r.Next(0, k) < n then
                        xs.[r.Next(0, xs.Length)]
                    else
                        string c
                | None -> string c
        else
            fun c ->
                match Map.tryFind c table with
                | Some xs ->
                    xs.[r.Next(0, xs.Length)]
                | None -> string c

    String.collect fn