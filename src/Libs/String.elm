module Libs.String exposing (filterStartsWith, hashCode, inflect, nonEmpty, nonEmptyMaybe, orElse, plural, pluralize, pluralizeD, pluralizeL, stripRight, unique, wordSplit)

import Bitwise
import Dict exposing (Dict)
import Libs.Maybe as Maybe
import Libs.Regex as Regex


nonEmpty : String -> Bool
nonEmpty string =
    string /= ""


nonEmptyMaybe : String -> Maybe String
nonEmptyMaybe str =
    if str == "" then
        Nothing

    else
        Just str


stripRight : String -> String -> String
stripRight suffix str =
    if str |> String.endsWith suffix then
        str |> String.dropRight (String.length suffix)

    else
        str


orElse : String -> String -> String
orElse other str =
    if str == "" then
        other

    else
        str


filterStartsWith : String -> String -> String
filterStartsWith prefix str =
    if str |> String.startsWith prefix then
        str

    else
        ""


wordSplit : String -> List String
wordSplit input =
    List.foldl (\sep words -> words |> List.concatMap (\word -> String.split sep word)) [ input ] [ "_", "-", " " ]


hashCode : String -> Int
hashCode input =
    String.foldl updateHash 5381 input


updateHash : Char -> Int -> Int
updateHash char code =
    Bitwise.shiftLeftBy code (5 + code + Char.toCode char)


unique : List String -> String -> String
unique takenIds id =
    if takenIds |> List.any (\taken -> taken == id) then
        case id |> Regex.matches "^(.*?)([0-9]+)?(\\.[a-z]+)?$" of
            (Just prefix) :: num :: extension :: [] ->
                unique
                    takenIds
                    (prefix
                        ++ (num |> Maybe.andThen String.toInt |> Maybe.mapOrElse (\n -> n + 1) 2 |> String.fromInt)
                        ++ (extension |> Maybe.withDefault "")
                    )

            _ ->
                id ++ "-err"

    else
        id


inflect : String -> String -> String -> Int -> String
inflect none one many count =
    if count == 0 then
        none

    else if count == 1 then
        one

    else
        many


plural : String -> String
plural word =
    -- trivial pluralize that works only for usual words, use `inflect` for more flexibility
    if String.endsWith "y" word && not (String.endsWith "ay" word || String.endsWith "ey" word || String.endsWith "oy" word || String.endsWith "uy" word) then
        (word |> String.dropRight 1) ++ "ies"

    else if String.endsWith "s" word || String.endsWith "x" word || String.endsWith "z" word || String.endsWith "sh" word || String.endsWith "ch" word then
        word ++ "es"

    else
        word ++ "s"


pluralize : String -> Int -> String
pluralize word count =
    count |> inflect ("0 " ++ word) ("1 " ++ word) (String.fromInt count ++ " " ++ plural word)


pluralizeL : String -> List a -> String
pluralizeL word list =
    list |> List.length |> pluralize word


pluralizeD : String -> Dict k a -> String
pluralizeD word list =
    list |> Dict.size |> pluralize word
