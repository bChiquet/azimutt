module DataSources.SqlParser.Parsers.CreateUnique exposing (ParsedUnique, parseCreateUniqueIndex)

import DataSources.SqlParser.Utils.Helpers exposing (buildRawSql, buildSchemaName, buildSqlLine, buildTableName, parseIndexDefinition)
import DataSources.SqlParser.Utils.Types exposing (ParseError, SqlColumnName, SqlConstraintName, SqlStatement, SqlTableRef)
import Libs.Nel as Nel exposing (Nel)
import Libs.Regex as Regex


type alias ParsedUnique =
    { name : SqlConstraintName, table : SqlTableRef, columns : Nel SqlColumnName, definition : String }


parseCreateUniqueIndex : SqlStatement -> Result (List ParseError) ParsedUnique
parseCreateUniqueIndex statement =
    case statement |> buildSqlLine |> Regex.matches "^CREATE UNIQUE INDEX\\s+(?<name>[^ ]+)\\s+ON\\s+(?:(?<schema>[^ .]+)\\.)?(?<table>[^ (]+)\\s*(?<definition>.+);$" of
        (Just name) :: schema :: (Just table) :: (Just definition) :: [] ->
            parseIndexDefinition definition
                |> Result.andThen (\columns -> Nel.fromList columns |> Result.fromMaybe [ "Unique index can't have empty columns" ])
                |> Result.map
                    (\columns ->
                        { name = name
                        , table = { schema = schema |> Maybe.map buildSchemaName, table = table |> buildTableName }
                        , columns = columns
                        , definition = definition
                        }
                    )

        _ ->
            Err [ "Can't parse create unique index: '" ++ buildRawSql statement ++ "'" ]
