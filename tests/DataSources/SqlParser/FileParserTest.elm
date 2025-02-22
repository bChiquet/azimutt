module DataSources.SqlParser.FileParserTest exposing (..)

import DataSources.SqlParser.FileParser exposing (buildSqlLines, buildStatements, hasKeyword, parseLines)
import Expect
import Libs.Nel as Nel
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "FileParser"
        [ describe "buildStatements"
            [ testBuildStatements "basic"
                """-- a comment

                   CREATE TABLE public.users (
                     id bigint NOT NULL,
                     name character varying(255)
                   );

                   COMMENT ON TABLE public.users IS 'A comment ; ''tricky'' one';

                   ALTER TABLE ONLY public.users
                     ADD CONSTRAINT users_id_pkey PRIMARY KEY (id);"""
                [ """CREATE TABLE public.users (
                       id bigint NOT NULL,
                       name character varying(255)
                     );"""
                , """COMMENT ON TABLE public.users IS 'A comment ; ''tricky'' one';"""
                , """ALTER TABLE ONLY public.users
                       ADD CONSTRAINT users_id_pkey PRIMARY KEY (id);"""
                ]
            , testBuildStatements "nested begin"
                """CREATE FUNCTION public.set_log_min_duration(integer) RETURNS void
                       LANGUAGE plpgsql STRICT SECURITY DEFINER
                       SET search_path TO 'pg_catalog', 'pg_temp'
                   AS $_$
                   BEGIN
                       EXECUTE 'SET log_min_duration_statement = ' || $1::text;
                   END
                   $_$;"""
                [ """CREATE FUNCTION public.set_log_min_duration(integer) RETURNS void
                         LANGUAGE plpgsql STRICT SECURITY DEFINER
                         SET search_path TO 'pg_catalog', 'pg_temp'
                     AS $_$
                     BEGIN
                         EXECUTE 'SET log_min_duration_statement = ' || $1::text;
                     END
                     $_$;""" ]
            , testBuildStatements "with markers"
                """alter table event.event add constraint event_event_type__name_fkey
                   foreign key (event_type__name)
                   references event.event_type (event_type__name)
                   match simple
                   on delete restrict
                   on update restrict;

                   create or replace function event.dispatch()
                       returns trigger
                       language plpgsql
                   as
                   $$
                   declare
                       key text;
                       value text;
                       subscription_id uuid;
                   begin
                       for key, value in select * from jsonb_each_text(new.labels) limit 50
                           loop
                               for subscription_id in
                                   select subscription__id
                                   from webhook.subscription
                                   where is_enabled
                                     and label_key = key
                                     and label_value = value
                                   loop
                                       raise notice '[event %] matching subscription: %', event__id, subscription_id;
                                       insert into webhook.request_attempt (event__id, subscription__id)
                                       values (new.event__id, subscription_id);
                                   end loop;
                           end loop;
                       update event.event set dispatched_at = statement_timestamp() where event__id = new.event__id;
                       return new;
                   end;
                   $$;

                   create schema webhook;"""
                [ """alter table event.event add constraint event_event_type__name_fkey
                     foreign key (event_type__name)
                     references event.event_type (event_type__name)
                     match simple
                     on delete restrict
                     on update restrict;"""
                , """create or replace function event.dispatch()
                         returns trigger
                         language plpgsql
                     as
                     $$
                     declare
                         key text;
                         value text;
                         subscription_id uuid;
                     begin
                         for key, value in select * from jsonb_each_text(new.labels) limit 50
                             loop
                                 for subscription_id in
                                     select subscription__id
                                     from webhook.subscription
                                     where is_enabled
                                       and label_key = key
                                       and label_value = value
                                     loop
                                         raise notice '[event %] matching subscription: %', event__id, subscription_id;
                                         insert into webhook.request_attempt (event__id, subscription__id)
                                         values (new.event__id, subscription_id);
                                     end loop;
                             end loop;
                         update event.event set dispatched_at = statement_timestamp() where event__id = new.event__id;
                         return new;
                     end;
                     $$;"""
                , "create schema webhook;"
                ]
            , testBuildStatements "case blocks"
                """PRAGMA foreign_keys=OFF;

                   CREATE VIEW `tasks_view` as
                   select
                   `tasks`.`ulid` as `ulid`,
                   ifnull(`tasks`.`priority_adjustment`, 0.0)
                    + case   when waiting_utc is null then 0.0
                    when waiting_utc >= datetime('now') then 0.0
                    when waiting_utc <  datetime('now') then -10.0
                   end
                   as `priority`
                   from
                   `tasks`;

                   CREATE TRIGGER `set_modified_utc_after_update`
                   after update on `tasks`
                   when `new`.`modified_utc` is `old`.`modified_utc`
                   begin
                    update `tasks`
                   set `modified_utc` = datetime('now')
                   where `ulid` = `new`.`ulid`
                   ;
                   end;"""
                [ "PRAGMA foreign_keys=OFF;"
                , """CREATE VIEW `tasks_view` as
                     select
                     `tasks`.`ulid` as `ulid`,
                     ifnull(`tasks`.`priority_adjustment`, 0.0)
                      + case   when waiting_utc is null then 0.0
                      when waiting_utc >= datetime('now') then 0.0
                      when waiting_utc <  datetime('now') then -10.0
                     end
                     as `priority`
                     from
                     `tasks`;"""
                , """CREATE TRIGGER `set_modified_utc_after_update`
                     after update on `tasks`
                     when `new`.`modified_utc` is `old`.`modified_utc`
                     begin
                      update `tasks`
                     set `modified_utc` = datetime('now')
                     where `ulid` = `new`.`ulid`
                     ;
                     end;"""
                ]
            ]
        , describe "hasKeyword"
            [ test "keyword" (\_ -> { line = 0, text = "  END" } |> hasKeyword "END" |> Expect.equal True)
            , test "lowercase at the end" (\_ -> { line = 0, text = "  end;" } |> hasKeyword "END" |> Expect.equal True)
            , test "column name" (\_ -> { line = 0, text = "  `end` timestamp," } |> hasKeyword "END" |> Expect.equal False)
            , test "inside column name" (\_ -> { line = 0, text = "  pending bool," } |> hasKeyword "END" |> Expect.equal False)
            , test "start column name" (\_ -> { line = 0, text = "  end_at timestamp," } |> hasKeyword "END" |> Expect.equal False)
            , test "inside name" (\_ -> { line = 0, text = "CREATE TABLE job_end (" } |> hasKeyword "END" |> Expect.equal False)
            , test "inside string" (\_ -> { line = 0, text = "COMMENT ON COLUMN public.t1.c1 IS 'Item end date'" } |> hasKeyword "END" |> Expect.equal False)
            , test "$$ separator" (\_ -> { line = 0, text = "$$;" } |> hasKeyword "\\$\\$;" |> Expect.equal True)
            ]
        ]


testBuildStatements : String -> String -> List String -> Test
testBuildStatements name content statements =
    test name
        (\_ ->
            content
                |> parseLines
                |> buildSqlLines
                |> buildStatements
                |> List.map (\s -> s |> Nel.toList |> List.map .text |> List.map String.trim)
                |> Expect.equal (statements |> List.map (\s -> s |> String.split "\n" |> List.map String.trim))
        )
