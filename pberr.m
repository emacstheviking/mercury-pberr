%-----------------------------------------------------------------------------%
%
% File: pberr.m
% Main author: emacstheviking <objitsu@gmail.com>
% Date: Tue Mar  1 21:15:59 2022
%
%
% This merges the output from the Mercury compilers '.err' files into the 
% currently open file in Nano. If no corresponding error file exists then
% nothing happens. Otherwise the contents of that file are interleaved into
% the buffer as comments, at the relevant positions. Coupled with a custom
% nano colouring file and it all feels nice and cosy.
%
% Thanks to Paul Bone for the inspiration...and, as it turns out, some FIFTEEN
% years earlier writing a very similar program which I have no learned about
% during the course of this exercise! LMAO You couldn't make it up!
%-----------------------------------------------------------------------------%
:- module pberr.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module dir.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module multi_map.
:- import_module string.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

main(!IO) :-
    cwd(CWD, !IO),
    log("\n---- session in: %s ----\n", [s(CWD)], !IO),
    io.command_line_arguments(Argv, !IO),
    log("ARGS: %s", [s(string(Argv))], !IO),
    ( if
        Argv = [ "nanoformatter", File ]
    then
        log("NANO: formatter: from: %s", [s(File)], !IO),
        module_from_temp(CWD, File, Mres, !IO)
    else
        if Argv = [File]
    then
        log("CLI: from: %s", [s(File)], !IO),
        module_from_filename(CWD, File, Mres, !IO)
    else
        Mres = no
    ),

    (   Mres = no,
        log("no module identified, ending", [], !IO)
    ;
        Mres = yes({ Module, SrcFile, ErrFile }),
        log("Module: %s", [s(Module)], !IO),
        load_errors(ErrFile, Eres, !IO),
        (
            Eres = no,
            log("%s: no errors to process", [s(Module)], !IO)
        ;
            Eres = yes(Elist),
            (   multi_map.is_empty(Elist)
            ->  true
            ;   interweave(SrcFile, Elist, !IO)
            )
        )
    ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

    % Interweave a source file with its error content, if any.
    % SrcFile contains the file containing the Mercury code, this file
    % IS OVERWRITTEN by the process.
    %
:- pred interweave(string::in, error_lines::in, io::di, io::uo) is det.

interweave(File, Errors, !IO) :-
    io.get_temp_directory(Tmp, !IO),
    TmpFile = Tmp / "pberr.temp.m",

    open_streams(File, TmpFile, Ores, !IO),
    (   Ores = no
    ;
        Ores = yes({ Sin, Sout }),
        log("interweave: from: %s, %s",
            [s(string(File)), s(string(Sin))], !IO),
        log("interweave: dest: %s, %s",
            [s(string(TmpFile)), s(string(Sout))], !IO),

        (   multi_map.is_empty(Errors)
        ->
            io.read_file_as_string(Sin, CodeM, !IO),
            (
                CodeM = io.error(_, E),
                log("interweave: ERROR: %s", [s(error_message(E))], !IO)
            ;
                CodeM = io.ok(TheRest),
                io.format(Sout, "%s", [s(TheRest)], !IO),
                log("interweave: empty errors, transfer all ok", [], !IO)
            )
        ;
            put_back_errors(
                multi_map.sorted_keys(Errors),
                Errors,
                Sin,
                Sout,
                !IO
            )
        ),
        io.close_input(Sin, !IO),
        io.close_output(Sout, !IO),
        log("interweave: streams closed ok", [], !IO),
        file_copy(TmpFile, File, !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Transfer one file to another.
    % Copies one file to another, I was going to use system but then it
    % would no longer be platform portable.
    %
:- pred file_copy(string::in, string::in, io::di, io::uo) is det.

file_copy(From, Dest, !IO) :-
    open_streams(From, Dest, Ores, !IO),
    (   Ores = no
    ;
        Ores = yes({ Sin, Sout }),
        file_copy_(Sin, Sout, !IO),
        io.close_input(Sin, !IO),
        io.close_output(Sout, !IO),
        log("file_copy: %s ==> %s completed",
            [s(From), s(Dest)], !IO)
    ).

:- pred file_copy_(io.input_stream::in, io.output_stream::in,
    io::di, io::uo) is det.

file_copy_(From, Dest, !IO) :-
    io.read_line_as_string(From, Fres, !IO),
    (
        Fres = io.eof,
        log("file_copy_: End of file on: %s",
            [s(string(From))], !IO)
    ;
        Fres = io.error(E),
        log("file_copy_: Error on: %s, %s",
            [s(string(From)), s(error_message(E))], !IO)
    ;
        Fres = io.ok(X),
        io.format(Dest, "%s", [s(X)], !IO),
        file_copy_(From, Dest, !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Put back errors.
    % This interleaves the error content with the contents of the source file
    % until there are no more errors remaining at which point the remainder of
    % the source file is copied to the output stream.
    %
:- pred put_back_errors(list(int)::in, error_lines::in, io.input_stream::in,
    io.output_stream::in, io::di, io::uo) is det.

put_back_errors([], _, I, O, !IO) :-
    log("put_bacK_errors: done, flushing remainder", [], !IO),
    io.read_file_as_string(I, Res, !IO),
    (
        Res = io.error(_, _)
    ;
        Res = io.ok(String),
        io.format(O, "%s", [s(String)], !IO)
    ).

put_back_errors([ E | Es ], Map, I, O, !IO) :-
    io.get_line_number(I, L, !IO),
    ( if L < E then
        io.read_line_as_string(I, Res, !IO),
        (
            Res = io.error(Err),
            log("pbe:error:%s", [s(error_message(Err))], !IO)
        ;
            Res = io.eof
        ;
            Res = io.ok(String),
            io.format(O, "%s", [s(String)], !IO),
            put_back_errors([ E | Es ], Map, I, O, !IO)
        )
    else
        (   multi_map.search(Map, E, Strings)
        ->
            io.format(O, "%%E\n", [], !IO),

            list.foldl(
                (pred(Line::in, !.IO::di, !:IO::uo) is det :-
                    io.format(O, "%%E%s\n", [s(Line)], !IO)
                ),
                Strings, !IO
            ),

            io.format(O, "%%E\n", [], !IO)
        ;
            true
        ),
        put_back_errors(Es, Map, I, O, !IO)
    ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
% Load errors from a corresponding '.err' file.
% If we can't open the file we'll assume that it's because there isn't
% any error file to find i.e. the compile was a good one!
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pred load_errors(string::in, maybe(error_lines)::out,
    io::di, io::uo) is det.

load_errors(Src, Out, !IO) :-
    get_error_content(Src, Eres, !IO),
    (
        Eres = yes(Errors),
        parse_errors(Errors, Mmap),
        (   multi_map.is_empty(Mmap)
        ->  Out = no
        ;   Out = yes(Mmap)
        )
    ;
        Eres = no
        , Out = no
    ).


:- pred get_error_content(string::in, maybe(list(string))::out,
    io::di, io::uo) is det.

get_error_content(File, Out, !IO) :-
    log("get_error_content: File: %s", [s(File)], !IO),
    io.open_input(File, Res, !IO),
    (
        Res = io.ok(S),
        io.read_file_as_string(S, CodeM, !IO),
        io.close_input(S, !IO),
        (
            CodeM = io.error(_, E),
            log("get_error_content: %s: read fail: %s",
                [s(File), s(error_message(E))], !IO),
            Out = no
        ;
            CodeM = io.ok(Value),
            Out = yes(string.split_into_lines(Value))
        )
    ;
        Res = io.error(E),
        log("get_error_content: open fail: %s, %s",
            [s(error_message(E)), s(File)], !IO),
        Out = no
    ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

    % Parse error content.
    % This transforms the source data, a list of strings from the .err
    % file into the output data format, a multi_map keyed by the integer
    % line number, each entry containing the strings for that particular
    % entry.
    % The list of keys from the multi_map is used to drive the weaving of
    % the errors into the output.
    %
:- type error_line == {int, string}.
:- type error_lines == multi_map(int, string).

:- pred parse_errors(list(string)::in, error_lines::out) is det.

parse_errors(Content, Mmap) :-
    list.filter_map(parse_error, Content, Errors),
    list.foldl(
        (pred({N,S}::in, !.M::in, !:M::out) is det :-
            multi_map.add(N, S, !M)
        ),
        Errors,
        multi_map.init:error_lines, Mmap
    ).

    % Parse a single error line:
    % The format is always: FILENAME:LINE:TEXTCONTENT
    % Out is a tuple: {int, string}.
    %
:- pred parse_error(string::in, error_line::out) is semidet.

parse_error(E, Out) :-
    Parts = string.split_at_char((':'), E),
    Parts = [ _, Line | Rest ],
    Int = string.det_to_int(Line),
    Out = { Int, string.format(" %06i %s",
            [i(Int), s(string.join_list(":", Rest))])
        }.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
%
%   Fetching a module name from either a nano invocation
%   or a vanilla command line invocation.
%
%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- pred module_from_filename(string::in, string::in,
    maybe({string, string, string})::out, io::di, io::uo) is det.

module_from_filename(CWD, File, Out, !IO) :-
    (   dir.basename(File, B)
    ->  Parts = string.split_at_char(('.'), B),
        (   Parts = [ Module | _ ]
        ->  Root = string.format("%s%c",
                [s(CWD), c(dir.directory_separator)]),
        Out = yes({
                Module,
                string.format("%s%s.m", [s(Root), s(Module)]),
                string.format("%s%s.err", [s(Root), s(Module)])
            })
        ;   Out = no,
            log("split fail: %s", [s(File)], !IO)
        )
    ;   Out = no,
        log("basename fail: %s", [s(File)], !IO)
    ).

%----------------------------------------------------------------------------%

:- pred module_from_temp(string::in, string::in,
    maybe({string, string, string})::out, io::di, io::uo) is det.

module_from_temp(CWD, TmpIn, Out, !IO) :-
    io.open_input(TmpIn, Ires, !IO),
    (
        Ires = io.ok(S),
        module_read(S, Mres, !IO),
        (
            Mres = yes(Module),
            Root = string.format("%s%c",
                [s(CWD), c(dir.directory_separator)]),
            Out = yes({
                    Module,
                    TmpIn,
                    string.format("%s%s.err", [s(Root), s(Module)])
                  })
        ;
            Mres = no,
            Out = no,
            log("no :- module in: %s", [s(TmpIn)], !IO)
        )
    ;
        Ires = io.error(E),
        log("open failed: %s: %s",
            [s(TmpIn), s(error_message(E))], !IO),
        Out = no
    ).


:- pred module_read(io.input_stream::in, maybe(string)::out,
    io::di, io::uo) is det.

module_read(Sin, Out, !IO) :-
    io.read_line_as_string(Sin, Res, !IO),
    (
        Res = io.error(_),
        Out = no
    ;
        Res = io.eof,
        Out = no
    ;
        Res = io.ok(Line),
        ( if
            string.remove_prefix(":- module ", string.strip(Line), Rest),
            string.remove_suffix(Rest, ".", Name)
        then
            Out = yes(Name)
        else
            module_read(Sin, Out, !IO)
        )
    ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

    % Open input and output streams.
    % Given two filenames we attempt to open streams for reading and writing
    % those locations.
    %
:- pred open_streams(string::in, string::in,
    maybe({io.input_stream, io.output_stream})::out,
    io::di, io::uo) is det.

open_streams(InFile, OutFile, Out, !IO) :-
    open_input(InFile, Ires, !IO),
    (
        Ires = io.error(E),
        Out = no,
        log("open_streams:InFile: %s, %s",
            [s(InFile), s(error_message(E))], !IO)
    ;
        Ires = io.ok(Sin),
        (
            open_output(OutFile, Ores, !IO),
            (
                Ores = io.error(E),
                Out = no,
                log("open_streams:OutFile: %s, %s",
                    [s(OutFile), s(error_message(E))], !IO),
                io.close_input(Sin, !IO)
            ;
                Ores = io.ok(Sout),
                Out = yes({Sin, Sout})
            )
        )
    ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

    % Get current working directory.
    % This should be the folder the nano session was launched from!
    %
:- pred cwd(string::out, io::di, io::uo) is det.

cwd(CWD, !IO) :-
    dir.current_directory(Dres, !IO),
    (
        Dres = io.ok(CWD),
        log("cwd: %s", [s(CWD)], !IO)
    ;
        Dres = io.error(E),
        CWD = dir.this_directory,
        log("cwd: %s", [s(error_message(E))], !IO)        
    ).

%----------------------------------------------------------------------------%

    % Log Helper, hack to suit!
    %
    % For development and testing, I run the program like this:
    %
    %     $ ./pberr nano FILENAME 2>pberr.log
    %
    % and then in another window I just do this:
    %
    %    $ tail -f pberr.log
    %
:- pred log(string::in, list(poly_type)::in, io::di, io::uo) is det.

log(Format, Args, !IO) :-
    Log = string.format(Format, Args),

    open_output("pberr.log", Ores, !IO),
    (
        Ores = io.error(_)
    ;
        Ores = io.ok(Sout),
        io.format(Sout, "%s\n", [s(Log)], !IO),
        io.close_output(Sout, !IO)
    ).

% 2> friendly version
%log(Format, Args, !IO) :-
%    Log = string.format(Format, Args),
%    io.format(io.stderr_stream, "%s\n", [s(Log)], !IO).

% Release version!
%log(_Format, _Args, !IO).

%----------------------------------------------------------------------------%
%:- end_module pberr.
%----------------------------------------------------------------------------%
