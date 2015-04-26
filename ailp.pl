/*
 *      ailp.pl
 *
 *		AI and Logic Programming assignment runner.
 *		Loads and runs specified assignmentN.pro file from folder assignmentN.
 */
 

:- dynamic
     user:prolog_file_type/2,
     user:file_search_path/2.

:- multifile
     user:prolog_file_type/2,
     user:file_search_path/2.


:-  % parse command line arguments
    current_prolog_flag(argv, _),
    % take first argument to be the name of the assignment folder
%    (Args = [Assignment_name|_]
%     ->  true
%     ;  % missing argument, so display syntax and halt
%        nl,
%        write('Syntax:'),nl,
%        nl,
%        write('  swipl -s ailp.pl <assignment_folder_name>'),nl,
%        write('  e.g. swipl -s ailp.pl assignment1'),nl,
%        nl,
%        halt(1)
%    ),
 	Assignment_name = 'oscar',
    assert(assignment(Assignment_name)),
    atom_concat(Assignment_name, '_library', Assignment_library_name),
    assert(assignment_library(Assignment_library_name)).


:-  % define a module path ailp_root()
    prolog_load_context(directory, Sys),
    (\+ user:file_search_path(ailp_root, Sys)
    ->  asserta(user:file_search_path(ailp_root, Sys))
    ),
    %
    % define a our own library path ailp_library()
    atom_concat(Sys, '/ailp', Lib),
    (\+ user:file_search_path(ailp_library, Lib)
    ->  asserta(user:file_search_path(ailp_library, Lib))
    ),
    % get assignment details
    assignment(Assignment_name),
    assignment_library(Assignment_library_name),
    %
    % define a module path for the specified assignment
    atomic_list_concat([Sys, '/', Assignment_name], Ass),
    (\+ user:file_search_path(ailp_assignment, Ass)
    ->  asserta(user:file_search_path(ailp_assignment, Ass))
    ),
    %
    % define a module path for the specified assignment library
    atom_concat(Ass, '/library', Asslib),
    (\+ user:file_search_path(ailp_assignment_library, Asslib)
    ->  asserta(user:file_search_path(ailp_assignment_library, Asslib))
    ),
    %
    load_files([
        ailp_library(command_channel),
        ailp_assignment(Assignment_name),
        ailp_assignment_library(Assignment_library_name)
    ],
    [
        silent(true)
    ]).

