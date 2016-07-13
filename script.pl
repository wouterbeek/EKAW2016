:- module(script, [run/0]).

/** <module> EKAW 2016 experiment

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(debug)).
:- use_module(library(lodapi/lodapi_query)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf_prefix), []).

:- debug(http(parse)).
:- debug(http(send_request)).
:- debug(sparql(_)).

run :-
  q_member(P, [geop:hasBorderWith]),
  call_on_lod(_, P, _, dummy).

dummy(_, S, P, O, Doc) :-
  q_print_quad(S, P, O, Doc).
