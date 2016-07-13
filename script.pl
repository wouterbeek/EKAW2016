:- module(script, [run/0]).

/** <module> EKAW 2016 experiment

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(debug)).
:- use_module(library(lodapi/lodapi_gml)).
:- use_module(library(lodapi/lodapi_query)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf_prefix), []).

:- debug(http(parse)).
:- debug(http(send_request)).
:- debug(sparql(_)).

run :-
  q_member(P, [geop:hasBorderWith]),
  gml(_, P, _).
