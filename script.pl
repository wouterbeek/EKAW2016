:- module(script, [run/0]).

/** <module> EKAW 2016 experiment

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(conv/rdf2gml)).
:- use_module(library(debug)).
:- use_module(library(lodapi/lodapi_gml)).
:- use_module(library(lodapi/lodapi_query)).
:- use_module(library(os/io)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf_prefix), []).

:- debug(http(parse)).
:- debug(http(send_request)).
:- debug(rdf(guess)).
:- debug(sparql(_)).

run :-
  q_member(P, [geop:hasBorderWith]),
  rdf2gml_start(_{}, NFile, EFile, GFile, Opts2),
  call_to_streams(NFile, EFile, callback0(P, Opts2), [compression(false)]),
  rdf2gml_end(NFile, EFile, GFile).

callback0(P, Opts, NOut, EOut) :-
  forall(ll_ldf(S, P, O), rdf2gml_triple(NOut, EOut, S, P, O, Opts)).
