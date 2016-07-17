:- module(
  script,
  [
    number_of_docs/0,
    run/0,
    run/1
  ]
).

/** <module> EKAW 2016 experiment

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(conv/rdf2gml)).
:- use_module(library(csv_ext)).
:- use_module(library(debug)).
:- use_module(library(list_ext)).
:- use_module(library(lodapi/lodapi_doc)).
:- use_module(library(lodapi/lodapi_gml)).
:- use_module(library(lodapi/lodapi_query)).
:- use_module(library(os/io)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf_prefix), []).
:- use_module(library(real)).

:- r(library(igraph)).

:- debug(conv(rdf2gml)).
:- debug(http(parse)).
:- debug(http(send_request)).
:- debug(rdf(guess)).
:- debug(sparql(_)).

run :-
  property(P),
  run(P).

run(Alias:Local) :-
  atomic_list_concat([Alias,Local], '_', Base),
  SinkOpts = [base_name(Base),compression(false)],
  rdf2gml_start(SinkOpts, NFile, EFile, GFile, ExportOpts),
  rdf_global_id(Alias:Local, P),
  call_to_streams(NFile, EFile, callback1(P, ExportOpts), SinkOpts),
  rdf2gml_end(NFile, EFile, GFile, SinkOpts).

number_of_docs :-
  findall(NumDocs-[P,NumDocs], number_of_docs(P, NumDocs), Pairs),
  asc_pairs_values(Pairs, Lists),
  maplist(list_row, Lists, Rows),
  csv_to_file('number_of_docs.csv', Rows).

number_of_docs(P, NumDocs) :-
  property(Alias:Local),
  rdf_global_id(Alias:Local, P),
  docs(_, P, _, Docs),
  length(Docs, NumDocs).

callback1(P, ExportOpts, NOut, EOut) :-
  forall(ll_ldf(S, P, O), rdf2gml_triple(NOut, EOut, S, P, O, ExportOpts)).

property(foaf:knows).
property(geop:hasBorderWith).
property(gn:parentCountry).
property(gr:includes).
property(lexinfo:partOfSpeech).
property(osspr:contains).
property(osspr:within).
property(skos:broader).
property(skos:broadMatch).
property(skos:closeMatch).
property(skos:exactMatch).
property(skos:hasTopConcept).
property(skos:narrower).
property(skos:narrowMatch).
property(skos:related).
property(skos:relatedMatch).
property(swrc:affiliation).
property(tags:associatedTag).
