:- module(
  script,
  [
    lastfm/0,
    number_of_docs/0,
    run/0,
    run/1 % +P
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
:- use_module(library(ordsets)).
:- use_module(library(os/io)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf_prefix), []).
:- use_module(library(service/ll_index)).

:- debug(conv(rdf2gml)).
:- debug(http(parse)).
:- debug(http(send_request)).
:- debug(rdf(guess)).
:- debug(sparql(_)).





lastfm :-
  SinkOpts = [base_name(lastfm),compression(false)],
  rdf2gml_start(SinkOpts, NFile, EFile, GFile, ExportOpts),
  Prefix = 'http://dbtune.org/last-fm/',
  rdf_global_id(foaf:knows, P),
  ll_iri_docs(P, Docs1),
  ll_prefix_docs(Prefix, Docs2),
  ord_intersection(Docs1, Docs2, Docs),
  call_to_streams(NFile, EFile, lastfm_callback(Prefix, P, Docs, ExportOpts), SinkOpts),
  rdf2gml_end(NFile, EFile, GFile, SinkOpts).

lastfm_callback(Prefix, P, Docs, ExportOpts, NOut, EOut) :-
  forall(
    (
      member(Doc, Docs),
      ll_ldf(S, P, O, Doc),
      atom_prefix(S, Prefix),
      atom_prefix(O, Prefix)
    ),
    rdf2gml_triple(NOut, EOut, S, P, O, ExportOpts)
  ).



%! number_of_docs is det.
%
% Exports a table with the number of documents in which predicates
% appear at least once.
%
% @tbd Also record, for each document, the number of statements with
% the given predicate.

number_of_docs :-
  findall(NumDocs-[P,NumDocs], number_of_docs(P, NumDocs), Pairs),
  asc_pairs_values(Pairs, Lists),
  maplist(list_row, Lists, Rows),
  csv_to_file('number_of_docs.csv', Rows, [compression(false)]).


number_of_docs(P, NumDocs) :-
  property(Alias:Local),
  rdf_global_id(Alias:Local, P),
  docs(_, P, _, Docs),
  length(Docs, NumDocs).
number_of_docs(lastfm, NumDocs) :-
  ll_iri_docs(foaf:knows, Docs1),
  ll_prefix_docs('http://dbtune.org/last-fm/', Docs2),
  ord_intersection(Docs1, Docs2, Docs),
  length(Docs, NumDocs).



run :-
  property(P),
  run(P).


run(Alias:Local) :-
  atomic_list_concat([Alias,Local], '_', Base),
  SinkOpts = [base_name(Base),compression(false)],
  rdf2gml_start(SinkOpts, NFile, EFile, GFile, ExportOpts),
  rdf_global_id(Alias:Local, P),
  call_to_streams(NFile, EFile, callback(P, ExportOpts), SinkOpts),
  rdf2gml_end(NFile, EFile, GFile, SinkOpts).


callback(P, ExportOpts, NOut, EOut) :-
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
