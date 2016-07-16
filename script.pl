:- module(script, [run/0,number_of_graphs/2]).

/** <module> EKAW 2016 experiment

library('igraph') #load igraph library
g <- read.graph("skoshastopcon.graphml",format=c("graphml") #read graphml file
mean(degree(g,mode = c("all") #average degree
mean(degree(g,mode =c("in") #in or out degree
transitivity(g,type = "global") #Global clustering coefficient
transitivity(g,type = "localaverage") #Local clustering coefficient
c <- components(g, mode ="weak") #number of connected components
bipartite_mapping(g) #Bipartite test
fit1 <- fit_power_law(d+1,10) #calculates power law fit
fit1 #shows p-value
fit2 <- fit_power_fit(d+1,10,implementation = "R.mle") #power law fit approximation to exponent 3. Suggested by R documentation
stats4::coef(fit2) #Corresponding alpha
library("ineq") #Loads inequality libraries
d<-degree(g,mode = c("all") #Average degree
ineq(d,type = "Gini") #Inequality coeff. of degrees
ineq(c$csize,type = "Gini") #Inequality coeff. of component sizes

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(conv/rdf2gml)).
:- use_module(library(debug)).
:- use_module(library(lodapi/lodapi_doc)).
:- use_module(library(lodapi/lodapi_gml)).
:- use_module(library(lodapi/lodapi_query)).
:- use_module(library(os/io)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf_prefix), []).
:- use_module(library(real)).

:- r(library(igraph)).

:- debug(http(parse)).
:- debug(http(send_request)).
:- debug(rdf(guess)).
:- debug(sparql(_)).

run :-
  property(Alias:Local),
  atomic_list_concat([Alias,Local], '_', Base),
  SinkOpts = [base_name(Base),compression(false)],
  rdf2gml_start(SinkOpts, NFile, EFile, GFile, ExportOpts),
  rdf_global_id(Alias:Local, P),
  call_to_streams(NFile, EFile, callback0(P, ExportOpts), SinkOpts),
  rdf2gml_end(NFile, EFile, GFile, SinkOpts).
  
number_of_graphs(P, NumDocs) :-
  property(Alias:Local),
  rdf_global_id(Alias:Local, P),
  docs(_, P, _, Docs),
  length(Docs, NumDocs).

callback0(P, ExportOpts, NOut, EOut) :-
  forall(ll_ldf(S, P, O), rdf2gml_triple(NOut, EOut, S, P, O, ExportOpts)).

%property(foaf:knows).
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
