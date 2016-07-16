:- module(script, [run/0]).

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
  q_member(
    P,
    [
      %foaf:knows,
      geop:hasBorderWith,
      gn:parentCountry,
      gr:includes,
      lexinfo:partOfSpeech,
      osspr:contains,
      osspr:within,
      skos:broader,
      skos:broadMatch,
      skos:closeMatch,
      skos:exactMatch,
      skos:hasTopConcept,
      skos:narrower,
      skos:narrowMatch,
      skos:related,
      skos:relatedMatch,
      swrc:affiliation
    ]
  ),
  run(P).

run(P) :-
  SinkOpts = [compression(false)],
  rdf2gml_start(SinkOpts, NFile, EFile, GFile, ExportOpts),
  call_to_streams(NFile, EFile, callback0(P, ExportOpts), SinkOpts),
  rdf2gml_end(NFile, EFile, GFile, SinkOpts).
  %g <- read_graph(GFile, format="gml").
  

callback0(P, ExportOpts, NOut, EOut) :-
  forall(ll_ldf(S, P, O), rdf2gml_triple(NOut, EOut, S, P, O, ExportOpts)).
