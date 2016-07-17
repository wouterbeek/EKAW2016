library(igraph);

sink("result.csv");

cat("#");
cat("file name");
cat(",");
cat("average degree");
cat(",");
cat("average in-degree");
cat(",");
cat("average out-degree");
cat(",");
cat("global clustering coefficient");
cat(",");
cat("local clustering coefficient");
cat(",");
cat("is bipartite?");
#cat(",");
#cat("number of components");
cat("\n");

inputFiles <- list.files(pattern = "*.gml");
for(i in 1:length(inputFiles)) {
  inputFile = inputFiles[i];
  cat(inputFile);
  g <- read.graph(inputFile, format="gml");
  
  degree <- degree(g, mode="total");
  avgDegree <- mean(degree);
  cat(",");
  cat(avgDegree);
  
  inDegree <- degree(g, mode="in");
  avgInDegree <- mean(inDegree);
  cat(",");
  cat(avgInDegree);
  
  outDegree <- degree(g, mode="out");
  avgOutDegree <- mean(outDegree);
  cat(",");
  cat(avgOutDegree);
  
  globalClusteringCoefficient <- transitivity(g, type="global");
  cat(",");
  cat(globalClusteringCoefficient);

  localClusteringCoefficient <- transitivity(g, type="localaverage");
  cat(",");
  cat(localClusteringCoefficient);

  b <- bipartite_mapping(g);
  cat(",");
  if (b["res"] == TRUE) {cat("TRUE");} else {cat("FALSE");}
  
  fitPowerLaw <- fit_power_law(inDegree + 1, 10);
  cat(",");
  cat(fitPowerLaw[["KS.p"]]);
  
  #comps <- components(g);
  #cat(",");
  #cat(length(comps));
  
  cat("\n");
}
