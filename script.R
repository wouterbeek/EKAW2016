library(igraph);
library(ineq);

sink("result.csv");

cat("#");
cat("file name");
cat(",");
cat("number of edges");
cat(",");
cat("number of vertices");
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
cat("bipartite");
cat(",");
cat("power law fit alpha");
cat(",");
cat("power law fit p-value");
cat(",");
cat("inequality coefficient of degree");
cat(",");
cat("inequality coefficient of component sizes");
cat("\n");

inputFiles <- list.files(pattern = "*.gml");
for(i in 1:length(inputFiles)) {
  inputFile = inputFiles[i];
  cat(inputFile);
  g <- read.graph(inputFile, format="gml");

  numberOfEdges <- gsize(g);
  cat(",");
  cat(numberOfEdges);

  numberOfVertices <- gorder(g);
  cat(",");
  cat(numberOfVertices);
  
  degree <- degree(g, mode="total");
  averageDegree <- mean(degree);
  cat(",");
  cat(averageDegree);
  
  inDegree <- degree(g, mode="in");
  averageInDegree <- mean(inDegree);
  cat(",");
  cat(averageInDegree);
  
  outDegree <- degree(g, mode="out");
  averageOutDegree <- mean(outDegree);
  cat(",");
  cat(averageOutDegree);
  
  globalClusteringCoefficient <- transitivity(g, type="global");
  cat(",");
  cat(globalClusteringCoefficient);

  localClusteringCoefficient <- transitivity(g, type="localaverage");
  cat(",");
  cat(localClusteringCoefficient);

  b <- bipartite_mapping(g);
  cat(",");
  if (b["res"] == TRUE) {cat("TRUE");} else {cat("FALSE");}
  
  # The exponent of the fitted power-law distribution.
  fitPowerLaw <- fit_power_law(inDegree + 1, 10);
  cat(",");
  cat(fitPowerLaw[["alpha"]]);
  
  # Small p-values (less than 0.05) indicate that the test rejected
  # the hypothesis that the original data could have been drawn from
  # the fitted power-law distribution.
  cat(",");
  cat(fitPowerLaw[["KS.p"]]);

  # Inequality coefficient of degree.
  ineqDegree <- ineq(degree, type="Gini");
  cat(",");
  cat(ineqDegree);
  
  # Inequality coefficient of component sizes.
  components <- components(g, mode ="weak");
  ineqComponents <- ineq(components$csize, type="Gini");
  cat(",");
  cat(ineqComponents);

  cat("\n");
}
