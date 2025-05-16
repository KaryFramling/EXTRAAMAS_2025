# # Install required packages if not already installed
# install.packages("igraph")
# install.packages("visNetwork")
# install.packages("scales")

# Load libraries
library(igraph)
library(visNetwork)
library(scales)  # For rescaling CI and CU values

# voc_graph <- sxai.get.sub.graph(sxai.graph, sxai.partner.models[[1]]$root_concepts)
ciu.igraph.explain <- function(instance, CIU_object, voc_graph, concept,
                               out_index = 1, scale_to_IC = TRUE, IC_samples = 500,
                               level = 1) {
  g <- voc_graph # Make code shorter

  # Check if this is a topmost concept.
  if ( length(E(g)[.to(concept) & relation=="feature-of"]) == 0 ) {
    V(g)[concept]$CI <- 1.0 # Must be so by definition
    # Temporary fix: we assume that output utility function is linear
    ciu <- sxai.ciu$as.ciu()
    min <- ciu$abs.min.max[out_index,1]
    max <- ciu$abs.min.max[out_index,2]
    outval <- ciu$predict.function(ciu$model, instance)
    V(g)[concept]$CU <- (outval - min)/(max - min)
    target_concept <- NULL # Expected by "meta.explain" for root concept.
    V(g)[concept]$level <- level
  }
  else
    target_concept <- concept

  # Then go for the child concepts.
  if ( V(g)[concept]$type == "Intermediate concept") {
    target_CI <- V(g)[concept]$CI
    child_concepts <- get.child.features(g, concept)
    meta_ciu <- CIU_object$meta.explain(instance,
                                        target.concept = target_concept,
                                        concepts.to.explain = child_concepts,
                                        n.samples = IC_samples)
    CIval <- as.numeric(lapply(meta_ciu$ciuvals, function(df) df$CI))
    if ( scale_to_IC )
      CIval <- target_CI*CIval
    V(g)[child_concepts]$CI <- CIval
    V(g)[child_concepts]$CU <- as.numeric(lapply(meta_ciu$ciuvals, function(df) df$CU))
    V(g)[child_concepts]$level <- level + 1
  }

  # Call function recursively for all child concepts that are ICs
  for ( c in child_concepts ) {
    if ( V(g)[c]$type == "Intermediate concept")
      g <- ciu.igraph.explain(instance, CIU_object, g, c,
                              out_index, scale_to_IC, IC_samples,
                              level = level + 1)
  }

  # We are ready, return final graph
  return(g)
}

ciu.plots.visnetwork <- function(graph, hierarchical_layout = FALSE,
                                 default_radius = 20, min_radius = 0, max_radius = 50,
                                 CI_as_area = TRUE,
                                 default_node_color = "lightblue", default_node_border_color = "darkblue",
                                 cu_color_palette = colorRampPalette(c("red", "yellow", "darkgreen"))(100)
                                 ) {

  # Scale node area (CI) -> Convert to radius
  if ( is.null(V(graph)$CI) )
    diam_value <- node_radius <- default_radius
  else {
    if ( CI_as_area ) {
      node_radius <- sqrt(rescale(V(graph)$CI, to = c(min_radius^2, max_radius^2)))  # Apply sqrt for area-based scaling
      diam_value <- node_radius * 2
    }
    else
      diam_value <- rescale(V(graph)$CI, to = c(min_radius, max_radius))*2
  }

  # Create a color gradient for CU using red-yellow-darkgreen
  if ( is.null(V(graph)$CU) ) {
    V(graph)$color <- default_node_color
    V(graph)$color.border <- default_node_border_color
  }
  else {
    CUs <- rescale(pmax(0, pmin(1, V(graph)$CU)), from = c(0,1), to = c(1,100))
    V(graph)$color.border <- V(graph)$color <- cu_color_palette[CUs]
  }

  # Node size
  V(graph)$value <- diam_value

  # Make visNetwork tooltips
  if ( !is.null(V(graph)$label) && !is.null(V(graph)$CI) ) {
    t <- paste0(V(graph)$label, "<br>CI: ", sprintf("%.3f", V(graph)$CI))
    if ( !is.null(V(graph)$CU) )
      t <- paste0(t, "<br>CU: ", format(V(graph)$CU, digits = 3))
    V(graph)$title <- t
  }
  if ( !is.null(E(graph)$relation) )
    E(graph)$title <- E(graph)$relation

  # Convert igraph into visNetwork data structure
  vis <- toVisNetworkData(graph)

  # Create interactive visNetwork graph with directed edges
  visnet <- visNetwork(vis$nodes, vis$edges) %>%
    visNodes(font = list(multi = TRUE), scaling = list(min = min_radius, max = max_radius)) %>%
    visEdges(arrows = "to", smooth = FALSE) %>%  # Ensure directed edges
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visPhysics(stabilization = TRUE)

  # Options for the network
  if ( hierarchical_layout ) {
    visnet <- visnet %>%
    visHierarchicalLayout()  # Apply hierarchical (tree) layout
  }
  return(visnet)
}

voc_graph <- sxai.get.sub.graph(sxai.graph, sxai.partner.models[[1]]$root_concepts)
ciu_g <- ciu.igraph.explain(instance, sxai.ciu, voc_graph, "SalesPrice_def_voc")
print(ciu.plots.visnetwork(ciu_g))
