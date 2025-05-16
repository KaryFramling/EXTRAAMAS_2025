# Utility functions to deal with graphs.

# Extract subgraph that starts from given node and (recursively) includes all
# "feature-of"-connected vertices. root_concept_names can be a single value or
# an array.
sxai.get.sub.graph <- function(g, root_concept_names, relation = "feature-of", mode = "out") {
  # BFS with a filter for edges with name = "feature_of"
  reachable_vertices <- bfs(
    g,
    root = root_concept_names,
    mode = mode,
    unreachable = FALSE,
    order = TRUE,
    dist = FALSE,
    extra = E(g)$name == relation
  )$order

  # Extract only the non-NA reachable vertices
  reachable_vertices <- reachable_vertices[!is.na(reachable_vertices)]

  # Get the names of reachable vertices
  reachable_vertex_names <- V(g)$name[reachable_vertices]

  # Create the subgraph
  subgraph <- induced_subgraph(g, vids = reachable_vertex_names)
  return(subgraph)
}


# Make a CIU-compatible vocabulary based on the graph and data column names
# Remember to remove the target value column(s) from the passed data!
# E.g. "column_names <- colnames[colnames != "Sale_Price"] so
# ciu.voc.from.graph(pg, colnames(ames_data)[colnames(ames_data) != "Sale_Price"])
ciu.voc.from.graph <- function(g, column_names) {
  voc <- list()
  for ( v in V(g)$name ) {
    voc[[v]] <- recursive.get.col.names.from.graph(g, v, column_names)
  }
  return(voc)
}


# Get all child nodes of the node with name "node_name" and whose edges
# have a "relation" attribute with the given name.
get.child.features <- function(g, node_name, relation = "feature-of") {
  filtered_edges <- E(g)[.from(node_name) & relation == relation]
  ends(g, filtered_edges)[,2]
}

# Find all leaf column names for the "concept" in the graph.
recursive.get.col.names.from.graph <- function(g, concept, column_names) {
  v <- V(g)[name == concept]
  if ( v$type == "Intermediate concept") {
    children <- get.child.features(g, concept)
    vc <- V(g)[name %in% children]
    leaf_concepts <- c()
    for ( n in children )
      leaf_concepts <- c(leaf_concepts, recursive.get.col.names.from.graph(g, n, column_names))
  }
  else
    return(which(column_names == concept))
  return(leaf_concepts)
}
