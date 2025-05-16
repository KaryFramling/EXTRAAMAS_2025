# Define alternative vocabulary to the "defeult" one.
# Run "AmesHousingVocabulary.R" first before running this.
#

bob_voc_root_name <- "SalesPrice_Bob"

# Define Intermediate Concept names and labels
bob_IC_names <- c(bob_voc_root_name, "Surfaces")
bob_IC_labels <- c("House Sale Price", "Surfaces")

# Add Intermediate Concept vertices first, better for UI
g <- add_vertices(g, length(bob_IC_names), name = bob_IC_names, label = bob_IC_labels,
                  type = "Intermediate concept")

# We don't need to add basic feature vertices, they should be there already.

# Add "feature_of" edges
bob_house_features <- c("Surfaces")
bob_house_feature_edges <- interleave.value.in.array(bob_voc_root_name, bob_house_features)
bob_surfaces_features <- c("Garage_Area", "Total_Bsmt_SF", "Lot_Area")
bob_surfaces_feature_edges <- interleave.value.in.array("Surfaces", bob_surfaces_features)

# Create all "feature-of" edges
g <- add_edges(g, c(bob_house_feature_edges, bob_surfaces_feature_edges),
               relation="feature-of")

# Remove orphans
orphan_vertices <- V(g)[degree(g) == 0]
sxai.graph <- delete_vertices(g, orphan_vertices)

# Add this vocabulary to the XAI_vocabularies global variable
pmodel <- list(
  name = "Bob",
  root_concepts = bob_voc_root_name
)
sxai.partner.models[[pmodel$name]] <- pmodel

# Extract the sub-tree for Bob and plot it
subgraph <- sxai.get.sub.graph(g, pmodel$root_concepts)
plot(subgraph, vertex.label = V(subgraph)$name, edge.label = E(subgraph)$name)
