# Install necessary libraries
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("visNetwork", quietly = TRUE)) install.packages("visNetwork")
if (!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(shiny)
library(shinyBS)
library(visNetwork)
library(igraph)
library(ggplot2)
library(ciu)

# Check that we have all the required variables before launching.
# These are:
# sxai.ai.model: The model to explain.
# sxai.data: The instance data to use.
# sxai.ciu: The explainer.
# sxai.graph: Default knowledge graph that defines vocabulary etc.
# sxai.partner.models: A [list] of "partner models, where each model has at least
#   a "name" and a "root_concepts" that is the name (or array of names) of the
#  root concept(s) in the main graph for the vocabulary.
required_vars <- c("sxai.ai.model", "sxai.data", "sxai.ciu", "sxai.graph", "sxai.partner.models")
missing_vars <- required_vars[!sapply(required_vars, exists)]
if (length(missing_vars) > 0) {
  stop(paste("Error: The following required variables are missing:", paste(missing_vars, collapse = ", ")))
}

# Check that we have at least one valid vocabulary
if (length(sxai.partner.models) < 1) {
  stop("Error: There has to be at least one vocabulary in sxai.partner.models")
}

# # Force the app to open in an external browser
# options(shiny.launch.browser = TRUE)

# Initialize information for navigating through instances.
max_inst_ind <- nrow(sxai.data)

# Make visNetwork tooltips
V(sxai.graph)$title <- V(sxai.graph)$label
E(sxai.graph)$title <- E(sxai.graph)$relation

# Set initial value of "graph".
viz_graph <- sxai.get.sub.graph(sxai.graph, sxai.partner.models[[1]]$root_concepts)
min_CI_radius <- 1

# Set colors and options for coloring graph nodes.
nbr_palette_levels <- 100
color_palette <- colorRampPalette(c("darkred", "yellow", "darkgreen"))(nbr_palette_levels)
node_border_color <- "black"
default_node_color <- "lightblue"
default_node_border_color <- "#2B7CE9"

# Memorize what kind of plot we had selected for IC plot.
comparative_plot_type <- "PI-plot"

# App-specific functions
meta.explain <- function(instance, target.concept, concepts.to.explain, n.samples = 1000) {
  return(sxai.ciu$meta.explain(instance,
                               target.concept = target.concept,
                               concepts.to.explain=concepts.to.explain,
                               n.samples = n.samples))
}

# Define UI
ui <- fluidPage(
  # tags$head(
  #   tags$script(HTML("
  #     $(document).on('shiny:connected', function() {
  #       window.resizeTo(800, 600); // Set the desired width and height
  #     });
  #   "))
  # ),
  titlePanel("Interactive XAI with CIU"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_partner_model", "Partner model:",
                  choices = sapply(sxai.partner.models, function(x) x$name)),
      bsTooltip("selected_partner_model",
                "A partner model has its own vocabulary and interaaction preferences that can be adjusted for each explainee.",
                placement = "right", trigger = "hover"),
      selectInput("selected_node_list", "Select feature/concept:", choices = V(viz_graph)$name),
      bsTooltip("selected_node_list",
                "The explained feature or concept can be selected from this list or from the graph.",
                placement = "right", trigger = "hover"),
      HTML("<b>Description of feature/concept</b>"),
      htmlOutput("node_definition"),
      hr(),
      h3("Graph options"),
      checkboxInput("graph_by_ciu", "Illustrate CIU", value = FALSE),
      bsTooltip("graph_by_ciu",
                paste("Adjust node size by CI value and color according to CU value.",
                      "This can take long if there are many Intermediate Concepts (IC).",
                      "If so, then it can be made faster by reducing the number of",
                      "samples to use for IC estimation."),
                placement = "right", trigger = "hover"),
      numericInput("nsamples_IC", "# samples fo5r IC est.", min = 100, step = 100, value = 1000),
      bsTooltip("nsamples_IC", "The number of samples to use for estimating CIU of Intermediate Concepts",
                placement = "right", trigger = "hover"),
      checkboxInput("graph_hierarchical", "Graph as tree", value = FALSE),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(
          8,
          visNetworkOutput("network", height = "600px")
        ),
        column(
          4,
          numericInput("instance", "Instance #:", min = 1, value = 1, max = max_inst_ind),
          htmlOutput("instance_information"),
          uiOutput("xai_plot"),
          tags$style(HTML("
            #text_explanation {
              width: 100%;
              height: 200px;
              overflow-y: scroll;
              border: 1px solid #ccc;
              padding: 10px;
              background-color: #f9f9f9;
            }
          ")),
          htmlOutput("text_explanation")
        )
      ),
      width = 9
    )
  )
)

# Define server
server <- function(input, output, session) {

  # We want changes to these values to cause dependent Shiny elements to update.
  reactives <- reactiveValues(
    partner = sxai.partner.models[[1]],
    instance = NULL,
    viz_graph = viz_graph,
    meta_ciu = NULL,
    visnet = NULL,
    ciu_colored_vis = NULL,
    contrastive_instance_2 = min(2, max_inst_ind)
  )

  # Update ciu result if instance or concept has changed.
  observe({
    # We have to check that both graph and selected_node_list have been updated
    # if ever there has been a change in partner model.
    req(input$selected_node_list %in% V(reactives$viz_graph)$name)

    # Everything should be OK and we can go ahead.
    reactives$instance <- subset(sxai.data[input$instance,], select=-Sale_Price) # THIS IS NOT GOOD, DATA SET SPECIFIC!
    target_concept <- concept <- input$selected_node_list
    g <- reactives$viz_graph

    # We don't want to have a target concept for top-most nodes.
    if ( length(E(g)[.to(target_concept) & relation=="feature-of"]) == 0 )
      target_concept <- NULL

    # Different plots for ICs and basic features.
    if ( V(g)[concept]$type == "Intermediate concept") {
      child_features <- get.child.features(g, concept)
      reactives$meta_ciu <- meta.explain(reactives$instance, target_concept, child_features, n.samples = input$nsamples_IC)
      #V(g)[child_features]$utility <- as.numeric(lapply(reactives$meta_ciu$ciuvals, function(df) df$CU))
    }
    else
      reactives$meta_ciu <- NULL
  })

  # Render visNetwork graph
  output$network <- renderVisNetwork({
    if ( input$graph_by_ciu ) {
      if ( is.null(reactives$ciu_colored_vis )) {
        ciu_g <- ciu.igraph.explain(reactives$instance, sxai.ciu, reactives$viz_graph,
                                    reactives$partner$root_concepts[1])
        reactives$ciu_colored_vis <-
          ciu.plots.visnetwork(ciu_g, min_radius = min_CI_radius,
                               cu_color_palette = color_palette,
                               hierarchical_layout = input$graph_hierarchical)
      }
      visnet <- reactives$ciu_colored_vis
    }
    else {
      vis <- toVisNetworkData(reactives$viz_graph)
      visnet <- visNetwork(vis$nodes, vis$edges) %>%
        visNodes(size = 30, font = list(size = 35))
      if ( input$graph_hierarchical ) {
        visnet <- visnet %>%
          visHierarchicalLayout()  # Apply hierarchical (tree) layout
      }
    }
    visnet <- visnet %>%
      visOptions(highlightNearest = TRUE) %>% #, selectedBy = "id") %>%
      visEvents(selectNode = "function(nodes) {
        Shiny.setInputValue('selected_node_id', nodes.nodes[0]);
      }")
    reactives$visnet <- visnet
    return(visnet)
  })

  # Change the partner model. This needs to be reflected in the graph, as well
  # as in the concept menu, as well as resetting CIU results (and potentially
  # other things too).
  observeEvent(input$selected_partner_model, {
    # Initialize reactives$viz_graph either directly to partner model's igraph
    # or extract the graph as a subgraph from the "default" one.
    reactives$partner <- sxai.partner.models[[input$selected_partner_model]]
    if ( is.null(reactives$partner$graph) ) {
      g <- sxai.get.sub.graph(sxai.graph, reactives$partner$root_concepts)
    }
    else {
      g <- reactives$partner$graph
    }

    # Update list of concepts
    updateSelectInput(session, "selected_node_list",
                      choices = V(g)$name,
                      selected = reactives$partner$root_concepts[1])

    # Launch reactive.
    reactives$viz_graph <- g

    # Reset ciu results
    reactives$meta_ciu <- NULL
    reactives$ciu_colored_vis <- NULL
  })

  # Here we need to reset CI values in the graph, as well as in the visNetwork
  observeEvent(input$instance, {
    # if ("utility" %in% vertex_attr_names(reactives$viz_graph)) {
    #   reactives$viz_graph <- delete_vertex_attr(reactives$viz_graph, "utility")
    # }
    reactives$ciu_colored_vis <- NULL
  })

  # React to vertex clicks
  observeEvent(input$selected_node_id, {
    updateSelectInput(session, "selected_node_list", selected = input$selected_node_id)
  })

  # We have to force an update of visNetwork when this happens
  observeEvent(input$graph_hierarchical, {
    reactives$visnet <- NULL
    reactives$ciu_colored_vis <- NULL
  })

  # Set selection for visNetwork
  observe({
    visNetworkProxy("network") %>%
      visSelectNodes(input$selected_node_list)
  })

  # Show description/definition of node
  output$node_definition <- renderText({
    # We have to check that both graph and selected_node_list have been updated
    # if ever there has been a change in partner model.
    req(input$selected_node_list %in% V(reactives$viz_graph)$name)

    # Now we can go ahead
    g <- reactives$viz_graph
    node <- V(g)[input$selected_node_list]
    description <- node$description
    if ( is.na(description) || is.null(description) ) {
      if ( !(is.na(node$type) || is.null(node$type)) && node$type == "Intermediate concept" )
        description = "This is an Intermediate Concept"
      else
        description <- "No description for selected node."
    }
    else {
      data_col <- sxai.data[,V(g)[input$selected_node_list]$name]
      if ( is.factor(data_col) ) {
        vals <- unlist(levels(data_col))
        description <- paste(description, "<br>This is a <b>categorical</b> feature.<br>",
                             "Possible values are: ",
                             paste(vals, collapse = ", "))
      }
      else if ( is.numeric(data_col) ) {
        description <- paste(description, "<br>This is a <b>numeric</b> feature.<br>")
      }
    }
    return(description)
  })

  # Show information about the instance, predicted output value
  output$instance_information <- renderText({
    req(input$instance)
    ciu_obj <- sxai.ciu$as.ciu()
    min_out_val <- ciu_obj$abs.min.max[1]
    max_out_val <- ciu_obj$abs.min.max[2]
    outval <- predict(sxai.ai.model, newdata=sxai.data[input$instance,])
    s <- paste0("Predicted output value: ", round(outval, 3),
                " is at ",
                signif(((outval - min_out_val)/(max_out_val - min_out_val))*100, 2),
                "% of the possible output value interval [", min_out_val, ",",
                max_out_val, "]."
    )
  })

  # Memorize what plot type was selected so that we can restore it for the
  # dynamic IC plot after change of selected concept or whatever.
  observeEvent(input$comparative_plot_type, {
    comparative_plot_type <<- input$comparative_plot_type
  })

  # Render the dynamic UI for CIU bar plot explanations
  output$xai_plot <- renderUI({
    # We have to check that both graph and selected_node_list have been updated
    # if ever there has been a change in partner model.
    req(input$selected_node_list %in% V(reactives$viz_graph)$name)

    # Now we should be fine to go ahead
    g <- reactives$viz_graph
    concept <- input$selected_node_list
    if ( V(g)[concept]$type == "Intermediate concept") {
      verticalLayout(
        radioButtons(inputId = "comparative_plot_type",
                     label = "Visualisation type:",
                     choices = c("PI-plot", "Influence", "Contrastive"),
                     selected = comparative_plot_type,
                     inline = TRUE
        ),
        uiOutput("xai_plot_options"),
        plotOutput("ciuplot_output", height = "300px")
      )
    }
    else {
      verticalLayout(
        checkboxInput(inputId = "illustrate_ciu", label = "Illustrate CIU?", value = TRUE),
        plotOutput("ciuplot_output", height = "300px")
      )
    }
  })

  # Memorize what instance id the second to use for contrastive comparisons.
  observeEvent(input$contrastive_instance_2, {
    reactives$contrastive_instance_2 <- input$contrastive_instance_2
  })

  # Render the dynamic UI for the parameters of CIU plot explanations
  output$xai_plot_options <- renderUI({
    if (input$comparative_plot_type == "Contrastive") {
      numericInput("contrastive_instance_2", "Compare with Instance #:", min = 1,
                   value = reactives$contrastive_instance_2, max = max_inst_ind)
    }
  })

  # Bar plot explanation for ICs, IO plot for basic features.
  output$ciuplot_output <- renderPlot({
    # We have to check that both graph and selected_node_list have been updated
    # if ever there has been a change in partner model.
    req(input$selected_node_list %in% V(reactives$viz_graph)$name)

    # Now we should be fine to go ahead
    # Different plots for ICs and basic features.
    g <- reactives$viz_graph
    concept <- input$selected_node_list
    if ( V(g)[concept]$type == "Intermediate concept") {
      req(reactives$meta_ciu)
      if (input$comparative_plot_type == "Contrastive") {
        inst2_ind <- reactives$contrastive_instance_2
        instance2 <- subset(sxai.data[inst2_ind,], select=-Sale_Price) # THIS IS NOT GOOD, DATA SET SPECIFIC!
        target_concept <- concept <- input$selected_node_list
        if ( length(E(g)[.to(target_concept) & relation=="feature-of"]) == 0 )
          target_concept <- NULL
        child_features <- get.child.features(g, concept)
        meta_ciu2 <- meta.explain(instance2, target_concept, child_features, n.samples = input$nsamples_IC)
        ciuvals1 <- ciu.list.to.frame(reactives$meta_ciu$ciuvals)
        ciuvals2 <- ciu.list.to.frame(meta_ciu2$ciuvals)
        contrastive <- ciu.contrastive(ciuvals1, ciuvals2)
        p <- ciu.ggplot.contrastive(reactives$meta_ciu, contrastive) +
          theme(legend.position = "none") +
          labs(title = paste0("Instance ", input$instance, " vs. instance ", inst2_ind))
      }
      else {
        use_influence <- ifelse(input$comparative_plot_type == "PI-plot", FALSE, TRUE)
        p <- sxai.ciu$ggplot.col.ciu(ciu.meta = reactives$meta_ciu,
                                     plot.mode = "overlap", use.influence = use_influence)
      }
    }
    else {
      instance <- subset(sxai.data[input$instance,], select=-Sale_Price)

      # We have to deal with the case that the input hasn't been rendered yet.
      if (is.null(input$illustrate_ciu))
        illustrate.ciu <- TRUE
      else
        illustrate.ciu <- input$illustrate_ciu
      p <- sxai.ciu$ggplot.ciu(instance, which(colnames(instance) == concept),
                               illustrate.CIU = illustrate.ciu)
    }
    return(p)
  })

  # Show textual explanation
  output$text_explanation <- renderText({
    # We have to check that both graph and selected_node_list have been updated
    # if ever there has been a change in partner model.
    req(input$selected_node_list %in% V(reactives$viz_graph)$name)

    # Now we should be fine to go ahead
    # Different plots for ICs and basic features.
    g <- reactives$viz_graph

    # Different explanations for ICs and basic features.
    concept <- input$selected_node_list
    if ( V(g)[concept]$type == "Intermediate concept") {
      req(reactives$meta_ciu)
      s <- paste("<b>Textual explanation</b><br>",
                 sxai.ciu$textual(ciu.meta = reactives$meta_ciu))
    }
    else {
      instance <- subset(sxai.data[input$instance,], select=-Sale_Price)
      inp_ind <- which(colnames(instance) == concept)
      ciuvals <- sxai.ciu$explain(instance, ind.inputs.to.explain = inp_ind)
      s <- paste0(
        "The feature <b>", V(g)[concept]$label, "</b>'s importance is ",
        signif(ciuvals$CI*100, 3), "%. The utility of the feature's value <i>",
        instance[,inp_ind],
        "</i> is ", signif(ciuvals$CU*100, 3), "%.<br>",
        "The feature's influence is ", signif(sxai.ciu$influence(ciuvals), 3),
        " when 'neutral' utility is ", 0.5, ".<br><br>"
      )
      s <- paste(
        s,
        "The importance, utility and influence values can be 'read' directly",
        "from the <i>Input-Output (IO) plot</i> above. An IO plot shows how the output value ",
        "changes as a function of the selected input/feature value, ",
        "while maintaining the values of other features fixed at the",
        "ones of the studied instance.<br>",
        "The plot shows the whole range of possible output values on the y-axis,",
        "as indicated by 'MIN', 'MAX' and the corresponding blue lines.",
        "If the output value changes a lot when modifying the feature",
        "value (as indicated by the red 'ymin' and green 'ymax' lines)",
        "then the feature is important. <br>",
        "The current input value and the corresponding output value",
        "are indicated by the red dot in the plot. If the red dot is",
        "in the upper part of the [ymin, ymax] range, then the input",
        "value has 'high utility' (i.e. is 'good', 'typical', ...)",
        "and, correspondingly, if it is in the lower part of the range,",
        "then it has low utility (i.e. is 'bad', not typical', ...).<br>",
        "The influence value is calculated as <i>importance*(utility - neutral)</i>",
        "and indicates to what extent the feature has a positive or negative",
        "influence on the result compared to a reference value 'neutral'.<br>",
        "Influence can also be used for contrastive explanations that compare",
        "two instances with each other."
      )
    }
    return(s)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
