

########################################
# Avoid messages when read_csv is called
nodes_cols <- cols( Id = col_double(), LABEL = col_character(), SEQ_NUM = col_double(), HADM_ID = col_double() )
edges_cols <- cols( Id = col_double(), Source = col_double(), Target = col_double(), Weight = col_double() )
########################################

shinyServer(
  function(input, output) {
    
  ####################################
  # Dynamic variables
  ###################################
  dyn <- reactiveValues() # Dynamic values read from external files
  selected <- reactiveValues(
    sel_nodes = list(NA,NA,NA,NA), 
    sel_num_patients = list(0,0,0,0), 
    previous_selected_nodes = vector() ,
    nodes_sel = list(NA,NA,NA,NA)) 
  num_select <- reactiveVal(0)
  ####################################
  # Title: Add the ICD code
  ###################################
  output$title <- renderText({  
    row_id <- which(diagnosis_title$icd == input$icd)
    paste0("ICD Code ", input$icd, ": ", diagnosis_title$LONG_TITLE[row_id])
  })
  
  ####################################
  # Observer for ICD code
  ###################################
  observeEvent(input$icd, {
    
    # Selected ICD code
    icd <- input$icd # "03849"
    
    # Define the graph
    
    # Nodes
    nodes <- read_csv(paste0("data/icd_list/a_", icd, "_nodes.csv"), col_types = nodes_cols ) %>%
      rename(id = Id, label = HADM_ID)
    # Edges
    dyn$edges <- read_csv(paste0("data/icd_list/a_", icd, "_edges.csv"), col_types = edges_cols ) %>%
      select(-Id) %>%
      rename(from = Source, to = Target, weight = Weight)
    # Create graph object
    dyn$graph <- graph.data.frame(dyn$edges, directed = FALSE, vertices = nodes)
    # Community detection
    nodes$communities <- as.numeric(membership(cluster_louvain(dyn$graph, weights = 1 - dyn$edges$weight)))
    # Re-label communities
    # Compute proportions
    tables_communities <- sort(prop.table(table(nodes$communities)), decreasing = TRUE )
    
    if (length(tables_communities) > 12) {
      re_label <- data.frame(communities = as.numeric(names(tables_communities)),
                             proportion = round(as.numeric(tables_communities) * 100, 1) ) %>%
        mutate(group = ifelse(row_number() < 12, LETTERS[row_number()], "L. Other" ))
    } else {
      re_label <- data.frame(communities = as.numeric(names(tables_communities)),
                             proportion = round(as.numeric(tables_communities) * 100, 1) ) %>%
        mutate(group = LETTERS[row_number()] )
    }

    # Recompute proportions
    re_label_porp <- re_label %>% 
      group_by(group) %>%
      summarise(proportion = sum(proportion)) %>%
      ungroup() %>%
      mutate(group_label =  paste0(group, " ", proportion, "%")) %>%
      select(-proportion)
    # Join
    re_label <- re_label %>% 
      select(-proportion) %>%
      inner_join(re_label_porp, by = "group") %>%
      select(-group) %>%
      rename(group = group_label)
  
    # Updating nodes
    nodes <- nodes %>%
      inner_join(re_label, by = "communities")
    
    # ---- #
    dyn$nodes <- nodes
    
    re_label_comp <- data.frame(group = letters[nrow(re_label_porp):12], group_label = letters[nrow(re_label_porp):12] )
    # print(re_label_comp)
    # print(re_label_porp)
    dyn$re_label_porp <- rbind(re_label_porp, re_label_comp)
    
    # Layout positions
    dyn$positions <- readRDS(paste0("data/layout/a_", icd, "_layout.rds"))
    colnames(dyn$positions) <- c("x", "y")
    
  })
  
  
  ####################################
  # Network plot
  ###################################
  output$network_id <- renderVisNetwork({
      visNetwork(dyn$nodes, dyn$edges, width = "100%") %>%
      visNodes(size = input$node_size) %>% 
      visEdges() %>%
      visIgraphLayout(layout = "layout.norm", layoutMatrix = dyn$positions) %>%
      visOptions(highlightNearest = TRUE,
                 selectedBy = list(variable = "group")) %>%
      visInteraction(navigationButtons = TRUE,
                     dragNodes = TRUE, 
                     dragView = TRUE,
                     zoomView = FALSE,
                     multiselect = TRUE) %>%
      visLegend(ncol = 2, width = 0.15) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[1] ,  color = "#8dd3c7" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[2] ,  color = "#ffffb3" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[3] ,  color = "#bebada" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[4] ,  color = "#fb8072" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[5] ,  color = "#80b1d3" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[6] ,  color = "#fdb462" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[7] ,  color = "#b3de69" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[8] ,  color = "#fccde5" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[9] ,  color = "#ffed6f" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[10] ,  color = "#bc80bd" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[11] ,  color = "#ccebc5" ) %>%
      visGroups(groupname = dyn$re_label_porp$group_label[12] ,  color = "#d9d9d9" )
    
  })
  ####################################
  # Observer Selection network
  ###################################
  observeEvent({
    input$select
    input$obs
    },{
      
      if ( !sjmisc::is_empty(input$network_id_selectedBy) ) {
        selected_nodes <- which(as.character(dyn$nodes$group) == input$network_id_selectedBy)
      } else {
        selected_nodes <- c(input$network_id_highlight_color_id, input$network_id_highlight_label_id)
      }

      if ( length(selected_nodes) > 0 ) {
        # Nodes selected from interaction
        # We filter the number of nodes selected from VisNetwork
        nodes_sel <- dyn$nodes %>%
          mutate(id  = row_number()) %>%
          filter(id %in% selected_nodes) %>%
          rename(HADM_ID = label)
        
                
        # List of diagnosis for all patients selected
        # We create the full list of diagnosis for the selected patients
        nodes_diag <- nodes_sel %>%
          select(HADM_ID) %>%
          inner_join(diagnosis, by = "HADM_ID") %>%
          select(HADM_ID, SEQ_NUM, ICD, LONG_TITLE)
        
        # -----------------------------------
        # Summary number to compute percetage
        # -----------------------------------
        #  Compute proportion of diagnosis per SEQ_NUM
        nodes_by_seq_num <- nodes_diag %>%
          group_by(SEQ_NUM) %>%
          summarize(num_seq_num = n() ) %>%
          ungroup()
        # -----------------------------------

        
        # Compute proportion of diagnosis per ICD and SEQ_NUM (nodes_by_seq_num_and_icd)
        sel_nodes <- nodes_diag %>%
          inner_join(nodes_by_seq_num, by = "SEQ_NUM") %>%
          group_by(ICD, SEQ_NUM, num_seq_num) %>%
          summarize(num_seq_num_and_icd = n() )  %>%
          ungroup() %>%
          # Define proportion
          mutate(freq_seq_num_and_icd = 100 * round(num_seq_num_and_icd / num_seq_num, 2) ) %>%
          # Add labels
          inner_join( unique(nodes_diag %>% select(ICD, SEQ_NUM, LONG_TITLE)), by = c("ICD", "SEQ_NUM") ) %>%
          group_by(ICD, SEQ_NUM, num_seq_num, num_seq_num_and_icd) %>%
          summarise(LABEL = paste(LONG_TITLE, collapse = "<br>")) %>%
          ungroup() 
        
        
        # Save the generate table inside the dynamic variable selected$sel_nodes
        if (any(is.na(selected$sel_nodes[[1]])) & !identical(selected_nodes, selected$previous_selected_nodes)) {
          selected$sel_nodes[[1]] <-  sel_nodes
          selected$sel_num_patients[[1]] <- nrow(nodes_sel)
          selected$previous_selected_nodes <- selected_nodes
          selected$nodes_sel[[1]] <- nodes_sel
          #num_select(1)
        } else if (any(is.na(selected$sel_nodes[[2]])) & !identical(selected_nodes, selected$previous_selected_nodes)) {
          selected$sel_nodes[[2]] <-  sel_nodes
          selected$sel_num_patients[[2]] <- nrow(nodes_sel)
          selected$previous_selected_nodes <- selected_nodes
          selected$nodes_sel[[2]] <- nodes_sel
          #num_select(2)
        } else if (any(is.na(selected$sel_nodes[[3]])) & !identical(selected_nodes, selected$previous_selected_nodes)) {
          selected$sel_nodes[[3]] <-  sel_nodes
          selected$sel_num_patients[[3]] <- nrow(nodes_sel)
          selected$previous_selected_nodes <- selected_nodes
          selected$nodes_sel[[3]] <- nodes_sel
          #num_select(3)
        } else if (any(is.na(selected$sel_nodes[[4]])) & !identical(selected_nodes, selected$previous_selected_nodes)) {
          selected$sel_nodes[[4]] <-  sel_nodes
          selected$sel_num_patients[[4]] <- nrow(nodes_sel)
          selected$previous_selected_nodes <- selected_nodes
          selected$nodes_sel[[4]] <- nodes_sel
          #num_select(4)
        }

        
        # -----------------------------------------------------
        # Update/Set Filter ICD codes higher than the paremeter
        # -----------------------------------------------------
        for( it in 1:length(selected$sel_nodes)) {
          
          if (any(!is.na(selected$sel_nodes[[it]]))) {
            
            # Filter ICD: Values 0 or 1
            selected$sel_nodes[[it]] <- selected$sel_nodes[[it]] %>%
              mutate(filter = ifelse(num_seq_num_and_icd /  selected$sel_num_patients[[it]] < input$obs/100, 0, 1))
            
            # Define the list of inherited icd from iteration
            inherited_icd <- selected$sel_nodes[[it]] %>%
              select(ICD, filter) %>%
              filter(filter == 1) %>%
              rename(inherited = filter)
            
            # Append into the global list        
            if(it == 1)  {
              all_inherited_icd <- inherited_icd
            } else {
              all_inherited_icd <- rbind(all_inherited_icd, inherited_icd)
            }
            
          }
        }
        # -----------------------------------------------------
        
        # -----------------------------------------------------
        # Adding inherited ICD to the current sel_nodes
        # -----------------------------------------------------
        all_inherited_icd <-  unique(all_inherited_icd)
        
        for( it in 1:length(selected$sel_nodes)) {
          
          if (any(!is.na(selected$sel_nodes[[it]]))) {
            # Trick: inherited is create and removed to avoid errors
            selected$sel_nodes[[it]] <- selected$sel_nodes[[it]] %>%
              mutate(inherited = 0) %>%
              select(-inherited) %>%
              left_join(all_inherited_icd, by = "ICD") %>%
              mutate(inherited = ifelse(is.na(inherited), 0, inherited))
          } 
        }
        # -----------------------------------------------------

        # -----------------------------------------------------
        # Execute plots
        # -----------------------------------------------------
        # Define the color scale 
        domain_size <- nrow(all_inherited_icd)
          pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
                   "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", 
                   "#666666", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", 
                   "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#FBB4AE", 
                   "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", 
                   "#F2F2F2", "#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", 
                   "#F1E2CC", "#CCCCCC", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
                   "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", 
                   "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", 
                   "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", 
                   "#BC80BD", "#CCEBC5", "#FFED6F"
          )
          all_inherited_icd$range <- pal[1:domain_size]
          # Add Other
          all_inherited_icd <- rbind(all_inherited_icd, c("Other", 1, "#C0C0C0"))
          
        
        for( it in 1:length(selected$sel_nodes)) {
          
          if (any(!is.na(selected$sel_nodes[[it]]))) {
            sel_nodes <- selected$sel_nodes[[it]] %>%
              # Details
              mutate(SEQ_NUM = as.factor(SEQ_NUM),
                     ICD = ifelse(filter == 0 &  inherited == 0, "Other", ICD)) %>%
              arrange(ICD, SEQ_NUM, desc(num_seq_num_and_icd))
            
            # Plot object
            plot <- sel_nodes %>%
              ggvis(y = ~SEQ_NUM, fill = ~ICD, stroke := "black", fillOpacity := 0.85, strokeWidth := 0.3) %>%
              compute_stack(stack_var = ~num_seq_num_and_icd, group_var = ~SEQ_NUM) %>%
              layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
              scale_logical("fill", range = all_inherited_icd$range, domain = all_inherited_icd$ICD ) %>%
              set_options(width = "auto", height = 480) %>%
              add_axis("x", title = "Number of patients", title_offset = 50) %>%
              add_axis("y", title = "Order of importance", title_offset = 50) %>%
              add_tooltip(function(x){
                if (x$ICD !=  "Other") {
                  sel <- which(all_sel_nodes$ICD == x$ICD & all_sel_nodes$SEQ_NUM == x$SEQ_NUM)
                  c( paste0(x$ICD,"<br>"), all_sel_nodes$LABEL[sel])
                } else {
                  "Other"
                }
              }, "hover")
            
            # Assign plot to the available slot
            if (it == 1) {
              plot %>% bind_shiny("dist_codes1")
              show("dist_codes1")
              show("summ_codes1")
              all_sel_nodes <- sel_nodes
            } else if (it == 2) {
              plot %>% bind_shiny("dist_codes2")
              show("dist_codes2")
              show("summ_codes2")
              all_sel_nodes <- rbind(all_sel_nodes, sel_nodes)
            } else if (it == 3) {
              plot %>% bind_shiny("dist_codes3")
              show("dist_codes3")
              show("summ_codes3")
              all_sel_nodes <- rbind(all_sel_nodes, sel_nodes)
            } else if (it == 4) {
              plot %>% bind_shiny("dist_codes4")
              show("dist_codes4")
              show("summ_codes4")
              all_sel_nodes <- rbind(all_sel_nodes, sel_nodes)
            }
          }
        }
        # -----------------------------------------------------
      } # End if
  })
  
  ####################################
  # Titles Stacked Bar chart
  ###################################
  output$summ_codes1 <-renderText({ 
    if(selected$sel_num_patients[[1]] != 0)
    paste0( "Selection 1: ", selected$sel_num_patients[[1]], " patients selected" ) 
  })
  output$summ_codes2 <-renderText({ 
    if(selected$sel_num_patients[[2]] != 0)
    paste0( "Selection 2: ", selected$sel_num_patients[[2]], " patients selected" )
  })
  output$summ_codes3 <-renderText({ 
    if(selected$sel_num_patients[[3]] != 0)
    paste0( "Selection 3: ", selected$sel_num_patients[[3]], " patients selected" ) 
  })
  output$summ_codes4 <-renderText({ 
    if(selected$sel_num_patients[[4]] != 0)
      paste0( "Selection 4: ", selected$sel_num_patients[[4]], " patients selected" ) 
  })
  ####################################
  # Decision tree
  ###################################
  output$tree <- renderVisNetwork({
    
    if ( (selected$sel_num_patients[[1]] != 0) + 
         (selected$sel_num_patients[[2]] != 0) + 
         (selected$sel_num_patients[[3]] != 0) + 
         (selected$sel_num_patients[[4]] != 0) > 1 ) {

      selected_nodes <- NULL
      selected_icd <- NULL
      
      # Decision Tree task: Define Selected values for decison tree
      for( it in 1:length(selected$nodes_sel)) {
  
        if (any(!is.na(selected$nodes_sel[[it]])) ) {
          if (is.null(selected_nodes)) {
            # Extract HADM_ID
            selected_nodes <- selected$nodes_sel[[it]] %>%
            select(HADM_ID) %>%
            mutate(selected_groups = it)
            # Extarct ICD
            selected_icd <- unique( selected$sel_nodes[[it]] %>% filter( filter == 1 & inherited == 1)  %>% select(ICD))
          } else {
            # Extract HADM_ID
            selected_nodes <- bind_rows(
              selected_nodes,
              selected$nodes_sel[[it]] %>% select(HADM_ID) %>% mutate(selected_groups = it)
            )
            # Extarct ICD
            selected_icd <- bind_rows(selected_icd, unique( selected$sel_nodes[[it]] %>% filter( filter == 1 & inherited == 1)  %>% select(ICD)) )
          }
        }
      }
      
      # Rpart
      if ( !is.null(selected_nodes) & nrow(unique(selected_nodes[,"selected_groups"])) > 1 ) {
  
        ds <- diagnosis %>%
          select(HADM_ID, SEQ_NUM, ICD) %>%
          filter(ICD %in% selected_icd$ICD) %>%
          spread(SEQ_NUM, ICD) %>%
          inner_join(selected_nodes %>% select(HADM_ID, selected_groups), by = "HADM_ID") %>%
          select(-HADM_ID) %>%
          mutate(selected_groups = as.factor(paste("Selection",  selected_groups)))
        
        colnames(ds)[1:length(colnames(ds))-1] <- paste("Position", colnames(ds)[1:length(colnames(ds))-1])
  
        res <- rpart::rpart(selected_groups~., data=ds)
        
        visTree(res,
                main = "Decision tree",
                width = "100%",
                direction = "LR")
      }
    }
  })
  
  ####################################
  # Enable Disable Close Button
  ###################################
  observeEvent(selected$sel_nodes,{
    for( it in 1:length(selected$sel_nodes)) {
      if (any(is.na(selected$sel_nodes[[it]]))) {
        hide(paste0("close", it))
      } else {
        show(paste0("close", it))
      }
    }
  })
  ####################################
  # Action Close Button
  ###################################
  observeEvent(input$close1, {
    hide("dist_codes1")
    hide("close1")
    hide("summ_codes1")
    selected$sel_nodes[[1]] <- NA
  })
  
  observeEvent(input$close2, {
    hide("dist_codes2")
    hide("close2")
    hide("summ_codes2")
    selected$sel_nodes[[2]] <- NA
  })
  
  observeEvent(input$close3, {
    hide("dist_codes3")
    hide("close3")
    hide("summ_codes3")
    selected$sel_nodes[[3]] <- NA
  })
  
  observeEvent(input$close4, {
    hide("dist_codes4")
    hide("close4")
    hide("summ_codes4")
    selected$sel_nodes[[4]] <- NA
  })
  ####################################
  # Hide/Show bar_chart Box
  ###################################
  observeEvent(selected$sel_num_patients, {
    
    if ( (selected$sel_num_patients[[1]] != 0) + 
         (selected$sel_num_patients[[2]] != 0) + 
         (selected$sel_num_patients[[3]] != 0) + 
         (selected$sel_num_patients[[4]] != 0) > 0 ) {
      show("bar_chart_box")
    } else {
      hide("bar_chart_box")
    }
  })
  ####################################
  # Hide/Show decision_tree Box
  ###################################
  observeEvent(selected$sel_num_patients, {
    
    if ( (selected$sel_num_patients[[1]] != 0) + 
         (selected$sel_num_patients[[2]] != 0) + 
         (selected$sel_num_patients[[3]] != 0) + 
         (selected$sel_num_patients[[4]] != 0) > 1 ) {
      show("decision_tree_box")
    } else {
      hide("decision_tree_box")
    }
  })
  ####################################
  # Network plot
  ###################################
  
  # output$diagPlot <- renderPlot({
  # 
  #   t <- input$plot_brush
  # 
  #   node_info_subset <- values$nodes_igraph %>%
  #     inner_join(values$layout_gephi, by = "Id") %>%
  #     filter( t$xmin <= V1 & t$xmax >= V1 & t$ymin <= V2 & t$ymax >= V2)
  # 
  # 
  #     diagnoses %>%
  #     inner_join(node_info_subset %>% select(HADM_ID), by = "HADM_ID") %>%
  #     select(HADM_ID, SEQ_NUM, ICD) %>%
  #     ggplot(aes(x = SEQ_NUM, y = ICD, group = HADM_ID, label = ICD) ) +
  #     geom_text()
  #     
  # 
  # })
  
  
  # 
  # output$ranking <- renderTable({
  #   t <- input$plot_brush
  #   node_info_subset <- node_info %>% 
  #     filter( t$xmin <= x & t$xmax >= x & t$ymin <= y & t$ymax >= y)
  #   
  #   dt <- as.data.frame(table(node_info_subset$SEQ_NUM)) %>%
  #     mutate(Freq = paste0( round(100 * Freq / nrow(node_info_subset),1), "%", "(", Freq, ")"))
  #   colnames(dt) <- c("Diag. ranking", "% patients")
  #   
  #   dt
  # })
  # 
  
  
  # output$diagnosis <- renderTable({
  #   
  #   t <- input$plot_brush
  #   
  #   node_info_subset <- values$nodes_igraph %>% 
  #     inner_join(values$layout_gephi, by = "Id") %>%
  #     filter( t$xmin <= V1 & t$xmax >= V1 & t$ymin <= V2 & t$ymax >= V2)
  # 
  #   print(node_info_subset)
  #   
  #   dt <- diagnoses %>%
  #     inner_join(node_info_subset %>% select(HADM_ID), by = "HADM_ID") %>%
  #     group_by(ICD) %>%
  #     summarise(
  #       LONG_TITLE = paste(unique(LONG_TITLE), collapse = ". "),
  #       Freq = n(),
  #       SEQ_NUM_Q1 = quantile(SEQ_NUM, probs = 0.25),
  #       SEQ_NUM_Q2 = quantile(SEQ_NUM, probs = 0.50),
  #       SEQ_NUM_Q3 = quantile(SEQ_NUM, probs = 0.75)
  #       #SEQ_NUM_Q4 = quantile(SEQ_NUM, probs = 1)
  #     ) %>%
  #     ungroup() %>%
  #     filter(Freq / nrow(node_info_subset) > 0.4) %>% # <-- filter more than 40%
  #     arrange(SEQ_NUM_Q1, SEQ_NUM_Q2, desc(Freq)) %>%
  #     mutate(Freq = paste0( round(100 * Freq / nrow(node_info_subset),1), "%", "(", Freq, ")")
  #            # Quantile = paste0("Q1:", SEQ_NUM_Q1, " ",
  #            #                   "Q2:", SEQ_NUM_Q2, " ",
  #            #                   "Q3:", SEQ_NUM_Q3, " ",
  #            #                   "Q4:", SEQ_NUM_Q4)
  #     )
  # 
  #   dt
  # })
  # # 
  # # # Outputs
  # # output$ids <- renderText({
  # #   
  # #   t <- input$plot_brush
  # #   node_info_subset <- node_info %>%
  # #     filter( t$xmin <= x & t$xmax >= x & t$ymin <= y & t$ymax >= y) 
  # #   
  # #   node_info_subset$Id
  # # })
  # # 
  
  
})
