linchpin_centrality <- function(g, id = 'name', attr = 'spec', specs = NULL, type = NULL) {
  
  ##### Docstring #####
  #' Calculate linchpin score on each node in an igraph network
  #' 
  #' Arguments: 
  #'   g: An igraph network object
  #'   id: The vertex attribute specifying the node identifier
  #'   attr: The vertex attribute specifying the classification of each node
  #'   specs: A vector containing the subset of classifications to consider during linchpin score calculation
  #'   type: The input that controls the directionality of linchpin score that is calculated
  #' 
  #' Returns: 
  #'   A vector of linchpin score values which correspond to the ordering of nodes in the input igraph object
  #'   
  #' Examples: 
  #'   linchpin_centrality(net, 'name', 'spec', specs = c('surgery','medonc','radonc')) # calculates undirected linchpin scores
  #'   linchpin_centrality(net, 'name', 'spec', specs = c('surgery','medonc','radonc'), type='in') # calculates directed in-linchpin scores
  #'   linchpin_centrality(net, 'name', 'spec', specs = c('surgery','medonc','radonc'), type='out') # calculates directed out-linchpin scores
  
  ##### Code #####
  
  # load required packages
  require(dplyr)
  require(igraph)
  
  # decipher if graph is directed and/or weighted
  directed <- is_directed(g)
  weighted <- is_weighted(g)
  
  # defensive programming
  if (directed & (is.null(type))) {
    stop("Warning: Graph is directed, but 'type' argument not provided")
  } else if (!directed) {
    type <- 'all'
  }
  
  # store first-order degree/strength into dataframe
  deg.stats <- data.frame(
    name = vertex_attr(g)[id], spec = vertex_attr(g)[attr],
    strength = strength(g, mode = type), # if unweighted, degree == strength
    row.names = NULL
  )
  
  # loop through each node
  linchpin <- c()
  counter <- 0
  for (i in deg.stats$name) {
    if (counter %% 500 == 0) {print(paste0(counter, '/', nrow(deg.stats)))}
    
    ## store focal specialty 
    focal_spec <- deg.stats[deg.stats$name==i, attr]
    
    ## check if focal_spec in specs and specs not null
    if (!focal_spec %in% specs & !is.null(specs)) {
      linchpin <- c(linchpin, NA)
      counter <- counter + 1
      next
    }
    
    ## calculate denominator and check for isolates (denom == 0)
    denom <- deg.stats[deg.stats$name==i, 'strength']
  
    if (denom == 0) {
      linchpin <- c(linchpin, NA) # default to NA due to zero division 
      counter <- counter + 1
      next
    }
  
    ## calculate numerator by looping  node i's neighbors' connections
    num <- 0
    
    for (j in unique(neighbors(g, i, mode = type))) { 
      
      ### extract name of neighbor and second-order ties
      nm <- deg.stats$name[j]
      
      ### in-linchpin
      if (type == 'in') {
        edges <- incident(g, nm, mode = 'out')
        edge_list <- as_data_frame(g, what = 'edges')[edges, ] 
        joined <- edge_list %>% 
          left_join(deg.stats[, c('name', 'spec')], by = c('to'='name')) %>% 
          filter(spec == focal_spec)
        
        if (weighted) {
          focal_strength <- joined %>% filter(to == i) %>% with(., sum(weight))
          total_strength <- joined %>% filter(to != i) %>% with(., sum(weight))
          addition <- ifelse(total_strength < focal_strength, focal_strength-total_strength, 0)
          
        } else {
          addition <- as.numeric(!nrow(joined %>% filter(to != i)) > 0)
        }
      
      ### out-linchpin
      } else if (type == 'out') {
        edges <- incident(g, nm, mode = 'in')
        edge_list <- as_data_frame(g, what = 'edges')[edges, ] 
        joined <- edge_list %>% 
          left_join(deg.stats[, c('name', 'spec')], by = c('from'='name')) %>% 
          filter(spec == focal_spec)
        
        if (weighted) {
          focal_strength <- joined %>% filter(from == i) %>% with(., sum(weight))
          total_strength <- joined %>% filter(from != i) %>% with(., sum(weight))
          addition <- ifelse(total_strength < focal_strength, focal_strength-total_strength, 0)
          
        } else {
          addition <- as.numeric(!nrow(joined %>% filter(from != i)) > 0)
        }
        
      ### all-linchpin
      } else {
        edges <- incident(g, nm, mode = 'all')
        edge_list <- as_data_frame(g, what = 'edges')[edges, ]
        from_idx <- which(edge_list$from == nm)
        to_idx <- which(edge_list$to == nm)
        
        joined <- rbind(
          edge_list[from_idx, ],
          edge_list[to_idx, ] %>% rename(from2=to, to=from) %>% rename(from=from2)
          ) %>% 
          left_join(deg.stats[, c('name', 'spec')], by = c('to'='name')) %>% 
          filter(spec == focal_spec)
        
        if (weighted) {
          focal_strength <- joined %>% filter(to == i) %>% with(., sum(weight))
          total_strength <- joined %>% filter(to != i) %>% with(., sum(weight))
          addition <- ifelse(total_strength < focal_strength, focal_strength-total_strength, 0)
          
        } else {
          addition <- as.numeric(!nrow(joined %>% filter(to != i)) > 0)
        }
        
      } 
      
      num <- num + addition
      
    }
    
    ## calculate linchpin score and store
    linchpin <- c(linchpin, num/denom)
    counter <- counter + 1
  }

  # return output 
  return(linchpin)
}
