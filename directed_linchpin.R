linchpin_centrality <- function(g, id = 'name', attr = 'spec', specs = NULL, type1 = NULL, type2 = NULL) {
  
  #' Calculate linchpin score on each node in an igraph network
  #' 
  #' Arguments: 
  #'   g: An igraph network object
  #'   id: The vertex attribute specifying the node identifier
  #'   attr: The vertex attribute specifying the classification of each node
  #'   specs: A vector containing the subset of classifications to consider during linchpin score calculation
  #'   type1: The input that controls the directionality of first-order ties; takes only 'in', 'out', or 'all' as inputs
  #'   type2: The input that controls the directionality of second-order ties; takes only 'in', 'out', or 'all' as inputs
  #' 
  #' Returns: 
  #'   A vector of linchpin score values which correspond to the ordering of nodes in the inputted igraph network object
  #'   
  #' Examples: 
  #'   linchpin_centrality(net, 'name', 'spec', specs = c('surgery','medonc','radonc')) # does not consider directionality
  #'   linchpin_centrality(net, 'name', 'spec', specs = c('surgery','medonc','radonc'), type1='all', type2='in') # considers directionality of second-order ties
  
  
  # load required packages
  require(igraph)
  
  # decipher if graph is directed and/or weighted
  directed <- is.directed(g)
  weighted <- is.weighted(g)
  
  # defensive programming
  if (directed & (is.null(type1) | is.null(type2))) {
    stop("Warning: Graph is directed, but 'type1' and 'type2' arguments not provided")
  } else if (!directed) {
    type1 <- 'all'; type2 <- 'all'
  }
  
  # store first-order degree/strength into dataframe
  deg.stats <- data.frame(
    name = vertex_attr(g)[id], spec = vertex_attr(g)[attr],
    degree = degree(g, mode = type1),
    strength = strength(g, mode = type1),
    row.names = NULL
  )
  
  # loop through each node
  linchpin <- c()
  counter <- 0
  for (i in deg.stats$name) {
    if (counter %% 500 == 0) {print(paste0(counter, '/', nrow(deg.stats)))}
    
    # store focal specialty 
    focal_spec <- deg.stats[deg.stats$name==i, attr]
    
    # check if focal_spec in specs and specs not null
    if (!focal_spec %in% specs & !is.null(specs)) {
      linchpin <- c(linchpin, NA)
      counter <- counter + 1
      next
    }
    
    # calculate denominator
    num <- 0
    denom <- ifelse(weighted,deg.stats[deg.stats$name==i, 'strength'],deg.stats[deg.stats$name==i, 'degree'])
  
    # check for isolates (denom == 0)
    if (denom == 0) {
      linchpin <- c(linchpin, NA) # default to NA due to zero division 
      counter <- counter + 1
      next
    }
  
    # loop through node i's neighbors' connections (if denom > 0)
    for (j in unique(neighbors(g, i, mode = type1))) { 
      # extract name of neighbor and second-order ties
      nm <- deg.stats$name[j]
      temp <- deg.stats[unique(neighbors(g, nm, mode = type2)), ] # consider second-order directionality
      temp <- temp[temp$name != i, ]
  
      # if focal node i's specialty is NOT in node j's neighbors specialties
      if (!(focal_spec %in% temp$spec)) {
  
        if (directed & weighted) { # directed and weighted case
          mat <- get.edgelist(g, names = TRUE)
          idx <- ifelse(type2 == 'in', which(mat[, 1] == nm & mat[, 2] == i), which(mat[, 1] == i & mat[, 2] == nm))
          num <- num + as.numeric(E(g)$weight[idx])
  
        } else if (!directed & weighted) { # undirected and weighted case
          idx <- get.edge.ids(g, vp = c(nm, i))
          num <- num + as.numeric(E(g)$weight[idx])
  
        } else { # all unweighted cases
          num <- num + 1
        }
      }
    }
    
    # calculate linchpin score and store
    linchpin <- c(linchpin, num/denom)
    counter <- counter + 1
  }

  # return output 
  return(linchpin)
}
