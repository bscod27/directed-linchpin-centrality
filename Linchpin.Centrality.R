# creating functions for linchpin
Linchpin.Centrality <- function(links = edgelist, characteristics = key, label_column_name = char, weighted = TRUE, char_subset = c('medonc','radonc','surgery')) {
  
  # If only interested in a subset of characteristics this will help with runtime
  if(all(char_subset != c('empty'))) { ## NOTE: I added the all on 4/30 because bad programming
    # first get unimportant indexes
    non_important_inds = which(!(characteristics[,label_column_name] %in% char_subset))
    characteristics[non_important_inds, label_column_name] = 'NAN'
  }
  
  
  links$label_1 = NA # create columns for characteristic one and two in the edge list
  links$label_2 = NA
  for(i in unique(characteristics[,label_column_name])) { # loop through each specialty
    ref = characteristics$ID_1[which(characteristics[,label_column_name] == i)] # find all NPIs with that specialty
    link1 = links[,1] %in% ref # true false array for edge1 phys whether they are specialty i
    link2 = links[,2] %in% ref
    links$label_1[which(link1 == TRUE)] = as.character(i) # Store the specialty for the physician in the appropriate column of the edge list
    links$label_2[which(link2 == TRUE)] = as.character(i)
  }
  net_with_labels <- links[complete.cases(links), ]
  
  list_ids = unique(c(net_with_labels[,1],net_with_labels[,2])) # gather all edge ids then get just the unique ids
  total_labels = unique(as.character(characteristics[, label_column_name])) # get all unique labs of the characteristic
  # Make an empty dataframe that is (unique actors X unique labs of characteristic)
  df <- data.frame(matrix(ncol = length(total_labels), nrow = length(list_ids)))
  colnames(df) <- total_labels
  row.names(df) <- list_ids
  for(i in total_labels) { # for each speciality
    col_ind = which(colnames(df) == i) # name of column for final df
    link1_inds = which(net_with_labels$label_1 == i) # which rows have ID 1 with that characteristic
    link2_inds = which(net_with_labels$label_2 == i) # which rows have ID 2 with that characteristic
    
    link1 = net_with_labels[,2][link1_inds] # all links to to that characteristic in col 2
    self1 = net_with_labels[,1][link1_inds] # the IDs that are matched to link1
    link2 = net_with_labels[,1][link2_inds] 
    self2 = net_with_labels[,2][link2_inds]
    
    tups1 = mapply(c, self1, link1, SIMPLIFY = FALSE) # make a list of tuples (self, link)
    tups2 = mapply(c, self2, link2, SIMPLIFY = FALSE)
    #Combine the tuples to get all unique links
    if(length(tups1)>length(tups2)) {
      inc = !(tups2 %in% tups1)
      ids_full = c(link1, link2[inc])
    } else {
      inc = !(tups1 %in% tups2)
      ids_full = c(link1[inc], link2)
    }
    ft = table(ids_full)
    row_inds = match(names(ft), rownames(df))
    df[row_inds, col_ind] = ft[names(ft)]
  }
  df[is.na(df)] <- 0
  lookup_df <- df
  
  lookup_df$self_label = NA
  for(i in unique(characteristics[, label_column_name])) {
    IDs = characteristics[ , "ID_1"][which(characteristics[, label_column_name] == i)]
    self_IDs = row.names(lookup_df) 
    tf = self_IDs %in% IDs
    lookup_df$self_label[tf] = i
  }
  
  links.results <- list()
  all_linkage = c()
  label = c()
  c=0
  for(ids in 1:length(lookup_df[,1])) {
    if(c%% 10 == 0) {print(paste(c, '/', length(lookup_df[,1]), sep = ''))}
    c=c+1
    
    
    
    if(weighted == FALSE) {
      type = lookup_df$self_label[ids]
      if(type=='NAN') {new_linkage = 0}
      # id = as.numeric(rownames(lookup_df)[ids])
      else{
        id = rownames(lookup_df)[ids]
        test_inds1 = which(net_with_labels[,1] == id) #& net_with_labels$label_2 != type)
        test_inds2 = which(net_with_labels[,2] == id) #& net_with_labels$label_1 != type)
        links = unique(c(net_with_labels[test_inds1, 2],net_with_labels[test_inds2, 1]))
        dfinds = match(links, rownames(lookup_df))
        col_i = which(colnames(lookup_df) == type)
        new_linkage <- length(which(lookup_df[dfinds, col_i] < 2)) / length(links)}
    } else {
      type = lookup_df$self_label[ids]
      if(type=='NAN') {new_linkage = 0}
      # id = as.numeric(rownames(lookup_df)[ids])
      else{
        # id = as.numeric(rownames(lookup_df)[ids])
        id = rownames(lookup_df)[ids]
        test_inds1 = which(net_with_labels[,1] == id)
        test_inds2 = which(net_with_labels[,2] == id)
        
        col_i = which(colnames(lookup_df) == type)
        ids = net_with_labels[test_inds1, 2]
        ids2 = net_with_labels[test_inds2, 1]
        if(length(ids)>length(ids2)) {
          id2_incs = !(ids2 %in% ids)
          # Consider summing edge weights
          all_inds = c(test_inds1, test_inds2[id2_incs])
          all_ids = c(ids, ids2[id2_incs])
        } else {
          id1_incs = !(ids %in% ids2)
          all_inds = c(test_inds2, test_inds1[id1_incs])
          all_ids = c(ids2, ids[id1_incs])
        }
        shared_links = net_with_labels[all_inds,3]
        dfinds = match(all_ids, rownames(lookup_df))
        new_linkage <- sum(shared_links[lookup_df[dfinds, col_i] < 2]) / sum(shared_links)}
    }
    all_linkage = c(all_linkage, new_linkage)
  }
  lookup_df$linkage = all_linkage
  lookup_df$ID_1 <- as.character(rownames(lookup_df))
  # The code from github used the variable "key" here when I think it should be "characteristics"
  # I changed it and it worked
  characteristics$ID_1 <- as.character(characteristics$ID_1)
  links.results[[3]] <- characteristics <- characteristics %>% left_join((lookup_df %>% select(ID_1, linkage)))
  
  count = 1
  means = c()
  labs = c()
  variance = c()
  n_actors = c()
  for(i in unique(lookup_df$self_label)) {
    if(length(which(lookup_df$self_label == i))>1) {
      print(paste(i, ':    ', mean(lookup_df$linkage[which(lookup_df$self_label == i)]), sep = ''))
      if(count == 1) {
        plot(density(lookup_df$linkage[which(lookup_df$self_label == i)]), col = count)
      } else {
        lines(density(lookup_df$linkage[which(lookup_df$self_label == i)]), col = count)
      }
      count = count + 1
      means = c(means, round(mean(lookup_df$linkage[which(lookup_df$self_label == i)]), 3))
      variance = c(variance, round(var(lookup_df$linkage[which(lookup_df$self_label == i)]), 3))
      labs = c(labs, as.character(i))
      n_actors = c(n_actors, length(which(lookup_df$self_label == i)))
    } else {
      means = c(means, lookup_df$linkage[which(lookup_df$self_label == i)])
      variance = c(variance, 0)
      labs = c(labs, as.character(i))
      n_actors = c(n_actors, length(which(lookup_df$self_label == i)))
    }
  }
  summary.links <- data.frame(labs, n_actors, means, variance)
  links.results[[2]] <- summary.links
  
  all.results <- as.data.frame(matrix(nrow = 1, ncol = 1))
  if(!is.null(n_actors)){
    for(j in 1:length(n_actors)){
      j.results <- as.data.frame(matrix(nrow = 1, ncol = 3))
      j.results[1, ] <- c(n_actors[j], means[j], variance[j])
      colnames(j.results) <- c(paste(labs[j], c("n_actors", "means", "variance"), sep = "_"))
      all.results <- all.results %>% cbind(j.results)
    }
  }
  all.results$V1 <- NULL
  links.results[[1]] <- as.data.frame(all.results)
  return(links.results)
}
