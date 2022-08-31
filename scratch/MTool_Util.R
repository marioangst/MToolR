MTool_GetBetweenness <- function( outputdata, data ){
  
  ## Get unique ids
  ids <- unique( outputdata$User_ID )
  
  ## Initialize betweenness output
  output = matrix( 0, 1, 4 )
  
  ## For each User_ID do:
  for( i in ids ){
    
    ## Get the edgelist corresponding to this User_ID
    edgelist <- as.matrix( subset( data, User_ID == i, select=( From:To ) ) )
    
    ## Create a graph from the edgelist
    graph <- graph_from_edgelist( edgelist, directed = TRUE )
    
    ## Calculate unweighted betweenness
    between_unweighted <- betweenness( graph, v = V(graph), directed = TRUE )
    
    ## Get the weights of all edges of this User_ID
    weight <- subset( data, User_ID == i, select = Weight )
    
    ## Set the weights
    ## In case of negative weights ( denoting inverse proportionality ) we take the absolute value
    ## Meaning higher negative values result in higher betweenness values
    E(graph)$weight <- ( max( abs( as.matrix( weight ) ) ) + 1 ) - abs( as.matrix( weight ) )
    
    ## Calculate weighte betweenness
    between_weighted <- betweenness( graph, v = V(graph), directed = TRUE )
    
    ## Ensure that we store User_ID for each betweenness measure
    id_store <- matrix( i, length( between_weighted ), 1 )
    
    ## Store the result in output
    result <- cbind( V(graph)$name, id_store, between_unweighted, between_weighted )
    output <- rbind( output, result )
  }
  
  ## Delete superfluous first row
  output <- output[-1,]
  
  ## Set the column names
  colnames( output ) <- c( 'Node_ID', 'User_ID', 'Betweenness_Weighted', 'Betweenness_Unweighted' )
  
  ## Cast to dataframe ( for subsetting purposes )
  output <- as.data.frame( output )
  
  ## For each data row in the output of MTool_ProcessData do:
  for( j in 1:nrow( outputdata ) ){
    
    ## Get the Node_ID of this datarow
    node = as.character( outputdata$Node_ID[j] )
    
    ## Get the User_ID of this datarow
    id = as.character( outputdata$User_ID[j] )
    
    ## Find correspoding Node_ID and User_ID in betweenness measure
    output_to_append = subset( output, User_ID == id & Node_ID == node )
    
    ## Check if we have found anything
    if( nrow( output_to_append ) > 0 )
    {
      ## If match is found store betweenness measure in outputdata  
      outputdata[j,11] <- output_to_append[1,3]
      outputdata[j,12] <- output_to_append[1,4]
    }
    
  }
  
  ## Set the column names
  colnames( outputdata ) <- MTool_GetColumnNames( FALSE )
  
  ## return the outputdata
  return( outputdata )
}

MTool_ProcessData <- function( data ){
  
  ## get nodelist
  completeNodelist <- MTool_GetNodes( data )
  
  ## Remove any empty strings ( just in case )
  completeNodelist <- completeNodelist[!completeNodelist=='']
  
  ## Ensure that the User_ID variable is set as character and stored as such
  ID <- data$User_ID
  ID <- as.character( ID )
  data$User_ID <- ID
  
  ## Get a list of all unique User_IDs
  IDs = unique( ID )
  
  ## Number of unique IDs
  N <- length( unique( ID ) ) 
  
  ## Number of nodes
  nr_nodes <- length( completeNodelist )
  
  ## initialize output, where the resulting data is stored
  output <- matrix( 0, 1 ,10 )
  
  ## set the names of the columns
  colnames( output ) <- MTool_GetColumnNames( TRUE )
  
  ## Split the data per ID
  for( id in IDs ){
    
    ## Get all the datarows belonging to a certain ID
    df <- subset( data, User_ID == id )
    
    ##  Initialize the result for this ID
    result = matrix( 0, nr_nodes, 10 )
    
    for( j in 1:nr_nodes ){
      
      ## Get the node we are currently interested in
      node = as.character( completeNodelist[j] )
      
      ## Store the node and the ID in result
      result[j,1] = as.character( id )
      result[j,2] = node
      result[j,3] = length( MTool_GetNodes( df ) )
      result[j,4] = nrow( df )
      
      for( k in 1:length( df[,1] ) ){ 
        
        ## We loop ( with variable k ) through the datarows of a certain ID, to get the 'from' and 'to' nodes in this datarow
        from = as.character( df$From[k] )
        to = as.character( df$To[k] )
        
        ## If the from-node equals the node of interest, then add it to the degree count and add the weight to the total strength
        if( from == node ){
          result[j,5] = as.numeric( result[j,5] ) + 1;
          result[j,6] = as.numeric( result[j,6] ) + as.numeric( df$Weight[k] )
        }
        
        ## If the to-node equals the node of interest, then add it to the degree count and add the weight to the total strength    
        if( to == node ){
          result[j,8] = as.numeric( result[j,8] ) + 1;
          result[j,9] = as.numeric( result[j,9] ) + as.numeric( df$Weight[k] )
        }
      }
    }
    
    ## Store the result in the output matrix
    output = rbind( output, result )  
  }
  
  ## Above, We  initialized the output as a zero-matrix of certain size, making the first row superfluous. Here we remove the first row.
  output = output[-1,]
  
  ## Total number of observations
  M <- nrow( output )
  
  ## Calculate the average weight (i.e. strength / degree)
  for( i in 1:M )
  {
    if( as.numeric( output[i,5] ) > 0 )
    {
      output[i,7] = as.numeric( output[i,6] ) / as.numeric( output[i,5] )
    }
    
    if( as.numeric( output[i,8] ) > 0 )
    {
      output[i,10] = as.numeric( output[i,9] ) / as.numeric( output[i,8] )
    }
  }
  
  ## Cast to a dataframe
  output = as.data.frame( output )
  
  ## Calculate betweenness 
  output <- MTool_GetBetweenness( output, data )
  
  return( output )
}

MTool_GetNodes <- function( data )
{
  ## get nodelist
  fromNodelist <- unique( as.character( data$From ) )
  toNodelist <- unique( as.character( data$To ) )
  completeNodelist <- unique( c( fromNodelist, toNodelist ) )
  
  return( completeNodelist )
}

MTool_GetColumnNames <- function( FromProcessData )
{
    ## To set the column names of the output data frame
    names <- c( 'User_ID',
                'Node_ID',
                'Total_Nr_Nodes',
                'Total_Nr_Edges',
                'Out_Degree',
                'Out_Strength',
                'Out_Average',
                'In_Degree',
                'In_Strength',
                'In_Average' )
    
  if( !FromProcessData )
  {
    names <- c( names,               
                'Betweenness_Unweighted',
                'Betweenness_Weighted' )

  }
  
  return( names )
}
