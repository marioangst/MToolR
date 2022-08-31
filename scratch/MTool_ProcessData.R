## Process M-Tool data 

## - Please check www.m-tool.org for the latest version of this script 

## This script will output a data frame containing (in this order):
## - User_ID: as defined in the input data
## - Node_Name: as defined in the input data
## - Total_Nr_nodes: Total number of nodes (concepts) included in the participant's mental model 
## - Total_Nr_Edges: Total number of edges (arrows) included in the participant's mental model 
## - Out_Degree: defined as the number of outgoing edges 
## - Out_Strength: defined as the sum of the weights of the outgoing edges 
## - Out_Average: defined as the average weight of the incoming edges 
## - In_Degree: defined as the number of incoming edges of a node 
## - In_Strength: defined as the sum of the weights of the incoming edges 
## - In_Average: defined as the average weight of the incoming edges 
## - Betweenness unweighted: defines as the number of times a node lies on the shortest path between two other nodes 
## - Betweenness weighted: defines as the number of times a node lies on the shortest path between two other nodes. 
##                         Compared to the unweighted Betweenness, nodes that lie on a higher weighted paths obtain higher betweenness values (Brandes,2001)


## Source libraries
library( qgraph )
library( igraph )
library( dplyr )

## Load MTool utility functions
source( 'MTool_Util.R' )

## Get your input data (Change dataname)
inputdata <- read.csv2( file = 'YOUR_M-Tool_DATA_SET_NAME.csv', 
                        head = TRUE, 
                        sep = ",", 
                        fill = TRUE )

## Find non response data
non_response_inputdata <- subset( inputdata, 
                                  inputdata$From == '' | inputdata$To == '' )

User_IDs_non_response <- unique( non_response_inputdata$User_ID )

## Get data with responses
response_inputdata <- subset( inputdata, 
                              !( inputdata$From == '' | inputdata$To == '' ) )

## Process the data
outputdata <- MTool_ProcessData( response_inputdata )

## Get the edgelist
edgelist <- subset( response_inputdata, select = ( From:To ) )

## Set the weights of the edges
edgelist$Weight <- abs( as.matrix( response_inputdata$Weight ) )

## Find the 'aggregate' mental model
edgelist <- edgelist %>%
            group_by( From, To ) %>%
            summarise( Weight = sum( abs( Weight ) ), .groups = 'keep' )  %>%
            ungroup()

## Visualise aggregate mental model
Graph <- qgraph( edgelist, directed = TRUE )


## Connect any Survey you may have conducted, if not ignore this code
dataSurvey <- read.csv2( file = 'YOUR_SURVEY_DATA_SET_NAME.csv', 
                         head = TRUE, 
                         sep = ",", 
                         fill = TRUE )

## Merge the survey data with MTool data
mergedData <- merge( outputdata, 
                     dataSurvey, 
                     id = User_ID )

## Participant retention indicators
print( paste( 'Mtool:', 
              length( User_IDs_non_response ), 
              'user(s) have not submitted any mental model.') )
print( paste( 'Mtool:', 
              length( unique( response_inputdata$User_ID ) ), 
              'user(s) have submitted a mental model.') )


