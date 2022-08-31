## Source libraries
library( qgraph )
library( igraph )
library( dplyr )
library( png )
library( jpeg )

## Get your input data (Change dataname)
inputdata <- read.csv2( file = 'Your data file name here.csv', 
                        head = TRUE, 
                        sep = ",", 
                        fill = TRUE )

## Get data with responses
response_inputdata <- subset( inputdata, 
                              !( inputdata$From == '' | inputdata$To == '' ) )

types = unique( response_inputdata$Type )

for( i in types )
{
  mappingdata = subset( response_inputdata, 
                        response_inputdata$Type == i, select = c( User_ID, From, To, Weight ) )
  sum = mappingdata %>% group_by(From,To) %>% summarise(Weight = sum(Weight))
  
  ## fine tune the parameters here
  g     = qgraph(sum, 
                 curve = 0.5, 
                 width = 6, 
                 repulsion = 0.8, 
                 labels = TRUE,
                 edge.color="dimgray", 
                 threshold = mean(sum$Weight), 
                 directed = TRUE,
                 layout='spring',  
                 asize = 2, 
                 fade= FALSE, 
                 node.width=0.9, 
                 node.height=0.9, 
                 shape='square',
                 label.scale.equal=TRUE, 
                 DoNotPlot = TRUE )
  
  labelcheck = g$graphAttributes$Nodes$labels
  images = matrix(NA, length(labelcheck), 1)
  c = 1;
  for( i in labelcheck)
  {
    ## Please replace png by jpg in case your images are in jpg format
    images[c] = tolower(paste(i, 'png', sep = '.'))
    c = c+1;
  }
  
  plot = qgraph(g, images = images, DoNotPlot = FALSE, labels = FALSE )
}
