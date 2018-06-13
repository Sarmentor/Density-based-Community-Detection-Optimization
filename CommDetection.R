#library(igraph)

#mygraph <- read.graph("karate-edge.txt", format = "ncol", directed = FALSE)
#mygraph <- read.graph("toynet4.txt", format = "ncol", directed = TRUE)

#graph.n <- 1
#mygraph <- read.graph(paste("grafo_",graph.n,".txt",sep=""), format = "ncol", directed = TRUE)
mygraph <- read.graph(graph.n, format = "ncol", directed = TRUE)


benchmark.comm <- label.propagation.community(mygraph)

#plot(mygraph)
bridges <- function (communities, component, graph) 
{
  m <- communities
  el <- get.edgelist(graph, names = FALSE)
  nodes <- df.comms[df.comms[,2]==component,1]
  m1 <- m[el[nodes, 1]]
  m2 <- m[el[nodes, 2]]
  res <- m1 != m2
  if (!is.null(names(m1))) {
    names(res) <- paste(names(m1), names(m2), sep = "|")
  }
  res <- unique(m2[which(res==TRUE)])
}

components <- clusters(mygraph, mode=c("strong"))

#each found component is a community
df.comms <<- data.frame(node = 1:vcount(mygraph), comm = as.numeric(membership(components)))

#for each component finds near components
for (component in unique(df.comms[,2])){
  max.cc <- 0
  vec <- bridges(df.comms[,2], component, mygraph)
  #if no other components near continues with next component
  if(length(vec) == 0 || vec == component){
    next
  }else{
    #subgraph of near components
    compnodeslist <- c()
  
    for(i in vec){
      compnodes <- df.comms[df.comms[,2]==component, 1]
      compnodeslist <- c(compnodes, df.comms[df.comms[,2]==i, 1])
      subgraph <- induced.subgraph(graph = mygraph, vids = compnodeslist, impl = "copy_and_delete")
      #for each near component checks subgraph clustering coefficient
      merge.cc <- transitivity(subgraph, type="global")
      
      if(is.na(merge.cc)){
        next
      }else{
        #if max cc then the component joins the community
        if(merge.cc >= max.cc){
          max.cc <- merge.cc
          max.merge.comp <- component 
        }
        df.comms[df.comms[,1]==i,2] <- component      
      }
    }
  }
}

ori.comm.mod <- modularity(mygraph, benchmark.comm$membership)
new.comm.mod <- modularity(mygraph,df.comms[,2])
#calculates modularity of communities
cat("\nModularity of benchmark algorithm (label propagation): ", ori.comm.mod)
cat("\nModularity of Graph: ", new.comm.mod)

#write.csv(df.comms, file = "toynet4_res_comm.csv", sep = " ")
write.csv(df.comms, file = paste("Wiki_Vote2_res_comm_",counter2,".csv",sep=""), sep = " ")
Community.df.results <<- rbind(Community.df.results, c(graph.n,ori.comm.mod,new.comm.mod))
names(Community.df.results) <- c("graph.n","ori.comm.mod","new.comm.mod")

