if (!require('igraph')) install.packages('igraph'); library('igraph')
if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('data.table')) install.packages('data.table'); library('data.table')
if (!require('DT')) install.packages('DT'); library('DT')
if (!require('ggnet')) install.packages('ggnet'); library('ggnet')
library('sna')
detach("package:sna")
server <- function(input, output,session) {
    
    #reading the file
    data <- reactive({ 
        req(input$file) ## ?req #  require that the input is available
        inFile <- input$file 
        dfs <- read.csv(inFile$datapath, header = input$header,sep=input$sep)
        return(dfs)
    })
    # a function to find the nodes out of the uploaded file
    find_nodes <- function() {
        req(data())
        y<-c("user1","user2")
        x<-data()
        x<-x%>%select(1:2)
        names(x) <-y
        #finding the thotal nodes from the file (the first two coloms)
        nodes1<-x %>% distinct(user1)
        nodes2<-x %>% distinct(user2)
        #renaming the colomn in the 2 nodes file (S we can bind them)
        names(nodes1)[1] <- "node"
        names(nodes2)[1] <- "node"
        totalNodes <- rbind(nodes1, nodes2)
        #the Total Nodes without any duplicate values 
        totalNodes<-distinct(totalNodes)
        
        return(totalNodes)
    }
    ################
    # a function to find the edges out of the uploaded file
    find_edges<- function() {
        req(data())
        y<-c("user1","user2")
        x<-data()
        x<-x%>%select(1:2)
        names(x) <-y
        
        #finding the edges (the edge is total number of interaction nomatter what the interaction is : RE , RT , MT)
        edges=x %>% count(user1,user2)
        
        return(edges)
    }################
    #get the graph
    find_graph<-function(){
        req(find_nodes())
        req(find_edges())
        totalNodes<-find_nodes()
        edges<-find_edges()
        net_shiny <- graph_from_data_frame(d=edges, vertices=totalNodes, directed=T) 
        return(net_shiny)
    }
    #################
    #find the modularity
    find_modularity<-function(community)
    {
        req(find_nodes())
        req(find_edges())
        links<-find_edges()
        links<-links%>%select("user1","user2")
        totalNodes<-find_nodes()
        #totalNodes will contain: node_id , community_id
        totalNodes$comm<-as.factor(membership(community))
        #sizes_com_info_ will contain : community_id, community_size
        sizes_com_info_<-as.data.frame(sizes(community))
        names(sizes_com_info_)<-c("comm","size")
        sizes_com_info_$comm<-as.factor(sizes_com_info_$comm)
        #result will contain: node_id,community_id,community_size
        result<-merge(x=totalNodes,y=sizes_com_info_,by="comm")
        
        comm_node_info<-result %>% select("comm","node")
        names(comm_node_info)<-c("comm_from","from")
        names(links)<-c("from","to")
        # comm_from, from,to
        comm_from_to<-merge(x=comm_node_info,y=links,by="from")
        names(comm_node_info)<-c("comm_to","to")
        names(comm_from_to)<-c("from","comm_from","to")
        # comm_from, from,to, comm_to
        comm_from_to<-merge(x=comm_from_to,y=comm_node_info,by="to")
        num_comm<-length(community)
        num_comm<-as.numeric(num_comm)
        mat <- matrix(, nrow = num_comm, ncol = num_comm)
        total_edges_num<-nrow(links)
        total_edges_num<-as.numeric(total_edges_num)
        for(i in 1:num_comm)
        {
            for(j in 1:num_comm)
            {
                mat[i,j]<-nrow(filter(comm_from_to,(comm_from_to$comm_from == i )& (comm_from_to$comm_to==j)))
                if(mat[i,j]!=0) mat[i,j]<-mat[i,j]/total_edges_num
            }
            
        }
        sum_row<-matrix(,nrow=num_comm,ncol=1)
        #sum of each row
        for(i in 1:num_comm)
        {
            sum_row[i,1]<-mat[i,1]
            if(num_comm>1)
            {
                for(j in 2:num_comm)
                sum_row[i,1]<-sum_row[i,1]+mat[i,j]
            }
                    
        }
        #sum_row<-rowSums( mat[,1:num_comm] )
        #find the square
        square_sum_row<-matrix(,nrow=num_comm,ncol=1)
        for(i in 1:num_comm)
        {
            square_sum_row[i,1]<-sum_row[i]*sum_row[i]
        }
        #find the difference : e[i,i]-a[i]^2
        difference<-matrix(,nrow=num_comm,ncol=1)
        for(i in 1:num_comm)
        {
            difference[i,1]<-mat[i,i]-square_sum_row[i]
        }
        modularity<-sum(difference)
        as.data.frame(modularity)
        return(modularity)
    }
    ####################################
    #recomended or not 
    rec<-function(value)
    {
       
        if((value>0.3) & (value<0.7))
            {v<-"recommended"}
        else
        {v<- "not recommended"}
        return( v)
    }
    ###################################
    # handle detection buttom 
    observeEvent(input$do_detect,
                 {
                     req(find_nodes())
                     req(find_edges())
                     totalNodes<-find_nodes()
                     edges<-find_edges()
                     net_shiny <- find_graph()
                     graph_undirected <- graph_from_data_frame(d=edges, vertices=totalNodes, directed=F) 
                     #######################
                     #find the selected algo
                     community <-reactive({
                         if (input$algorithm == "Infomap") {
                             community <- cluster_infomap(net_shiny, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = FALSE)
                             
                             
                         }
                         else if (input$algorithm == "Edge-Betweenness") {
                             community <- cluster_edge_betweenness(graph_undirected,weights = E(graph_undirected)$weight,
                                                                   directed = FALSE, edge.betweenness = TRUE, merges = TRUE,
                                                                   bridges = TRUE, modularity = FALSE, membership = TRUE)
                             
                         }
                         else if (input$algorithm == "fast greedy") {
                             community <- cluster_fast_greedy(simplify(graph_undirected), merges = TRUE, modularity = TRUE,
                                                              membership = TRUE, weights = E(graph_undirected)$weight)
                             
                         }
                         else if (input$algorithm == "Label Propagation") {
                             community <- com_fb_label_prop<-cluster_label_prop(graph_undirected)
                         }
                         else if (input$algorithm == "Leading Eigenvector") {
                             community <- com_fb_clus_lead<-cluster_leading_eigen(graph_undirected)
                         }
                         else if (input$algorithm == "MultiLevel(louvian)") {
                             community <- com_fb_cluster_louvain<-cluster_louvain(graph_undirected, weights = NULL)
                         }
                         else if (input$algorithm == "walktrap") {
                             community <- cluster_walktrap(net_shiny)
                         }
                         else if(input$algorithm =="optimal modularity"){
                             community <- com_fb_optimal_modularity <- cluster_optimal(net_shiny)
                         }
                         return(community)
                     })
                     ### the length of the community
                     output$len_comm <- renderText({length(community())})
                     ####
                     ####append column 
                     #add the community id to the nodes
                     totalNodes$comm<-as.factor(membership(community()))
                     #convert the vector sizes of the communities to a data frame
                     sizes_com<-as.data.frame(sizes(community()))
                     names(sizes_com)<-c("comm","size")
                     #make the colomn join(community id), factor
                     ####totalNodes$comm<-as.factor(totalNodes$comm)
                     sizes_com$comm<-as.factor(sizes_com$comm)
                     #a new dataframe : community_id, node_Id inside it, the size of the community
                     result<-merge(x=totalNodes,y=sizes_com,by="comm")
                     output$comm_details <-renderDataTable(result,options = list( scrollX = TRUE,  pageLength = 4,lengthMenu = c(4, 8, 12, 18)))
                     
                     
                 })
    #handle plot buttom
    observeEvent(input$do_plot,
                 {
                     
                     net_shiny <- find_graph()
                     output$plot_net<-renderPlot(ggnet2(net_shiny, alpha = 0.75, size = 4, edge.alpha = 0.5))
                 })
    #handle comparing buttom
    observeEvent(input$do_compare,
                 {
                     req(find_nodes())
                     req(find_edges())
                     totalNodes<-find_nodes()
                     edges<-find_edges()
                     net_shiny <- graph_from_data_frame(d=edges, vertices=totalNodes, directed=T) 
                     graph_undirected <- graph_from_data_frame(d=edges, vertices=totalNodes, directed=F)
                     
                     community_first <-reactive({
                         if (input$first_algo == "Infomap") {
                             community <- cluster_infomap(net_shiny, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = FALSE)
                             
                             
                         }
                         else if (input$first_algo == "Edge-Betweenness") {
                             community <- cluster_edge_betweenness(graph_undirected,weights = E(graph_undirected)$weight,
                                                                   directed = FALSE, edge.betweenness = TRUE, merges = TRUE,
                                                                   bridges = TRUE, modularity = FALSE, membership = TRUE)
                             
                         }
                         else if (input$first_algo == "fast greedy") {
                             community <- cluster_fast_greedy(simplify(graph_undirected), merges = TRUE, modularity = TRUE,
                                                              membership = TRUE, weights = E(graph_undirected)$weight)
                             
                         }
                         else if (input$first_algo == "Label Propagation") {
                             community <-cluster_label_prop(graph_undirected)
                         }
                         else if (input$first_algo == "Leading Eigenvector") {
                             community <-cluster_leading_eigen(graph_undirected)
                         }
                         else if (input$first_algo == "MultiLevel(louvian)") {
                             community <-cluster_louvain(graph_undirected, weights = NULL)
                         }
                         else if (input$first_algo == "walktrap") {
                             community <- cluster_walktrap(net_shiny)
                         }
                         else if(input$first_algo =="optimal modularity"){
                             community  <- cluster_optimal(net_shiny)
                         }
                         return(community)
                     })
                     community_second <-reactive({
                         if (input$second_algo == "Infomap") {
                             community <- cluster_infomap(net_shiny, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = FALSE)
                             
                             
                         }
                         else if (input$second_algo == "Edge-Betweenness") {
                             community <- cluster_edge_betweenness(graph_undirected,weights = E(graph_undirected)$weight,
                                                                   directed = FALSE, edge.betweenness = TRUE, merges = TRUE,
                                                                   bridges = TRUE, modularity = FALSE, membership = TRUE)
                             
                         }
                         else if (input$second_algo == "fast greedy") {
                             community <- cluster_fast_greedy(simplify(graph_undirected), merges = TRUE, modularity = TRUE,
                                                              membership = TRUE, weights = E(graph_undirected)$weight)
                             
                         }
                         else if (input$second_algo == "Label Propagation") {
                             community <-cluster_label_prop(graph_undirected)
                         }
                         else if (input$second_algo == "Leading Eigenvector") {
                             community <-cluster_leading_eigen(graph_undirected)
                         }
                         else if (input$second_algo == "MultiLevel(louvian)") {
                             community <- com_fb_cluster_louvain<-cluster_louvain(graph_undirected, weights = NULL)
                         }
                         else if (input$second_algo == "walktrap") {
                             community <- cluster_walktrap(net_shiny)
                         }
                         else if(input$second_algo =="optimal modularity"){
                             community <- cluster_optimal(net_shiny)
                         }
                         return(community)
                     })
                     output$len_comm_first <- renderText({length(community_first())})
                     output$len_comm_second <- renderText({length(community_second())})
                     comm_f<-as.data.frame(sizes(community_first()))
                     names(comm_f)<-c("id","size")
                     output$size_comm_first <- renderDataTable(comm_f,options = list( scrollX = TRUE,  pageLength = 3,lengthMenu = c(3, 6, 9, 12)), rownames= FALSE)
                     comm_s<-as.data.frame(sizes(community_second()))
                     names(comm_s)<-c("id","size")
                     output$size_comm_second <- renderDataTable(comm_s,options = list( scrollX = TRUE,  pageLength = 3,lengthMenu = c(3, 6, 9, 12)), rownames= FALSE)
                     compare_methode<-reactive({ 
                         req(input$comp_method) ## ?req #  require that the input is available
                         in_meth <- input$comp_method
                         return(in_meth)
                     })
                     output$distance<-renderText(compare(community_first(),community_second(),method=compare_methode()))

                 })
    #handle recomend buttom
    observeEvent(input$do_Recomend,
                 {
                     req(find_nodes())
                     req(find_edges())
                     totalNodes<-find_nodes()
                     edges<-find_edges()
                     net_shiny <- find_graph()
                     graph_undirected <- graph_from_data_frame(d=edges, vertices=totalNodes, directed=F)
                     #find mod for each algo
                     community <- cluster_infomap(net_shiny, e.weights = NULL, v.weights = NULL, nb.trials = 10, modularity = FALSE)
                     output$infomap_mod<-renderText(find_modularity(community))
                     
                     output$state_info<-renderText(rec(find_modularity(community)))
                     
                     
                     
                     community_1 <- cluster_edge_betweenness(graph_undirected,weights = E(graph_undirected)$weight,
                                                               directed = FALSE, edge.betweenness = TRUE, merges = TRUE,
                                                               bridges = TRUE, modularity = FALSE, membership = TRUE)
                     output$edge_mod<-renderText(find_modularity(community_1))
                     output$state_2<-renderText(rec(find_modularity(community_1)))
                     
                     community_2 <- cluster_fast_greedy(simplify(graph_undirected), merges = TRUE, modularity = TRUE,
                                                          membership = TRUE, weights = E(graph_undirected)$weight)
                     output$greed_mod<-renderText(find_modularity(community_2))
                     output$state_3<-renderText(rec(find_modularity(community_2)))
                     
                     community_3 <-cluster_label_prop(graph_undirected)
                     output$progrop_mod<-renderText(find_modularity(community_3))
                     output$state_4<-renderText(rec(find_modularity(community_3)))
                     
                     community_4 <-cluster_leading_eigen(graph_undirected)
                     output$eigen_mod<-renderText(find_modularity(community_4))
                     output$state_5<-renderText(rec(find_modularity(community_4)))
                     
                     community_5 <- com_fb_cluster_louvain<-cluster_louvain(graph_undirected, weights = NULL)
                     output$louvian_mod<-renderText(find_modularity(community_5))
                     output$state_6<-renderText(rec(find_modularity(community_5)))
                     
                     community_6 <- cluster_walktrap(net_shiny)
                     output$walktrap_mod<-renderText(find_modularity(community_6))
                     output$state_7<-renderText(rec(find_modularity(community_6)))
                     
                     
                     community_7 <- cluster_optimal(net_shiny)
                     output$modul_mod<-renderText(find_modularity(community_7))
                     output$state_8<-renderText(rec(find_modularity(community_7)))
                     
  
                                    })
    ############################
    #handle measurments 
    observeEvent(input$do_centrality,
                 {
                     req(find_nodes())
                     req(find_edges())
                     totalNodes<-find_nodes()
                     edges<-find_edges()
                     net_shiny <- find_graph()
                     graph_undirected <- graph_from_data_frame(d=edges, vertices=totalNodes, directed=F) 
                     degrees<-degree(net_shiny)
                     output$avg_deg<-renderText(mean(degrees))
                     bet<-betweenness(net_shiny)
                     output$avg_bet <-renderText(mean(bet))
                     cls<-closeness(net_shiny)
                     output$avg_clos <-renderText(mean(cls))
                     evec<-eigen_centrality(net_shiny)
                     output$avg_evec <-renderText(mean(evec$vector))
                     pa<-page.rank(net_shiny)
                     output$avg_pafe <-renderText(mean(pa$vector))
    })
    observeEvent(input$degree_details,{
        d<-degree(find_graph())
        d<-as.data.frame(d)
     names(d)<-c("degree")
        output$newWindowContent <- renderDataTable(
            d,options = list( scrollX = TRUE,  pageLength = 3,lengthMenu = c(3, 6, 9, 12)), rownames= TRUE)
        })
    observeEvent(input$bet_details,{
        d<-betweenness(find_graph())
        d<-as.data.frame(d)
        names(d)<-c("betwenness")
        output$newWindowContent <- renderDataTable(
            d,options = list( scrollX = TRUE,  pageLength = 3,lengthMenu = c(3, 6, 9, 12)), rownames= TRUE)
        })

    observeEvent(input$clos_details,{
        d<-closeness(find_graph())
        d<-as.data.frame(d)
        names(d)<-c("closeness")
        output$newWindowContent <- renderDataTable(
            d,options = list( scrollX = TRUE,  pageLength = 3,lengthMenu = c(3, 6, 9, 12)), rownames= TRUE)
    })
    
    observeEvent(input$eig_details,{
        d<-eigen_centrality(find_graph())
        d<-as.data.frame(d$vector)
        names(d)<-c("eigenvector")
        output$newWindowContent <- renderDataTable(
            d,options = list( scrollX = TRUE,  pageLength = 3,lengthMenu = c(3, 6, 9, 12)), rownames= TRUE)
    })
    observeEvent(input$page_details,{
        d<-page.rank(find_graph())
        d<-as.data.frame(d$vector)
        names(d)<-c("page rank")
        output$newWindowContent <- renderDataTable(
            d,options = list( scrollX = TRUE,  pageLength = 3,lengthMenu = c(3, 6, 9, 12)), rownames= TRUE)
    })
    #handle finding the influencer buttom
    observeEvent(input$do_Influence_pr,
                 {
                     req(find_nodes())
                     req(find_edges())
                     totalNodes<-find_nodes()
                     edges<-find_edges()
                     net_shiny <- find_graph()
                     graph_undirected <- graph_from_data_frame(d=edges, vertices=totalNodes, directed=F)
                     d<-page.rank(net_shiny)
                     d<-as.data.frame(d$vector)
                     names(d)<-c("page_rank")
                     output$the_page_rank<-renderText(max(d))
                     f<-as.numeric(unlist(d))
                     output$the_influencer<-renderText(which.max(f))
                     sort(f, decreasing = FALSE)
                     g<-as.data.frame(f)
                     output$Influencer_PR<-renderDataTable(
                         g,options=list( scrollX = TRUE,  pageLength = 3,lengthMenu = c(3, 6, 9, 12)), rownames= TRUE)
                     
                     
                     }
                 )
    }
