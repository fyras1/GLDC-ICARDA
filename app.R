library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)



#Creates the corresponding dataframe for each region
ls<-list();
df9<-data.frame();
for (i in 1:8){
    fileName<-paste0("data/data_",as.character(i),".csv");
    df<-data.frame();
    df<-read.csv(fileName);
    names(df)<-c("ID","Crops","Technology_Options","BCR","Poverty","Nutrition")
    ls<-c(ls,list(df))
  
     df9<-rbind(df9,df)
    
}

ls[[9]]<-df9;
ls[[10]]<-df9;


#Normalizes the matrix
stnd<-function(x)
{
     x<-x/sqrt(sum(x^2))
    x
}


#calculates the weighted noramalized decision matrix
trans<-function(df,c1,c2,c3){
    
    dfx<-df
    dfx$score=0;
    
    dfx$BCR<-stnd(dfx$BCR)*c1
    dfx$Poverty<-stnd(dfx$Poverty)*c2
    dfx$Nutrition<-stnd(dfx$Nutrition)*c3
    dfx
    
}

#Sorts a table
sort_table<-function(dfx){
dfx<-dfx[order(-dfx$p),]

dfx[,c(input$id,input$crops,input$tech,input$bcr,input$poverty,input$nut,input$p)]
}


calc_p<-function(df,c1,c2,c3)
{
    df2<-trans(df,c1,c2,c3)
    
    
    # Determine the worst alternative (vp) and the best alternative (vm)
    vp<-c(max(df2[,4]) , max(df2[,5]), max(df2[,6]))
    vm<-c(min(df2[,4]) , min(df2[,5]), min(df2[,6]))
    
    
    # We Calculate the L^2 distance between the target alternative (i) and the worst
    # condition (vp) , and the distance between the target alternative (i) and the best condition (vm)
    
    sim<-0
    sip<-0
    for(i in 1:nrow(df2)){
        for(j in 4:6){
            sip<-sip+(df2[i,j]-vp[j-3])^2
            sim<-sim+(df2[i,j]-vm[j-3])^2
        }
        sim<-sqrt(sim)
        sip<-sqrt(sip)
        df2[i,"sip"]<-sip
        df2[i,"sim"]<-sim
        sim<-0
        sip<-0
    }
    
    #calculates the similarity to the best conditio
    df2$p<-df2$sim/(df2$sim+df2$sip)
    
    
    #We rank the alternatives based on their P 
    dfx<-df
    dfx$p<-df2$p
    dfx<-dfx[order(-dfx$p),]
    
    #Resulting table.
    dfx
}

ui<-fluidPage(theme = shinytheme("cerulean"),
    titlePanel("GLDC Ranking - icarda"),
    sidebarLayout(
        sidebarPanel(
            numericInput("c1","Weight of BCR",0.333,min=0,max=1),
            numericInput("c2","Weight of Pov.Change",0.333,min=0,max=1),
            numericInput("c3","Weight of Nutrition",0.333,min=0,max=1),
            selectInput("region", "Region:",
                        c("All regions" = "9",
                          
                          "Semi-arid West & Central Africa" = "1",
                          "Dry sub-humid West & Central Africa." = "2",
                          "Semi-arid East Africa" = "3",
                          "Dry sub-humid East Africa" = "4",
                          "Semi-arid Southern Africa" = "5",
                          "Dry sub-humid Southern Africa" = "6",
                          "Semi-arid South Asia" = "7",
                          "dry sub-humid South Asia" = "8")),
            h4("Show"),
            checkboxInput("id", "label", TRUE),
            checkboxInput("crops", "Crops", FALSE),
            checkboxInput("tech", "Technology options", FALSE),
            checkboxInput("bcr", "BCR", TRUE),
            checkboxInput("poverty", "Poverty", TRUE),
            checkboxInput("nut", "Nutrition", TRUE),
            checkboxInput("p", "P score", TRUE)

        ),
        mainPanel(
            
            tabsetPanel(type="tabs",
                        tabPanel("Table",h4("Ranking"), DT::dataTableOutput("dfx")),
                        tabPanel("Graph",h4("Ranking"), plotOutput("graph"))
        )
    )
)
)



##################################################################
############################# SERVER #############################


server<- function(input, output, session) {
    

    dfx<-reactive({
        c1<-input$c1
        c2<-input$c2
        c3<-input$c3
        rg<-as.numeric(input$region)
        
        df<-ls[[rg]]

        dfx<-calc_p(df,c1,c2,c3)
         
         df_final<-dfx[,c(input$id,input$crops,input$tech,input$bcr,input$poverty,input$nut,input$p)]
         
       

         df_final
         
  
      
    })
    
    

    
    

 
       output$dfx <- DT::renderDataTable(dfx(), selection= 'none' , editable = TRUE)
  
    
    



    
    output$graph<-renderPlot({
        
        
        
        g<-ggplot(dfx(),aes(x=1:nrow(dfx()),y=p))+
            geom_point(size=2.5,colour="red")+
            geom_line(lwd=1.1,colour="red")+
           # geom_text(aes(label=ID),hjust=-0.7, vjust=-0.7)+
            labs(x="Rank",y="P")
        
        if(input$region!="9")
            g<-g+geom_text(aes(label=ID),hjust=-0.7, vjust=-0.7)
        
        g
        
    })
    
    
    
}



shinyApp(ui,server)




