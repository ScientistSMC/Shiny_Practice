library(shiny)
library(ggplot2)

metabs <- read.csv("data/MetabData.csv")
metabs$Group <- factor(metabs$Group, levels = c("Scrambled","Down","Up", "CRISPR 2-12", 
                                                "CRISPR 2-19", "CRISPR 5-50"))
metabnames<-c(paste("M", seq(1:567), sep=""))
metabnames2<-read.csv("data/BiochemNames.csv")
metabnames3<-read.csv("data/MNames.csv")

ui <- fluidPage("Visualizing Metabolite Relative Abundances in MDA-MB-231 Breast Cancer Cell 
                Lines That Vary in Arylamine N-Acetyltransferase 1 (NAT1)", 
                selectInput(inputId="metab",label="Choose a Metabolite", 
                            choices=metabnames, selected="M1"),
                plotOutput("bp"))

server <- function(input, output) {

  output$bp <- renderPlot({
    Data <- data.frame(Group = metabs$Group, var = (metabs[[input$metab]]))

   p<- ggplot(Data, aes(x=Group,y=var, fill=Group)) +
      stat_boxplot(geom ='errorbar',width = 0.35) +
      geom_boxplot(outlier.shape=0) +
      labs(x="", y = "Median Scaled Relative Abundance", title=input$metab) +
      theme_bw()+theme(legend.position = "none",
                       axis.text.x=element_text(color="black", size=18),
                       axis.title.x=element_text(face="bold", size=16),
                       axis.title.y = element_text(face="bold", size=18),
                       axis.text.y=element_text(size=20, color="black"),
                       plot.title = element_text(face="bold", color = "black", size=22, hjust=.5))
    print(p)
  })
}

shinyApp(ui=ui, server=server)