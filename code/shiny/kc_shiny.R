library(ggplot2)
library(dplyr)
library(ggiraph)
library(shiny)

df <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/all_umis_merged.tsv", sep = '\t')
#df <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/cells_682-684_all_umis_merged.tsv")
#df <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/cells_0-250_all_umis_merged.csv")
meta <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/umi_with_metadata.csv")

df <- df %>%
  left_join(meta) %>%
  mutate(Age = factor(Age)) %>%
  mutate(Gender = factor(Gender)) %>%
  mutate(Genotype = factor(Genotype)) %>%
  mutate(hdb_clust = factor(hdb_clust)) %>%
  mutate(subtype = factor(subtype))

str(meta)
var_of_int <- c("Age", "Gender", "Genotype", "hdb_clust", "subtype")


ui <- fluidPage(
  titlePanel("Last Exon Viz tool"),
  fluidRow(
    column(width=2,
           selectizeInput("gene","Gene of Interest", sort(unique(df$contig)), selected = "FBgn0001222"),
           selectInput("var", "Variable of Interest", var_of_int, selected="Age"),
           actionButton("apply", "Apply")
    ),    
    column(width=10,
           ggiraphOutput("last_exon")
    )
  )
)

server <- function(input, output) {
  
  v <- reactiveValues()
  v$gene <- "FBgn0001222"
  v$var <- "Age"
  
  observeEvent(input$apply, {
    v$gene <- input$gene
    v$var <- input$var
  })
  
  

  
  output$last_exon <- renderggiraph({
    
    gg <- df %>%
      filter(contig == v$gene) %>%
      ggplot() +
      geom_point(aes_string(x=v$var, y='coord', color=v$var), position='jitter')
    
    ggiraph(code = print(gg))
  })
}

runApp(list(ui=ui,server=server),host="10.124.159.146",port=5001)


