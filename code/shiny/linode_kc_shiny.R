library(ggplot2)
library(dplyr)
library(ggiraph)
library(shiny)

df <- read.csv("./data/06_tss_data/all_umis_merged.csv")

meta <- read.csv("./data/06_tss_data/umi_with_metadata.csv") %>%
  mutate(cell_number = factor(cell_number))

exon_map <- read.csv("./data/06_tss_data/exon_local_coords.csv") %>%
  mutate(facet = factor(facet, levels=c("Exon Map", "Read Mappings")))

df <- df %>%
  mutate(coord = as.integer(as.character(coord))) %>%
  mutate(facet = factor(facet, levels=c("Exon Map", "Read Mappings"))) %>%
  left_join(meta) %>%
  mutate(Age = factor(Age, levels=c("0", "1", "3", "6", "9", "15", "30", "50"))) %>%
  mutate(Gender = factor(Gender)) %>%
  mutate(Genotype = factor(Genotype)) %>%
  mutate(hdb_clust = factor(hdb_clust, levels=c(-1:6))) %>%
  mutate(subtype = factor(subtype))

var_of_int <- c("Age", "Gender", "Genotype", "hdb_clust", "subtype")


ui <- fluidPage(
  titlePanel("Last Exon Viz tool"),
  fluidRow(
    column(width=2,
           selectizeInput("gene","Gene of Interest", sort(unique(df$symbol)), selected = "nAChRalpha1"),
           selectInput("xvar", "X-Axis", var_of_int, selected="Age"),
           selectInput("cvar", "Color By", var_of_int, selected="Age"),
           actionButton("apply", "Apply"),
           downloadButton('downloadPlot', 'Download Plot')
    ),    
    column(width=10,
           ggiraphOutput("last_exon")
    )
  )
)

server <- function(input, output) {
  
  v <- reactiveValues()
  v$gene <- "nAChRalpha1"
  v$xvar <- "Age"
  v$cvar <- "Age"
  
  observeEvent(input$apply, {
    v$gene <- input$gene
    v$xvar <- input$xvar
    v$cvar <- input$cvar
    
  })
  


  output$last_exon <- renderggiraph({
    
    max_exon <- df %>%
      filter(symbol == v$gene) %>%
      select(coord) %>%
      max()
    min_exon <- df %>%
      filter(symbol == v$gene) %>%
      select(coord) %>%
      min()
    t_exon_map <- exon_map %>%
      filter(symbol == v$gene) %>%
      filter(coord >= min_exon) %>%
      filter(coord <= max_exon)

    
    gg <- df %>%
      filter(symbol == v$gene) %>%
      ggplot() +
      geom_point(aes_string(x=v$xvar, y='coord', color=v$cvar), position='jitter') +
      geom_segment(data=t_exon_map, mapping=aes(x="Map", xend="Map", y=coord, yend=back_coord), size=2, color='orange', alpha=.6) +
      facet_grid(.~facet, space = "free", scales = "free") +
      theme_bw() +
      ylab("Gene Coordinates")
    
    ggiraph(code = print(gg))
  })
  
  plotInput = reactive({
    max_exon <- df %>%
      filter(symbol == v$gene) %>%
      select(coord) %>%
      max()
    min_exon <- df %>%
      filter(symbol == v$gene) %>%
      select(coord) %>%
      min()
    t_exon_map <- exon_map %>%
      filter(symbol == v$gene) %>%
      filter(coord >= min_exon) %>%
      filter(coord <= max_exon)
    
    
    gg <- df %>%
      filter(symbol == v$gene) %>%
      ggplot() +
      geom_point(aes_string(x=v$xvar, y='coord', color=v$cvar), position='jitter') +
      geom_segment(data=t_exon_map, mapping=aes(x="Map", xend="Map", y=coord, yend=back_coord), size=2, color='orange', alpha=.6) +
      facet_grid(.~facet, space = "free", scales = "free") +
      theme_bw() +
      ylab("Gene Coordinates")
    return(gg)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$gene, '.pdf', sep='') },
    content = function(file) { ggsave(file, plotInput(), "pdf") }
  )
}



runApp(list(ui=ui,server=server),host="172.104.22.51",port=5001)


