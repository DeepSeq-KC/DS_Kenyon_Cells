library(ggplot2)
library(dplyr)
library(ggiraph)
library(shiny)

rm(list=)

df <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/all_umis_merged.csv")
#df <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/cells_682-684_all_umis_merged.tsv")
#df <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/cells_0-250_all_umis_merged.csv")
meta <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/umi_with_metadata.csv") %>%
  mutate(cell_number = factor(cell_number))
exon_map <- read.csv("~/research/DS_Kenyon_Cells/data/06_tss_data/exon_local_coords.csv")

df <- df %>%
  mutate(coord = as.integer(as.character(coord))) %>%
  mutate(back_coord = as.integer(as.character(back_coord))) %>%
  left_join(meta) %>%
  mutate(Age = factor(Age, levels=c("0", "1", "3", "6", "9", "15", "30", "50"))) %>%
  mutate(Gender = factor(Gender)) %>%
  mutate(Genotype = factor(Genotype)) %>%
  mutate(hdb_clust = factor(hdb_clust, levels=c(-1:6))) %>%
  mutate(subtype = factor(subtype))




df <- df %>%
  mutate(facet = factor(facet, levels=c("Exon Map", "Read Mappings")))
exon_map <- exon_map %>%
  mutate(facet = factor(facet, levels=c("Exon Map", "Read Mappings")))

######################################
t_exon_map <- exon_map %>%
  filter(symbol == "Hsf")
  
df %>%
  filter(symbol == "Hsf") %>%
  ggplot() +
  geom_segment(data=t_exon_map, aes(x="Map", xend="Map", y=coord, yend=back_coord), size=2, color='orange', alpha=.6) +
  geom_point(aes(x=Age, y=coord, color=Age), position='jitter') +
  facet_grid(.~facet, space = "free", scales = "free") +
  theme_bw() +
  ylab("Gene Coordinates")
  
    
head(df)

ggiraph(code = print(gg))
#######################################################################


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
    
    t_exon_map <- exon_map %>%
      filter(gene_id == v$gene)
    
    exon_max <- t_exon_map %>%
      select(local_greater_coord) %>%
      max()
    exon_min <- t_exon_map %>%
      select(local_lesser_coord) %>%
      min()
    
    label_height = exon_max + (exon_max - exon_min)*.10
    
      
    
    gg <- df %>%
      filter(contig == v$gene) %>%
      ggplot() +
      geom_point(aes_string(x=v$var, y='coord', color=v$var), position='jitter') +
      geom_segment(data=t_exon_map, mapping=aes(x=0.5, xend=0.5, y=local_lesser_coord, yend=local_greater_coord), size=2, color='orange', alpha=.6) +
      annotate("text", x=0.5, y=label_height, label="Exon Map")
      
    
    ggiraph(code = print(gg))
  })
}

runApp(list(ui=ui,server=server),host="192.168.0.11",port=5001)


