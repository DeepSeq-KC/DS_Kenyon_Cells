# Command to push umi data to server
scp ~/research/DS_Kenyon_Cells/data/06_tss_data/all_umis_merged.csv thomas@172.104.22.51:/home/thomas/shiny/data/06_tss_data
# Command to push metadata for each cell to server
scp ~/research/DS_Kenyon_Cells/data/06_tss_data/umi_with_metadata.csv thomas@172.104.22.51:/home/thomas/shiny/data/06_tss_data
# command to push exon map data to server
scp ~/research/DS_Kenyon_Cells/data/06_tss_data/exon_local_coords.csv thomas@172.104.22.51:/home/thomas/shiny/data/06_tss_data


# command to push actual shiny script to server
scp /Users/thomas/research/DS_Kenyon_Cells/code/shiny/linode_kc_shiny.R thomas@172.104.22.51:/home/thomas/shiny