####################################################
# R code for the custom functions used to identify #
#  papers that reported genetic diversity metrics  #
####################################################

# Packages required
library(pdfsearch)
library(dplyr)
library(stringi)

################################################################
# 1. Identify page numbers of the methods and results sections #
################################################################

# Create function for page number identification
# Note that papers are in subfolders labelled:
# "Batch_1, Batch 2, ... etc." 
# within a parent folder, specified using "inPath"

Find.Section.Pg.Nos <- function(inPath, outPath, Batch) {
  
  # Get list of all PDFs in path
  PDF_List <- list.files(paste0(inPath, "Batch_", Batch, "/"))
  
  ############################## Search: Paper Sections ##############################
  # Search all variations of the section headings, including strange formatting.
  # Formatting problems were determined by loading papers that weren’t picked up one-by-one
  # using read_pdf() in the textreadr package. Most failed because the pdf reader doesn't 
  # handle bulleted points and caps well, and common formatting for these were included below.
  
  Methods.Keywords <- c("Method",
                        "Material",
                        "Material and Method",
                        "Materials and Method",
                        "Material & Method",
                        "Materials & Method",
                        "M ATE R I A L S A N D M E TH O DS",
                        "M A T E R I A L S A N D M ET H O D S")
  
  Disc.Keywords <- c("Results and Discussion",
                     "Results & Discussion",
                     "Discussion",
                     "DI SCU SSION",
                     "D I S CU S S I O N",
                     "DI SCU SSION",
                     "D I S CU S S I O N",
                     "D IS C U SSION",
                     "D I S C U SS I O N",
                     "DIS CUS S IO N",
                     "DIS CUSSION",
                     "DISCUSSI O N")
  
  Ack.Keywords <- "Acknowledgements"
  
  # Run search
  start.time <- Sys.time() # time how long search takes
  SectionSearch <- keyword_directory(directory = paste0(inPath, "Batch_", Batch, "/"), 
                                     keyword = c(Methods.Keywords, 
                                                 Disc.Keywords, 
                                                 Ack.Keywords),
                                     surround_lines = 1,
                                     recursive = TRUE,  
                                     split_pdf = TRUE,
                                     ignore_case = TRUE, 
                                     remove_hyphen = FALSE)
  end.time <- Sys.time()
  cat(paste("Search time over", length(PDF_List), "papers:", end.time - start.time, sep = " "), "\n")
  
  # IMPORTANT NOTE: The page numbers in the PDF do not match the actual document pages.
  # This discrepancy is due to how the 'pdfsearch' package reads in the PDF.
  # However, the page numbering is consistent every time the PDF is read into R.
  # Therefore, the location is accurate relative to the intended starting/stopping points 
  # for the metric search, even if the absolute page numbers are incorrect.
  
  
  # List papers with no hits (indicates there might be a problem/pdf is unreadable)
  if(length(setdiff(PDF_List, as.character(unique(SectionSearch$pdf_name)))) > 0) {
    
    PDFs_to_check <- setdiff(PDF_List, as.character(unique(SectionSearch$pdf_name)))
    
    #Write PDFs to check to csv, append to existing file (i.e., so all batches in same file)
    write.table(data.frame(PDF_Name = PDFs_to_check), paste0(outPath, "PDFs_to_check.csv"), 
                row.names = FALSE, append = TRUE, quote = TRUE, sep = ",", 
                col.names = !file.exists(paste0(outPath, "PDFs_to_check.csv")))
    
    # Copy potentially unreadable papers to new folder for manual check
    if(!dir.exists(paste0(outPath, "PDFs_to_Check"))) {
      dir.create(path = paste0(outPath, "PDFs_to_Check"))
    }
    
    PDFNo <- 0
    
    for(PDFNo in (1:length(PDFs_to_check)-1)) { 
      PDF <- PDFNo + 1
      file.copy(from = file.path(paste0(inPath, "Batch_", Batch, "/", PDFs_to_check[PDF])),
                to = paste0(outPath, "PDFs_to_Check/"), overwrite = FALSE)
    }
    
    cat(paste(length(PDFs_to_check), "PDFs may be unreadable, check these manually (copied to PDFs_to_Check folder)"), "\n")
  } else {
    cat("All PDFs were readable", "\n")
  }
  
  ######################################### METHODS #####################################################
  # Not all keywords are equal. For example, "Material and Method" is more likely to actually be a heading 
  # than "method" (which may just be used in a sentence). Create a category column, recode keywords (a-c) 
  # in order of the most likely to be heading. Then order and only keep the first entry for each paper. 
  # Finally, if methods section found on a page number > 4, replace this page number with a 1 (i.e., to be
  # conservative if section header is not being picked up, or if the method section is at the end of paper).
  
  # Create new data frame with methods keywords only
  Methods_Head <- SectionSearch %>%
    filter(keyword %in% Methods.Keywords)
  
  # Recode keywords in new column
  Methods_Head$keywordCat <- Methods_Head$keyword
  
  Methods_Head$keywordCat[Methods_Head$keywordCat == "M ATE R I A L S A N D M E TH O DS" | Methods_Head$keywordCat == "M A T E R I A L S A N D M ET H O D S" | Methods_Head$keywordCat == "Materials & Method" | Methods_Head$keywordCat == "Materials and Method" | Methods_Head$keywordCat == "Material & Method" | Methods_Head$keywordCat == "Material and Method"] <- "a"
  Methods_Head$keywordCat[Methods_Head$keywordCat == "Method"] <- "b"
  Methods_Head$keywordCat[Methods_Head$keywordCat == "Material"] <- "c"
  
  # Order alphabetically within each pdf
  Methods_Head_arranged <- arrange(Methods_Head, pdf_name, keywordCat, page_num) 
  
  # Remove duplicates, only keeping the first entry per pdf
  Methods_Head_First <- Methods_Head_arranged[!duplicated(Methods_Head_arranged$pdf_name),] 
  
  # Search from page 1 if methods page > 4 (in case methods at the end of paper) 
  # or if methods section not found
  Methods_Head_First$page_num[Methods_Head_First$page_num > 4] <- 1
  
  ########################################### DISCUSSION ############################################
  # Don't use page numbers for 'results and discussion' sections since we want to include the results 
  # section in our search (delete these pdfs so there's no entry). Also, delete pdf if 'discussion' found 
  # on page 1, as this is most likely in the abstract. Next, create a category column, recode keywords (a-b)
  # in order of the most likely to be heading. Then order and only keep the first entry for each paper.   
  
  # Create new data frame with discussion keywords only
  Disc_Head <- SectionSearch %>%
    filter(keyword %in% Disc.Keywords)
  
  # Remove combined results and discussion sections
  Discussion_No_Results_pdfs <- Disc_Head[(Disc_Head$keyword == "Results and Discussion") | (Disc_Head$keyword == "Results & Discussion"), ]$pdf_name
  
  Discussion_No_Results <- Disc_Head[!(Disc_Head$pdf_name %in% Discussion_No_Results_pdfs), ]
  
  # Remove discussion in abstract
  Disc_Head_ResAbs <- Discussion_No_Results[!(Discussion_No_Results$page_num == 1), ] 
  
  # Recode keywords in new column
  Disc_Head_ResAbs$keywordCat <- Disc_Head_ResAbs$keyword
  
  Disc_Head_ResAbs$keywordCat[Disc_Head_ResAbs$keywordCat == "DI SCU SSION" | Disc_Head_ResAbs$keywordCat == "D I S CU S S I O N" | Disc_Head_ResAbs$keywordCat == "DI SCU SSION" | Disc_Head_ResAbs$keywordCat == "D I S CU S S I O N" | Disc_Head_ResAbs$keywordCat == "D IS C U SSION" | Disc_Head_ResAbs$keywordCat == "D I S C U SS I O N" | Disc_Head_ResAbs$keywordCat == "DIS CUS S IO N" | Disc_Head_ResAbs$keywordCat == "DIS CUSSION" | Disc_Head_ResAbs$keywordCat == "DISCUSSI O N"] <- "a"
  Disc_Head_ResAbs$keywordCat[Disc_Head_ResAbs$keywordCat == "Discussion"] <- "b"
  
  # Order alphabetically within each pdf
  Disc_Head_arranged <- arrange(Disc_Head_ResAbs, pdf_name, keywordCat, page_num)
  
  # Remove duplicates, only keeping the first entry per pdf
  Disc_Head_First <- Disc_Head_arranged[!duplicated(Disc_Head_arranged$pdf_name),]
  
  ######################################### ACKNOWLEDGEMENTS #############################################
  # Delete pdf if 'acknowledgements' found on page 1, as we want it to be the last section before the refs.
  # Next, sort within pdfs by page number in descending order, to get the last page number (hopefully this 
  # is the last page, excluding the ref list). Then only keep the first entry for each paper.
  
  # Create new data frame with acknowledgements keywords only
  Ack_Head <- SectionSearch %>%
    filter(keyword %in% Ack.Keywords)
  
  # Remove acknowledgements on page 1
  Ack_Head_DelAbs <- Ack_Head[!(Ack_Head$page_num == 1),]
  
  # Arrange in descending order by page number within each pdf
  Ack_Head_arranged <- arrange(Ack_Head_DelAbs, pdf_name, desc(page_num))
  
  # Remove duplicates, only keeping the last entry per pdf
  Ack_Head_First <- Ack_Head_arranged[!duplicated(Ack_Head_arranged$pdf_name),]
  
  ######################## Create data frame with section pages ######################## 
  # Create a new data frame with the page numbers of the methods section (start search) through to the 
  # end of the results section (stop search). If the methods section wasn't found, start from page 1. If 
  # the discussion section wasn't found, use the acknowledgements section. If this also wasn't found, stop at
  # page 50 (i.e., much longer than most papers, so will search through all pages).
  
  # Subset data frame to pdf name and page no.
  Meth_Pages <- Methods_Head_First[, c(2, 4)]
  Disc_Pages <- Disc_Head_First[, c(2, 4)]
  Ack_Pages <- Ack_Head_First[, c(2, 4)]
  
  # Create data frame listing all pdfs in folder
  pdf_df <- data.frame(pdf_name = PDF_List)
  
  # Change to character (rather than factor) so we can filter by name 
  pdf_df$pdf_name <- as.character(pdf_df$pdf_name)
  Meth_Pages$pdf_name <- as.character(Meth_Pages$pdf_name)
  Disc_Pages$pdf_name <- as.character(Disc_Pages$pdf_name)
  Ack_Pages$pdf_name <- as.character(Ack_Pages$pdf_name)
  
  # Combine methods, discussion and acknowledgements dfs by "pdf_name"
  Section_Pages <- pdf_df %>% 
    left_join(Meth_Pages, by = "pdf_name") %>% 
    rename(Meth_Pages = page_num) %>% 
    left_join(Disc_Pages, by = "pdf_name") %>% 
    rename(Disc_page_num = page_num) %>% 
    left_join(Ack_Pages, by = "pdf_name") %>% 
    rename(Ack_page_num = page_num)
  
  # If methods section not found, replace with page 1
  Section_Pages$Meth_Pages[is.na(Section_Pages$Meth_Pages)] <- 1
  
  # If discussion section not found, replace with acknowledgements page no.
  Section_Pages$Disc_page_num[is.na(Section_Pages$Disc_page_num)] <- Section_Pages$Ack_page_num[is.na(Section_Pages$Disc_page_num)]
  
  # If discussion/acknowledgements sections not found, replace with page 50
  Section_Pages$Disc_page_num[is.na(Section_Pages$Disc_page_num)] <- 50
  
  # Create final data frame
  StartStop_Page_Search <- Section_Pages[ , 1:3]
  colnames(StartStop_Page_Search)[2:3] <- c("Start_Page", "Stop_Page")
  StartStop_Page_Search$Batch <- paste("Batch", Batch, sep = "_")
  StartStop_Page_Search <- StartStop_Page_Search[, c(1, 4, 2:3)]
  
  #Write to csv, append to existing file (i.e., so all batches in same file)
  write.table(StartStop_Page_Search, paste0(outPath, "StartStop_Page_Search.csv"), row.names = FALSE, append = TRUE, quote = TRUE, sep = ",", col.names = !file.exists(paste0(outPath, "StartStop_Page_Search.csv")))
  
}



################################################################################
# 2. Search for metrics likely to be in studies that measure genetic diversity #
################################################################################


Metric.Search <- function(inPath, outPath, inFile_PgNos, Batch) {
  
  # Get list of all PDFs in path
  PDF_List <- list.files(paste0(inPath, "Batch_", Batch))
  
  # What are we trying to capture?
  ## Alleles (mean/total number, private), allelic richness (AR), 
  ## diversity (nucleotide, haplotype, Shannon diversity index), 
  ## Number of polymorphic loci, Polymorphic Information Criteria (PIC),
  ## Heterozygosity (He, Ho), F/G statistics, effective population size (Ne),
  ## Approximate Bayesian Computation, MSVAR, BOTTLENECK
  
  Alleles.keywords <- "alleles"
  AR.keywords <- c("allelic")
  Diversity.keywords <- "diversity"
  Poly.keywords <- "Polymorphic"
  HoHe.keywords <- c("Heterozygosity", 
                     " Ho ", 
                     " Ho=",
                     " Ho,",
                     " He ", 
                     " He=",
                     " He,")
  F_G_STATS.keywords <- c("F-statistics", 
                          "G-statistics", 
                          "F statistics", 
                          "G statistics", 
                          "FST", 
                          "F'ST", 
                          "GST", 
                          "G'ST")
  Ne.keywords <- c("population", 
                   " Ne ", 
                   " Ne=",
                   " Ne,")
  ABC.keywords <- c("ABC", 
                    "ONESAMP",
                    "Bayesian")
  MSVAR.keywords <- "MSVAR"
  Bottleneck.keywords <- "Bottleneck"
  
  start.time <- Sys.time() # time how long search takes
  
  Metric_Search <- keyword_directory(directory = paste0(inPath, "Batch_", Batch),
                                     keyword = c(Alleles.keywords, 
                                                 AR.keywords, 
                                                 Diversity.keywords, 
                                                 Poly.keywords, 
                                                 HoHe.keywords, 
                                                 F_G_STATS.keywords, 
                                                 Ne.keywords, 
                                                 ABC.keywords, 
                                                 MSVAR.keywords, 
                                                 Bottleneck.keywords),
                                     recursive = TRUE,
                                     surround_lines = 1, 
                                     split_pdf = TRUE,
                                     ignore_case = TRUE, 
                                     remove_hyphen = FALSE)
  
  end.time <- Sys.time()
  cat(paste("Search time over", length(PDF_List), "papers:", end.time - start.time, sep = " "), "\n")
  
  Metric.Pg.Nos <- Metric_Search[, c(2:4, 6)]
  Metric.Pg.Nos$pdf_name <- as.character(Metric.Pg.Nos$pdf_name)
  StartStop_Page_Search <- read.csv(inFile_PgNos)
  StartStop_Page_Search$pdf_name <- as.character(StartStop_Page_Search$pdf_name)
  StartStop_Page_Search$Batch <- as.character(StartStop_Page_Search$Batch)
  Metric_StartStop <- left_join(Metric.Pg.Nos, StartStop_Page_Search, by = "pdf_name")
  
  Txt_Lines_NA <- lapply(Metric_StartStop$line_text, function(x) x[!is.na(x)])
  
  Join_Txt_Lines <- lapply(Txt_Lines_NA, function(x) stri_flatten(x, collapse = " "))
  
  PDFs_Metric_Hit <- data.frame(pdf_name = character(), Metric = character())
  
  ######################## Alleles (mean/total number, private) ######################## 
  
  ### Mean number of Alleles ###
  Metric_StartStop$MeanNoAlleles <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "mean", ignore.case = TRUE) & grep(x, pattern = "number", ignore.case = TRUE))) >= 1)
  
  MeanNoAlleles_df <- Metric_StartStop %>%
    filter(MeanNoAlleles == TRUE) %>% 
    filter(keyword %in% Alleles.keywords) %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(MeanNoAlleles_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(MeanNoAlleles_df$pdf_name)), Metric = "Mean number of alleles"))
  }
  
  ### Total number of Alleles ###
  Metric_StartStop$TotalNoAlleles <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "total", ignore.case = TRUE) & grep(x, pattern = "number", ignore.case = TRUE))) >= 1)
  
  TotalNoAlleles_df <- Metric_StartStop %>%
    filter(TotalNoAlleles == TRUE) %>% 
    filter(keyword %in% Alleles.keywords) %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(TotalNoAlleles_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(TotalNoAlleles_df$pdf_name)), Metric = "Total number of alleles"))
  }
  
  ### Private Alleles ###
  Metric_StartStop$PrivateAlleles <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "private", ignore.case = TRUE))) >= 1)
  
  PrivateAlleles_df <- Metric_StartStop %>%
    filter(PrivateAlleles == TRUE) %>% 
    filter(keyword %in% Alleles.keywords) %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(PrivateAlleles_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(PrivateAlleles_df$pdf_name)), Metric = "Private alleles"))
  }
  
  ################################# Allelic Richness ###################################
  
  Metric_StartStop$AllRichness <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "richness", ignore.case = TRUE))) >= 1)
  
  AllRichness_df <- Metric_StartStop %>%
    filter(AllRichness == TRUE) %>% 
    filter(keyword == "allelic") %>% 
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(AllRichness_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(AllRichness_df$pdf_name)), Metric = "Allelic richness"))
  }
  
  ############# Diversity (nucleotide, haplotype, Shannon diversity index) #############
  
  ### Nucleotide Diversity ###  
  Metric_StartStop$NucDiv <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "nucleotide", ignore.case = TRUE))) >= 1)
  
  NucDiv_df <- Metric_StartStop %>%
    filter(NucDiv == TRUE) %>% 
    filter(keyword %in% Diversity.keywords) %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(NucDiv_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(NucDiv_df$pdf_name)), Metric = "Nucleotide diversity"))
  }
  
  ### Haplotype Diversity ###  
  Metric_StartStop$HapDiv <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "haplotype", ignore.case = TRUE))) >= 1)
  
  HapDiv_df <- Metric_StartStop %>%
    filter(HapDiv == TRUE) %>% 
    filter(keyword %in% Diversity.keywords) %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(HapDiv_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(HapDiv_df$pdf_name)), Metric = "Haplotype diversity"))
  }
  
  ### Shannon Diversity ###
  Metric_StartStop$ShanDiv <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "Shannon", ignore.case = TRUE))) >= 1)
  
  ShanDiv_df <- Metric_StartStop %>%
    filter(ShanDiv == TRUE) %>% 
    filter(keyword %in% Diversity.keywords) %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(ShanDiv_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(ShanDiv_df$pdf_name)), Metric = "Shannon diversity"))
  }
  
  ########## No. polymorphic loci and Polymorphic Information Criteria (PIC) ########## 
  
  ##### No. polymorphic loci #####
  Metric_StartStop$PolyLoci <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "number", ignore.case = TRUE) & grep(x, pattern = "loci", ignore.case = TRUE))) >= 1)
  
  PolyLoci_df <- Metric_StartStop %>%
    filter(PolyLoci == TRUE) %>% 
    filter(keyword == "Polymorphic") %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(PolyLoci_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(PolyLoci_df$pdf_name)), Metric = "Number of polymorphic loci"))
  }
  
  ##### PIC #####
  Metric_StartStop$PIC <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "information", ignore.case = TRUE) & grep(x, pattern = "criteria", ignore.case = TRUE))) >= 1)
  
  PIC_df <- Metric_StartStop %>%
    filter(PIC == TRUE) %>% 
    filter(keyword == "Polymorphic") %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(PIC_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(PIC_df$pdf_name)), Metric = "Polymorphic Information Content (PIC)"))
  }
  
  ############################### Heterozygosity (He, Ho) ##############################
  Het_df <- Metric_StartStop %>%
    filter(keyword == "Heterozygosity") %>% 
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  HoHe_df <- Metric_StartStop %>%
    filter(keyword == " Ho " | keyword == " Ho=" | keyword == " Ho," | keyword == " He " | keyword == " He=" | keyword == " He,") %>% 
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(isTRUE(dim(Het_df)[1] > 0) & isTRUE(dim(HoHe_df)[1] > 0) & (length(intersect(as.character(unique(Het_df$pdf_name)), as.character(unique(HoHe_df$pdf_name)))) > 0)) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = intersect(as.character(unique(Het_df$pdf_name)), as.character(unique(HoHe_df$pdf_name))), Metric = "Heterozygosity (Ho, He)"))
  }
  
  ####################### F/G statistics (FST, GST, F'ST, G'ST) ########################
  F_G_STATS_df <- Metric_StartStop %>%
    filter(keyword %in% F_G_STATS.keywords) %>% 
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(F_G_STATS_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(F_G_STATS_df$pdf_name)), Metric = "F/G Statistics (FST, GST, F'ST, G'ST)"))
  }
  
  ############################ Effective population size (Ne) ###########################
  Metric_StartStop$EffPopSize <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "effective", ignore.case = TRUE) & grep(x, pattern = "size", ignore.case = TRUE))) >= 1)
  
  EffPopSize_df <- Metric_StartStop %>%
    filter(EffPopSize == TRUE) %>% 
    filter(keyword == "population")
  
  Ne_df <- Metric_StartStop %>%
    filter(keyword == " Ne " | keyword == " Ne=" | keyword == " Ne,") %>% 
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(isTRUE(dim(EffPopSize_df)[1] > 0) & isTRUE(dim(Ne_df)[1] > 0) & (length(intersect(as.character(unique(EffPopSize_df$pdf_name)), as.character(unique(Ne_df$pdf_name)))) > 0)) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = intersect(as.character(unique(EffPopSize_df$pdf_name)), as.character(unique(Ne_df$pdf_name))), Metric = "Ne"))
  }
  
  ######################### Approximate Bayesian Computation (ABC) ########################
  Metric_StartStop$ABC <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "Approximate", ignore.case = TRUE))) >= 1)
  
  ApproxBayesian_df <- Metric_StartStop %>%
    filter(ABC == TRUE) %>% 
    filter(keyword == "Bayesian")
  
  ABC_df <- Metric_StartStop %>%
    filter(keyword == "ABC" | keyword == "ONESAMP") %>% 
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  
  if(isTRUE(dim(ApproxBayesian_df)[1] > 0) & isTRUE(dim(ABC_df)[1] > 0) & (length(intersect(as.character(unique(ApproxBayesian_df$pdf_name)), as.character(unique(ABC_df$pdf_name)))) > 0)) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = intersect(as.character(unique(ApproxBayesian_df$pdf_name)), as.character(unique(ABC_df$pdf_name))), Metric = "ABC"))
  }
  
  ######################################## MSVAR #######################################
  MSVAR_df <- Metric_StartStop %>%
    filter(keyword %in% MSVAR.keywords) %>% 
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(MSVAR_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(MSVAR_df$pdf_name)), Metric = "MSVAR"))
  }
  
  ###################################### BOTTLENECK ######################################
  Metric_StartStop$Bottleneck <- !is.na((lapply(Join_Txt_Lines, function(x) grep(x, pattern = "Piry", ignore.case = TRUE))) >= 1)
  
  Bottleneck_df <- Metric_StartStop %>%
    filter(Bottleneck == TRUE) %>% 
    filter(keyword %in% Bottleneck.keywords) %>%
    filter(page_num >= Start_Page) %>%
    filter(page_num <= Stop_Page)
  
  if(dim(Bottleneck_df)[1] > 0) {
    PDFs_Metric_Hit <- rbind(PDFs_Metric_Hit, data.frame(pdf_name = as.character(unique(Bottleneck_df$pdf_name)), Metric = "Bottleneck (Piry)"))
  }
  
  
  PDFs_Metric_Hit <- arrange(PDFs_Metric_Hit, pdf_name, Metric)
  
  #Write to csv, append to existing file (i.e., so all batches in same file)
  write.table(PDFs_Metric_Hit, paste0(outPath, "PDFs_Metric_Hits.csv"), row.names = FALSE, append = TRUE, quote = TRUE, sep = ",", col.names = !file.exists(paste0(outPath, "PDFs_Metric_Hits.csv")))
  
  
  if(!dir.exists(paste0(outPath, "PDFs_Genetic_Diversity_Hits/"))) {
    dir.create(path = paste0(outPath, "PDFs_Genetic_Diversity_Hits"))
  }
  
  
  if(dim(PDFs_Metric_Hit)[1] > 0) {
    PDFs_Keep <- as.character(unique(PDFs_Metric_Hit$pdf_name))
    HitNo <- 0
    for(HitNo in (1:length(PDFs_Keep)-1)) { 
      Hit <- HitNo + 1
      file.copy(from = file.path(paste0(inPath, "Batch_", Batch, "/", PDFs_Keep[Hit])),
                to = paste0(outPath, "PDFs_Genetic_Diversity_Hits/"), overwrite = FALSE)
    }
    cat(paste("Metric search is now complete.", "Hits:", length(unique(as.character(PDFs_Metric_Hit$pdf_name))), "of", length(PDF_List), "papers. Copied to PDFs_Genetic_Diversity_Hits folder."), "\n")
    
  } else {
    print("No metrics hit within PDFs")
  }
}




#####################################
# 3. Run functions on test data set #
#####################################

# Run page number function
# This will output a csv file that identifies the start/stop location 
# of the metric search ("StartStop_Page_Search")
# It will also move "unreadable pdfs" into a separate folder
# IMPORTANT NOTE: The page numbers in the PDF do not match the actual document pages.
# This discrepancy is due to how the 'pdfsearch' package reads in the PDF.
# However, the page numbering is consistent every time the PDF is read into R.
# Therefore, the location is accurate relative to the intended starting/stopping points 
# for the metric search, even if the absolute page numbers are incorrect.
Find.Section.Pg.Nos(inPath = "Text_Mining/", 
                    outPath = "Text_Mining/TextMining_Outputs/", 
                    Batch = 1)

# Run metric search function, specifying location of csv file that was output during the previous function
# This will output a file listing the "metric hits" from each paper ("PDFs_Metric_Hits.csv"), 
# and move papers with "hits" into a separate folder called "PDFs_Genetic_Diversity_Hits"
Metric.Search(inPath = "Text_Mining/",
              outPath = "Text_Mining/TextMining_Outputs/", 
              inFile_PgNos = "Text_Mining/TextMining_Outputs/StartStop_Page_Search.csv", 
              Batch = 1)
