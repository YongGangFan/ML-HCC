library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
get_data <- function(){
  origin_data <- read.csv('./data/肝和肝内胆管癌2004-2017.csv',check.names = F)
  
  data <- origin_data %>%  filter(`Primary Site`==220) %>% #liver site code C22.0
    filter(grepl('8170|8171|8172|8173|8174|8175',`ICD-O-3 Hist/behav`)) %>% #(ICD-O-3) histology codes 8170/3 and 8172/3–8175/3,
    filter(`RX Summ--Surg Prim Site (1998+)` %in%  c(20:25,30,36,37,50,51,52,60,61)) %>% #hepatic resection,lobectomy,Hepatectomy with transplant
    filter(`Derived AJCC Stage Group, 6th ed (2004-2015)` %in%  c('I','II')) %>%
    select(where(~ n_distinct(.) > 1)) %>% # remove features that have only a unique value
    mutate(`Age recode with single ages and 100+`=gsub('years','',`Age recode with single ages and 100+`))%>%
    filter(`Age recode with single ages and 100+`>=18) %>% #excluded if below the age of 18 years
    filter(`CS tumor size (2004-2015)`<=989 && `CS tumor size (2004-2015)`>0) %>%
    filter(grepl('Ishak',`Fibrosis Score Recode (2010+)`))%>%
    filter(!grepl('not|Not|undetermined',`AFP Pretreatment Interpretation Recode (2010+)`))%>%
    filter(`Grade (thru 2017)`!='Unknown')%>%
    filter(`Race recode (W, B, AI, API)`!='Unknown')%>%
    filter(`Marital status at diagnosis`!='Unknown') %>%
    filter(!grepl('Blank',`Derived AJCC Stage Group, 7th ed (2010-2015)`)) %>% #%>% write.csv('afterfliter.csv')
    subset(select=c(`Year of diagnosis`,`Sex`,`Age recode with single ages and 100+`,`Race recode (White, Black, Other)`,
                    `Marital status at diagnosis`,`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`, `ICD-O-3 Hist/behav`,
                    `Grade (thru 2017)`,`SEER historic stage A (1973-2015)`,`Derived AJCC Stage Group, 7th ed (2010-2015)`,
                    `RX Summ--Surg Prim Site (1998+)`,
                    `RX Summ--Surg/Rad Seq`,`Chemotherapy recode (yes, no/unk)`,#`RX Summ--Systemic/Sur Seq`,
                    `AFP Pretreatment Interpretation Recode (2010+)`,`Fibrosis Score Recode (2010+)`,
                    `CS tumor size (2004-2015)`,`Total number of in situ/malignant tumors for patient`,`Survival months`,
                    `Vital status recode (study cutoff used)`
    )) %>%
    setNames(c('Year','Sex','Age','Race','Marital status','Ethnicity','Histologic type','Grade','Seer stage','AJCC stage','Surgery','Radiotherapy','Chemotherapy',
               'AFP','Fibrosis score','Tumor size','Tumor number','Survival months','Status'#'Systemictherapy',
    )) %>%
    mutate(
      Year = as.factor(Year),
      Sex = factor(Sex,levels = c('Female', 'Male')),
      Age = as.numeric(Age),
      Race = Race %>% factor(levels = c('White', 'Black', 'Other (American Indian/AK Native, Asian/Pacific Islander)'),
                             labels = c('White', 'Black', 'Other')),
      `Marital status` = grepl('^Married',`Marital status`) %>% factor(labels = c('Not married','Married')),
      Ethnicity = grepl('^Hispan',Ethnicity) %>% factor(labels = c('Non-Hispanic','Hispanic')),
      `Histologic type` = `Histologic type` %>% factor(
        levels = c("8170/3: Hepatocellular carcinoma, NOS", "8171/3: Hepatocellular carcinoma, fibrolamellar",
                   "8172/3: Hepatocellular carcinoma, scirrhous","8174/3: Hepatocellular carcinoma, clear cell type", 
                   "8175/3: Hepatocellular carcinoma, pleomorphic type"
                   ),
        labels=
          c("8170, NOS","8171, fibrolamellar",
            "8172, scirrhous","8174, clear cell type", 
            "8175, pleomorphic type"
          ),
          ),
      Grade = factor(Grade,levels = c("Well differentiated; Grade I","Moderately differentiated; Grade II",  
                                      "Poorly differentiated; Grade III", "Undifferentiated; anaplastic; Grade IV"),
                     labels = c("Well differentiated","Moderately differentiated",  
                                "Poorly differentiated", "Undifferentiated")
      ),
      `Seer stage` = `Seer stage` %>% as.factor,
      `AJCC stage` = `AJCC stage` %>% as.factor,
      Surgery = grepl('^61',Surgery) %>% factor(labels = c('Hepatic resection/lobectomy','Hepatectomy with transplant')),
      Radiotherapy = grepl('^No',Radiotherapy) %>% `!` %>% factor(labels = c('No','Yes')),
      Chemotherapy = grepl('^Yes',Chemotherapy) %>% factor(labels = c('No','Yes')),
      # Systemictherapy = grepl('^No',Systemictherapy) %>% `!` %>% factor(labels = c('No','Yes')),
      AFP = grepl('Positive/elevated',AFP) %>% factor(labels = c('Negative or normal','Positive or elevated')),
      # AFP = AFP %>% factor(
      #   levels = c("Negative/normal; within normal limits", "Borderline; undetermined if positive or negative", "Positive/elevated",
      #              "Not documented; Not assessed or unknown if assessed", "Test ordered, results not in chart", "Blank(s)"),
      #   labels = c("Negative", "Borderline", "Positive", "Unknown", "Unknown","Unknown"),
      # ),
      `Fibrosis score` = `Fibrosis score` %>% factor(
        levels = c("Ishak 0-4; No to moderate fibrosis; METAVIR F0-F3; Batt-Ludwig 0-3",
                   "Ishak 5-6; Advanced/severe fibrosis; METAVIR F4; Batt-Ludwig 4; Cirrhosis"),
        labels = c('Ishak 0-4','Ishak 5-6')
      ),
      # `Fibrosis score` = `Fibrosis score` %>% factor(
      #   levels = c("Ishak 0-4; No to moderate fibrosis; METAVIR F0-F3; Batt-Ludwig 0-3",
      #              "Ishak 5-6; Advanced/severe fibrosis; METAVIR F4; Batt-Ludwig 4; Cirrhosis", 
      #              "Blank(s)", 
      #              "Unknown; MR statement w/o hist conf; Uncategorized", 
      #              "Not applicable: Information not collected for this case"),
      #   labels = c('Ishak 0-4','Ishak 5-6','Unknown','Unknown','Unknown')
      # ),
      `Tumor size` = `Tumor size` %>% as.numeric() %>% cut(breaks=c(-Inf,30,50,Inf),labels=c('<3cm','>=3 and < 5 cm','>=5 cm'),right=F),
      `Survival months` = `Survival months` %>% as.numeric,
      Status = Status %>% factor(levels = c('Alive','Dead'))
    )
  plot_distribution <- function(data_){
    plot_list <- list()
    for (colname in colnames(data_)){
      print(colname)
      p <- ggplot(data_,aes(x=.data[[colname]])) + geom_bar() + 
        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
        theme(axis.text.x = element_text(angle = 40,hjust=1,size = 5))
      plot_list <- append(plot_list,list(p))
    }
    grid.arrange(grobs = plot_list, ncol = 4)
  }
  # data %>% plot_distribution()
  # write.csv(data,'data1.csv')
  data
}
