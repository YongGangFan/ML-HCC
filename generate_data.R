library(survival)
library(magrittr)
library(gtsummary)
library(forestplot)
library(data.table)
library(flextable)
#### Create dirs####
dirs <- c('data','Figures','Tables')
for(dir in dirs){
  path_ <- dir
  print(path_)
  if(!file.exists(path_)){
    dir.create(path_)
  }
}

source('preprocessing.R')
data <- get_data() %>% data.frame(check.names = T)
data %<>% .[.$Survival.months>0,]
##### Lasso ####
library(glmnet)
x = data %>% subset(select=-c(Year,Survival.months,Status)) %>%{mutate_at(.,colnames(.),as.numeric)}%>%as.matrix()
y = cbind(time=data$`Survival.months`,status=data$Status%>% as.numeric() %>% -1) %>% data.matrix()
fit_cv <- cv.glmnet(x,y,family='cox', type.measure="C",nfolds = 4)
plot(fit_cv)
lasso.coef <- coef(fit_cv$glmnet.fit,s=fit_cv$lambda.min,exact = F)
imp.df <- data.frame(features = lasso.coef@Dimnames[[1]][lasso.coef@i+1],
                     coef = lasso.coef@x)
#### Overall distrubution ####
overall <- tbl_summary(data)
##### multi cox ####
data_cox = data %>% subset(select=-c(Year,Histologic.type)) %>% mutate(Status= Status %>% as.numeric() %>% -1) 
fml = as.formula(Surv(Survival.months, Status) ~ .)
multi_cox <- coxph(fml, data = data_cox) %>%
  tbl_regression(exponentiate=T,
                 pvalue_fun = ~style_pvalue(.x, digits = 2),
                 hide_n = T
                 )%>%
  add_global_p() %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()
#### Univariate survival analysis ####
unicox <- data_cox %>% 
  tbl_uvregression(
  method = coxph,
  y = Surv(Survival.months,Status),
  exponentiate = TRUE,
  pvalue_fun = function(x) style_pvalue(x, digits = 2),
  hide_n = T
) %>%
  add_global_p() %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()
#### Merge cox survival analysis ####
tbl_merge(
    tbls = list(overall,unicox, multi_cox),
    tab_spanner = c("","**Unicox**", "**Multicox**")
  ) %>% 
  bold_labels() %>%
  italicize_levels()%>%
  as_flex_table() %>%
  autofit()%>%
  bold(bold = TRUE, part = "header")%>%
  flextable::save_as_docx(path = 'Tables/Merge_Unicox_Multicox.docx')

#### save data ####
data_surv <- data %>% subset(select=c(Age,Marital.status,Grade,AJCC.stage,Chemotherapy,Surgery,AFP,Fibrosis.score,Tumor.size,Tumor.number,Survival.months,Status)) %>%
  mutate_at(.,sapply(.,function(x) length(levels(x))) %>% .[.<5] %>% names,~as.numeric(.)-1) %>%
  data.frame()

write.csv(data_surv,'data/data_surv.csv',row.names = F)


#### Baseline table ####
library(tableone)
library(flextable)
base_data <- data %>% subset(select=-c(Year,Histologic.type))
out_col = 'Surgery'
dput(names(base_data))
my_vars <- base_data %>% colnames %>% setdiff(c(out_col,'type'))
cat_vars <- my_vars %>% sapply(function(x){is.character(levels(base_data[[x]]))}) %>% .[which(.)] %>% names
my_tab <- CreateTableOne(vars = my_vars, strata = out_col, data = base_data, factorVars = cat_vars,addOverall = T)
tabMat <- print(my_tab, quote = FALSE, noSpaces = TRUE, printToggle = T,showAllLevels = TRUE)
tabMat %>% rownames()%>% lapply(function(x){gsub('\\.',' ',x)}) %>% as.character() %>% cbind(tabMat) %>% as.data.frame() %>% subset(select=-c(test))%>%
  flextable() %>%
  # theme_zebra()%>%
  bold(j = '.', bold = TRUE, part = "body")%>%
  bold(j = 'p',i= ~p < 0.05,bold = TRUE, part = "body")%>%
  set_header_labels('.'='','level'='Level','p'='P-value') %>%
  autofit()%>%
  save_as_docx(path=paste('Tables','baseline_group_by_surgery.docx',sep = '/'))


