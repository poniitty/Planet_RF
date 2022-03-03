library(tidyverse)
library(randomForest)

data_path <- "extracted/"   # path to the data
files <- dir(data_path, pattern = "*.csv") # get file names

d <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>% 
  map(~ mutate(., across(X1:july_nir, as.numeric))) %>% 
  reduce(bind_rows)

summary(d)

d <- d %>% filter(X1 != 6,
                  X1 != 0) %>% 
  mutate(X1 = factor(X1)) %>% 
  mutate(clouds = factor(clouds)) %>% 
  select(-imageid)

layers <- c("blue","green","red","nir")
for(i in layers){
  print(i)
  layers <- layers[-1]
  for(ii in layers){
    d[,paste(i, ii, sep = "_")] <- (d[,i]-d[,ii])/(d[,i]+d[,ii])
  }
}

d %>% 
  as.data.frame() %>% 
  filter(complete.cases(.)) %>% 
  select(-chm_conif) %>% 
  # group_by(class) %>% 
  #sample_n(., count(.) %>% as.data.frame() %>% .$n %>% min()) %>% 
  randomForest(X1 ~ ., data = .,
               ntree = 500, nodesize = 2,
               importance = T) -> mod
mod
importance(mod)
varImpPlot(mod)

saveRDS(mod, "output/RF_model_all.rds")

