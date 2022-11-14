load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())

beera <- function(expr){
  tryCatch(expr,
           error = function(e){
             message("An error occurred:\n", e)
           },
           warning = function(w){
             message("A warning occured:\n", w)
           },
           finally = {
             message("Finally done!")
           })
}

read_in <- function(eleresi_ut, sheet, new_filename){
  death <- read_excel(eleresi_ut,sheet = sheet)
  value <- death[1,2]
  index <- which(death[,1] == "TIME")
  col_names <- death[index,]
  death <- death %>% select(which(!is.na(col_names)))
  colnames(death) <- col_names[which(!is.na(col_names))]
  colnames(death)[1] <- "NUTS_ID"
  for (i in 2:length(colnames(death))){
    colnames(death)[i] <- paste0(new_filename,"_", colnames(death)[i])
  }
  death <- death[(index + 2):nrow(death),]
  return(list(value, death))
}

files <- list.files("d:/Users/Fanni/Downloads/excel/")
for (filename in files){
  for (j in 1:20){
    new_filename <- strsplit(filename, "defaultview")[[1]]
    if (length(new_filename) > 1) {new_filename <- new_filename[1]} else {new_filename <- strsplit(new_filename, split = "_")[[1]][1]}
    new_filename <- paste0(new_filename, "_", j)
    beera({assign(new_filename, read_in(paste0("d:/Users/Fanni/Downloads/excel/", filename), paste0("Sheet ", j), new_filename)[[2]])})
    beera({assign(paste0(new_filename, "_2"), read_in(paste0("d:/Users/Fanni/Downloads/excel/", filename), paste0("Sheet ", j), new_filename)[[1]])})
  }}
rm(files, filename, new_filename, j, beera, read_in)

file_list <- ls()
file_list

save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/eurostat_data.RData")

#-------------------------------------------------------------------------------
load("d:/Users/Fanni/Desktop/Területi autokorreláció/eurostat_data.RData")
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_1.RData")
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_2.RData")
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_3.RData")

for (i in 1:length(file_list)){
  if (i %% 2 == 0) next
  var <- get(file_list[i])
  proba <- merge(var, NUTS_1, by = "NUTS_ID")
  if (nrow(proba) != 0)NUTS_1 <- merge(var, NUTS_1, by = "NUTS_ID", all.y = TRUE)
  proba <- merge(var, NUTS_2, by = "NUTS_ID")
  if (nrow(proba) != 0)NUTS_2 <- merge(var, NUTS_2, by = "NUTS_ID", all.y = TRUE)
  proba <- merge(var, NUTS_3, by = "NUTS_ID")
  if (nrow(proba) != 0)NUTS_3 <- merge(var, NUTS_3, by = "NUTS_ID", all.y = TRUE)
}

var_list <- vector()
value <- vector()
for (i in 1:length(file_list)){
  if (i %% 2 == 1) next
  var <- get(file_list[i])
  var_list[i / 2] <- file_list[i]
  value[i / 2] <- var[1,1][[1]]
}

leiras <- data.frame(variable = var_list, value = value)
leiras$code <- vector(length = length(leiras$value))
for(i in 1:length(leiras$value)){
  seged <- strsplit(leiras$value[i], split = " ")[[1]]  
  leiras$code[i] <- seged[length(seged)]
}

write_xlsx(leiras, "d:/Users/Fanni/Desktop/Területi autokorreláció/leiras.xlsx")

code_list <- unique(leiras$code)
code <- c()
for (i in code_list){
  code <- paste(code, i, sep = ",")  
}

code

rm(list=ls()[! ls() %in% c("NUTS_1","NUTS_2", "NUTS_3", "leiras")])

NUTS_1_n <- NUTS_1
NUTS_2_n <- NUTS_2
NUTS_3_n <- NUTS_3
rm(NUTS_1, NUTS_2, NUTS_3)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/eurostat_data_2.RData")

#-------------------------------------------------------------------------------
load("d:/Users/Fanni/Desktop/Területi autokorreláció/data_shp.RData")
LoadLibrary()
rm(list = ls())
load("d:/Users/Fanni/Desktop/Területi autokorreláció/eurostat_data_2.RData")
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_1.RData")
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_2.RData")
load("d:/Users/Fanni/Desktop/Területi autokorreláció/matrix_nuts_3.RData")
NUTS_1_eredeti <- colnames(NUTS_1)
NUTS_2_eredeti <- colnames(NUTS_2)
NUTS_3_eredeti <- colnames(NUTS_3)
rm(NUTS_1, NUTS_2, NUTS_3)

NUTS_1_n <- NUTS_1_n %>% select(-c(`demo_r_find3$_6_2016`, `demo_r_find3$_6_2017`, 
                                   `demo_r_find3$_6_2018`, `demo_r_find3$_6_2019`,
                                   `demo_r_find3$_3_2016`, `demo_r_find3$_3_2017`,
                                   `demo_r_find3$_3_2018`, `demo_r_find3$_3_2019`,
                                   `demo_r_find3$_1_2016`, `demo_r_find3$_1_2017`,
                                   `demo_r_find3$_1_2018`, `demo_r_find3$_1_2019`,
                                   `cens_11ms_r3$_1_2011`, `cens_11dwob_r3$_1_2011`,
                                   `cens_11ag_r3$_3_2011`, `cens_11ag_r3$_2_2011`,
                                   `cens_11ag_r3$_1_2011`
                                   ))

p_value <- vector()
observed <- vector()
expected <- vector()
sd <- vector()
matrix_l <- vector()
col_sd <- vector()
col_range <- vector()
col_skew <- vector()
col_kurt <- vector()
variable <- vector()
col_mean <- vector()
col_na <- vector()
count <- 0

for (i in 1:ncol(NUTS_1_n)){
  if (colnames(NUTS_1_n)[i] %in% NUTS_1_eredeti) next
  print(colnames(NUTS_1_n)[i])
  NUTS_1_n[which(NUTS_1_n[,i] == ":"),i] <- NA
  NUTS_1_n[,i] <- as.numeric(NUTS_1_n[,i])
  
  for (matrix in c("elso_szomszed_1", "kozut_elsodleges_km_1", "kozut_elsodleges_ora_1",
                   "kozut_masodlagos_km_1", "kozut_masodlagos_ora_1", "legkozelebbi_negy_1",
                   "masod_szomszed_1", "min_tav_1", "regio_kozep_tav_1")){
    count <- count + 1
    moran <- Moran.I(NUTS_1_n[,i], get(matrix), na.rm = TRUE)
    p_value[count] <- moran$p.value
    observed[count] <- moran$observed
    expected[count] <- moran$expected
    sd[count] <- moran$sd
    matrix_l[count] <- matrix
    col_sd[count] <- sd(NUTS_1_n[,i], na.rm = TRUE)
    range_v <- range(NUTS_1_n[,i], na.rm = TRUE)
    col_range[count] <- range_v[2] - range_v[1]
    col_skew[count] <- skewness(NUTS_1_n[,i], na.rm = TRUE)
    col_kurt[count] <- kurtosis(NUTS_1_n[,i], na.rm = TRUE)
    variable[count] <- colnames(NUTS_1_n)[i]
    col_mean[count] <- mean(NUTS_1_n[,i], na.rm = TRUE)
    col_na[count] <- length(which(is.na(NUTS_1_n[,i])))
  }
}

moran_1 <- data.frame(p_value = p_value, 
                      observed = observed, 
                      expected = expected,
                      sd = sd,
                      matrix = matrix_l, 
                      col_sd = col_sd,
                      col_range = col_range,
                      col_skew = col_skew,
                      col_kurt = col_kurt,
                      col_mean = col_mean,
                      col_na = col_na,
                      variable = variable)

moran_1$test <- ifelse(moran_1$p_value > 0.05, 0, ifelse(moran_1$observed > moran_1$expected, 1, -1))
moran_1 <- moran_1 %>% filter(col_na < 25)

proba <- moran_1 %>% filter(test == 1)
table(proba$matrix, proba$variable)

proba <- moran_1 %>% filter(test == -1)
table(proba$matrix, proba$variable)

#-------------------------------------------------------------------------------
NUTS_2_n <- NUTS_2_n %>% select(-c(tgs00111_3_2020, tgs00111_2_2020, tgs00111_1_2020,
                                   tgs00037_1_2015, tgs00010_18_2009, tgs00010_18_2010,
                                   tgs00010_18_2011, tgs00010_18_2012, tgs00010_18_2013,
                                   tgs00010_18_2014, tgs00010_18_2015, tgs00010_18_2016,
                                   tgs00010_18_2017, tgs00010_18_2018, tgs00010_18_2019,
                                   tgs00010_18_2020, tgs00010_17_2009,tgs00010_17_2010,
                                   tgs00010_17_2011, tgs00010_17_2012, tgs00010_17_2013,
                                   tgs00010_17_2014, tgs00010_17_2015, tgs00010_17_2016,
                                   tgs00010_17_2017, tgs00010_17_2018, tgs00010_17_2019,
                                   tgs00010_17_2020, tgs00010_16_2009,tgs00010_16_2010,
                                   tgs00010_16_2011, tgs00010_16_2012, tgs00010_16_2013,
                                   tgs00010_16_2014, tgs00010_16_2015, tgs00010_16_2016,
                                   tgs00010_16_2017, tgs00010_16_2018, tgs00010_16_2019,
                                   tgs00010_16_2020, tgs00010_15_2009,tgs00010_15_2010,
                                   tgs00010_15_2011, tgs00010_15_2012, tgs00010_15_2013,
                                   tgs00010_15_2014, tgs00010_15_2015, tgs00010_15_2016,
                                   tgs00010_15_2017, tgs00010_15_2018, tgs00010_15_2019,
                                   tgs00010_15_2020,tgs00010_14_2009,tgs00010_14_2010,
                                   tgs00010_14_2011, tgs00010_14_2012, tgs00010_14_2013,
                                   tgs00010_14_2014, tgs00010_14_2015, tgs00010_14_2016,
                                   tgs00010_14_2017, tgs00010_14_2018, tgs00010_14_2019,
                                   tgs00010_14_2020, tgs00010_13_2009,tgs00010_13_2010,
                                   tgs00010_13_2011, tgs00010_13_2012, tgs00010_13_2013,
                                   tgs00010_13_2014, tgs00010_13_2015, tgs00010_13_2016,
                                   tgs00010_13_2017, tgs00010_13_2018, tgs00010_13_2019,
                                   tgs00010_13_2020, `demo_r_find3$_6_2016`, `demo_r_find3$_6_2017`, 
                                   `demo_r_find3$_6_2018`, `demo_r_find3$_6_2019`,
                                   `demo_r_find3$_3_2016`, `demo_r_find3$_3_2017`,
                                   `demo_r_find3$_3_2018`, `demo_r_find3$_3_2019`,
                                   `demo_r_find3$_1_2016`, `demo_r_find3$_1_2017`,
                                   `demo_r_find3$_1_2018`, `demo_r_find3$_1_2019`,
                                   `cens_11ms_r3$_1_2011`, `cens_11dwob_r3$_1_2011`,
                                   `cens_11ag_r3$_3_2011`, `cens_11ag_r3$_2_2011`,
                                   `cens_11ag_r3$_1_2011`))
NUTS_2_n <- NUTS_2_n %>% select(-c())

p_value <- vector()
observed <- vector()
expected <- vector()
sd <- vector()
matrix_l <- vector()
col_sd <- vector()
col_range <- vector()
col_skew <- vector()
col_kurt <- vector()
variable <- vector()
col_mean <- vector()
col_na <- vector()
count <- 0

for (i in 1:ncol(NUTS_2_n)){
  if (colnames(NUTS_2_n)[i] %in% NUTS_2_eredeti) next
  print(colnames(NUTS_2_n)[i])
  NUTS_2_n[which(NUTS_2_n[,i] == ":"),i] <- NA
  NUTS_2_n[,i] <- as.numeric(NUTS_2_n[,i])
  
  for (matrix in c("elso_szomszed_2", "kozut_elsodleges_km_2", "kozut_elsodleges_ora_2",
                   "kozut_masodlagos_km_2", "kozut_masodlagos_ora_2", "legkozelebbi_negy_2",
                   "masod_szomszed_2", "min_tav_2", "regio_kozep_tav_2")){
    count <- count + 1
    moran <- Moran.I(NUTS_2_n[,i], get(matrix), na.rm = TRUE)
    p_value[count] <- moran$p.value
    observed[count] <- moran$observed
    expected[count] <- moran$expected
    sd[count] <- moran$sd
    matrix_l[count] <- matrix
    col_sd[count] <- sd(NUTS_2_n[,i], na.rm = TRUE)
    range_v <- range(NUTS_2_n[,i], na.rm = TRUE)
    col_range[count] <- range_v[2] - range_v[1]
    col_skew[count] <- skewness(NUTS_2_n[,i], na.rm = TRUE)
    col_kurt[count] <- kurtosis(NUTS_2_n[,i], na.rm = TRUE)
    variable[count] <- colnames(NUTS_2_n)[i]
    col_mean[count] <- mean(NUTS_2_n[,i], na.rm = TRUE)
    col_na[count] <- length(which(is.na(NUTS_2_n[,i])))
  }
}

moran_2 <- data.frame(p_value = p_value, 
                      observed = observed, 
                      expected = expected,
                      sd = sd,
                      matrix = matrix_l, 
                      col_sd = col_sd,
                      col_range = col_range,
                      col_skew = col_skew,
                      col_kurt = col_kurt,
                      col_mean = col_mean,
                      col_na = col_na,
                      variable = variable)

moran_2$test <- ifelse(moran_2$p_value > 0.05, 0, ifelse(moran_2$observed > moran_2$expected, 1, -1))
moran_2 <- moran_2 %>% filter(!is.na(col_skew))
moran_2 <- moran_2 %>% filter(col_na < 70)

proba <- moran_2 %>% filter(test == 1)
proba <- table(proba$matrix, proba$variable)
colsum <- apply(proba, 2, sum)
hist(colsum)
length(which(colsum == 1))
rowsum <- apply(proba[, which(colsum == 1)], 1, sum)
rowsum

proba <- moran_2 %>% filter(test == -1)
table(proba$matrix, proba$variable)


moran_2 %>% filter(test != -1) %>% ggplot(aes(x = col_skew, 
                                              color = as.factor(test), 
                                              fill = as.factor(test)))+
  geom_histogram(aes(y=..density..), position="dodge")+
  geom_density(alpha=0.6)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

moran_2 %>% filter(test != -1) %>% ggplot(aes(x = col_kurt, 
                                              color = as.factor(test), 
                                              fill = as.factor(test)))+
  geom_histogram(aes(y=..density..), position="dodge")+
  geom_density(alpha=0.6)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

moran_2 %>% filter(test != -1 & col_sd < 100) %>% ggplot(aes(x = col_range, 
                                                               color = as.factor(test), 
                                                               fill = as.factor(test)))+
  geom_histogram(aes(y=..density..), position="dodge")+
  geom_density(alpha=0.6)+
  facet_wrap(~matrix) +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

#-------------------------------------------------------------------------------
NUTS_3_n <- NUTS_3_n %>% select(-c(`demo_r_find3$_6_2016`, `demo_r_find3$_6_2017`, 
                                   `demo_r_find3$_6_2018`, `demo_r_find3$_6_2019`,
                                   `demo_r_find3$_3_2016`, `demo_r_find3$_3_2017`,
                                   `demo_r_find3$_3_2018`, `demo_r_find3$_3_2019`,
                                   `demo_r_find3$_1_2016`, `demo_r_find3$_1_2017`,
                                   `demo_r_find3$_1_2018`, `demo_r_find3$_1_2019`,
                                   `cens_11ms_r3$_1_2011`, `cens_11dwob_r3$_1_2011`,
                                   `cens_11ag_r3$_3_2011`, `cens_11ag_r3$_2_2011`,
                                   `cens_11ag_r3$_1_2011`))

p_value <- vector()
observed <- vector()
expected <- vector()
sd <- vector()
matrix_l <- vector()
col_sd <- vector()
col_range <- vector()
col_skew <- vector()
col_kurt <- vector()
variable <- vector()
col_mean <- vector()
col_na <- vector()
count <- 0

for (i in 1:ncol(NUTS_3_n)){
  if (colnames(NUTS_3_n)[i] %in% NUTS_3_eredeti) next
  print(colnames(NUTS_3_n)[i])
  NUTS_3_n[which(NUTS_3_n[,i] == ":"),i] <- NA
  NUTS_3_n[,i] <- as.numeric(NUTS_3_n[,i])
  
  for (matrix in c("elso_szomszed_3", "kozut_elsodleges_km_3", "kozut_elsodleges_ora_3",
                   "kozut_masodlagos_km_3", "kozut_masodlagos_ora_3", "legkozelebbi_negy_3",
                   "masod_szomszed_3", "min_tav_3", "regio_kozep_tav_3")){
    count <- count + 1
    moran <- Moran.I(NUTS_3_n[,i], get(matrix), na.rm = TRUE)
    p_value[count] <- moran$p.value
    observed[count] <- moran$observed
    expected[count] <- moran$expected
    sd[count] <- moran$sd
    matrix_l[count] <- matrix
    col_sd[count] <- sd(NUTS_3_n[,i], na.rm = TRUE)
    range_v <- range(NUTS_3_n[,i], na.rm = TRUE)
    col_range[count] <- range_v[2] - range_v[1]
    col_skew[count] <- skewness(NUTS_3_n[,i], na.rm = TRUE)
    col_kurt[count] <- kurtosis(NUTS_3_n[,i], na.rm = TRUE)
    variable[count] <- colnames(NUTS_3_n)[i]
    col_mean[count] <- mean(NUTS_3_n[,i], na.rm = TRUE)
    col_na[count] <- length(which(is.na(NUTS_3_n[,i])))
  }
}

moran_3 <- data.frame(p_value = p_value, 
                      observed = observed, 
                      expected = expected,
                      sd = sd,
                      matrix = matrix_l, 
                      col_sd = col_sd,
                      col_range = col_range,
                      col_skew = col_skew,
                      col_kurt = col_kurt,
                      col_mean = col_mean,
                      col_na = col_na,
                      variable = variable)

moran_3$test <- ifelse(moran_3$p_value > 0.05, 0, ifelse(moran_3$observed > moran_3$expected, 1, -1))
moran_3 <- moran_3 %>% filter(!is.na(col_skew))
moran_3 <- moran_3 %>% filter(col_na < 338)

proba <- moran_3 %>% filter(test == 1)
proba <- table(proba$matrix, proba$variable)
colsum <- apply(proba, 2, sum)
hist(colsum)
length(which(colsum == 1))
proba[,which(colsum == 1)]

proba <- moran_3 %>% group_by(variable) %>% summarize(
  min = min(test),
  max = max(test)
)

proba <- proba %>% filter(min == -1 & max == 1)

NUTS_1_n %>% ggplot() + ggtitle("Szimuláció bemutatása NUTS 1 szinten\nRégióközép távolság mátrix\n") + 
  geom_sf(aes(fill = `tgs00106_1_2009`, geometry = geometry))  +
  theme(plot.title = element_text(color="black", size=18, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=10, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=10, face = "bold"),                
        axis.ticks.x=element_blank(),
        text = element_text(family = "serif"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "serif"),
        legend.background = element_rect( size=.5))

proba <- moran_3 %>% filter(test == -1)
table(proba$matrix, proba$variable)


moran_3 %>% filter(test != -1) %>% ggplot(aes(x = col_skew, 
                                              color = as.factor(test), 
                                              fill = as.factor(test)))+
  geom_histogram(aes(y=..density..), position="dodge")+
  geom_density(alpha=0.6)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

moran_3 %>% filter(test != -1) %>% ggplot(aes(x = col_kurt, 
                                              color = as.factor(test), 
                                              fill = as.factor(test)))+
  geom_histogram(aes(y=..density..), position="dodge")+
  geom_density(alpha=0.6)+
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

moran_3 %>% filter(test != -1 & col_sd < 100) %>% ggplot(aes(x = col_range, 
                                                             color = as.factor(test), 
                                                             fill = as.factor(test)))+
  geom_histogram(aes(y=..density..), position="dodge")+
  geom_density(alpha=0.6)+
  facet_wrap(~matrix) +
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2")

rm(col_kurt, col_range, col_sd, col_skew, count, expected, i, matrix, matrix_l, observed, p_value, range_v, sd, variable, col_mean, col_na)
save.image("d:/Users/Fanni/Desktop/Területi autokorreláció/eurostat_data_3.RData")

#-------------------------------------------------------------------------------
load("d:/Users/Fanni/Desktop/Területi autokorreláció/eurostat_data_3.RData")

proba <- moran_2 %>% group_by(variable)%>% summarise(
  test = sum(test),
  sd = mean(col_sd), #ugyanaz az érték mind a 9 esetben
  skew = mean(col_skew),  #ugyanaz az érték mind a 9 esetben
  kurt = mean(col_kurt),  #ugyanaz az érték mind a 9 esetben
  range = mean(col_range),  #ugyanaz az érték mind a 9 esetben
  mean = mean(col_mean),
  na = mean(col_na),
  observed_r = diff(range(observed)),
  p_value_r = diff(range(p_value))
)

moran <- rbind(moran_1, moran_2, moran_3)
moran$level <- c(rep(1, nrow(moran_1)),
                 rep(2, nrow(moran_2)),
                 rep(3, nrow(moran_3)))

matrix_l <- vector()
for (i in 1:nrow(moran)){
  matrix <- moran$matrix[i]
  seged <- strsplit(matrix, split ="_")[[1]]
  matrix <- seged[1]
  for (j in 2:(length(seged)-1)){
    matrix <- paste(matrix, seged[j], sep = "_")
  }
  matrix_l[i] <- matrix
}
moran$matrix <- matrix_l

var_list <- vector()
level_list <- vector()
egy_l <- vector()
ketto_l <- vector()
harom_l <- vector()
negy_l <- vector()
ot_l <- vector()
hat_l <- vector()
het_l <- vector()
nyolc_l <- vector()
kilenc_l <- vector()

count <- 0

for (var in unique(moran$variable)){
  for (l in 1:3){
    seged <- moran %>% filter(variable == var & level == l)
    if (nrow(seged) == 0) next
    count <- count + 1
    var_list[count] <- var
    level_list[count] <- l
    
    egy_l[count] <- seged$test[which(seged$matrix == "elso_szomszed")]
    ketto_l[count] <- seged$test[which(seged$matrix == "masod_szomszed")]
    harom_l[count] <- seged$test[which(seged$matrix == "kozut_elsodleges_km")]
    negy_l[count] <- seged$test[which(seged$matrix == "kozut_elsodleges_ora")]
    ot_l[count] <- seged$test[which(seged$matrix == "kozut_masodlagos_km")]
    hat_l[count] <- seged$test[which(seged$matrix == "kozut_masodlagos_ora")]
    het_l[count] <- seged$test[which(seged$matrix == "regio_kozep_tav")]
    nyolc_l[count] <- seged$test[which(seged$matrix == "min_tav")]
    kilenc_l[count] <- seged$test[which(seged$matrix == "legkozelebbi_negy")]
  }
}

proba <- data.frame(var_list = var_list,
                    level_list = level_list, 
                    egy_l = egy_l, 
                    ketto_l = ketto_l, 
                    harom_l  = harom_l,
                    negy_l  = negy_l,
                    ot_l = ot_l,
                    hat_l = hat_l,
                    het_l = het_l,
                    nyolc_l = nyolc_l,
                    kilenc_l = kilenc_l
)



for (i in 3:11){
  for (j in 3:11){
    if (i >= j) next
    seged <- table(proba[,i], proba[,j])
    if (sum(diag(seged)) / sum(seged) > 0.6) next
    print(seged)
    print(paste(as.character(i), as.character(j)))
  }
}

for (i in 3:11){
  print(table(proba[,i]))
}

moran$group <- paste(as.character(moran$variable),  as.character(moran$level), sep ="-")

var_way <- moran %>% group_by(group) %>% summarize(
  test = sum(test),
  sd = mean(col_sd), #ugyanaz az érték mind a 9 esetben
  skew = mean(col_skew),  #ugyanaz az érték mind a 9 esetben
  kurt = mean(col_kurt),  #ugyanaz az érték mind a 9 esetben
  range = mean(col_range),  #ugyanaz az érték mind a 9 esetben
  mean = mean(col_mean),
  na = mean(col_na),
  observed_r = diff(range(observed)),
  p_value_r = diff(range(p_value)),
  level = mean(level)
)

var_way$rel_range <- var_way$range / var_way$mean
var_way$rel_range <- abs(var_way$rel_range)
var_way$rel_sd <- abs(var_way$sd / var_way$mean)
var_way$test <- ifelse(var_way$test == 9, 2, ifelse(var_way$test >= 1, 1, ifelse(var_way$test == 0, 0, -1)))
var_way$test <- ifelse(var_way$test == -1, 1, var_way$test)
var_way <- var_way %>% filter(rel_sd < 10 & 
                          kurt < 50 &
                          rel_range < 200)
var_way %>%  ggplot(aes(x = rel_sd, y = kurt, color = as.factor(test))) + 
  geom_point()

var_way %>% ggplot(aes(x = kurt, fill = as.factor(test), color = as.factor(test))) + 
  geom_histogram(aes(y=..density..), position="dodge")

library(ggpubr)
library(broom)
library(AICcmodavg)
library("lawstat")

#-------------------------------------------------------------------------------

table(var_way$test)

levene.test(var_way$skew, as.factor(var_way$test), "mean") # itt mégis mi a H0?
levene.test(var_way$kurt, as.factor(var_way$test), "mean")
levene.test(var_way$rel_range, as.factor(var_way$test), "mean")
levene.test(var_way$rel_sd, as.factor(var_way$test), "mean")

one.way <- aov(skew ~ test, data = var_way)
summary(one.way)

one.way <- aov(kurt ~ test, data = var_way)
summary(one.way)

one.way <- aov(rel_sd ~ test, data = var_way)
summary(one.way)

one.way <- aov(rel_range ~ test, data = var_way)
summary(one.way)

var_way %>% ggplot(aes(x = skew, y = as.factor(test))) + geom_boxplot()
var_way %>% ggplot(aes(x = kurt, y = as.factor(test))) + geom_boxplot()
var_way %>% ggplot(aes(x = rel_range, y = as.factor(test))) + geom_boxplot()
var_way %>% ggplot(aes(x = rel_sd, y = as.factor(test))) + geom_boxplot()

# 1-2 összevonása 
var_way$test <- ifelse(var_way$test == 2, 2, 1)

table(var_way$test)

levene.test(var_way$skew, as.factor(var_way$test), "mean") #így már egyezik a variancia!
levene.test(var_way$kurt, as.factor(var_way$test), "mean")
levene.test(var_way$rel_range, as.factor(var_way$test), "mean")
levene.test(var_way$rel_sd, as.factor(var_way$test), "mean")

one.way <- aov(skew ~ test, data = var_way)
summary(one.way)

one.way <- aov(kurt ~ test, data = var_way)
summary(one.way)

var_way %>% ggplot(aes(x = kurt, y = as.factor(test),  color = as.factor(test))) + 
  geom_boxplot() +
  geom_vline(xintercept = 1.45, linetype = "longdash", size = 1.2) +
  scale_color_brewer(palette="Dark2")  + theme_minimal() +
  theme(plot.title = element_text(color="black", size=22, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),
        axis.title.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.title.x = element_text(color="black", size=16, face = "bold"),
        text = element_text(family = "serif"),
        legend.position = "none") +
  labs(title="Változók csúcsossági értéke\naz egyes csoportokban\n",
       x="\nCsúcsosság",
       y = "Csoport\nEredmények egyezésének foka\n")

var_way %>% ggplot(aes(x = skew, y = as.factor(test),  color = as.factor(test))) + 
  geom_boxplot() +
  geom_vline(xintercept = 0.28, linetype = "longdash", size = 1.2) +
  scale_color_brewer(palette="Dark2")  + theme_minimal() +
  theme(plot.title = element_text(color="black", size=22, hjust=0.5, face = "bold"),
        axis.text.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.text.x = element_text(color="black", size=16, face = "bold"),
        axis.title.y = element_text(color="black", size=16, hjust=0.5, face = "bold"),
        axis.title.x = element_text(color="black", size=16, face = "bold"),
        text = element_text(family = "serif"),
        legend.position = "none") +
  labs(title="Változók ferdeségi értéke\naz egyes csoportokban\n",
       x="\nFerdeség",
       y = "Csoport\nEredmények egyezésének foka\n")
