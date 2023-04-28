suppressPackageStartupMessages(library(WDI))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(countrycode))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(sjPlot))
suppressPackageStartupMessages(library(sjmisc))
suppressPackageStartupMessages(library(sjlabelled))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(vtable))
suppressPackageStartupMessages(library(imager))
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(patchwork))

knitr::include_graphics("Kapitalintensitet.png")

df_gdp0 <- WDI(
  country = "all",
  indicator = c('gdppc'="NY.GDP.PCAP.PP.KD"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_gdp <- subset(df_gdp0, select = c(country, region, income, iso2c, iso3c, year, gdppc) ) %>%  arrange(iso3c, year)
df_gdp <- df_gdp %>% mutate_at(c('iso3c'), ~na_if(., ''))
df_gdp <- df_gdp[complete.cases( df_gdp$gdppc, df_gdp$iso3c),]
df_gdp = df_gdp  %>%  
  mutate(year = as.numeric(year)) 
df_gdp <- df_gdp[!duplicated(df_gdp[c("iso3c", "year", max("gdppc"))]), ]  %>%  arrange(iso3c, year)
df_gdp2000  <- df_gdp %>%  arrange(iso3c, year) %>% group_by(iso3c) %>% 
  slice(1) %>%
  ungroup()
df_gdp2000 = subset(df_gdp2000, select = -c(year) ) 
df_gdp2000 <-   plyr:: rename(df_gdp2000,c("gdppc" = "gdppc0")) 

df_gdp <- left_join(df_gdp,df_gdp2000, by=c("country", "iso2c", "iso3c", "region", "income")) 

df_educ0<-WDI(
  country = "all",
  indicator = c('educ'="BAR.SCHL.15UP"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_educ <- subset(df_educ0, select = c(country, region, income, iso2c, iso3c, year, educ) ) %>%  arrange(iso3c, year) 
df_educ <- df_educ[complete.cases(df_educ$educ),] %>%  arrange(iso3c, year) 

df_educ = df_educ %>%  
  arrange(iso3c, year) %>%  
  mutate(educ = as.numeric(educ, na.rm = TRUE)) %>% 
  ddply("iso3c",transform,
        avg_educ=mean(educ, na.rm = TRUE)) 

df_educ <- subset(df_educ, select = c(country, region, income, iso2c, iso3c, avg_educ))  
df_educ <- df_educ[!duplicated(df_educ[c("iso3c")]), ]  %>%  arrange(iso3c) 

df_nsy0<-WDI(
  country = "all",
  indicator = c( 'nsy'="NY.ADJ.NNAT.GN.ZS"),  
  start = 2000,
  end = 2015,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_nsy <- subset(df_nsy0, select = c(country, region, income, iso2c, iso3c, year, nsy) ) %>%  arrange(iso3c, year) 
df_nsy <- df_nsy[complete.cases(df_nsy$nsy),] %>%  arrange(iso3c, year)

df_lf0<-WDI(
  country = "all",
  indicator = c('lf'="JI.TLF.TOTL"), 
  start = 2000,
  end = 2019,
  extra = TRUE, 
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_lf <- subset(df_lf0, select = c(country, region, income, iso2c, year, lf) ) %>%  arrange(iso2c, year)
df_lf <-   plyr:: rename(df_lf,c("iso2c" = "iso3c")) 
df_lf [df_lf == 0]<-NA
df_lf <- df_lf[complete.cases(df_lf$iso3c, df_lf$lf),] 
df_lf = df_lf  %>%  
  mutate(year = as.numeric(year)) 

df_lf <- df_lf[!duplicated(df_lf[c("iso3c", "year")]), ]  %>%  arrange(iso3c, year) 


df_n = df_lf %>%  
  arrange(iso3c, year) %>% 
  ddply("iso3c",transform,
        t=c(NA,diff(year)),
        lf_growth=c(NA,diff(log(lf)))) 

df_n <- df_n[complete.cases(df_n$t, df_n$lf_growth),] 

df_n = df_n %>%  
  mutate(t = as.numeric(t)) %>%   
  mutate(lf_growth = as.numeric(lf_growth))
df_n <- transform(df_n, n =lf_growth/t)

df_n <- df_n %>% # 
  ddply("iso3c",transform,
        avg_n=mean(n, na.rm = TRUE))

df_n <- subset(df_n, select = c(iso3c, avg_n) )
df_n <- df_n[!duplicated(df_n["iso3c"]), ]  %>%  arrange(iso3c) 
df_nsy = df_nsy %>%  
  arrange(iso3c, year) %>% 
  mutate(nsy = as.numeric(nsy, na.rm = TRUE)) %>%
  ddply("iso3c",transform,
        avg_nsy=mean(nsy, na.rm = TRUE))

df_nsy <- subset(df_nsy, select = c(country, region, income, iso2c, iso3c, avg_nsy)) 
df_nsy <- df_nsy[!duplicated(df_nsy[c("iso3c")]), ]  %>%  arrange(iso3c)

df <- left_join(df_gdp, df_educ, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_nsy, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_n, by="iso3c")
df <- subset(df, select = c(country, region, income, iso2c, iso3c, year, gdppc, gdppc0, avg_educ, avg_nsy, avg_n))


df <- df  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                      & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                      & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                      & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                      & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                      & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                      & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                      & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 

df_rest0<-WDI(
  country = "all",
  indicator = c('poptot'="SP.POP.TOTL", 'gi'="NE.GDI.FTOT.KD.ZG", 'gx'="NE.EXP.GNFS.KD.ZG", 'nry'="NY.ADJ.DRES.GN.ZS", 'p'="SP.POP.GROW" ),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_rest0 <- df_rest0 %>% mutate_at(c('iso3c'), ~na_if(., ''))
df_rest <- df_rest0[complete.cases( df_rest0$iso3c),]  %>%  arrange(iso2c) 


df_rest <- df_rest  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                                & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                                & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                                & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                                & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                                & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                                & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                                & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 

df_rest <- subset(df_rest, select = c("country", "region", "income", "iso3c", "iso2c", "year", "poptot", "p", "nry", "gi", "gx"))
df_all <- left_join(df, df_rest, by=c("country", "region", "income", "iso2c", "iso3c", "year"))

col_order <- c("country",  "region", "income", "iso3c", "iso2c", "year", "gdppc", "gdppc0", "poptot", "p", "avg_n", "avg_nsy", "nry", "gi", "gx", "avg_educ")
df_all <- df_all[, col_order]

df_growth0 = df_all %>%  
  arrange(iso3c, year) %>%
  ddply("iso3c",transform,
        gdpgrowth=c(NA,diff(log(gdppc)))*100) %>%
  mutate(gdpgrowth = as.numeric(gdpgrowth, na.rm = TRUE)) %>% # 
  ddply("iso3c",transform,
        avg_gdpgrowth=mean(gdpgrowth, na.rm = TRUE),
        avg_gi=mean(gi, na.rm = TRUE),
        avg_nry=mean(nry, na.rm = TRUE),
        avg_gx=mean(gx, na.rm = TRUE),
        avg_p=mean(p, na.rm = TRUE))


df_growth0 <-  df_growth0 %>% mutate_at(c('iso3c'), ~na_if(., ''))
df_growth <- df_growth0[complete.cases( df_growth0$country, df_growth0$income, df_growth0$iso3c, df_growth0$avg_gdpgrowth, df_growth0$gdppc0, df_growth0$avg_n, df_growth0$avg_p, df_growth0$avg_nsy, df_growth0$avg_nry,df_growth0$avg_gi, df_growth0$avg_gx, df_growth0$avg_educ),]


df_growth <- subset(df_growth, select = c("country",  "region", "income", "iso3c", "iso2c","year", "poptot", "gdppc", "gdppc0", "avg_gdpgrowth", "avg_n", "avg_p", "avg_nsy", "avg_nry", "avg_gi", "avg_gx", "avg_educ"))

df_growth2019  <- df_growth %>%  arrange(iso3c, year) %>% group_by(iso3c) %>% 
  slice(n()) %>%
  ungroup()

df <- df_growth2019[complete.cases( df_growth2019$avg_gi, df_growth2019$avg_n),]

Q1gi <- quantile(df$avg_gi, .25 )
Q3gi <- quantile(df$avg_gi, .75)
IQRgi <- IQR(df$avg_gi)

Q1n <- quantile(df$avg_n, .25 )
Q3n <- quantile(df$avg_n, .75)
IQRn <- IQR(df$avg_n)

no_outliers <- subset(df, df$avg_gi > (Q1gi - 1.5*IQRgi) & df$avg_gi < (Q3gi + 1.5*IQRgi) &  df$avg_n > (Q1n - 1.5*IQRn) & df$avg_n < (Q3n + 1.5*IQRn))
dim(no_outliers)


no_outliers$dppc <-as.numeric(no_outliers$gdppc)
no_outliers$ln_gdppc<-log(no_outliers$gdppc) 
no_outliers$ln_gdppc0<-log(no_outliers$gdppc0)


kable(head(no_outliers[, 1:8]))

df <- subset(no_outliers, select = c("avg_gdpgrowth", "avg_p", "avg_nsy", "avg_educ", "avg_nry", "avg_gi", "avg_n", "avg_gx", "ln_gdppc"))

labs <- c("Årlig vekstrate i BNP", "Årlig befolkningsvekst", "Netto-sparing (andel av BNI)", "Antall år i skole (befolkning 15+", "Reduksjonsrate i naturressurser", "Vekstrate i investeringer", "Vekstrate i arbeidskraft", "Vekstrate i eksport", "BNP per innbygger (log)") 

st(df, out = 'kable', title = 'Deskriptiv statistikk', labels=labs,
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks')
   ))

plot1 <- ggplot(no_outliers, aes(x = avg_nsy , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Sparing som andel av BNI (Netto)") + 
  ylab("BNP/innbygger 2019") + 
  theme_minimal(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  
  scale_size_area(guide = "none", max_size = 14) +
  theme(legend.text = element_text(size = 5,color="black"),
        legend.title = element_blank(),
        legend.key.height = unit(.3, 'cm'),
        legend.key.width = unit(.3, 'cm'))+ 
  scale_colour_manual(values = rainbow(9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  labs(title = "Figur 1: BNP/innbygger og sparing") + 
  scale_y_continuous(trans = 'log2', labels = dollar, breaks=c(500, 2000, 8000, 32000, 120000)) + 
  scale_x_continuous(breaks=c(-5,0,5,10,15,20,25)) 
plot1

plot2 <- ggplot(no_outliers, aes(x = avg_p , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Befolkningsvekst") + 
  ylab("BNP/innbygger 2019") +
  theme_minimal(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  scale_size_area(guide = "none", max_size = 14) + 
  theme(legend.text = element_text(size = 5,color="black"),
        legend.title = element_blank(),
        legend.key.height = unit(.3, 'cm'),
        legend.key.width = unit(.3, 'cm'))+  
  scale_colour_manual(values = rainbow(9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  labs(title = "Figur 2: BNP/innbygger og befolkningsvekst") + 
  scale_y_continuous(trans = 'log2', labels = dollar, breaks=c(500, 2000, 8000, 32000, 120000)) + 
  scale_x_continuous(breaks=c(-1, 0, 1, 2, 3, 4, 5 )) 
plot2

plot3 <- ggplot(no_outliers, aes(x = avg_educ , y = ln_gdppc, na.rm = TRUE)) +
  xlab("Humankapital, utdanning") + 
  ylab("BNP/innbygger 2019") + 
  theme_minimal(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  scale_size_area(guide = "none", max_size = 14) + 
  theme(legend.text = element_text(size = 5,color="black"),
        legend.title = element_blank(),
        legend.key.height = unit(.3, 'cm'),
        legend.key.width = unit(.3, 'cm'))+  
  scale_colour_manual(values = rainbow(9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  labs(title = "Figur 3: BNP/innbygger og humankapital") + 
  scale_y_continuous(trans = 'log2', labels = dollar, breaks=c(500, 2000, 8000, 32000, 120000)) + 
  scale_x_continuous(breaks=c(0,2.5,5,7.5,10,12.5,15))
plot3

#################################################

plot4 <- ggplot(no_outliers, aes(x = avg_nsy , y = avg_gdpgrowth, na.rm = TRUE)) +
  xlab("Sparing som andel av BNI (Netto)") + 
  ylab("Vekstrate i BNP") + 
  theme_minimal(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  scale_size_area(guide = "none", max_size = 14) + 
  theme(legend.text = element_text(size = 5,color="black"),
        legend.title = element_blank(),
        legend.key.height = unit(.3, 'cm'),
        legend.key.width = unit(.3, 'cm'))+  
  scale_colour_manual(values = rainbow(9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  labs(title = "Figur 4: Vekstrate i BNP og sparing") +
  scale_y_continuous() +
  scale_x_continuous(breaks=c(-5,0,5,10,15,20,25)) 
plot4

plot5 <- ggplot(no_outliers, aes(x = avg_educ , y = avg_gdpgrowth, na.rm = TRUE)) +
  xlab("Humankapital, utdanning") + 
  ylab("Vekstrate i BNP") + 
  theme_minimal(base_size = 14) + 
  geom_point(aes(size = poptot, color = region), alpha = 0.8) + 
  scale_size_area(guide = "none", max_size = 14) + 
  theme(legend.text = element_text(size = 5,color="black"),
        legend.title = element_blank(),
        legend.key.height = unit(.3, 'cm'),
        legend.key.width = unit(.3, 'cm'))+ 
  scale_colour_manual(values = rainbow(9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white'))+
  labs(title = "Figur 5: Vekstrate i BNP og humankapital") +
  scale_y_continuous() +
  scale_x_continuous(breaks=c(0,2.5,5,7.5,10,12.5,15))
plot5


regression <- no_outliers %>% 
  dplyr::rename(BNP_per_innbygger = gdppc,
                Sparing_andel_BNI = avg_nsy,
                Befolkningsvekstrate = avg_p,
                Utdanning = avg_educ,
                Investeringsvekstrate = avg_gi,
                Eksportvekstrate = avg_gx,
                Reduksjonsrate_i_naturressurser = avg_nry,
                BNP_per_innbygger_log = ln_gdppc,
                Vekstrate_i_BNP = avg_gdpgrowth)

model1 <- lm(BNP_per_innbygger_log  ~ Sparing_andel_BNI + Investeringsvekstrate + Reduksjonsrate_i_naturressurser, data= regression)
tab_model(model1, CSS = css_theme("cells"))

model2 <- lm(BNP_per_innbygger_log ~ Befolkningsvekstrate + Utdanning +  Eksportvekstrate, data = regression)
tab_model(model2, CSS = css_theme("cells"))

tab_model(model1, model2, CSS = css_theme("cells"))

