UCIzip <- "UCIdata.zip"

if(!file.exists("AMES119URN.csv")){
     fileURL <- "https://fred.stlouisfed.org/graph/fredgraph.csv?chart_type=line&recession_bars=on&log_scales=&bgcolor=%23e1e9f0&graph_bgcolor=%23ffffff&fo=Open+Sans&ts=12&tts=12&txtcolor=%23444444&show_legend=yes&show_axis_titles=yes&drp=0&cosd=1990-01-01&coed=2017-10-01&height=450&stacking=&range=&mode=fred&id=AMES119URN&transformation=lin&nd=1990-01-01&ost=-99999&oet=99999&lsv=&lev=&mma=0&fml=a&fgst=lin&fgsnd=2009-06-01&fq=Monthly&fam=avg&vintage_date=&revision_date=&line_color=%234572a7&line_style=solid&lw=2&scale=left&mark_type=none&mw=2&width=1168"
     download.file(fileURL, destfile="amesUnemployment.csv", method = "curl")
}
ames <- read.csv("amesUnemployment.csv")

#------------------------------------------------------------------------------

timeframe <- function(file){
     file$DATE <- as.Date(file$DATE, format= "%Y-%m-%d")
     subyrs <- subset(file, DATE >= "2006-01-01" & DATE <= "2010-12-01")
     subyrs
}

amesUnemp <- read.csv("AMES119URN.csv", col.names = c("DATE", "AmesUnemp")) %>% timeframe
earnwkly <- read.csv("SMU19111800500000011.csv", col.names = c("DATE", "AmesWeeklyEarnings")) %>% timeframe
houseunits <- read.csv("IABP1FH.csv", col.names = c("DATE", "IAHouseUnitPermits")) %>% timeframe

a <- merge(amesUnemp, earnwkly, all=TRUE)
a <- merge(a, houseunits, all=TRUE)
head(a)
