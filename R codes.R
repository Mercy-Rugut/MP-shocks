###############################################################################

###############################################################################
###--------------------------- Milestones ----------------------------------### 
###############################################################################

# 1.Identification using Jarocinski & Karadi approach (Traditional MP shocks)
# 2.Auroba & Drechsel shocks to identify conventional MP shocks(Text-based shocks)
# 3.Obtain sentiment from Beige  book and do PCA 
# 4.Clean traditional MP shocks and obtain new MP shocks 

###############################################################################
###--------------------- Load/install required packages --------------------### 
###############################################################################

# Load/install an additional library/package called "zoo" 
if ("zoo" %in% installed.packages()){
  library(zoo)
} else{
  install.packages("zoo")
  library(zoo)
}

# Load/install an additional library/package called "vars" 
if ("vars" %in% installed.packages()){
  library(vars)
} else{
  install.packages("vars")
  library(vars)
}

# Load/install an additional library/package called "stargazer" 
if ("stargazer" %in% installed.packages()){
  library(stargazer)
} else{
  install.packages("stargazer")
  library(stargazer)
}

# Load/install factoextra
if (!require("factoextra")) {
  install.packages("factoextra")
  library(factoextra)
}

# Load/install corrplot
if (!require("corrplot")) {
  install.packages("corrplot")
  library(corrplot)
}

options(scipen = 9) # Avoid scientific notation

# Get working directory 
getwd()
###############################################################################
###------------------ Part 1: Descriptive analysis------ -------------------###
###############################################################################
###############################################################################
# Five main low-frequency (monthly) macroeconomic variables 
# gs1       - One-year constant-maturity Treasury yield (monetary policy indicator)
# logsp500  - S&P 500 in log levels (stock price index)
# us_gdpdef - GDP deflator in log levels (price index)
# us_rgdp   - Real GDP in log levels (real activity)
# ebpnew    - Excess bond premium (financial conditions/uncertainty) 

# our instruments
# pmnegm_ff4sp500 - Surprises in the three-month fed funds futures (poor man's sign restricted)
# Shocks- Monetary policy shocks constructed using Fed internal forecasts and textual analysis of staff documents
# New Mpshocks - from regressing sentiments on pmnegm_ff4sp500
###############################################################################
###---  Plot of monetary policy and central bank information shocks------###
###############################################################################
# Allocating MP shocks as a timeseries
if("jk2020_us_shocks.csv" %in% list.files()){
  mpshks <- read.table("jk2020_us_shocks.csv", sep=",",header=T)
}else{ # file.choose() allows to choose the file interactively
  mpshks <- read.table(file.choose(), sep=",",header=T)
}

# sub setting the shocks
mpshks.MP  <- subset(mpshks, sp500_hf*ff4_hf < 0) # Subset of "pure monetary policy shocks"
mpshks.CBI <- subset(mpshks, sp500_hf*ff4_hf > 0) # Subset of "pure central bank information shocks"
# Plotting the shocks
plot(x = mpshks.MP[,"ff4_hf"],   # Define variable on the x-axis
     y = mpshks.MP[,"sp500_hf"], # Define variable on the y-axis
     
     xlab = "Surprise in the three-month fed funds futures", #Label of x-axis
     ylab = "Surprise in the S&P 500",                       #Label of y-axis    
     
     xlim = c(-0.3, 0.3), # Ranges of values on the x-axis
     ylim = c(-3, 4),   # Ranges of values on the y-axis
     pch = 16,  # Shape of points
     cex = 0.75, # Size of points
     col = "red"
)
points(x = mpshks.CBI[,"ff4_hf"],   # Define variable on the x-axis
       y = mpshks.CBI[,"sp500_hf"], # Define variable on the y-axis
       col = "blue", 
       pch = 16,  # Shape of points
       cex = 0.75, # Size of points
)
abline(h = 0, v = 0) # Add vertical/horizontal line at zero
abline(lm(sp500_hf ~ ff4_hf, mpshks.MP), col = "red", lwd = 2)
abline(lm(sp500_hf ~ ff4_hf, mpshks.CBI), col = "blue", lwd = 2)
legend("topright", 
       legend = c("Monetary Policy Shocks", "Central Bank Information Shocks"), 
       col = c("red", "blue"), 
       pch = 16, 
       pt.cex = 0.75,
       bty = "n")
###############################################################################
###------Plot of sentiment indicators for selected economic concepts--------###
###############################################################################
library(lubridate) # For date parsing
# # Allocate the sentiment variables to the object "beigesent"
if ("Beigebook sentiments.csv" %in% list.files()) {
  beigesent <- read.csv("Beigebook sentiments.csv")
} else {
  beigesent <- read.csv(file.choose())
}

# Convert the FOMC meeting date to a proper Date format (first column)
beigesent$Date <- as.Date(paste0(substr(beigesent[, 1], 1, 4), "-",
                                 substr(beigesent[, 1], 6, 7), "-01"))
# z-score standardization
numeric_cols <- sapply(beigesent, is.numeric)
beigesent_scaled <- beigesent
beigesent_scaled[, numeric_cols] <- scale(beigesent[, numeric_cols])

# Add NBER recession periods
recessions <- data.frame(
  start = as.Date(c("1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01")),
  end   = as.Date(c("1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01"))
)
# Shading function
shade_recession <- function(start_dates, end_dates, ymin, ymax) {
  for (i in seq_along(start_dates)) {
    rect(start_dates[i], ymin, end_dates[i], ymax, col = rgb(1, 0, 0, 0.2), border = NA)
  }
}
# Plotting function
plot_sentiment <- function(df, varname, title, recessions) {
  if (!varname %in% names(df)) stop(paste("Column", varname, "not found!"))
   x <- df$Date
  y <- df[[varname]]
  plot(x, y, type = "l", col = "blue", lwd = 1,
       main = title,
       xlab = "Date",           # X-axis label
       ylab = "Standardized Sentiment Score")  # Y-axis label
   shade_recession(recessions$start, recessions$end, min(y, na.rm = TRUE), max(y, na.rm = TRUE))
  abline(h = 0, col = "black", lty = 2)
  lines(x, y, col = "blue", lwd = 1)
}
# Set up a 2x2 plot layout
par(mfrow = c(2, 2))  # 2 rows, 2 columns
# Plot selected variables
plot_sentiment(beigesent_scaled, "wages", "Wages", recessions)
plot_sentiment(beigesent_scaled, "credit", "Credit", recessions)
plot_sentiment(beigesent_scaled, "oil.prices", "Oil prices", recessions)
plot_sentiment(beigesent_scaled, "consumer.confidence", "Consumer confidence", recessions)

# Common axis labels
mtext("FOMC meeting date", side = 1, outer = TRUE)
mtext("Standardized sentiment score", side = 2, outer = TRUE)
par(mfrow = c(1, 1))  # Reset to default single plot layout

###############################################################################
###------------- Plot  macroeconomic variables timeseries over time---------###
###############################################################################
# Allocate the macroeconomic variables to the object "usmacro"
if("jk2020-us_variables.csv" %in% list.files()){
  usmacro <- read.table("jk2020-us_variables.csv", sep=",",header=T)
}else{ # file.choose() allows to choose the file interactively
  usmacro <- read.table(file.choose(), sep=",",header=T)
}
# Select the following five macroeconomic variables 
var.slct <- c("gs1", "logsp500", "us_gdpdef", "us_rgdp", "ebpnew")
# Specify "usmacro" as time series object
usmacro <- ts(usmacro[,var.slct], end = c(2016, 12), frequency = 12)
colnames(usmacro) <- c("GS1",  "SP500", "GDPDEF", "RGDP", "EBP") # Use shorter labels for variables
# Subset data 
# Specify start date 
ts.start <- c(1980,1)
usmacro <- window(usmacro, 
                  start = ts.start)
# Time series plot 
plot.ts(usmacro, 
        main = "US time series data",        # Title of plot
        xlab = "Month",                      # Label of x-axis
        col  = 1,                            # Define colours
        lty  = 1,                            # Define line types
        lwd  = 2                             # Define line width
)
###############################################################################
###------------- Part 2: Identification using traditional MP shocks---------###
###############################################################################
# Variables ordering via Cholesky
var.order <- c("RGDP",   # real activity: slow-moving
               "GDPDEF", # price index: slow-moving
               "GS1",    # policy rate: fast-moving 
               "SP500",  # stock market: fast-moving
               "EBP")    # financial conditions: fast-moving

usmacro   <- usmacro[,var.order] # Reorder columns in dataset

# Select high-frequency instrument
shk.slct <- "pmnegm_ff4sp500" # Monetary policy shock instrument associated with a "pure monetary policy shock"
mpshks <- ts(mpshks[,shk.slct], end = c(2016, 12), frequency = 12)
mpshks <- window(mpshks, end = c(2007,12))

usmacro.iv <- ts.union(mpshks, usmacro) # Merge monetary policy shocks with macroecnomic variables
var.order.iv <- c("MPshks", var.order)  
colnames(usmacro.iv) <- var.order.iv    # Relabel the variables of this new object
usmacro.iv[is.na(usmacro.iv)] <- 0 # Replace NAs with 0

# Estimate augmented VAR
var.6var <- vars::VAR(usmacro.iv,      # Available data
                      p = 12,          # Consider 12 lags
                      type = "const")  # Include an intercept

# Obtain the estimation output
summary(var.6var)
# Obtain the estimation output as a stargazer table
stargazer::stargazer(var.6var$varresult, type = "text")

nhor <- 36 # Maximum horizon of impulse responses
irf.6var <- irf(var.6var,        #VAR object
                impulse = "MPshks", # Impulse variable (in this case a "monetary policy shock")
                response =  var.order.iv, # Response variables
                n.ahead = nhor,  
                ci = 0.67,     # Confidence interval (67% corresponds to +/- one standard deviation) 
                runs = 1000)   # Number of bootstrapping runs to obtain confidence intervals

plot(irf.6var)

irf.mp   <- data.frame(irf.6var$irf$MPshks, irf.6var$Lower$MPshks, irf.6var$Upper$MPshks)
colnames(irf.mp) <- c(paste0(var.order.iv, ".m"), 
                      paste0(var.order.iv, ".l"),
                      paste0(var.order.iv, ".u"))

par(mfrow=c(2,3)) # Divides the plotting window into three separate panels (2 rows x 3 columns)

# 1.) IRF of MP instrument to MP shock
plot(x = 0:nhor,
     y = irf.mp$MPshks.m, type = "l", main = "MP instrument to MP shock", 
     xlab = '', ylab = '',
     ylim = c(min(irf.mp$MPshks.l), max(irf.mp$MPshks.u))*1.1)
polygon(x = c(0:nhor, rev(0:nhor)), 
        y = c(irf.mp$MPshks.u, rev(irf.mp$MPshks.l)),
        col = 'lightgray',
        lty = 0)
lines(x = 0:nhor, y = irf.mp$MPshks.m, lty=1, col = "black", lwd = 2) # Solid line for median response
lines(x = 0:nhor, y = irf.mp$MPshks.l,  lty=2, col = "black", lwd = 1) # Dashed lines for bounds
lines(x = 0:nhor, y = irf.mp$MPshks.u,  lty=2, col = "black", lwd = 1)
abline(h=0) # Horizontal zero line

# 2.) IRF of real GDP to MP shock
plot(x = 0:nhor,
     y = irf.mp$RGDP.m, type = "l", main = "100 x log(RGDP) to MP shock", 
     xlab = '', ylab = '',
     ylim = c(min(irf.mp$RGDP.l), max(irf.mp$RGDP.u))*1.1)
polygon(x = c(0:nhor, rev(0:nhor)), 
        y = c(irf.mp$RGDP.u, rev(irf.mp$RGDP.l)),
        col = 'lightgray',
        lty = 0)
lines(x = 0:nhor, y = irf.mp$RGDP.m, lty=1, col = "black", lwd = 2) # Solid line for median response
lines(x = 0:nhor, y = irf.mp$RGDP.l,  lty=2, col = "black", lwd = 1) # Dashed lines for bounds
lines(x = 0:nhor, y = irf.mp$RGDP.u,  lty=2, col = "black", lwd = 1)
abline(h=0) # Horizontal zero line

# 3.) IRF of GDP deflator to MP shock
plot(x = 0:nhor,
     y = irf.mp$GDPDEF.m, type = "l", main = "100 x log(GDP deflator) to MP shock", 
     xlab = '', ylab = '',
     ylim = c(min(irf.mp$GDPDEF.l), max(irf.mp$GDPDEF.u))*1.1)
polygon(x = c(0:nhor, rev(0:nhor)), 
        y = c(irf.mp$GDPDEF.u, rev(irf.mp$GDPDEF.l)),
        col = 'lightgray',
        lty = 0)
lines(x = 0:nhor, y = irf.mp$GDPDEF.m, lty=1, col = "black", lwd = 2) # Solid line for median response
lines(x = 0:nhor, y = irf.mp$GDPDEF.l,  lty=2, col = "black", lwd = 1) # Dashed lines for bounds
lines(x = 0:nhor, y = irf.mp$GDPDEF.u,  lty=2, col = "black", lwd = 1)
abline(h=0) # Horizontal zero line

# 4.) IRF of GS1 to MP shock
plot(x = 0:nhor,
     y = irf.mp$GS1.m, type = "l", main = "GS1 to MP shock", 
     xlab = '', ylab = '',
     ylim = c(min(irf.mp$GS1.l), max(irf.mp$GS1.u))*1.1)
polygon(x = c(0:nhor, rev(0:nhor)), 
        y = c(irf.mp$GS1.u, rev(irf.mp$GS1.l)),
        col = 'lightgray',
        lty = 0)
lines(x = 0:nhor, y = irf.mp$GS1.m, lty=1, col = "black", lwd = 2) # Solid line for median response
lines(x = 0:nhor, y = irf.mp$GS1.l,  lty=2, col = "black", lwd = 1) # Dashed lines for bounds
lines(x = 0:nhor, y = irf.mp$GS1.u,  lty=2, col = "black", lwd = 1)
abline(h=0) # Horizontal zero line


# 5.) IRF of SP500 to MP shock
plot(x = 0:nhor,
     y = irf.mp$SP500.m, type = "l", main = "100 x log(SP 500) to MP shock", 
     xlab = '', ylab = '',
     ylim = c(min(irf.mp$SP500.l), max(irf.mp$SP500.u))*1.1)
polygon(x = c(0:nhor, rev(0:nhor)), 
        y = c(irf.mp$SP500.u, rev(irf.mp$SP500.l)),
        col = 'lightgray',
        lty = 0)
lines(x = 0:nhor, y = irf.mp$SP500.m, lty=1, col = "black", lwd = 2) # Solid line for median response
lines(x = 0:nhor, y = irf.mp$SP500.l,  lty=2, col = "black", lwd = 1) # Dashed lines for bounds
lines(x = 0:nhor, y = irf.mp$SP500.u,  lty=2, col = "black", lwd = 1)
abline(h=0) # Horizontal zero line

# 6.) IRF of EBP to MP shock
plot(x = 0:nhor,
     y = irf.mp$EBP.m, type = "l", main = "EBP to MP shock", 
     xlab = '', ylab = '',
     ylim = c(min(irf.mp$EBP.l), max(irf.mp$EBP.u))*1.1)
polygon(x = c(0:nhor, rev(0:nhor)), 
        y = c(irf.mp$EBP.u, rev(irf.mp$EBP.l)),
        col = 'lightgray',
        lty = 0)
lines(x = 0:nhor, y = irf.mp$EBP.m, lty=1, col = "black", lwd = 2) # Solid line for median response
lines(x = 0:nhor, y = irf.mp$EBP.l,  lty=2, col = "black", lwd = 1) # Dashed lines for bounds
lines(x = 0:nhor, y = irf.mp$EBP.u,  lty=2, col = "black", lwd = 1)
abline(h=0) # Horizontal zero line
###############################################################################
######---------- Part 3: Identification using	text-based shocks --------#######
###############################################################################

# Allocate the shocks as a timeseries
if("Auroba shocks.csv" %in% list.files()){
  sentmpshks <- read.table("Auroba shocks.csv", sep=",",header=T)
}else{ # file.choose() allows to choose the file interactively
  sentmpshks <- read.table(file.choose(), sep=",",header=T)
}

# Select sentiment instrument
sentshk.slct <- "Shock" # sentiment shock  associated with conventional monetary policy shock"
sentshks <- ts(sentmpshks[, sentshk.slct], end = c(2008, 10), frequency = 12)
sentshks <- window(sentshks, start = c(1990,2), end = c(2007,12))

usmacro.v <- ts.union(sentshks, usmacro) # Merge monetary policy shocks with macroecnomic variables
var.order.v <- c("sentshks", var.order)  
colnames(usmacro.v) <- var.order.v    # Relabel the variables of this new object
usmacro.v[is.na(usmacro.v)] <- 0 # Replace NAs with 0

# Estimate augmented VAR
var2.6var <- vars::VAR(usmacro.v,      # Available data
                       p = 12,          # Consider 12 lags
                       type = "const")  # Include an intercept

# Obtain the estimation output
summary(var2.6var)
# Obtain the estimation output as a stargazer table
stargazer::stargazer(var2.6var$varresult, type = "text")
nhor <- 36 # Maximum horizon of impulse responses
irf2.6var <- irf(var2.6var,        #VAR object
                 impulse = "sentshks", # Impulse variable (in this case a "sentiment policy shock")
                 response =  var.order.v, # Response variables
                 n.ahead = nhor,  
                 ci = 0.67,     # Confidence interval (67% corresponds to +/- one standard deviation) 
                 runs = 1000)   # Number of bootstrapping runs to obtain confidence intervals

plot(irf2.6var)

irf.sent   <- data.frame(irf2.6var$irf$sentshks, irf2.6var$Lower$sentshks, irf2.6var$Upper$sentshks)
colnames(irf.sent) <- c(paste0(var.order.v, ".m"), 
                        paste0(var.order.v, ".l"),
                        paste0(var.order.v, ".u"))

par(mfrow=c(2,3)) # Divides the plotting window into three separate panels (2 rows x 3 columns)

# 1.) IRF of Text-based MP instrument to MP shock
plot(x = 0:nhor,
     y = irf.sent[[paste0("sentshks", ".m")]], type = "l", main = "Text-based MP instrument to MP shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sent[[paste0("sentshks", ".l")]]), max(irf.sent[[paste0("sentshks", ".u")]])) * 1.1)
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sent[[paste0("sentshks", ".u")]], rev(irf.sent[[paste0("sentshks", ".l")]])),
        col = "lightgray",
        lty = 0)
lines(x = 0:nhor, y = irf.sent[[paste0("sentshks", ".m")]], lty = 1, col = "black", lwd = 2)
lines(x = 0:nhor, y = irf.sent[[paste0("sentshks", ".l")]], lty = 2, col = "black", lwd = 1)
lines(x = 0:nhor, y = irf.sent[[paste0("sentshks", ".u")]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 2.) IRF of real GDP to Text-based MP shock
plot(x = 0:nhor,
     y = irf.sent[[paste0("RGDP", ".m")]], type = "l", main = "100 x log(RGDP) to Text-based MP shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sent[[paste0("RGDP", ".l")]]), max(irf.sent[[paste0("RGDP", ".u")]])) * 1.1)
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sent[[paste0("RGDP", ".u")]], rev(irf.sent[[paste0("RGDP", ".l")]])),
        col = "lightgray",
        lty = 0)
lines(x = 0:nhor, y = irf.sent[[paste0("RGDP", ".m")]], lty = 1, col = "black", lwd = 2)
lines(x = 0:nhor, y = irf.sent[[paste0("RGDP", ".l")]], lty = 2, col = "black", lwd = 1)
lines(x = 0:nhor, y = irf.sent[[paste0("RGDP", ".u")]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 3.) IRF of GDP deflator to Text-based MP shock
plot(x = 0:nhor,
     y = irf.sent[[paste0("GDPDEF", ".m")]], type = "l", main = "100 x log(GDP deflator) to Text-based MP shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sent[[paste0("GDPDEF", ".l")]]), max(irf.sent[[paste0("GDPDEF", ".u")]])) * 1.1)
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sent[[paste0("GDPDEF", ".u")]], rev(irf.sent[[paste0("GDPDEF", ".l")]])),
        col = "lightgray",
        lty = 0)
lines(x = 0:nhor, y = irf.sent[[paste0("GDPDEF", ".m")]], lty = 1, col = "black", lwd = 2)
lines(x = 0:nhor, y = irf.sent[[paste0("GDPDEF", ".l")]], lty = 2, col = "black", lwd = 1)
lines(x = 0:nhor, y = irf.sent[[paste0("GDPDEF", ".u")]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 4.) IRF of GS1 to Text-based MP shock
plot(x = 0:nhor,
     y = irf.sent[[paste0("GS1", ".m")]], type = "l", main = "GS1 to Text-based MP shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sent[[paste0("GS1", ".l")]]), max(irf.sent[[paste0("GS1", ".u")]])) * 1.1)
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sent[[paste0("GS1", ".u")]], rev(irf.sent[[paste0("GS1", ".l")]])),
        col = "lightgray",
        lty = 0)
lines(x = 0:nhor, y = irf.sent[[paste0("GS1", ".m")]], lty = 1, col = "black", lwd = 2)
lines(x = 0:nhor, y = irf.sent[[paste0("GS1", ".l")]], lty = 2, col = "black", lwd = 1)
lines(x = 0:nhor, y = irf.sent[[paste0("GS1", ".u")]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 5.) IRF of SP500 to Text-based MP shock
plot(x = 0:nhor,
     y = irf.sent[[paste0("SP500", ".m")]], type = "l", main = "SP500 to Text-based MP shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sent[[paste0("SP500", ".l")]]), max(irf.sent[[paste0("SP500", ".u")]])) * 1.1)
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sent[[paste0("SP500", ".u")]], rev(irf.sent[[paste0("SP500", ".l")]])),
        col = "lightgray",
        lty = 0)
lines(x = 0:nhor, y = irf.sent[[paste0("SP500", ".m")]], lty = 1, col = "black", lwd = 2)
lines(x = 0:nhor, y = irf.sent[[paste0("SP500", ".l")]], lty = 2, col = "black", lwd = 1)
lines(x = 0:nhor, y = irf.sent[[paste0("SP500", ".u")]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 6.) IRF of EBP to Text-based MP shock
plot(x = 0:nhor,
     y = irf.sent[[paste0("EBP", ".m")]], type = "l", main = "EBP to Text-based MP shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sent[[paste0("EBP", ".l")]]), max(irf.sent[[paste0("EBP", ".u")]])) * 1.1)
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sent[[paste0("EBP", ".u")]], rev(irf.sent[[paste0("EBP", ".l")]])),
        col = "lightgray",
        lty = 0)
lines(x = 0:nhor, y = irf.sent[[paste0("EBP", ".m")]], lty = 1, col = "black", lwd = 2)
lines(x = 0:nhor, y = irf.sent[[paste0("EBP", ".l")]], lty = 2, col = "black", lwd = 1)
lines(x = 0:nhor, y = irf.sent[[paste0("EBP", ".u")]], lty = 2, col = "black", lwd = 1)
abline(h = 0)
###############################################################################
###----------- Part 4: creating PCA for beige book sentiments--------------#####
###############################################################################
# Select 14 sentiment variables
sent.slct <- c("borrowing", "banks", "credit", "consumption", "inflation",
  "liquidity", "mortgage.interest", "loan.rates","vacancy.rates", "money.market", 
  "financial.markets", "credit.quality", "consumer.confidence", "wages")

# Convert full dates (YYYY-MM-DD) to yearmon format for monthly alignment
# Replace 'Date' with the exact column name if it's different
dates <- as.yearmon(beigesent_scaled$Date)  # auto-detects format like "1982-01-15"

# Create a zoo object for the selected sentiments since months are not aligned
beigesent_data <- zoo(beigesent_scaled[, sent.slct], order.by = dates)

# Rename columns for simpler plotting
colnames(beigesent_data) <- paste0("st", 1:14)

# Subset 
beigesent_data_sub <- window(beigesent_data, start = as.yearmon("1990-02"), end = as.yearmon("2007-12"))

# Plot all 14 time series
par(mfrow = c(1, 1))  # resets to default (1 plot at a time)

ts.plot(beigesent_data_sub,
        main = "Standardized Sentiment Measures (1990–2007)",
        ylab = "Standardized Index",
        xlab = "Month",
        col = 1:14,     # Use different colors
        lty = "solid",
        lwd = 2
)
abline(h = 0, col = "grey") # Add a horizontal line at 0

# Remove missing values for PCA
pca_input <- na.omit(beigesent_data_sub)
#  Perform PCA
PCA <- prcomp(pca_input, scale. = TRUE)
#  View PCA summary
print(summary(PCA))
# Scree plot - variance explained by each component
fviz_eig(PCA, addlabels = TRUE, ylim = c(0, 100))
# Create time series of principal components
PC.series <- ts(PCA$x, start = c(1990, 2), frequency = 12)  # Adjust start if different
#  Plot the first principal component
plot(PC.series[, 1], main = "First Principal Component (PC1)", ylab = "Score", xlab = "Time")
###############################################################################
###------------------------ Regression with MP shock ---------------------#####
###############################################################################
#Create yearmon date index
mp_dates <- as.yearmon(with(mpshks.MP, paste(year, month, sep = "-")), "%Y-%m")
#Create zoo object for MP shock variable
mp_zoo <- zoo(mpshks.MP$pmnegm_ff4sp500, order.by = mp_dates)
#Extract PC1 from ts object
PC1_ts <- PC.series[, 1]
#Create date index (matching start of ts object)
pc1_dates <- as.yearmon(time(PC1_ts))# extracts time from ts and converts to yearmon
#Convert to zoo
pc1_zoo <- zoo(PC1_ts, order.by = pc1_dates)
# Match MP shock series to Beige Book months
matched_mp <- mp_zoo[index(pc1_zoo)]
# Combine and run regression
reg_data <- na.omit(cbind(MPshock = matched_mp, PC1 = pc1_zoo))
model <- lm(MPshock ~ PC1, data = as.data.frame(reg_data))
summary(model)

# Extract residuals
resid_zoo <- zoo(residuals(model), order.by = index(reg_data))

# Extract MP shock again (for clarity)
mp_zoo_sub <- zoo(reg_data$MPshock, order.by = index(reg_data))

plot(mp_zoo_sub, type = "l", col = "blue", lwd = 2,
     main = "MP shock and regression residuals",
     ylab = "Value", xlab = "Time")
lines(resid_zoo, col = "red", lwd = 2, lty = 2)
legend("bottomleft", legend = c("MP Shock", "Residuals"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

###############################################################################
######----------- Part 3: Identification using	new shocks -------------#######
###############################################################################
# Safely merge zoo objects by date
usmacro.vi <- merge(mp_zoo_sub, usmacro, all = TRUE)  # or all = FALSE to only keep overlapping dates

var.order.vi <- c("mp_zoo_sub", var.order)  
colnames(usmacro.vi) <- var.order.vi    # Relabel the variables of this new object
usmacro.vi[is.na(usmacro.vi)] <- 0 # Replace NAs with 0

# Estimate augmented VAR
var3.6var <- vars::VAR(usmacro.vi,      # Available data
                       p = 12,          # Consider 12 lags
                       type = "const")  # Include an intercept

# Obtain the estimation output
summary(var3.6var)
# Obtain the estimation output as a stargazer table
stargazer::stargazer(var3.6var$varresult, type = "text")
nhor <- 36 # Maximum horizon of impulse responses
irf3.6var <- irf(var3.6var,        #VAR object
                 impulse = "mp_zoo_sub", # Impulse variable (in this case a "sentiment policy shock")
                 response =  var.order.vi, # Response variables
                 n.ahead = nhor,  
                 ci = 0.67,     # Confidence interval (67% corresponds to +/- one standard deviation) 
                 runs = 1000)   # Number of bootstrapping runs to obtain confidence intervals

plot(irf3.6var)

irf.sentmp   <- data.frame(irf3.6var$irf$mp_zoo_sub, irf3.6var$Lower$mp_zoo_sub, irf3.6var$Upper$mp_zoo_sub)
colnames(irf.sentmp) <- c(paste0(var.order.vi, ".m"), 
                        paste0(var.order.vi, ".l"),
                        paste0(var.order.vi, ".u"))

par(mfrow=c(2,3)) # Divides the plotting window into three separate panels (2 rows x 3 columns)

# 1. New MP Instrument to  MP Shock
plot(x = 0:nhor,
     y = irf.sentmp[["mp_zoo_sub.m"]], type = "l",
     main = "New MP Instrument to  MP Shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sentmp[["mp_zoo_sub.l"]]),
              max(irf.sentmp[["mp_zoo_sub.u"]]) * 1.1))
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sentmp[["mp_zoo_sub.u"]],
              rev(irf.sentmp[["mp_zoo_sub.l"]])),
        col = "lightgray", lty = 0)
lines(0:nhor, irf.sentmp[["mp_zoo_sub.m"]], lty = 1, col = "black", lwd = 2)
lines(0:nhor, irf.sentmp[["mp_zoo_sub.l"]], lty = 2, col = "black", lwd = 1)
lines(0:nhor, irf.sentmp[["mp_zoo_sub.u"]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 2. RGDP to New MP Shock
plot(x = 0:nhor,
     y = irf.sentmp[["RGDP.m"]], type = "l",
     main = "100 x log(RGDP) to New MP Shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sentmp[["RGDP.l"]]),
              max(irf.sentmp[["RGDP.u"]]) * 1.1))
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sentmp[["RGDP.u"]],
              rev(irf.sentmp[["RGDP.l"]])),
        col = "lightgray", lty = 0)
lines(0:nhor, irf.sentmp[["RGDP.m"]], lty = 1, col = "black", lwd = 2)
lines(0:nhor, irf.sentmp[["RGDP.l"]], lty = 2, col = "black", lwd = 1)
lines(0:nhor, irf.sentmp[["RGDP.u"]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 3. GDP Deflator to New MP Shock
plot(x = 0:nhor,
     y = irf.sentmp[["GDPDEF.m"]], type = "l",
     main = "100 x log(GDP deflator) to New MP Shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sentmp[["GDPDEF.l"]]),
              max(irf.sentmp[["GDPDEF.u"]]) * 1.1))
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sentmp[["GDPDEF.u"]],
              rev(irf.sentmp[["GDPDEF.l"]])),
        col = "lightgray", lty = 0)
lines(0:nhor, irf.sentmp[["GDPDEF.m"]], lty = 1, col = "black", lwd = 2)
lines(0:nhor, irf.sentmp[["GDPDEF.l"]], lty = 2, col = "black", lwd = 1)
lines(0:nhor, irf.sentmp[["GDPDEF.u"]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 4. GS1 to New MP Shock
plot(x = 0:nhor,
     y = irf.sentmp[["GS1.m"]], type = "l",
     main = "GS1 to New MP Shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sentmp[["GS1.l"]]),
              max(irf.sentmp[["GS1.u"]]) * 1.1))
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sentmp[["GS1.u"]],
              rev(irf.sentmp[["GS1.l"]])),
        col = "lightgray", lty = 0)
lines(0:nhor, irf.sentmp[["GS1.m"]], lty = 1, col = "black", lwd = 2)
lines(0:nhor, irf.sentmp[["GS1.l"]], lty = 2, col = "black", lwd = 1)
lines(0:nhor, irf.sentmp[["GS1.u"]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 5. SP500 to New MP Shock
plot(x = 0:nhor,
     y = irf.sentmp[["SP500.m"]], type = "l",
     main = "SP500 to New MP Shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sentmp[["SP500.l"]]),
              max(irf.sentmp[["SP500.u"]]) * 1.1))
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sentmp[["SP500.u"]],
              rev(irf.sentmp[["SP500.l"]])),
        col = "lightgray", lty = 0)
lines(0:nhor, irf.sentmp[["SP500.m"]], lty = 1, col = "black", lwd = 2)
lines(0:nhor, irf.sentmp[["SP500.l"]], lty = 2, col = "black", lwd = 1)
lines(0:nhor, irf.sentmp[["SP500.u"]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

# 6. EBP to New MP Shock
plot(x = 0:nhor,
     y = irf.sentmp[["EBP.m"]], type = "l",
     main = "EBP to New MP Shock",
     xlab = '', ylab = '',
     ylim = c(min(irf.sentmp[["EBP.l"]]),
              max(irf.sentmp[["EBP.u"]]) * 1.1))
polygon(x = c(0:nhor, rev(0:nhor)),
        y = c(irf.sentmp[["EBP.u"]],
              rev(irf.sentmp[["EBP.l"]])),
        col = "lightgray", lty = 0)
lines(0:nhor, irf.sentmp[["EBP.m"]], lty = 1, col = "black", lwd = 2)
lines(0:nhor, irf.sentmp[["EBP.l"]], lty = 2, col = "black", lwd = 1)
lines(0:nhor, irf.sentmp[["EBP.u"]], lty = 2, col = "black", lwd = 1)
abline(h = 0)

