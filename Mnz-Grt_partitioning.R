#------------------------------------------------------------------------------#
#---------- SCRIPT FOR PLOTING MONAZITE-GARNET HREE PARTITIONING  -------------#
#--------------------- By Lucas R. Tesser, USP - Brazil -----------------------#
#------------------------------------------------------------------------------#

#------------------------ Uploading the libraries-------------------------------
library(openxlsx) 
library(ggplot2)
library(GGally)
library(viridis)
library(gridExtra)
#----------------------- Importing the dataframe -------------------------------
# input spreadsheet format (check the example)
# column 2 = Age; column 5 = Yttrium (ppm); columns 6 to 18 = La to Yb (ppm)
data <- read.xlsx(file.choose()) # import the data from excel
data_org <- data[order(data$Age),] # organize the dataframe by age

#----------------------- Input chondrite composition --------------------------#
# Create a data frame with garnet composition
chon <- data.frame('Reference' = c('Grt1'), 
                   'La' = c(0.2939), 'Ce' = c(0.6046), 'Pr' = c(0.1369), 
                   'Nd' = c(0.8715), 'Sm' = c(1.0030), 'Eu' =c(0.4460), 
                   'Gd' = c(5.2540), 'Tb' = c(1.8000), 'Dy' = c(13.3900), 
                   'Ho' = c(2.9180), 'Er' = c(8.5830), 'Tm' = c(1.5190), 
                   'Yb' = c(11.9800), stringsAsFactors = FALSE)

# You can add other samples below
# chon <- data.frame('Reference' = c('Grt2'), 
#                    'La' = c(0.2491), 'Ce' = c(0.5952), 'Pr' = c(0.1451), 
#                    'Nd' = c(0.8544), 'Sm' = c(1.4110), 'Eu' =c(0.5420), 
#                    'Gd' = c(12.6200), 'Tb' = c(4.5450), 'Dy' = c(13.3900), 
#                    'Ho' = c(4.6040), 'Er' = c(9.6290), 'Tm' = c(1.1630), 
#                    'Yb' = c(7.8050), stringsAsFactors = FALSE)

# chon <- data.frame('Reference' = c('Grt3'), 
#                    'La' = c(0.2067), 'Ce' = c(0.1992), 'Pr' = c(0.0650), 
#                    'Nd' = c(0.4085), 'Sm' = c(1.1380), 'Eu' =c(0.3861), 
#                    'Gd' = c(8.5940), 'Tb' = c(1.5120), 'Dy' = c(6.9650), 
#                    'Ho' = c(0.9535), 'Er' = c(1.9540), 'Tm' = c(0.2725), 
#                    'Yb' = c(1.3140), stringsAsFactors = FALSE)


equil <- data.frame('Reference' = c('Rubatto2006-SGL5'), 
                    'La' = c(12172786), 'Ce' = c(3207594), 
                    'Pr' = c(567768), 'Nd' = c(112660),
                    'Sm' = c(5936), 'Eu' =c(1283), 
                    'Gd' = c(644), 'Tb' = c(163), 'Dy' = c(56), 
                    'Ho' = c(24), 'Er' = c(12), 'Tm' = c(6.5), 
                    'Yb' = c(3.8), stringsAsFactors = FALSE)

# Create a new dataframe to store the normalized values
normalized_data <- data_org[, c(2, 5, 6:18)]

for (i in 1:dim(data_org)[1]) {
  normalized_data[i, 3:15] <- data_org[i, 6:18] / chon[1, 2:14]
}

normalized_equil <- equil[, c(2, 3:14)]

# First plot
plot1 <- ggparcoord(normalized_data, 
                    columns = 9:15, # Range of data to be plotted
                    groupColumn = 1, # 1 = Age
                    showPoints = FALSE, # disable/enable points in the lines
                    scale="globalminmax", 
                    title = "Example 1", # graph title
                    boxplot = FALSE) +
  scale_y_log10(oob = scales::squish_infinite, 
                limits = c(1.0, 10000)) + # define limits of y-axis
  scale_color_viridis(option = "D", # select A to H to change the color scheme
                      discrete = FALSE, 
                      direction = -1) +
  theme_light()+
  labs(x = "Trace Element", y = "Monazite/Garnet") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_line(linewidth = 1.2, alpha = 0.5) # Change line thickness and alpha

# Second plot (equilibrium monazite-garnet)
 plot2 <- ggparcoord(normalized_equil, 
                    columns = 7:13, # Range of data to be plotted
                    showPoints = TRUE, # disable/enable points in the lines
                    scale="globalminmax", 
                    title = "Monazite–garnet Rubatto et al. 2006",
                    boxplot = FALSE) +
  scale_y_log10(oob = scales::squish_infinite, 
                limits = c(1.0, 10000)) + # define limits of y-axis
  theme_light() +
  labs(x = "Trace Element", y = "Monazite-Garnet") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_line(linewidth = 0.6, alpha = 0.4) # Change line thickness and alpha
# Arrange the plots side by side
grid.arrange(plot1, 
             plot2, 
             ncol = 2)
# --------------------------------- END --------------------------------------#
