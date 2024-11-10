#------------------------------------------------------------------------------#
#---------- SCRIPT FOR PLOTING PETROCHRONOLOGICAL TRACE ELEMENT DATA ----------#
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
# Create a data frame with CI chondrite composition
chon <- data.frame('Reference' = c('CI_McDonough_Sun_1995'), 
                   'La' = c(0.237), 'Ce' = c(0.613), 'Pr' = c(0.0928), 
                   'Nd' = c(0.457), 'Sm' = c(0.148), 'Eu' =c(0.0563), 
                   'Gd' = c(0.199), 'Tb' = c(0.0361), 'Dy' = c(0.246), 
                   'Ho' = c(0.0546), 'Er' = c(0.16), 'Tm' = c(0.0247), 
                   'Yb' = c(0.161), stringsAsFactors = FALSE)

equil <- data.frame('Reference' = c('Rubatto2006-SGL5', 'Hacker2019-E9819D1'), 
                   'La' = c(12172786, 25148575), 'Ce' = c(3207594, 17011927), 
                   'Pr' = c(567768, 3244703), 'Nd' = c(112660,406824),
                   'Sm' = c(5936, 10756), 'Eu' =c(1283, 1356), 
                   'Gd' = c(644, 1456), 'Tb' = c(163, 638), 'Dy' = c(56, 326), 
                   'Ho' = c(24, 161), 'Er' = c(12, 81), 'Tm' = c(6.5, 48), 
                   'Yb' = c(3.8, 28), stringsAsFactors = FALSE)

# Create a new dataframe to store the normalized values
normalized_data <- data_org[, c(2, 5, 6:18)]

for (i in 1:dim(data_org)[1]) {
  normalized_data[i, 3:14] <- data_org[i, 6:18] / chon[1, 2:14]
}

normalized_equil <- equil[, c(2, 3:14)]

# First plot
plot1 <- ggparcoord(normalized_data, 
                    columns = 3:15, # Range of data to be plotted
                    groupColumn = 1, # 1 = Age
                    showPoints = FALSE, # disable/enable points in the lines
                    scale="globalminmax", 
                    title = "Sample DLT39", # graph title
                    boxplot = FALSE) +
  scale_y_log10(oob = scales::squish_infinite, 
                limits = c(1, 1000000)) + # define limits of y-axis
  scale_color_viridis(option = "A", # select A to H to change the color scheme
                      discrete = FALSE, 
                      direction = 1) +
  theme_light()+
  labs(x = "Trace Element", y = "Monazite/Chondrite") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_line(linewidth = 1, alpha = 0.3) # Change line thickness and alpha

# Second plot
plot2 <- ggparcoord(normalized_data, 
                    columns = 3:15, # Range of data to be plotted
                    groupColumn = 2, # 2 = Y in ppm
                    showPoints = FALSE, # disable/enable points in the lines
                    scale="globalminmax", 
                    title = "Sample DLT39", # graph title
                    boxplot = FALSE) +
  scale_y_log10(oob = scales::squish_infinite, 
                limits = c(1, 1000000)) + # define limits of y-axis
  scale_color_viridis(option = "A", # select A to H to change the color scheme
                      discrete = FALSE, 
                      direction = 1) +
  theme_light() +
  labs(x = "Trace Element", y = "Monazite/Chondrite") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_line(linewidth = 1, alpha = 0.3) # Change line thickness and alpha

# Third plot (equilibrium monazite-garnet)
# plot3 <- ggparcoord(normalized_equil, 
#                    columns = 1:13, # Range of data to be plotted
#                    showPoints = TRUE, # disable/enable points in the lines
#                    scale="globalminmax", 
#                    title = "Monazite–garnet REE equilibrium",
#                    boxplot = FALSE) +
#  scale_y_log10(oob = scales::squish_infinite, 
#                limits = c(1, 100000000)) + # define limits of y-axis
#  theme_light() +
#  labs(x = "Trace Element", y = "Monazite-Garnet") +
#  theme(panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank()) +
#  geom_line(linewidth = 0.6, alpha = 0.8) # Change line thickness and alpha
# Arrange the plots side by side
grid.arrange(plot1, plot2, ncol = 2)
# --------------------------------- END --------------------------------------#