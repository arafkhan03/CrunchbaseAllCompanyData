#All Company Data Descriptive Stats

# 1.0 Data Preparation and Library Loading
library(skimr)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)

        ## Summary
dim(all_company)
skim(all_company)

        ## Convert Date Type
all_company$`Founded Date` <- as.Date(all_company$`Founded Date`)

        ## Only Keep Columns that have NAs Less than 75%
all_company_NA <- as.data.frame(
        round(colMeans(is.na(all_company)), 2)
)
sum(all_company_NA$`round(colMeans(is.na(all_company)), 2)` < 0.75)
all_company_WNA <- all_company[, all_company_NA$`round(colMeans(is.na(all_company)), 2)` < 0.75]

        ## Summary
skim(all_company_WNA)






# 2.0 Count of Company by Founding Year

        ## Cut Year Variable into Factor
all_company_WNA$Year <- lubridate::year(all_company$`Founded Date`)
all_company_WNA$year_factor <- cut(all_company_WNA$Year, breaks = 10, dig.lab = 4)

        ## Summarize No. of Companies Formed by Year
year_count_company <- all_company_WNA %>%
        dplyr::group_by(year_factor, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Year = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Plot Number of Companies Formed by Founding Year
ylim_year <- c(0, 1.1*max(year_count_company$Count_by_Year))
xx_year <- barplot(year_count_company$Count_by_Year, xaxt = 'n', xlab = '', width = 0.85,
              ylim = ylim_year, main = "Number of Companies Formed by Founding Year", 
              ylab = "Frequency")
text(x = xx_year, y = year_count_company$Count_by_Year, 
     label = year_count_company$Count_by_Year, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_year, labels=year_count_company$year_factor, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)

        ## Percentage of All Companies by Founding Year
year_count_company$Percentage <- as.numeric(format(year_count_company$Count_by_Year/
                                                           sum(year_count_company$Count_by_Year), 
                                                   scientific = FALSE, digits = 2))

        ## Sort DF by Desc(Percentage)
ipos_count_company <- arrange(ipos_count_company, desc(Percentage))





# 3.0 Count of Company by HQ Country

        ## Country of Headquarter
library(tidyr)
all_company_WNA_split <- separate(all_company_WNA, col = "Headquarters Location", sep = ",",
         into = c("City","Province","Country"))
country_latlon <- read.csv("/Users/araf03/Desktop/MS Thesis/External Data Files/Countries Lat Lon.csv")

        ## Summarize No. of Companies by HQ's Country
country_count_company <- all_company_WNA_split %>%
        dplyr::group_by(Country, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Country = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Plot Number of Companies by HQ's Country
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)

                ### Generate the layout. This function return a dataframe with one line per bubble. 
                ### It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(country_count_company$Count_by_Country, 
                                   sizetype='area')

                ### We can add these packing information to the initial data frame
data_country_circlepack <- cbind(country_count_company, packing)

                ### The next step is to go from one center + a radius to the coordinates of a circle that
                ### is drawn by a multitude of straight lines.
dat.gg_country_circle <- circleLayoutVertices(packing, npoints=50)

        ## Make the plot
HQ_Country_Circleplot <- ggplot() + 
        
                ### Make the bubbles
        geom_polygon(data = dat.gg_country_circle, aes(x, y, group = id, 
                                                        fill=as.factor(id)), 
                     colour = "black", alpha = 0.6) +
        
                ### Add text in the center of each bubble + control its size
        geom_text(data = data_country_circlepack, aes(x, y, size=Count_by_Country, 
                                                      label = Country)) +
        ggtitle("Distribution of Companies by Headquarter Location - Country Wise") +
        
                ### General theme:
        scale_size_continuous(range = c(1,4)) +
        theme_void() + 
        theme(legend.position="none") +
        coord_equal()

HQ_Country_Circleplot

        ## Percentage of All Companies by HQ Country
country_count_company$Percentage <- format(country_count_company$Count_by_Country/
        sum(country_count_company$Count_by_Country), scientific = FALSE, digits = 2)

        ## Sort DF by Percentage
country_count_company <-  arrange(country_count_company, desc(Count_by_Country))






# 4.0 Count of Company by Industry Group

        ## Summarize No. of Companies by Industry Group
industry_count_company <- all_company_WNA %>%
        dplyr::group_by(`Industry Groups`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Industry = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of All Companies by HQ Country
industry_count_company$Percentage <- as.numeric(format(industry_count_company$Count_by_Industry/
                                                   sum(industry_count_company$Count_by_Industry), 
                                                   scientific = FALSE, digits = 2))

        ## Sort DF by Percentage
industry_count_company <-  arrange(industry_count_company, desc(Count_by_Industry))

        ## Top 20 Industry Groups
top20_industry_count_company <- industry_count_company[1:20,]
sum(top20_industry_count_company$Percentage)

## Plot Number of Companies by Industry Groups
ylim_industry <- c(0, 1.1*max(top20_industry_count_company$Count_by_Industry))
xx_industry <- barplot(top20_industry_count_company$Count_by_Industry, xaxt = 'n', xlab = '', width = 0.85,
                   ylim = ylim_industry, main = "Number of Companies by Industry Groups", 
                   ylab = "Frequency")
text(x = xx_industry, y = top20_industry_count_company$Count_by_Industry, 
     label = top20_industry_count_company$Count_by_Industry, pos = 3, cex = 0.4, col = "black")
axis(1, at=xx_industry, labels=top20_industry_count_company$`Industry Groups`, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 5.0 Count of Company by Company Type

        ## Summarize No. of Companies by Company Type
type_count_company <- all_company_WNA %>%
        dplyr::group_by(`Company Type`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Type = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of All Companies by Company Type
type_count_company$Percentage <- as.numeric(format(type_count_company$Count_by_Type/
                                                           sum(type_count_company$Count_by_Type), 
                                                   scientific = FALSE, digits = 2))
        ## Write the Table
write.table(type_count_company, file = "/Users/araf03/Desktop/MS Thesis/Tables/Company Type.txt",
            sep = ",")






# 6.0 Count of Company by Company Status

        ## Summarize No. of Companies by Company Status
status_count_company <- all_company_WNA %>%
        dplyr::group_by(`Operating Status`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Status = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of All Companies by Company Status
status_count_company$Percentage <- as.numeric(format(status_count_company$Count_by_Status/
                                                             sum(status_count_company$Count_by_Status), 
                                                     scientific = FALSE, digits = 2))

        ## Write the Table
write.table(status_count_company, file = "/Users/araf03/Desktop/MS Thesis/Tables/Company Status.txt",
            sep = ",")






# 5.0 Count of Company by Employee Range

        ## Summarize No. of Companies by Emp Range
employee_count_company <- all_company_WNA %>%
        dplyr::group_by(`Number of Employees`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Emp_Range = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of All Companies by Emp Range
employee_count_company$Percentage <- as.numeric(format(employee_count_company$Count_by_Emp_Range/
                                                               sum(employee_count_company$Count_by_Emp_Range), 
                                                       scientific = FALSE, digits = 2))

        ## Assign Index
employee_count_company$index <- c(1, 9, 7, 4, 2, 5, 10, 8, 6, 3, 11, 12, 13)

        ## Sort DF by Index
employee_count_company <-  arrange(employee_count_company, employee_count_company$index)

        ## Drop Unwanted Data
employee_count_company <- employee_count_company[1:10,]

        ## Plot Number of Companies by Emp Range
ylim_employee <- c(0, 1.1*max(employee_count_company$Count_by_Emp_Range))
xx_employee <- barplot(employee_count_company$Count_by_Emp_Range, xaxt = 'n', xlab = '', width = 0.85,
                       ylim = ylim_employee, main = "Number of Companies by Employee", 
                       ylab = "Frequency")
text(x = xx_employee, y = employee_count_company$Count_by_Emp_Range, 
     label = employee_count_company$Count_by_Emp_Range, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_employee, labels=employee_count_company$`Number of Employees`, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 6.0 Count of Company by Estimated Revenue Range

        ## Summarize No. of Companies by Rev Range
rev_count_company <- all_company_WNA %>%
        dplyr::group_by(`Estimated Revenue Range`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Rev = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

        ## Percentage of All Companies by Rev Range
rev_count_company$Percentage <- as.numeric(format(rev_count_company$Count_by_Rev/
                                                               sum(rev_count_company$Count_by_Rev), 
                                                       scientific = FALSE, digits = 2))

        ## Assign Index
rev_count_company$index <- c(5, 8, 3, 7, 2, 6, 4, 1)

        ## Sort DF by Index
rev_count_company <-  arrange(rev_count_company, rev_count_company$index)

        ## Plot Number of Companies by Rev Range
ylim_rev <- c(0, 1.1*max(rev_count_company$Count_by_Rev))
xx_rev <- barplot(rev_count_company$Count_by_Rev, xaxt = 'n', xlab = '', width = 0.85,
                       ylim = ylim_rev, main = "Number of Companies by Estimated Revenue Range", 
                       ylab = "Frequency")
text(x = xx_rev, y = rev_count_company$Count_by_Rev, 
     label = rev_count_company$Count_by_Rev, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_rev, labels=rev_count_company$`Estimated Revenue Range`, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)











# 9.0 Count of Company by Funding Status

        ## Summarize No. of Companies by Funding Status
fund_status_count_company <- all_company_WNA %>%
        dplyr::group_by(`Funding Status`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Fundstatus = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE) 

        ## Percentage of All Companies by Funding Status
fund_status_count_company$Percentage <- as.numeric(format(fund_status_count_company$Count_by_Fundstatus/
                                                          sum(fund_status_count_company$Count_by_Fundstatus), 
                                                  scientific = FALSE, digits = 2))

        ## Sort DF by Desc(Percentage)
fund_status_count_company <- arrange(fund_status_count_company, desc(Percentage))

        ## Drop Unnecessary
fund_status_count_company <- fund_status_count_company[1:6,]

        ## Plot Number of Companies by Funding Status
ylim_fundstatus <- c(0, 1.1*max(fund_status_count_company$Count_by_Fundstatus))
xx_fundstatus <- barplot(fund_status_count_company$Count_by_Fundstatus, xaxt = 'n', xlab = '', width = 0.85,
                  ylim = ylim_rev, main = "Number of Companies by Funding Status", 
                  ylab = "Frequency")
text(x = xx_fundstatus, y = fund_status_count_company$Count_by_Fundstatus, 
     label = fund_status_count_company$Count_by_Fundstatus, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_fundstatus, labels=fund_status_count_company$`Funding Status`, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 10.0 Count of Company by Last Funding Type (LFT)

        ## Summarize No. of Companies by LFT
lft_count_company <- all_company_WNA %>%
        dplyr::group_by(`Last Funding Type`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Lft = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE) 

        ## Percentage of All Companies by LFT
lft_count_company$Percentage <- as.numeric(format(lft_count_company$Count_by_Lft/
                                                                  sum(lft_count_company$Count_by_Lft), 
                                                          scientific = FALSE, digits = 2))

        ## Sort DF by Desc(Percentage)
lft_count_company <- arrange(lft_count_company, desc(Percentage))

        ## Drop Unnecessary
lft_count_company <- lft_count_company[1:28,]

        ## Plot Number of Companies by LFT
ylim_lft <- c(0, 1.1*max(lft_count_company$Count_by_Lft))
xx_lft <- barplot(lft_count_company$Count_by_Lft, xaxt = 'n', xlab = '', width = 0.85,
                         ylim = ylim_lft, main = "Number of Companies by Last Funding Type", 
                         ylab = "Frequency")
text(x = xx_lft, y = lft_count_company$Count_by_Lft, 
     label = lft_count_company$Count_by_Lft, pos = 3, cex = 0.4, col = "black")
axis(1, at=xx_lft, labels=lft_count_company$`Last Funding Type`, tick=FALSE, las=2, line=-0.5, cex.axis=0.5)






# 11.0 Count of Company by IPO Status (Ipos)

        ## Summarize No. of Companies by Ipos
ipos_count_company <- all_company_WNA %>%
        dplyr::group_by(`IPO Status`, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Ipos = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE) 

        ## Percentage of All Companies by Ipos
ipos_count_company$Percentage <- as.numeric(format(ipos_count_company$Count_by_Ipos/
                                                          sum(ipos_count_company$Count_by_Ipos), 
                                                  scientific = FALSE, digits = 2))

        ## Sort DF by Desc(Percentage)
ipos_count_company <- arrange(ipos_count_company, desc(Percentage))

        ## Drop Unnecessary
ipos_count_company <- ipos_count_company[1:3,]

        ## Write the Table
write.table(ipos_count_company, file = "/Users/araf03/Desktop/MS Thesis/Tables/IPO Status.txt",
            sep = ",")


