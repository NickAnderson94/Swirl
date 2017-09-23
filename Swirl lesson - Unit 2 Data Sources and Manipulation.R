##############################################################################################################
############################## R Script from Swirl  ##########################################################
###############################      HUDK 4050     ###########################################################
#-------------------------------------------------------------------------------------------------------------

library(swirl)
swirl()



# 1 Matricies and data frame -------------------------------------------------------------------------------


# 2 Manipulating data with dplyr --------------------------------------------------------------------------
#dplyr is home to select, filter, arrange, summarize, and mutate
#use select to select columns or varaiables
select(cran,ip_id, package, country)
select(cran, r_arch:country)
select(cran, country:r_arch)
select(cran, -time)
select(cran, -(X:size))

#use filter to select rows of data according to some criteria
filter(cran, package == "swirl")
filter(cran, r_version == "3.1.1", country == "US")
filter(cran, country == "IN", r_version <= "3.0.2")
filter(cran,country == "US" | country == "IN")
filter(cran, size > 100500, r_os == "linux-gnu")
filter(cran, !is.na(r_version))

#use arrange to sort the data
cran2 <- select(cran, size:ip_id)
arrange(cran2, ip_id)
arrange(cran2, desc(ip_id))
arrange(cran2, package, ip_id)
arrange(cran2, country, desc(r_version), ip_id)

#use mutate to add varaibles in a new column to dataset


# 3 Tidying data with tidyr -------------------------------------------------------------------------------
select(cran,ip_id, package, country)
select(cran, r_arch:country)
select(cran, country:r_arch)
cran
select(cran, -time)
select(cran, -(X:size))
filter(cran, r_version == "3.1.1", country == "US")
filter(cran, country == "IN", r_version <= "3.0.2")
filter(cran,country == "US" | country == "IN")
filter(cran, size > 100500, r_os == "linux-gnu")
filter(cran, !is.na(r_version))
cran2 <- select(cran, size:ip_id)
arrange(cran2, ip_id)
arrange(cran2, desc(ip_id))
arrange(cran2, package, ip_id)
arrange(cran2, country, desc(r_version), ip_id)
cran3 <- select(cran, ip_id, package, size)
cran3
mutate(cran3, size_mb = size / 2^20)
mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10)
mutate(cran3, correct_size = size + 1000)
summarize(cran, avg_bytes = mean(size))


#gather function
gather(students, sex, count, -grade)

#use the pipe to gather and separate data 
students2 %>%
  gather(key = sex_class ,value = count, -grade) %>%
  separate(col = sex_class , c("sex", "class")) %>%
  print

students3 %>%
  gather(key = class ,value = grade , class1:class5 ,na.rm = TRUE) %>%
  print

#gather grades from each class into single column and then add new column for midterm and finals
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(key = test, value = grade ) %>%
  print

#gather grades from each class into single column and then add new column for midterm and finals and remove "class" string
students3 %>%
  gather(class, grade, class1:class5, na.rm = TRUE) %>%
  spread(test, grade) %>%
  mutate(class = parse_number(class)) %>%
  print

#students in multiple rows
#step 1: get student id info and filter out unique values
student_info <- students4 %>%
  select(id, name, sex) %>%
  unique() %>%
  print

#step 2: get grades for each student with unique identifier
gradebook <- students4 %>%
  select(id, class, midterm, final) %>%
  print

##combing data from two tables into one
passed <- passed %>% mutate(status = "passed")
failed <- failed %>% mutate(status = "failed")
bind_rows(passed, failed)

#make the sat data tidy
sat %>%
  select(-contains("total")) %>%
  gather(part_sex, count, -score_range) %>%
  separate(part_sex, c("part", "sex")) %>%
  group_by(part, sex) %>%
  mutate(total = sum(count),
         prop = count / total
  ) %>% print



# 4 grouping and chaining with dplyr ----------------------------------------------------------------------
summarize(by_package, mean = mean(size))

pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
pack_sum

#look at top 1% - In statistics, this is called the 0.99, or 99%, sample quantile. 
#Use quantile(pack_sum$count, probs = 0.99) to determine this number.
quantile(pack_sum$count, probs = 0.99)

top_counts <- filter(pack_sum, count > 679)
top_counts <- filter(pack_sum, count > 679)
top_counts
View(top_counts)
top_counts_sorted <- arrange(top_counts, desc(count))
View(top_counts_sorted)
quantile(pack_sum$unique, probs = 0.99)
top_unique <- filter(pack_sum, unique > 465)
View(top_unique)
top_unique_sorted <- arrange(top_unique, desc(unique))
View(top_unique_sorted)


result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(),
            unique = n_distinct(ip_id),
            countries = n_distinct(country),
            avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)

# Print result to console
print(result3)

# 5 Looking at data ---------------------------------------------------------------------------------------
ls()
class(plants)
dim(plants)
nrow(plants)
ncol(plants)
object.size(plants)
names(plants)
head(plants)
head(plants, 10)
tail(plants, 15)
summary(plants)
table(plants$Active_Growth_Period)
str(plants)


# 6 Data Visualzation -------------------------------------------------------------------------------------
#visualzations for continuous variables
#this reviewed the dot plot, histograms, stem and leaf plots, and boxplots or box and whisker plot 


# 7 Plotting Systems --------------------------------------------------------------------------------------




# 8 Base Graphics -----------------------------------------------------------------------------------------


