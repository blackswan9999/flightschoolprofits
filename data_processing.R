library(lubridate)

# clone data frame, changing exercise column from string to list, adding date column
my_cd <- clean_data %>% 
  distinct( Session_ID, .keep_all = T) %>% 
  mutate(Exercises = str_split(Exercises, ",")) %>%
  mutate(Date = make_date(year = Year, month = Month, day = Day))

# fixing typo in aircraft name
my_cd$Aircraft[my_cd$Aircraft == "C152"] <- "C-152"

# removing years before 2018
my_cd <- my_cd %>%
  filter(Year >= 2018)

# fixing typo in month
my_cd$Month[my_cd$Month == 111] <- 11
my_cd <- my_cd %>%
  mutate(Date = replace(Date, Month == 111, make_date(2019, 11, 17))) %>%
  mutate(Date = replace(Date, Date == make_date(2019, 1, 17), make_date(2019, 11, 17))) %>%
  mutate(Month = replace(Month, Date == make_date(2019, 11, 17), 11))

# removing rows with NA in Aircraft
my_cd <- my_cd %>%
  filter(!(is.na(Aircraft)))

# -------------CLEANING UP EXERCISES --------------

# fixing empty string cases
my_cd$Exercises <- lapply(my_cd$Exercises, FUN = stri_remove_empty)

# fixing straggler cases
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "34"))]] <- c("1", "2", "3", "4", "5")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "`7"))]] <- c("15", "16", "17", "18", "30")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "89"))]] <- c("1", "2", "3", "4", "5", "6", "7", "8" ,"9", "16", "17", "18", "30")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "1617"))]] <- c("9", "11", "12", "14", "16", "17", "18", "30")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "81"))]] <- c("16", "17", "18", "24", "24", "29", "30")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "18.30"))]] <- c("4", "5", "7", "8", "9", "12", "13", "14", "15", "16", "18" ,"30")
a <- c("11", "12", "13", "16", "17", "18", "30")
b <- c("12", "13", "16", "17", "18")
my_cd$Exercises[which(str_detect(my_cd$Exercises, "1316"))] <- list(a, b, b)
rm(a)
rm(b)
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "2.4"))]] <- c("2", "4", "5", "6")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "112"))]] <- c("9", "11", "12", "16", "17", "18")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "55"))]] <- c("3", "4", "5", "6", "7", "8", "9", "30")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "1314"))]] <- c("12", "13", "14", "16", "17", "18", "30")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "1821"))]] <- c("16", "17", "18", "21", "22", "23", "24")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, " 16 17"))]] <- c("13", "14", "16","17", "18")
my_cd$Exercises[[which(str_detect(my_cd$Exercises, "67"))]] <- c("4", "6", "7", "8", "9", "16", "18", "30")

# wasn't sure, removed this one
my_cd <- my_cd %>%
  filter(!(str_detect(Exercises, "40")))

exercise_count <- my_cd %>% summarise(Exercise = flatten(Exercises)) %>% count(Exercise) %>% arrange(as.numeric(Exercise))


