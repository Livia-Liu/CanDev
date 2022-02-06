
# library 
library(tidyverse)
library(janitor)
library(ggplot2)

# read data 
raw <- read.csv("data.csv")

# basic cleaning 
clean <- raw %>% clean_names() %>% 
  drop_na(anscount) %>% 
  filter(surveyr == 2020) %>%  
  separate(bycond, c("condition", "type")) %>% 
  mutate(condition = str_remove(condition, "\\D$")) %>% 
  filter(!(condition == "Q121" & type == 2)) %>% 
  filter(!(condition == "Q119" & type == 2)) %>% 
  select(-c(level2id, level3id, level4id, level5id, 
            descrip_f, title_e, title_f, dept_f, indicatorfra, 
            subindicatorfra, surveyr)) %>%
  pivot_wider(names_from = condition, values_from = type)

# clean gender 
clean_gender <- clean %>%
  filter(str_detect(descrip_e, "gender") | str_detect(descrip_e, "Gender")) %>% 
  separate(descrip_e, c("gender", "default")) %>% 
  select(-c(default, Q115:Q122, demcode)) %>% 
  mutate(gender = str_replace_all(gender, "Gender", "gender diverse"))

gender_retention <- clean_gender %>% 
  filter(question == "Q53") %>%
  group_by(dept_e, gender, answer1, answer2, answer3,anscount, surveyr) %>%
  rename(c(Yes = answer1, No = answer2, Notsure = answer3))%>%
  summarize()

gender_agree <- clean_gender %>% 
  filter(subindicatoreng != "Mobility and Retention") %>% 
  group_by(dept_e, subindicatoreng,gender) %>% 
  summarize(mean = mean(agree))

gender_table <- left_join(gender_retention, gender_agree, 
                          by = c("dept_e", "gender")) %>%
  pivot_wider(names_from = subindicatoreng,
              values_from = mean)

write.csv(gender_table, 'gender.csv')


# clean disability 
clean_disability <- clean %>% 
  filter(demcode %in% c(2019, 2020)) %>% 
  rename(yes = answer1, no=answer2, notSure = answer3, 
         disability_status = descrip_e) 

disability_retention <- clean_disability %>% 
  filter(question == "Q53") %>% 
  group_by(dept_e, disability_status, yes, no, notSure, anscount) %>% 
  summarise()

disability_agree <- clean_disability %>% 
  filter(subindicatoreng != "Mobility and Retention") %>% 
  group_by(dept_e, Q118, subindicatoreng, disability_status) %>% 
  summarise(mean = mean(agree))

disability_tbl <- left_join(disability_retention, clean_disability, 
                          by = c("dept_e", "disability_status")) %>% 
  pivot_wider(names_from = subindicatoreng, values_from = mean)

write_csv(disability_tbl, "disability.csv")


# clean Indigenous
clean_disability <- clean %>%
  select(-c(Q117:Q122))%>%
  filter(demcode == 2014 | demcode == 2015) %>% 
  rename(c(yes = answer1, no=answer2, notSure = answer3, 
           indigenous = descrip_e))

indigenous_retention <- clean_disability %>%
  filter(question == "Q53") %>%
  group_by(dept_e, indigenous, yes, no, notSure, anscount, surveyr) %>%
  summarize()

indigenous_agree <- clean_disability %>%
  filter(subindicatoreng != "Mobility and Retention")
  group_by(dept_e, Q116, subindicatoreng, indigenous) %>% 
  summarize(mean = mean(agree))

indigenous_tbl <- left_join(indigenous_retention, clean_disability, 
                            by = c("dept_e", "indigenous")) %>%
  pivot_wider(names_from = subindicatoreng,
              values_from = mean)

write.csv(table, 'indigenous.csv')


# clean Minorities 
clean_minority <- clean %>% 
  filter(demcode %in% c(2043,2045,2047,2049,2051,2053,
                        2055,2057,2059,2061,2063)) %>% 
  rename(yes = answer1, no=answer2, notSure = answer3, 
         visible_minority = descrip_e) 

minority_retention <- clean_minority %>% 
  filter(question == "Q53") %>% 
  group_by(dept_e, visible_minority, yes, no, notSure, anscount)  %>% 
  summarise()

clean_minority <- clean_minority %>% 
  filter(subindicatoreng != "Mobility and Retention") %>% 
  group_by(dept_e, subindicatoreng, visible_minority) %>% 
  summarise(mean = mean(agree))

minority_tbl <- left_join(minority_retention, clean_minority, 
                          by = c("dept_e", "visible_minority")) %>% 
  pivot_wider(names_from = subindicatoreng, values_from = mean)

write_csv(minority_tbl, "mino_dept.csv")

# The intent of leaving group by minorities 
minority_leave <- cleaned %>% 
  filter(!is.na(Q121)) %>% 
  filter(question == "Q53") %>% 
  rename("minority" = descrip_e, 
         "yes" = answer1, 
         "no" = answer2, 
         "notSure" = answer3) %>% 
  select(dept_e, minority, yes, no, notSure, agree, 
         demcode, anscount) %>% 
  pivot_wider(names_from = minority, values_from = demcode) %>%
  clean_names() %>% 
  mutate(Black = case_when(is.na(black) ~ 0, TRUE ~ 1), 
         Chinese = case_when(is.na(chinese) ~ 0, TRUE ~ 1), 
         Filipino = case_when(is.na(filipino) ~ 0, TRUE ~ 1),
         Japanese = case_when(is.na(japanese) ~ 0, TRUE ~ 1),
         Korean = case_when(is.na(korean) ~ 0, TRUE ~ 1), 
         south_asian_east_indian = case_when(
           is.na(south_asian_east_indian_including_indian_from_india_bangladeshi_pakistani_east_indian_from_guyana_trinidad_east_africa_etc) ~ 0, 
           TRUE ~ 1), 
         southeast_asian = case_when(
           is.na(southeast_asian_including_burmese_cambodian_laotian_thai_vietnamese_etc) ~ 0, TRUE ~ 1), 
         nWwhite_WestAsianNorthAfrican_or_Arab = case_when(
           is.na(non_white_west_asian_north_african_or_arab_including_egyptian_libyan_lebanese_iranian_etc) ~ 0, 
           TRUE ~1), 
         nonWhite_latin_american = case_when(
           is.na(non_white_latin_american_including_indigenous_persons_from_central_and_south_america_etc) ~ 0, 
           TRUE ~1), 
         mixed_origin = case_when(
           is.na(person_of_mixed_origin_with_one_parent_in_one_of_the_visible_minority_groups) ~ 0, 
           TRUE ~ 1), 
         other = case_when(
           is.na(other_visible_minority_group) ~ 0, TRUE ~ 1)) %>% 
  select(-c(black, chinese, filipino, japanese, korean, 
            south_asian_east_indian_including_indian_from_india_bangladeshi_pakistani_east_indian_from_guyana_trinidad_east_africa_etc, 
            southeast_asian_including_burmese_cambodian_laotian_thai_vietnamese_etc, 
            non_white_west_asian_north_african_or_arab_including_egyptian_libyan_lebanese_iranian_etc, 
            non_white_latin_american_including_indigenous_persons_from_central_and_south_america_etc,
            person_of_mixed_origin_with_one_parent_in_one_of_the_visible_minority_groups,
            other_visible_minority_group))

write_csv(minority_leave, "minority_leave.csv")