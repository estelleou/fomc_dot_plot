library(tidyverse)

raw_data <-
  readxl::read_xlsx("R/grid1_yw2kfuqk.xlsx")

new_data <-
  readxl::read_xlsx("R/new_data.xlsx")

referenc_data <-
  raw_data %>% 
  pivot_longer(-Target) %>% 
  filter(!is.na(value)) %>% 
  filter(name != "2021")

new_reference_data <-
  new_data %>% 
  pivot_longer(-Target) %>% 
  filter(!is.na(value)) %>% 
  filter(name != "2021")

#code for cleaning data into observations
dup_data <- list()
for (n in 1:nrow(referenc_data)) {  
  
  data <-
    referenc_data %>% 
    slice(rep(n:n, each = referenc_data$value[n]))
  
  dup_data <- bind_rows(dup_data, data)   
  
}

#code for cleaning new data into observations
new_dup_data <- list()
for (n in 1:nrow(new_reference_data)) {  
  
  new_data <-
    new_reference_data %>% 
    slice(rep(n:n, each = new_reference_data$value[n]))
  
  new_dup_data <- bind_rows(new_dup_data, new_data)   
  
}


ggplot()+
  geom_dotplot(data =  dup_data %>% 
                 select(-value) %>% 
                 mutate(name = ifelse(name == "Longer Term", "2025", name)) %>% 
                 mutate(name = as.Date(name, "%Y")) %>% 
                 mutate(name = name-530), 
               aes(x= name, y = Target, group = name), 
               binaxis = "y", stackdir = "center", dotsize = 0.4, fill = "grey", color = "grey") +
  geom_dotplot(data =  new_dup_data %>% 
                 select(-value) %>% 
                 mutate(name = ifelse(name == "Longer Term", "2025", name)) %>%  
                 mutate(name = as.Date(name, "%Y")) %>% 
                 mutate(name = name-340), 
               aes(x= name, y = Target, group = name), 
               binaxis = "y", stackdir = "center", dotsize = 0.4,  fill = "red", color = "red") +
  labs(x = "Projection Year End", y = "Implied Fed Funds Target Rate", 
       title = "December 2021 (grey) vs. March 2022 (red) FOMC Dot Plot Projections \n(midpoint of target range for fed funds rate)") +
  scale_x_date( date_break = "1 year", date_label = c("Longer Term", "2022", "2023", "2024")) +
  scale_y_continuous(breaks=seq(0,4.0, 0.50))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(face = "bold"))
ggsave("dot_plot.png")
