# Setup ====

# Load packages
library(dplyr)
library(extrafont)
library(ggplot2)
library(readxl)
library(tm)
library(topicmodels)

# Load data ====

df = list()

for (file in list.files("data/")) {
  name = strsplit(file,".", fixed = T)[[1]][1]
  df[[name]] = read_xlsx(path = paste("data/", file, sep = ""), sheet = "COMBINE")
  df[[name]] = df[[name]][,c("AGENCY", "DESC", "NAICS", "SUBJECT")]
}

remove(file, name)

# Total rows and percentage ====

t15 = df$`FBO Report 2015` %>%
  group_by(AGENCY) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(percentage = round(n/sum(n),2))

t16 = df$`FBO Report 2016` %>%
  group_by(AGENCY) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(percentage = round(n/sum(n),2))

t17 = df$`FBO Report 2017` %>%
  group_by(AGENCY) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(percentage = round(n/sum(n),2))

t18 = df$`FBO Report 2018` %>%
  group_by(AGENCY) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(percentage = round(n/sum(n),2))


# Convert to a corpus ====

all_docs = c(df$`FBO Report 2015`$SUBJECT, df$`FBO Report 2016`$SUBJECT, df$`FBO Report 2017`$SUBJECT, df$`FBO Report 2018`$SUBJECT)

# Save agencies and subjects to pair with the cleaned documents later on
agencies = c(df$`FBO Report 2015`$AGENCY, df$`FBO Report 2016`$AGENCY, df$`FBO Report 2017`$AGENCY, df$`FBO Report 2018`$AGENCY)
subjects = c(df$`FBO Report 2015`$SUBJECT, df$`FBO Report 2016`$SUBJECT, df$`FBO Report 2017`$SUBJECT, df$`FBO Report 2018`$SUBJECT)

# Create corpus
docs = Corpus(VectorSource(all_docs))

# Remove unnecessary objects
remove(all_docs)

# Convert to lowercase and remove special characters, punctuation, digits, stopwords, and whitespace ====

# Convert to lowercase
docs = tm_map(docs,content_transformer(tolower))

# Remove special characters
toSpace = content_transformer(function(x, pattern) { return (gsub(pattern, "", x))})
docs = tm_map(docs, toSpace, "-")
docs = tm_map(docs, toSpace, "'")
docs = tm_map(docs, toSpace, ",")
docs = tm_map(docs, toSpace, "/")

# Remove punctuation
docs = tm_map(docs, removePunctuation)

# Remove digits
docs = tm_map(docs, removeNumbers)

# Remove stopwords
docs = tm_map(docs, removeWords, stopwords("english"))

# Remove whitespace
docs = tm_map(docs, stripWhitespace)

# Stem document ====

# Stem document
docs = tm_map(docs,stemDocument)

# Print elements to see ====

# Printing 50 random elements to see if I missed any special characters
for (i in sample(x = 1:length(docs), size = 50, replace = F)){
  print(writeLines(as.character(docs[[i]])))
}

# Create document term matrix ====

# Create document-term matrix
dtm = DocumentTermMatrix(docs)

# Save agency-document and subject-document pairs
dtm_agencies = dtm
dtm_subjects = dtm
rownames(dtm_agencies) = agencies
rownames(dtm_subjects) = subjects

# Set parameters for Gibbs sampling in LDA ====

# Set parameters for Gibbs sampling
burnin = 4000
iter = 2000
thin = 500
seed = list(2003,5,63,100001,765)
nstart = 5
best = TRUE

# Set number of topics parameter ====

k = 5

# Run LDA ====

#Run LDA
ui = unique(dtm$i)
dtm.new = dtm[ui,]

ldaOut = LDA(dtm.new,
             k, 
             method = "Gibbs",
             control = list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin))

# LDA Outputs ====

# Get document-topic classification
ldaOut_topics = as.matrix(topics(ldaOut))

# Get top 10 terms of each tpic
ldaOut_terms = as.matrix(terms(ldaOut,10))

# Get probabilities associated with each topic assignment
topic_Probabilities = as.data.frame(ldaOut@gamma)

# Find the number and percentage of solicitation subjects classified into each topic by agency ====

# Get agency-subject topic pair
agencies_df = data.frame(agency = agencies[as.integer(rownames(ldaOut_topics))], topic = ldaOut_topics)

# Count the number of subjects classified into each topic for each agency
agency_number_by_topic = agencies_df %>%
  group_by(agency, topic) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(agency, desc(n))

# Get percentage of solicitations categorized by topic for each agency
agency_number_percentage_by_topic = agencies_df %>%
  group_by(agency) %>%
  summarise(Total = n()) %>%
  ungroup() %>%
  right_join(y = agency_number_by_topic, by = "agency") %>%
  mutate(Percentage = round(n/Total,2)) %>%
  arrange(agency, desc(Percentage))

# Set factor and give topics a name
agency_number_percentage_by_topic$agency = factor(agency_number_percentage_by_topic$agency, levels = (sort(unique(agency_number_percentage_by_topic$agency))))
agency_number_percentage_by_topic$topic2 = sapply(X = agency_number_percentage_by_topic$topic, FUN = function(x){
  
  if (x == 1){
    return("Supplies")
  } else if (x == 2){
    return("Services & Support")
  } else if (x == 3){
    return("Installation & Electronics")
  } else if (x == 4){
    return("Assembly & Parts")
  } else if (x == 5){
    return("Software & System Maintenance")
  }
  
})

# Split full data frame into two to make it easier for plotting
agency_number_percentage_by_topic_1 = subset(agency_number_percentage_by_topic, agency_number_percentage_by_topic$agency %in% sort(unique(as.character(agency_number_percentage_by_topic$agency)))[1:round(length(sort(unique(agency_number_percentage_by_topic$agency)))/2)])
agency_number_percentage_by_topic_2 = subset(agency_number_percentage_by_topic, agency_number_percentage_by_topic$agency %in% sort(unique(as.character(agency_number_percentage_by_topic$agency)))[(round(length(sort(unique(agency_number_percentage_by_topic$agency)))/2) + 1):length(sort(unique(agency_number_percentage_by_topic$agency)))])
agency_number_percentage_by_topic_1$agency = factor(agency_number_percentage_by_topic_1$agency, levels = sort(unique(agency_number_percentage_by_topic_1$agency)))
agency_number_percentage_by_topic_2$agency = factor(agency_number_percentage_by_topic_2$agency, levels = sort(unique(agency_number_percentage_by_topic_2$agency)))

# Plot ====

# Plot 1
g1 = ggplot(agency_number_percentage_by_topic_1, aes(topic2, y = agency)) + 
  geom_tile(aes(fill = Percentage), colour = "white") + 
  scale_fill_gradient(low = "white", 
                      high = "steelblue",
                      name = "Percentage: ",
                      guide = guide_colorbar(label = TRUE,
                                             draw.ulim = TRUE, 
                                             draw.llim = TRUE,
                                             frame.colour = "black",
                                             ticks = TRUE, 
                                             nbin = 10,
                                             label.position = "bottom",
                                             barwidth = 13,
                                             barheight = 1.3, 
                                             direction = "horizontal",
                                             title.vjust = 0.8)) +
  labs(y = "Agency",
       x = "Topic",
       title = "Most Common Topic of FedBizOpps Solicitation Subject by Agency",
       subtitle = "Solicitations posted on FedBizOpps from Jan-2015 to Sep-2018") + 
  theme(text = element_text(family = "Cambria"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"))

# Plot 2
g2 = ggplot(agency_number_percentage_by_topic_2, aes(topic2, y = agency)) + 
  geom_tile(aes(fill = Percentage), colour = "white") + 
  scale_fill_gradient(low = "white", 
                      high = "steelblue",
                      name = "Percentage: ",
                      guide = guide_colorbar(label = TRUE,
                                             draw.ulim = TRUE, 
                                             draw.llim = TRUE,
                                             frame.colour = "black",
                                             ticks = TRUE, 
                                             nbin = 10,
                                             label.position = "bottom",
                                             barwidth = 13,
                                             barheight = 1.3, 
                                             direction = "horizontal",
                                             title.vjust = 0.8)) +
  labs(y = "Agency",
       x = "Topic",
       title = "Most Common Topic of FedBizOpps Soliciation Subject by Agency",
       subtitle = "Solicitations posted on FedBizOpps from Jan-2015 to Sep-2018") + 
  theme(text = element_text(family = "Cambria"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"))

# Save plots ====

# Save plot 1
png(file = "figs/g1.png", 
    width = 1000,
    height = 900)
print(g1)
dev.off()

# Save plot 2
png(file = "figs/g2.png", 
    width = 1000,
    height = 900)
print(g2)
dev.off()







