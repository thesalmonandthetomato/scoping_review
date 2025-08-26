# from https://koheiw.github.io/seededlda/articles/pkgdown/basic.html

library(seededlda)
library(synthesisr)
library(LDAvis)
library(quanteda)

rayyan <- read_refs("visualisations and analysis/provisional includes 21.12.24.ris")

# remove references from abstracts
rayyan$abstract <- sub(". References .*", ".", rayyan$abstract)

# create corpus
corp <- paste(rayyan$title, rayyan$abstract)

# add common words <3 letters long in corpus to stopword list
extra_stopwords <- c('jats', 'italic', 'italics', 'abstract', 'copyright', 'copyrightinformation',
                     'et al.', 'et al', 'research', 'article', 'conclusions', 'results', 'methodology',
                     'introduction', 'background', 'discussion', 'paper', 'chapter', 'science',
                     'part', 'report', 'terms', 'author', 'science', 'issue', 'volume', 'references',
                     'full-text', 'version', 'full text', 'papers', 'show', 'bold', 'style', 'list-item',
                     'abstracttext', 'label', 'styled-content', 'fixed-case', 'list',
                     'published', 'lō', 'h3', 'h2', 'h1', 'p22')

# run dummy model to identify candidate terms to remove in stoplist
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, 
               remove_numbers = TRUE, remove_url = TRUE)
stopword_list <- stopwords("en")
dfmt <- dfm(toks) |> 
  dfm_remove(stopword_list) |>
  dfm_remove("*@*") |>
  dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")
model <- textmodel_lda(dfmt, k = 20, verbose = TRUE)

# add to stopwords any words with fewer than 4 characters
terms <- list(terms(model, 1000000))
new_stopwords <- unlist(lapply(terms, function(x) x[nchar(x) %in% 0:3]))

# combine all stopwords
stopword_list <- c(stopwords("en"), new_stopwords, extra_stopwords)

# rerun dfmt removing all new stopwords
dfmt <- dfm(toks) |> 
  dfm_remove(stopword_list) |>
  dfm_remove("*@*") |>
  dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")
print(dfmt)

### standardised LDA
lda <- textmodel_lda(dfmt, k = 50, verbose = TRUE)
# topic terms
#knitr::kable(terms(lda))
# document topics
#dat <- docvars(lda$data)
#dat$topic <- topics(lda)
#knitr::kable(head(dat[,c("date", "topic", "head")], 10))
# create a json output
json <- createJSON(phi = lda$phi, 
                   theta = lda$theta, 
                   doc.length = rowSums(dfmt), 
                   vocab = colnames(dfmt), 
                   term.frequency = colSums(dfmt)
)
serVis(json)

## Seeded (semi-supervised) TM: https://koheiw.github.io/seededlda/articles/pkgdown/seeded.html
# load dictionary
dict <- dictionary(file = "dictionary.yml")
print(dict)

# seeded LDA
lda_seed <- textmodel_seededlda(dfmt, dict, batch_size = 0.01, auto_iter = TRUE,
                                verbose = TRUE)
knitr::kable(terms(lda_seed))

# write topic to RIS data column
topic <- topics(lda_seed)
rayyan$topic <- topic

## seeded LDA with residual topics
#lda_res <- textmodel_seededlda(dfmt, dict, residual = 2, batch_size = 0.01, auto_iter = TRUE,
#                               verbose = TRUE)
#knitr::kable(terms(lda_res))


## Visualise

# create a json output
json <- createJSON(phi = lda_seed$phi, 
                   theta = lda_seed$theta, 
                   doc.length = rowSums(dfmt), 
                   vocab = colnames(dfmt), 
                   term.frequency = colSums(dfmt)
)
serVis(json)

# topics by document
lda_seed$theta
topic_df <- data.frame(rayyanID=rayyan$accession_zr,
                       title=rayyan$title, 
                       abstract=rayyan$abstract)
topic_df <- cbind(topic_df, lda_seed$theta)

# verify species
S_salar <- topic_df[grep("salmo salar", tolower(topic_df$abstract)), ]
