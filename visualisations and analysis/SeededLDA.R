# from https://koheiw.github.io/seededlda/articles/pkgdown/basic.html

library(seededlda)

corp <- corpus

# add common words <3 letters long in corpus to stopword list
extra_stopwords <- c('jats', 'italic', 'italics', 'abstract', 'copyright', 'copyrightinformation',
                     'et al.', 'et al', 'research', 'article', 'conclusions', 'results', 'methodology',
                     'introduction', 'background', 'discussion', 'paper', 'chapter', 'science',
                     'part', 'report', 'terms', 'author', 'science', 'issue', 'volume', 'references',
                     'full-text', 'version', 'full text', 'papers', 'show', 'bold', 'style', 'list-item',
                     'abstracttext', 'label', 'styled-content', 'fixed-case', 'list',
                     'published')

toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, 
               remove_numbers = TRUE, remove_url = TRUE)
dfmt <- dfm(toks) |> 
  dfm_remove(stopword_list) |>
  dfm_remove("*@*") |>
  dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")
model <- textmodel_lda(dfmt, k = 20, verbose = TRUE)

# add to stopwords words with fewer than 4 characters
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
#lda <- textmodel_lda(dfmt, k = 20, verbose = TRUE)
# topic terms
#knitr::kable(terms(lda))
# document topics
#dat <- docvars(lda$data)
#dat$topic <- topics(lda)
#knitr::kable(head(dat[,c("date", "topic", "head")], 10))

## Seeded (semi-supervised) TM: https://koheiw.github.io/seededlda/articles/pkgdown/seeded.html
# load dictionary
dict <- dictionary(file = "dictionary.yml")
print(dict)

# seeded LDA
lda_seed <- textmodel_seededlda(dfmt, dict, batch_size = 0.01, auto_iter = TRUE,
                                verbose = TRUE)
knitr::kable(terms(lda_seed))

## seeded LDA with residual topics
#lda_res <- textmodel_seededlda(dfmt, dict, residual = 2, batch_size = 0.01, auto_iter = TRUE,
#                               verbose = TRUE)
#knitr::kable(terms(lda_res))


# visualise
library(pacman)
p_load("seededlda",
       "quanteda",
       "LDAvis")

json <- createJSON(phi = lda_seed$phi, 
                   theta = lda_seed$theta, 
                   doc.length = rowSums(dfmt), 
                   vocab = colnames(dfmt), 
                   term.frequency = colSums(dfmt)
)
serVis(json)
