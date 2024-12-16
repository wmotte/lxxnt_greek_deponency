#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
# Prepare for embedding; statistical processing.
#
# C.f., https://text2vec.org/glove.html
#
# C.f., https://medium.com/cmotions/nlp-with-r-part-2-training-word-embedding-models-and-visualize-results-ae444043e234
#
################################################################################
library( "text2vec" )

###
# Get data in single line, with proper verse separator: " @ @ @ @ @ @ @ @ @ @ "
##
get_data <- function()
{
    # input data
    #df <- readr::read_tsv( 'out.02.check.stuff/lxx_and_nt.tsv.gz', show_col_types = FALSE )
    df <- readr::read_tsv( 'out.02.prepare.and.stats/glove_input.tsv.gz', show_col_types = FALSE )
        
    # input to GloVe is a single line, but we do not want word counts to influence at boundaries
    # therefore, we concat with a dummy term in between
    list_separator <- paste0( " ", paste0( rep( "@", 10 ), collapse = ' ' ), " " )
    
    # get single string, separated by list_separator
    single_df <- paste( df$text, collapse = list_separator )
    
    return( single_df )
}

###
# Return tokens, it, and vectorizer and save token freq
##
get_preparation <- function( df, outdir, term_count_min )
{
    # Create iterator over tokens
    tokens <- space_tokenizer( df )
    
    # Create vocabulary. Terms will be unigrams (simple words).
    it <- itoken( tokens, progressbar = TRUE )
    vocab <- create_vocabulary( it )
    
    # prune to get rid of maximal "@"
    vocab <- prune_vocabulary( vocab, term_count_max = vocab[ vocab$term == '@', 'term_count' ] - 1 )
    
    # prune to remove all tokens with < 50 occurances
    vocab <- prune_vocabulary( vocab, term_count_min = term_count_min )

    # write to file
    readr::write_tsv( vocab, file = paste0( outdir, '/vocab_summary__', term_count_min, '.tsv' ), quote = 'all' )
        
    # Use our filtered vocabulary
    vectorizer <- vocab_vectorizer( vocab )
    
    output <- list( vocab = vocab, it = it, vectorizer = vectorizer )
    
    return( output )
}

################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.03.emb'
dir.create( outdir, showWarnings = FALSE )

# get single line data
df <- get_data()

# minimal frequency to be included
term_count_min <- 20

# vectorizer
preparation <- get_preparation( df, outdir, term_count_min )


nwindows <- c( 5 )

nwindow <- nwindows[ 1 ]

# loop over window of n context words
for( nwindow in nwindows )
{
    # term-co-occurrence matrix (TCM).
    tcm <- create_tcm( preparation$it, preparation$vectorizer, skip_grams_window = nwindow, skip_grams_window_context = "symmetric" )
    
    set.seed( 1234 + nwindow )
    
    # vector length
    rank <- 150
    
    # glove model
    glove <- GlobalVectors$new( rank = rank, x_max = 100 )
    wv_main <- glove$fit_transform( tcm, n_iter = 500, convergence_tol = 0.00001, n_threads = 4 )
    
    # get context matrix
    wv_context <- glove$components
    
    # 150 x 491
    dim( wv_context )
    
    # combine main embedding and context embedding (sum) into one matrix
    # 491 x 150
    dim( embedding <- wv_main + t( wv_context ) )
    
    # save files to disk
    save( preparation, tcm, glove, wv_main, wv_context, embedding, file = paste0( outdir, "/saved_glove__", nwindow, ".RData" ) )

    glove <- NULL
}

