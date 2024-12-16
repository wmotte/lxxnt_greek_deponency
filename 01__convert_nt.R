#!/usr/bin/env Rscript
#
# Process morph of NT - add deponent parsing
#
# Wim Otte (w.m.otte@umcutrecht.nl)
#
################################################################################
library( 'plyr' )
library( 'stringr' )

###
# Get nt books
##
get_nt_books <- function( indir )
{
    # 27 books
    length( books <- dir( indir, "*.txt.gz" ) )
    return( books )
}

###
# Add deponent label
#
# Mounce: 4th edition: 18.16 Lexical form. 
# Regardless of your teacher’s decision, recognition is the same. 
#
# You can tell if a verb is middle-only/deponent by its lexical form. The
# lexical form of a middle-only verb ends in ομαι (e.g., ἔρχομαι).
##
add_deponent <- function( df )
{
    # change 'voice' to 'deponent' if lemma ends with 'omai'
    
    # Mounce: p. 188: The lexical form of a middle-only verb ends in ομαι (e.g., ἔρχομαι).
    # use more liberal version, as not all lemmas contain 'ο'
    marker <- 'μαι$'
    deponents <- unique( df[ stringr::str_detect( df$lemma, marker ), 'lemma' ] )
    
    # change deponent
    df[ df$lemma %in% deponents, 'voice' ] <- 'deponent'
    
    return( df )
}

###
# Pre-process
##
process <- function( infile, book )
{
    bookname <- gsub( '-morphgnt.txt.gz', '', book )
    
    # read input
    df <- read.table( infile, sep = ' ' )
    df$bookname <- bookname
    
    # set length to 6
    df$V1 <- sprintf( "%06d", df$V1 )
    
    # convert chapter/verse to separate columns
    df$chapter <- as.integer( substr( df$V1, 3, 4 ) )
    df$verse <- as.integer( substr( df$V1, 5, 6 ) )
    
    # to prevent resorting of chapters and verses incorrectly (after re-reading)
    df$chapter <- stringr::str_pad( df$chapter, 3, 'left', pad = '0' )
    df$verse <- stringr::str_pad( df$verse, 3, 'left', pad = '0' )
    
    # normalized word
    df$word <- df$V6
    df$word <- gsub( "\\(", "", df$word )
    df$word <- gsub( "\\)", "", df$word )
    
    # lemma
    df$lemma <- df$V7
    
    # morphology
    df$morphology <- paste( df$V2, df$V3 )
    
    ## 1. Part-of-Speech
    df$V2 <- gsub( 'A-', 'adjective', df$V2 )
    df$V2 <- gsub( 'C-', 'conjunction', df$V2 )
    df$V2 <- gsub( 'D-', 'adverb', df$V2 )
    df$V2 <- gsub( 'I-', 'interjection', df$V2 )
    df$V2 <- gsub( 'N-', 'noun', df$V2 )
    df$V2 <- gsub( 'P-', 'preposition', df$V2 )
    df$V2 <- gsub( 'RA', 'def_article', df$V2 )
    df$V2 <- gsub( 'RD', 'demon_pronoun', df$V2 )
    df$V2 <- gsub( 'RI', 'interr_indef_pronoun', df$V2 )
    df$V2 <- gsub( 'RP', 'person_pronoun', df$V2 )
    df$V2 <- gsub( 'RR', 'relat_pronoun', df$V2 )
    df$V2 <- gsub( 'V-', 'verb', df$V2 )
    df$V2 <- gsub( 'X-', 'particle', df$V2 )
    df$V2 <- gsub( 'M-', 'indecl_number', df$V2 )
    
    df$part_of_speech <- df$V2
    
    df$V1 <- df$V2 <- df$V4 <- df$V5 <- df$V6 <- df$V7 <- NULL
    
    # 2. Parsing Code
    df$V3 <- as.character( df$V3 )

    df$degree <- substr( df$V3, 8, 8 )
    df$voice <- substr( df$V3, 3, 3 )
    df$tense <- substr( df$V3, 2, 2 )
    df$mood <- substr( df$V3, 4, 4 )
    df$person <- substr( df$V3, 1, 1 )
    df$case <- substr( df$V3, 5, 5 )
    df$number <- substr( df$V3, 6, 6 )
    df$gender <- substr( df$V3, 7, 7 )

    df$V3 <- NULL
    
    # tense (P=present, I=imperfect, F=future, A=aorist, X=perfect, Y=pluperfect)
    df[ df$tense == 'P', 'tense' ] <- 'present'
    df[ df$tense == 'I', 'tense' ] <- 'imperfect'
    df[ df$tense == 'F', 'tense' ] <- 'future'
    df[ df$tense == 'A', 'tense' ] <- 'aorist'
    df[ df$tense == 'X', 'tense' ] <- 'perfect'
    df[ df$tense == 'Y', 'tense' ] <- 'pluperfect'
    summary( as.factor( df$tense ) )
    
    # voice (A=active, M=middle, P=passive)
    df[ df$voice == 'A', 'voice' ] <- 'active'
    df[ df$voice == 'M', 'voice' ] <- 'middle'
    df[ df$voice == 'P', 'voice' ] <- 'passive'
    summary( as.factor( df$voice ) )
    
    # mood (I=indicative, D=imperative, S=subjunctive, O=optative, N=infinitive, P=participle)
    df[ df$mood == 'I', 'mood' ] <- 'indicative'
    df[ df$mood == 'D', 'mood' ] <- 'imperative'
    df[ df$mood == 'S', 'mood' ] <- 'subjunctive'
    df[ df$mood == 'O', 'mood' ] <- 'optative'
    df[ df$mood == 'N', 'mood' ] <- 'infinitive'
    df[ df$mood == 'P', 'mood' ] <- 'participle'
    summary( as.factor( df$mood ) )
    
    # case (N=nominative, G=genitive, D=dative, A=accusative,V=vocative)
    df[ df$case == 'N', 'case' ] <- 'nominative'
    df[ df$case == 'G', 'case' ] <- 'genitive'
    df[ df$case == 'D', 'case' ] <- 'dative'
    df[ df$case == 'A', 'case' ] <- 'accusative'
    df[ df$case == 'V', 'case' ] <- 'vocative'
    summary( as.factor( df$case ) )
    
    # number (S=singular, P=plural)
    df[ df$number == 'S', 'number' ] <- 'singular'
    df[ df$number == 'P', 'number' ] <- 'plural'
    summary( as.factor( df$number ) )
    
    # gender (M=masculine, F=feminine, N=neuter)
    df[ df$gender == 'M', 'gender' ] <- 'masculine'
    df[ df$gender == 'F', 'gender' ] <- 'feminine'
    df[ df$gender == 'N', 'gender' ] <- 'neuter'
    summary( as.factor( df$gender ) )
    
    # degree (C=comparative, S=superlative)
    df[ df$degree == 'C', 'degree' ] <- 'comparative'
    df[ df$degree == 'S', 'degree' ] <- 'superlative'
    summary( as.factor( df$degree ) )
    
    # person
    df[ df$person == '1', 'person' ] <- '1st'
    df[ df$person == '2', 'person' ] <- '2nd'
    df[ df$person == '3', 'person' ] <- '3rd'
    summary( as.factor( df$person ) )
    
    # replace '-'
    df[ df$degree == '-', 'degree' ] <- ''
    df[ df$voice == '-', 'voice' ] <- ''
    df[ df$tense == '-', 'tense' ] <- ''
    df[ df$mood == '-', 'mood' ] <- ''
    df[ df$person == '-', 'person' ] <- ''
    df[ df$case == '-', 'case' ] <- ''
    df[ df$number == '-', 'number' ] <- ''
    df[ df$gender == '-', 'gender' ] <- ''
    
    # add deponent
    df <- add_deponent( df )
    
    df$single <- paste0( df$part_of_speech, "] ", paste( df$tense, df$voice, df$mood, df$person, df$number, df$gender, df$case ) )
    df$single <- gsub( ' ', '_', stringr::str_squish( df$single ) )
    df$single <- gsub( ']_', '___', df$single )    
    df$single <- gsub( ']$', '', df$single )
    
    return( df )
}

################################################################################
# END CUSTOM FUNCTIONS
################################################################################

indir <- 'data/nt'
outdir <- 'out.01.convert.nt'
dir.create( outdir, showWarnings = FALSE )

books <- get_nt_books( indir )

book <- books[ 1 ]

for( book in books )
{
    print( book )
    infile <- paste0( indir, '/', book )
    
    # preprocess morphology
    df <- process( infile, book )

    # for output
    bookname <- df$bookname[ 1 ]
    
    # save morphology to file
    write.csv( df, file = paste0( outdir, '/', bookname, '__metainfo.csv' ), quote = TRUE )
    
    # compile book with normalized text and lemma text
    df_book_normalized <- ddply( df, c( "chapter", "verse" ), summarise, 
                        text = paste( word, collapse = " " ) )
    df_book_normalized$book <- bookname
    df_book_normalized <- df_book_normalized[ , c( 'book', 'chapter', 'verse', 'text' ) ]
    
    # compile book with lemma text
    df_book_lemmatized <- ddply( df, c( "chapter", "verse" ), summarise,
                                 text = paste( lemma, collapse = " " ) )
    df_book_lemmatized$book <- bookname
    df_book_lemmatized <- df_book_lemmatized[ , c( 'book', 'chapter', 'verse', 'text' ) ]
    
    # compile book with morphology text
    df_book_morph <- ddply( df, c( "chapter", "verse" ), summarise,
                                 text = paste( single, collapse = " " ) )
    df_book_morph$book <- bookname 
    df_book_morph <- df_book_morph[ , c( 'book', 'chapter', 'verse', 'text' ) ]
    
    # save book file
    write.csv( df_book_normalized, file = paste0( outdir, '/', bookname, '__normalized.csv' ), quote = TRUE )
    write.csv( df_book_lemmatized, file = paste0( outdir, '/', bookname, '__lemmatized.csv' ), quote = TRUE )
    write.csv( df_book_morph, file = paste0( outdir, '/', bookname, '__morphology.csv' ), quote = TRUE )           
}
