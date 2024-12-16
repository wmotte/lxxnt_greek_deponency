#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
# 
# Process morph of LXX and NT
#
################################################################################
library( 'plyr' )
library( 'stringr' )

###
# Get lxx books
##
get_lxx_books <- function( indir )
{
    # 59 books
    length( books <- dir( indir, "*.txt.gz" ) )
    return( books )
}

###
# Add deponent label
# Mounce: 4th edition: 18.16 Lexical form. 
# Regardless of your teacher’s decision, recognition is the same. 
#
# You can tell if a verb is middle-only/deponent by its lexical form. The
# lexical form of a middle-only verb ends in ομαι (e.g., ἔρχομαι).
##
add_deponent <- function( df )
{
    # change 'voice' from 'middle' to 'deponent' if lemma ends with 'omai' AND voice == 'middle'
    
    # Mounce: p. 188: The lexical form of a middle-only verb ends in ομαι (e.g., ἔρχομαι).
    #marker <- 'ομαι$'
    
    # more liberal identification
    marker <- 'μαι$'
    
    #deponents <- unique( df[ stringr::str_detect( df$lemma, marker ) & df$voice == 'active', 'lemma' ] )
    deponents <- unique( df[ stringr::str_detect( df$lemma, marker ), 'lemma' ] )
        
    # change deponent
    #df[ df$lemma %in% deponents & df$voice == 'middle', 'voice' ] <- 'deponent'
    df[ df$lemma %in% deponents, 'voice' ] <- 'deponent'
    
    return( df )
}

###
# Pre-process
##
pre_process <- function( infile )
{
    if( !file.exists( infile ) )
        stop( "*** ERROR ***: input file does not exist!" )
    
    # read as V1 column
    head( df <- read.table( infile, quote = "", row.names = NULL, stringsAsFactors = FALSE, sep = '|' ) )
    
    # initiate variables
    bookname <- NA
    chapter <- NA
    verse <- NA
    
    df$bookname <- NA
    df$chapter <- NA
    df$verse <- NA
    df$remove <- 0
    
    i <- 1
    
    # loop over all rows and tag heading as 'remove' == 1
    for( i in 1:nrow( df ) )
    {	
        s <- df$V1[ i ]
        
        # if string contains '-' it is regular
        if( !str_detect( s, "-" ) )
        {
            df$remove[ i ] <- 1
            
            if( str_detect( s, ':' ) )
            {
                m <- str_split( s, " ", simplify = TRUE )
                m2 <- str_split( m[[ 2 ]], ":", simplify = TRUE )
                bookname <- gsub( "/", "-", m[[ 1 ]] ) # required to handle '1Sam/K', etc.
                chapter <- m2[[ 1 ]]
                verse <- m2[[ 2 ]]
                
            } else { # Susanna books, with no ':' (n=2)
                m <- str_split( s, " ", simplify = TRUE )
                bookname <- gsub( "/", "-", m[[ 1 ]] ) # required to handle '1Sam/K', etc.
                chapter <- 1
                verse <- gsub( "/", "-", m[[ 2 ]] )
            }
        } # end of parsing header
        
        # add markers
        df$bookname[ i ] <- bookname
        df$chapter[ i ] <- chapter
        df$verse[ i ] <- verse
    } # // end of row loop
    
    # remove headers
    df <- df[ df$remove != 1, ]
    df$remove <- NULL
    
    # create columns with 'word', 'lemma' and 'morphology'
    m <- data.frame( str_split( df$V1, " ", simplify = TRUE ) )
    df$word <- m$X1
    df$lemma <- paste0( m$X4, m$X3 )
    df$morphology <- m$X2
    df$V1 <- NULL
    
    return( df )
}

###
# Process df
##
process_df <- function( df )
{
    
    # add identifier to sort subdatasets in the end
    df$whereabout <- 1:nrow( df )
    
    # to prevent resorting of chapters and verses incorrectly (after re-reading)
    df$verse <- stringr::str_pad( df$verse, 3, 'left', pad = '0' )
    df$chapter <- stringr::str_pad( df$chapter, 3, 'left', pad = '0' )
    
    # correct errors in LXX source 
    df[ df$bookname == '1Esdr' & df$chapter == '006' & df$verse == '004' & df$morphology == 'RA+A------', 'morphology' ] <-  "RA--NPN---"
    df[ df$bookname == 'Deut' & df$chapter == '022' & df$verse == '006' & df$morphology == 'N1A--DSF--', 'morphology' ] <- 'N1A-DSF---'
    df[ df$bookname == 'Isa' & df$chapter == '014' & df$verse == '002' & df$morphology == 'VF-FAI3P--', 'morphology' ] <- 'VF--FAI3P-'
    df[ df$bookname == '2Mac' & df$chapter == '008' & df$verse == '011' & df$morphology == 'V2--PMPNDM', 'morphology' ] <- 'V2--PMPNSM'
    df[ df$bookname == '2-4Kgs' & df$chapter == '012' & df$verse == '008' & df$morphology == 'N---AAN---', 'morphology' ] <- 'N---ASN---'
    df[ df$bookname == 'Deut' & df$morphology == 'N---G/D---', 'morphology' ] <- 'N---GS----'
    df[ stringr::str_detect( df$morphology, '-B-' ) & stringr::str_detect( df$morphology, '^A3U' ) & stringr::str_detect( df$lemma, 'ς$' ), 'morphology' ] <- "A3H-------"
    df[ stringr::str_detect( df$morphology, '-B-' ) & stringr::str_detect( df$morphology, '^A1A' ) & stringr::str_detect( df$lemma, 'ς$' ), 'morphology' ] <- "A1A-------"
    df[ stringr::str_detect( df$morphology, '-B-' ) & stringr::str_detect( df$morphology, '^A3H' ) & stringr::str_detect( df$lemma, 'ς$' ), 'morphology' ] <- "A3U-------"
    df[ df$bookname == 'Isa' & ( df$morphology == "A1B-B-----" | df$morphology == "A1--B-----" ), 'morphology' ] <- "A1--------"
    df[ df$bookname == 'PsSol' & ( df$morphology == "A1--B-----" ), 'morphology' ] <- "A1--------"
    df[ df$bookname == 'Ruth' & df$chapter == '004' & df$verse == '004' & df$morphology == 'A1A-P-----', 'morphology' ] <- 'P---------'
    
    df$part_of_speech <- ""
    
    ## 1. Part-of-Speech
    df[ grepl( '^C', df$morphology ), 'part_of_speech' ] <- 'conjunction'
    df[ grepl( '^D', df$morphology ), 'part_of_speech' ] <- 'adverb'
    df[ grepl( '^I', df$morphology ), 'part_of_speech' ] <- 'interjection'
    
    df[ grepl( '^P', df$morphology ), 'part_of_speech' ] <- 'preposition'
    df[ grepl( '^X', df$morphology ), 'part_of_speech' ] <- 'particle'
    
    df[ grepl( '^RA', df$morphology ), 'part_of_speech' ] <- 'def_article'
    df[ grepl( '^RD', df$morphology ), 'part_of_speech' ] <- 'demon_pronoun'
    df[ grepl( '^RI', df$morphology ), 'part_of_speech' ] <- 'interr_indef_pronoun'
    df[ grepl( '^RP', df$morphology ), 'part_of_speech' ] <- 'person_pronoun'
    df[ grepl( '^RR', df$morphology ), 'part_of_speech' ] <- 'relat_pronoun' 
    df[ grepl( '^RX', df$morphology ), 'part_of_speech' ] <- 'relat_pronoun' # ὅστῐς indefinite relative pronoun (set to RR)
    
    df[ grepl( '^A', df$morphology ), 'part_of_speech' ] <- 'adjective'
    df[ grepl( '^N', df$morphology ), 'part_of_speech' ] <- 'noun'
    df[ grepl( '^V', df$morphology ), 'part_of_speech' ] <- 'verb'
    
    df[ grepl( '^M', df$morphology ), 'part_of_speech' ] <- 'indecl_number' # lacking in NT morph (under 'adjective' (A))  
    
    
    # fill empty
    df$degree <- ""
    df$voice <- ""
    df$tense <- ""
    df$mood <- ""
    df$person <- ""
    df$case <- ""
    df$number <- ""
    df$gender <- ""
    
    ############
    # MISC   ### 0.
    ############
    
    dfm <- df[ df$part_of_speech %in% c( 'conjunction', 'adverb', 'interjection', 'preposition', 'particle', 'indecl_number' ), ]
    
    
    ############
    ### NOUN ### 1.
    ############
    
    dfn <- df[ df$part_of_speech == 'noun', ]
    
    dfn$case <- substr( dfn$morphology, 5, 5 ) # [A, D, G, N, V]
    dfn$number <- substr( dfn$morphology, 6, 6 ) # [P, S]
    dfn$gender <- substr( dfn$morphology, 7, 7 ) # [F, M, N]
    
    summary( as.factor( dfn$case ) )
    summary( as.factor( dfn$number ) )
    summary( as.factor( dfn$gender ) )
    
    ################
    ### ADJECIVE ### 2.
    ################
    
    dfa <- df[ df$part_of_speech == 'adjective', ]
    
    dfa$case <- substr( dfa$morphology, 5, 5 ) # [A, D, G, N]
    dfa$number <- substr( dfa$morphology, 6, 6 ) # [P, S]
    dfa$gender <- substr( dfa$morphology, 7, 7 ) # [F, M, N]
    dfa$degree <- substr( dfa$morphology, 8, 8 ) # [-, C, S], comparative, superlative
    
    summary( as.factor( dfa$case ) )
    summary( as.factor( dfa$number ) )
    summary( as.factor( dfa$gender ) )
    summary( as.factor( dfa$degree ) )
    
    ################
    ### RELATIVE ### 3.
    ################
    
    dfr <- df[ df$part_of_speech %in% c( 'def_article', 'demon_pronoun', 'interr_indef_pronoun', 'person_pronoun', 'relat_pronoun' ), ]
    
    dfr$case <- substr( dfr$morphology, 5, 5 ) # [A, D, G, N]
    dfr$number <- substr( dfr$morphology, 6, 6 ) # [P, S]
    dfr$gender <- substr( dfr$morphology, 7, 7 ) # [-, F, M, N]
    
    summary( as.factor( dfr$case ) )
    summary( as.factor( dfr$number ) )
    summary( as.factor( dfr$gender ) )
    
    ############
    ### VERB ### 4.
    ############
    dfv <- df[ df$part_of_speech == 'verb', ]
    
    # tense (P=present, I=imperfect, F=future, A=aorist, X=perfect, Y=pluperfect)
    dfv$tense <- substr( dfv$morphology, 5, 5 )
    summary( as.factor( dfv$tense ) ) # [A, F, I, P, X, Y]
    
    # voice (A=active, M=middle, P=passive)
    #df[ df$voice == 'A', 'voice' ] <- 'active'
    #df[ df$voice == 'M', 'voice' ] <- 'middle'
    #df[ df$voice == 'P', 'voice' ] <- 'passive'
    dfv$voice <- substr( dfv$morphology, 6, 6 )
    summary( as.factor( dfv$voice ) ) # [A, D, M, P]
    
    # mood (I=indicative, D=imperative, S=subjunctive, O=optative, N=infinitive, P=participle)
    #df[ df$mood == 'I', 'mood' ] <- 'indicative'
    #df[ df$mood == 'D', 'mood' ] <- 'imperative'
    #df[ df$mood == 'S', 'mood' ] <- 'subjunctive'
    #df[ df$mood == 'O', 'mood' ] <- 'optative'
    #df[ df$mood == 'N', 'mood' ] <- 'infinitive'
    #df[ df$mood == 'P', 'mood' ] <- 'participle'
    dfv$mood <- substr( dfv$morphology, 7, 7 )
    summary( as.factor( dfv$mood ) ) # [D, I, N,  O, P, S]
    
    # number
    dfv$number <- substr( dfv$morphology, 9, 9 )
    summary( as.factor( dfv$number ) ) # [P,S]
    
    # gender
    dfv$gender <- substr( dfv$morphology, 10, 10 )
    summary( as.factor( dfv$gender ) ) # [F,M,N]
    
    # combines 'person' [1,2,3] with 'case' [A, D, G, N, V]
    dfv$case <- substr( dfv$morphology, 8, 8 )
    
    # so: determine selection indices
    idx_person <- dfv$case %in% c( '1', '2', '3' )
    
    # and transfer 1,2,3 to 'person'
    dfv[ idx_person, 'person' ] <- dfv[ idx_person, 'case' ]
    dfv[ idx_person, 'case' ] <- "-"

    summary( as.factor( dfv$case ) ) # [-, A, D, G, N]
    summary( as.factor( dfv$person ) ) # [1, 2, 3]

    ### MERGE
    sub1 <- rbind( dfn, dfa )
    sub2 <- rbind( dfr, dfv )
    
    sub3 <- rbind( sub1, sub2 )
    output <- rbind( sub3, dfm )
    
    # sort
    m <- sort.int( output$whereabout, index.return = TRUE )$ix
    output <- output[ m, ]
    
    # clean
    output$whereabout <- NULL
    
    # relabel
    df <- output
    
    # degree (C=comparative, S=superlative)
    df[ df$degree == 'C', 'degree' ] <- 'comparative'
    df[ df$degree == 'S', 'degree' ] <- 'superlative'
    summary( as.factor( df$degree ) )
    
    # gender (M=masculine, F=feminine, N=neuter)
    df[ df$gender == 'M', 'gender' ] <- 'masculine'
    df[ df$gender == 'F', 'gender' ] <- 'feminine'
    df[ df$gender == 'N', 'gender' ] <- 'neuter'
    summary( as.factor( df$gender ) )
    
    # case (N=nominative, G=genitive, D=dative, A=accusative) 
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

    ## CORRECT Gen 22:5 voice [D->M]
    #
    # Inflected:	καθίσατε
    # Root:	καθίζω
    # Strong's:	G2523
    # Code:	V-AMI-2P
    # Verb - Aorist Middle Indicative - 2nd Person Plural
    df[ df$word == 'καθίσατε' & df$voice == 'D', 'voice' ] <- 'middle'
    df[ df$word == 'καθίσατε' & df$voice == 'D', 'morphology' ] <- 'VA--AMI2P-'
    
    summary( as.factor( df$voice ) )
        
    # mood (I=indicative, D=imperative, S=subjunctive, O=optative, N=infinitive, P=participle)
    df[ df$mood == 'I', 'mood' ] <- 'indicative'
    df[ df$mood == 'D', 'mood' ] <- 'imperative'
    df[ df$mood == 'S', 'mood' ] <- 'subjunctive'
    df[ df$mood == 'O', 'mood' ] <- 'optative'
    df[ df$mood == 'N', 'mood' ] <- 'infinitive'
    df[ df$mood == 'P', 'mood' ] <- 'participle'
    summary( as.factor( df$mood ) )
    
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
    
    # to check: https://www.blueletterbible.org/search/search.cfm?Criteria=%CE%91%CF%83%CE%B1%CF%83%CE%B1%CE%BD%CE%B8%CE%B1%CE%BC%CE%B1%CF%81&t=LXX#s=s_primary_0_1
    df$single <- paste0( df$part_of_speech, "] ", paste( df$tense, df$voice, df$mood, df$person, df$number, df$gender, df$case ) )
    df$single <- gsub( ' ', '_', stringr::str_squish( df$single ) )
    df$single <- gsub( ']_', '___', df$single )    
    df$single <- gsub( ']$', '', df$single )
    

    
    return( df )    
}



################################################################################
# END CUSTOM FUNCTIONS
################################################################################

indir <- 'data/lxx'
outdir <- 'out.00.convert.lxx'
dir.create( outdir, showWarnings = FALSE )

books <- get_lxx_books( indir )

book <- books[ 5 ]

for( book in books )
{
    print( book )
    infile <- paste0( indir, '/', book )
    
    # preprocess
    df <- pre_process( infile )

    ## process morphology to match LXX with NT
    df <- process_df( df )

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
