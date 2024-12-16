#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
# Plot signature of active, middle, passive, and deponent form.
#
# Signature for node "aor prt pl m nom"
#
# 'verb___aorist_< VOICE >_participle_plural_masculine_nominative'
#
################################################################################
library( "ggplot2" )
library( "plyr" )

################################################################################
# FUNCTIONS
################################################################################

###
# Number of ticks
##
number_ticks <- function( n )
{
    function( limits )
        pretty( limits, n + 1 )
}

###
# Make symmatrical matrix, including doubling of diagonal
##
ultosymmetric <- function( m )
{
    # copy triangle
    out <- m + t( m )
    
    # normalize overall counts again
    out <- out / 2.0
    
    return( out )
} 

###
# Load tcm, make symmetric (so count is 200%)
##
return_symmatric_tcm <- function( nwindow )
{
    load( paste0( "out.03.emb/saved_glove__", nwindow, ".RData" ) )
 
    # get both triangles filled
    out <- ultosymmetric( as.matrix( tcm ) )
    
    return( out )
}


###
# Process vec
##
process_vec <- function( srcname, vec )
{
    
    ## 1. Part-of-Speech
    data <- data.frame( form = c( 
        rep( 'part_of_speech', 14 ),
        rep( 'tense', 6 ), 
        rep( 'voice', 4 ),
        rep( 'mood', 6 ), 
        rep( 'case', 5 ),
        rep( 'number', 2 ),
        rep( 'gender', 3 ),
        rep( 'degree', 2 ),
        rep( 'person', 3 )
    ),
    
    # part-of-speech
    text = c( '^adjective', '^conjunction', 
              '^adverb', '^interjection', '^noun', '^preposition', 
              '^def_article', '^demon_pronoun', '^interr_indef_pronoun', 
              '^person_pronoun', '^relat_pronoun', 
              '^verb', '^particle', '^indecl_number', 
              
              # tense (P=present, I=imperfect, F=future, A=aorist, X=perfect, Y=pluperfect)
              '_present', '_imperfect', '_future', '_aorist', '_perfect', '_pluperfect',
              
              # voice (A=active, M=middle, P=passive)
              '_active', '_middle', '_deponent', '_passive',
              
              # mood (I=indicative, D=imperative, S=subjunctive, O=optative, N=infinitive, P=participle)
              '_indicative', '_imperative', '_subjunctive', '_optative', '_infinitive', '_participle',
              
              # case (N=nominative, G=genitive, D=dative, A=accusative,V=vocative)
              '_nominative', '_genitive', '_dative', '_accusative', '_vocative',
              
              # number (S=singular, P=plural)
              '_singular', '_plural',
              
              # gender (M=masculine, F=feminine, N=neuter)
              '_masculine', '_feminine', '_neuter',
              
              # degree (C=comparative, S=superlative)
              '_comparative', '_superlative',
              
              # person
              '_1st', '_2nd', '_3rd' ) )
    
    
    all <- NULL
    
    for( i in 1:nrow( data ) )
    {
        form <- data[ i, 'form' ]
        text <- data[ i, 'text' ]
        sumvalue <- sum( vec[ stringr::str_detect( names( vec ), text ) ] )
        
        single <- data.frame( srcname, trgform = form, trgtext = text, sumvalue )
        
        all <- rbind( all, single )
    }
    
    return( all )
}

###
# Get selected values
##
get_selected_values <- function( mat )
{
    
    # select top right in 'deponent_forms__scatterplot_a__vs__m.png'
    srcname_act <- 'verb___aorist_active_participle_plural_masculine_nominative'
    srcname_dep <- 'verb___aorist_deponent_participle_plural_masculine_nominative'
    srcname_mid <- 'verb___aorist_middle_participle_plural_masculine_nominative'
    srcname_pas <- 'verb___aorist_passive_participle_plural_masculine_nominative'
    
    colnames( mat )[ stringr::str_detect( colnames( mat ), srcname_act ) ]
    colnames( mat )[ stringr::str_detect( colnames( mat ), srcname_dep ) ]
    colnames( mat )[ stringr::str_detect( colnames( mat ), srcname_mid ) ]
    colnames( mat )[ stringr::str_detect( colnames( mat ), srcname_pas ) ]
    
    # combine
    srcnames <- c( srcname_act, srcname_dep, srcname_mid, srcname_pas )
    
    out <- NULL
    
    for( srcname in srcnames )
    {
        # check if present as column
        if( sum( colnames( mat ) %in% srcname ) > 0 )
        {
            # vector with selected numbers
            vec <- mat[ , srcname ]
            
            # get signature for given vector
            signature <- process_vec( srcname, vec )
            
            # add to container
            out <- rbind( out, signature )
            
            
        } else {
            stop( paste0( "*** ERROR ***: form [", selection_form, "] not found!" ) )   
        }
    }
    
    # add category voice
    out$srcform <- ''
    
    out[ stringr::str_detect( out$srcname, '_active' ), 'srcform' ] <- 'active'
    out[ stringr::str_detect( out$srcname, '_deponent' ), 'srcform' ] <- 'deponent'
    out[ stringr::str_detect( out$srcname, '_middle' ), 'srcform' ] <- 'middle'
    out[ stringr::str_detect( out$srcname, '_passive' ), 'srcform' ] <- 'passive'
    
    
    # clean trgtext
    out$trgtext <- gsub( '\\^', '', out$trgtext )
    out$trgtext <- gsub( '^_', '', out$trgtext )
    
    return( out )
}


################################################################################
# END FUNCTIONS
################################################################################

# output dir
outdir <- 'out.09.signature_a_m'
dir.create( outdir, showWarnings = FALSE )


nwindows <- c( 5 )

nwindow <- nwindows[ 1 ]

# loop over windows
for( nwindow in nwindows )
{
    print( nwindow )
    mat <- return_symmatric_tcm( nwindow )
    
    # get signatures for 4 voices
    sel <- get_selected_values( mat )
    
    # write to disk
    write.csv( sel, file = paste0( outdir, '/signatures_voices__nwindow_', nwindow, '.csv' ), quote = TRUE )
}


# round
sel$sumvalue <- round( sel$sumvalue )

# remove '_'
sel$trgtext <- gsub( "_", " ", sel$trgtext )

#######################################################################
## Plot types
#######################################################################

# degree (superl. comp. == 0), and voice not included
stypes <- c( "person", 'part_of_speech', 'tense', "mood", "case", "number", "gender" )        

for( stype in stypes )
{
    print( stype )
    
    # sub-selection
    df <- sel[ sel$trgform == stype, ]

    # get average per voice
    avg <- plyr::ddply( df, .( srcname, trgform, srcform ), summarise, N = sum( sumvalue ) )
    m <- merge( df, avg )
    
    # write to disk
    write.csv( m, file = paste0( outdir, '/plot_', stype, '__nwindow_', nwindow, '.csv' ), quote = TRUE )

    # calculate average co-occurance
    m$perc <- 100 * ( m$sumvalue / m$N )
    
    # custom colors
    custom_colors <- c( "#33a02c","#1f78b4", "#E69F00" ) 
    
    m2 <- m[ m$srcform != 'passive', ]
    
    # plot
    p <- ggplot( data = m2 ) + 
        geom_col( aes( x = srcform, y = perc, group = srcform, fill = srcform ), colour = 'gray30' ) +
        scale_fill_manual( values = custom_colors ) +
        scale_y_continuous( breaks = number_ticks( 8 ) ) +
        facet_wrap( ~trgtext, ncol = 7 ) +
        xlab( "Voice [aor prt pl m nom]" ) +
        ylab( "Co-occurance (%)" ) +
        theme( legend.position = 'none', axis.text.x = element_text(angle = -60, hjust = 0, vjust = 1 ) )


    pheight <- 7

    if( stype == 'part_of_speech' )
    {
        pheight <- 7
        pwidth <- 2 + 7
    }
    
    if( stype == 'case' )
        pwidth <- 2 + 5
    
    if( stype == 'gender' )
        pwidth <- 2 + 3
    
    if( stype == 'mood' )
        pwidth <- 2 + 6
    
    if( stype == 'number' )
        pwidth <- 2 + 2
    
    if( stype == 'tense' )
        pwidth <- 2 + 6
    
    if( stype == 'person' )
        pwidth <- 2 + 3
    
    # save to image
    ggsave( plot = p, file = paste0( outdir, '/plot_', stype, '__nwindow_', nwindow, '.png' ), dpi = 600, height = pheight, width = pwidth )
    
    # save data from image
    write.csv( m, file = paste0( outdir, '/plot_', stype, '__nwindow_', nwindow, '.csv' ), quote = TRUE )
}
