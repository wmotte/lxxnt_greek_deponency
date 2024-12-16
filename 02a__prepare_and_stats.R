#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
# Prepare and print stats
#
################################################################################

###
# Collect meta-info
##
collect_metainfo <- function( type )
{
    if( type == 'nt' )
        in_files <- dir( paste0( 'out.01.convert.nt/' ), '*__metainfo.csv', full.names = TRUE )
    
    if( type == 'lxx' )
        in_files <- dir( paste0( 'out.00.convert.lxx/' ), '*__metainfo.csv', full.names = TRUE )
    
    all <- NULL
    
    in_file <- in_files[ 1 ]
    
    for( in_file in in_files )
    {
        print( in_file )
        df <- read.csv( in_file, row.names = 1 )
        all <- rbind( all, df )
    }
    
    return( all )
}

################################################################################


ot_files <- dir( 'out.00.convert.lxx/', '*__morphology.csv', full.names = TRUE )
nt_files <- dir( 'out.01.convert.nt/', '*__morphology.csv', full.names = TRUE )

length( ot_files ) # 59
length( nt_files ) # 27

all_nt <- NULL

nt_file <- nt_files[ 1 ]

for( nt_file in nt_files )
{
    print( nt_file )
    df <- readr::read_csv( nt_file, show_col_types = FALSE )
    
    all_nt <- rbind( all_nt, df )
}


all_ot <- NULL

ot_file <- ot_files[ 1 ]

for( ot_file in ot_files )
{
    print( ot_file )
    df <- readr::read_csv( ot_file, show_col_types = FALSE )
    
    all_ot <- rbind( all_ot, df )
}

################################################################################

# outdir
outdir <- 'out.02.prepare.and.stats'
dir.create( outdir, showWarnings = FALSE )

# single string nt
single_nt <- paste( all_nt$text, collapse = ' ' )

# single string ot
single_ot <- paste( all_ot$text, collapse = ' ' )

# single string ot + nt
all <- rbind( all_ot, all_nt )
single_all <- paste( all$text, collapse = ' ' )

# get frequency table
freq_nt <- data.frame( sort( table( unlist( strsplit( single_nt, " " ) ) ), decreasing = TRUE ) )
freq_ot <- data.frame( sort( table( unlist( strsplit( single_ot, " " ) ) ), decreasing = TRUE ) )
freq_all <- data.frame( sort( table( unlist( strsplit( single_all, " " ) ) ), decreasing = TRUE ) )

head( freq_nt )
head( freq_ot )
head( freq_all )

sum( freq_nt$Freq ) # 137,554 words
sum( freq_ot$Freq ) # 623,685 words
sum( freq_all$Freq ) # 761,239 words

# write to disk
readr::write_csv( freq_nt, file = paste0( outdir, '/freq_nt.csv' ), quote = 'all' )
readr::write_csv( freq_ot, file = paste0( outdir, '/freq_ot.csv' ), quote = 'all' )
readr::write_csv( freq_all, file = paste0( outdir, '/freq_all.csv' ), quote = 'all' )

# get differences between LXX and NT
idx1 <- freq_nt$Var1 %in% freq_ot$Var1 
idx2 <- freq_ot$Var1 %in% freq_nt$Var1 

notfoundin_lxx <- freq_nt[ !idx1, ]
notfoundin_nt <- freq_ot[ !idx2, ]

readr::write_csv( notfoundin_lxx, file = paste0( outdir, '/not_found_in_lxx.csv' ), quote = 'all' )
readr::write_csv( notfoundin_nt, file = paste0( outdir, '/not_found_in_nt.csv' ), quote = 'all' )

# write input for GloVe
output <- as.data.frame( all )
output$`...1` <- NULL
rownames( output ) <- NULL
readr::write_tsv( output, file = gzfile( paste0( outdir, '/glove_input.tsv.gz' ) ), quote = 'all' ) 


###################################################
############ Get list of verb voices ##############
###################################################

meta_nt <- collect_metainfo( 'nt' )
meta_lxx <- collect_metainfo( 'lxx' )

# combine
meta <- rbind( meta_lxx, meta_nt )

write.table( meta, file = paste0( outdir, '/meta_all.tsv' ), quote = TRUE, sep = '\t' )

# get selection of one example
sel <- meta[ meta$single == 'verb___aorist_deponent_participle_plural_masculine_nominative', ]
sel <- sel[ , c( 'bookname', 'chapter', 'verse', 'word', 'lemma' ) ]
rownames( sel ) <- NULL
head( sel )

# get all verbs
allverbs <- as.data.frame( table( sel$lemma ) )

# write stats
readr::write_csv( allverbs, file = paste0( outdir, '/verb___aorist_deponent_participle_plural_masculine_nominative___verbs.csv' ), quote = 'all' )
readr::write_csv( sel, file = paste0( outdir, '/verb___aorist_deponent_participle_plural_masculine_nominative___sel.csv' ), quote = 'all' )

# get percentages
#
# (-mai)
#   val      n  perc    voice
# 88781 129953 68.32   active
# 16535 129953 12.72 deponent
# 13176 129953 10.14   middle
# 11461 129953  8.82  passive
comb <- meta[ meta$part_of_speech == 'verb' & meta$voice %in% c( 'deponent', 'active', 'middle', 'passive' ), ]
p <- data.frame( val = summary( as.factor( comb$voice ) ) )
p$n <- sum( p$val )
p$perc <- round( ( p$val / p$n ) * 100, 2 )
p$voice <- rownames( p )
rownames( p ) <- NULL

# write to disk
readr::write_csv( p, file = paste0( outdir, '/verb_voice_n_and_percentage__nt_lxx_combined.csv' ), quote = 'all' )


##################################################################
######### Select list of individual verbs with frequency #########
##################################################################


# active verbs
actives <- unique( meta[ meta$voice == 'active', 'lemma' ] )

all_act <- NULL
for( active in actives )
{
    n <- sum( meta$lemma == active )
    single <- data.frame( active, n )
    all_act <- rbind( all_act, single )
}

# deponent verbs
deponents <- unique( meta[ meta$voice == 'deponent', 'lemma' ] )

all_dep <- NULL
for( deponent in deponents )
{
    n <- sum( meta$lemma == deponent )
    single <- data.frame( deponent, n )
    all_dep <- rbind( all_dep, single )
}

# middles verbs
middles <- unique( meta[ meta$voice == 'middle', 'lemma' ] )

all_mid <- NULL
for( middle in middles )
{
    n <- sum( meta$lemma == middle )
    single <- data.frame( middle, n )
    all_mid <- rbind( all_mid, single )
}

# passive verbs
passives <- unique( meta[ meta$voice == 'passive', 'lemma' ] )

all_pas <- NULL
for( passive in passives )
{
    n <- sum( meta$lemma == passive )
    single <- data.frame( passive, n )
    all_pas <- rbind( all_pas, single )
}

# sort
all_act <- all_act[ sort.int( all_act$n, index.return = TRUE, decreasing = TRUE )$ix, ]

# sort
all_dep <- all_dep[ sort.int( all_dep$n, index.return = TRUE, decreasing = TRUE )$ix, ]

# sort
all_mid <- all_mid[ sort.int( all_mid$n, index.return = TRUE, decreasing = TRUE )$ix, ]

# sort
all_pas <- all_pas[ sort.int( all_pas$n, index.return = TRUE, decreasing = TRUE )$ix, ]

# write to disk
readr::write_csv( all_act, file = paste0( outdir, '/verbs_active__nt_lxx_combined.csv' ), quote = 'all' )
readr::write_csv( all_dep, file = paste0( outdir, '/verbs_deponent__nt_lxx_combined.csv' ), quote = 'all' )
readr::write_csv( all_mid, file = paste0( outdir, '/verbs_middle__nt_lxx_combined.csv' ), quote = 'all' )
readr::write_csv( all_pas, file = paste0( outdir, '/verbs_passive__nt_lxx_combined.csv' ), quote = 'all' )

# not -omai
head( all_dep[ !stringr::str_detect( all_dep$deponent, 'ομαι$' ), ] )

