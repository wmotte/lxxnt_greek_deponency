# Quantitative Analysis of the ‘Deponency’ in Biblical Greek

This repository contains the input data, scripts and Suppl. Tables and Figures from a study that employs distributional linguistics, a newly introduced technique to examine the controversial concept of verbal ‘deponency’ in Biblical Greek. 

Renaissance grammarians introduced ‘deponency’ as an additional classifier to the traditional voice paradigm (active, middle, passive) to account for verbs exhibiting middle-passive morphology with active interpretation. 

While contemporary New Testament scholars often dismiss this classification as superfluous, advocating for its reintegration into the middle voice category, our distributional analysis of the LXX-NT corpus reveals distinct grammatical patterns. 

The analyses demonstrates that ‘deponent’ verbs inhabit grammatical contexts more closely aligned with active voice rather than middle voice forms. 

These findings suggest that Renaissance grammatical insights merit reconsideration, potentially warranting a reassessment of deponency’s role in Greek verbal taxonomy rather than its wholesale rejection from contemporary linguistic analysis.


### Background and Significance

The study employs **distributional analysis**, a quantitative linguistic method, to investigate the debated concept of **deponency** in Biblical Greek. Traditional grammatical approaches have long struggled with the classification of deponent verbs—those that exhibit middle or passive morphology but are interpreted with active meaning. While contemporary scholars often advocate for the elimination of deponency as an unnecessary Latin-based construct, this study challenges that position by utilizing corpus-wide **computational analysis** to detect systematic grammatical patterns.

Historically, the concept of deponency was introduced by **Renaissance grammarians** who sought to impose Latin grammatical structures onto Greek, leading to the classification of certain middle-voice verbs as 'deponent'. Modern scholarship, particularly from the late 20th and early 21st centuries, has called this classification into question, arguing that the Greek middle voice itself is poorly understood and that many so-called deponent verbs may be better analyzed as authentic middle forms. However, this critique has largely been based on qualitative and interpretive analyses, without large-scale empirical validation. 

The present study fills this gap by applying **distributional analysis**, a method rooted in computational linguistics, to the **entire LXX-NT corpus**. Unlike traditional approaches that emphasize **lexical meaning**, this study systematically isolates **grammatical form** by constructing an **artificial corpus** where lemmas are replaced with their corresponding grammatical categories. This approach enables an objective examination of the syntactic behavior of deponent verbs without interference from semantic interpretations.

The core of the methodology involves generating **vector-based word embeddings** using a modified **GLoVe model**, a well-established distributional approach that quantifies word similarity based on co-occurrence patterns. The study specifically measures whether deponent verbs cluster more closely with active-voice or middle-voice grammatical forms. The findings indicate that in **75.4% of cases**, deponent verbs exhibit stronger syntactic similarity to **active** voice constructions than to middle voice constructions. This statistically significant pattern suggests that Renaissance grammarians may have correctly identified a real grammatical phenomenon rather than erroneously importing Latin concepts onto Greek.

### The GLoVe Model in Detail

The **GLoVe (Global Vectors for Word Representation) model** is a widely used unsupervised learning algorithm for generating word embeddings based on statistical co-occurrence information across a large corpus. Unlike models such as Word2Vec, which rely on predictive learning frameworks, GLoVe constructs embeddings based on the **global co-occurrence matrix**, ensuring that words appearing in similar contexts are mapped to similar vector representations. 

GLoVe operates by first constructing a word co-occurrence matrix, where each entry represents the frequency with which two words appear together within a defined context window. This co-occurrence data is then transformed into a **log-bilinear model**, which optimizes vector representations by minimizing the difference between predicted and actual co-occurrence probabilities. The key advantage of this approach is that it preserves **both local and global corpus information**, offering a more comprehensive representation of linguistic structures.

Mathematically, the GLoVe model solves the following optimization problem:

\[ J = \sum_{i,j=1}^{V} f(X_{ij}) ( w_i^T w_j + b_i + b_j - \log X_{ij} )^2 \]

where:
- \(X_{ij}\) represents the co-occurrence count between words \(i\) and \(j\),
- \(w_i\) and \(w_j\) are the word vectors being trained,
- \(b_i\) and \(b_j\) are bias terms,
- \(f(X_{ij})\) is a weighting function to balance frequent and infrequent co-occurrences.

By factoring the logarithm of co-occurrence probabilities, GLoVe captures **meaningful semantic relationships** between words. The model assigns similar vector representations to words that frequently co-occur in similar contexts, thereby encoding syntactic and semantic similarities within the embedding space.

In the context of this study, the **LXX-NT corpus** was processed using the GLoVe framework, ensuring that **grammatical forms** were embedded in a continuous vector space based purely on their distributional behavior. The selected context window of five words on either side of the target word allowed the model to capture **morpho-syntactic dependencies**, effectively distinguishing **active, middle, and deponent forms** based on their statistical relationships. 

By leveraging GLoVe embeddings, this study provides an **empirical, corpus-wide validation** of deponency as a potential grammatical category, countering the prevailing scholarly consensus that it should be discarded. The approach also highlights the importance of **quantitative methods** in addressing long-standing linguistic debates. By prioritizing grammatical distribution over semantic intuition, this study provides **a reproducible, data-driven framework** for analyzing voice distinctions in Biblical Greek. Future research could refine these findings by integrating **probabilistic modeling** to express deponency as a graded rather than binary category, potentially reconciling traditional and modern perspectives on Greek voice morphology.




