+++
title = "Mining for Health: A Comparison of Word Embedding Methods for Analysis of EHRs Data"
author = ["Chloe"]
date = 2022-11-09
lastmod = 2022-11-09T23:03:14-05:00
tags = ["test"]
draft = true
+++

tags
: Statistics,DataScience

keywords
: EHRsData


## Mining for Health: A Comparison of Word Embedding Methods for Analysis of EHRs Data {#getzenMiningHealthComparison2022}


### Word embedding algorithms - a powerful tool to extract information for EHRs {#word-embedding-algorithms-a-powerful-tool-to-extract-information-for-ehrs}


#### What word embedding algorithms does: build lower dimensional vector representation of a word from a corpus of text {#what-word-embedding-algorithms-does-build-lower-dimensional-vector-representation-of-a-word-from-a-corpus-of-text}

How it is extended to EHR:

-   medical events/codes &lt;- words
-   a collection of medical records &lt;- corpus
-   order of medical events happening before and after **the medical event**
    &lt;- context around an event


#### The structured medical codes are sequential and dependent, making embedding algorithms an effective tool (how?) <span class="tag"><span class="unanswered">unanswered</span></span> {#the-structured-medical-codes-are-sequential-and-dependent-making-embedding-algorithms-an-effective-tool--how}


#### word embedding algorithms being used in embedding useful information in patients' representation vectors {#word-embedding-algorithms-being-used-in-embedding-useful-information-in-patients-representation-vectors}


### Heterogeneous nature of the data makes it not possible to use PCA and other classical methods to do dimensional reduction {#heterogeneous-nature-of-the-data-makes-it-not-possible-to-use-pca-and-other-classical-methods-to-do-dimensional-reduction}


### Extracting useful features such as patients' phenotype, using word ending algorithms to covert text into forms that learning algorithms can understand {#extracting-useful-features-such-as-patients-phenotype-using-word-ending-algorithms-to-covert-text-into-forms-that-learning-algorithms-can-understand}


#### Application: use word embedding vectors on structured medical code and developed Patient-Diagnosis Projection Similarity (PDPS) model {#application-use-word-embedding-vectors-on-structured-medical-code-and-developed-patient-diagnosis-projection-similarity--pdps--model}


#### The purpose of the paper is to assess different word embedding algorithms developed in recent year and reevaluate the performance of prediction model metioned above {#the-purpose-of-the-paper-is-to-assess-different-word-embedding-algorithms-developed-in-recent-year-and-reevaluate-the-performance-of-prediction-model-metioned-above}


### Different word embedding methods {#different-word-embedding-methods}


#### Word2Vec - input is a corpus of text and output is vector space; each word has a unique assignment of vector. <span class="tag"><span class="algorithm">algorithm</span></span> {#word2vec-input-is-a-corpus-of-text-and-output-is-vector-space-each-word-has-a-unique-assignment-of-vector-dot}

-   two model formulation
    -   Continuous bag of words (CBOW)
        -   using surrounding context (other words **around** targeting word) to
            predict the target word
    -   skip-gram
        -   use a single word to predict a target context


#### FastTest - extension of Word2Vec but have different representation of words <span class="tag"><span class="unanswered">unanswered</span><span class="algorithm">algorithm</span></span> {#fasttest-extension-of-word2vec-but-have-different-representation-of-words}


#### GloVe - unsupervised learning which also relies on global statistics (word co-occurrence) to generate vectors <span class="tag"><span class="algorithm">algorithm</span></span> {#glove-unsupervised-learning-which-also-relies-on-global-statistics--word-co-occurrence--to-generate-vectors}

-   The relationship between words is characterized by **ratio** of conditional
    probability
-   Training objective: configuring word vectors &lt;- dot-product of word
    vectors equal to the log of words' conditional probability ratio
