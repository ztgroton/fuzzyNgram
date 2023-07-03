
CREATE TABLE corpus.ngram_data (

id SERIAL CHECK (id > 0), 

value TEXT NOT NULL, 

length INTEGER NOT NULL CHECK (length > 0), 

CONSTRAINT corpus_ngram_data__pkey PRIMARY KEY(id), 

CONSTRAINT corpus_ngram_data__unq1 UNIQUE(value)

);
