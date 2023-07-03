
CREATE TABLE corpus.dataset (

id SERIAL CHECK (id > 0), 

name TEXT NOT NULL, 

CONSTRAINT corpus_dataset__pkey PRIMARY KEY(id), 

CONSTRAINT corpus_dataset__unq1 UNIQUE(name)

); 
