
CREATE TABLE document.ngram_data (

id SERIAL CHECK (id > 0), 

value TEXT NOT NULL, 

length INTEGER NOT NULL CHECK (length > 0), 

CONSTRAINT document_ngram_data__pkey PRIMARY KEY(id), 

CONSTRAINT document_ngram_data__unq1 UNIQUE(value)

);
