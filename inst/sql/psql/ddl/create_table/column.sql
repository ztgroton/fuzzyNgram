
CREATE TABLE corpus.column (

id SERIAL CHECK (id > 0), 

dataset_id INTEGER NOT NULL CHECK (dataset_id > 0), 

name TEXT NOT NULL, 

CONSTRAINT corpus_column__pkey PRIMARY KEY(id), 

CONSTRAINT corpus_column__fkey1 FOREIGN KEY (dataset_id) REFERENCES corpus.dataset(id) ON UPDATE CASCADE ON DELETE CASCADE, 

CONSTRAINT corpus_column__unq1 UNIQUE(dataset_id, name)

);
