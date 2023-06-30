
CREATE TABLE document.ngram (

dataset_id INTEGER NOT NULL CHECK (dataset_id > 0), 

column_id INTEGER NOT NULL CHECK (column_id > 0), 

record_id INTEGER NOT NULL CHECK (record_id > 0), 

ngram_id INTEGER NOT NULL CHECK (ngram_id > 0), 

CONSTRAINT document_ngram__pkey PRIMARY KEY (dataset_id, column_id, record_id), 

CONSTRAINT document_ngram__fkey1 FOREIGN KEY (dataset_id, record_id) REFERENCES document.record(dataset_id, id) ON UPDATE CASCADE ON DELETE CASCADE, 

CONSTRAINT document_ngram__fkey2 FOREIGN KEY (column_id) REFERENCES document.column(id) ON UPDATE CASCADE ON DELETE CASCADE, 

CONSTRAINT document_ngram__fkey3 FOREIGN KEY (ngram_id) REFERENCES document.ngram_data(id) ON UPDATE CASCADE ON DELETE CASCADE

);
