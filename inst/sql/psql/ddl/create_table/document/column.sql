
CREATE TABLE document.column (

id SERIAL CHECK (id > 0), 

dataset_id INTEGER NOT NULL CHECK (dataset_id > 0), 

name TEXT NOT NULL, 

CONSTRAINT document_column__pkey PRIMARY KEY(id), 

CONSTRAINT document_column__fkey1 FOREIGN KEY (dataset_id) REFERENCES document.dataset(id) ON UPDATE CASCADE ON DELETE CASCADE, 

CONSTRAINT document_column__unq1 UNIQUE(dataset_id, name)

);
