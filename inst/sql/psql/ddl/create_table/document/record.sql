
CREATE TABLE document.record (

dataset_id INTEGER NOT NULL CHECK (dataset_id > 0), 

id INTEGER NOT NULL CHECK (id > 0), 

CONSTRAINT document_record__pkey PRIMARY KEY (dataset_id, id), 

CONSTRAINT document_record__fkey1 FOREIGN KEY (dataset_id) REFERENCES document.dataset(id) ON UPDATE CASCADE ON DELETE CASCADE 

); 
