
CREATE TABLE document.dataset (

id SERIAL CHECK (id > 0), 

name TEXT NOT NULL, 

CONSTRAINT document_dataset__pkey PRIMARY KEY(id), 

CONSTRAINT document_dataset__unq1 UNIQUE(name)

); 
