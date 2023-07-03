
CREATE OR REPLACE PROCEDURE public.upsert_ngram(ngram_table TEXT, dataset_id INTEGER)
LANGUAGE plpgsql SECURITY DEFINER
AS $$
BEGIN
  
  EXECUTE format(
    'INSERT INTO corpus.ngram (dataset_id, column_id, record_id, ngram_position, ngram_id) ' 
    'SELECT DISTINCT '
    '%L::INTEGER as dataset_id, ' --dataset_id
    'ct.id as column_id, '
    'ngt.row_num as record_id, '
    'CONCAT(ngt.ind_start, ''-'', ngt.ind_end) as ngram_position, '
    'u.id as ngram_id '
    ' '
    'FROM public.%I ngt ' --ngram_table
    ' '
    'LEFT OUTER JOIN corpus.column ct ' 
    'ON ngt.name = ct.name '
    'AND ct.dataset_id = %L ' --dataset_id
    ' '
    'LEFT OUTER JOIN corpus.ngram_data u ' 
    'ON ngt.ngram_value = u.value '
    'ON CONFLICT DO NOTHING '
    , dataset_id::INTEGER
    , ngram_table::TEXT
    , dataset_id::INTEGER
  );

END
$$
