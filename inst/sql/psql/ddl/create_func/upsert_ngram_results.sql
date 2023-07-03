
CREATE OR REPLACE FUNCTION public.upsert_ngram_results(ngram_table TEXT, dataset INTEGER)
RETURNS TABLE (
    dataset_id INTEGER, 
    column_id INTEGER, 
    record_id INTEGER, 
    ngram_id INTEGER
)
LANGUAGE plpgsql SECURITY DEFINER
AS $$
BEGIN
  
  RETURN QUERY EXECUTE format(
    'SELECT DISTINCT'
    '%L::INTEGER as dataset_id, ' --dataset
    'ct.id as column_id, '
    'ngt.row_num as record_id, '
    'u.id as ngram_id '
    ' '
    'FROM public.%I ngt ' --ngram_table
    ' '
    'LEFT OUTER JOIN corpus.column ct ' 
    'ON ngt.name = ct.name '
    'AND ct.dataset_id = %L ' --dataset
    ' '
    'LEFT OUTER JOIN corpus.ngram_data u ' 
    'ON ngt.ngram_value = u.value '
    , dataset::INTEGER
    , ngram_table::TEXT
    , dataset::INTEGER
  );

END
$$
