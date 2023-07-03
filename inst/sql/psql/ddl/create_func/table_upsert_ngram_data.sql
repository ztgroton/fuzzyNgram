
CREATE OR REPLACE FUNCTION public.table_upsert_ngram_data(ngram_table TEXT)
  RETURNS TABLE (
    id INTEGER, 
    value TEXT, 
    length INTEGER
  )
  LANGUAGE PLPGSQL
AS $$
BEGIN
  
  RETURN QUERY EXECUTE format(
    'INSERT INTO corpus.ngram_data (value, length) '
    'SELECT DISTINCT '
    't.ngram_value as value, '
    't.ngram_len as length '
    'FROM public.%I t ' --ngram_table
    'ON CONFLICT DO NOTHING '
    'RETURNING id, value, length '
    , ngram_table::TEXT
  );

END;
$$
