
CREATE OR REPLACE PROCEDURE public.upsert_tmp_ngram_data(ngram_table TEXT)
  LANGUAGE plpgsql SECURITY DEFINER
AS $$
BEGIN
  
  EXECUTE format(
    'INSERT INTO corpus.ngram_data (value, length) '
    'SELECT DISTINCT '
    't.ngram_value as value, '
    't.ngram_len as length '
    'FROM public.%I t '
    'ON CONFLICT DO NOTHING '
    , ngram_table::TEXT
  );

END;
$$
