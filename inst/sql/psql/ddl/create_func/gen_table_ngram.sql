
CREATE OR REPLACE FUNCTION public.gen_table_ngram(data_table TEXT)
  RETURNS TEXT
  LANGUAGE PLPGSQL
AS $$
DECLARE
  temp_table_name TEXT;
BEGIN

  -- Initialize Random Temp Table Name
  select 'tbl_' || substr(md5(random()::text), 0, 15) into temp_table_name;
  
  -- Execute Dynamic DDL Statement
  EXECUTE format(
    'CREATE TABLE public.%I AS '
    'SELECT * FROM public.table_pivot_ngram(''public''::TEXT, %L::TEXT) '
    , temp_table_name::TEXT, data_table::TEXT
  );
  
  -- Return Temp Table Name
  RETURN temp_table_name;

END;
$$
