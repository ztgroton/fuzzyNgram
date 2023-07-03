
CREATE OR REPLACE FUNCTION public.table_pivot_ngram(sch TEXT, tbl TEXT)
  RETURNS TABLE (
    row_num INTEGER, 
    name TEXT, 
    value TEXT, 
    val_array TEXT[], 
    val_array_len INTEGER, 
    ind_start INTEGER, 
    ind_end INTEGER, 
    ngram_array TEXT[], 
    ngram_value TEXT, 
    ngram_len INTEGER
  )
  LANGUAGE PLPGSQL
AS $$
BEGIN
  
  RETURN QUERY EXECUTE format(
    'WITH table_token AS '
    '( '
    ' select * from public.table_pivot_token(%L::TEXT, %L::TEXT)' -- sch, tbl
    '), '
    ' '
    'qry_const (max_array_len) AS '
    '( '
    '  select max(val_array_len) '
    '  from table_token '
    '), '
    ' '
    'table_indicies AS '
    '( '
    '  select * from qry_const, public.table_ngram_indicies(qry_const.max_array_len::INTEGER, TRUE) '
    ') '
    ' '
    'select '
    't.row_num, '
    't.name, '
    't.value, '
    't.val_array, '
    't.val_array_len, '
    ' '
    'u.ind_start, '
    'u.ind_end, '
    ' '
    't.val_array[u.ind_start:u.ind_end] as ngram_array, '
    'array_to_string(t.val_array[u.ind_start:u.ind_end], '' '') as ngram_value, '
    'array_length(t.val_array[u.ind_start:u.ind_end], 1) as ngram_len '
    ' '
    'from table_token t '
    'inner join table_indicies u '
    'on t.val_array_len = u.ngram_len '
    , sch::TEXT, tbl::TEXT
  );

END;
$$
