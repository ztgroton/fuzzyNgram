
CREATE OR REPLACE FUNCTION public.table_pivot_token(sch TEXT, tbl TEXT)
  RETURNS TABLE (
    row_num INTEGER, 
    name TEXT, 
    value TEXT, 
    val_array TEXT[], 
    val_array_len INTEGER
  )
  LANGUAGE PLPGSQL
AS $$
BEGIN

  RETURN QUERY EXECUTE format(
    'WITH jsonb_data AS '
    '( '
    '  select '
    '  ROW_NUMBER() OVER() as row_num, '
    '  to_jsonb(t.*) as jsonb_col '
    '  from %I.%I t ' -- sch, tbl
    '), '
    ' '
    'table_pivoted AS  '
    '( '
    '  select '
    '  t.row_num::INTEGER as row_num, '
    '  u.name, ' 
    '  LOWER(REGEXP_REPLACE(REGEXP_REPLACE(u.value, ''[^[:alnum:][:blank:]]'', ''''), ''\s+'', '' '')) as value'
    '  from jsonb_data t '
    '  cross join jsonb_each_text(t.jsonb_col) as u(name, value) '
    ') '
    ' '
    'select '
    't.row_num::INTEGER, '
    't.name::TEXT, ' 
    't.value::TEXT, '
    'string_to_array(t.value, '' '')::TEXT[] as val_array, '
    'ARRAY_LENGTH(string_to_array(t.value, '' '')::TEXT[], 1) as val_array_len '
    'from table_pivoted t '
    , sch::TEXT, tbl::TEXT
  );

END;
$$
