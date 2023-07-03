
CREATE OR REPLACE FUNCTION public.table_pivot_long(sch TEXT, tbl TEXT)
  RETURNS TABLE (
    row_num INTEGER, 
    name TEXT, 
    value TEXT
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
    ') '
    ' '
    'select '
    't.row_num::INTEGER as row_num, u.name, u.value '
    'from jsonb_data t '
    'cross join jsonb_each_text(t.jsonb_col) as u(name, value) '
    , sch::TEXT, tbl::TEXT
  );

END;
$$
