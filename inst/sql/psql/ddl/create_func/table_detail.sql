
CREATE OR REPLACE FUNCTION public.table_detail(sch TEXT, tbl TEXT)
  RETURNS TABLE (
    table_schema TEXT, 
    table_name TEXT, 
    ordinal_position INTEGER, 
    column_name TEXT, 
    data_type TEXT, 
    is_nullable TEXT, 
    is_pk BOOLEAN
  )
  LANGUAGE PLPGSQL
AS $$
BEGIN

  RETURN QUERY SELECT 
  c.table_schema::TEXT, 
  c.table_name::TEXT, 
  c.ordinal_position::INTEGER, 
  c.column_name::TEXT, 
  c.data_type::TEXT, 
  c.is_nullable::TEXT, 
  COALESCE(pk.is_pk::BOOLEAN, FALSE) as is_pk
      
  FROM information_schema.columns c
  
  INNER JOIN information_schema.tables t 
  ON t.table_name = c.table_name 
  AND t.table_schema = c.table_schema 
  AND t.table_catalog = c.table_catalog
  
  LEFT OUTER JOIN  -- primary keys 
  (
    SELECT DISTINCT
    tc.table_name, tc.table_schema, tc.table_catalog, kcu.column_name, TRUE as is_pk
    FROM information_schema.table_constraints AS tc
    JOIN information_schema.key_column_usage AS kcu
      ON tc.constraint_name = kcu.constraint_name
    WHERE constraint_type = 'PRIMARY KEY'
  ) pk
  ON pk.table_name = c.table_name
  AND pk.column_name = c.column_name
  AND pk.table_schema = c.table_schema
  AND pk.table_catalog = c.table_catalog
  
  WHERE c.table_schema = sch::TEXT 
  AND c.table_name = tbl::TEXT 
  AND t.table_type = 'BASE TABLE' 
  
  ORDER BY t.table_schema, t.table_name, c.ordinal_position;

END;
$$
