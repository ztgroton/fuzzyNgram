
CREATE OR REPLACE FUNCTION public.table_ngram_indicies(n INTEGER, rolling BOOLEAN DEFAULT FALSE)
  RETURNS TABLE (
    ngram_len INTEGER, 
    ind_start INTEGER, 
    ind_end INTEGER 
  )
  LANGUAGE PLPGSQL
AS $$
DECLARE 
  
  len_indicies INTEGER[];
  x_indicies INTEGER[];
  y_indicies INTEGER[];
  
BEGIN
  
  -- Generate Index Values / Store in Arrays 
  IF (rolling) THEN 
    
    for x in 1..n loop
      
      for m in 1..x loop
        for i in 1..(x-m+1) loop
          len_indicies := ARRAY_APPEND(len_indicies, x);
          x_indicies := ARRAY_APPEND(x_indicies, i);
          y_indicies := ARRAY_APPEND(y_indicies, i+m-1);
        end loop;
      end loop;
      
    end loop;
    
    -- Convert Arrays to Columns / Return as Table
    RETURN QUERY SELECT * FROM unnest(len_indicies, x_indicies, y_indicies) as x(ngram_len, ind_start, ind_end);
    
  ELSE
    
    for m in 1..n loop
      for i in 1..(n-m+1) loop
        x_indicies := ARRAY_APPEND(x_indicies, i);
        y_indicies := ARRAY_APPEND(y_indicies, i+m-1);
      end loop;
    end loop;
    
    -- Convert Arrays to Columns / Return as Table
    RETURN QUERY SELECT n as ngram_len, x.ind_start, x.ind_end
    FROM unnest(x_indicies, y_indicies) as x(ind_start, ind_end);
    
  END IF;

END;
$$
