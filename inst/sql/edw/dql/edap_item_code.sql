
select distinct

cast(i.ITEM_CODE as numeric) as item_code,
i.PRODUCT_NAME as item_name,
i.PACKAGE_TYPE as package_type,
i.CATEGORY as category,
i.SUBCATEGORY as subcategory,
i.CLASS as class,
i.DEPT_NAME as department,
i.BRAND_NAME as brand_name

from edw.DIM_ITEM i

where i.current_flag = true
