
select distinct

cast(u.UPC as string) as upc,
u.PRIMARY_UPC as is_primary,
cast(i.ITEM_CODE as numeric) as item_code,
cast(u.POSITION as numeric) as position_key,
cast(u.POSITION_NAME as string) as position_name,
i.BRAND_NAME as brand_name,
i.PRODUCT_NAME as item_name,
i.PACKAGE_TYPE as package_type,
cast(i.VINTAGE as string) as vintage,
cast(i.DEFINED_LABEL as string) as defined_label,
i.DEPT_NAME as department,
i.CATEGORY as category,
i.SUBCATEGORY as subcategory,
i.CLASS as class,
i.SUPPLIER_LOCATION_NAME as supplier_name,
i.TRADING_PARTNER_NAME as trading_partner --,
--i.ASSORTMENT_TRAIT_NAME as assortment_trait,
--i.ASSORTMENT_TRAIT_TYPE as assortment_type

from edw.DIM_ITEM i

inner join edw.DIM_UPC u
on i.APEX_ITEM_ID = u.APEX_ITEM_ID

where i.current_flag = true and u.current_flag = true
