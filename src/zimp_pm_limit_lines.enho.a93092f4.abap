"Name: \TY:CL_RAP_QUERY_PAGING\IN:IF_RAP_QUERY_PAGING\ME:GET_PAGE_SIZE\SE:END\EI
ENHANCEMENT 0 ZIMP_PM_LIMIT_LINES.
if rv_page_size <= 100.
  select single low from tvarvc into @data(var_rv_page_size) where name eq 'ZIMP_PM_LIMIT_LINES_OBJ_TECN'. "Limite de linhas para exibir.
  if sy-subrc eq 0.
    rv_page_size = var_rv_page_size.
  endif.
endif.
ENDENHANCEMENT.
