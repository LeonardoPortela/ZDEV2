"Name: \TY:CL_EAM_TECHNICALOBJHIERARCHY\IN:IF_EAM_TECHNICALOBJHIERARCHY\ME:DETERMINE_TOHIERARCHY_DOWN\SE:BEGIN\EI
ENHANCEMENT 0 ZIMP_PM_LIMITE_LINES.
if iv_level <= 100.
  select single low from tvarvc into @data(var_rv_page_size) where name eq 'ZIMP_PM_LIMIT_LINES_OBJ_TECN'. "Limite de linhas para exibir.
  if sy-subrc eq 0.
*    iv_level = var_rv_page_size.
  endif.
endif.
ENDENHANCEMENT.
