FORM f_define_alv_header TABLES pt_alv_header TYPE tt_alv_header.

  DATA: ls_alv_header TYPE ty_alv_header.

  CASE abap_true.
    WHEN p_f01.
      CONCATENATE text-a01 'F.01' INTO ls_alv_header-text SEPARATED BY space.
    WHEN p_gl035.
      CONCATENATE text-a01 'ZGL035' INTO ls_alv_header-text SEPARATED BY space.
    WHEN OTHERS.
  ENDCASE.

  ls_alv_header-row    = 1.
  ls_alv_header-column = 1.
  ls_alv_header-label  = 'X'.
  ls_alv_header-flow   = space.
  APPEND ls_alv_header TO pt_alv_header.
  CLEAR: ls_alv_header.

  ls_alv_header-text   = text-a02. "Balancete contábil
  ls_alv_header-row    = 2.
  ls_alv_header-column = 1.
  ls_alv_header-label  = space.
  ls_alv_header-flow   = 'X'.
  APPEND ls_alv_header TO pt_alv_header.
  CLEAR: ls_alv_header.

  CONCATENATE p_month '/' p_year INTO ls_alv_header-text.
  ls_alv_header-row    = 4.
  ls_alv_header-column = 1.
  ls_alv_header-label  = space.
  ls_alv_header-flow   = 'X'.
  APPEND ls_alv_header TO pt_alv_header.
  CLEAR: ls_alv_header.

ENDFORM.
FORM f_define_cenario.

  DATA: lt_empresas TYPE ace_ds_bukrs_range_t.

  lt_empresas[] = s_bukrs[].

  CASE abap_true.
    WHEN p_f01.
      CREATE OBJECT go_saldo_contas TYPE lcl_account_balance_ativo
        EXPORTING
          iv_ano      = p_year
          iv_mes      = p_month
          iv_empresas = lt_empresas.
    WHEN p_gl035.
      CREATE OBJECT go_saldo_contas TYPE lcl_account_balance_resultado
        EXPORTING
          iv_ano      = p_year
          iv_mes      = p_month
          iv_empresas = lt_empresas.
  ENDCASE.

ENDFORM.
