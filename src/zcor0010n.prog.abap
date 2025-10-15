REPORT zcor0010n.

INCLUDE zcor0010n_t01.
INCLUDE zcor0010n_s01.
INCLUDE zcor0010n_cd01.
INCLUDE zcor0010n_ci01.
INCLUDE zcor0010n_f01.

START-OF-SELECTION.

  PERFORM f_define_cenario.

  gt_saldo_contas[] = go_saldo_contas->build_account_balance( ).


  IF sy-batch IS INITIAL.

    CREATE OBJECT go_saldo_contas_alv.

    "Verificar se p_month é maior que 12, se for maior alterar o valor para 12.

    IF p_month > 12.
      p_month = '12'.

      "Alterar o valor da culuna da ALV para 12.
      LOOP AT gt_saldo_contas ASSIGNING FIELD-SYMBOL(<lw_saldo>).
        <lw_saldo>-month = p_month.
      ENDLOOP.
    ENDIF.


    go_saldo_contas_alv->build_alv( CHANGING
                                     rt_table = gt_saldo_contas ).

    PERFORM f_define_alv_header TABLES gt_alv_header.

    LOOP AT gt_alv_header INTO DATA(ls_alv_header).

      go_saldo_contas_alv->config_alv_header( iv_text   = ls_alv_header-text
                                              iv_row    = ls_alv_header-row
                                              iv_column = ls_alv_header-column
                                              iv_label  = ls_alv_header-label
                                              iv_flow   = ls_alv_header-flow ).

    ENDLOOP.

    go_saldo_contas_alv->config_alv_functions( iv_pf_status = 'SALDO_CONTAS_ALV'
                                               iv_report    = sy-repid
                                               iv_functions = '2' ).

    go_saldo_contas_alv->config_columns( iv_set_optimize        = 'X'
                                         iv_set_striped_pattern = 'X'
                                         iv_set_list_header     = CONV #( TEXT-a01 ) ).

    go_saldo_contas_alv->config_sort_columns( iv_column   = 'RBUKRS'
                                              iv_sequence = if_salv_c_sort=>sort_up ).

    go_saldo_contas_alv->config_sort_columns( iv_column   = 'RACCT'
                                              iv_sequence = if_salv_c_sort=>sort_up ).

    go_saldo_contas_alv->config_events( ).

    go_saldo_contas_alv->display( ).
  ENDIF.

END-OF-SELECTION.
