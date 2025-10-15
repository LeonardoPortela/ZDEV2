REPORT zcor0010.

INCLUDE: zcor0010_t01,
         zcor0010_s01,
         zcor0010_cd01,
         zcor0010_ci01,
         zcor0010_f01.

START-OF-SELECTION.

  PERFORM f_define_cenario.

  gt_saldo_contas[] = go_saldo_contas->build_account_balance( ).

  IF sy-batch IS INITIAL.

    CREATE OBJECT go_saldo_contas_alv.

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
                                         iv_set_list_header     = CONV #( text-a01 ) ).

    go_saldo_contas_alv->config_sort_columns( iv_column   = 'RBUKRS'
                                              iv_sequence = if_salv_c_sort=>sort_up ).

    go_saldo_contas_alv->config_sort_columns( iv_column   = 'RACCT'
                                              iv_sequence = if_salv_c_sort=>sort_up ).

    go_saldo_contas_alv->config_events( ).

    go_saldo_contas_alv->display( ).
  ENDIF.

END-OF-SELECTION.
