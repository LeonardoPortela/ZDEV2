*----------------------------------------------------------------------*
***INCLUDE MZPLANCOMP_0001 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.


  DATA: tl_parametros TYPE ustyp_t_parameters.

  REFRESH: tl_parametros.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  IF vg_dynnr_xxxx IS INITIAL.
    vg_dynnr_xxxx = vg_dynnr_0010.
  ENDIF.

  CASE vg_dynnr_xxxx.
    WHEN vg_dynnr_0010.
      SET PF-STATUS 'PF0001'.
      SET TITLEBAR 'TL0001'.
    WHEN vg_dynnr_0020.
      SET PF-STATUS 'PF0002'.
      SET TITLEBAR 'TL0002'.
    WHEN vg_dynnr_0030.
      READ TABLE t_usermd WITH KEY from = sy-uname.
      IF sy-subrc NE 0.
        APPEND 'VER_LOG' TO fcode.
      ENDIF.
      SET PF-STATUS 'PF0003' EXCLUDING fcode.
      SET TITLEBAR 'TL0003'.
    WHEN vg_dynnr_0040.
      SET PF-STATUS 'PF0004'.
      SET TITLEBAR 'TL0004'.
    WHEN vg_dynnr_0060.

      READ TABLE tl_parametros INTO DATA(wl_parametros) WITH KEY parid = 'ZMEMO00_BLOQ_DUE'.
      IF sy-subrc NE 0 .
        APPEND 'BLOQ_DESBL' TO fcode.
      ENDIF.

      SET PF-STATUS 'PF0006' EXCLUDING fcode.
      SET TITLEBAR 'TL0006'.
    WHEN vg_dynnr_0070.

      READ TABLE tl_parametros TRANSPORTING NO FIELDS WITH KEY parid = 'ZMEMO_CANC_RETDUE'.
      IF sy-subrc IS NOT INITIAL.
        APPEND 'CANCEL_DUE' TO fcode.
        SET PF-STATUS 'PF0007' EXCLUDING fcode.
      ELSE.
        SET PF-STATUS 'PF0007'.
      ENDIF.

      SET TITLEBAR 'TL0007'.

    WHEN vg_dynnr_0073.

      SET PF-STATUS 'PF0073'.
      SET TITLEBAR 'TL0073'.

  ENDCASE.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code_0001.
    WHEN 'PARAM_DIFSALDO'.

      CALL TRANSACTION 'ZMEMO00_PARAM01'.
    WHEN ok_pesq.
      PERFORM consulta_nomeacoes.
      CLEAR: ok_code_0001.
    WHEN ok_ger_mail OR ok_ger_mail_e.
      PERFORM imprimir_nomeacao.
      CLEAR: ok_code_0001.
    WHEN ok_tab01 OR ok_back.
      IF vg_dynnr_xxxx EQ vg_dynnr_0020.
        PERFORM troca_aba_05 USING space.
        PERFORM consulta_due_antecipada.
      ELSEIF vg_dynnr_xxxx EQ vg_dynnr_0030.
        PERFORM troca_aba_02 USING space.
      ELSEIF vg_dynnr_xxxx EQ vg_dynnr_0040.
        PERFORM troca_aba_03 USING space.
      ELSEIF vg_dynnr_xxxx EQ vg_dynnr_0060.
        PERFORM troca_aba_01.
        PERFORM consulta_nomeacoes.
      ELSEIF vg_dynnr_xxxx EQ vg_dynnr_0070.
        PERFORM troca_aba_04 USING space.
      ELSEIF vg_dynnr_xxxx EQ vg_dynnr_0073.  "// wbarbosa 28102024 US-153330
        PERFORM troca_aba_06 USING space. "// wbarbosa 28102024 US-153330
      ENDIF.
      CLEAR: ok_code_0001.
    WHEN ok_tab02.
      PERFORM troca_aba_02 USING space.
      CLEAR: ok_code_0001.
    WHEN ok_tab03.
      IF ( vg_dynnr_xxxx EQ vg_dynnr_0020 ) OR
         ( vg_dynnr_xxxx EQ vg_dynnr_0040 ) OR
         ( vg_dynnr_xxxx EQ vg_dynnr_0070 ).
        PERFORM troca_aba_03 USING space.
      ENDIF.
      CLEAR: ok_code_0001.
    WHEN ok_tab04.
      IF ( vg_dynnr_xxxx EQ vg_dynnr_0030 ) OR ( vg_dynnr_xxxx EQ vg_dynnr_0070 ).
        PERFORM troca_aba_04 USING space.
      ENDIF.
      CLEAR: ok_code_0001.
    WHEN ok_tab05.
      PERFORM troca_aba_05 USING space.
      CLEAR: ok_code_0001.
    WHEN ok_tab06.
      IF vg_dynnr_xxxx EQ vg_dynnr_0040 OR
        ( vg_dynnr_xxxx EQ vg_dynnr_0073 ). "// wbarbosa 28102024 US-153330
        PERFORM troca_aba_06 USING space.
      ENDIF.
      CLEAR: ok_code_0001.

*      "// wbarbosa 28102024 US-153330
    WHEN ok_tab07.

      IF vg_dynnr_xxxx EQ vg_dynnr_0070.
        PERFORM troca_aba_07 USING space.
      ENDIF.

      CLEAR: ok_code_0001.
*      "// wbarbosa 28102024 US-153330
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.
  PERFORM libera_nomeacoes.
  LEAVE PROGRAM.
ENDMODULE.                 " USER_COMMAND_0001_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_NOMEACAO  OUTPUT
*&---------------------------------------------------------------------*
*       Criar alv nomeçãoes
*----------------------------------------------------------------------*
MODULE cria_alv_nomeacao OUTPUT.

  PERFORM plan_cria_nomeacoes_alv.

ENDMODULE.                 " CRIA_ALV_NOMEACAO  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  CONSULTA_NOMEACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consulta_nomeacoes.

  CLEAR: it_znom_transporte[],
         it_znom_transporte_alv[],
         it_znom_reme_dnotas_alv[],
         it_znom_prog_reme[],
         it_zdoc_exp[],
         it_zdoc_nf_produtor[].


  " Usuários que podem mudar o Status
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZMEMO00'
    TABLES
      set_values    = t_usermd
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_usermd BY from.

  LOOP AT t_nr_mes.

    IF NOT t_nr_mes-low IS INITIAL .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = t_nr_mes-low
        IMPORTING
          output = t_nr_mes-low.
    ENDIF.

    IF NOT t_nr_mes-high IS INITIAL .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = t_nr_mes-high
        IMPORTING
          output = t_nr_mes-high.
    ENDIF.

    MODIFY t_nr_mes INDEX sy-tabix.

  ENDLOOP.

  SELECT * INTO TABLE it_znom_transporte
    FROM znom_transporte
   "WHERE ID_NOMEACAO_TRAN IN T_IDNOME
    WHERE nr_ano           IN t_nr_ano
     AND  nr_mes           IN t_nr_mes
     AND  in_status_comex  EQ t_cancs
     AND  ds_porto         IN t_porto
     AND  ds_nome_transpor IN t_trans
     AND  bukrs            IN t_emp.


  IF sy-subrc IS INITIAL.

    SELECT * INTO TABLE it_znom_programacao
      FROM znom_programacao
       FOR ALL ENTRIES IN it_znom_transporte
     WHERE id_nomeacao_tran EQ it_znom_transporte-id_nomeacao_tran.

    SELECT * INTO TABLE it_znom_remetente
      FROM znom_remetente
       FOR ALL ENTRIES IN it_znom_transporte
     WHERE id_nomeacao_tran EQ it_znom_transporte-id_nomeacao_tran.

    CLEAR: it_znom_reme_notas[].
    IF it_znom_remetente[] IS NOT INITIAL.
      SELECT * INTO TABLE it_znom_reme_notas
        FROM znom_reme_notas
         FOR ALL ENTRIES IN it_znom_remetente
       WHERE id_nomeacao_tran EQ it_znom_remetente-id_nomeacao_tran
         AND id_empresa       EQ it_znom_remetente-id_empresa
         AND id_filial        EQ it_znom_remetente-id_filial
         AND id_material      EQ it_znom_remetente-id_material
         AND id_remetente     EQ it_znom_remetente-id_remetente
         AND grp_retorno      EQ it_znom_remetente-grp_retorno.

      SORT it_znom_reme_notas                                   BY id_nomeacao_tran id_empresa id_filial id_material id_remetente docnum itmnum grp_retorno.
      DELETE ADJACENT DUPLICATES FROM it_znom_reme_notas COMPARING id_nomeacao_tran id_empresa id_filial id_material id_remetente docnum itmnum grp_retorno.
    ENDIF.

    SELECT * INTO TABLE it_znom_prog_reme
      FROM znom_prog_reme
       FOR ALL ENTRIES IN it_znom_transporte
     WHERE id_nomeacao_tran EQ it_znom_transporte-id_nomeacao_tran.

    IF NOT it_znom_prog_reme[] IS INITIAL.

      SELECT * INTO TABLE it_zdoc_exp
        FROM zdoc_exp
         FOR ALL ENTRIES IN it_znom_prog_reme
       WHERE vbeln            = it_znom_prog_reme-id_remessa
         AND id_registro_expo = it_znom_prog_reme-id_registro_expo.

      IF NOT it_zdoc_exp[] IS INITIAL.
        SELECT * INTO TABLE it_zdoc_nf_produtor
          FROM zdoc_nf_produtor
           FOR ALL ENTRIES IN it_zdoc_exp
         WHERE vbeln = it_zdoc_exp-vbeln.
      ENDIF.

    ENDIF.

  ENDIF.

  LOOP AT it_znom_transporte INTO wa_znom_transporte.
    CLEAR: wa_znom_transporte_alv.
    MOVE-CORRESPONDING wa_znom_transporte TO wa_znom_transporte_alv.

**** Programada **************************************************************
*************************************************************************************************
    wa_znom_transporte_alv-nr_qtde_programada = 0.
    wa_znom_transporte_alv-nr_qtde_saldo_prog = 0.
    LOOP AT it_znom_programacao INTO wa_znom_programacao WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran.
      wa_znom_transporte_alv-nr_qtde_programada = wa_znom_transporte_alv-nr_qtde_programada + wa_znom_programacao-nr_programada.
    ENDLOOP.
    wa_znom_transporte_alv-nr_qtde_saldo_prog = wa_znom_transporte_alv-nr_qtde_nomeada -  wa_znom_transporte_alv-nr_qtde_programada.
**** Programada **************************************************************


**** Remetente ***************************************************************
*************************************************************************************************
    wa_znom_transporte_alv-nr_qtde_remetente  = 0.
    wa_znom_transporte_alv-nr_qtde_saldo_rem  = 0.
    LOOP AT it_znom_remetente INTO wa_znom_remetente WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran.
      wa_znom_transporte_alv-nr_qtde_remetente = wa_znom_transporte_alv-nr_qtde_remetente + wa_znom_remetente-nr_programada.
    ENDLOOP.
    wa_znom_transporte_alv-nr_qtde_saldo_rem  = wa_znom_transporte_alv-nr_qtde_nomeada - wa_znom_transporte_alv-nr_qtde_remetente.
**** Remetente ***************************************************************


**** Planejada **************************************************************
*************************************************************************************************
    wa_znom_transporte_alv-nr_qtde_planejada = 0.
    wa_znom_transporte_alv-nr_qtde_saldo_pla = 0.
    wa_znom_transporte_alv-nr_qtde_plan_fili = 0.
    LOOP AT it_znom_reme_notas INTO wa_znom_reme_notas WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran.
      wa_znom_transporte_alv-nr_qtde_planejada = wa_znom_transporte_alv-nr_qtde_planejada +
      wa_znom_reme_notas-nr_quantidade.
    ENDLOOP.
    LOOP AT it_znom_remetente INTO wa_znom_remetente WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran
                                                       AND id_remetente     EQ space.
      wa_znom_transporte_alv-nr_qtde_planejada = wa_znom_transporte_alv-nr_qtde_planejada + wa_znom_remetente-nr_programada.
      wa_znom_transporte_alv-nr_qtde_plan_fili = wa_znom_transporte_alv-nr_qtde_plan_fili + wa_znom_remetente-nr_programada.
    ENDLOOP.
    wa_znom_transporte_alv-nr_qtde_saldo_pla = wa_znom_transporte_alv-nr_qtde_nomeada - wa_znom_transporte_alv-nr_qtde_planejada.
**** Planejada **************************************************************


**** Efetivada **************************************************************
*************************************************************************************************
    wa_znom_transporte_alv-nr_efetivada      = 0.
    wa_znom_transporte_alv-nr_saldo_efetivar = 0.
    LOOP AT it_znom_prog_reme INTO wa_znom_prog_reme WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran.
      LOOP AT it_zdoc_nf_produtor INTO wa_zdoc_nf_produtor WHERE vbeln EQ wa_znom_prog_reme-id_remessa.
        wa_znom_transporte_alv-nr_efetivada = wa_znom_transporte_alv-nr_efetivada + wa_zdoc_nf_produtor-menge.
      ENDLOOP.
    ENDLOOP.
    wa_znom_transporte_alv-nr_efetivada      = wa_znom_transporte_alv-nr_efetivada + wa_znom_transporte_alv-nr_qtde_plan_fili.
    wa_znom_transporte_alv-nr_saldo_efetivar = wa_znom_transporte_alv-nr_qtde_nomeada - wa_znom_transporte_alv-nr_efetivada.
**** Efetivada **************************************************************


    LOOP AT it_znom_programacao INTO wa_znom_programacao WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran.
      IF NOT wa_znom_programacao-id_cliente IS INITIAL.
        wa_znom_transporte_alv-nr_qtde_remetente = wa_znom_transporte_alv-nr_qtde_remetente + wa_znom_programacao-nr_programada.
        wa_znom_transporte_alv-nr_qtde_saldo_rem = wa_znom_transporte_alv-nr_qtde_saldo_rem - wa_znom_programacao-nr_programada.
        wa_znom_transporte_alv-nr_qtde_planejada = wa_znom_transporte_alv-nr_qtde_planejada + wa_znom_programacao-nr_programada.
        wa_znom_transporte_alv-nr_qtde_saldo_pla = wa_znom_transporte_alv-nr_qtde_saldo_pla - wa_znom_programacao-nr_programada.
        wa_znom_transporte_alv-nr_efetivada      = wa_znom_transporte_alv-nr_efetivada      + wa_znom_programacao-nr_programada.
        wa_znom_transporte_alv-nr_saldo_efetivar = wa_znom_transporte_alv-nr_saldo_efetivar - wa_znom_programacao-nr_programada.
      ENDIF.
    ENDLOOP.


    IF wa_znom_transporte_alv-nr_qtde_programada EQ 0.

      wa_znom_transporte_alv-status = icon_led_red.
      wa_znom_transporte_alv-ds_status = 'Sem Programação'.

    ELSEIF wa_znom_transporte_alv-nr_qtde_programada GT 0.

      wa_znom_transporte_alv-status = icon_alert.
      wa_znom_transporte_alv-ds_status = 'Com Programação'.

      IF wa_znom_transporte_alv-nr_qtde_saldo_prog EQ 0.
        wa_znom_transporte_alv-status    = icon_led_yellow.
        wa_znom_transporte_alv-ds_status = 'A Planejar'.

        IF ( wa_znom_transporte_alv-nr_qtde_saldo_rem GT 0 ) OR ( wa_znom_transporte_alv-nr_qtde_saldo_pla GT 0 ).
          wa_znom_transporte_alv-status = icon_led_yellow.
          wa_znom_transporte_alv-ds_status = 'A Planejar'.
        ELSEIF ( wa_znom_transporte_alv-nr_qtde_saldo_rem EQ 0 ) AND ( wa_znom_transporte_alv-nr_qtde_saldo_pla EQ 0 ).
          wa_znom_transporte_alv-status = icon_led_green.
          wa_znom_transporte_alv-ds_status = 'Planejado'.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND wa_znom_transporte_alv TO it_znom_transporte_alv.
  ENDLOOP.

ENDFORM. " CONSULTA_NOMEACOES

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_NOMEACOES_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_nomeacoes_alv .

  CONSTANTS: tabela_nomeacao TYPE string VALUE 'IT_ZNOM_TRANSPORTE_ALV'.

  DATA: text_n000 TYPE c LENGTH 50 VALUE 'Status',
        text_n016 TYPE c LENGTH 50 VALUE 'Desc. Status',
        text_n001 TYPE c LENGTH 50 VALUE 'Id. Nomeação',
        text_n002 TYPE c LENGTH 50 VALUE 'Ano',
        text_n003 TYPE c LENGTH 50 VALUE 'Mês',
        text_n004 TYPE c LENGTH 50 VALUE 'Porto',
        text_n005 TYPE c LENGTH 50 VALUE 'Terminal',
        text_n006 TYPE c LENGTH 50 VALUE 'Id. Transporte',
        "TEXT_N007 TYPE C LENGTH 50 VALUE 'Nome Transporte',
        text_n007 TYPE c LENGTH 50 VALUE 'Navio',
        text_n008 TYPE c LENGTH 50 VALUE 'Qtd. Nomeada',
        text_n009 TYPE c LENGTH 50 VALUE 'Nr. Viagem',
        text_n010 TYPE c LENGTH 50 VALUE 'Qtd. Planejada',
        text_n019 TYPE c LENGTH 50 VALUE 'Qtd. Filial',
        text_n011 TYPE c LENGTH 50 VALUE 'Qtd. Saldo Planejar',
        text_n012 TYPE c LENGTH 50 VALUE 'Qtd. Remetente',
        text_n013 TYPE c LENGTH 50 VALUE 'Qtd. Saldo Remetente',
        text_n014 TYPE c LENGTH 50 VALUE 'Qtd. Programada',
        text_n015 TYPE c LENGTH 50 VALUE 'Qtd. Saldo Programar',
        text_n017 TYPE c LENGTH 50 VALUE 'Qtd. Efetivada',
        text_n018 TYPE c LENGTH 50 VALUE 'Qtd. Efetivar'.

  IF plan_prim_nomeacao IS INITIAL.

    CREATE OBJECT plan_container_nomeacao
      EXPORTING
        container_name = 'CTN_NOMEACAO'.

    CREATE OBJECT plan_alv_nomeacao
      EXPORTING
        i_parent = plan_container_nomeacao.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_nomeacao USING:
        tabela_nomeacao 'STATUS'             text_n000 'X' 01 03 space space space 'X'   space space             space,
        tabela_nomeacao 'DS_STATUS'          text_n016 ' ' 02 15 space space space 'X'   'X'   c_grid_color_c200 space,
        tabela_nomeacao 'ID_NOMEACAO_TRAN'   text_n001 ' ' 03 06 space space space space space space             space,
        tabela_nomeacao 'NR_ANO'             text_n002 ' ' 04 04 space space space space space space             space,
        tabela_nomeacao 'NR_MES'             text_n003 ' ' 05 04 space space space space space space             space,
        tabela_nomeacao 'DS_PORTO'           text_n004 ' ' 06 20 space space space space space space             space,
        tabela_nomeacao 'DS_TERMINAL'        text_n005 ' ' 07 20 space space space space space space             space,
        tabela_nomeacao 'ID_TRANSPORTE'      text_n006 ' ' 08 10 space space space space space space             space,
        tabela_nomeacao 'DS_NOME_TRANSPOR'   text_n007 ' ' 09 20 space space space space space space             space,
        tabela_nomeacao 'NR_VIAGEM'          text_n009 ' ' 10 05 space space space space space space             space,
        tabela_nomeacao 'NR_QTDE_NOMEADA'    text_n008 ' ' 11 15 space space 'X'   space space c_grid_color_c300 space,
        tabela_nomeacao 'NR_QTDE_PROGRAMADA' text_n014 ' ' 12 15 space space 'X'   space space c_grid_color_c400 space,
        tabela_nomeacao 'NR_QTDE_SALDO_PROG' text_n015 ' ' 13 15 space space 'X'   space space c_grid_color_c500 space,
        tabela_nomeacao 'NR_QTDE_REMETENTE'  text_n012 ' ' 14 15 space space 'X'   space space c_grid_color_c400 space,
        tabela_nomeacao 'NR_QTDE_SALDO_REM'  text_n013 ' ' 15 15 space space 'X'   space space c_grid_color_c500 space,
        tabela_nomeacao 'NR_QTDE_PLANEJADA'  text_n010 ' ' 16 15 space space 'X'   space space c_grid_color_c400 space,
        tabela_nomeacao 'NR_QTDE_PLAN_FILI'  text_n019 ' ' 17 15 space space 'X'   space space c_grid_color_c700 space,
        tabela_nomeacao 'NR_QTDE_SALDO_PLA'  text_n011 ' ' 18 15 space space 'X'   space space c_grid_color_c500 space,
        tabela_nomeacao 'NR_EFETIVADA'       text_n017 ' ' 19 15 space space 'X'   space space c_grid_color_c600 space,
        tabela_nomeacao 'NR_SALDO_EFETIVAR'  text_n018 ' ' 20 15 space space 'X'   space space c_grid_color_c500 space.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra    = c_x.
    plan_gs_layout-sel_mode = space.

    CALL METHOD plan_alv_nomeacao->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_nomeacao
        it_outtab       = it_znom_transporte_alv[].

*   Create Object for Event Handler
    CREATE OBJECT plan_event_handler.
    SET HANDLER plan_event_handler->handle_hotspot_click FOR plan_alv_nomeacao.
    SET HANDLER plan_event_handler->on_double_click FOR plan_alv_nomeacao.

    plan_prim_nomeacao = c_x.
  ENDIF.

  CALL METHOD plan_alv_nomeacao->refresh_table_display.

  CALL METHOD plan_alv_nomeacao->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_NOMEACOES_ALV

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.

ENDMODULE.                 " STATUS_0002  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_NOMEACAO
*&---------------------------------------------------------------------*
*       Seleciona Nomeação a ser programada
*----------------------------------------------------------------------*
FORM verifica_selecao_nomeacao  USING  vg_verifica_selecao TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_znom_transporte_alv.

  CALL METHOD plan_alv_nomeacao->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_znom_transporte_alv INTO wa_znom_transporte_alv INDEX wa_selected_rows-index.
    READ TABLE it_znom_transporte     INTO wa_znom_transporte     WITH KEY id_nomeacao_tran = wa_znom_transporte_alv-id_nomeacao_tran.
  ENDLOOP.

  IF NOT wa_znom_transporte_alv IS INITIAL.
    vg_verifica_selecao = 0.
  ELSE.
    vg_verifica_selecao = 1.
  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_NOMEACAO

*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_02
*&---------------------------------------------------------------------*
*       Troca para ABA 02
*----------------------------------------------------------------------*
FORM troca_aba_02 USING forcar TYPE c.

  DATA: vg_verifica_selecao_nm TYPE sy-subrc.

  DATA: BEGIN OF wa_enq.
          INCLUDE STRUCTURE seqg7.
  DATA: END OF wa_enq.

  DATA: garg LIKE seqg3-garg,
        enq  LIKE STANDARD TABLE OF wa_enq.

  PERFORM verifica_selecao_nomeacao USING vg_verifica_selecao_nm.

  IF vg_verifica_selecao_nm IS INITIAL.

    IF vg_desbloq_nom IS INITIAL.

      CALL FUNCTION 'ZENQUEUE_ZNOMEACAO'
        EXPORTING
          id_nomeacao_tran = wa_znom_transporte_alv-id_nomeacao_tran
        EXCEPTIONS
          foreign_lock     = 1
          system_failure   = 2
          OTHERS           = 3.

      CASE sy-subrc.
        WHEN 1.
          CONCATENATE sy-mandt wa_znom_transporte_alv-id_nomeacao_tran INTO garg.

          CALL FUNCTION 'ENQUE_READ2'
            EXPORTING
              gname  = 'ZNOM_TRANSPORTE'
              garg   = garg
              guname = '*'
            TABLES
              enq    = enq.

          READ TABLE enq INTO wa_enq WITH KEY gname = 'ZNOM_TRANSPORTE'.

          MESSAGE s032 WITH 'Nomeação' wa_znom_transporte_alv-id_nomeacao_tran 'bloqueado por usuário' wa_enq-guname.
          EXIT.
        WHEN 2.
          MESSAGE s032 WITH 'Erro em bloqueio!'.
          EXIT.
      ENDCASE.
      vg_desbloq_nom = c_x.
    ENDIF.

    vg_dynnr_xxxx    = vg_dynnr_0020.
    tabpag-activetab = ok_tab02.
    CLEAR: wa_filtro_remetente-nr_qtd_vinc.

    PERFORM consulta_programacoes.

  ELSE.
    MESSAGE s001.
  ENDIF.

ENDFORM.                    " TROCA_ABA_02

FORM troca_aba_05 USING forcar TYPE c.

  DATA: tg_znom_remetente_aux  TYPE TABLE OF znom_remetente  WITH HEADER LINE,
        tg_znom_reme_notas_aux TYPE TABLE OF znom_reme_notas WITH HEADER LINE.

  DATA: wl_znom_rem_aux TYPE znom_remetente.

  DATA: vg_verifica_selecao_nm TYPE sy-subrc.

  DATA: BEGIN OF wa_enq.
          INCLUDE STRUCTURE seqg7.
  DATA: END OF wa_enq.

  DATA: garg LIKE seqg3-garg,
        enq  LIKE STANDARD TABLE OF wa_enq.

  PERFORM verifica_selecao_nomeacao USING vg_verifica_selecao_nm.

  IF vg_verifica_selecao_nm IS INITIAL.

    IF vg_desbloq_nom IS INITIAL.

      CALL FUNCTION 'ZENQUEUE_ZNOMEACAO'
        EXPORTING
          id_nomeacao_tran = wa_znom_transporte_alv-id_nomeacao_tran
        EXCEPTIONS
          foreign_lock     = 1
          system_failure   = 2
          OTHERS           = 3.

      CASE sy-subrc.
        WHEN 1.
          CONCATENATE sy-mandt wa_znom_transporte_alv-id_nomeacao_tran INTO garg.

          CALL FUNCTION 'ENQUE_READ2'
            EXPORTING
              gname  = 'ZNOM_TRANSPORTE'
              garg   = garg
              guname = '*'
            TABLES
              enq    = enq.

          READ TABLE enq INTO wa_enq WITH KEY gname = 'ZNOM_TRANSPORTE'.

          MESSAGE s032 WITH 'Nomeação' wa_znom_transporte_alv-id_nomeacao_tran 'bloqueado por usuário' wa_enq-guname.
          EXIT.
        WHEN 2.
          MESSAGE s032 WITH 'Erro em bloqueio!'.
          EXIT.
      ENDCASE.
      vg_desbloq_nom = c_x.
    ENDIF.

    vg_dynnr_xxxx    = vg_dynnr_0060.
    tabpag-activetab = ok_tab05.

    PERFORM consulta_due_antecipada.

  ELSE.
    MESSAGE s001.
  ENDIF.

ENDFORM.                    " TROCA_ABA_05

FORM troca_aba_06 USING forcar TYPE c.

  LOOP AT it_znom_programacao_alv WHERE id_nomeacao_tran EQ wa_znom_programacao-id_nomeacao_tran
                                    AND id_empresa       EQ wa_znom_programacao-id_empresa
                                    AND id_material      EQ wa_znom_programacao-id_material.

    IF it_znom_programacao_alv-nr_qtde_saldo_pla > 0.
      MESSAGE 'Existem programações com saldo a Planejar!' TYPE 'I'.
      RETURN.
    ENDIF.

    IF it_znom_programacao_alv-nr_saldo_efetivar > 0.
      MESSAGE 'Existem programações com saldo a Efetivar!' TYPE 'I'.
      RETURN.
    ENDIF.

  ENDLOOP.

  vg_dynnr_xxxx    = vg_dynnr_0070.
  tabpag-activetab = ok_tab06.
  PERFORM consulta_due_retificacao.

ENDFORM.                    " TROCA_ABA_06



FORM consulta_due_antecipada.

  DATA: it_znom_programacao_aux TYPE TABLE OF znom_programacao WITH HEADER LINE,
        it_zsdt0172             TYPE TABLE OF zsdt0172 WITH HEADER LINE,
        it_zsdt0174             TYPE TABLE OF zsdt0174 WITH HEADER LINE,
        it_t001                 TYPE TABLE OF t001 WITH HEADER LINE,
        it_j_1bbranch           TYPE TABLE OF j_1bbranch WITH HEADER LINE,
        it_makt                 TYPE TABLE OF makt WITH HEADER LINE,
        it_kna1                 TYPE TABLE OF kna1 WITH HEADER LINE.

  CLEAR: it_due_antecipada_alv[],
         it_due_antecipada[],
         it_zsdt0172[],
         it_zsdt0174[].

  CHECK wa_znom_transporte-id_nomeacao_tran IS NOT INITIAL.

  SELECT *
    FROM zsdt0170 INTO TABLE it_due_antecipada
   WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran
     AND id_due_ref       EQ 0. "Não é retificação

  DELETE it_due_antecipada WHERE lcto_avulso EQ abap_true AND loekz EQ abap_true.    "SD - Bug 166585 Correção Listagem DU-e Marcada Eliminação - WPP

  CHECK it_due_antecipada[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0172 INTO TABLE it_zsdt0172
    FOR ALL ENTRIES IN it_due_antecipada
   WHERE id_due     EQ it_due_antecipada-id_due.

  SELECT *
    FROM zsdt0174 INTO TABLE it_zsdt0174
    FOR ALL ENTRIES IN it_due_antecipada
   WHERE id_due     EQ it_due_antecipada-id_due.

  LOOP AT it_due_antecipada INTO wa_due_antecipada.

    CLEAR: wa_due_antecipada_alv.

    MOVE-CORRESPONDING wa_due_antecipada TO wa_due_antecipada_alv.

    CLEAR: wa_due_antecipada_alv-qtde_ue_exportada,
           wa_due_antecipada_alv-peso_liq_total.

    LOOP AT it_zsdt0172 WHERE id_due EQ wa_due_antecipada-id_due.

      ADD it_zsdt0172-qtde_ue_exportada TO wa_due_antecipada_alv-qtde_ue_exportada.
      ADD it_zsdt0172-peso_liq_total    TO wa_due_antecipada_alv-peso_liq_total.

      wa_due_antecipada_alv-ue_exportada = it_zsdt0172-ue_exportada.

      wa_due_antecipada_alv-codigo_ncm           = it_zsdt0172-codigo_ncm.
      wa_due_antecipada_alv-preco_ton            = it_zsdt0172-preco_ton.
      wa_due_antecipada_alv-codigo_enquadramento = it_zsdt0172-codigo_enquadramento.
      wa_due_antecipada_alv-matnr                = it_zsdt0172-matnr.
      wa_due_antecipada_alv-codigo_cond_venda    = it_zsdt0172-codigo_cond_venda. "CS2019001113

      SELECT SINGLE *
        FROM makt INTO @DATA(_wl_makt)
       WHERE matnr EQ @it_zsdt0172-matnr
         AND spras EQ @sy-langu.

      IF sy-subrc EQ 0.
        wa_due_antecipada_alv-maktx = _wl_makt-maktx.
      ENDIF.

      LOOP AT it_zsdt0174 WHERE id_due      EQ it_zsdt0172-id_due
                            AND id_due_item EQ it_zsdt0172-id_due_item.
        IF wa_due_antecipada_alv-pais_dest IS INITIAL.
          wa_due_antecipada_alv-pais_dest = it_zsdt0174-destino_country.
        ELSE.
          CONCATENATE wa_due_antecipada_alv-pais_dest ',' it_zsdt0174-destino_country
                 INTO wa_due_antecipada_alv-pais_dest.
        ENDIF.
      ENDLOOP.

      EXIT.
    ENDLOOP.

    IF wa_due_antecipada_alv-bloqueio_interno EQ abap_true.
      wa_due_antecipada_alv-bloq_desbloq = icon_locked.
    ELSE.
      wa_due_antecipada_alv-bloq_desbloq = icon_unlocked.
    ENDIF.

    APPEND wa_due_antecipada_alv TO it_due_antecipada_alv.
  ENDLOOP.

  SORT it_due_antecipada_alv BY id_due.

ENDFORM.

FORM consulta_due_retificacao.

  DATA: it_znom_prog_reme_aux TYPE TABLE OF ty_zplac_nom_prog_reme WITH HEADER LINE,
        it_zsdt0183           TYPE TABLE OF zsdt0183 WITH HEADER LINE,
        it_zsdt0170           TYPE TABLE OF zsdt0170 WITH HEADER LINE,
        it_zsdt0172           TYPE TABLE OF zsdt0172 WITH HEADER LINE,
        lt_nf_exp_due         TYPE zde_nf_exp_due_t.

  CLEAR: it_due_retificacao_alv[],
         it_due_retificacao[],
         it_due_ret_conf_alv[],
         it_znom_prog_reme_aux[],
         it_zsdt0170[],
         it_zsdt0172[].

  PERFORM consulta_remessas USING abap_true.

  LOOP AT it_znom_prog_reme_alv WHERE id_nomeacao_tran EQ wa_znom_programacao-id_nomeacao_tran
                                  AND id_empresa       EQ wa_znom_programacao-id_empresa
                                  AND id_material      EQ wa_znom_programacao-id_material.

    CLEAR: it_znom_prog_reme_aux.
    APPEND it_znom_prog_reme_alv TO it_znom_prog_reme_aux.
  ENDLOOP.

  DELETE it_znom_prog_reme_aux WHERE id_due IS INITIAL.
  SORT it_znom_prog_reme_aux BY id_due.
  DELETE ADJACENT DUPLICATES FROM it_znom_prog_reme_aux COMPARING id_due.

  CHECK it_znom_prog_reme_aux[] IS NOT INITIAL.

  "Monta Saida Conferencia
  LOOP AT it_znom_prog_reme_aux.
    CLEAR: it_due_ret_conf_alv.

    it_due_ret_conf_alv-id_nomeacao_tran      = it_znom_prog_reme_aux-id_nomeacao_tran.

    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE id_due = @it_znom_prog_reme_aux-id_due.

    CHECK sy-subrc = 0.

    it_due_ret_conf_alv-codigo_urf_embarque   = _wl_0170-codigo_urf_embarque.
    it_due_ret_conf_alv-codigo_ra_embarque    = _wl_0170-codigo_ra_embarque.
    it_due_ret_conf_alv-bukrs                 = _wl_0170-bukrs.
    it_due_ret_conf_alv-matnr                 = wa_znom_programacao-id_material.

    CLEAR: it_zsdt0183[].
    SELECT *
      FROM zsdt0183 INTO TABLE it_zsdt0183
     WHERE id_nomeacao_tran   = it_due_ret_conf_alv-id_nomeacao_tran
       AND bukrs              = it_due_ret_conf_alv-bukrs
       AND codigo_ra_embarque = it_due_ret_conf_alv-codigo_ra_embarque
       AND matnr              = it_due_ret_conf_alv-matnr.

    LOOP AT it_zsdt0183.
      it_due_ret_conf_alv-dt_registro    = it_zsdt0183-dt_registro.
      it_due_ret_conf_alv-hr_registro    = it_zsdt0183-hr_registro.
      it_due_ret_conf_alv-us_registro    = it_zsdt0183-us_registro.
      ADD it_zsdt0183-peso_liq_total  TO it_due_ret_conf_alv-peso_liq_total.
    ENDLOOP.

    APPEND it_due_ret_conf_alv.
  ENDLOOP.

  "Monta Saida Conferencia

  SORT it_due_ret_conf_alv                                   BY id_nomeacao_tran bukrs codigo_ra_embarque matnr.
  DELETE ADJACENT DUPLICATES FROM it_due_ret_conf_alv COMPARING id_nomeacao_tran bukrs codigo_ra_embarque matnr.

  SELECT *
    FROM zsdt0170 INTO TABLE it_due_retificacao
     FOR ALL ENTRIES IN it_znom_prog_reme_aux
   WHERE id_due      EQ it_znom_prog_reme_aux-id_due
     AND id_due_ref  EQ 0  "Não é retificação
     AND loekz       EQ abap_false.

  CHECK it_due_retificacao[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0172 INTO TABLE it_zsdt0172
     FOR ALL ENTRIES IN it_due_retificacao
   WHERE id_due  EQ it_due_retificacao-id_due.

  "Consulta Registro DU-e Retificação
  SELECT *
    FROM zsdt0170 APPENDING TABLE it_zsdt0170
     FOR ALL ENTRIES IN it_znom_prog_reme_aux
   WHERE id_due_ref  EQ it_znom_prog_reme_aux-id_due
     AND loekz       EQ abap_false.

  SELECT *
    FROM zsdt0172 APPENDING TABLE it_zsdt0172
     FOR ALL ENTRIES IN it_zsdt0170
   WHERE id_due EQ it_zsdt0170-id_due.

  SORT it_zsdt0172 BY id_due id_due_item.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0172 COMPARING id_due id_due_item.

  LOOP AT it_due_retificacao INTO wa_due_retificacao.

    CLEAR: wa_due_retificacao_alv.

    MOVE-CORRESPONDING wa_due_retificacao TO wa_due_retificacao_alv.

    CLEAR: wa_due_retificacao_alv-qtde_ue_exportada.
    LOOP AT it_zsdt0172 WHERE id_due EQ wa_due_retificacao_alv-id_due.
      ADD it_zsdt0172-qtde_ue_exportada TO wa_due_retificacao_alv-qtde_ue_exportada.

      wa_due_retificacao_alv-ue_exportada = it_zsdt0172-ue_exportada.
      wa_due_retificacao_alv-matnr        = it_zsdt0172-matnr.

      ADD it_zsdt0172-peso_liq_total TO wa_due_retificacao_alv-peso_liq_total.
    ENDLOOP.

    "Atribuir DU-e Retificação
    READ TABLE it_zsdt0170 WITH KEY id_due_ref = wa_due_retificacao_alv-id_due.
    IF sy-subrc = 0.
      wa_due_retificacao_alv-id_due_ret   = it_zsdt0170-id_due.
      LOOP AT it_zsdt0172 WHERE id_due EQ wa_due_retificacao_alv-id_due_ret.
        ADD it_zsdt0172-peso_liq_total TO wa_due_retificacao_alv-peso_liq_tot_ret.
      ENDLOOP.
    ELSE.
      CALL FUNCTION 'ZDUE_DADOS_RETIFICAR'
        EXPORTING
          i_id_due             = wa_due_retificacao_alv-id_due
        IMPORTING
          e_peso_liq_retificar = wa_due_retificacao_alv-peso_liq_tot_ret.
    ENDIF.

    CALL FUNCTION 'ZDUE_DADOS_RETIFICAR'
      EXPORTING
        i_id_due     = wa_due_retificacao_alv-id_due
      TABLES
        t_nf_exp_due = lt_nf_exp_due.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_nf_exp_due ASSIGNING FIELD-SYMBOL(<fs_nf_exp_due>).
        wa_due_retificacao_alv-nf_exp = wa_due_retificacao_alv-nf_exp + <fs_nf_exp_due>-menge.
      ENDLOOP.
    ENDIF.

    APPEND wa_due_retificacao_alv TO it_due_retificacao_alv.

  ENDLOOP.


  SORT it_due_retificacao_alv BY id_due.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_PROGRAMACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consulta_programacoes .

  DATA: it_znom_programacao_aux TYPE TABLE OF znom_programacao WITH HEADER LINE,
        it_t001                 TYPE TABLE OF t001 WITH HEADER LINE,
        it_j_1bbranch           TYPE TABLE OF j_1bbranch WITH HEADER LINE,
        it_makt                 TYPE TABLE OF makt WITH HEADER LINE,
        it_kna1                 TYPE TABLE OF kna1 WITH HEADER LINE.

  DATA: it_zdoc_exp_recusa_cons  TYPE TABLE OF zdoc_exp_recusa WITH HEADER LINE.

  CLEAR: it_znom_programacao_alv[],
         it_znom_programacao[],
         it_znom_remetente[],
         it_znom_reme_notas[],
         it_znom_prog_reme[],
         it_zdoc_exp[],
         it_zdoc_exp_recusa_cons[],
         it_zdoc_nf_produtor[].

  SELECT * INTO TABLE it_znom_programacao
    FROM znom_programacao
   WHERE id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran.

  IF sy-subrc IS INITIAL.

    SELECT * INTO TABLE it_znom_remetente
      FROM znom_remetente
       FOR ALL ENTRIES IN it_znom_programacao
     WHERE id_nomeacao_tran EQ it_znom_programacao-id_nomeacao_tran.

    IF it_znom_remetente[] IS NOT INITIAL.
      SELECT * INTO TABLE it_znom_reme_notas
        FROM znom_reme_notas
         FOR ALL ENTRIES IN it_znom_remetente
       WHERE id_nomeacao_tran EQ it_znom_remetente-id_nomeacao_tran
         AND id_empresa       EQ it_znom_remetente-id_empresa
         AND id_filial        EQ it_znom_remetente-id_filial
         AND id_material      EQ it_znom_remetente-id_material
         AND id_remetente     EQ it_znom_remetente-id_remetente
         AND grp_retorno      EQ it_znom_remetente-grp_retorno.

      SORT it_znom_reme_notas                                   BY id_nomeacao_tran id_empresa id_filial id_material id_remetente docnum itmnum grp_retorno.
      DELETE ADJACENT DUPLICATES FROM it_znom_reme_notas COMPARING id_nomeacao_tran id_empresa id_filial id_material id_remetente docnum itmnum grp_retorno.
    ENDIF.

    MOVE it_znom_programacao[] TO it_znom_programacao_aux[].
    SORT it_znom_programacao_aux BY id_empresa.
    DELETE ADJACENT DUPLICATES FROM it_znom_programacao_aux COMPARING id_empresa.

    SELECT * INTO TABLE it_t001
      FROM t001
      FOR ALL ENTRIES IN it_znom_programacao_aux
     WHERE bukrs EQ it_znom_programacao_aux-id_empresa.

    CLEAR: it_znom_programacao_aux.
    MOVE it_znom_programacao[] TO it_znom_programacao_aux[].
    SORT it_znom_programacao_aux BY id_empresa id_filial.
    DELETE ADJACENT DUPLICATES FROM it_znom_programacao_aux COMPARING id_empresa id_filial.

    SELECT * INTO TABLE it_j_1bbranch
      FROM j_1bbranch
      FOR ALL ENTRIES IN it_znom_programacao_aux
     WHERE bukrs  EQ it_znom_programacao_aux-id_empresa
       AND branch EQ it_znom_programacao_aux-id_filial.

    CLEAR: it_znom_programacao_aux.
    MOVE it_znom_programacao[] TO it_znom_programacao_aux[].
    SORT it_znom_programacao_aux BY id_material.
    DELETE ADJACENT DUPLICATES FROM it_znom_programacao_aux COMPARING id_material.

    SELECT * INTO TABLE it_makt
      FROM makt
      FOR ALL ENTRIES IN it_znom_programacao_aux
     WHERE spras EQ sy-langu
       AND matnr EQ it_znom_programacao_aux-id_material.

    CLEAR: it_znom_programacao_aux.
    MOVE it_znom_programacao[] TO it_znom_programacao_aux[].
    SORT it_znom_programacao_aux BY id_cliente.
    DELETE ADJACENT DUPLICATES FROM it_znom_programacao_aux COMPARING id_cliente.
    DELETE it_znom_programacao_aux WHERE id_cliente EQ space.

    SELECT * INTO TABLE it_kna1
      FROM kna1
       FOR ALL ENTRIES IN it_znom_programacao_aux
     WHERE kunnr EQ it_znom_programacao_aux-id_cliente.

    SELECT * INTO TABLE it_znom_prog_reme
      FROM znom_prog_reme
       FOR ALL ENTRIES IN it_znom_programacao
     WHERE id_nomeacao_tran EQ it_znom_programacao-id_nomeacao_tran.

    IF NOT it_znom_prog_reme[] IS INITIAL.

      SELECT * INTO TABLE it_zdoc_exp
        FROM zdoc_exp
         FOR ALL ENTRIES IN it_znom_prog_reme
       WHERE vbeln            = it_znom_prog_reme-id_remessa
         AND id_registro_expo = it_znom_prog_reme-id_registro_expo.

      IF NOT it_zdoc_exp[] IS INITIAL.
        SELECT * INTO TABLE it_zdoc_nf_produtor
          FROM zdoc_nf_produtor
           FOR ALL ENTRIES IN it_zdoc_exp
         WHERE vbeln = it_zdoc_exp-vbeln.
      ENDIF.

      SELECT *
        FROM zdoc_exp_recusa INTO TABLE it_zdoc_exp_recusa_cons
         FOR ALL ENTRIES IN it_znom_prog_reme
       WHERE vbeln_re_exp  = it_znom_prog_reme-id_remessa.

    ENDIF.

  ENDIF.
  vg_total_nomeado = 0.
  vg_saldo_nomeado = 0.
  LOOP AT it_znom_programacao INTO wa_znom_programacao.

    vg_total_nomeado = vg_total_nomeado + wa_znom_programacao-nr_programada.
    CLEAR: wa_znom_programacao_alv.
    MOVE-CORRESPONDING wa_znom_programacao TO wa_znom_programacao_alv.

    IF wa_znom_programacao-id_cliente IS INITIAL.

      wa_znom_programacao_alv-nr_total_remetente = wa_znom_programacao_alv-nr_programada.

** Rementes ************************************************************************
********************************************************************************************************************************
      wa_znom_programacao_alv-nr_qtde_remetente  = 0.
      wa_znom_programacao_alv-nr_qtde_saldo_rem  = 0.
      LOOP AT it_znom_remetente INTO wa_znom_remetente WHERE id_nomeacao_tran = wa_znom_programacao-id_nomeacao_tran
                                                         AND id_empresa       = wa_znom_programacao-id_empresa
                                                         AND id_filial        = wa_znom_programacao-id_filial
                                                         AND id_material      = wa_znom_programacao-id_material.
        wa_znom_programacao_alv-nr_qtde_remetente = wa_znom_programacao_alv-nr_qtde_remetente + wa_znom_remetente-nr_programada.
      ENDLOOP.
      wa_znom_programacao_alv-nr_qtde_saldo_rem  = wa_znom_programacao-nr_programada - wa_znom_programacao_alv-nr_qtde_remetente.
** Rementes ************************************************************************

** Planejamento ********************************************************************
********************************************************************************************************************************
      wa_znom_programacao_alv-nr_qtde_planejada  = 0.
      wa_znom_programacao_alv-nr_qtde_plan_fili  = 0.
      wa_znom_programacao_alv-nr_qtde_saldo_pla  = 0.
      LOOP AT it_znom_reme_notas INTO wa_znom_reme_notas WHERE id_nomeacao_tran = wa_znom_programacao-id_nomeacao_tran
                                                         AND id_empresa         = wa_znom_programacao-id_empresa
                                                         AND id_filial          = wa_znom_programacao-id_filial
                                                         AND id_material        = wa_znom_programacao-id_material.

        "WPP 23102024 - US-153330 --->>>
        READ TABLE it_znom_remetente INTO wa_znom_remetente WITH KEY id_nomeacao_tran = wa_znom_reme_notas-id_nomeacao_tran
                                                                     id_empresa       = wa_znom_reme_notas-id_empresa
                                                                     id_filial        = wa_znom_reme_notas-id_filial
                                                                     id_material      = wa_znom_reme_notas-id_material
                                                                     id_remetente     = wa_znom_reme_notas-id_remetente.
        CHECK sy-subrc EQ 0 AND wa_znom_remetente-tipov NE 'ZEXP'.
        "WPP 23102024 - US-153330 <-----


        wa_znom_programacao_alv-nr_qtde_planejada = wa_znom_programacao_alv-nr_qtde_planejada + wa_znom_reme_notas-nr_quantidade.
      ENDLOOP.
      LOOP AT it_znom_remetente INTO wa_znom_remetente WHERE id_nomeacao_tran = wa_znom_programacao-id_nomeacao_tran
                                                         AND id_empresa       = wa_znom_programacao-id_empresa
                                                         AND id_filial        = wa_znom_programacao-id_filial
                                                         AND id_material      = wa_znom_programacao-id_material
                                                         AND ( ( id_remetente     = space ) OR ( tipov = 'ZEXP' ) ). "WPP 23102024 - US-153330 --->>>
        wa_znom_programacao_alv-nr_qtde_planejada = wa_znom_programacao_alv-nr_qtde_planejada + wa_znom_remetente-nr_programada.
        wa_znom_programacao_alv-nr_qtde_plan_fili = wa_znom_programacao_alv-nr_qtde_plan_fili + wa_znom_remetente-nr_programada.
      ENDLOOP.
      wa_znom_programacao_alv-nr_qtde_saldo_pla  = wa_znom_programacao-nr_programada - wa_znom_programacao_alv-nr_qtde_planejada.
** Planejamento ********************************************************************

** Efetivado ***********************************************************************
********************************************************************************************************************************
      wa_znom_programacao_alv-nr_efetivada       = 0.
      wa_znom_programacao_alv-nr_saldo_efetivar  = 0.
      LOOP AT it_znom_prog_reme INTO wa_znom_prog_reme WHERE id_nomeacao_tran = wa_znom_programacao-id_nomeacao_tran
                                                         AND id_empresa       = wa_znom_programacao-id_empresa
                                                         AND id_filial        = wa_znom_programacao-id_filial
                                                         AND id_material      = wa_znom_programacao-id_material.

        READ TABLE it_zdoc_exp_recusa_cons WITH KEY vbeln_re_exp = wa_znom_prog_reme-id_remessa.

        CHECK sy-subrc NE 0.

        LOOP AT it_zdoc_nf_produtor INTO wa_zdoc_nf_produtor WHERE vbeln EQ wa_znom_prog_reme-id_remessa.
          wa_znom_programacao_alv-nr_efetivada = wa_znom_programacao_alv-nr_efetivada + wa_zdoc_nf_produtor-menge.
        ENDLOOP.
      ENDLOOP.
      wa_znom_programacao_alv-nr_efetivada       = wa_znom_programacao_alv-nr_efetivada + wa_znom_programacao_alv-nr_qtde_plan_fili.
      wa_znom_programacao_alv-nr_saldo_efetivar  = wa_znom_programacao-nr_programada - wa_znom_programacao_alv-nr_efetivada.
** Efetivado ***********************************************************************
    ELSE.
      wa_znom_programacao_alv-nr_total_remetente = wa_znom_programacao-nr_programada.
      wa_znom_programacao_alv-nr_qtde_remetente  = wa_znom_programacao-nr_programada.
      wa_znom_programacao_alv-nr_qtde_saldo_rem  = 0.
      wa_znom_programacao_alv-nr_qtde_planejada  = wa_znom_programacao-nr_programada.
      wa_znom_programacao_alv-nr_qtde_plan_fili  = 0.
      wa_znom_programacao_alv-nr_qtde_saldo_pla  = 0.
      wa_znom_programacao_alv-nr_efetivada       = wa_znom_programacao-nr_programada.
      wa_znom_programacao_alv-nr_saldo_efetivar  = 0.
    ENDIF.

    READ TABLE it_t001 WITH KEY bukrs = wa_znom_programacao-id_empresa.
    IF sy-subrc IS INITIAL.
      wa_znom_programacao_alv-ds_empresa = it_t001-butxt.
    ENDIF.

    IF NOT wa_znom_programacao-id_filial IS INITIAL.
      READ TABLE it_j_1bbranch WITH KEY bukrs  = wa_znom_programacao-id_empresa
                                        branch = wa_znom_programacao-id_filial.
      IF sy-subrc IS INITIAL.
        wa_znom_programacao_alv-ds_filial = it_j_1bbranch-name.
      ENDIF.
    ENDIF.

    IF NOT wa_znom_programacao-id_cliente IS INITIAL.
      READ TABLE it_kna1 WITH KEY kunnr = wa_znom_programacao-id_cliente.
      IF sy-subrc IS INITIAL.
        wa_znom_programacao_alv-ds_cliente = it_kna1-name1.
      ENDIF.
    ENDIF.

    READ TABLE it_makt WITH KEY matnr = wa_znom_programacao-id_material.
    IF sy-subrc IS INITIAL.
      wa_znom_programacao_alv-ds_material = it_makt-maktx.
    ENDIF.

    IF wa_znom_programacao_alv-nr_qtde_saldo_pla GT 0.
      wa_znom_programacao_alv-status    = icon_led_red.
      wa_znom_programacao_alv-ds_status = 'A Planejar'.
    ELSE.
      IF wa_znom_programacao-id_cliente IS INITIAL.
        IF wa_znom_programacao_alv-nr_saldo_efetivar GT 0.
          wa_znom_programacao_alv-status    = icon_led_yellow.
          wa_znom_programacao_alv-ds_status = 'Planejado'.
        ELSE.
          wa_znom_programacao_alv-status    = icon_led_green.
          wa_znom_programacao_alv-ds_status = 'Efetivado'.
        ENDIF.
      ELSE.
        wa_znom_programacao_alv-status    = icon_led_green.
        wa_znom_programacao_alv-ds_status = 'Efetivado'.
      ENDIF.
    ENDIF.
    APPEND wa_znom_programacao_alv TO it_znom_programacao_alv.
  ENDLOOP.
  vg_saldo_nomeado = wa_znom_transporte-nr_qtde_nomeada - vg_total_nomeado.
ENDFORM.                    " CONSULTA_PROGRAMACOES

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_PROGRAMACAO  OUTPUT
*&---------------------------------------------------------------------*
MODULE cria_alv_programacao OUTPUT.
  PERFORM plan_cria_programacao_alv.
ENDMODULE.                 " CRIA_ALV_PROGRAMACAO  OUTPUT

MODULE cria_alv_due_antecipada OUTPUT.
  PERFORM plan_cria_due_antecipada_alv.
ENDMODULE.                 " CRIA_ALV_PROGRAMACAO  OUTPUT

MODULE cria_alv_due_retificacao OUTPUT.
  PERFORM plan_cria_due_retificacao_alv.
ENDMODULE.                 " CRIA_ALV_PROGRAMACAO  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_PROGRAMACAO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_programacao_alv .

  CONSTANTS: tabela_programa TYPE string VALUE 'IT_ZNOM_PROGRAMACAO_ALV'.

  DATA: text_n000 TYPE c LENGTH 50 VALUE 'Status',
        text_n001 TYPE c LENGTH 50 VALUE 'Desc. Status',
        text_n014 TYPE c LENGTH 50 VALUE 'Empresa',
        text_n004 TYPE c LENGTH 50 VALUE 'Desc. Empresa',
        text_n015 TYPE c LENGTH 50 VALUE 'Filial',
        text_n005 TYPE c LENGTH 50 VALUE 'Desc. Filial',
        text_n020 TYPE c LENGTH 50 VALUE 'Cliente',
        text_n021 TYPE c LENGTH 50 VALUE 'Nome Cliente',
        text_n016 TYPE c LENGTH 50 VALUE 'Material',
        text_n006 TYPE c LENGTH 50 VALUE 'Desc. Material',
        text_n008 TYPE c LENGTH 50 VALUE 'Qtd. Programada',
        text_n010 TYPE c LENGTH 50 VALUE 'Qtd. Planejada',
        text_n011 TYPE c LENGTH 50 VALUE 'Qtd. Saldo Planejar',
        text_n012 TYPE c LENGTH 50 VALUE 'Qtd. Remetente',
        text_n013 TYPE c LENGTH 50 VALUE 'Qtd. Saldo Remetente',
        text_n017 TYPE c LENGTH 50 VALUE 'Qtd. Efetivada',
        text_n018 TYPE c LENGTH 50 VALUE 'Qtd. Efetivar',
        text_n019 TYPE c LENGTH 50 VALUE 'Qtd. Filial',
        text_n022 TYPE c LENGTH 50 VALUE 'Contrato'.

  DATA: ls_variant TYPE disvariant.

  ls_variant-report = sy-repid.

  IF plan_prim_programa IS INITIAL.

    CREATE OBJECT plan_container_programa
      EXPORTING
        container_name = 'CTN_PROGRAMADA'.

    CREATE OBJECT plan_alv_programa
      EXPORTING
        i_parent = plan_container_programa.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_programa USING:
        tabela_programa 'CHK'                space     'X' 01 02 space space   space space space space             space,
        tabela_programa 'STATUS'             text_n000 'X' 01 04 space space   space 'X'   space space             space,
        tabela_programa 'DS_STATUS'          text_n001 ' ' 02 15 space space   space 'X'   'X'   c_grid_color_c200 space,
        tabela_programa 'ID_EMPRESA'         text_n014 ' ' 03 04 space space   space space space space             space,
        tabela_programa 'DS_EMPRESA'         text_n004 ' ' 04 15 space space   space space space space             space,
        tabela_programa 'ID_FILIAL'          text_n015 ' ' 05 04 space space   space space space space             space,
        tabela_programa 'DS_FILIAL'          text_n005 ' ' 06 15 space space   space space space space             space,
        "TABELA_PROGRAMA 'ID_CLIENTE'         TEXT_N020 ' ' 07 10 SPACE SPACE   SPACE SPACE SPACE SPACE             SPACE,
        "TABELA_PROGRAMA 'DS_CLIENTE'         TEXT_N021 ' ' 08 30 SPACE SPACE   SPACE SPACE SPACE SPACE             SPACE,
        tabela_programa 'ID_MATERIAL'        text_n016 ' ' 09 10 space 'ALPHA' space space space space             space,
        tabela_programa 'DS_MATERIAL'        text_n006 ' ' 10 30 space space   space space space space             space,
        tabela_programa 'CONTRATO'           text_n022 'X' 11 10 space space   space space space space             space,
        tabela_programa 'NR_TOTAL_REMETENTE' text_n008 ' ' 12 15 space space   'X'   space space c_grid_color_c300 space,
        tabela_programa 'NR_QTDE_REMETENTE'  text_n012 ' ' 13 15 space space   'X'   space space c_grid_color_c400 space,
        tabela_programa 'NR_QTDE_SALDO_REM'  text_n013 ' ' 14 15 space space   'X'   space space c_grid_color_c500 space,
        tabela_programa 'NR_QTDE_PLANEJADA'  text_n010 ' ' 15 15 space space   'X'   space space c_grid_color_c400 space,
        tabela_programa 'NR_QTDE_PLAN_FILI'  text_n019 ' ' 16 15 space space   'X'   space space c_grid_color_c400 space,
        tabela_programa 'NR_QTDE_SALDO_PLA'  text_n011 ' ' 17 15 space space   'X'   space space c_grid_color_c500 space,
        tabela_programa 'NR_EFETIVADA'       text_n017 ' ' 18 15 space space   'X'   space space c_grid_color_c600 space,
        tabela_programa 'NR_SALDO_EFETIVAR'  text_n018 ' ' 19 15 space space   'X'   space space c_grid_color_c500 space.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra    = c_x.
    plan_gs_layout-sel_mode = space.

    CALL METHOD plan_alv_programa->set_table_for_first_display
      EXPORTING
        i_default       = 'X'
        is_layout       = plan_gs_layout
        i_save          = 'X'
        is_variant      = ls_variant
      CHANGING
        it_fieldcatalog = plan_catalogo_programa
        it_outtab       = it_znom_programacao_alv[].

*   Create Object for Event Handler
    CREATE OBJECT plan_event_programa.
    SET HANDLER plan_event_programa->handle_hotspot_programa FOR plan_alv_programa.
    SET HANDLER plan_event_programa->on_double_programa FOR plan_alv_programa.

    plan_prim_programa = c_x.
  ENDIF.

  CALL METHOD plan_alv_programa->refresh_table_display.

  CALL METHOD plan_alv_programa->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_PROGRAMACAO_ALV

FORM plan_cria_due_antecipada_alv .

  DATA: ls_variant TYPE disvariant.

  ls_variant-report = sy-repid.

  IF plan_alv_due_antecipada IS INITIAL.

    CREATE OBJECT plan_container_due_antecip
      EXPORTING
        container_name = 'CTN_DUE_ANTECIPADA'.

    CREATE OBJECT plan_alv_due_antecipada
      EXPORTING
        i_parent = plan_container_due_antecip.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_due_antecip USING:

        'IT_DUE_ANTECIPADA_ALV' 'ID_DUE'                  'Id.DU-e'           ' ' 01 10 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'BLOQ_DESBLOQ'            'St.Bloq.'          ' ' 01 08 space space   space space 'C'   space             space,
        'IT_DUE_ANTECIPADA_ALV' 'BUKRS'                   'Empresa'           ' ' 02 07 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'NUMERO_DUE'              'Número DU-e'       ' ' 03 15 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'NUMERO_RUC'              'Número RUC'        ' ' 04 35 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'CODIGO_COND_VENDA'       'Código Condição'   ' ' 04 15 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'REGIO'                   'Região'            ' ' 05 06 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'TP_EXPORTACAO'           'Tp.Exp.'           ' ' 05 07 space space   space space 'C'   space             space,
        'IT_DUE_ANTECIPADA_ALV' 'CNPJ_DECLARANTE'         'CNPJ Declarante'   ' ' 06 15 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'PESO_LIQ_TOTAL'          'Peso Liq.Tot.(KG)' ' ' 06 18 space space   space space space space             space,
        "'IT_DUE_ANTECIPADA_ALV' 'UE_EXPORTADA'            'UE.Exp.'           ' ' 06 07 SPACE SPACE   SPACE SPACE SPACE SPACE             SPACE,
        "'IT_DUE_ANTECIPADA_ALV' 'QTDE_UE_EXPORTADA'       'Qtde.UE.Exp.'      ' ' 07 12 SPACE SPACE   SPACE SPACE SPACE SPACE             SPACE,
        'IT_DUE_ANTECIPADA_ALV' 'CODIGO_URF_DESPACHO'     'URF.Despacho'      ' ' 08 13 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'CODIGO_RA_DESPACHO'      'RA.Despacho'       ' ' 09 13 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'CODIGO_URF_EMBARQUE'     'URF.Emabarque'     ' ' 10 13 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'CODIGO_RA_EMBARQUE'      'RA.Embarque'       ' ' 11 13 space space   space space space space             space,

        'IT_DUE_ANTECIPADA_ALV' 'MATNR'                   'Material'          ' ' 12 10 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'MAKTX'                   'Ds.Material'       ' ' 13 30 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'CODIGO_NCM'              'NCM'               ' ' 14 12 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'PRECO_TON'               'Preco Ton.'        ' ' 15 10 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'CODIGO_ENQUADRAMENTO'    'Cod.Enquadramento' ' ' 16 17 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'PAIS_DEST'               'País Destino'      ' ' 17 12 space space   space space space space             space,

        'IT_DUE_ANTECIPADA_ALV' 'LIB_LEITURA_OPUS'        'Lib.Leitura Comex' ' ' 18 18 space space   space space 'C'   space             space,
        'IT_DUE_ANTECIPADA_ALV' 'LEITURA_OPUS'            'Leitura Comex'     ' ' 19 13 space space   space space 'C'   space             space,
        'IT_DUE_ANTECIPADA_ALV' 'DT_LEITURA_OPUS'         'Dt.Leitura'        ' ' 20 10 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'HR_LEITURA_OPUS'         'Hr.Leitura'        ' ' 21 10 space space   space space space space             space,
        'IT_DUE_ANTECIPADA_ALV' 'SOLIC_MODIFICACAO_OPUS'  'Sol.Modificação'   ' ' 22 15 space space   space space 'C'   space             space,
        'IT_DUE_ANTECIPADA_ALV' 'MSG_OPUS'                'Msg.Comex'         ' ' 23 20 space space   space space space space             space.


    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra    = c_x.
    plan_gs_layout-sel_mode = space.

    DATA : eventreceiver TYPE REF TO lcl_event_receiver.

    SET HANDLER eventreceiver->handle_data_changed FOR plan_alv_due_antecipada.

    CALL METHOD plan_alv_due_antecipada->set_table_for_first_display
      EXPORTING
        i_default       = 'X'
        is_layout       = plan_gs_layout
        i_save          = 'X'
        is_variant      = ls_variant
      CHANGING
        it_fieldcatalog = plan_catalogo_due_antecip
        it_outtab       = it_due_antecipada_alv[].

*   Create Object for Event Handler
    CREATE OBJECT gr_event_handler.

    SET HANDLER gr_event_handler->on_f4 FOR plan_alv_due_antecipada.

    DATA: lt_f4 TYPE lvc_t_f4.
    DATA: lw_f4 TYPE lvc_s_f4.

    CLEAR lw_f4.
    lw_f4-fieldname  = 'MATNR'.
    lw_f4-register   = 'X'.
    APPEND lw_f4 TO lt_f4.

    CALL METHOD plan_alv_due_antecipada->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.
    "SET HANDLER PLAN_EVENT_PROGRAMA->HANDLE_HOTSPOT_PROGRAMA FOR PLAN_ALV_PROGRAMA.
    "SET HANDLER PLAN_EVENT_PROGRAMA->ON_DOUBLE_PROGRAMA FOR PLAN_ALV_PROGRAMA.
  ENDIF.

  CALL METHOD plan_alv_due_antecipada->refresh_table_display.

  CALL METHOD plan_alv_due_antecipada->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

  CALL METHOD plan_alv_due_antecipada->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

ENDFORM.                    " PLAN_CRIA_DUE_ANTECIPADA_ALV


FORM plan_cria_due_retificacao_alv .

  DATA: ls_variant TYPE disvariant.

  ls_variant-report = sy-repid.

  IF plan_alv_due_retificacao IS INITIAL.

*----------------------------------------------------------------------------------------------------------*
*   Confererencia
*----------------------------------------------------------------------------------------------------------*

    CREATE OBJECT plan_container_due_ret_conf
      EXPORTING
        container_name = 'CTN_DUE_RETIFICACAO_CONF'.

    CREATE OBJECT plan_alv_due_ret_conf
      EXPORTING
        i_parent = plan_container_due_ret_conf.

    CREATE OBJECT toolbar_due_conf_exp
      EXPORTING
        io_alv_grid = plan_alv_due_ret_conf.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_due_ret_conf USING:

        'IT_DUE_RET_CONF_ALV'   'BUKRS'                  'Empresa'            ' ' 01 07 space space   space space space space             space,
        'IT_DUE_RET_CONF_ALV'   'CODIGO_URF_EMBARQUE'    'URF.Embarque'       ' ' 01 13 space space   space space space space             space,
        'IT_DUE_RET_CONF_ALV'   'CODIGO_RA_EMBARQUE'     'RA.Embarque'        ' ' 02 13 space space   space space space space             space,
        'IT_DUE_RET_CONF_ALV'   'MATNR'                  'Material'           ' ' 02 10 space space   space space space space             space,
        'IT_DUE_RET_CONF_ALV'   'PESO_LIQ_TOTAL'         'Peso Liq.Tot.(KG)'  ' ' 04 18 space space   space space space space             space,
        'IT_DUE_RET_CONF_ALV'   'DT_REGISTRO'            'Data'               ' ' 05 10 space space   space space space space             space,
        'IT_DUE_RET_CONF_ALV'   'HR_REGISTRO'            'Hora'               ' ' 06 10 space space   space space space space             space,
        'IT_DUE_RET_CONF_ALV'   'US_REGISTRO'            'Usuário'            ' ' 07 12 space space   space space space space             space.

    "'IT_DUE_RET_CONF_ALV'   'UE_EXPORTADA'           'UE.Exp.'            ' ' 03 07 SPACE SPACE   SPACE SPACE SPACE SPACE             SPACE,
    "'IT_DUE_RET_CONF_ALV'   'QTDE_UE_EXPORTADA'      'Qtde.UE.Exp.'       ' ' 04 13 SPACE SPACE   SPACE SPACE SPACE SPACE             SPACE.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra    = c_x.
    plan_gs_layout-sel_mode = space.

    SET HANDLER toolbar_due_conf_exp->on_toolbar          FOR plan_alv_due_ret_conf.
    SET HANDLER toolbar_due_conf_exp->handle_user_command FOR plan_alv_due_ret_conf.

    CALL METHOD plan_alv_due_ret_conf->set_table_for_first_display
      EXPORTING
        i_default       = 'X'
        is_layout       = plan_gs_layout
        i_save          = 'X'
        is_variant      = ls_variant
      CHANGING
        it_fieldcatalog = plan_catalogo_due_ret_conf
        it_outtab       = it_due_ret_conf_alv[].

*----------------------------------------------------------------------------------------------------------*
*   DU-e' Retificar
*----------------------------------------------------------------------------------------------------------*

    CREATE OBJECT plan_container_due_retific
      EXPORTING
        container_name = 'CTN_DUE_RETIFICACAO'.

    CREATE OBJECT plan_alv_due_retificacao
      EXPORTING
        i_parent = plan_container_due_retific.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_due_retific USING:

        'IT_DUE_RETIFICACAO_ALV' 'ID_DUE'                  'Id.DU-e'           ' ' 01 10 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'BUKRS'                   'Empresa'           ' ' 02 07 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'NUMERO_DUE'              'Número DU-e'       ' ' 03 15 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'NUMERO_RUC'              'Número RUC'        ' ' 04 35 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'REGIO'                   'Região'            ' ' 05 06 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'TP_EXPORTACAO'           'Tp.Exp.'           ' ' 05 07 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'CNPJ_DECLARANTE'         'CNPJ Declarante'   ' ' 06 15 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'PESO_LIQ_TOTAL'          'Peso Liq.Tot.(KG)' ' ' 06 18 space space   space space space space             space,
        "'IT_DUE_RETIFICACAO_ALV' 'UE_EXPORTADA'            'UE.Exp.'           ' ' 06 07 SPACE SPACE   SPACE SPACE SPACE SPACE             SPACE,
        "'IT_DUE_RETIFICACAO_ALV' 'QTDE_UE_EXPORTADA'       'Qtde.UE.Exp.'      ' ' 07 12 SPACE SPACE   SPACE SPACE SPACE SPACE             SPACE,
        'IT_DUE_RETIFICACAO_ALV' 'ID_DUE_RET'              'Id.DU-e Ret.'      ' ' 08 10 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'PESO_LIQ_TOT_RET'        'Peso.Liq.Ret.(KG)' ' ' 09 18 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'CODIGO_URF_DESPACHO'     'URF.Despacho'      ' ' 10 13 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'CODIGO_RA_DESPACHO'      'RA.Despacho'       ' ' 11 13 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'CODIGO_URF_EMBARQUE'     'URF.Embarque'      ' ' 12 13 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'CODIGO_RA_EMBARQUE'      'RA.Embarque'       ' ' 13 13 space space   space space space space             space,
        'IT_DUE_RETIFICACAO_ALV' 'NF_EXP'                  'NF.Exportação'     'X' 13 14 space space   space space 'C'   space             space.


    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra    = c_x.
    plan_gs_layout-sel_mode = space.
    plan_gs_layout-grid_title = 'DU-e''s Antecipadas'.

    CALL METHOD plan_alv_due_retificacao->set_table_for_first_display
      EXPORTING
        i_default       = 'X'
        is_layout       = plan_gs_layout
        i_save          = 'X'
        is_variant      = ls_variant
      CHANGING
        it_fieldcatalog = plan_catalogo_due_retific
        it_outtab       = it_due_retificacao_alv[].

*   Create Object for Event Handler
    CREATE OBJECT plan_event_due_ret.
    SET HANDLER plan_event_due_ret->handle_hotspot FOR plan_alv_due_retificacao.

  ENDIF.

  CALL METHOD plan_alv_due_retificacao->refresh_table_display.

  CALL METHOD plan_alv_due_retificacao->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

  CALL METHOD plan_alv_due_ret_conf->refresh_table_display.

  CALL METHOD plan_alv_due_ret_conf->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_DUE_RETIFICACAO_ALV

*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM troca_aba_01 .
  vg_dynnr_xxxx    = vg_dynnr_0010.
  tabpag-activetab = ok_tab01.
  PERFORM libera_nomeacoes.
ENDFORM.                    " TROCA_ABA_01

*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM troca_aba_03  USING forcar TYPE c.

  DATA: vg_verifica_selecao_pr TYPE sy-subrc.


  IF forcar = 'S'.
    "Le tabela interna
    READ TABLE it_znom_programacao     INTO wa_znom_programacao
              WITH KEY id_nomeacao_tran = wa_znom_programacao_f-id_nomeacao_tran
                       id_empresa       = wa_znom_programacao_f-id_empresa
                       id_filial        = wa_znom_programacao_f-id_filial
                       id_material      = wa_znom_programacao_f-id_material
                       id_cliente       = wa_znom_programacao_f-id_cliente.

    "Le tabela interna ALV para exibir
    READ TABLE it_znom_programacao_alv    INTO wa_znom_programacao_alv
              WITH KEY id_nomeacao_tran = wa_znom_programacao_f-id_nomeacao_tran
                       id_empresa       = wa_znom_programacao_f-id_empresa
                       id_filial        = wa_znom_programacao_f-id_filial
                       id_material      = wa_znom_programacao_f-id_material
                       id_cliente       = wa_znom_programacao_f-id_cliente.
    CLEAR vg_verifica_selecao_pr.
  ELSE.
    PERFORM verifica_selecao_programa USING vg_verifica_selecao_pr.
  ENDIF.


  IF vg_verifica_selecao_pr IS INITIAL.

    PERFORM verifica_planeja_filial USING wa_znom_programacao-id_filial vg_verifica_selecao_pr.

    IF vg_verifica_selecao_pr IS INITIAL.

      CLEAR: it_znom_notasfiscais_alv[],
             wa_filtro_remetente-empresa,
             wa_filtro_remetente-centro,
             wa_filtro_remetente-material,
             wa_filtro_remetente-contrato,
             wa_filtro_remetente-grp_retorno,
             wa_filtro_remetente-tipov,
             wa_filtro_remetente-preco,
             wa_filtro_remetente-depst,
             wa_filtro_remetente-safra,
             wa_filtro_remetente-cvirt,
             wa_filtro_remetente-desc_cvirt.

      CLEAR: wa_qtd_carac-nr_nf,
             wa_qtd_carac-remet,
             wa_qtd_carac-ret.
**<<<------"145379 - NMS - INI------>>>
      CLEAR znom_remetente-retorno_com_nf_de_terceiro.
**<<<------"145379 - NMS - FIM------>>>
      vg_dynnr_xxxx    = vg_dynnr_0030.
      tabpag-activetab = ok_tab03.

      wa_filtro_remetente-empresa  = wa_znom_programacao-id_empresa.
      wa_filtro_remetente-centro   = wa_znom_programacao-id_filial.
      wa_filtro_remetente-material = wa_znom_programacao-id_material.
      wa_filtro_remetente-contrato = wa_znom_programacao-contrato.
      wa_filtro_remetente-tp_vinc1 = abap_true.
      wa_filtro_remetente-tp_vinc2 = abap_false.
      wa_filtro_remetente-c_cct    = abap_true.
      wa_filtro_remetente-s_cct    = abap_false.
      wa_filtro_remetente-ck_nf_restr = abap_true.
      CLEAR: wa_filtro_remetente-nr_qtd_vinc.

      "Limpa trocas
      REFRESH : it_znom_remetente_log,
                it_znom_reme_dnotas_alv.

      PERFORM consulta_remetentes.

    ELSE.
      MESSAGE s021 WITH wa_znom_programacao-id_filial.
    ENDIF.

    CLEAR: wa_filtro_remetente-remetente,
           wa_filtro_remetente-data_ini,
           wa_filtro_remetente-data_fim.

  ELSE.
    MESSAGE s004.
  ENDIF.

ENDFORM.                    " TROCA_ABA_03

*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_04
*&---------------------------------------------------------------------*
*       Troca Aba 04
*----------------------------------------------------------------------*
FORM troca_aba_04  USING  forcar TYPE c.

  DATA: vg_verifica_selecao_pr TYPE sy-subrc.

  PERFORM verifica_selecao_programa_plan USING vg_verifica_selecao_pr.

  IF vg_verifica_selecao_pr IS INITIAL.
    vg_dynnr_xxxx    = vg_dynnr_0040.
    tabpag-activetab = ok_tab04.
    CLEAR: wa_filtro_remetente-nr_qtd_vinc.
    PERFORM consulta_remessas USING abap_false.
  ELSE.
    MESSAGE s005.
  ENDIF.

ENDFORM.                    " TROCA_ABA_04

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_REMETENTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consulta_remetentes .

  DATA: it_znom_remetente_aux TYPE TABLE OF znom_remetente WITH HEADER LINE,
        it_lfa1               TYPE TABLE OF lfa1 WITH HEADER LINE,
        wa_j_1bbranch         TYPE j_1bbranch,
        text_cliente_cgc      TYPE c LENGTH 18,
        text_cliente_cpf      TYPE c LENGTH 14,
        wa_adrc               TYPE adrc,
        cidade                TYPE j_1btxjurt.

  CLEAR: it_znom_remetente_alv[], it_znom_remetente[], it_lfa1[], it_znom_prog_reme[], it_zdoc_exp[], it_zdoc_nf_produtor[].

  "Seleciona Remetentes de Mercadoria para Compromisso
  SELECT * INTO TABLE it_znom_remetente
    FROM znom_remetente
   WHERE id_nomeacao_tran EQ wa_znom_programacao-id_nomeacao_tran
     AND id_empresa       EQ wa_znom_programacao-id_empresa
     AND id_filial        EQ wa_znom_programacao-id_filial
     AND id_material      EQ wa_znom_programacao-id_material
     AND id_remetente     NE space.

  "Comentando 26.09.2018 - Ini
*  SELECT PR~MANDT            AS MANDT
*         PR~ID_NOMEACAO_TRAN AS ID_NOMEACAO_TRAN
*         PR~ID_EMPRESA       AS ID_EMPRESA
*         PR~ID_FILIAL        AS ID_FILIAL
*         PR~ID_MATERIAL      AS ID_MATERIAL
*         RE~ID_REMETENTE     AS ID_REMETENTE
*         RE~ID_UNIDADE       AS ID_UNIDADE
*         SUM( RE~MENGE )     AS NR_PROGRAMADA
*    APPENDING CORRESPONDING FIELDS OF TABLE IT_ZNOM_REMETENTE
*    FROM ZDOC_EXP_REC_NF AS RE
*    INNER JOIN ZNOM_PROG_REME AS PR ON PR~ID_REMESSA EQ RE~VBELN_RE_EXP
*   WHERE PR~ID_NOMEACAO_TRAN  EQ WA_ZNOM_PROGRAMACAO-ID_NOMEACAO_TRAN
*     AND PR~ID_EMPRESA        EQ WA_ZNOM_PROGRAMACAO-ID_EMPRESA
*     AND PR~ID_FILIAL         EQ WA_ZNOM_PROGRAMACAO-ID_FILIAL
*     AND PR~ID_MATERIAL       EQ WA_ZNOM_PROGRAMACAO-ID_MATERIAL
*     AND NOT EXISTS ( SELECT * FROM ZNOM_REME_NOTAS AS NF
*                       WHERE NF~ID_NOMEACAO_TRAN EQ PR~ID_NOMEACAO_TRAN
*                         AND NF~ID_EMPRESA       EQ PR~ID_EMPRESA
*                         AND NF~ID_FILIAL        EQ PR~ID_FILIAL
*                         AND NF~ID_MATERIAL      EQ PR~ID_MATERIAL
*                         AND NF~ID_REMETENTE     EQ RE~ID_REMETENTE )
*  GROUP BY PR~MANDT
*         PR~ID_NOMEACAO_TRAN
*         PR~ID_EMPRESA
*         PR~ID_FILIAL
*         PR~ID_MATERIAL
*         RE~ID_REMETENTE
*         RE~ID_UNIDADE.
  "Comentando 26.09.2018 - Fim

  MOVE it_znom_remetente[] TO it_znom_remetente_aux[].
  SORT it_znom_remetente_aux BY id_remetente.
  DELETE ADJACENT DUPLICATES FROM it_znom_remetente_aux COMPARING id_remetente grp_retorno.

  IF NOT it_znom_remetente_aux[] IS INITIAL.
    SELECT * INTO TABLE it_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN it_znom_remetente_aux
     WHERE lifnr EQ it_znom_remetente_aux-id_remetente.
  ENDIF.

  LOOP AT it_znom_remetente INTO wa_znom_remetente.

    CLEAR: wa_znom_remetente_alv.
    MOVE-CORRESPONDING wa_znom_remetente TO wa_znom_remetente_alv.
    wa_znom_remetente_alv-nr_remetente2     = wa_znom_remetente-nr_programada.
    wa_znom_remetente_alv-nr_efetivada      = 0.
    wa_znom_remetente_alv-nr_saldo_efetivar = wa_znom_remetente-nr_programada.

    wa_znom_remetente_alv-icone         = icon_customer.

    READ TABLE it_lfa1 WITH KEY lifnr = wa_znom_remetente-id_remetente.
    IF sy-subrc IS INITIAL.
      wa_znom_remetente_alv-name1 = it_lfa1-name1.
      IF ( it_lfa1-stkzn IS INITIAL ) AND ( it_lfa1-stcd1 IS NOT INITIAL ).
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = it_lfa1-stcd1
          IMPORTING
            output = text_cliente_cgc.
        wa_znom_remetente_alv-stcd1 = text_cliente_cgc.
      ELSEIF ( it_lfa1-stkzn IS NOT INITIAL ) AND ( it_lfa1-stcd2 IS NOT INITIAL ).
        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = it_lfa1-stcd2
          IMPORTING
            output = text_cliente_cpf.
        wa_znom_remetente_alv-stcd1 = text_cliente_cpf.
      ENDIF.

      SELECT SINGLE * INTO wa_adrc
        FROM adrc
       WHERE addrnumber EQ it_lfa1-adrnr.

      IF sy-subrc IS INITIAL.
        wa_znom_remetente_alv-uf = wa_adrc-taxjurcode(3).

        SELECT SINGLE * INTO cidade
          FROM j_1btxjurt
         WHERE spras      = wa_adrc-langu
           AND country    = wa_adrc-country
           AND taxjurcode = wa_adrc-taxjurcode.

        IF sy-subrc IS INITIAL.
          wa_znom_remetente_alv-munic = cidade-text.
        ENDIF.
      ENDIF.

    ENDIF.
    APPEND wa_znom_remetente_alv TO it_znom_remetente_alv.
  ENDLOOP.

  SORT it_znom_remetente_alv BY name1.

  CLEAR: wa_j_1bbranch.

  SELECT * APPENDING TABLE it_znom_remetente
    FROM znom_remetente
   WHERE id_nomeacao_tran EQ wa_znom_programacao-id_nomeacao_tran
     AND id_empresa       EQ wa_znom_programacao-id_empresa
     AND id_filial        EQ wa_znom_programacao-id_filial
     AND id_material      EQ wa_znom_programacao-id_material
     AND id_remetente     EQ space.

  READ TABLE it_znom_remetente WITH KEY id_remetente = space.

  IF NOT sy-subrc IS INITIAL.

    SELECT pr~mandt                AS mandt
           pr~id_nomeacao_tran     AS id_nomeacao_tran
           pr~id_empresa           AS id_empresa
           pr~id_filial            AS id_filial
           pr~id_material          AS id_material
           re~id_unidade           AS id_unidade
           SUM( re~nm_quantidade ) AS nr_programada
           APPENDING CORRESPONDING FIELDS OF TABLE it_znom_remetente
      FROM zdoc_exp_recusa AS re
     INNER JOIN znom_prog_reme AS pr ON pr~id_remessa EQ re~vbeln_re_exp
     WHERE pr~id_nomeacao_tran  EQ wa_znom_programacao-id_nomeacao_tran
       AND pr~id_empresa        EQ wa_znom_programacao-id_empresa
       AND pr~id_filial         EQ wa_znom_programacao-id_filial
       AND pr~id_material       EQ wa_znom_programacao-id_material
       AND NOT EXISTS ( SELECT *
                          FROM zdoc_exp_rec_nf AS nf
                         WHERE nf~id_doc_exp EQ re~id_doc_exp )
     GROUP BY pr~mandt
              pr~id_nomeacao_tran
              pr~id_empresa
              pr~id_filial
              pr~id_material
              re~id_unidade.
  ELSE.
    LOOP AT it_znom_remetente .
      IF it_znom_remetente-id_remetente IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = it_znom_remetente-id_filial
          IMPORTING
            output = it_znom_remetente-lifnr.
        MODIFY it_znom_remetente INDEX sy-tabix TRANSPORTING lifnr.

      ENDIF.

    ENDLOOP.

    SELECT * APPENDING
    TABLE it_lfa1
    FROM lfa1
     FOR ALL ENTRIES IN it_znom_remetente
   WHERE lifnr EQ it_znom_remetente-lifnr.


  ENDIF.

  LOOP AT it_znom_remetente INTO wa_znom_remetente WHERE id_remetente EQ space.

    CLEAR: wa_znom_remetente_alv.
    MOVE-CORRESPONDING wa_znom_remetente TO wa_znom_remetente_alv.
    wa_znom_remetente_alv-nr_remetente2 = wa_znom_remetente-nr_programada.
    wa_znom_remetente_alv-icone         = icon_customer_warehouse.

    SELECT SINGLE * INTO wa_j_1bbranch
      FROM j_1bbranch
     WHERE bukrs  EQ wa_znom_remetente-id_empresa
       AND branch EQ wa_znom_remetente-id_filial.

    IF sy-subrc IS INITIAL.
      wa_znom_remetente_alv-name1 = wa_j_1bbranch-name.
      IF wa_j_1bbranch-stcd1 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_j_1bbranch-stcd1
          IMPORTING
            output = text_cliente_cgc.
        wa_znom_remetente_alv-stcd1 = text_cliente_cgc.
      ENDIF.

      READ TABLE it_lfa1 WITH KEY lifnr = wa_znom_remetente-lifnr.
      IF  sy-subrc = 0.
        SELECT SINGLE * INTO wa_adrc
          FROM adrc
         WHERE addrnumber EQ it_lfa1-adrnr.

        IF sy-subrc IS INITIAL.
          wa_znom_remetente_alv-uf = wa_adrc-taxjurcode(3).

          SELECT SINGLE * INTO cidade
            FROM j_1btxjurt
           WHERE spras      = wa_adrc-langu
             AND country    = wa_adrc-country
             AND taxjurcode = wa_adrc-taxjurcode.

          IF sy-subrc IS INITIAL.
            wa_znom_remetente_alv-munic = cidade-text.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    wa_znom_remetente_alv-nr_efetivada      = wa_znom_remetente-nr_programada.
    wa_znom_remetente_alv-nr_saldo_efetivar = 0.
    APPEND wa_znom_remetente_alv TO it_znom_remetente_alv.

  ENDLOOP.

**  Seleciona Notas Fiscais Vinculadas aos Remetentes
  PERFORM consulta_notas_vinculadas TABLES it_lfa1 USING wa_j_1bbranch.

ENDFORM.                    " CONSULTA_REMETENTES

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_REMETENTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consulta_remetentes_log USING tipo TYPE c.

  DATA: it_znom_remetente_aux TYPE TABLE OF znom_remetente WITH HEADER LINE,
        it_lfa1               TYPE TABLE OF lfa1 WITH HEADER LINE,
        wa_j_1bbranch         TYPE j_1bbranch,
        text_cliente_cgc      TYPE c LENGTH 18,
        text_cliente_cpf      TYPE c LENGTH 14,
        wa_adrc               TYPE adrc,
        cidade                TYPE j_1btxjurt.

  CLEAR: it_znom_remetente_alv[], it_lfa1[], it_znom_prog_reme[], it_zdoc_exp[], it_zdoc_nf_produtor[].


  it_znom_remetente[] = it_znom_remetente_log[].
  DELETE it_znom_remetente WHERE novo = 'D'.

  MOVE it_znom_remetente[] TO it_znom_remetente_aux[].
  SORT it_znom_remetente_aux BY id_remetente.
  DELETE ADJACENT DUPLICATES FROM it_znom_remetente_aux COMPARING id_remetente grp_retorno.

  IF NOT it_znom_remetente_aux[] IS INITIAL.
    SELECT * INTO TABLE it_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN it_znom_remetente_aux
     WHERE lifnr EQ it_znom_remetente_aux-id_remetente.
  ENDIF.

  LOOP AT it_znom_remetente INTO wa_znom_remetente.
    IF wa_znom_remetente-id_remetente IS INITIAL.
      CONTINUE.
    ENDIF.
    CLEAR: wa_znom_remetente_alv.
    MOVE-CORRESPONDING wa_znom_remetente TO wa_znom_remetente_alv.
    wa_znom_remetente_alv-nr_remetente2     = wa_znom_remetente-nr_programada.
    wa_znom_remetente_alv-nr_efetivada      = 0.
    wa_znom_remetente_alv-nr_saldo_efetivar = wa_znom_remetente-nr_programada.

    wa_znom_remetente_alv-icone         = icon_customer.

    READ TABLE it_lfa1 WITH KEY lifnr = wa_znom_remetente-id_remetente.
    IF sy-subrc IS INITIAL.
      wa_znom_remetente_alv-name1 = it_lfa1-name1.
      IF ( it_lfa1-stkzn IS INITIAL ) AND ( it_lfa1-stcd1 IS NOT INITIAL ).
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = it_lfa1-stcd1
          IMPORTING
            output = text_cliente_cgc.
        wa_znom_remetente_alv-stcd1 = text_cliente_cgc.
      ELSEIF ( it_lfa1-stkzn IS NOT INITIAL ) AND ( it_lfa1-stcd2 IS NOT INITIAL ).
        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = it_lfa1-stcd2
          IMPORTING
            output = text_cliente_cpf.
        wa_znom_remetente_alv-stcd1 = text_cliente_cpf.
      ENDIF.

      SELECT SINGLE * INTO wa_adrc
        FROM adrc
       WHERE addrnumber EQ it_lfa1-adrnr.

      IF sy-subrc IS INITIAL.
        wa_znom_remetente_alv-uf = wa_adrc-taxjurcode(3).

        SELECT SINGLE * INTO cidade
          FROM j_1btxjurt
         WHERE spras      = wa_adrc-langu
           AND country    = wa_adrc-country
           AND taxjurcode = wa_adrc-taxjurcode.

        IF sy-subrc IS INITIAL.
          wa_znom_remetente_alv-munic = cidade-text.
        ENDIF.
      ENDIF.

    ENDIF.
    APPEND wa_znom_remetente_alv TO it_znom_remetente_alv.
  ENDLOOP.

  SORT it_znom_remetente_alv BY name1.

  CLEAR: wa_j_1bbranch.


  LOOP AT it_znom_remetente INTO wa_znom_remetente WHERE id_remetente EQ space.

    CLEAR: wa_znom_remetente_alv.
    MOVE-CORRESPONDING wa_znom_remetente TO wa_znom_remetente_alv.
    wa_znom_remetente_alv-nr_remetente2 = wa_znom_remetente-nr_programada.
    wa_znom_remetente_alv-icone         = icon_customer_warehouse.

    SELECT SINGLE * INTO wa_j_1bbranch
      FROM j_1bbranch
     WHERE bukrs  EQ wa_znom_remetente-id_empresa
       AND branch EQ wa_znom_remetente-id_filial.

    IF sy-subrc IS INITIAL.
      wa_znom_remetente_alv-name1 = wa_j_1bbranch-name.
      IF wa_j_1bbranch-stcd1 IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_j_1bbranch-stcd1
          IMPORTING
            output = text_cliente_cgc.
        wa_znom_remetente_alv-stcd1 = text_cliente_cgc.
      ENDIF.

      READ TABLE it_lfa1 WITH KEY lifnr = wa_znom_remetente-lifnr.
      IF  sy-subrc = 0.
        SELECT SINGLE * INTO wa_adrc
          FROM adrc
         WHERE addrnumber EQ it_lfa1-adrnr.

        IF sy-subrc IS INITIAL.
          wa_znom_remetente_alv-uf = wa_adrc-taxjurcode(3).

          SELECT SINGLE * INTO cidade
            FROM j_1btxjurt
           WHERE spras      = wa_adrc-langu
             AND country    = wa_adrc-country
             AND taxjurcode = wa_adrc-taxjurcode.

          IF sy-subrc IS INITIAL.
            wa_znom_remetente_alv-munic = cidade-text.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    wa_znom_remetente_alv-nr_efetivada      = wa_znom_remetente-nr_programada.
    wa_znom_remetente_alv-nr_saldo_efetivar = 0.
    APPEND wa_znom_remetente_alv TO it_znom_remetente_alv.

  ENDLOOP.

**  Seleciona Notas Fiscais Vinculadas aos Remetentes
  IF tipo = 'N'.
    PERFORM consulta_notas_vinculadas TABLES it_lfa1 USING wa_j_1bbranch.
  ELSE.
    DESCRIBE TABLE it_znom_reme_notas_alv  LINES wa_filtro_reme_qtde_sel.
  ENDIF.

ENDFORM.                    " CONSULTA_REMETENTES

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_NOTAS_VINCULADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM consulta_notas_vinculadas TABLES it_fornecedor STRUCTURE lfa1 USING p_j_1bbranch TYPE j_1bbranch.

  DATA: it_znom_reme_notas_aux  TYPE TABLE OF znom_reme_notas WITH HEADER LINE,
        it_j_1bnfdoc            TYPE TABLE OF j_1bnfdoc       WITH HEADER LINE,
        it_j_1bnflin            TYPE TABLE OF j_1bnflin       WITH HEADER LINE,
        vg_tabix_reme           TYPE sy-tabix,
        v_lifnr_aux             TYPE lfa1-lifnr,
        it_zdoc_exp_rec_nf      TYPE TABLE OF zdoc_exp_rec_nf WITH HEADER LINE,
        it_zdoc_exp_recusa      TYPE TABLE OF zdoc_exp_recusa WITH HEADER LINE,
        it_znom_prog_reme_aux   TYPE TABLE OF znom_prog_reme  WITH HEADER LINE,
        it_znom_reme_notas_exl  TYPE TABLE OF znom_reme_notas WITH HEADER LINE,
        it_zdoc_exp_recusa_cons TYPE TABLE OF zdoc_exp_recusa WITH HEADER LINE,
        wa_znom_reme_notas_exl  TYPE znom_reme_notas.

  REFRESH : it_znom_reme_notas_alv[], it_znom_prog_reme[], it_zdoc_exp[], it_zdoc_nf_produtor[], it_zdoc_exp_recusa_cons[].
  IF it_znom_reme_dnotas_alv[]     IS INITIAL. "ALRS
    CLEAR: it_znom_reme_notas[].
  ENDIF.

  IF NOT it_znom_remetente[] IS INITIAL.

    IF it_znom_reme_dnotas_alv[]     IS INITIAL. "ALRS
      SELECT * INTO TABLE it_znom_reme_notas
        FROM znom_reme_notas
         FOR ALL ENTRIES IN it_znom_remetente
       WHERE id_nomeacao_tran EQ it_znom_remetente-id_nomeacao_tran
         AND id_empresa       EQ it_znom_remetente-id_empresa
         AND id_filial        EQ it_znom_remetente-id_filial
         AND id_material      EQ it_znom_remetente-id_material
         AND id_remetente     EQ it_znom_remetente-id_remetente
         AND grp_retorno      EQ it_znom_remetente-grp_retorno.
    ENDIF.

    SELECT * INTO TABLE it_znom_reme_notase
      FROM znom_reme_notase
       FOR ALL ENTRIES IN it_znom_remetente
     WHERE id_nomeacao_tran EQ it_znom_remetente-id_nomeacao_tran
       AND id_empresa       EQ it_znom_remetente-id_empresa
       AND id_filial        EQ it_znom_remetente-id_filial
       AND id_material      EQ it_znom_remetente-id_material
       AND id_remetente     EQ it_znom_remetente-id_remetente.

    SELECT * INTO TABLE it_znom_prog_reme
      FROM znom_prog_reme
       FOR ALL ENTRIES IN it_znom_remetente
     WHERE id_nomeacao_tran EQ it_znom_remetente-id_nomeacao_tran
       AND id_empresa       EQ it_znom_remetente-id_empresa
       AND id_filial        EQ it_znom_remetente-id_filial
       AND id_material      EQ it_znom_remetente-id_material.

    IF NOT it_znom_prog_reme[] IS INITIAL.

      SELECT * INTO TABLE it_zdoc_exp
        FROM zdoc_exp
         FOR ALL ENTRIES IN it_znom_prog_reme
       WHERE vbeln            = it_znom_prog_reme-id_remessa
         AND id_registro_expo = it_znom_prog_reme-id_registro_expo.

      SELECT * APPENDING TABLE it_zdoc_exp
        FROM zdoc_exp
         FOR ALL ENTRIES IN it_znom_prog_reme
       WHERE vbeln  = it_znom_prog_reme-id_remessa
         AND id_due = it_znom_prog_reme-id_due.

      SORT it_zdoc_exp                                   BY id_doc_exp vbeln id_registro_expo id_due.
      DELETE ADJACENT DUPLICATES FROM it_zdoc_exp COMPARING id_doc_exp vbeln id_registro_expo id_due.

      IF NOT it_zdoc_exp[] IS INITIAL.
        "Efetivado
        SELECT * INTO TABLE it_zdoc_nf_produtor
          FROM zdoc_nf_produtor
           FOR ALL ENTRIES IN it_zdoc_exp
         WHERE vbeln = it_zdoc_exp-vbeln.

        "Recusado/Devolvido
        SELECT * INTO TABLE it_zdoc_exp_rec_nf
          FROM zdoc_exp_rec_nf
           FOR ALL ENTRIES IN it_zdoc_exp
         WHERE vbeln_re_exp = it_zdoc_exp-vbeln.

        "Verifica se alguma nota fiscal recusada foi retirada da programação 21.09.2018
        IF NOT it_zdoc_exp_rec_nf[] IS INITIAL.
          SELECT p~mandt p~id_nomeacao_tran p~id_empresa p~id_filial p~id_material r~id_remetente r~docnum_prod AS docnum r~itmnum_prod AS itmnum r~id_unidade
                 SUM( r~menge ) AS nr_quantidade
                 INTO CORRESPONDING FIELDS OF TABLE it_znom_reme_notas_exl
            FROM zdoc_exp_rec_nf AS r
           INNER JOIN znom_prog_reme AS p ON p~id_remessa EQ r~vbeln_re_exp
           WHERE NOT EXISTS ( SELECT *
                                FROM znom_reme_notas AS g
                               WHERE g~id_nomeacao_tran EQ p~id_nomeacao_tran
                                 AND g~id_empresa       EQ p~id_empresa
                                 AND g~id_filial        EQ p~id_filial
                                 AND g~id_material      EQ p~id_material
                                 AND g~id_remetente     EQ r~id_remetente
                                 AND g~docnum           EQ r~docnum_prod
                                 AND g~itmnum           EQ r~itmnum_prod )
           GROUP BY p~mandt p~id_nomeacao_tran p~id_empresa p~id_filial p~id_material r~id_remetente r~docnum_prod r~itmnum_prod r~id_unidade.

*          IF SY-SUBRC IS INITIAL.
*            LOOP AT IT_ZNOM_REME_NOTAS_EXL INTO WA_ZNOM_REME_NOTAS_EXL.
*              APPEND WA_ZNOM_REME_NOTAS_EXL TO IT_ZNOM_REME_NOTAS.
*            ENDLOOP.
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF it_zdoc_nf_produtor[] IS NOT INITIAL.
      SELECT *
        FROM zdoc_exp_recusa INTO TABLE it_zdoc_exp_recusa_cons
         FOR ALL ENTRIES IN it_zdoc_nf_produtor
       WHERE vbeln_re_exp  = it_zdoc_nf_produtor-vbeln.
    ENDIF.

  ENDIF.

  CLEAR: it_znom_reme_notas_aux[].
  MOVE it_znom_reme_notas[] TO it_znom_reme_notas_aux[].
  DELETE it_znom_reme_notas WHERE docnum = '9999999999'.
  SORT it_znom_reme_notas_aux BY docnum.
  DELETE ADJACENT DUPLICATES FROM it_znom_reme_notas_aux COMPARING docnum grp_retorno.

  IF NOT it_znom_reme_notas_aux[] IS INITIAL.

    SELECT * INTO TABLE it_j_1bnfdoc
      FROM j_1bnfdoc
       FOR ALL ENTRIES IN it_znom_reme_notas_aux
     WHERE docnum EQ it_znom_reme_notas_aux-docnum.

    LOOP AT it_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_nota_fiscal>) WHERE partyp EQ 'B'.
      PERFORM f_converte_parid_br_to_lf CHANGING <fs_nota_fiscal>-parid
                                                 <fs_nota_fiscal>-partyp
                                                 <fs_nota_fiscal>-parvw.
    ENDLOOP.

    CLEAR: it_znom_reme_notas_aux[].
    MOVE it_znom_reme_notas[] TO it_znom_reme_notas_aux[].
    SORT it_znom_reme_notas_aux BY docnum itmnum.
    DELETE ADJACENT DUPLICATES FROM it_znom_reme_notas_aux COMPARING docnum itmnum grp_retorno.

    IF it_znom_reme_notas_aux[] IS NOT INITIAL.
      SELECT * INTO TABLE it_j_1bnflin
        FROM j_1bnflin
         FOR ALL ENTRIES IN it_znom_reme_notas_aux
       WHERE docnum EQ it_znom_reme_notas_aux-docnum
         AND itmnum EQ it_znom_reme_notas_aux-itmnum.
    ENDIF.

  ENDIF.

  LOOP AT it_znom_reme_notas INTO wa_znom_reme_notas.
    IF wa_znom_reme_notas-docnum = '9999999999'.
      CONTINUE.
    ENDIF.
    CLEAR: wa_znom_reme_notas_alv.
    MOVE-CORRESPONDING wa_znom_reme_notas TO wa_znom_reme_notas_alv.
    wa_znom_reme_notas_alv-nr_quantidade2 = wa_znom_reme_notas-nr_quantidade.
    wa_znom_reme_notas_alv-nr_efetivada   = 0.

    READ TABLE it_j_1bnfdoc WITH KEY docnum = wa_znom_reme_notas_alv-docnum.
    IF sy-subrc IS INITIAL.
      wa_znom_reme_notas_alv-docdat = it_j_1bnfdoc-docdat.
      wa_znom_reme_notas_alv-model  = it_j_1bnfdoc-model.
      wa_znom_reme_notas_alv-series = it_j_1bnfdoc-series.
      wa_znom_reme_notas_alv-branch = it_j_1bnfdoc-branch.
      wa_znom_reme_notas_alv-parvw  = it_j_1bnfdoc-parvw.
      wa_znom_reme_notas_alv-parid  = it_j_1bnfdoc-parid.
      READ TABLE it_fornecedor WITH KEY lifnr = it_j_1bnfdoc-parid.
      IF sy-subrc IS INITIAL.
        wa_znom_reme_notas_alv-name1  = it_fornecedor-name1.
        wa_znom_reme_notas_alv-regio  = it_fornecedor-regio.
      ENDIF.

      IF it_j_1bnfdoc-nfe IS INITIAL.
        MOVE it_j_1bnfdoc-nfnum TO wa_znom_reme_notas_alv-nfenum.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_znom_reme_notas_alv-nfenum
          IMPORTING
            output = wa_znom_reme_notas_alv-nfenum.
      ELSE.
        wa_znom_reme_notas_alv-nfenum = it_j_1bnfdoc-nfenum.
      ENDIF.

    ENDIF.
    wa_znom_reme_notas_alv-nr_efetivada = 0.
    LOOP AT it_zdoc_nf_produtor INTO wa_zdoc_nf_produtor WHERE docnum_prod EQ wa_znom_reme_notas_alv-docnum
                                                           AND itmnum_prod EQ wa_znom_reme_notas_alv-itmnum
                                                           AND grp_retorno EQ wa_znom_reme_notas_alv-grp_retorno.

      READ TABLE it_zdoc_exp_recusa_cons WITH KEY vbeln_re_exp = wa_zdoc_nf_produtor-vbeln.

      CHECK sy-subrc NE 0.
*      IF SY-SUBRC EQ 0. "Se Remessa Recusada
*        "Verificar se nota de recusa do produtor não foi trocada
*        READ TABLE IT_ZDOC_EXP_REC_NF_CONS WITH KEY VBELN_RE_EXP = WA_ZDOC_NF_PRODUTOR-VBELN
*                                                    DOCNUM_PROD  = WA_ZDOC_NF_PRODUTOR-DOCNUM_PROD
*                                                    ITMNUM_PROD  = WA_ZDOC_NF_PRODUTOR-ITMNUM_PROD.
*        CHECK SY-SUBRC EQ 0.
*      ENDIF.

      wa_znom_reme_notas_alv-nr_efetivada = wa_znom_reme_notas_alv-nr_efetivada + wa_zdoc_nf_produtor-menge.
    ENDLOOP.

    wa_znom_reme_notas_alv-nr_recusado = 0.
*    LOOP AT IT_ZDOC_EXP_REC_NF WHERE DOCNUM_PROD EQ WA_ZNOM_REME_NOTAS_ALV-DOCNUM
*                                 AND ITMNUM_PROD EQ WA_ZNOM_REME_NOTAS_ALV-ITMNUM.
*      WA_ZNOM_REME_NOTAS_ALV-NR_RECUSADO = WA_ZNOM_REME_NOTAS_ALV-NR_RECUSADO + IT_ZDOC_EXP_REC_NF-MENGE.
*    ENDLOOP.

    READ TABLE it_znom_reme_notas_exl WITH KEY docnum      = wa_znom_reme_notas_alv-docnum
                                               itmnum      = wa_znom_reme_notas_alv-itmnum
                                               grp_retorno = wa_znom_reme_notas_alv-grp_retorno.
    IF sy-subrc IS INITIAL.
      wa_znom_reme_notas_alv-nr_saldo_efetivar = 0.
    ELSE.
      wa_znom_reme_notas_alv-nr_saldo_efetivar = wa_znom_reme_notas_alv-nr_quantidade2 -
                                                 wa_znom_reme_notas_alv-nr_efetivada +
                                                 wa_znom_reme_notas_alv-nr_recusado.
    ENDIF.

    IF wa_znom_reme_notas_alv-nr_efetivada GT 0.
      READ TABLE it_znom_remetente_alv
            INTO wa_znom_remetente_alv
            WITH KEY id_remetente = wa_znom_reme_notas_alv-parid
                     grp_retorno  = wa_znom_reme_notas_alv-grp_retorno.
      IF sy-subrc IS INITIAL.
        vg_tabix_reme = sy-tabix.
        wa_znom_remetente_alv-nr_recusado       = wa_znom_remetente_alv-nr_recusado       + wa_znom_reme_notas_alv-nr_recusado.
        wa_znom_remetente_alv-nr_efetivada      = wa_znom_remetente_alv-nr_efetivada      + wa_znom_reme_notas_alv-nr_efetivada.
        IF wa_znom_reme_notas_alv-nr_saldo_efetivar NE 0.
          wa_znom_remetente_alv-nr_saldo_efetivar = wa_znom_remetente_alv-nr_saldo_efetivar - ( wa_znom_reme_notas_alv-nr_efetivada - wa_znom_reme_notas_alv-nr_recusado ).
        ELSE.
          READ TABLE it_znom_reme_notas_exl WITH KEY docnum      = wa_znom_reme_notas_alv-docnum
                                                     itmnum      = wa_znom_reme_notas_alv-itmnum
                                                     grp_retorno = wa_znom_reme_notas_alv-grp_retorno.
          IF sy-subrc IS INITIAL.
            wa_znom_remetente_alv-nr_saldo_efetivar = wa_znom_remetente_alv-nr_saldo_efetivar - it_znom_reme_notas_exl-nr_quantidade.
          ELSE.
            wa_znom_remetente_alv-nr_saldo_efetivar = wa_znom_remetente_alv-nr_saldo_efetivar - ( wa_znom_reme_notas_alv-nr_efetivada - wa_znom_reme_notas_alv-nr_recusado ).
          ENDIF.
        ENDIF.
        MODIFY it_znom_remetente_alv INDEX vg_tabix_reme FROM wa_znom_remetente_alv TRANSPORTING nr_efetivada nr_saldo_efetivar nr_recusado.
      ENDIF.
    ENDIF.

    READ TABLE it_j_1bnflin WITH KEY docnum = wa_znom_reme_notas_alv-docnum
                                     itmnum = wa_znom_reme_notas_alv-itmnum.
    IF sy-subrc IS INITIAL.
      wa_znom_reme_notas_alv-matnr = it_j_1bnflin-matnr.
      wa_znom_reme_notas_alv-maktx = it_j_1bnflin-maktx.
      wa_znom_reme_notas_alv-nbm   = it_j_1bnflin-nbm.

      wa_znom_reme_notas_alv-charg = it_j_1bnflin-charg.
      wa_znom_reme_notas_alv-cfop	 = it_j_1bnflin-cfop.
    ENDIF.

    "Verifica se documento está estornado pelo XI
    READ TABLE it_znom_reme_notase WITH KEY docnum = wa_znom_reme_notas-docnum
                                            itmnum = wa_znom_reme_notas-itmnum.
    IF sy-subrc IS INITIAL.
      wa_znom_reme_notas_alv-rowcolor = c_grid_color_c600.
    ENDIF.

    IF wa_znom_reme_notas-mandt = 999.
      wa_znom_reme_notas_alv-novo = 'N'.
    ENDIF.

    "WBARBOSA 23102024 - US-153330 --->>>
    zcl_eudr_utils=>check_doc_fiscal_eudr(
      EXPORTING
        i_docnum = wa_znom_reme_notas_alv-docnum " Nº documento
      RECEIVING
        r_eudr   = wa_znom_reme_notas_alv-eudr   " Atende critérios Europeu
    ).
    "WBARBOSA 23102024 - US-153330 <<<---

    APPEND wa_znom_reme_notas_alv TO it_znom_reme_notas_alv.

  ENDLOOP.

  SORT it_znom_reme_notas_alv BY name1 docdat.

  LOOP AT it_znom_remetente INTO wa_znom_remetente WHERE id_remetente EQ space.
    CLEAR: wa_znom_reme_notas_alv.
    wa_znom_reme_notas_alv-grp_retorno       = wa_znom_remetente-grp_retorno.
    wa_znom_reme_notas_alv-docnum            = '9999999999'.
    wa_znom_reme_notas_alv-itmnum            = '999999'.
    wa_znom_reme_notas_alv-nr_quantidade2    = wa_znom_remetente-nr_programada.
    wa_znom_reme_notas_alv-id_unidade        = wa_znom_remetente-id_unidade.
    wa_znom_reme_notas_alv-nr_quantidade     = wa_znom_remetente-nr_programada.
    wa_znom_reme_notas_alv-parid             = wa_znom_remetente-id_filial.
    wa_znom_reme_notas_alv-name1             = p_j_1bbranch-name.

    wa_znom_reme_notas_alv-id_due               = wa_znom_remetente-id_due.
    wa_znom_reme_notas_alv-numero_due           = wa_znom_remetente-numero_due.
    wa_znom_reme_notas_alv-codigo_urf_embarque  = wa_znom_remetente-codigo_urf_embarque.
    wa_znom_reme_notas_alv-codigo_ra_embarque   = wa_znom_remetente-codigo_ra_embarque.

    v_lifnr_aux = p_j_1bbranch-branch.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_lifnr_aux
      IMPORTING
        output = v_lifnr_aux.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(_wl_lfa1)
     WHERE lifnr  =  @v_lifnr_aux.
    IF sy-subrc = 0.
      wa_znom_reme_notas_alv-regio = _wl_lfa1-regio.
    ENDIF.


    wa_znom_reme_notas_alv-matnr             = wa_znom_programacao_alv-id_material.
    wa_znom_reme_notas_alv-maktx             = wa_znom_programacao_alv-ds_material.
    wa_znom_reme_notas_alv-nbm               = wa_znom_programacao_alv-nbm.
    wa_znom_reme_notas_alv-nr_efetivada      = wa_znom_remetente-nr_programada.
    wa_znom_reme_notas_alv-grp_retorno       = wa_znom_remetente-grp_retorno.
    wa_znom_reme_notas_alv-nr_saldo_efetivar = 0.

    wa_znom_reme_notas_alv-nr_recusado       = 0.

    CLEAR: it_znom_prog_reme_aux[].
    SELECT * INTO TABLE it_znom_prog_reme_aux
      FROM znom_prog_reme AS p
     WHERE p~id_nomeacao_tran  EQ wa_znom_remetente-id_nomeacao_tran
       AND p~id_empresa        EQ wa_znom_remetente-id_empresa
       AND p~id_filial         EQ wa_znom_remetente-id_filial
       AND p~id_material       EQ wa_znom_remetente-id_material
       AND NOT EXISTS ( SELECT * FROM zdoc_nf_produtor AS n WHERE n~vbeln EQ p~id_remessa ).

    IF it_znom_prog_reme_aux[] IS NOT INITIAL.
      SELECT * INTO TABLE it_zdoc_exp_recusa
        FROM zdoc_exp_recusa
         FOR ALL ENTRIES IN it_znom_prog_reme_aux

       WHERE vbeln_re_exp EQ it_znom_prog_reme_aux-id_remessa.
      LOOP AT it_zdoc_exp_recusa.
        wa_znom_reme_notas_alv-nr_recusado = wa_znom_reme_notas_alv-nr_recusado + it_zdoc_exp_recusa-nm_quantidade.
      ENDLOOP.
    ENDIF.

    APPEND wa_znom_reme_notas_alv TO it_znom_reme_notas_alv.
    IF it_znom_reme_dnotas_alv[]     IS NOT INITIAL. "ALRS
      MOVE-CORRESPONDING wa_znom_reme_notas_alv TO wa_znom_reme_notas.
      wa_znom_reme_notas-mandt = 999.
      APPEND wa_znom_reme_notas TO it_znom_reme_notas.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_znom_reme_notas_alv  LINES wa_filtro_reme_qtde_sel.

ENDFORM.                    " CONSULTA_NOTAS_VINCULADAS

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_NOTAS_VINCLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_FORNECEDOR  text
*      -->P_J_1BBRANCH   text
*----------------------------------------------------------------------*
*FORM CONSULTA_NOTAS_VINCLOG TABLES IT_FORNECEDOR STRUCTURE LFA1 USING P_J_1BBRANCH TYPE J_1BBRANCH.
*
*  DESCRIBE TABLE IT_ZNOM_REME_NOTAS_ALV  LINES WA_FILTRO_REME_QTDE_SEL.
*
*ENDFORM.                    " CONSULTA_NOTAS_VINCULADAS

*&---------------------------------------------------------------------*
*&      Module  CRIA_COMPONENTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_componentes OUTPUT.

  DATA: url(132),
        query_table    LIKE w3query OCCURS 1 WITH HEADER LINE,
        html_table     LIKE w3html OCCURS 1,
        pic_data       LIKE w3mime OCCURS 0,
        return_code    LIKE  w3param-ret_code,
        content_type   LIKE  w3param-cont_type,
        content_length LIKE  w3param-cont_len,
        pic_size       TYPE i.

  IF plan_prim_0010 IS INITIAL.

    plan_prim_0010 = c_x.

    CREATE OBJECT container_0010
      EXPORTING
        container_name = 'CUSTOM'.

    CREATE OBJECT picture_0010
      EXPORTING
        parent = container_0010.

    REFRESH query_table.
    query_table-name = '_OBJECT_ID'.
    query_table-value = 'ZGRUPOMAGGI'.
    APPEND query_table.

    CALL FUNCTION 'WWW_GET_MIME_OBJECT'
      TABLES
        query_string        = query_table
        html                = html_table
        mime                = pic_data
      CHANGING
        return_code         = return_code
        content_type        = content_type
        content_length      = content_length
      EXCEPTIONS
        object_not_found    = 1
        parameter_not_found = 2
        OTHERS              = 3.
    IF sy-subrc = 0.
      pic_size = content_length.
    ENDIF.

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type     = 'image/jpeg'
        subtype  = cndp_sap_tab_unknown
        size     = pic_size
        lifetime = cndp_lifetime_transaction
      TABLES
        data     = pic_data
      CHANGING
        url      = url
      EXCEPTIONS
        OTHERS   = 1.

    CALL METHOD picture_0010->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.

  ENDIF.

ENDMODULE.                 " CRIA_COMPONENTES  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  LIBERA_NOMEACOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM libera_nomeacoes .

  DATA: BEGIN OF wa_enq.
          INCLUDE STRUCTURE seqg7.
  DATA: END OF wa_enq.

  DATA: garg LIKE seqg3-garg,
        enq  LIKE STANDARD TABLE OF wa_enq.

  IF NOT wa_znom_transporte_alv-id_nomeacao_tran IS INITIAL.

    CONCATENATE sy-mandt wa_znom_transporte_alv-id_nomeacao_tran INTO garg.

    CALL FUNCTION 'ENQUE_READ2'
      EXPORTING
        gname = 'ZNOM_TRANSPORTE'
      TABLES
        enq   = enq.

    CALL FUNCTION 'ZDEQUEUE_ZNOMEACAO'
      EXPORTING
        id_nomeacao_tran = wa_znom_transporte_alv-id_nomeacao_tran.

  ENDIF.

  vg_desbloq_nom = space.

ENDFORM.                    " LIBERA_NOMEACOES

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_NOMEACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprimir_nomeacao .
  DATA: vg_verifica_selecao_nm TYPE sy-subrc.

  PERFORM verifica_selecao_nomeacao USING vg_verifica_selecao_nm.

  IF vg_verifica_selecao_nm IS INITIAL.
    IF ok_code_0001 NE ok_ger_mail_e.
      CALL FUNCTION 'ZPLANCOMP_EMAIL_PROGRAMACAO'
        EXPORTING
          p_id_nomeacao_tran = wa_znom_transporte-id_nomeacao_tran
        EXCEPTIONS
          not_qualified      = 1
          user_not_found     = 2
          address_not_found  = 3
          OTHERS             = 4.
    ELSE.
      CALL FUNCTION 'ZPLANCOMP_EMAIL_PROGRAMACAO'
        EXPORTING
          p_id_nomeacao_tran = wa_znom_transporte-id_nomeacao_tran
          p_executado        = c_x
        EXCEPTIONS
          not_qualified      = 1
          user_not_found     = 2
          address_not_found  = 3
          OTHERS             = 4.
    ENDIF.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " IMPRIMIR_NOMEACAO

MODULE status_0061 OUTPUT.
  SET PF-STATUS 'PF0061'.
  SET TITLEBAR 'T0061'.
ENDMODULE.

MODULE status_0062 OUTPUT.
  SET PF-STATUS 'PF0062'.
  SET TITLEBAR 'T0062'.
ENDMODULE.

MODULE status_0063 OUTPUT.
  SET PF-STATUS 'PF0063'.
  SET TITLEBAR 'T0063'.
ENDMODULE.

*"// wbarbosa 25102024 - US-153330
MODULE status_0064 OUTPUT.
  SET PF-STATUS 'PF0064'.
  SET TITLEBAR 'T0064'.
ENDMODULE.
*"// wbarbosa 25102024 - US-153330

MODULE status_0071 OUTPUT.
  SET PF-STATUS 'PF0071'.
  SET TITLEBAR 'T0071'.
ENDMODULE.

MODULE user_command_0061 INPUT.

  DATA: ex_due TYPE REF TO zcx_due.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      FREE zcl_due.
      CREATE OBJECT zcl_due
        EXPORTING
          i_id_due = wa_due_antecipada-id_due.
      TRY.
          DATA(_alterado) = zcl_due->modify_region( EXPORTING i_land1 = wg_regio_due-land1
                                                              i_regio = wg_regio_due-bland ).
        CATCH zcx_due INTO ex_due.
          ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

      IF _alterado IS NOT INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.



ENDMODULE.

MODULE user_command_0063 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      FREE zcl_due.
      CREATE OBJECT zcl_due
        EXPORTING
          i_id_due = wa_due_antecipada-id_due.
      TRY.
          DATA(_trocada) = zcl_due->troca_due( EXPORTING i_id_due = wg_id_due_troca ).
        CATCH zcx_due INTO ex_due.
          ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

      IF _trocada IS NOT INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.



ENDMODULE.

*"// wbarbosa 25102024 - US-153330
MODULE user_command_0064 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      FREE zcl_due.
      CREATE OBJECT zcl_due
        EXPORTING
          i_id_due = wa_due_antecipada-id_due.
      TRY.
          DATA(is_ok) = zcl_due->reclassificacao_eudr( EXPORTING i_eudr = wg_reclassificacao_eudr ).
        CATCH zcx_due INTO ex_due.
          ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

      IF is_ok IS NOT INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*"// wbarbosa 25102024 - US-153330

MODULE user_command_0062 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      FREE zcl_due.
      CREATE OBJECT zcl_due
        EXPORTING
          i_id_due = wa_due_antecipada-id_due.
      TRY.
          _alterado = zcl_due->modify_tp_exportacao( EXPORTING i_tp_exportacao =  wg_tp_exp_due ).
        CATCH zcx_due INTO ex_due.
          ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

      IF _alterado IS NOT INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_0071 INPUT.

  DATA: wl_0183 TYPE zsdt0183.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      DELETE FROM zsdt0183 WHERE id_nomeacao_tran    = wa_due_ret_conf_alv-id_nomeacao_tran
                             AND bukrs               = wa_due_ret_conf_alv-bukrs
                             AND codigo_ra_embarque  = wa_due_ret_conf_alv-codigo_ra_embarque
                             AND matnr               = wa_due_ret_conf_alv-matnr.

      IF ( wa_due_ret_conf_alv-id_nomeacao_tran    IS INITIAL ) OR
         ( wa_due_ret_conf_alv-bukrs               IS INITIAL ) OR
         ( wa_due_ret_conf_alv-codigo_ra_embarque  IS INITIAL ) OR
         ( wa_due_ret_conf_alv-matnr               IS INITIAL ).
        MESSAGE 'Dados para apontar quantidade não foram encontrados!' TYPE 'S'.
        RETURN.
      ENDIF.

      CLEAR: wl_0183.
      wl_0183-id_nomeacao_tran    = wa_due_ret_conf_alv-id_nomeacao_tran.
      wl_0183-bukrs               = wa_due_ret_conf_alv-bukrs.
      wl_0183-codigo_ra_embarque  = wa_due_ret_conf_alv-codigo_ra_embarque.
      wl_0183-matnr               = wa_due_ret_conf_alv-matnr.
      wl_0183-peso_liq_total      = wg_peso_liq_exp.
      wl_0183-dt_registro         = sy-datum.
      wl_0183-hr_registro         = sy-uzeit.
      wl_0183-us_registro         = sy-uname.

      MODIFY zsdt0183 FROM wl_0183.
      IF sy-subrc NE 0.
        MESSAGE 'Houve um erro ao informar a quantidade!' TYPE 'S'.
        RETURN.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

FORM retificar_due  USING p_lancamento.

  DATA: wa_registro_due TYPE zde_registro_due.

  DATA: vg_verifica_selecao_dr TYPE sy-subrc.

  DATA(_validou) = abap_false.
  PERFORM f_valida_qtde_exp CHANGING _validou.

  CHECK _validou EQ abap_true.

  PERFORM verifica_sel_due_retificacao USING vg_verifica_selecao_dr.
  IF vg_verifica_selecao_dr EQ 0.
    FREE zcl_due.
    CREATE OBJECT zcl_due.

    CLEAR: wa_registro_due.
    wa_registro_due-modo                           = c_due_novo.  "Novo
    wa_registro_due-id_nomeacao_tran               = wa_znom_transporte-id_nomeacao_tran.
    wa_registro_due-retificar                      = abap_true.
    wa_registro_due-id_due_ref                     = wa_due_retificacao_alv-id_due.
    wa_registro_due-forma_exportacao               = '1001'.      "Forma Exportação/Por conta própria
    wa_registro_due-situacao_especial              = '2002'.      "Situação Especial/Embarque antecipado
    wa_registro_due-moeda_cambio                   = 'USD'.       "Moeda Negociação
    wa_registro_due-tp_due                         = '2'.         "Com NF-e
    wa_registro_due-tp_cod_local_despacho          = '281'.       "Em Recinto Alfandegado
    wa_registro_due-fatura_tp_codigo               = '388'.       "Nota Fiscal
    wa_registro_due-codigo_cond_venda              = 'FOB'.       "Código Condição de Venda
    wa_registro_due-codigo_enquadramento           = '80000'.     "Código Enquadramento

    IF p_lancamento EQ abap_true.
      wa_registro_due-lcto_avulso                  = abap_true.
    ENDIF.

    zcl_due->registro_due( EXPORTING i_registro_due = wa_registro_due ).
  ENDIF.

  CLEAR: ok_code_0001.

  PERFORM consulta_due_retificacao.

ENDFORM.

FORM on_f4  USING f_fieldname    TYPE lvc_fname
                  f_fieldvalue   TYPE lvc_value
                  fw_row_no      TYPE lvc_s_roid
                  fcl_event_data TYPE REF TO cl_alv_event_data
                  ft_bad_cells   TYPE lvc_t_modi
                  f_display      TYPE char01.

  DATA: lw_modi TYPE lvc_s_modi.
  DATA: lv_matnr TYPE mara-matnr.

  FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

  ASSIGN fcl_event_data->m_data->* TO <lfst_modi>.
  CHECK sy-subrc = 0.

  READ TABLE  it_due_antecipada_alv INTO wa_due_antecipada_alv INDEX fw_row_no-row_id.
  CHECK sy-subrc = 0.

  CASE f_fieldname.
    WHEN 'MATNR'.

      PERFORM f4_matnr  USING wa_due_antecipada_alv-codigo_ncm
                        CHANGING lv_matnr.

      f_fieldvalue = lv_matnr.

      CLEAR lw_modi.
      lw_modi-row_id    = fw_row_no-row_id.
      lw_modi-fieldname = f_fieldname.
      lw_modi-value = f_fieldvalue.
      APPEND lw_modi TO <lfst_modi>.


      fcl_event_data->m_event_handled = 'X'.

  ENDCASE.

ENDFORM.                                                    " ON_F4

*&---------------------------------------------------------------------*
*&      Form  F4_CODRECEITA
*&---------------------------------------------------------------------*
FORM f4_matnr  USING u_ncm TYPE marc-steuc
               CHANGING f_matnr TYPE mara-matnr.

  TYPES: BEGIN OF ty_mara,
           matnr TYPE mara-matnr,
           maktx TYPE makt-maktx,
         END OF ty_mara.

  DATA: lt_rettab TYPE TABLE OF ddshretval,
        lt_mara   TYPE TABLE OF ty_mara.

  DATA: ls_mara   TYPE ty_mara.
  DATA: lw_rettab TYPE ddshretval.

  SELECT * INTO TABLE @DATA(lt_marc)
    FROM marc
    WHERE steuc = @u_ncm.

  CHECK sy-subrc = 0.

  SORT lt_marc BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_marc COMPARING matnr.

  SELECT * INTO TABLE @DATA(lt_makt)
    FROM makt
    FOR ALL ENTRIES IN @lt_marc
    WHERE matnr = @lt_marc-matnr
      AND spras = @sy-langu.

  SORT lt_makt BY matnr.

  LOOP AT lt_marc INTO DATA(ls_marc).
    ls_mara-matnr = ls_marc-matnr.
    READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_marc-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      ls_mara-maktx = ls_makt-maktx.
    ENDIF.
    APPEND ls_mara TO lt_mara.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MATNR'
      value_org       = 'S'
    TABLES
      value_tab       = lt_mara
      return_tab      = lt_rettab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CHECK sy-subrc = 0.

  READ TABLE lt_rettab INTO lw_rettab INDEX 1.

  CHECK sy-subrc = 0.

  f_matnr = lw_rettab-fieldval.

ENDFORM.                    " F4_CODRECEITA
*&---------------------------------------------------------------------*
*& Form troca_aba_07
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SPACE
*&---------------------------------------------------------------------*
FORM troca_aba_07  USING forcar TYPE c.

  CHECK wa_znom_transporte-id_nomeacao_tran IS NOT INITIAL.

  zcl_eudr_utils=>check_generation_file_geojson(
    EXPORTING
      i_id_nomeacao = wa_znom_transporte-id_nomeacao_tran
    RECEIVING
      r_ok          = DATA(is_ok)
  ).

  IF is_ok IS NOT INITIAL.
    vg_dynnr_xxxx    = vg_dynnr_0073.
    tabpag-activetab = ok_tab07.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0073  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0073 INPUT.

  CASE sy-ucomm.
    WHEN 'DOWNJSON'.
      PERFORM f_download_arquivo.
    WHEN 'UPJSON'.
      PERFORM f_upload_arquivo_geojson.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0073 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0073 OUTPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_download_arquivo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_download_arquivo .

  zcl_eudr_utils=>download_arquivo_geojson( EXPORTING i_id_nomeacao = wa_znom_transporte-id_nomeacao_tran ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UPLOAD_ARQUIVO_GEOJSON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_upload_arquivo_geojson .

  CALL SCREEN 0074 STARTING AT 07 05 ENDING AT 70 05.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0074 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0074 OUTPUT.

  SET PF-STATUS 'PF0074'.
  SET TITLEBAR 'T0074'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0074  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0074 INPUT.


  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      FREE zcl_due.
      CREATE OBJECT zcl_due
        EXPORTING
          i_id_due = wa_due_antecipada-id_due.

      TRY.
          zcl_eudr_utils=>get_arquivo_geoson(
            EXPORTING
              i_id_nomeacao = wa_znom_transporte-id_nomeacao_tran
            RECEIVING
              e_file_geoson = DATA(e_geojson)
          ).

          IF e_geojson IS INITIAL.
            DATA(r_dues) = zcl_eudr_utils=>get_dues_eudr_from_nomeacao( EXPORTING i_id_nomeacao = wa_znom_transporte-id_nomeacao_tran ).

            IF r_dues[] IS NOT INITIAL.
              e_geojson = zcl_eudr_utils=>create_arquivo_geojson( i_id_nomeacao  = wa_znom_transporte-id_nomeacao_tran ).
            ENDIF.

            IF e_geojson IS INITIAL.
              MESSAGE 'Arquivo GEOJSON não encontrado!' TYPE 'I'.
              LEAVE TO SCREEN 0.
            ENDIF.
          ENDIF.

          zcl_due->zif_due~enviar_email_file_eudr(
            EXPORTING
              i_email        = wg_email
              i_file_geojson = e_geojson
              i_id_nomeacao  = wa_znom_transporte-id_nomeacao_tran
          ).

        CATCH zcx_due INTO ex_due.
          ex_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
