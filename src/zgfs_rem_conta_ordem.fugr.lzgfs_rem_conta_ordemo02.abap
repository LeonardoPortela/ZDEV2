*----------------------------------------------------------------------*
***INCLUDE LZGFS_REM_CONTA_ORDEMO02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.

  FREE: t_exctab.

  IF w_zlest0211-doc_transp IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name CS 'MOTOR'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'PESO'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'L_SAFRA_ORDEM_CAR'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'L_NRO_ORDEM_CAR'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'BTN_PESQUISAR'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'BTN_CLEAR_DADOS'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    l_tem_lancamento  = 'X'.
    MOVE '&ELIMINAR' TO w_exctab-fcode.
    APPEND w_exctab  TO t_exctab.
    MOVE '&SALVAR'   TO w_exctab-fcode.
    APPEND w_exctab  TO t_exctab.
  ELSE.
    l_tem_lancamento = ''.

** Usuário precisa eliminar o transporte ( Não pode limpar o modal )
    IF w_zlest0211-placa_cav IS NOT INITIAL OR w_zlest0211-placa_car1 IS NOT INITIAL OR
       w_zlest0211-placa_car2 IS NOT INITIAL OR w_zlest0211-placa_car3 IS NOT INITIAL .
      LOOP AT SCREEN.
        IF screen-name CS 'BTN_CLEAR_DADOS'   OR
*-CS2024000522-12.09.2024-JT-#152417-inicio
           screen-name CS 'BTN_PESQUISAR'     OR
           screen-name CS 'L_SAFRA_ORDEM_CAR' OR
           screen-name CS 'L_NRO_ORDEM_CAR'   OR
           screen-name CS 'L_COD_MOTORISTA'   OR
           screen-name CS 'L_PESO_BRUTO'      OR
           screen-name CS 'L_PESO_TARA'.
*-CS2024000522-12.09.2024-JT-#152417-fim
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  SET PF-STATUS 'ZSDORDEM2' EXCLUDING t_exctab.
  SET TITLEBAR 'ZSDORDEM2'.

  PERFORM init_alv2.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv2.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  PERFORM build_fieldcatalog2.
  PERFORM toolbar_alv2.

  l_stable-row          = abap_true.
  l_stable-col          = abap_true.

  w_layout-zebra        = abap_false.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
*     it_sort              = lt_sort
      it_outtab            = t_transp.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

  IF m_event_handler IS INITIAL.
    CREATE OBJECT m_event_handler.
    SET HANDLER : m_event_handler->toolbar FOR g_grid.
    SET HANDLER : m_event_handler->user_command FOR g_grid.
  ENDIF.

  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid.
*              lcl_event_handler=>on_double_click  FOR g_grid.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = l_stable.

ENDFORM.                    " init_tree

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM build_fieldcatalog2.

  FREE: t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'TIPO_PLACA'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'Tp.Placa'.
  ls_fieldcatalog-edit      = abap_false.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'PC_VEICULO'.
  ls_fieldcatalog-ref_table = 'ZLEST0002'.
  ls_fieldcatalog-ref_field = 'PC_VEICULO'.

  IF l_tem_romaneio = abap_true OR
     w_zlest0211-doc_transp IS NOT INITIAL.
    ls_fieldcatalog-edit    = abap_false.
  ELSE.
    ls_fieldcatalog-edit    = abap_true.
  ENDIF.

  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'Placa'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'CD_CIDADE'.
  ls_fieldcatalog-ref_table = 'ZLEST0002'.
  ls_fieldcatalog-ref_field = 'CD_CIDADE'.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Cidade'.
  ls_fieldcatalog-edit      = abap_false.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'CD_UF'.
  ls_fieldcatalog-ref_table = 'ZLEST0002'.
  ls_fieldcatalog-ref_field = 'CD_UF'.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 03.
  ls_fieldcatalog-dd_outlen = 03.
  ls_fieldcatalog-coltext   = 'UF'.
  ls_fieldcatalog-edit      = abap_false.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'CD_RENAVAM'.
  ls_fieldcatalog-ref_table = 'ZLEST0002'.
  ls_fieldcatalog-ref_field = 'CD_RENAVAM'.
  ls_fieldcatalog-col_pos   = 5.
  ls_fieldcatalog-outputlen = 10.
  ls_fieldcatalog-dd_outlen = 10.
  ls_fieldcatalog-coltext   = 'Renavan'.
  ls_fieldcatalog-edit      = abap_false.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'PROPRIETARIO'.
  ls_fieldcatalog-ref_table = 'ZLEST0002'.
  ls_fieldcatalog-ref_field = 'PROPRIETARIO'.
  ls_fieldcatalog-col_pos   = 6.
  ls_fieldcatalog-outputlen = 12.
  ls_fieldcatalog-dd_outlen = 12.
  ls_fieldcatalog-coltext   = 'Cod.Prop.'.
  ls_fieldcatalog-edit      = abap_false.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'DES_PROPRIETARIO'.
  ls_fieldcatalog-ref_table = 'LFA1'.
  ls_fieldcatalog-ref_field = 'NAME1'.
  ls_fieldcatalog-col_pos   = 7.
  ls_fieldcatalog-outputlen = 19.
  ls_fieldcatalog-dd_outlen = 19.
  ls_fieldcatalog-coltext   = 'Nome Prop.'.
  ls_fieldcatalog-edit      = abap_false.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'TP_VEICULO'.
  ls_fieldcatalog-ref_table = 'ZLEST0002'.
  ls_fieldcatalog-ref_field = 'TP_VEICULO'.
  ls_fieldcatalog-col_pos   = 8.
  ls_fieldcatalog-outputlen = 08.
  ls_fieldcatalog-dd_outlen = 08.
  ls_fieldcatalog-coltext   = 'Tp.Veic.'.

* BUG - 98417 - CBRAND Inicio
*  IF w_vbkd-inco1 = 'CIF' OR l_tem_romaneio = abap_true OR l_tem_lancamento = abap_true.
*    ls_fieldcatalog-edit    = abap_false.
*  ELSE.
*    ls_fieldcatalog-edit    = abap_true.
*  ENDIF.
* BUG - 98417 - CBRAND Fim
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'T_TRANSP'.
  ls_fieldcatalog-fieldname = 'CNPJ_CPF_PROP'.
  ls_fieldcatalog-ref_table = 'LFA1'.
  ls_fieldcatalog-ref_field = 'STCD1'.
  ls_fieldcatalog-col_pos   = 9.
  ls_fieldcatalog-outputlen = 16.
  ls_fieldcatalog-dd_outlen = 16.
  ls_fieldcatalog-coltext   = 'CNPJ/CPF'.
  ls_fieldcatalog-edit      = abap_false.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv2.

  FREE: pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.

* IF ( p_escala = abap_false ) OR l_edit = abap_false.
*   APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
*   APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.
* ENDIF.

ENDFORM.

*******************************************************************************************
* Form monta dados
*******************************************************************************************
FORM f_monta_dados.

  DATA: l_placa TYPE zde_tx_campo.  "*-CS2024000522-18.07.2024-JT-#143588

  CLEAR: w_transp, w_info_k, w_info_c.

*-CS2024000522-18.07.2024-JT-#143588-inicio
* zcl_remessa_terceiro=>zif_remessa_terceiro~get_agente_frete( EXPORTING i_chave_nfe      = g_chave_nf_venda
*                                                                        i_vbeln          = g_vbeln_venda
*                                                              IMPORTING e_placa          = l_placa ).
* IF l_placa IS NOT INITIAL.
*   w_zsdt0001-placa_cav = l_placa.
* ENDIF.
*-CS2024000522-18.07.2024-JT-#143588-fim

  IF w_zsdt0001-placa_cav IS NOT INITIAL.
    CLEAR w_transp.
    READ TABLE t_transp INTO w_transp WITH KEY tipo_placa = 'Placa Cavalo'.
    l_tabix = sy-tabix.

    PERFORM f_dados_veic    USING 'Placa Cavalo'
                                  w_zsdt0001-placa_cav
                         CHANGING w_transp.
    MODIFY t_transp FROM w_transp INDEX l_tabix.
  ENDIF.

  IF w_zsdt0001-placa_car1 IS NOT INITIAL.
    CLEAR w_transp.
    READ TABLE t_transp INTO w_transp WITH KEY tipo_placa = 'Placa Car1'.
    l_tabix = sy-tabix.

    PERFORM f_dados_veic    USING 'Placa Car1'
                                  w_zsdt0001-placa_car1
                         CHANGING w_transp.
    MODIFY t_transp FROM w_transp INDEX l_tabix.
  ENDIF.

  IF w_zsdt0001-placa_car2 IS NOT INITIAL.
    CLEAR w_transp.
    READ TABLE t_transp INTO w_transp WITH KEY tipo_placa = 'Placa Car2'.
    l_tabix = sy-tabix.

    PERFORM f_dados_veic    USING 'Placa Car2'
                                  w_zsdt0001-placa_car2
                         CHANGING w_transp.
    MODIFY t_transp FROM w_transp INDEX l_tabix.
  ENDIF.

  IF w_zsdt0001-placa_car3 IS NOT INITIAL.
    CLEAR w_transp.
    READ TABLE t_transp INTO w_transp WITH KEY tipo_placa = 'Placa Car3'.
    l_tabix = sy-tabix.

    PERFORM f_dados_veic    USING 'Placa Car3'
                                  w_zsdt0001-placa_car3
                         CHANGING w_transp.
    MODIFY t_transp FROM w_transp INDEX l_tabix.
  ENDIF.

  IF w_zsdt0001-motorista IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_zsdt0001-motorista
      IMPORTING
        output = w_zsdt0001-motorista.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = w_zsdt0001-motorista
        p_partype    = 'V'
      CHANGING
        wa_info_part = w_info_k
        wa_info_c    = w_info_c.

    IF w_info_k-ktokk = 'ZMOT'.
      l_cod_motorista   = w_info_k-lifnr.
      l_nome_motorista  = w_info_k-name1.

      IF      w_info_k-stcd1 IS NOT INITIAL.
        l_cpf_motorista = w_info_k-stcd1.
      ELSEIF w_info_k-stcd2 IS NOT INITIAL.
        l_cpf_motorista = w_info_k-stcd2.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

*******************************************************************************************
* Form dados veiculo
*******************************************************************************************
FORM f_dados_veic    USING p_tp_veic
                           p_placa
                  CHANGING w_transp TYPE ty_transp.

  CLEAR: w_zlest0002,
         w_lfa1.

  SELECT SINGLE *
    INTO w_zlest0002
    FROM zlest0002
   WHERE pc_veiculo = p_placa.

  SELECT SINGLE *
    INTO w_lfa1
    FROM lfa1
   WHERE lifnr = w_zlest0002-proprietario.

  w_transp-pc_veiculo        = p_placa.
  w_transp-cd_cidade         = w_zlest0002-cd_cidade.
  w_transp-cd_uf             = w_zlest0002-cd_uf.
  w_transp-cd_renavam        = w_zlest0002-cd_renavam.
  w_transp-proprietario      = w_zlest0002-proprietario.
  w_transp-tp_veiculo        = w_zlest0002-tp_veiculo.
  w_transp-des_proprietario  = w_lfa1-name1.

  IF     w_lfa1-stcd1 IS NOT INITIAL.
    w_transp-cnpj_cpf_prop   = w_lfa1-stcd1.
  ELSEIF w_lfa1-stcd2 IS NOT INITIAL.
    w_transp-cnpj_cpf_prop   = w_lfa1-stcd2.
  ENDIF.

***  BUG  - 98417 - CBRAND  Inicio
*  IF ( w_vbkd-inco1 = 'CPT' AND l_tem_romaneio = abap_false )
***  BUG  - 98417 - CBRAND  FIm
  IF ( w_vbkd-inco1 = 'CPT' AND l_tem_romaneio = abap_false ) OR ( vg_tipo_frete = 'CPT' AND g_tipo_proc = 'T' ).
    IF p_tp_veic = 'Placa Cavalo'.
      w_transp-tp_veiculo    = w_zlest0211-tp_veiculo_cav.
    ENDIF.
    IF p_tp_veic = 'Placa Car1'.
      w_transp-tp_veiculo    = w_zlest0211-tp_veiculo_car1.
    ENDIF.
    IF p_tp_veic = 'Placa Car2'.
      w_transp-tp_veiculo    = w_zlest0211-tp_veiculo_car2.
    ENDIF.
    IF p_tp_veic = 'Placa Car3'.
      w_transp-tp_veiculo    = w_zlest0211-tp_veiculo_car3.
    ENDIF.
  ENDIF.

ENDFORM.

*******************************************************************************************
* Form valida dados
*******************************************************************************************
FORM f_valida_dados  CHANGING p_ok.

  DATA l_erro TYPE c.

  FREE: l_erro, w_info_k, w_info_c.

  p_ok = abap_true.

*  " US - 92467 - CBRAND - Inicio
*  IF g_tipo_proc IS INITIAL.
*  IF w_vbkd-inco1 = 'CIF'
*    " US - 92467 - CBRAND - Fim
  IF w_vbkd-inco1 = 'CIF' OR ( vg_tipo_frete = 'CIF' AND g_tipo_proc = 'T' ).
*--------------------------
*-- CIF
*--------------------------
    LOOP AT t_transp INTO w_transp.
      IF w_transp-tipo_placa = 'Placa Cavalo'.
        IF w_transp-pc_veiculo IS INITIAL.
          l_erro = '1'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          INTO w_zlest0002
          FROM zlest0002
         WHERE pc_veiculo = w_transp-pc_veiculo.
        IF sy-subrc <> 0.
          l_erro = '2'.
          EXIT.
        ENDIF.

        IF w_zlest0002-tp_veiculo <> '0'.
          l_erro = '3'.
          EXIT.
        ENDIF.
      ENDIF.

      IF w_transp-tipo_placa = 'Placa Car1' OR
         w_transp-tipo_placa = 'Placa Car2' OR
         w_transp-tipo_placa = 'Placa Car3'.
        IF w_transp-pc_veiculo IS NOT INITIAL.
          SELECT SINGLE *
            INTO w_zlest0002
            FROM zlest0002
           WHERE pc_veiculo = w_transp-pc_veiculo.
          IF sy-subrc <> 0.
            l_erro = '2'.
            EXIT.
          ENDIF.

          IF w_zlest0002-tp_veiculo <> '1'.
            l_erro = '4'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
*--------------------------
*-- CPT
*--------------------------
*-CS2024000522-29.08.2024-JT-#150113-inicio - comentado
*    LOOP AT t_transp INTO w_transp.
*      IF w_transp-tipo_placa = 'Placa Cavalo'.
*        IF w_transp-pc_veiculo IS INITIAL.
*          l_erro = '1'.
*          EXIT.
*        ENDIF.
*
*        IF strlen( w_transp-pc_veiculo ) <> 7.
*          l_erro = '5'.
*          EXIT.
*        ENDIF.
*
*        "===============Comentado ajuste realizado referente problema em PRD --- 13/04/2023 / AOENNING.
**        IF w_transp-tp_veiculo <> '0'.
**          l_erro = '3'.
**          EXIT.
**        ENDIF.
*        "===============Comentado ajuste realizado referente problema em PRD --- 13/04/2023 / AOENNING.
*      ENDIF.
*
*      IF w_transp-tipo_placa = 'Placa Car1' OR
*         w_transp-tipo_placa = 'Placa Car2' OR
*         w_transp-tipo_placa = 'Placa Car3'.
*        IF w_transp-pc_veiculo IS NOT INITIAL.
*          IF w_transp-tp_veiculo <> '1'.
*            l_erro = '4'.
*            EXIT.
*          ENDIF.
*
*          IF strlen( w_transp-pc_veiculo ) <> 7.
*            l_erro = '5'.
*            EXIT.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*-CS2024000522-29.08.2024-JT-#150113-fim - comentado
  ENDIF.

  t_transp_aux1[] = t_transp[].
  t_transp_aux2[] = t_transp[].

  DELETE t_transp_aux1 WHERE pc_veiculo IS INITIAL.
  DELETE t_transp_aux2 WHERE pc_veiculo IS INITIAL.

  SORT t_transp_aux1 BY pc_veiculo.
  DELETE ADJACENT DUPLICATES FROM t_transp_aux1
                        COMPARING pc_veiculo.

  IF lines( t_transp_aux1[] ) <> lines( t_transp_aux2[] ).
    p_ok = abap_false.
    MESSAGE s024(sd) WITH TEXT-014 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF     l_erro = '1'.
    p_ok = abap_false.
    MESSAGE s024(sd) WITH TEXT-010 DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF l_erro = '2'.
    p_ok = abap_false.
    MESSAGE s024(sd) WITH TEXT-011 DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF l_erro = '3'.
    p_ok = abap_false.
    MESSAGE s024(sd) WITH TEXT-012 DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF l_erro = '4'.
    p_ok = abap_false.
    MESSAGE s024(sd) WITH TEXT-013 DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF l_erro = '5'.
    p_ok = abap_false.
    MESSAGE s024(sd) WITH TEXT-015 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  " US - 92467 - CBRAND - Inicio
*  IF w_vbkd-inco1 = 'CIF'
*  " US - 92467 - CBRAND - Fim
  IF w_vbkd-inco1 = 'CIF' OR ( vg_tipo_frete = 'CIF' AND g_tipo_proc = 'T' ).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_cod_motorista
      IMPORTING
        output = l_cod_motorista.

*--------------------------
* checa cod motorista
*--------------------------
    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = l_cod_motorista
        p_partype    = 'V'
      CHANGING
        wa_info_part = w_info_k
        wa_info_c    = w_info_c.

    IF w_info_k-ktokk <> 'ZMOT'.
      p_ok = abap_false.
      MESSAGE s024(sd) WITH TEXT-002 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  " US - 92467 - CBRAND - Inicio
  IF g_safra IS NOT INITIAL OR g_nr_ordem   IS NOT INITIAL .
    IF l_peso_tara IS INITIAL OR l_peso_bruto IS INITIAL.
      p_ok = abap_false.
      MESSAGE s024(sd) WITH TEXT-016 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.
  " US - 92467 - CBRAND - Fim

ENDFORM.


*******************************************************************************************
* Form gravar dados
*******************************************************************************************
FORM f_grava_transp.
  " US - 92467 - CBRAND - Inicio
  IF g_tipo_proc IS INITIAL.
    " US - 92467 - CBRAND - Fim
*--------------------------------
* gravar tabelas transporte
*--------------------------------
    TRY.
        zcl_remessa_terceiro=>zif_remessa_terceiro~set_salva_transporte(
            EXPORTING i_vbeln_venda   = g_vbeln_venda
                      i_ov_dummy      = g_ov_dummy
                      i_remessa_dummy = g_remessa_dummy
                      i_refkey        = g_refkey
                      i_ag_frete      = g_ag_frete
                      i_cod_motorista = l_cod_motorista
                      i_nr_safra      = l_safra_ordem_car  "*-CS2024000522-12.09.2024-JT-#152417-inicio
                      i_nr_ordem      = l_nro_ordem_car    "*-CS2024000522-12.09.2024-JT-#152417-inicio
                      i_id_ordem      = l_id_ordem         "*-CS2024000522-12.09.2024-JT-#152417-inicio
                      i_nf_remessa    = g_nf_remessa
                      i_nf_venda      = g_nf_venda
                      i_p_bruto       = l_peso_bruto
                      i_p_liquido     = l_peso_tara
                      t_tab_transp    = t_transp ).
      CATCH zcx_remessa_terceiro INTO DATA(ex_zcx_remessa_terceiro).
      CATCH zcx_veiculos         INTO DATA(ex_zcx_veiculos).
    ENDTRY.

  ELSE.
    IF g_tipo_proc = 'T'.
*--------------------------------
* gravar tabelas transporte
*--------------------------------
      TRY.
          zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_salva_transporte(
              EXPORTING i_rem_vbeln     = g_rem_vbeln
                        i_rem_posnr     = g_rem_posnr
                        i_ag_frete      = g_ag_frete
                        i_ped_bsart     = g_bsart
                        i_cod_motorista = l_cod_motorista
                        i_nr_safra      = l_safra_ordem_car
                        i_nr_ordem      = l_nro_ordem_car
                        i_id_ordem      = l_id_ordem
                        i_p_bruto       = l_peso_bruto
                        i_p_liquido     = l_peso_tara
                        t_tab_transp    = t_transp ).
        CATCH zcx_frete_remessa_trans INTO DATA(ex_zcx_frete_remessa_trans).
        CATCH zcx_veiculos         INTO ex_zcx_veiculos.
      ENDTRY.
    ENDIF.
  ENDIF.

ENDFORM.

*******************************************************************************************
* Form elimina dados transporte
*******************************************************************************************
FORM f_elimina_dados.
  IF g_tipo_proc = 'T'.
    zcl_frete_remessa_trans=>zif_frete_remessa_trans~set_elimina_transporte(
    EXPORTING i_rem_vbeln = g_rem_vbeln ).

  ELSE.
*--------------------------------
* elimina tabelas transporte
*--------------------------------
    zcl_remessa_terceiro=>zif_remessa_terceiro~set_elimina_transporte(
        EXPORTING i_remessa_dummy = g_remessa_dummy ).
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DADOS_VEIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_dados_placa USING w_transp TYPE  ty_transp
                                p_idx_placa   TYPE lvc_index.

  IF ( w_transp-pc_veiculo IS NOT INITIAL ).

    CLEAR w_zlest0002.
    SELECT SINGLE * INTO w_zlest0002 FROM zlest0002 WHERE pc_veiculo = w_transp-pc_veiculo.

    IF sy-subrc EQ 0.
      w_transp-cd_cidade         = w_zlest0002-cd_cidade.
      w_transp-cd_uf             = w_zlest0002-cd_uf.
      w_transp-cd_renavam        = w_zlest0002-cd_renavam.
      w_transp-proprietario      = w_zlest0002-proprietario.
      w_transp-tp_veiculo        = w_zlest0002-tp_veiculo.

      CLEAR w_lfa1.
      SELECT SINGLE * INTO w_lfa1 FROM lfa1 WHERE lifnr = w_transp-proprietario.
      w_transp-des_proprietario  = w_lfa1-name1.

      IF w_lfa1-stcd1 IS NOT INITIAL.
        w_transp-cnpj_cpf_prop     = w_lfa1-stcd1.
      ELSEIF w_lfa1-stcd2 IS NOT INITIAL.
        w_transp-cnpj_cpf_prop = w_lfa1-stcd2.
      ENDIF.

      MODIFY t_transp FROM w_transp INDEX p_idx_placa .
    ENDIF.

  ENDIF.

ENDFORM.                    " ATUALIZA_DADOS_VEIC
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_CAMPOS_MODAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_campos_modal .
  DATA: obj_ws_ord_car TYPE REF TO zcl_webservice_ord_car,
        obj_ord_car    TYPE REF TO zcl_ordem_car.

  DATA: vl_safra        TYPE string,
        vl_nr_ordem     TYPE string,
        vl_mensagem_ret TYPE string,
        vl_msg_exibir   TYPE string,
        vl_idx_placa    TYPE lvc_index.

  SELECT SINGLE *
     FROM lfa1
     INTO @DATA(wlfa1)
     WHERE lifnr = @g_ag_frete.

  IF wlfa1-ktokk = 'ZFIC'.
    vg_tipo_frete = 'CIF'.
  ELSE.
    vg_tipo_frete = 'CPT'.
  ENDIF.


  "Só habilita os campos após limpar os dados do Modal e não estar em modo de visualização.
  IF r_atrib IS INITIAL.

    IF ( l_safra_ordem_car IS NOT INITIAL ) AND
       ( l_nro_ordem_car   IS NOT INITIAL ) AND
       ( t_transp[] IS INITIAL ) AND
       ( vg_view_transp IS INITIAL )  AND
       ( vg_tipo_frete NE 'CPT' ).

      REFRESH: t_transp[].
      CLEAR: l_cod_motorista.

      vl_safra    = l_safra_ordem_car.
      vl_nr_ordem = l_nro_ordem_car.

      FREE: obj_ws_ord_car, obj_ord_car.

      CREATE OBJECT obj_ws_ord_car.
      CREATE OBJECT obj_ord_car.

" ajuste busca da OC incluir filtro por filial #190702 - BG INICIO

select single werks from vbap into @data(v_werks) where VBELN = @g_vbeln_venda.

      obj_ws_ord_car->buscar_transporte( EXPORTING
                                             i_safra    = vl_safra
                                             i_nr_ordem = vl_nr_ordem
                                             I_FILIAL   = v_werks
                                         RECEIVING
                                             e_ordem_car = obj_ord_car ).
" ajuste busca da OC incluir filtro por filial #190702 - BG - FIM
      IF obj_ord_car IS NOT INITIAL.

        vl_mensagem_ret = obj_ord_car->get_mensagem_ret( ).

        IF vl_mensagem_ret = 'Sucesso'.

          w_transp-tipo_placa = 'Placa Cavalo'.
          w_transp-pc_veiculo = obj_ord_car->get_placa_cav( ).
          APPEND w_transp TO t_transp.

          w_transp-tipo_placa = 'Placa Car1'.
          w_transp-pc_veiculo = obj_ord_car->get_placa1( ).
          APPEND w_transp TO t_transp.

          w_transp-tipo_placa = 'Placa Car2'.
          w_transp-pc_veiculo = obj_ord_car->get_placa2( ).
          APPEND w_transp TO t_transp.

          w_transp-tipo_placa = 'Placa Car3'.
          w_transp-pc_veiculo = obj_ord_car->get_placa3( ).  "*-CS2024000522-12.09.2024-JT-#152417-inicio
          APPEND w_transp TO t_transp.

          l_cod_motorista  = obj_ord_car->get_motorista( ).
          l_id_ordem = obj_ord_car->at_zsdt0001od-id_ordem.

          vl_idx_placa = 1.
          LOOP AT t_transp INTO w_transp.
            PERFORM atualiza_dados_placa USING w_transp  vl_idx_placa.
            ADD 1 TO  vl_idx_placa.
          ENDLOOP.

          IF ( t_transp[] IS INITIAL ) OR ( l_cod_motorista IS INITIAL ).
            REFRESH: t_transp.
            CLEAR: l_cod_motorista.
          ENDIF.

        ELSE.
          IF ( t_transp[] IS INITIAL ) OR ( l_cod_motorista IS INITIAL ).
            REFRESH: t_transp.
            CLEAR: l_cod_motorista.
          ENDIF.

          MESSAGE vl_mensagem_ret TYPE 'S'.
          RETURN.
        ENDIF.
      ELSE.
        IF ( t_transp[] IS INITIAL ) OR ( l_cod_motorista  IS INITIAL ).
          REFRESH: t_transp[].
          CLEAR: l_cod_motorista .
        ENDIF.
        MESSAGE 'Não foi possível fazer a busca dos dados do Modal!' TYPE 'S'.
        RETURN.
      ENDIF.
    ENDIF.


    IF ( l_cod_motorista IS NOT INITIAL ).

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = l_cod_motorista
          p_partype    = 'V'
        CHANGING
          wa_info_part = w_info_k
          wa_info_c    = w_info_c.

      IF w_info_k-ktokk <> 'ZMOT'.
        CLEAR: vl_msg_exibir.
        CONCATENATE 'Dados do cadastro do Motorista(' l_cod_motorista ') estão inválidos!'
               INTO vl_msg_exibir SEPARATED BY space.

        CLEAR: l_nome_motorista,l_cpf_motorista, l_cod_motorista.
        REFRESH: t_transp[].
        MESSAGE vl_msg_exibir TYPE 'S'.
      ELSE.
        l_nome_motorista = w_info_k-name1.
        l_cpf_motorista  = w_info_k-stcd2.
      ENDIF.

    ENDIF.

    IF ( t_transp[] IS INITIAL ) OR ( l_cod_motorista IS INITIAL AND vg_tipo_frete = 'CIF' ).
      REFRESH: t_transp[].
      CLEAR: l_cod_motorista.
    ENDIF.

    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'L_SAFRA_ORDEM_CAR'    OR
             'L_NRO_ORDEM_CAR'.
          IF vg_tipo_frete EQ 'CPT' .
            screen-input  = '0'.
          ELSEIF ( t_transp[] IS INITIAL ) AND ( vg_view_transp IS INITIAL ).
            screen-input  = '1'.
          ELSE.
            screen-input  = '0'.
          ENDIF.
          MODIFY SCREEN.
        WHEN 'L_COD_MOTORISTA'.
          IF vg_tipo_frete EQ 'CPT' .
            screen-input  = '1'.
          ELSE.
            screen-input  = '0'.
          ENDIF.
          MODIFY SCREEN.
      ENDCASE.

    ENDLOOP.

  ELSE.

*---------------------------------------------------------------------
*---------------------------------------------------------------------
* Tratativa de tela para Atribuição
*---------------------------------------------------------------------
*---------------------------------------------------------------------
    IF ( l_safra_ordem_car IS NOT INITIAL ) AND
       ( l_nro_ordem_car   IS NOT INITIAL ) AND
       ( t_transp[] IS INITIAL ).

      REFRESH: t_transp.
      CLEAR: l_cod_motorista.

      vl_safra    = l_safra_ordem_car.
      vl_nr_ordem = l_nro_ordem_car .

      FREE: obj_ws_ord_car, obj_ord_car.

      CREATE OBJECT obj_ws_ord_car.
      CREATE OBJECT obj_ord_car.

      obj_ws_ord_car->buscar_transporte( EXPORTING
                                             i_safra    = vl_safra
                                             i_nr_ordem = vl_nr_ordem
                                         RECEIVING
                                             e_ordem_car = obj_ord_car ).

      IF obj_ord_car IS NOT INITIAL.

        vl_mensagem_ret = obj_ord_car->get_mensagem_ret( ).

        IF vl_mensagem_ret = 'Sucesso'.

          w_transp-tipo_placa = 'Placa Cavalo'.
          w_transp-pc_veiculo = obj_ord_car->get_placa_cav( ).
          APPEND w_transp TO t_transp.

          w_transp-tipo_placa = 'Placa Car1'.
          w_transp-pc_veiculo = obj_ord_car->get_placa1( ).
          APPEND w_transp TO t_transp.

          w_transp-tipo_placa = 'Placa Car2'.
          w_transp-pc_veiculo = obj_ord_car->get_placa2( ).
          APPEND w_transp TO t_transp.

          l_cod_motorista  = obj_ord_car->get_motorista( ).
          l_id_ordem = obj_ord_car->at_zsdt0001od-id_ordem.

          vl_idx_placa = 1.
          LOOP AT t_transp INTO w_transp.
            PERFORM atualiza_dados_placa USING w_transp  vl_idx_placa.
            ADD 1 TO  vl_idx_placa.
          ENDLOOP.

          IF ( t_transp[] IS INITIAL ) OR ( l_cod_motorista IS INITIAL ).
            REFRESH: t_transp.
            CLEAR: l_cod_motorista.
          ENDIF.

        ELSE.
          IF ( t_transp[] IS INITIAL ) OR ( l_cod_motorista IS INITIAL ).
            REFRESH: t_transp[].
            CLEAR: l_cod_motorista.
          ENDIF.

          MESSAGE vl_mensagem_ret TYPE 'S'.
          RETURN.
        ENDIF.

      ELSE.
        IF ( t_transp[] IS INITIAL ) OR ( l_cod_motorista IS INITIAL ).
          REFRESH: t_transp[].
          CLEAR: l_cod_motorista.
        ENDIF.
        MESSAGE 'Não foi possível fazer a busca dos dados do Modal!' TYPE 'S'.
        RETURN.
      ENDIF.

    ENDIF.

    IF ( l_cod_motorista IS NOT INITIAL ).

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = l_cod_motorista
          p_partype    = 'V'
        CHANGING
          wa_info_part = w_info_k
          wa_info_c    = w_info_c.

      IF w_info_k-ktokk <> 'ZMOT'.
        CLEAR: vl_msg_exibir.
        CONCATENATE 'Dados do cadastro do Motorista(' l_cod_motorista ') estão inválidos!'
               INTO vl_msg_exibir SEPARATED BY space.

        CLEAR: l_nome_motorista, l_cpf_motorista, l_cod_motorista.
        REFRESH:  t_transp[].
        MESSAGE vl_msg_exibir TYPE 'S'.
      ELSE.
        l_nome_motorista = w_info_k-name1.
        l_cpf_motorista  = w_info_k-stcd2.
      ENDIF.

    ENDIF.

    IF ( t_transp[] IS INITIAL ) OR ( l_cod_motorista IS INITIAL ).
      REFRESH: t_transp[].
      CLEAR: l_cod_motorista.
    ENDIF.

    LOOP AT SCREEN.
*      CASE screen-name.
      IF screen-name = 'L_SAFRA_ORDEM_CAR'    OR
        screen-name =  'L_NRO_ORDEM_CAR' OR
         screen-name =  'L_COD_MOTORISTA'.
*          IF VG_TIPO_FRETE EQ 'CPT' .
*            SCREEN-INPUT  = '0'.
*          ELSE
        IF ( t_transp[] IS INITIAL ) AND ( vg_view_transp IS INITIAL ).
          screen-input  = '1'.
        ELSE.
          screen-input  = '0'.
        ENDIF.
        MODIFY SCREEN.
*        WHEN 'WA_DADOS_TRANSP-MOTORISTA'.
*          IF vg_tipo_frete EQ 'CPT' .
*            screen-input  = '1'.
*          ELSE.
*            screen-input  = '0'.
*          ENDIF.
*          MODIFY SCREEN.
      ENDIF.

    ENDLOOP.

  ENDIF.


ENDFORM.
