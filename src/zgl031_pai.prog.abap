*&---------------------------------------------------------------------*
*&  Include           ZGL031_PAI
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2JVI |12/05/2025 |Ajuste Campo Competencia para  &*
*&                                    |range para os tipos 17, 29, 21 &*
*&                                    |e '22'                         &*
*&                                    |Chamado: 164255.               &*
*&--------------------------------------------------------------------&*
MODULE pai_0100 INPUT.
  DATA: r_insert_row TYPE REF TO z_insert_row_alv,
        lv_answer    TYPE n.

  CREATE OBJECT r_insert_row.
  CREATE OBJECT go_utils.

  CASE sy-ucomm.
    WHEN c_back.
      IF ( wl_cabecalho_0110-seq_lcto IS INITIAL ).
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Gravar registros'
            text_question         = 'Tem certeza que deseja sair sem salvar os registros?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_OKAY'
            text_button_2         = 'Não'
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ''
          IMPORTING
            answer                = lv_answer.

        IF ( lv_answer = 1 ).
          LEAVE TO SCREEN 0.
        ELSE.
          EXIT.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN c_show_msgre.
      go_utils->z_show_splitter_error( i_show = 'X' ).

*    WHEN C_CANC OR C_EXIT.
*      LEAVE PROGRAM.

    WHEN c_tp_seguro.

      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
        EXPORTING
          tcode  = 'ZGL045'
        EXCEPTIONS
          ok     = 1
          not_ok = 2.

      IF ( sy-subrc IS INITIAL ).
        CALL TRANSACTION 'ZGL045'. "Tela de parametrização de tipo seguro.
      ELSE.
        MESSAGE e077(s#) WITH 'ZGL045'.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT

*----------------------------------------------------------------------*
*  MODULE PAI_0110 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pai_0110 INPUT.
  DATA : fnam TYPE char40,
         fval TYPE char40.

  DATA: r_tipo_operacao  TYPE REF TO z_tipo_operacao,
        r_seguro_geracao TYPE REF TO z_seguro_geracao,
        r_event_click    TYPE REF TO lcl_event_handler,
        v_ans            TYPE n,
        vl_error         TYPE c,
        vl_nr_lote       TYPE zglt034-lote.

  CREATE OBJECT: r_tipo_operacao,
                 r_seguro_geracao,
                 r_insert_row,
                 go_utils.

  CASE sy-ucomm.

    WHEN c_enter.

*      IF WL_CABECALHO_0110-TP_OPR <> 'P'.
      PERFORM f_action_enter.
*      ELSE.
*        PERFORM F_ACTION_ENTER_PRORROG.
*      ENDIF.

    WHEN c_change.

      PERFORM f_action_enter.

      CHECK ( wl_cabecalho_0110-seq_lcto IS NOT INITIAL ).

      op_modo = c_change.

      r_tipo_operacao->z_modo_edicao( ).

    WHEN c_dele.
      PERFORM f_action_enter.

      CHECK ( wl_cabecalho_0110-seq_lcto IS NOT INITIAL ).

      op_modo = c_dele.

      r_tipo_operacao->z_del_apolice( ).

    WHEN c_tipo_moeda.

      IF ( wl_cabecalho_0110-seq_lcto IS INITIAL ) AND " Novo
         ( wl_cabecalho_0110-tp_opr   IS INITIAL ).    " Não informou tipo Operação
        MESSAGE TEXT-e40 TYPE 'S'.
        r_tipo_operacao->z_clear_referencia( ).
        RETURN.
      ENDIF.

      CASE wl_cabecalho_0110-tp_opr.
        WHEN 'N'. "Novo
          LOOP AT gt_fields WHERE campo NE 'WL_CABECALHO_0110-TP_OPR'.
            DELETE gt_fields.
          ENDLOOP.
        WHEN 'B' OR 'E' OR 'P'. "Baixa ou Endosso
          LOOP AT gt_fields WHERE campo NE 'WL_CABECALHO_0110-BUKRS'
                              AND campo NE 'WL_CABECALHO_0110-TP_OPR'
                              AND campo NE 'WL_CABECALHO_0110-COD_SEGURADORA'
                              AND campo NE 'WL_CABECALHO_0110-WAERS'.
            DELETE gt_fields.
          ENDLOOP.
      ENDCASE.

      op_modo = c_tipo_moeda.

      IF ( wl_cabecalho_0110-waers = 'BRL' ).
        CLEAR: wl_cabecalho_0110-vlr_premio_usd,
               wl_cabecalho_0110-vlr_asseg_usd.

        go_utils->z_tratar_campos( name      = space
                                   group1    = space
                                   group2    = 'GR3'
                                   value     = '0'
                                   invisible = '0' ).

      ELSEIF ( wl_cabecalho_0110-waers = 'USD').
        CLEAR: wl_cabecalho_0110-vlr_premio_brl,
               wl_cabecalho_0110-vlr_asseg_brl.

        go_utils->z_tratar_campos( name      = space
                                   group1    = space
                                   group2    = 'GR2'
                                   value     = '0'
                                   invisible = '0' ).
      ENDIF.

    WHEN c_search.
      PERFORM f_action_search.

    WHEN c_novo.
      REFRESH gt_fields.

      op_modo = c_novo.

      go_utils->z_tratar_campos( name      = space
                                 group1    = 'NU1'
                                 group2    = space
                                 value     = '0'
                                 invisible = '0' ).

      go_utils->z_tratar_campos( name      = space
                                 group1    = 'GR1'
                                 group2    = space
                                 value     = '1'
                                 invisible = '0' ).

      custom_mode = 0.
      r_tipo_operacao->z_criar_nova_operacao( ).

    WHEN c_save.
      REFRESH: gt_msg_return,
               lt_nodes_select.
      CLEAR: l_active_node.

      go_utils->z_validar_cabecalho_0110( ).

*     Verifico se o cabeçalho foi preenchido e analiso os cadastros "Contas a Pagar & Bens Assegurados".
*     Se houver erro em algum dos cadastros, é necessário corrigir antes de verificar o próximo.

      CHECK ( return_status IS INITIAL ).

*      IF ( WL_CABECALHO_0110-SEQ_LCTO IS INITIAL ) AND
*         ( WL_CABECALHO_0110-SEQ_PARC <> LINES( GT_SAIDA_0120 ) ).

      IF ( wl_cabecalho_0110-seq_parc <> lines( gt_saida_0120 ) ).
        r_insert_row->insert_row_tela_0120( ).
      ENDIF.

      go_utils->z_validar_info_alv_0120( ).

      IF wl_cabecalho_0110-tp_opr EQ 'B'.
        go_utils->z_ajusta_vlr_baixa( IMPORTING e_error = vl_error ).
        CHECK vl_error IS INITIAL.
      ENDIF.

      IF ( gt_msg_return IS NOT INITIAL ).
        l_active_node = 2.
        APPEND l_active_node TO lt_nodes_select.

      ELSE.
        go_utils->z_validar_info_alv_0130( i_valida_tot = 'X' ).

        IF ( gt_msg_return IS NOT INITIAL ).
          l_active_node = 3.
          APPEND l_active_node TO lt_nodes_select.
        ENDIF.
      ENDIF.


      IF ( gt_msg_return IS INITIAL ).
        r_tipo_operacao->z_salvar_registros( ).
      ELSE.
        go_utils->z_show_splitter_error( i_show = 'X' ).
      ENDIF.

*     Aqui defino qual o item (node) deve ser selecionado.
      CALL METHOD obj_alv_tree->set_selected_nodes
        EXPORTING
          it_selected_nodes = lt_nodes_select.

      CREATE OBJECT r_event_click.
      r_event_click->handle_double_click( EXPORTING node_key = l_active_node ).

    WHEN c_tp_opr.

      IF wl_cabecalho_0110-tp_opr <> 'P'.

        IF ( wl_cabecalho_0110-inf_tp_opr IS INITIAL ) AND
           ( wl_cabecalho_0110-seq_lcto   IS INITIAL ).
          r_tipo_operacao->z_check_operacao( ).
        ENDIF.

      ELSE.

        IF ( wl_cabecalho_0110-inf_tp_opr IS INITIAL ) AND
           ( wl_cabecalho_0110-seq_lcto   IS INITIAL ).
          r_tipo_operacao->z_check_operacao_prorrog( ).
        ENDIF.

      ENDIF.

    WHEN c_ger_ctb_apol.

      IF op_modo = c_change.
        MESSAGE s836(sd) WITH TEXT-e64 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CHECK wl_cabecalho_0110-seq_lcto IS NOT INITIAL.
      CHECK wl_cabecalho_0110-tp_opr NE 'B'. "B = Baixa

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Gerar Contábil'
          text_question         = TEXT-i48
          text_button_1         = 'Sim'
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'Não'
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = ''
        IMPORTING
          answer                = v_ans.

      CHECK v_ans = 1 .
      CREATE OBJECT r_seguro_geracao.
      r_seguro_geracao->z_gerar_ctb_apolice( ).
      PERFORM f_action_enter.

    WHEN c_est_ctb_apol.

      CHECK wl_cabecalho_0110-seq_lcto IS NOT INITIAL.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Estornar Contábil'
          text_question         = TEXT-i49
          text_button_1         = 'Sim'
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'Não'
          icon_button_2         = 'ICON_CANCEL'
          display_cancel_button = ''
        IMPORTING
          answer                = v_ans.

      CHECK v_ans = 1 .
      CREATE OBJECT r_seguro_geracao.
      r_seguro_geracao->z_estorno_ctb_apolice( ).
      PERFORM f_action_enter.

    WHEN c_anexo_apolice.

      PERFORM f_anexo_apolice.

    WHEN c_disp_ctb_apol.

      GET CURSOR FIELD fnam VALUE fval.

      CASE fnam.
        WHEN 'WL_CABECALHO_0110-LOTE'.
          CHECK ( wl_cabecalho_0110-lote IS NOT INITIAL ).

          SET PARAMETER ID 'LOT' FIELD wl_cabecalho_0110-lote.
          CALL TRANSACTION 'ZGL017' AND SKIP FIRST SCREEN.

        WHEN 'WL_CABECALHO_0110-DOC_LCTO'.
          CHECK ( wl_cabecalho_0110-doc_lcto IS NOT INITIAL ).

          SET PARAMETER ID 'BLN' FIELD wl_cabecalho_0110-doc_lcto.
          SET PARAMETER ID 'LOT' FIELD vl_nr_lote.
          CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.

        WHEN 'WL_CABECALHO_0110-BELNR'.
          CHECK ( wl_cabecalho_0110-belnr IS NOT INITIAL ).

          SET PARAMETER ID 'BLN' FIELD wl_cabecalho_0110-belnr.
          SET PARAMETER ID 'BUK' FIELD wl_cabecalho_0110-bukrs.
          SET PARAMETER ID 'GJR' FIELD wl_cabecalho_0110-dt_lcto_ctb(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDCASE.

      "USER STORY 163032 - MMSILVA - 09.01.2025 - Inicio
    WHEN 'HIST'.

      CHECK wl_cabecalho_0110-seq_lcto IS NOT INITIAL.

      PERFORM f_submit_rsvtprot.

      "USER STORY 163032 - MMSILVA - 09.01.2025 - Fim
  ENDCASE.
ENDMODULE.                 " PAI_0110  INPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_DROPDOWN_BOX_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_dropdown_box_0110 INPUT.
  DATA: name  TYPE vrm_id,
        list  TYPE vrm_values,
        value LIKE LINE OF list.

  CHECK list IS INITIAL.

  name = 'WL_CABECALHO_0110-WAERS'.

  value-key  = 'USD'.
  value-text = 'USD'.
  APPEND value TO list.

  value-key  = 'BRL'.
  value-text = 'BRL'.
  APPEND value TO list.
  CLEAR value.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = name
      values = list.
ENDMODULE.                 " CREATE_DROPDOWN_BOX_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_SEQ_LCTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_seq_lcto INPUT.
  DATA: gt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        gt_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF gt_seq_lcto OCCURS 0,
          seq_lcto       TYPE zglt050-seq_lcto,
          tp_lcto        TYPE zglt050-tp_lcto,
          dep_resp       TYPE zglt050-dep_resp,
          bukrs          TYPE zglt050-bukrs,
          nro_apolice    TYPE zglt050-nro_apolice,
          cod_seguradora TYPE zglt050-cod_seguradora,
          tp_opr         TYPE zglt050-tp_opr,
          seq_endosso    TYPE zglt050-seq_endosso, "RJF
          dt_criacao     TYPE zglt050-dt_criacao,
          usnam          TYPE zglt050-usnam,
        END OF gt_seq_lcto.

  REFRESH gt_seq_lcto.
  CLEAR gt_seq_lcto.

  SELECT seq_lcto tp_lcto dep_resp bukrs nro_apolice
         cod_seguradora tp_opr seq_endosso dt_criacao usnam
    FROM zglt050 INTO CORRESPONDING FIELDS OF TABLE gt_seq_lcto
   WHERE loekz = ''.

  SORT gt_seq_lcto BY seq_lcto DESCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'SEQ_LCTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WL_CABECALHO_0110-SEQ_LCTO'
      value_org       = 'S'
    TABLES
      value_tab       = gt_seq_lcto
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.
ENDMODULE.                 " HELP_SEQ_LCTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  HELP_NRO_APOLICE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_nro_apolice INPUT.
  CLEAR: gt_return_tab,
         gt_dselc.

  DATA: BEGIN OF gt_nro_apolice OCCURS 0,
          nro_apolice    TYPE zglt050-nro_apolice,
          dep_resp       TYPE zglt050-dep_resp,
          bukrs          TYPE zglt050-bukrs,
          cod_seguradora TYPE zglt050-cod_seguradora,
        END OF gt_nro_apolice.

  REFRESH gt_nro_apolice.
  CLEAR  gt_nro_apolice.

  SELECT dep_resp bukrs nro_apolice cod_seguradora
    FROM zglt050 INTO CORRESPONDING FIELDS OF TABLE gt_nro_apolice
   WHERE loekz  EQ ''.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NRO_APOLICE'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'WL_CABECALHO_0110-NRO_APOLICE'
      value_org       = 'S'
    TABLES
      value_tab       = gt_nro_apolice
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.
ENDMODULE.                 " HELP_NRO_APOLICE  INPUT

*----------------------------------------------------------------------*
*  MODULE PAI_0150 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pai_0150 INPUT.
  DATA r_geracao TYPE REF TO z_seguro_geracao.

  DATA:
    tf_seq_150 TYPE rstabfield,
    wl_r_seq1  LIKE LINE OF r_seq_150[].

  IF NOT gv_loopscr IS INITIAL.
    CLEAR gv_loopscr.
    EXIT.

  ENDIF.

  tf_seq_150-tablename = 'ZGLT050'.
  tf_seq_150-fieldname = 'SEQ_LCTO'.

  CREATE OBJECT: go_utils,
                 r_geracao.

  CASE sy-ucomm.
    WHEN 'SEQ_LCTO_ADD'.
      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
        EXPORTING
          title             = 'Tipos'
          tab_and_field     = tf_seq_150
        TABLES
          range             = r_seq_150
        EXCEPTIONS
          no_range_tab      = 1
          cancelled         = 2
          internal_error    = 3
          invalid_fieldname = 4
          OTHERS            = 5.

      IF wl_cabecalho_0150-seq_lcto IS INITIAL.

*        IF v_ctr IS NOT INITIAL.
        IF r_seq_150[] IS NOT INITIAL.
*            DELETE r_seq_150[] INDEX 1.

          READ TABLE r_seq_150[] INTO wa_seq_150 INDEX 1.
          IF sy-subrc IS INITIAL.
            wl_cabecalho_0150-seq_lcto = wa_seq_150-low.
          ELSE.
            CLEAR wl_cabecalho_0150-seq_lcto.
          ENDIF.
        ENDIF.
*        ENDIF.
*        v_ctr = abap_true.
        READ TABLE r_seq_150 INTO wl_r_seq1 INDEX 1.
        IF sy-subrc IS INITIAL.
          wl_cabecalho_0150-seq_lcto = wl_r_seq1-low.
        ELSE.
          CLEAR wl_cabecalho_0150-seq_lcto.
        ENDIF.
      ELSE.

        IF r_seq_150[] IS NOT INITIAL.
          READ TABLE r_seq_150 INTO wl_r_seq1 INDEX 1.
          IF sy-subrc IS INITIAL.
            wl_cabecalho_0150-seq_lcto = wl_r_seq1-low.
          ELSE.
            CLEAR wl_cabecalho_0150-seq_lcto.
          ENDIF.
        ELSE.
          CLEAR wl_cabecalho_0150-seq_lcto.
        ENDIF.

*        READ TABLE r_seq_150 INTO DATA(wa_seq) WITH KEY low = wl_cabecalho_0150-seq_lcto.
*        IF sy-subrc IS NOT INITIAL.
*          wa_seq_150-sign   = 'I'.
*          wa_seq_150-option = 'EQ'.
*          wa_seq_150-low    = wl_cabecalho_0150-seq_lcto.
*          APPEND wa_seq_150 TO r_seq_150.
*          CLEAR wa_seq_150.
*        ENDIF.
      ENDIF.

    WHEN c_enter.
* RJF - Ini

      IF wl_cabecalho_0150-seq_lcto IS INITIAL.

*        IF v_ctr IS NOT INITIAL.
        IF r_seq_150[] IS NOT INITIAL.
          DELETE r_seq_150[] INDEX 1.

          READ TABLE r_seq_150[] INTO wa_seq_150 INDEX 1.
          IF sy-subrc IS INITIAL.
            wl_cabecalho_0150-seq_lcto = wa_seq_150-low.
          ELSE.
            CLEAR wl_cabecalho_0150-seq_lcto.
          ENDIF.
        ENDIF.
*        ENDIF.
*        v_ctr = abap_true.
        READ TABLE r_seq_150 INTO wl_r_seq1 INDEX 1.
        IF sy-subrc IS INITIAL.
          wl_cabecalho_0150-seq_lcto = wl_r_seq1-low.
        ELSE.
          CLEAR wl_cabecalho_0150-seq_lcto.
        ENDIF.
      ELSE.

        READ TABLE r_seq_150 INTO DATA(wa_seq) WITH KEY low = wl_cabecalho_0150-seq_lcto.
        IF sy-subrc IS NOT INITIAL.
          wa_seq_150-sign   = 'I'.
          wa_seq_150-option = 'EQ'.
          wa_seq_150-low    = wl_cabecalho_0150-seq_lcto.
          APPEND wa_seq_150 TO r_seq_150.
          CLEAR wa_seq_150.
        ENDIF.
      ENDIF.
* RJF - Fim

      go_utils->z_validar_cabecalho_0150( ).

      CHECK ( return_status IS INITIAL ).
      go_utils->z_seleciona_dados_0150( ).
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " PAI_0150  INPUT

MODULE pai_0170 INPUT.
  CREATE OBJECT: go_utils,
                 r_geracao.

  CASE sy-ucomm.
    WHEN c_enter.
      go_utils->z_validar_cabecalho_0170( ).

      CHECK ( return_status IS INITIAL ).
      go_utils->z_seleciona_dados_0170( ).
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " PAI_0150  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0131  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0131 INPUT.
  CASE sy-ucomm.
    WHEN c_enter.
      PERFORM f_import_bens_asseg.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE carrega_arquivo INPUT.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_file
      mask             = ',*.xlsx.'
      mode             = 'O'
      title            = 'Arquivo a importar'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

ENDMODULE.                 " CARREGA_ARQUIVO  INPUT

MODULE define_tp_opr INPUT.

  DATA: values  TYPE vrm_values WITH HEADER LINE.

  REFRESH: values.
  CLEAR: values.

  values-text = 'Novo'.
  values-key  = 'N'.
  APPEND values.

  values-text = 'Endosso'.
  values-key  = 'E'.
  APPEND values.

  values-text = 'Baixa'.
  values-key  = 'B'.
  APPEND values.

  values-text = 'Prorrogação'.
  values-key  = 'P'.
  APPEND values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'WL_CABECALHO_0110-TP_OPR'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0132  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0132 INPUT.

  DATA: z_seg_geracao TYPE REF TO z_seguro_geracao.

  CREATE OBJECT: z_seg_geracao.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      z_seg_geracao->z_inc_bens_baixa( ).
      screen_item      = c_screen_0130.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      screen_item      = c_screen_0130.
      LEAVE TO SCREEN 0.
    WHEN 'SEL_ALL'.
      FREE: gt_selected_rows.
      CALL METHOD obj_alv_0132->set_selected_rows
        EXPORTING
          it_index_rows = gt_selected_rows.

      LOOP AT gt_selected_rows ASSIGNING FIELD-SYMBOL(<>).
        READ TABLE gt_saida_0132 ASSIGNING FIELD-SYMBOL(<wa_saida_0132>) INDEX wl_selected_rows-index.
        <wa_saida_0132>-check = abap_true.
      ENDLOOP.

      CALL METHOD obj_alv_0132->refresh_table_display
        EXPORTING
          is_stable = wl_stable.

    WHEN 'DESSEL_ALL'.
      FREE: gt_selected_rows.
      CALL METHOD obj_alv_0132->set_selected_rows
        EXPORTING
          it_index_rows = gt_selected_rows.

      READ TABLE gt_saida_0132 ASSIGNING <wa_saida_0132> INDEX wl_selected_rows-index.
      <wa_saida_0132>-check  = abap_false.

      CALL METHOD obj_alv_0132->refresh_table_display
        EXPORTING
          is_stable = wl_stable.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0133  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0133 INPUT.

  CREATE OBJECT: z_seg_geracao.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      z_seg_geracao->z_modify_centro( ).
    WHEN 'CANCEL'.
      screen_item      = c_screen_0130.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HELP_0133_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_0133_werks INPUT.

  DATA: gt_j_1bbranch TYPE TABLE OF j_1bbranch.

  CLEAR: gt_j_1bbranch[], gt_return_tab[], gt_dselc[].

  SELECT DISTINCT branch name
    FROM j_1bbranch
    INTO CORRESPONDING FIELDS OF TABLE gt_j_1bbranch
   WHERE bukrs EQ wl_cabecalho_0110-bukrs.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BRANCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WL_CHANGE_WERKS-WERKS'
      value_org       = 'S'
    TABLES
      value_tab       = gt_j_1bbranch
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  READ TABLE gt_return_tab WITH KEY retfield = 'WL_CHANGE_WERKS-WERKS'.
  IF sy-subrc EQ 0.
    wl_change_werks-werks = gt_return_tab-fieldval.
    CLEAR: wl_change_werks-kostl.
  ENDIF.

  LEAVE TO SCREEN sy-dynnr.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  HELP_0133_KOSTL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_0133_kostl INPUT.

  TYPES: BEGIN OF ty_csks,
           kokrs TYPE csks-kokrs,
           kostl TYPE csks-kostl,
           datbi TYPE csks-datbi,
           datab TYPE csks-datab,
           ltext TYPE cskt-ltext,
         END OF ty_csks.
  DATA: gt_csks TYPE TABLE OF ty_csks.

  CLEAR: gt_j_1bbranch[], gt_return_tab ,gt_return_tab[], gt_dselc[].

  IF wl_change_werks-werks IS INITIAL.
    MESSAGE s836(sd) WITH TEXT-e42 DISPLAY LIKE 'S'.
    RETURN.
  ENDIF.

  SELECT a~kokrs a~kostl a~datbi a~datab b~ltext
     INTO CORRESPONDING FIELDS OF TABLE gt_csks
    FROM csks AS a INNER JOIN cskt AS b ON a~kokrs = b~kokrs
                                       AND a~kostl = b~kostl
                                       AND a~datbi = b~datbi
   WHERE a~bukrs  = wl_cabecalho_0110-bukrs
     AND a~gsber  = wl_change_werks-werks
     AND a~datab  LE sy-datum
     AND a~datbi  GE sy-datum
     AND b~spras  EQ sy-langu.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KOSTL'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WL_CHANGE_WERKS-KOSTL'
      value_org       = 'S'
    TABLES
      value_tab       = gt_csks
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  READ TABLE gt_return_tab WITH KEY retfield = 'WL_CHANGE_WERKS-KOSTL'.
  IF sy-subrc EQ 0.
    wl_change_werks-kostl = gt_return_tab-fieldval.
  ENDIF.

  LEAVE TO SCREEN sy-dynnr.


ENDMODULE.

MODULE pai_0160 INPUT.
  DATA: tf_bukrs_160 TYPE rstabfield,
        tf_tipo_160  TYPE rstabfield,
        tf_seq_160   TYPE rstabfield,
        wl_r_bukrs   LIKE LINE OF r_bukrs_160[],
        wl_r_tipo    LIKE LINE OF r_tipo_160[],
        wl_r_seq     LIKE LINE OF r_seq_160[].

  tf_bukrs_160-tablename = 'ZGLT050'.
  tf_bukrs_160-fieldname = 'BUKRS'.

  tf_tipo_160-tablename = 'ZGLT050'.
  tf_tipo_160-fieldname = 'SEQ_TIPO'.

  tf_seq_160-tablename = 'ZGLT050'.
  tf_seq_160-fieldname = 'SEQ_LCTO'.

  CREATE OBJECT: go_utils.

  CASE sy-ucomm.

    WHEN 'BUKRS_ADD'.

      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
        EXPORTING
          title             = 'Empresas'
          tab_and_field     = tf_bukrs_160
        TABLES
          range             = r_bukrs_160
        EXCEPTIONS
          no_range_tab      = 1
          cancelled         = 2
          internal_error    = 3
          invalid_fieldname = 4
          OTHERS            = 5.

      IF wl_cabecalho_0160-bukrs IS INITIAL.
        READ TABLE r_bukrs_160 INTO wl_r_bukrs INDEX 1.
        wl_cabecalho_0160-bukrs = wl_r_bukrs-low.
      ENDIF.

    WHEN 'SEQ_TIPO_ADD'.

      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
        EXPORTING
          title             = 'Tipos'
          tab_and_field     = tf_tipo_160
          search_help       = 'Z_TP_SEGURO'
        TABLES
          range             = r_tipo_160
        EXCEPTIONS
          no_range_tab      = 1
          cancelled         = 2
          internal_error    = 3
          invalid_fieldname = 4
          OTHERS            = 5.

      IF wl_cabecalho_0160-seq_tipo IS INITIAL.
        READ TABLE r_tipo_160 INTO wl_r_tipo INDEX 1.
        wl_cabecalho_0160-seq_tipo = wl_r_tipo-low.
      ENDIF.

    WHEN 'SEQ_LCTO_ADD'.

      CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
        EXPORTING
          title             = 'Tipos'
          tab_and_field     = tf_seq_160
        TABLES
          range             = r_seq_160
        EXCEPTIONS
          no_range_tab      = 1
          cancelled         = 2
          internal_error    = 3
          invalid_fieldname = 4
          OTHERS            = 5.

      IF wl_cabecalho_0160-seq_lcto IS INITIAL.
        READ TABLE r_seq_160 INTO wl_r_seq INDEX 1.
        wl_cabecalho_0160-seq_lcto = wl_r_seq-low.
      ENDIF.

    WHEN c_enter.
      go_utils->z_validar_cabecalho_0160( ).

      CHECK ( return_status IS INITIAL ).
      go_utils->z_seleciona_dados_0160( ).


  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  COMPARAR_DATAS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE comparar_datas INPUT.

  IF wl_cabecalho_0110-vig_de > wl_cabecalho_0110-vig_ate.
    MESSAGE s000(zles) WITH 'Limite da vigência contrato inferior,' ' maior que o limite superior'
    DISPLAY LIKE 'E'.
  ENDIF.

ENDMODULE.
**<<<------"164255 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*&      Module  ZM_FILL_TIPO  INPUT
*&---------------------------------------------------------------------*
*       Valida se campo alterado para ajuste de tela campo competência
*----------------------------------------------------------------------*
MODULE zm_fill_tipo INPUT.

  CHECK wl_cabecalho_0150-seq_tipo EQ 17 OR
        wl_cabecalho_0150-seq_tipo EQ 29 OR
        wl_cabecalho_0150-seq_tipo EQ 21 OR
        wl_cabecalho_0150-seq_tipo EQ 22.

  gv_loopscr = abap_on.

ENDMODULE.
**<<<------"164255 - NMS - FIM------>>>
