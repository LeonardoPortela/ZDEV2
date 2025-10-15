*----------------------------------------------------------------------*
***INCLUDE LZDUEI01.
*----------------------------------------------------------------------*
MODULE user_command_0010 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      IF cab_due-id_due_ref IS INITIAL.
        MESSAGE 'Id. DU-e Retificar não foi Informada!' TYPE 'S'.
        RETURN.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_0100 INPUT.

  CASE ok_code_0100.
    WHEN due_tb01.
      CLEAR: ok_code_0100.
      due_dynnr_000 = due_0110.
    WHEN due_tb02.
      CLEAR: ok_code_0100.
      IF ( cab_due-bukrs IS INITIAL ) AND ( cab_due-performance EQ abap_false ).
        CLEAR: sy-ucomm.
        MESSAGE 'Empresa não foi informada!' TYPE 'W'.
        RETURN.
      ELSEIF ( cab_due-kunnr IS INITIAL ) AND ( cab_due-performance EQ abap_true ).
        CLEAR: sy-ucomm.
        MESSAGE 'Cliente não foi informado!' TYPE 'W'.
        RETURN.
      ELSE.
        due_dynnr_000 = due_0120.
      ENDIF.
    WHEN c_lib_leitura_opus.
      CLEAR: ok_code_0100.
      PERFORM f_lib_leitura_due_comex.
    WHEN c_sol_modific_opus.
      CLEAR: ok_code_0100.
      PERFORM f_sol_modif_due_comex.
    WHEN c_save.
      CLEAR: ok_code_0100.
      PERFORM f_gravar_due.
    WHEN c_event_interface.
      CLEAR: ok_code_0100.
      PERFORM f_eventos_interface_due.
    WHEN c_retransmitir.
      CLEAR: ok_code_0100.
      PERFORM f_retransmitir.
    WHEN c_fill_due.
      CLEAR: ok_code_0100.
      PERFORM f_fill_due.
    WHEN c_down_xml.
      CLEAR: ok_code_0100.
      PERFORM f_download_xml_due.
    WHEN c_inf_dt_reg_portal.
      CLEAR: ok_code_0100.
      PERFORM f_inf_dt_reg_portal.
    WHEN c_cancel.
      CLEAR: ok_code_0100.
      LEAVE TO SCREEN 0.
    WHEN c_ciencia_alerta.
      CASE cab_due-ciencia_alertas_rfb.
        WHEN abap_true.
          CLEAR: cab_due-dt_ciencia_alerta, cab_due-hr_ciencia_alerta, cab_due-us_ciencia_alerta.
        WHEN abap_false.
          cab_due-dt_ciencia_alerta = sy-datum.
          cab_due-hr_ciencia_alerta = sy-uzeit.
          cab_due-us_ciencia_alerta = sy-uname.
      ENDCASE.
  ENDCASE.

ENDMODULE.

MODULE tab_active_due INPUT.

  CASE sy-ucomm.
    WHEN due_tb01.
      info_due_tab-activetab = due_tb01.
    WHEN due_tb02.
      info_due_tab-activetab = due_tb02.
  ENDCASE.

ENDMODULE.


MODULE help_codigo_ra_despacho INPUT.

  DATA: gt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        gt_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tg_0168_despacho OCCURS 0,
          codigo_ra TYPE zsdt0168-codigo_ra,
          ds_ra     TYPE zsdt0168-ds_ra,
        END OF tg_0168_despacho.

  CLEAR: tg_0168_despacho[], gt_return_tab[], gt_dselc[], tg_0168_despacho.

  SELECT *
    FROM zsdt0168 INTO CORRESPONDING FIELDS OF TABLE tg_0168_despacho
   WHERE local_despacho EQ abap_true.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CODIGO_RA'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'CAB_DUE-CODIGO_RA_DESPACHO'
      value_org       = 'S'
    TABLES
      value_tab       = tg_0168_despacho
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  IF gt_return_tab[] IS INITIAL.
    MESSAGE 'Nenhum Recinto de Despacho encontrado!' TYPE 'S'.
  ENDIF.

ENDMODULE.

MODULE help_codigo_ra_embarque INPUT.

  DATA: BEGIN OF tg_0168_embarque OCCURS 0,
          codigo_ra TYPE zsdt0168-codigo_ra,
          ds_ra     TYPE zsdt0168-ds_ra,
        END OF tg_0168_embarque.

  CLEAR: tg_0168_embarque[], gt_return_tab[], gt_dselc[], tg_0168_embarque.

  SELECT *
    FROM zsdt0168 INTO CORRESPONDING FIELDS OF TABLE tg_0168_embarque
   WHERE local_embarque EQ abap_true.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CODIGO_RA'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'CAB_DUE-CODIGO_RA_EMBARQUE'
      value_org       = 'S'
    TABLES
      value_tab       = tg_0168_embarque
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  IF gt_return_tab[] IS INITIAL.
    MESSAGE 'Nenhum Recinto de Embarque encontrado!' TYPE 'S'.
  ENDIF.

ENDMODULE.

MODULE help_codigo_urf_despacho INPUT.

  DATA: BEGIN OF tg_0167_despacho OCCURS 0,
          codigo_urf TYPE zsdt0167-codigo_urf,
          ds_urf     TYPE zsdt0167-ds_urf,
        END OF tg_0167_despacho.

  CLEAR: tg_0167_despacho[], gt_return_tab[], gt_dselc[], tg_0167_despacho.

  SELECT *
    FROM zsdt0167 INTO CORRESPONDING FIELDS OF TABLE tg_0167_despacho.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CODIGO_URF'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'CAB_DUE-CODIGO_URF_DESPACHO'
      value_org       = 'S'
    TABLES
      value_tab       = tg_0167_despacho
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  IF gt_return_tab[] IS INITIAL.
    MESSAGE 'Nenhum Recinto de Despacho encontrado!' TYPE 'S'.
  ENDIF.

ENDMODULE.

MODULE user_command_0121 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF ( due_control-modo EQ c_due_view ).
        LEAVE TO SCREEN 0.
      ENDIF.

      PERFORM f_completa_campos_0121.

      IF vg_operacao_0120 EQ c_novo.
        CLEAR: wa_saida_0120.
        MOVE-CORRESPONDING zsdt0172 TO wa_saida_0120.
        APPEND wa_saida_0120 TO it_saida_0120.
      ELSE.
        READ TABLE it_saida_0120 ASSIGNING FIELD-SYMBOL(<fs_saida_0120>) INDEX wa_sel_rows-index.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING zsdt0172 TO <fs_saida_0120>.
        ENDIF.
      ENDIF.

      IF sy-subrc = 0.
        "MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  GET CURSOR FIELD vg_field_sel_0121 OFFSET vg_field_pos_0121.

ENDMODULE.

MODULE user_command_0122 INPUT.

  DATA: v_count_destino   TYPE i,
        v_destino_country TYPE zsdt0174-destino_country.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF ( due_control-modo EQ c_due_view ).
        LEAVE TO SCREEN 0.
      ENDIF.

      CALL METHOD obj_alv_0122->check_changed_data.

      CLEAR: v_count_destino, v_destino_country.

      LOOP AT it_saida_0122_itm ASSIGNING FIELD-SYMBOL(<fs_saida_0122_itm>) WHERE ck_modify EQ abap_true.
        IF <fs_saida_0122_itm>-id_due IS INITIAL.
          <fs_saida_0122_itm>-id_due      = wa_saida_0120-id_due.
          <fs_saida_0122_itm>-id_due_item = wa_saida_0120-id_due_item.
        ENDIF.

        IF <fs_saida_0122_itm>-ue_exportada IS INITIAL.
          MESSAGE 'U.E Exportada é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        IF <fs_saida_0122_itm>-destino_country IS INITIAL.
          MESSAGE 'País é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        IF <fs_saida_0122_itm>-peso_liq_total IS INITIAL.
          MESSAGE 'Peso Liq.Total é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        IF <fs_saida_0122_itm>-destino_qtde_ue_exportada IS INITIAL.
          MESSAGE 'Quantidade Exportada é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.

        ADD 1 TO v_count_destino.
        v_destino_country = <fs_saida_0122_itm>-destino_country.

      ENDLOOP.

      "Se primeiro Item
      IF ( wa_saida_0120-id_due_item EQ '0001' ) AND ( v_count_destino EQ 1 ) AND ( lines( it_saida_0120[] ) > 1  ).

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = |Deseja aplicar a quantidade total exportada p/ o País { v_destino_country } nos demais itens da DU-e ?|
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF var_answer EQ '1'.

          LOOP AT it_saida_0120 INTO DATA(_wl_sai_0120) WHERE id_due_item NE '0001'.

            READ TABLE it_saida_0122 INTO DATA(_wl_sai_0122) WITH KEY id_due_item = _wl_sai_0120-id_due_item.
            CHECK sy-subrc NE 0.

            CLEAR: wa_saida_0122.

            wa_saida_0122-id_due                     = _wl_sai_0120-id_due.
            wa_saida_0122-id_due_item                = _wl_sai_0120-id_due_item.
            wa_saida_0122-ue_exportada               = _wl_sai_0120-ue_exportada.
            wa_saida_0122-destino_country            = v_destino_country.
            wa_saida_0122-peso_liq_total             = _wl_sai_0120-peso_liq_total.
            wa_saida_0122-destino_qtde_ue_exportada  = _wl_sai_0120-qtde_ue_exportada.

            CHECK ( wa_saida_0122-id_due_item               IS NOT INITIAL ) AND
                  ( wa_saida_0122-ue_exportada              IS NOT INITIAL ) AND
                  ( wa_saida_0122-destino_country           IS NOT INITIAL ) AND
                  ( wa_saida_0122-peso_liq_total            IS NOT INITIAL ) AND
                  ( wa_saida_0122-destino_qtde_ue_exportada IS NOT INITIAL ).

            APPEND wa_saida_0122 TO it_saida_0122.

          ENDLOOP.

        ENDIF.

      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.



ENDMODULE.


MODULE user_command_0125 INPUT.

  DATA: v_count_lpco TYPE i,
        v_nr_lpco    TYPE zsdt0190-nr_lpco.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF ( due_control-modo EQ c_due_view ).
        LEAVE TO SCREEN 0.
      ENDIF.

      CALL METHOD obj_alv_0125->check_changed_data.

      CLEAR: v_count_lpco, v_nr_lpco.

      LOOP AT it_saida_0125_itm ASSIGNING FIELD-SYMBOL(<fs_saida_0125_itm>) WHERE ck_modify EQ abap_true.

        IF <fs_saida_0125_itm>-id_due IS INITIAL.
          <fs_saida_0125_itm>-id_due      = wa_saida_0120-id_due.
          <fs_saida_0125_itm>-id_due_item = wa_saida_0120-id_due_item.
        ENDIF.

        IF <fs_saida_0125_itm>-nr_lpco IS INITIAL.
          MESSAGE 'Nr.LPCO é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.
*** CS2019001113 - Alteração em DUE (Enquadramento e LPCO) Inicio
        ADD 1 TO v_count_lpco.
        v_nr_lpco = <fs_saida_0125_itm>-nr_lpco.
*** CS2019001113 - Alteração em DUE (Enquadramento e LPCO) Fim
      ENDLOOP.

*** CS2019001113 - Alteração em DUE (Enquadramento e LPCO) Inicio
      IF ( wa_saida_0120-id_due_item EQ '0001' ) AND ( v_count_lpco EQ 1 ) AND ( lines( it_saida_0120[] ) > 1  ).

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = |Deseja aplicar o Nro LPCO { v_nr_lpco } nos demais itens da DU-e ?|
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF var_answer EQ '1'.

          LOOP AT it_saida_0120 INTO DATA(_wl_sai_0120_aux) WHERE id_due_item NE '0001'.

            READ TABLE it_saida_0125 INTO DATA(_wl_sai_0125) WITH KEY id_due_item = _wl_sai_0120_aux-id_due_item.
            CHECK sy-subrc NE 0.

            CLEAR: wa_saida_0125.

            wa_saida_0125-id_due       = _wl_sai_0120_aux-id_due.
            wa_saida_0125-id_due_item  = _wl_sai_0120_aux-id_due_item.
            wa_saida_0125-nr_lpco      = v_nr_lpco.

            CHECK (  wa_saida_0125-id_due_item         IS NOT INITIAL ) AND
                  (  wa_saida_0125-nr_lpco             IS NOT INITIAL ).

            APPEND wa_saida_0125 TO it_saida_0125.

          ENDLOOP.

        ENDIF.

      ENDIF.
*** CS2019001113 - Alteração em DUE (Enquadramento e LPCO) Fim
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_0123 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF ( due_control-modo EQ c_due_view ).
        LEAVE TO SCREEN 0.
      ENDIF.

      CALL METHOD obj_alv_0123->check_changed_data.

      LOOP AT it_saida_0123_itm ASSIGNING FIELD-SYMBOL(<fs_saida_0123_itm>) WHERE ck_modify EQ abap_true.
        IF <fs_saida_0123_itm>-id_due IS INITIAL.
          <fs_saida_0123_itm>-id_due      = wa_saida_0120-id_due.
          <fs_saida_0123_itm>-id_due_item = wa_saida_0120-id_due_item.
        ENDIF.

        IF <fs_saida_0123_itm>-ue_exportada IS INITIAL.
          MESSAGE 'U.E Exportada é um campo obrigatório!' TYPE 'S'.
          RETURN.
        ENDIF.
      ENDLOOP.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_0110 INPUT.

  GET CURSOR FIELD vg_field_sel_0110 OFFSET vg_field_pos_0110.

ENDMODULE.


MODULE busca_ncm_material INPUT.

  DATA: v_matnr  TYPE mara-matnr.
  DATA: v_matnr18 TYPE matnr18.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  TYPES: BEGIN OF ty_marc,
           steuc TYPE marc-steuc,
         END OF ty_marc.

  DATA: lt_marc TYPE TABLE OF ty_marc WITH HEADER LINE.

  CLEAR: lt_marc[].

  CHECK zsdt0172-matnr IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = zsdt0172-matnr
    IMPORTING
      output = v_matnr18.

  v_matnr = v_matnr18.

  SELECT *
    FROM marc AS a INTO CORRESPONDING FIELDS OF TABLE lt_marc
   WHERE a~matnr EQ v_matnr
     AND a~steuc NE space
     AND EXISTS ( SELECT b~centrov_1
                    FROM zsdt_depara_cen AS b
                   WHERE b~centrov_1 EQ a~werks ).

  SORT lt_marc BY steuc.
  DELETE ADJACENT DUPLICATES FROM lt_marc COMPARING steuc.

  CHECK lt_marc[] IS NOT INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'STEUC'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZSDT0172-CODIGO_NCM'
      value_org       = 'S'
    TABLES
      value_tab       = lt_marc
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.                 " BUSCA_RE  INPUT

MODULE user_command_0124 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF vg_docum_exp_venda IS INITIAL.
        MESSAGE 'Documento NF Venda não foi informado!' TYPE 'S'.
        RETURN.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE user_command_0111 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF cab_due-id_due IS INITIAL.
        MESSAGE 'DU-e não foi gravada!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF vg_dt_reg_portal IS INITIAL.
        MESSAGE 'Informar Data de Registro no Portal!' TYPE 'S'.
        EXIT.
      ENDIF.

      UPDATE zsdt0170 SET dt_registro_portal = vg_dt_reg_portal
       WHERE id_due = cab_due-id_due.

      MESSAGE 'Data de Registro no Portal alterada com sucesso!' TYPE 'S'.

      cab_due-dt_registro_portal = vg_dt_reg_portal.

      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_1000 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE pai_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.
      PERFORM f_confirm_screen_0200.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE help_transporte INPUT.

  TYPES: BEGIN OF ty_transpor_sel,
           id_transporte    TYPE znom_cad_transpt-id_transporte,
           ds_nome_transpor TYPE znom_cad_transpt-ds_nome_transpor,
         END OF ty_transpor_sel.


  DATA: lit_transpor     TYPE TABLE OF ty_transpor_sel,
        lit_transpor_aux TYPE TABLE OF znom_cad_transpt,
        lit_return_tab   TYPE TABLE OF ddshretval,
        lit_dselc        TYPE TABLE OF dselc.

  CLEAR: gwa_nomeacao_sol_ov-ds_nome_transpor, lit_transpor[], lit_transpor_aux[], lit_return_tab[].

  SELECT *
    FROM znom_cad_transpt INTO TABLE lit_transpor_aux.

  DELETE lit_transpor_aux WHERE status EQ 'E'. "Eliminado

  lit_transpor[] = CORRESPONDING #( lit_transpor_aux[] ).

  CHECK lit_transpor[] IS NOT INITIAL.

  SORT lit_transpor BY id_transporte DESCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ID_TRANSPORTE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'GWA_NOMEACAO_SOL_OV-DS_NOME_TRANSPOR'
      value_org       = 'S'
    TABLES
      value_tab       = lit_transpor
      return_tab      = lit_return_tab
      dynpfld_mapping = lit_dselc.

  READ TABLE lit_return_tab INTO DATA(lwa_return_tab) WITH KEY retfield = 'GWA_NOMEACAO_SOL_OV-DS_NOME_TRANSPOR'.
  IF sy-subrc = 0.
    READ TABLE lit_transpor INTO DATA(lwa_transpor) WITH KEY id_transporte = lwa_return_tab-fieldval.
    IF sy-subrc = 0.
      gwa_nomeacao_sol_ov-id_transporte    = lwa_transpor-id_transporte.
      gwa_nomeacao_sol_ov-ds_nome_transpor = lwa_transpor-ds_nome_transpor.
    ENDIF.
  ENDIF.

  LEAVE TO SCREEN 0200.

ENDMODULE.

MODULE pai_0201 INPUT.

  DATA: lwa_dados_geracao_ruc TYPE zde_dados_geracao_ruc.

  CASE sy-ucomm.
    WHEN 'ACTION_RB_0201'.
      LEAVE TO SCREEN 0201.

    WHEN 'GERAR_RUC'.

      TRY.
          CLEAR: lwa_dados_geracao_ruc.
          MOVE-CORRESPONDING gwa_dados_geracao_ruc TO lwa_dados_geracao_ruc.

          gva_numero_ruc_gerado_sel = zcl_due=>gerar_nr_ruc( i_dados_geracao_ruc = lwa_dados_geracao_ruc ).
          IF gva_numero_ruc_gerado_sel IS NOT INITIAL.
            MESSAGE |RUC { gva_numero_ruc_gerado_sel } gerada com sucesso! | TYPE 'I'.
            LEAVE TO SCREEN 0.
          ENDIF.
        CATCH zcx_due INTO DATA(zcx_due).
          CLEAR: gva_numero_ruc_gerado_sel.
          zcx_due->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

    WHEN 'ENTER'.
      PERFORM f_selecionar_dados_0201.
    WHEN 'SELECT'.

      CLEAR: it_sel_rows[], wa_sel_rows.

      CALL METHOD obj_alv_0201->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS INITIAL.
        MESSAGE 'Selecione uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF lines( it_sel_rows ) NE 1.
        MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

      CHECK sy-subrc EQ 0.

      READ TABLE it_saida_0201 INTO wa_saida_0201 INDEX wa_sel_rows-index.

      CHECK ( sy-subrc = 0 ) AND ( wa_saida_0201-numero_ruc IS NOT INITIAL ).

      SELECT SINGLE *
        FROM zsdt0170 INTO @DATA(lwa_zsdt0170_ret)
       WHERE numero_ruc EQ @wa_saida_0201-numero_ruc
         AND tp_due     EQ '2'
         AND loekz      EQ @abap_false.

      IF sy-subrc EQ 0.
        MESSAGE |DU-e já lançada para RUC selecionada! Id: DU-e { lwa_zsdt0170_ret-id_due } | TYPE 'S'.
        EXIT.
      ENDIF.

      gva_numero_ruc_gerado_sel = wa_saida_0201-numero_ruc.


      LEAVE TO SCREEN 0.

    WHEN 'CONFIRM' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0126  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0126 INPUT.

  CASE sy-ucomm.
    WHEN 'GRAVAR'.

    "  PERFORM f_save_zsdt0305.

    WHEN 'OK'.

      PERFORM F_SAVE_DRAWBACK.
     LEAVE  TO SCREEN 0.

    WHEN 'BACK' OR 'LEAVE' OR 'EXIT' OR 'CANCEL' .
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
