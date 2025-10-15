*----------------------------------------------------------------------*
***INCLUDE LZGF_DESTINACAO_MERCADORIAF01.
*----------------------------------------------------------------------*

CLASS lcl_event_handler_0100a DEFINITION DEFERRED.
CLASS lcl_event_handler_0100c DEFINITION DEFERRED.

DATA: ctl_ctn_0100a TYPE REF TO cl_gui_custom_container,
      ctl_ctn_0100b TYPE REF TO cl_gui_custom_container,
      ctl_ctn_0100c TYPE REF TO cl_gui_custom_container.

DATA: ctl_alv_0100a TYPE REF TO cl_gui_alv_grid,
      ctl_alv_0100b TYPE REF TO cl_gui_alv_grid,
      ctl_alv_0100c TYPE REF TO cl_gui_alv_grid.

DATA: it_fiel_0100a TYPE lvc_t_fcat,
      it_fiel_0100b TYPE lvc_t_fcat,
      it_fiel_0100c TYPE lvc_t_fcat.

DATA: gs_variant_0100a TYPE disvariant,
      gs_variant_0100b TYPE disvariant,
      gs_variant_0100c TYPE disvariant.

DATA: gs_layout_0100a TYPE lvc_s_layo,
      gs_layout_0100b TYPE lvc_s_layo,
      gs_layout_0100c TYPE lvc_s_layo.

DATA: wa_stable_0100a TYPE lvc_s_stbl,
      wa_stable_0100b TYPE lvc_s_stbl,
      wa_stable_0100c TYPE lvc_s_stbl.

DATA: event_handler_0100a TYPE REF TO lcl_event_handler_0100a.
DATA: event_handler_0100c TYPE REF TO lcl_event_handler_0100c.

CLASS lcl_event_handler_0100a DEFINITION.
  PUBLIC SECTION.
    DATA: validar_data  TYPE c,
          error_in_data TYPE c,
          ls_good       TYPE lvc_s_modi,
          lv_value      TYPE lvc_value.

    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

  PRIVATE SECTION.
    TYPES: ddshretval_table TYPE TABLE OF ddshretval.
    METHODS: perform_semantic_checks IMPORTING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS lcl_event_handler_0100c DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION


CLASS lcl_event_handler_0100a IMPLEMENTATION.

  METHOD handle_hotspot_click.

  ENDMETHOD.                    "handle_hotspot_click

  METHOD data_changed_finished.

    IF e_modified IS NOT INITIAL.
      ctl_alv_0100a->refresh_table_display( EXPORTING is_stable =  VALUE #( row = abap_true col = abap_true ) ).
    ENDIF.

  ENDMETHOD.

  METHOD perform_semantic_checks.

    DATA: lv_value TYPE lvc_value,
          lc_menge TYPE p LENGTH 15 DECIMALS 3.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good)
      WHERE fieldname EQ 'UTILIZAR'.

      lc_menge = CONV #( ls_good-value ).

      READ TABLE it_livre ASSIGNING FIELD-SYMBOL(<fs_livre>) INDEX ls_good-row_id.

      IF <fs_livre>-saldo LT lc_menge.

        IF <fs_livre>-mblnr IS NOT INITIAL.
          CALL METHOD pr_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = 'ZDESTINACAO'
              i_msgno     = '008'
              i_msgty     = 'E'
              i_msgv1     = <fs_livre>-mblnr
              i_msgv2     = <fs_livre>-mjahr
              i_msgv3     = <fs_livre>-zeile
              i_fieldname = ls_good-fieldname
              i_row_id    = ls_good-row_id.
        ELSEIF <fs_livre>-chref IS NOT INITIAL.

          SELECT SINGLE * INTO @DATA(wa_zsdt0001)
            FROM zsdt0001
           WHERE ch_referencia EQ @<fs_livre>-chref.

          CALL METHOD pr_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = 'ZDESTINACAO'
              i_msgno     = '014'
              i_msgty     = 'E'
              i_msgv1     = wa_zsdt0001-nr_romaneio
              i_msgv2     = wa_zsdt0001-branch
              i_msgv3     = wa_zsdt0001-nr_safra
              i_fieldname = ls_good-fieldname
              i_row_id    = ls_good-row_id.

        ENDIF.

        error_in_data = abap_true.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_handler_0100c IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.

FORM carrega_vincular_rom_saida USING p_romaneio TYPE zsdt0001.

  DATA: coluna TYPE lvc_s_scol.

  SELECT * APPENDING TABLE @it_rom_vincular
    FROM zsdt0001
   WHERE ch_referencia EQ @p_romaneio-ch_referencia.

  LOOP AT it_rom_vincular INTO DATA(wa_rom_vincular).

    PERFORM carrega_lancados_rom USING wa_rom_vincular-ch_referencia.

    CLEAR: wa_livre.

    SELECT SINGLE * INTO @DATA(wa_ekko) FROM ekko WHERE ebeln EQ @wa_rom_vincular-vbeln.

    IF sy-subrc IS NOT INITIAL OR NOT ( wa_ekko-bstyp EQ 'F' AND wa_ekko-bsart EQ 'ZARM' ).
      CONTINUE.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_ekpo) FROM ekpo WHERE ebeln EQ @wa_rom_vincular-vbeln.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_eket)
      FROM eket
     WHERE ebeln EQ @wa_ekpo-ebeln
       AND ebelp EQ @wa_ekpo-ebelp.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    "Chave de Referência do Romaneio de Saída
    wa_livre-chref = wa_rom_vincular-ch_referencia.

    "Lote da Divisões do programa de remessas
    wa_livre-charg = wa_eket-charg.

    "Depósito do Item do Pedido de Compra
    wa_livre-lgort = wa_ekpo-lgort.

    "Material do Item do Pedido de Compra
    wa_livre-matnr = wa_ekpo-matnr.

    "Unidade do item do Pedido de Compra
    wa_livre-meins = wa_ekpo-meins.

    "Peso Fiscal do Romaneio
    wa_livre-menge = wa_rom_vincular-peso_fiscal.

    "Local de Negócio do item do Pedido de Compra
    wa_livre-werks = wa_ekpo-werks.

    TRY .
        zcl_deposito=>zif_deposito~get_instance(
          )->get_deposito_material_filial(
                EXPORTING
                  i_matnr = wa_livre-matnr
                  i_tp_produto = CONV #( COND string( WHEN wa_rom_vincular-tp_transgenia(1) EQ 'C' THEN zif_carga=>st_tp_transgeniase_co ELSE 'RR' ) )    " Tipo de Produto
                  i_bukrs = wa_ekko-bukrs
                  i_branch = wa_ekpo-werks
                IMPORTING
                  e_lgort          = wa_ekpo-lgort
                  e_centro_a_fixar = DATA(e_centro_a_fixar)
          ).
      CATCH zcx_deposito INTO DATA(ex_deposito).    "
        ex_deposito->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        EXIT.
    ENDTRY.

    zcl_material_destinacao=>zif_material_destinacao~set_saldo_item_rom(
      EXPORTING
        i_ch_referencia = wa_rom_vincular-ch_referencia
      IMPORTING
        e_valor         = wa_livre-valor
      RECEIVING
        r_menge         = wa_livre-saldo
    ).

    wa_livre-utilizar = wa_livre-saldo.

    IF wa_livre-menge EQ wa_rom_vincular-peso_fiscal.
      wa_livre-line_color = 'C500'.
    ELSE.
      wa_livre-line_color = 'C300'.
    ENDIF.

    CLEAR: coluna.
    coluna-fname = 'SALDO'.
    coluna-nokeycol = 'C200'.
    APPEND coluna TO wa_livre-color_cell.

    APPEND wa_livre TO it_livre.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_VINCULAR_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LC_NFE_MBLNR  text
*      -->P_LC_NFE_MJAHR  text
*----------------------------------------------------------------------*
FORM carrega_vincular_doc_material  USING p_mblnr TYPE mblnr
                                          p_mjahr TYPE mjahr.

  DATA: coluna TYPE lvc_s_scol.

  PERFORM carrega_lancados USING p_mblnr
                                 p_mjahr.

  SELECT * APPENDING TABLE @it_mseg_vincular
    FROM mseg AS m
   WHERE m~mjahr EQ @p_mjahr
     AND m~mblnr EQ @p_mblnr
     AND NOT EXISTS (
        SELECT *
          FROM mseg AS e
         WHERE e~sjahr EQ m~mjahr
           AND e~smbln EQ m~mblnr
           AND e~smblp EQ m~zeile ).

  CLEAR: it_livre[].

  LOOP AT it_mseg_vincular INTO DATA(wa_mseg).

    CLEAR: wa_livre.

    wa_livre-mblnr = wa_mseg-mblnr.
    wa_livre-mjahr = wa_mseg-mjahr.
    wa_livre-zeile = wa_mseg-zeile.

    wa_livre-charg = wa_mseg-charg.
    wa_livre-lgort = wa_mseg-lgort.
    wa_livre-matnr = wa_mseg-matnr.
    wa_livre-meins = wa_mseg-meins.
    wa_livre-werks = wa_mseg-werks.
    wa_livre-menge = wa_mseg-menge.
    wa_livre-valor = 0.
    wa_livre-ebeln = wa_mseg-ebeln.
    wa_livre-ebelp = wa_mseg-ebelp.

    CASE ob_destinacao->at_tp_destinacao.
      WHEN zif_material_destinacao=>st_tp_destinacao_armazenar OR zif_material_destinacao=>st_tp_destinacao_devolucao.

        "Ajusta Saldo
        zcl_material_destinacao=>zif_material_destinacao~set_saldo_item_mseg(
          EXPORTING
            i_mblnr = wa_livre-mblnr  " Nº documento de material
            i_mjahr = wa_livre-mjahr  " Ano do documento do material
            i_zeile = wa_livre-zeile  " Item no documento do material
          IMPORTING
            e_valor = wa_livre-valor  " Montante em moeda interna
          RECEIVING
            r_menge = wa_livre-saldo  " Quantidade
        ).

      WHEN OTHERS.

    ENDCASE.

    wa_livre-utilizar = wa_livre-saldo.

    IF wa_livre-menge EQ wa_mseg-menge.
      wa_livre-line_color = 'C500'.
    ELSE.
      wa_livre-line_color = 'C300'.
    ENDIF.

    CLEAR: coluna.
    coluna-fname = 'SALDO'.
    coluna-nokeycol = 'C200'.
    APPEND coluna TO wa_livre-color_cell.

    APPEND wa_livre TO it_livre.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_TELA_LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chamar_tela_lancamento .
  CALL SCREEN 0100 STARTING AT 30 05.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: ls_row  TYPE lvc_s_row,
        lt_rows TYPE lvc_t_row.

  CASE ok_code.
    WHEN 'VINSELE'.

      CLEAR: ok_code.
      CHECK zmmt0114-id_destinacao IS INITIAL.

      "Vincula Selecionado
      PERFORM vincular_registro.

    WHEN 'VINCALL'.

      CLEAR: ok_code.
      CHECK zmmt0114-id_destinacao IS INITIAL.

      DESCRIBE TABLE it_livre.

      DO sy-tfill TIMES.
        ls_row-index = syst-index.
        APPEND ls_row TO lt_rows.
      ENDDO.

      "Vincula Todos
      ctl_alv_0100a->set_selected_rows( EXPORTING it_index_rows = lt_rows ).
      PERFORM vincular_registro.

    WHEN 'DVINSELE'.

      CLEAR: ok_code.
      CHECK zmmt0114-id_destinacao IS INITIAL.

      "Desvincula Selecionado
      PERFORM desvincular_registro.

    WHEN 'DVINCALL'.

      CLEAR: ok_code.
      CHECK zmmt0114-id_destinacao IS INITIAL.

      DESCRIBE TABLE it_vinculado.

      DO sy-tfill TIMES.
        ls_row-index = syst-index.
        APPEND ls_row TO lt_rows.
      ENDDO.

      "Desvincula Todos
      ctl_alv_0100b->set_selected_rows( EXPORTING it_index_rows = lt_rows ).
      PERFORM desvincular_registro.

    WHEN 'ESTORNAR'.

      CLEAR: ok_code.
      CHECK zmmt0114-id_destinacao IS NOT INITIAL.

      PERFORM gerar_estorno.

    WHEN 'LANCAR'.

      CLEAR: ok_code.
      CHECK zmmt0114-id_destinacao IS INITIAL OR zmmt0114-tp_destinacao EQ zif_material_destinacao=>st_tp_destinacao_devolucao.
      PERFORM gerar_lancamento.

    WHEN 'NFE'.

      CLEAR: ok_code.
      PERFORM charmar_nota_fiscal.

    WHEN 'TEXTO_NF'.

      CLEAR: ok_code.
      PERFORM charmar_texto_nota_fiscal.

*-CS2025000249-27.05.2025-#175255-JT-inicio
    WHEN 'INCO_NF'.
      CLEAR: ok_code.
      PERFORM ajustar_incoterms_nota_fiscal.
*-CS2025000249-27.05.2025-#175255-JT-fim

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  PERFORM limpar_tela_0100.
  LEAVE TO SCREEN 0.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_tela_0100 .

  CLEAR: event_handler_0100a.

  IF ctl_alv_0100a IS NOT INITIAL.
    ctl_alv_0100a->free( ).
  ENDIF.
  CLEAR: ctl_alv_0100a.

  IF ctl_alv_0100b IS NOT INITIAL.
    ctl_alv_0100b->free( ).
  ENDIF.
  CLEAR: ctl_alv_0100b.

  IF ctl_alv_0100c IS NOT INITIAL.
    ctl_alv_0100c->free( ).
  ENDIF.
  CLEAR: ctl_alv_0100c.

  IF ctl_ctn_0100a IS NOT INITIAL.
    ctl_ctn_0100a->free( ).
  ENDIF.
  CLEAR: ctl_ctn_0100a.

  IF ctl_ctn_0100b IS NOT INITIAL.
    ctl_ctn_0100b->free( ).
  ENDIF.
  CLEAR: ctl_ctn_0100b.

  IF ctl_ctn_0100c IS NOT INITIAL.
    ctl_ctn_0100c->free( ).
  ENDIF.
  CLEAR: ctl_ctn_0100c.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: it_remov_opcoes TYPE TABLE OF sy-ucomm.

  CLEAR: it_remov_opcoes[], it_remov_opcoes.

  IF zmmt0114-mblnr IS NOT INITIAL.
    APPEND 'LANCAR' TO it_remov_opcoes.
  ELSE.
    IF ob_destinacao->at_tp_destinacao EQ zif_material_destinacao=>st_tp_destinacao_armazenar.
      APPEND 'ESTORNAR' TO it_remov_opcoes.
    ELSEIF
      "Devolução - Sem nota Fiscal
      ( ob_destinacao->at_tp_destinacao EQ zif_material_destinacao=>st_tp_destinacao_devolucao AND ob_destinacao->at_zmmt0114-docnum_dev    IS     INITIAL ) OR
      ( ob_destinacao->at_tp_destinacao EQ zif_material_destinacao=>st_tp_destinacao_devolucao AND ob_destinacao->at_zmmt0114-belnr_estorno IS NOT INITIAL ).

      APPEND 'ESTORNAR' TO it_remov_opcoes.

      IF ob_destinacao->at_zmmt0114-belnr_estorno IS NOT INITIAL.
        APPEND 'LANCAR' TO it_remov_opcoes.
      ENDIF.

    ENDIF.
  ENDIF.

  IF zmmt0114-mblnr_estorno IS NOT INITIAL.
    APPEND 'ESTORNAR' TO it_remov_opcoes.
    APPEND 'LANCAR' TO it_remov_opcoes.
  ENDIF.

  IF zmmt0114-docnum IS INITIAL AND zmmt0114-docnum_dev IS INITIAL.
    APPEND 'NFE' TO it_remov_opcoes.
    APPEND 'TEXTO_NF' TO it_remov_opcoes.
    APPEND 'INCO_NF'  TO it_remov_opcoes.  "*-CS2025000249-27.05.2025-#175255-JT
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING it_remov_opcoes.
  IF zmmt0114-id_destinacao IS NOT INITIAL.
    SET TITLEBAR 'TL0101' WITH zmmt0114-id_destinacao.
  ELSE.
    SET TITLEBAR 'TL0100'.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-input EQ 1 AND ck_read_only EQ abap_true.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  PERFORM criar_componentes.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_COMPONENTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM criar_componentes .

  DATA: it_function_0100a TYPE ui_functions.
  DATA: it_function_0100b TYPE ui_functions.

  IF ctl_ctn_0100a IS INITIAL.

    CREATE OBJECT ctl_ctn_0100a
      EXPORTING
        container_name = 'ALV_LIVRE'.

    CREATE OBJECT ctl_alv_0100a
      EXPORTING
        i_parent = ctl_ctn_0100a.

    PERFORM fill_it_fieldcatalog_0100a.

    PERFORM fill_gs_variant_0100a.

    CLEAR: it_function_0100a[].
    APPEND cl_gui_alv_grid=>mc_fc_excl_all TO it_function_0100a.

    CALL METHOD ctl_alv_0100a->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant_0100a
        i_save                        = 'A'
        is_layout                     = gs_layout_0100a
        it_toolbar_excluding          = it_function_0100a
      CHANGING
        it_outtab                     = it_livre
        it_fieldcatalog               = it_fiel_0100a
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD ctl_alv_0100a->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv_0100a->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler_0100a.
    SET HANDLER event_handler_0100a->handle_hotspot_click FOR ctl_alv_0100a.
    SET HANDLER event_handler_0100a->data_changed FOR ctl_alv_0100a.
    SET HANDLER event_handler_0100a->data_changed_finished FOR ctl_alv_0100a.

  ENDIF.

  IF ctl_ctn_0100b IS INITIAL.

    CREATE OBJECT ctl_ctn_0100b
      EXPORTING
        container_name = 'ALV_VINCU'.

    CREATE OBJECT ctl_alv_0100b
      EXPORTING
        i_parent = ctl_ctn_0100b.

    PERFORM fill_it_fieldcatalog_0100b.

    PERFORM fill_gs_variant_0100b.

    APPEND cl_gui_alv_grid=>mc_fc_excl_all TO it_function_0100b.

    CALL METHOD ctl_alv_0100b->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant_0100b
        i_save                        = 'A'
        is_layout                     = gs_layout_0100b
        it_toolbar_excluding          = it_function_0100b
      CHANGING
        it_outtab                     = it_vinculado
        it_fieldcatalog               = it_fiel_0100b
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

  IF ctl_ctn_0100c IS INITIAL.

    CREATE OBJECT ctl_ctn_0100c
      EXPORTING
        container_name = 'ALV_ORIGEM'.

    CREATE OBJECT ctl_alv_0100c
      EXPORTING
        i_parent = ctl_ctn_0100c.

    PERFORM fill_it_fieldcatalog_0100c.

    PERFORM fill_gs_variant_0100c.

    CALL METHOD ctl_alv_0100c->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant_0100c
        i_save                        = 'A'
        is_layout                     = gs_layout_0100c
      CHANGING
        it_outtab                     = it_zmmt0114_alv[]
        it_fieldcatalog               = it_fiel_0100c
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CREATE OBJECT event_handler_0100c.
    SET HANDLER event_handler_0100c->handle_hotspot_click FOR ctl_alv_0100c.

  ENDIF.

  wa_stable_0100a-row = abap_true.
  wa_stable_0100a-col = abap_true.
  CALL METHOD ctl_alv_0100a->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0100a.

  wa_stable_0100b-row = abap_true.
  wa_stable_0100b-col = abap_true.
  CALL METHOD ctl_alv_0100b->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0100b.

  wa_stable_0100c-row = abap_true.
  wa_stable_0100c-col = abap_true.
  CALL METHOD ctl_alv_0100c->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0100c.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0100A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0100a .

  CLEAR: it_fiel_0100a[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZDE_DESTINACAO_LIVRE'
    CHANGING
      ct_fieldcat            = it_fiel_0100a
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT it_fiel_0100a ASSIGNING FIELD-SYMBOL(<fs_livre>).

    IF <fs_livre>-fieldname EQ 'MENGE' OR
       <fs_livre>-fieldname EQ 'SALDO' OR
       <fs_livre>-fieldname EQ 'VALOR' OR
       <fs_livre>-fieldname EQ 'UTILIZAR'.
      <fs_livre>-outputlen = 15.
      <fs_livre>-do_sum    = abap_true.

      IF <fs_livre>-fieldname EQ 'UTILIZAR'.
        <fs_livre>-edit = abap_true.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0100C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0100c .

  CLEAR: it_fiel_0100c[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMT0114'
    CHANGING
      ct_fieldcat            = it_fiel_0100c
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT it_fiel_0100c ASSIGNING FIELD-SYMBOL(<fs_destinado>).

    IF <fs_destinado>-fieldname EQ 'LINE_COLOR' OR <fs_destinado>-fieldname EQ 'COLOR_CELL' OR <fs_destinado>-fieldname EQ 'STYLE'.
      <fs_destinado>-no_out = abap_true.
    ENDIF.

    IF <fs_destinado>-fieldname EQ 'ID_DESTINACAO'.
      <fs_destinado>-hotspot = abap_true.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0100A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0100a .

  gs_variant_0100a-report      = sy-repid.
  gs_variant_0100a-handle      = 'A100'.
  gs_variant_0100a-log_group   = abap_false.
  gs_variant_0100a-username    = abap_false.
  gs_variant_0100a-variant     = abap_false.
  gs_variant_0100a-text        = abap_false.
  gs_variant_0100a-dependvars  = abap_false.

  gs_layout_0100a-sel_mode     = 'A'.
  gs_layout_0100a-zebra        = abap_false.
  gs_layout_0100a-grid_title   = TEXT-001.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0100A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0100b .

  CLEAR: it_fiel_0100b[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZDE_DESTINACAO_VINCU'
    CHANGING
      ct_fieldcat            = it_fiel_0100b
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT it_fiel_0100b ASSIGNING FIELD-SYMBOL(<fs_vincu>).

    IF <fs_vincu>-fieldname EQ 'MENGE' OR <fs_vincu>-fieldname EQ 'VALOR'.
      <fs_vincu>-outputlen = 15.
      <fs_vincu>-do_sum    = abap_true.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0100B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0100b .

  gs_variant_0100b-report      = sy-repid.
  gs_variant_0100b-handle      = 'B100'.
  gs_variant_0100b-log_group   = abap_false.
  gs_variant_0100b-username    = abap_false.
  gs_variant_0100b-variant     = abap_false.
  gs_variant_0100b-text        = abap_false.
  gs_variant_0100b-dependvars  = abap_false.

  gs_layout_0100b-sel_mode     = 'A'.
  gs_layout_0100b-zebra        = abap_false.
  gs_layout_0100b-grid_title   = TEXT-002.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0100C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0100c .

  gs_variant_0100c-report      = sy-repid.
  gs_variant_0100c-handle      = 'C100'.
  gs_variant_0100c-log_group   = abap_false.
  gs_variant_0100c-username    = abap_false.
  gs_variant_0100c-variant     = abap_false.
  gs_variant_0100c-text        = abap_false.
  gs_variant_0100c-dependvars  = abap_false.

  gs_layout_0100c-sel_mode     = 'A'.
  gs_layout_0100c-zebra        = abap_false.
  gs_layout_0100c-grid_title   = TEXT-003.
  gs_layout_0100c-no_toolbar   = abap_true.
  gs_layout_0100c-info_fname   = 'LINE_COLOR'.
  gs_layout_0100c-stylefname   = 'STYLE'.
  gs_layout_0100c-ctab_fname   = 'COLOR_CELL'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_VINCULADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_vinculados .

  LOOP AT ob_destinacao->at_zmmt0116 INTO DATA(wa_zmmt0116).
    CLEAR: wa_vinculado.
    wa_vinculado-charg = wa_zmmt0116-charg.
    wa_vinculado-lgort = wa_zmmt0116-lgort.
    wa_vinculado-matnr = wa_zmmt0116-matnr.
    wa_vinculado-mblnr = wa_zmmt0116-orig_mblnr.
    wa_vinculado-meins = wa_zmmt0116-meins.
    wa_vinculado-menge = wa_zmmt0116-menge.
    wa_vinculado-mjahr = wa_zmmt0116-orig_mjahr.
    wa_vinculado-valor = wa_zmmt0116-valor.
    wa_vinculado-werks = wa_zmmt0116-werks.
    wa_vinculado-zeile = wa_zmmt0116-orig_zeile.
    wa_vinculado-ebeln = wa_zmmt0116-ebeln.
    wa_vinculado-ebelp = wa_zmmt0116-ebelp.
    APPEND wa_vinculado TO it_vinculado.
  ENDLOOP.

  LOOP AT ob_destinacao->at_zmmt0118 INTO DATA(wa_zmmt0118).
    CLEAR: wa_vinculado.
    wa_vinculado-chref = wa_zmmt0118-ch_referencia.
    wa_vinculado-charg = wa_zmmt0118-charg.
    wa_vinculado-lgort = wa_zmmt0118-lgort.
    wa_vinculado-matnr = wa_zmmt0118-matnr.
    wa_vinculado-meins = wa_zmmt0118-meins.
    wa_vinculado-menge = wa_zmmt0118-menge.
    wa_vinculado-valor = wa_zmmt0118-valor.
    wa_vinculado-werks = wa_zmmt0118-werks.
    APPEND wa_vinculado TO it_vinculado.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vincular_registro .

  ctl_alv_0100a->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(et_index_rows)
      et_row_no     = DATA(et_row_no)
  ).

  LOOP AT et_index_rows  INTO DATA(wa_registro) WHERE rowtype IS INITIAL.

    READ TABLE it_livre ASSIGNING FIELD-SYMBOL(<fs_livre>) INDEX wa_registro-index.

    IF <fs_livre>-utilizar LE 0.
      CONTINUE.
    ENDIF.

    CLEAR: wa_vinculado.
    wa_vinculado-charg = <fs_livre>-charg.
    wa_vinculado-chref = <fs_livre>-chref.
    wa_vinculado-lgort = <fs_livre>-lgort.
    wa_vinculado-matnr = <fs_livre>-matnr.

    wa_vinculado-mblnr = <fs_livre>-mblnr.
    wa_vinculado-mjahr = <fs_livre>-mjahr.
    wa_vinculado-zeile = <fs_livre>-zeile.

    wa_vinculado-meins = <fs_livre>-meins.
    wa_vinculado-menge = <fs_livre>-utilizar.
    wa_vinculado-valor = <fs_livre>-valor.
    wa_vinculado-werks = <fs_livre>-werks.
    wa_vinculado-ebeln = <fs_livre>-ebeln.
    wa_vinculado-ebelp = <fs_livre>-ebelp.
    APPEND wa_vinculado TO it_vinculado.

    <fs_livre>-saldo = <fs_livre>-saldo - <fs_livre>-utilizar.
    <fs_livre>-utilizar = <fs_livre>-saldo.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM desvincular_registro .

  ctl_alv_0100b->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(et_index_rows)
      et_row_no     = DATA(et_row_no)
  ).

  DATA(it_excluir) = it_vinculado[].

  LOOP AT et_index_rows INTO DATA(wa_index_rows)
    WHERE rowtype IS INITIAL.

    READ TABLE it_vinculado INDEX wa_index_rows-index INTO DATA(wa_vinculado).
    IF sy-subrc IS INITIAL.
      IF wa_vinculado-chref IS NOT INITIAL.
        DELETE it_excluir WHERE chref EQ wa_vinculado-chref.
      ELSE.
        DELETE it_excluir WHERE mblnr = wa_vinculado-mblnr AND mjahr = wa_vinculado-mjahr AND zeile = wa_vinculado-zeile.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: it_vinculado[].

  it_vinculado[] = it_excluir[].

  LOOP AT it_excluir INTO DATA(wa_excluir).

    IF wa_excluir-chref IS NOT INITIAL.
      READ TABLE it_livre ASSIGNING FIELD-SYMBOL(<fs_livre>) WITH KEY chref = wa_excluir-chref.
    ELSE.
      READ TABLE it_livre ASSIGNING <fs_livre>
        WITH KEY mblnr = wa_excluir-mblnr mjahr = wa_excluir-mjahr zeile = wa_excluir-zeile.
    ENDIF.

    IF sy-subrc IS INITIAL.
      ADD wa_excluir-menge TO <fs_livre>-menge.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_lancamento .

  TRY.

      IF ob_destinacao->at_zmmt0114-id_destinacao IS INITIAL.
        ob_destinacao->set_clear( ).
        LOOP AT it_vinculado INTO DATA(wa_vinculado).

          ob_destinacao->set_add_doc_material_origem(
            EXPORTING
              i_orig_mblnr             = wa_vinculado-mblnr    " Nº documento de material
              i_orig_mjahr             = wa_vinculado-mjahr    " Ano do documento do material
              i_orig_zeile             = wa_vinculado-zeile    " Item no documento do material
              i_orig_nfe               = lc_orig_nfe_inbound   " Chave NF-e
              i_orig_romaneio          = wa_vinculado-chref    " Chave de Referência
              i_menge                  = wa_vinculado-menge    " Quantidade
              i_meins                  = wa_vinculado-meins    " Unidade de medida básica
              i_valor                  = CONV #( wa_vinculado-valor )  " Montante básico
              i_forne                  = lc_forne                      " Nº conta do fornecedor
          ).

        ENDLOOP.
      ENDIF.

      ob_destinacao->set_gravar(
        )->set_gerar_movimento(
             EXPORTING
               i_gerar_via_job = abap_true     "*-CS2025000249-27.05.2025-#175255-JT
             IMPORTING
               e_gerou         = lc_gerou
               e_mblnr         = lc_mblnr
               e_mjahr         = lc_mjahr
               e_docnum        = lc_docnum
               e_belnr_dev     = lc_belnr_dev
               e_gjahr_dev     = lc_gjahr_dev
               e_docnum_dev    = lc_docnum_dev ).

      SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
        FROM j_1bnfdoc
       WHERE docnum EQ @lc_docnum
         AND cancel EQ @space.

      IF sy-subrc IS INITIAL.
*-CS2025000249-27.05.2025-#175255-JT-inicio
*       SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_j_1bnfdoc-docnum.
*       SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_j_1bnfdoc-bukrs.
*       CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
*-CS2025000249-27.05.2025-#175255-JT-fim

        DATA(lc_id_destinacao) = ob_destinacao->at_zmmt0114-id_destinacao.

        PERFORM carrega_by_id_destinacao USING lc_id_destinacao.
        PERFORM criar_componentes.
        EXIT.

      ENDIF.

    CATCH zcx_material_destinacao INTO DATA(ex_erro_material).    "
*     ex_erro_material->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ). "*-CS2025000249-27.05.2025-#175255-JT-inicio

      IF ex_erro_material->msgid EQ zcx_doc_eletronico=>zcx_nao_autorizado_uso-msgid AND
         ex_erro_material->msgno EQ zcx_doc_eletronico=>zcx_nao_autorizado_uso-msgno AND
         lc_docnum_dev IS NOT INITIAL.

        SELECT SINGLE * INTO @wa_j_1bnfdoc
          FROM j_1bnfdoc
         WHERE docnum EQ @lc_docnum_dev
           AND cancel EQ @space.

        IF sy-subrc IS INITIAL.
*-CS2025000249-27.05.2025-#175255-JT-inicio
*         SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_j_1bnfdoc-docnum.
*         SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_j_1bnfdoc-bukrs.
*         CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
*-CS2025000249-27.05.2025-#175255-JT-fim

          lc_id_destinacao = ob_destinacao->at_zmmt0114-id_destinacao.

          PERFORM carrega_by_id_destinacao USING lc_id_destinacao.

          TRY .

              zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = ob_destinacao->at_zmmt0114-docnum_dev
                )->set_registro( EXPORTING i_docnum = ob_destinacao->at_zmmt0114-docnum_dev i_sem_bloqueio = abap_true
                )->get_ck_autorizado_uso(
                ).

              ob_destinacao->set_gravar(
                )->set_gerar_movimento(
                     IMPORTING
                       e_gerou      = lc_gerou
                       e_mblnr      = lc_mblnr
                       e_mjahr      = lc_mjahr
                       e_docnum     = lc_docnum
                       e_belnr_dev  = lc_belnr_dev
                       e_gjahr_dev  = lc_gjahr_dev
                       e_docnum_dev = lc_docnum_dev ).

            CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).
*             ex_doc_eletronico->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).  "*-CS2025000249-27.05.2025-#175255-JT-inicio
            CATCH zcx_material_destinacao INTO ex_erro_material.
              ex_erro_material->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          ENDTRY.

          lc_id_destinacao = ob_destinacao->at_zmmt0114-id_destinacao.

          PERFORM carrega_by_id_destinacao USING lc_id_destinacao.
          PERFORM criar_componentes.

        ENDIF.
      ELSE.
        ex_erro_material->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ). "*-CS2025000249-27.05.2025-#175255-JT-inicio
      ENDIF.

      EXIT.
  ENDTRY.

  PERFORM limpar_tela_0100.
  LEAVE TO SCREEN 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_BY_ID_DESTINACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_ID_DESTINACAO  text
*----------------------------------------------------------------------*
FORM carrega_by_id_destinacao USING i_id_destinacao TYPE zde_destinacao.

  TRY .

      CLEAR: it_vinculado[].

      ck_permite_incluir_doc_materal = abap_false.

      IF ob_destinacao IS NOT INITIAL.
        ob_destinacao->set_clear( ).
        CLEAR: ob_destinacao.
      ENDIF.

      ob_destinacao = zcl_factory_mat_destinacao=>zif_factory_mat_destinacao~get_instance(
        )->set_factory_objeto( EXPORTING i_id_destinacao = i_id_destinacao
        )->get_factory_objeto(
        ).

      ob_destinacao->set_registro( i_id_destinacao = i_id_destinacao ).


      zmmt0114 = ob_destinacao->at_zmmt0114.
      it_zmmt0115 = ob_destinacao->at_zmmt0115.
      it_zmmt0116 = ob_destinacao->at_zmmt0116.
      it_zmmt0118 = ob_destinacao->at_zmmt0118.

      PERFORM carrega_vinculados.

    CATCH zcx_material_destinacao INTO DATA(ex_destinacao).
      ex_destinacao->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_LANCADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carrega_lancados  USING p_mblnr TYPE mblnr
                             p_mjahr TYPE mjahr.

  DATA: wa_zmmt0114_alv TYPE ty_zmmt0114_alv.

  CLEAR: it_zmmt0114[], it_zmmt0114, it_zmmt0114_alv[], it_zmmt0114_alv.

  SELECT * INTO TABLE @DATA(it_zmmt0116_ori)
    FROM zmmt0116
   WHERE orig_mblnr EQ @p_mblnr
     AND orig_mjahr EQ @p_mjahr.

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE @it_zmmt0114
    FROM zmmt0114
     FOR ALL ENTRIES IN @it_zmmt0116_ori
   WHERE id_destinacao EQ @it_zmmt0116_ori-id_destinacao
    ORDER BY PRIMARY KEY.

  CHECK sy-subrc IS INITIAL.

  LOOP AT it_zmmt0114 INTO DATA(wa_zmmt0114).
    CLEAR: wa_zmmt0114_alv.
    MOVE-CORRESPONDING wa_zmmt0114 TO wa_zmmt0114_alv.

    CASE wa_zmmt0114_alv-tp_destinacao.
      WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.
        wa_zmmt0114_alv-line_color = cs_line_color_a.
      WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.
        wa_zmmt0114_alv-line_color = cs_line_color_d.
    ENDCASE.

    APPEND wa_zmmt0114_alv TO it_zmmt0114_alv.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_LANCADOS_ROM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ROM_VINCULAR_CH_REFERENCIA  text
*----------------------------------------------------------------------*
FORM carrega_lancados_rom  USING p_ch_referencia TYPE zch_ref.

  DATA: wa_zmmt0114_alv TYPE ty_zmmt0114_alv.

  CLEAR: it_zmmt0114[], it_zmmt0114, it_zmmt0114_alv[], it_zmmt0114_alv.

  SELECT * INTO TABLE @DATA(it_zmmt0118_ori)
    FROM zmmt0118
   WHERE ch_referencia EQ @p_ch_referencia.

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE @it_zmmt0114
    FROM zmmt0114
     FOR ALL ENTRIES IN @it_zmmt0118_ori
   WHERE id_destinacao EQ @it_zmmt0118_ori-id_destinacao.

  CHECK sy-subrc IS INITIAL.

  LOOP AT it_zmmt0114 INTO DATA(wa_zmmt0114).
    CLEAR: wa_zmmt0114_alv.
    MOVE-CORRESPONDING wa_zmmt0114 TO wa_zmmt0114_alv.

    CASE wa_zmmt0114_alv-tp_destinacao.
      WHEN zif_material_destinacao=>st_tp_destinacao_armazenar.
        wa_zmmt0114_alv-line_color = cs_line_color_a.
      WHEN zif_material_destinacao=>st_tp_destinacao_devolucao.
        wa_zmmt0114_alv-line_color = cs_line_color_d.
    ENDCASE.

    APPEND wa_zmmt0114_alv TO it_zmmt0114_alv.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_estorno .

  IF ob_destinacao IS NOT INITIAL.
    TRY .
        ob_destinacao->set_estornar_movimento( IMPORTING e_estornou = DATA(e_estornou) ).
        CLEAR: ob_destinacao.
        LEAVE TO SCREEN 0.
      CATCH zcx_material_destinacao INTO DATA(ex_destinacao).
        ex_destinacao->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_zmmt0114 INDEX row_id INTO DATA(lc_zmmt0114).

  CASE fieldname.
    WHEN 'ID_DESTINACAO'.
      PERFORM carrega_by_id_destinacao USING lc_zmmt0114-id_destinacao.
      LEAVE TO SCREEN 0100.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  CHARMAR_NOTA_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM charmar_nota_fiscal .

  CHECK ob_destinacao IS NOT INITIAL.

  CHECK ob_destinacao->at_zmmt0114-docnum IS NOT INITIAL OR ob_destinacao->at_zmmt0114-docnum_dev IS NOT INITIAL.

  DATA(lc_docnum) = COND string( WHEN ob_destinacao->at_zmmt0114-docnum IS NOT INITIAL THEN ob_destinacao->at_zmmt0114-docnum
                                 ELSE ob_destinacao->at_zmmt0114-docnum_dev ).

  CHECK lc_docnum IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
    FROM j_1bnfdoc
   WHERE docnum EQ @lc_docnum.

  CHECK sy-subrc IS INITIAL.

  SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_j_1bnfdoc-docnum.
  SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_j_1bnfdoc-bukrs.
  CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHARMAR_TEXTO_NOTA_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM charmar_texto_nota_fiscal .

  CHECK ob_destinacao IS NOT INITIAL.

  CHECK ob_destinacao->at_zmmt0114-docnum IS NOT INITIAL OR ob_destinacao->at_zmmt0114-docnum_dev IS NOT INITIAL.

  CALL FUNCTION 'ZMF_DESTINACAO_MERC_OBS_FISCAL'
    EXPORTING
      i_id_destinacao = ob_destinacao->at_zmmt0114-id_destinacao
      i_informar      = abap_true.

ENDFORM.

*-CS2025000249-27.05.2025-#175255-JT-inicio
***********************************************************
* Ajustar incoterms NF
***********************************************************
FORM ajustar_incoterms_nota_fiscal.

  CHECK ob_destinacao IS NOT INITIAL.

  CHECK ob_destinacao->at_zmmt0114-docnum IS NOT INITIAL OR ob_destinacao->at_zmmt0114-docnum_dev IS NOT INITIAL.

  lc_docnum_inco = COND #( WHEN ob_destinacao->at_zmmt0114-docnum IS NOT INITIAL THEN ob_destinacao->at_zmmt0114-docnum
                                                                                 ELSE ob_destinacao->at_zmmt0114-docnum_dev ).
  CHECK lc_docnum_inco IS NOT INITIAL.

  SELECT SINGLE inco1, inco2
    INTO (@lc_inco1, @lc_inco2)
    FROM j_1bnfdoc
   WHERE docnum = @lc_docnum_inco.

  CHECK sy-subrc = 0.

  CALL SCREEN 0102 STARTING AT 70  05
                     ENDING AT 120 07.

ENDFORM.

***********************************************************
* validar incoterms NF
***********************************************************
FORM validar_incoterms CHANGING p_erro.

  p_erro = abap_false.

  IF lc_inco1 IS INITIAL.
    MESSAGE s024(sd) WITH 'Preencher o Incoterms!' DISPLAY LIKE 'E'.
    p_erro = abap_true.
    RETURN.
  ENDIF.

  SELECT SINGLE inco1
    INTO @DATA(_inco1)
    FROM tinc
   WHERE inco1 = @lc_inco1.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Incoterms Incorreto!' DISPLAY LIKE 'E'.
    p_erro = abap_true.
    RETURN.
  ENDIF.

ENDFORM.

***********************************************************
* module status 0102
***********************************************************
MODULE status_0102 OUTPUT.
  CLEAR ok_code2.

  SET PF-STATUS 'PF0102'.
  SET TITLEBAR 'PF0102'.

ENDMODULE.

***********************************************************
* module user_command_0102
***********************************************************
MODULE user_command_0102 INPUT.

  DATA: lv_erro TYPE char01.

  CASE ok_code2.
    WHEN 'CONFIRMAR'.
      PERFORM validar_incoterms CHANGING lv_erro.

      IF lv_erro = abap_false.
        ob_destinacao->set_ajustar_incoterms_nf( EXPORTING i_docnum = lc_docnum_inco
                                                           i_inco1  = lc_inco1
                                                           i_inco2  = lc_inco2 ).
        COMMIT WORK AND WAIT.
        MESSAGE s024(sd) WITH 'Incoterms ajustado!'.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR ok_code2.

ENDMODULE.
*-CS2025000249-27.05.2025-#175255-JT-fim

***********************************************************
***********************************************************
