*----------------------------------------------------------------------*
***INCLUDE ZMMR153_USER_0101.
*----------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0101a DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_alv_toolbar_0101b DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_event_handler_0101b DEFINITION.
  PUBLIC SECTION.
    DATA: validar_data  TYPE c,
          error_in_data TYPE c,
          ls_good       TYPE lvc_s_modi,
          lv_value      TYPE lvc_value.

    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    "METHODS HANDLE_DOUBLE_CLICK  FOR EVENT DOUBLE_CLICK  OF CL_GUI_ALV_GRID IMPORTING E_ROW E_COLUMN ES_ROW_NO.
    METHODS data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.

  PRIVATE SECTION.
    TYPES: ddshretval_table TYPE TABLE OF ddshretval.
    METHODS: perform_semantic_checks
      IMPORTING
        pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: splitter          TYPE REF TO cl_gui_splitter_container,
      cccontainer       TYPE REF TO cl_gui_custom_container,
      container_0101    TYPE REF TO cl_gui_custom_container,
      html_localiza     TYPE REF TO cl_gui_html_viewer,
      html_control_0101 TYPE REF TO cl_gui_html_viewer,
      htmllocalizador   TYPE string,
      htmlpesomedio     TYPE string,
      lc_id_ordem_ant   TYPE zde_id_ordem,
      lc_chave_nfe_ant  TYPE zde_chave_nfe.


DATA: ctl_cccontainer_0101a    TYPE REF TO cl_gui_container,
      ctl_alv_0101a            TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog_0101a    TYPE lvc_t_fcat,
      gs_variant_0101a         TYPE disvariant,
      gs_layout_0101a          TYPE lvc_s_layo,
      obg_toolbar_0101a        TYPE REF TO lcl_alv_toolbar_0101a,
      obj_toolbarmanager_0101a TYPE REF TO cl_alv_grid_toolbar_manager,
      gs_scroll_col_0101a      TYPE lvc_s_col,
      gs_scroll_row_0101a      TYPE lvc_s_roid,
      wa_stable_0101a          TYPE lvc_s_stbl,
      it_sort_0101a            TYPE lvc_t_sort.

DATA: ctl_cccontainer_0101b    TYPE REF TO cl_gui_container,
      ctl_alv_0101b            TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog_0101b    TYPE lvc_t_fcat,
      gs_variant_0101b         TYPE disvariant,
      gs_layout_0101b          TYPE lvc_s_layo,
      obg_toolbar_0101b        TYPE REF TO lcl_alv_toolbar_0101b,
      obj_toolbarmanager_0101b TYPE REF TO cl_alv_grid_toolbar_manager,
      gs_scroll_col_0101b      TYPE lvc_s_col,
      gs_scroll_row_0101b      TYPE lvc_s_roid,
      wa_stable_0101b          TYPE lvc_s_stbl,
      event_handler_0101b      TYPE REF TO lcl_event_handler_0101b.

DATA: ck_alterado_carga  TYPE c LENGTH 1.
DATA: ck_alterado_frete  TYPE c LENGTH 1.
DATA: ck_add_nota        TYPE c.
DATA: ck_add_oc          TYPE c.
DATA: ck_add_peso        TYPE c.
"DATA: CK_ADD_TAKE_UP     TYPE C.
DATA: ck_add_bloco       TYPE c.

CLASS lcl_alv_toolbar_0101a IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager_0101a
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD obj_toolbarmanager_0101a->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.

    CALL METHOD ctl_alv_0101a->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.

CLASS lcl_alv_toolbar_0101b IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager_0101b
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar  TYPE stb_button,
          cl_disabled TYPE c.

    cl_disabled = COND string(
                    WHEN ( zde_zsdt0001cg_alv-tp_status NE zif_carga=>st_status_aberto OR objeto->at_manutencao EQ abap_true ) THEN abap_true
                    ELSE abap_false ).

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_create.
    ty_toolbar-function  = 'INCLUIR'.
    ty_toolbar-quickinfo = TEXT-044.
    ty_toolbar-text      = TEXT-044.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = cl_disabled.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'ELIMINAR'.
    ty_toolbar-quickinfo = TEXT-045.
    ty_toolbar-text      = TEXT-045.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = cl_disabled.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Vincular Blocos e Fardos
    ty_toolbar-icon      = icon_bw_info_cube_ina.
    ty_toolbar-function  = 'VINCULAR'.
    ty_toolbar-quickinfo = TEXT-046.
    ty_toolbar-text      = TEXT-046.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = cl_disabled.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD obj_toolbarmanager_0101b->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'VINCULAR'.

*        alterado por guilherme rabelo inicio.
        IF zde_zsdt0001cg_alv-nm_peso_bruto <> '0' AND zde_zsdt0001cg_alv-nm_peso_tara <> '0' AND
          zde_zsdt0001cg_alv-nm_peso_subtotal <> '0'.

          PERFORM vincular_blocos_fardos.
        ELSE.

          MESSAGE 'Obrigatório informações do Peso de Balança' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.

        ENDIF.
*        alterado por guilherme rabelo fim.
      WHEN 'INCLUIR'.
        PERFORM incluir_ordem_venda.

      WHEN 'ELIMINAR'.
        PERFORM eliminar_ordem_venda.

    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.

CLASS lcl_event_handler_0101b IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_0101b USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD data_changed_finished.

    IF e_modified IS NOT INITIAL.

      PERFORM atualizar_dados_tela.

      wa_stable_0101a-row = abap_true.
      wa_stable_0101a-col = abap_true.
      CALL METHOD ctl_alv_0101a->refresh_table_display
        EXPORTING
          is_stable = wa_stable_0101a.

      wa_stable_0101b-row = abap_true.
      wa_stable_0101b-col = abap_true.
      CALL METHOD ctl_alv_0101b->refresh_table_display
        EXPORTING
          is_stable = wa_stable_0101b.
    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD data_changed.
    error_in_data = space.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.
  ENDMETHOD.                    "on_data_chaged

  METHOD perform_semantic_checks.

    DATA: i_volume TYPE	volum_ap.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good)
      WHERE fieldname EQ 'QT_FARDOS'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE it_ordens_venda_alv INTO wa_ordens_venda_alv INDEX ls_good-row_id.

      IF ls_good-value IS INITIAL.
        CONTINUE.
      ENDIF.
      MOVE lv_value TO i_volume.

      TRY .

          objeto->set_volume_ordem_venda(
              i_vbeln  = wa_ordens_venda_alv-nr_ordem_venda
              i_volume = i_volume
            ).

        CATCH zcx_carga INTO ex_carga.

          error_in_data = abap_true.

          CALL METHOD pr_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = ex_carga->msgid
              i_msgno     = ex_carga->msgno
              i_msgty     = 'E'
              i_msgv1     = ex_carga->msgv1
              i_msgv2     = ex_carga->msgv2
              i_msgv3     = ex_carga->msgv3
              i_msgv4     = ex_carga->msgv4
              i_fieldname = ls_good-fieldname
              i_row_id    = ls_good-row_id.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  DATA: lc_info_frete TYPE zde_zsdt0001od_alv.

  CLEAR: ok_suppress_dialog.

  READ TABLE it_tucomm INTO wa_code WITH KEY = ok_code.
  IF sy-subrc IS INITIAL.
    MESSAGE 'Função não permitida' TYPE 'S' DISPLAY LIKE 'E'.
    CLEAR: ok_code.
    EXIT.
  ENDIF.

  IF ok_code_suppress IS NOT INITIAL.
    ok_code = ok_code_suppress.
    CLEAR: ok_code_suppress.
  ENDIF.

  CASE ok_code.
    WHEN 'NOVA_SOLIC'.

      "Solicitar Manutenação
      CLEAR: ok_code.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = TEXT-105
          text_question         = TEXT-106
          text_button_1         = TEXT-007
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = TEXT-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      SUBMIT zmmr153
        WITH pck_cad  EQ abap_true
        WITH pmanut   EQ abap_true
        WITH psafra   EQ psafra
        WITH pempre   EQ pempre
        WITH pfilia   EQ pfilia
        WITH pidcarga EQ objeto->carga-id_carga
        AND RETURN.

    WHEN 'ADDNOTA'.

      CLEAR: ok_code, ck_add_nota.
      CALL SCREEN 8001 STARTING AT 40 01.

      IF wa_add_nfe_9002-nm_pesob IS NOT INITIAL AND zde_zsdt0001cg_alv-nm_peso_subtotal IS NOT INITIAL.
        zde_zsdt0001cg_alv-nm_peso_subtotal  = wa_add_nfe_9002-nm_pesob.
        IF wa_add_nfe_9002-nm_pesol             IS NOT INITIAL AND
           zde_zsdt0001cg_alv-nm_peso_descontos IS INITIAL AND
           zde_zsdt0001cg_alv-nm_peso_liquido   IS INITIAL.

          zde_zsdt0001cg_alv-nm_peso_subtotal  = wa_add_nfe_9002-nm_pesob.
          zde_zsdt0001cg_alv-nm_peso_embalagem = wa_add_nfe_9002-nm_pesob - wa_add_nfe_9002-nm_pesol.
          zde_zsdt0001cg_alv-nm_peso_liquido   = wa_add_nfe_9002-nm_pesol.
        ENDIF.
      ENDIF.

      IF ck_add_nota EQ abap_true.
        ok_code_suppress   = 'ADDOC'.
        ok_suppress_dialog = abap_true.
      ENDIF.

      PERFORM atualizar_dados_tela.

    WHEN 'ADDOC'.

      CHECK objeto->at_manutencao EQ abap_false.

      READ TABLE it_ordens_venda_alv INDEX 1 INTO wa_ordens_venda_alv.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE s038 DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      CLEAR: ok_code, ck_add_oc.

      zcl_ordem_venda=>zif_ordem_venda~get_instance(
        )->set_ordem_venda( i_vbeln = wa_ordens_venda_alv-nr_ordem_venda
        )->get_tipo_frete( IMPORTING e_tipo_frete = DATA(e_tipo_frete)
        ).

      CALL FUNCTION 'ZMF_LANC_ORDEM_CARREGAME'
        EXPORTING
          i_nr_safra       = zde_zsdt0001cg_alv-nr_safra
          i_id_bukrs       = zde_zsdt0001cg_alv-id_bukrs
          i_id_branch      = zde_zsdt0001cg_alv-id_branch
          i_tipo_frete     = e_tipo_frete
          i_id_ordem       = zde_zsdt0001cg_alv-id_ordem
          i_fundo          = abap_false
          i_id_agent_frete = zde_zsdt0001cg_alv-id_agent_frete
        IMPORTING
          e_id_ordem       = zde_zsdt0001cg_alv-id_ordem
          e_ordem_alv      = lc_info_frete.

      IF zde_zsdt0001cg_alv-id_ordem IS NOT INITIAL.

        TRY .
            objeto->set_ordem_carregamento(
              EXPORTING
                i_nr_safra  = zde_zsdt0001cg_alv-nr_safra
                i_id_bukrs  = zde_zsdt0001cg_alv-id_bukrs
                i_id_branch = zde_zsdt0001cg_alv-id_branch
                i_nr_ordem  = lc_info_frete-nr_ordem
                i_vbeln     = wa_ordens_venda_alv-nr_ordem_venda
              IMPORTING
                e_ordem_carrgamento = zde_zsdt0001od_alv
              CHANGING
                i_carga_alv = zde_zsdt0001cg_alv
            )->verif_ordem_carregamento(
            )->set_agente_frete( i_id_agent_frete = lc_info_frete-id_agent_frete
            ).

            ok_code_suppress   = 'ADDPESO'.
            ok_suppress_dialog = abap_true.

          CATCH zcx_ordem_carregamento INTO ex_ordem_carregamento.
            ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_carga INTO ex_carga.
            ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          CATCH zcx_parceiros INTO ex_parceiros.
            ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        ENDTRY.

      ELSEIF lc_info_frete IS NOT INITIAL.

        objeto->set_info_frete_without_oc(
          EXPORTING
            i_id_proprietario  = lc_info_frete-id_proprietario    " Proprietário do Veículo
            i_ds_proprietario  = lc_info_frete-ds_proprietario    " Proprietário do Veículo
            i_ds_placa_trator  = lc_info_frete-ds_placa_trator    " Placa Veículo Tração
            i_ds_placa_reboq_1 = lc_info_frete-ds_placa_reboq_1   " Placa Veículo Reboque 1
            i_ds_placa_reboq_2 = lc_info_frete-ds_placa_reboq_2   " Placa Veículo Reboque 2
            i_ds_placa_reboq_3 = lc_info_frete-ds_placa_reboq_3   " Placa Veículo Reboque 3
            i_id_motorista     = lc_info_frete-id_motorista       " Motorista
            i_ds_motorista     = lc_info_frete-ds_motorista       " Motorista
        )->set_agente_frete( i_id_agent_frete = lc_info_frete-id_agent_frete
        ).



      ENDIF.

      PERFORM atualizar_dados_tela.

    WHEN 'ADDPESO'.

      CLEAR: ok_code, ck_add_peso.
      CALL SCREEN 8004 STARTING AT 40 01.

      IF ck_add_peso EQ abap_true.
        ok_code_suppress   = 'ADDBLOCO'.
        ok_suppress_dialog = abap_true.
      ENDIF.

      PERFORM atualizar_dados_tela.

    WHEN 'ADDBLOCO'.

      CHECK objeto->at_manutencao EQ abap_false.

      READ TABLE it_ordens_venda_alv INDEX 1 INTO wa_ordens_venda_alv.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE s038 DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      CLEAR: ok_code, ck_add_bloco.

      DATA: it_index_rows TYPE lvc_t_row.
      CLEAR: it_index_rows.
      APPEND VALUE #( index = 1 ) TO it_index_rows.

      ctl_alv_0101b->set_selected_rows( EXPORTING it_index_rows = it_index_rows ).
      PERFORM vincular_blocos_fardos.

      IF ck_add_bloco EQ abap_true.
        ok_code_suppress   = 'XXXXXXX'.
        ok_suppress_dialog = abap_true.
      ENDIF.

      PERFORM atualizar_dados_tela.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  DATA: it_function_0101 TYPE ui_functions.

  PERFORM cria_localizador_0101.

  IF cccontainer IS INITIAL.

    CREATE OBJECT cccontainer
      EXPORTING
        container_name = 'ALVS'.

    CREATE OBJECT splitter
      EXPORTING
        parent  = cccontainer
        rows    = 2
        columns = 1.

    ctl_cccontainer_0101a = splitter->get_container( row = 2 column = 1 ).

    ctl_cccontainer_0101b = splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT ctl_alv_0101a
      EXPORTING
        i_parent = ctl_cccontainer_0101a.

    PERFORM fill_it_fieldcatalog_0101a.

    PERFORM fill_gs_variant_0101a.

    PERFORM fill_it_sort_0101a.

    CREATE OBJECT obg_toolbar_0101a
      EXPORTING
        io_alv_grid = ctl_alv_0101a.

    SET HANDLER obg_toolbar_0101a->on_toolbar FOR ctl_alv_0101a.
    SET HANDLER obg_toolbar_0101a->handle_user_command FOR ctl_alv_0101a.

    CALL METHOD ctl_alv_0101a->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_0101a
        is_variant      = gs_variant_0101a
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_0101a
        it_outtab       = it_blocos_vincu[]
        it_sort         = it_sort_0101a[].

    CREATE OBJECT ctl_alv_0101b
      EXPORTING
        i_parent = ctl_cccontainer_0101b.

    PERFORM fill_it_fieldcatalog_0101b.

    PERFORM fill_gs_variant_0101b.

    CREATE OBJECT obg_toolbar_0101b
      EXPORTING
        io_alv_grid = ctl_alv_0101b.

    SET HANDLER obg_toolbar_0101b->on_toolbar FOR ctl_alv_0101b.
    SET HANDLER obg_toolbar_0101b->handle_user_command FOR ctl_alv_0101b.

    CLEAR: it_function_0101[].
    APPEND cl_gui_alv_grid=>mc_fc_excl_all TO it_function_0101.

    CALL METHOD ctl_alv_0101b->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout_0101b
        is_variant           = gs_variant_0101b
        i_save               = 'A'
        it_toolbar_excluding = it_function_0101
      CHANGING
        it_fieldcatalog      = it_fieldcatalog_0101b
        it_outtab            = it_ordens_venda_alv[].

    CALL METHOD ctl_alv_0101b->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv_0101b->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT event_handler_0101b.
    SET HANDLER event_handler_0101b->handle_hotspot_click FOR ctl_alv_0101b.
    SET HANDLER event_handler_0101b->data_changed FOR ctl_alv_0101b.
    SET HANDLER event_handler_0101b->data_changed_finished FOR ctl_alv_0101b.

  ENDIF.

  PERFORM atualizar_dados_tela.

  "PERFORM CRIAR_DOCKING.

  wa_stable_0101a-row = abap_true.
  wa_stable_0101a-col = abap_true.

  CALL METHOD ctl_alv_0101a->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0101a.

  wa_stable_0101b-row = abap_true.
  wa_stable_0101b-col = abap_true.

  CALL METHOD ctl_alv_0101b->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0101b.

  LOOP AT SCREEN.
    SPLIT screen-name AT '-' INTO: DATA(str1a_0101) DATA(str2a_0101).
    IF str1a_0101 EQ 'ZDE_ZSDT0001CG_ALV' OR str1a_0101 EQ 'ZDE_ZSDT0001OD_ALV' OR str1a_0101 EQ 'ZDE_ZSDT0001OV_ALV'.

      IF str2a_0101 EQ 'NM_PESO_SUBTOTAL'.
        objeto->valida_atributo_alteravel( EXPORTING i_campo = 'NM_PESO_BRUTO' IMPORTING e_permitido = DATA(e_permitido_0101) ).
        IF e_permitido_0101 EQ abap_false.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
        CONTINUE.
      ENDIF.

      objeto->valida_atributo_alteravel( EXPORTING i_campo = CONV #( str2a_0101 ) IMPORTING e_permitido = e_permitido_0101 ).
      IF e_permitido_0101 EQ abap_false.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERA_BRUTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE altera_bruto INPUT.
  PERFORM seta_carga.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SETA_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seta_carga .

  TRY.
      objeto->set_carga( EXPORTING i_carga = zde_zsdt0001cg_alv IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv ).
      CLEAR: ck_alterado_carga.
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ALTERA_TARA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE altera_tara INPUT.
  PERFORM seta_carga.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDA_SUBTOTAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_subtotal INPUT.

  objeto->get_calcular_subtotal( EXPORTING i_peso_bruto = zde_zsdt0001cg_alv-nm_peso_bruto i_peso_tara = zde_zsdt0001cg_alv-nm_peso_tara
    IMPORTING
      e_peso_subtotal = DATA(e_peso_subtotal) ).

  IF zde_zsdt0001cg_alv-nm_peso_subtotal NE e_peso_subtotal.
    MESSAGE e063.
  ENDIF.

  PERFORM seta_carga.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTERA_DESCONTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE altera_embalagem INPUT.
  PERFORM seta_carga.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_CARGA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribui_info_carga INPUT.
  TRY .
      objeto->set_transgenia(
         CHANGING
           i_in_gmo                = zde_zsdt0001cg_alv-in_gmo
           i_nr_resultado_01       = zde_zsdt0001cg_alv-nr_resultado_01
           i_nr_resultado_02       = zde_zsdt0001cg_alv-nr_resultado_02
           i_nr_res_rr1_rr2        = zde_zsdt0001cg_alv-nr_res_rr1_rr2
           i_in_gmo_03             = zde_zsdt0001cg_alv-in_gmo_03
           i_in_srr_origem_partic  = zde_zsdt0001cg_alv-in_srr_origem_partic
           i_id_outro_partic       = zde_zsdt0001cg_alv-id_outro_partic
           i_in_srr_declarado      = zde_zsdt0001cg_alv-in_srr_declarado
           i_in_teste_srr          = zde_zsdt0001cg_alv-in_teste_srr
           i_in_srr_declarado_2    = zde_zsdt0001cg_alv-in_srr_declarado_2
           i_in_teste_srr_2        = zde_zsdt0001cg_alv-in_teste_srr_2
           i_id_classificadora     = zde_zsdt0001cg_alv-id_classificadora
           i_tp_transgenia         = zde_zsdt0001cg_alv-tp_transgenia   ).
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

  ck_alterado_carga = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0101A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0101a .

  gs_variant_0101a-report      = sy-repid.
  "GS_VARIANT_0101A-HANDLE      = '0101'.
  gs_variant_0101a-log_group   = abap_false.
  gs_variant_0101a-username    = abap_false.
  gs_variant_0101a-variant     = abap_false.
  gs_variant_0101a-text        = abap_false.
  gs_variant_0101a-dependvars  = abap_false.

  gs_layout_0101a-sel_mode     = 'A'.
  gs_layout_0101a-zebra        = abap_false.
  gs_layout_0101a-cwidth_opt   = abap_false.
  gs_layout_0101a-no_toolbar   = abap_true.
  gs_layout_0101a-grid_title   = TEXT-042.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0101b
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0101b .

  gs_variant_0101b-report      = sy-repid.
  "GS_VARIANT_0101B-HANDLE      = '0100'.
  gs_variant_0101b-log_group   = abap_false.
  gs_variant_0101b-username    = abap_false.
  gs_variant_0101b-variant     = abap_false.
  gs_variant_0101b-text        = abap_false.
  gs_variant_0101b-dependvars  = abap_false.

  gs_layout_0101b-sel_mode     = 'A'.
  gs_layout_0101b-zebra        = abap_false.
  gs_layout_0101b-cwidth_opt   = abap_true.
  gs_layout_0101b-info_fname   = 'LINE_COLOR'.
  gs_layout_0101b-stylefname   = 'STYLE'.
  gs_layout_0101b-ctab_fname   = 'COLOR_CELL'.
  gs_layout_0101b-grid_title   = TEXT-043.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0101b .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_0101b[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSDT0001OV_ALGODAO_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_0101b.

  DATA(cl_editabled) = COND string(
                         WHEN zde_zsdt0001cg_alv-tp_status EQ zif_carga=>st_status_aberto
                          AND objeto->at_manutencao EQ abap_false THEN abap_true
                         ELSE abap_false ).

  LOOP AT it_fieldcatalog_0101b ASSIGNING <fs_cat>.

    <fs_cat>-tabname = 'ZSDT0001OV_ALGODAO_ALV'.

    CASE <fs_cat>-fieldname.
      WHEN 'ADD_BLOCO'.
        <fs_cat>-hotspot = abap_true.
        <fs_cat>-icon = abap_true.
        <fs_cat>-scrtext_l = 'Blocos'.
        <fs_cat>-scrtext_m = 'Blocos'.
        <fs_cat>-scrtext_s = 'Blocos'.

      WHEN 'ID_CARGA' OR
           'NR_ROMANEIO_SAI' OR
           'CH_REFERENCIA_SAI' OR
           'NM_QTD_EMBALAGENS' OR 'LINE_COLOR' OR 'COLOR_CELL' OR 'STYLE' OR 'ID_ORDEM' OR
           'TOTAL_FARDOS_CG' OR 'TOTAL_FARDOS_RM' OR 'TOTAL_FARDOS_FT' OR 'STATUS'.
        <fs_cat>-no_out = abap_true.

      WHEN 'QT_FARDOS'.
        <fs_cat>-outputlen = 15.
        <fs_cat>-do_sum = abap_true.
        <fs_cat>-edit = cl_editabled.

      WHEN 'TOTAL_FARDOS' OR
           'TOTAL_FARDOS_CG' OR
           'TOTAL_FARDOS_RM' OR
           'TOTAL_FARDOS_FT' OR
           'NM_PESO_EMBALAGEM' OR
           'NM_PESO_LIQUIDO' OR
           'NM_PESO_BRUTO' OR
           'NM_PESO_TARA' OR
           'NM_PESO_SUBTOTAL'.

        <fs_cat>-outputlen = 15.
        <fs_cat>-do_sum = abap_true.

      WHEN 'NR_ORDEM_VENDA'.
        <fs_cat>-hotspot = abap_true.

      WHEN 'NR_ORDEM'.
        <fs_cat>-hotspot = abap_true.
        <fs_cat>-icon = abap_true.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0101A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0101a .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_0101a[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0001FD_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_0101a.

  lc_col_pos = 11.

  LOOP AT it_fieldcatalog_0101a ASSIGNING <fs_cat>.

    <fs_cat>-tabname = 'ZDE_ZSDT0001FD_ALV'.

    CASE <fs_cat>-fieldname.
      WHEN 'DS_PONTO_C'.
        <fs_cat>-outputlen = 40.
        <fs_cat>-col_opt = 1.

      WHEN 'NR_ORDEM_VENDA'.
        <fs_cat>-col_opt = 2.

      WHEN 'DS_CONTRATO'.
        <fs_cat>-outputlen = 10.
        <fs_cat>-col_opt = 3.

      WHEN 'DS_INSTRUCAO'.
        <fs_cat>-outputlen = 10.
        <fs_cat>-col_opt = 4.

      WHEN 'NM_BLOCO'.
        <fs_cat>-outputlen = 5.
        <fs_cat>-col_opt = 5.

      WHEN 'PS_FARDOS_BRUTO'.

        <fs_cat>-do_sum = abap_true.
        <fs_cat>-outputlen = 12.
        <fs_cat>-col_opt = 6.

      WHEN 'PS_FARDOS_LIQUI'.

        <fs_cat>-do_sum = abap_true.
        <fs_cat>-outputlen = 12.
        <fs_cat>-col_opt = 7.

      WHEN 'QT_FARDOS'.

        <fs_cat>-do_sum = abap_true.
        <fs_cat>-outputlen = 08.
        <fs_cat>-col_opt = 8.

      WHEN 'DS_SAFRA'.
        <fs_cat>-just = 'C'.
        <fs_cat>-outputlen = 8.
        <fs_cat>-col_opt = 9.

      WHEN 'ID_WERKS'.
        <fs_cat>-just = 'C'.
        <fs_cat>-outputlen = 8.
        <fs_cat>-col_opt = 10.

      WHEN OTHERS.
        <fs_cat>-no_out = abap_true.
        <fs_cat>-col_opt = lc_col_pos.
        ADD 1 TO lc_col_pos.

    ENDCASE.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101_exit INPUT.



ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_tela_0101 .

  CLEAR: obg_toolbar_0101a.

  IF ctl_alv_0101a IS NOT INITIAL.
    ctl_alv_0101a->free( ).
  ENDIF.
  CLEAR: ctl_alv_0101a.

  IF ctl_cccontainer_0101a IS NOT INITIAL.
    ctl_cccontainer_0101a->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_0101a.

  CLEAR: obg_toolbar_0101b.

  IF ctl_alv_0101b IS NOT INITIAL.
    ctl_alv_0101b->free( ).
  ENDIF.
  CLEAR: ctl_alv_0101b.

  IF ctl_cccontainer_0101b IS NOT INITIAL.
    ctl_cccontainer_0101b->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_0101b.

  IF splitter IS NOT INITIAL.
    splitter->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: splitter.

  IF cccontainer IS NOT INITIAL.
    cccontainer->free( ).
  ENDIF.
  CLEAR: cccontainer.

  IF html_localiza IS NOT INITIAL.
    html_localiza->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: html_localiza.

  IF container_0101 IS NOT INITIAL.
    container_0101->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: container_0101.

  IF html_control_0101 IS NOT INITIAL.
    html_control_0101->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: html_control_0101.

  CLEAR: lc_chave_nfe_ant.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_0101B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click_0101b
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: e_ordem_alv TYPE  zde_zsdt0001od_alv.

  READ TABLE it_ordens_venda_alv INDEX row_id INTO wa_ordens_venda_alv.

  CASE fieldname.
    WHEN 'ADD_BLOCO'.

      CHECK objeto->at_manutencao EQ abap_false.

      DATA: it_index_rows TYPE lvc_t_row.
      CLEAR: it_index_rows.
      APPEND VALUE #( index = row_id ) TO it_index_rows.

      ctl_alv_0101b->set_selected_rows( EXPORTING it_index_rows = it_index_rows ).
      PERFORM vincular_blocos_fardos.

    WHEN 'NR_ORDEM_VENDA'.

      zcl_ordem_venda=>zif_ordem_venda~open( i_vbeln = wa_ordens_venda_alv-nr_ordem_venda ).

    WHEN 'NR_ORDEM'.

      CHECK objeto->at_manutencao EQ abap_false.

      CLEAR: ck_add_oc.
      TRY .

          zcl_ordem_venda=>zif_ordem_venda~get_instance(
            )->set_ordem_venda( i_vbeln = wa_ordens_venda_alv-nr_ordem_venda
            )->get_tipo_frete( IMPORTING e_tipo_frete = DATA(e_tipo_frete)
            ).

          CALL FUNCTION 'ZMF_LANC_ORDEM_CARREGAME'
            EXPORTING
              i_nr_safra       = zde_zsdt0001cg_alv-nr_safra
              i_id_bukrs       = zde_zsdt0001cg_alv-id_bukrs
              i_id_branch      = zde_zsdt0001cg_alv-id_branch
              i_tipo_frete     = e_tipo_frete
              i_id_ordem       = wa_ordens_venda_alv-id_ordem
              i_fundo          = abap_false
              i_id_agent_frete = zde_zsdt0001cg_alv-id_agent_frete
            IMPORTING
              e_id_ordem       = wa_ordens_venda_alv-id_ordem
              e_ordem_alv      = e_ordem_alv.

          IF wa_ordens_venda_alv-id_ordem IS NOT INITIAL.

            objeto->set_ordem_carregamento(
              EXPORTING
                i_nr_safra  = zde_zsdt0001cg_alv-nr_safra
                i_id_bukrs  = zde_zsdt0001cg_alv-id_bukrs
                i_id_branch = zde_zsdt0001cg_alv-id_branch
                i_nr_ordem  = e_ordem_alv-nr_ordem
                i_vbeln     = wa_ordens_venda_alv-nr_ordem_venda
              CHANGING
                i_carga_alv = zde_zsdt0001cg_alv
            )->set_agente_frete( i_id_agent_frete = e_ordem_alv-id_agent_frete
            ).

            IF ck_add_oc EQ abap_true.
              ok_code_suppress   = 'ADDTAKE'.
              ok_suppress_dialog = abap_true.
            ENDIF.

          ELSE.

            objeto->set_info_frete_without_oc(
                 EXPORTING
                   i_id_proprietario  = e_ordem_alv-id_proprietario    " Proprietário do Veículo
                   i_ds_proprietario  = e_ordem_alv-ds_proprietario    " Proprietário do Veículo
                   i_ds_placa_trator  = e_ordem_alv-ds_placa_trator    " Placa Veículo Tração
                   i_ds_placa_reboq_1 = e_ordem_alv-ds_placa_reboq_1   " Placa Veículo Reboque 1
                   i_ds_placa_reboq_2 = e_ordem_alv-ds_placa_reboq_2   " Placa Veículo Reboque 2
                   i_ds_placa_reboq_3 = e_ordem_alv-ds_placa_reboq_3   " Placa Veículo Reboque 3
                   i_id_motorista     = e_ordem_alv-id_motorista       " Motorista
                   i_ds_motorista     = e_ordem_alv-ds_motorista       " Motorista
               )->set_agente_frete( i_id_agent_frete = e_ordem_alv-id_agent_frete
               ).

          ENDIF.

        CATCH zcx_ordem_carregamento INTO ex_ordem_carregamento.
          ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

      DATA(ordem_principal) = zde_zsdt0001od_alv.
      PERFORM atualizar_dados_tela.

      IF zde_zsdt0001cg_alv-id_ordem EQ zde_zsdt0001od_alv-id_ordem OR
         ordem_principal-id_agent_frete NE zde_zsdt0001od_alv-id_agent_frete.
        LEAVE TO SCREEN 0100.
      ELSE.
        wa_stable_0101b-row = abap_true.
        wa_stable_0101b-col = abap_true.
        CALL METHOD ctl_alv_0101b->refresh_table_display
          EXPORTING
            is_stable = wa_stable_0101b.
      ENDIF.


  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIA_LOCALIZADOR_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_localizador_0101 .

  CLEAR: zlest0185.

  DATA(ck_limpar) = abap_false.

  IF zde_zsdt0001od_alv-id_ordem IS NOT INITIAL.

    IF lc_id_ordem_ant NE zde_zsdt0001od_alv-id_ordem OR lc_id_ordem_ant IS INITIAL.
      lc_id_ordem_ant = zde_zsdt0001od_alv-id_ordem.

      SELECT SINGLE * INTO @zlest0185
        FROM zlest0185
       WHERE id_ordem EQ @zde_zsdt0001od_alv-id_ordem.

      IF sy-subrc IS INITIAL.

        IF container_0101 IS INITIAL.

          CREATE OBJECT container_0101
            EXPORTING
              container_name = 'LOCALIZADOR'
            EXCEPTIONS
              OTHERS         = 1.

          CREATE OBJECT html_localiza
            EXPORTING
              parent = container_0101.

        ENDIF.

        DATA: data_table TYPE STANDARD TABLE OF text255,
              i_urlloca  TYPE c LENGTH 200.

        PERFORM gera_html_localizador.

        CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
          EXPORTING
            i_string         = htmllocalizador
            i_tabline_length = 255
          TABLES
            et_table         = data_table.

        html_localiza->load_data(
          IMPORTING
            assigned_url           = i_urlloca
          CHANGING
            data_table             = data_table
          EXCEPTIONS
            dp_invalid_parameter   = 1
            dp_error_general       = 2
            cntl_error             = 3
            html_syntax_notcorrect = 4
            OTHERS                 = 5
        ).

        html_localiza->show_url(
          EXPORTING
            url                    = i_urlloca
          EXCEPTIONS
            cntl_error             = 1
            cnht_error_not_allowed = 2
            cnht_error_parameter   = 3
            dp_error_general       = 4
            OTHERS                 = 5
        ).

      ELSE.
        ck_limpar = abap_true.
      ENDIF.

    ENDIF.

  ELSE.
    CLEAR: lc_id_ordem_ant, htmllocalizador.
    ck_limpar = abap_true.
  ENDIF.

  CHECK ck_limpar EQ abap_true.

  IF html_localiza IS NOT INITIAL.
    html_localiza->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: html_localiza.

  IF container_0101 IS NOT INITIAL.
    container_0101->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
  ENDIF.
  CLEAR: container_0101.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_DOCKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_danfe .

  CHECK docking_0100 IS NOT INITIAL.

  DATA(ck_limpar) = abap_false.

  IF zde_zsdt0001nt_alv-id_mod_fiscal EQ zif_doc_eletronico=>at_st_model_nfe.
    IF lc_chave_nfe_ant IS INITIAL OR lc_chave_nfe_ant NE zde_zsdt0001nt_alv-nr_chave_nfe AND zde_zsdt0001nt_alv-nr_chave_nfe IS NOT INITIAL.
      lc_chave_nfe_ant = zde_zsdt0001nt_alv-nr_chave_nfe.

      TRY .
          zcl_nfe_inbound=>danfe( EXPORTING i_chave_nfe = zde_zsdt0001nt_alv-nr_chave_nfe i_chamar_browser = abap_false IMPORTING e_url = DATA(e_url) ).

          IF html_control_0101 IS INITIAL.
            CREATE OBJECT html_control_0101
              EXPORTING
                parent = docking_0100.
          ENDIF.

          DATA: lc_url TYPE c LENGTH 250.
          lc_url = e_url.
          html_control_0101->show_url(
            EXPORTING
              url                    = lc_url
            EXCEPTIONS
              cntl_error             = 1
              cnht_error_not_allowed = 2
              cnht_error_parameter   = 3
              dp_error_general       = 4
              OTHERS                 = 5
          ).

        CATCH zcx_nfe_inbound_exception INTO DATA(zcx_nfe_inbound_exception).
          zcx_nfe_inbound_exception->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          ck_limpar = abap_true.
      ENDTRY.

    ELSEIF zde_zsdt0001nt_alv-nr_chave_nfe IS INITIAL.
      ck_limpar = abap_true.
    ENDIF.
  ELSE.
    ck_limpar = abap_true.
  ENDIF.

  IF ck_limpar EQ abap_true.
    IF html_control_0101 IS NOT INITIAL.
      html_control_0101->free(
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3
      ).
    ENDIF.
    CLEAR: html_control_0101.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_BLOCOS_FARDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vincular_blocos_fardos .

  CHECK objeto->at_manutencao EQ abap_false.

  CALL METHOD ctl_alv_0101b->get_selected_rows
    IMPORTING
      et_index_rows = DATA(et_index_rows).

  DELETE et_index_rows WHERE rowtype IS NOT INITIAL.

  IF et_index_rows[] IS INITIAL.
    MESSAGE 'Deve ser Selecionado uma Ordem de Venda' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE et_index_rows INDEX 1 INTO DATA(wa_index_rows).
  READ TABLE it_ordens_venda_alv INTO wa_ordens_venda_alv INDEX wa_index_rows-index.

  PERFORM gerar_saldo_de_blocos_fardos.

  CLEAR: ok_code, ck_add_bloco.
  CALL SCREEN 8005 STARTING AT 40 01.

  IF ck_add_bloco EQ abap_true.
    "OK_CODE_SUPPRESS   = 'ADDTAKE'.
    "OK_SUPPRESS_DIALOG = ABAP_TRUE.
  ENDIF.

  CLEAR: wa_ordens_venda_alv.

  PERFORM atualizar_dados_tela.

  wa_stable_0101b-row = abap_true.
  wa_stable_0101b-col = abap_true.
  CALL METHOD ctl_alv_0101b->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0101b.

  wa_stable_0101a-row = abap_true.
  wa_stable_0101a-col = abap_true.
  CALL METHOD ctl_alv_0101a->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0101a.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_ORDEM_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eliminar_ordem_venda .

  CHECK objeto->at_manutencao EQ abap_false.

  CALL METHOD ctl_alv_0101b->get_selected_rows
    IMPORTING
      et_index_rows = DATA(et_index_rows).

  DELETE et_index_rows WHERE rowtype IS NOT INITIAL.

  LOOP AT et_index_rows INTO DATA(wa_index_rows).
    TRY .
        READ TABLE it_ordens_venda_alv INDEX wa_index_rows-index INTO wa_ordens_venda_alv.
        objeto->set_excluir_ordem_venda( i_vbeln = wa_ordens_venda_alv-nr_ordem_venda ).
      CATCH zcx_carga.    "
    ENDTRY.
  ENDLOOP.

  PERFORM atualizar_dados_tela.

  wa_stable_0101b-row = abap_true.
  wa_stable_0101b-col = abap_true.
  CALL METHOD ctl_alv_0101b->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0101b.

  wa_stable_0101a-row = abap_true.
  wa_stable_0101a-col = abap_true.
  CALL METHOD ctl_alv_0101a->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0101a.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_ORDEM_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_ordem_venda .

  DATA: e_algodao_alv TYPE  zsdt0001ov_algodao_alv_t,
        i_charg       TYPE  charg_d,
        l_message     TYPE char255.

  MOVE psafra TO i_charg.

  CHECK objeto->at_manutencao EQ abap_false.

  CALL FUNCTION 'ZMF_PESQ_INSTRU_OV_ALGO'
    EXPORTING
      i_vstel       = pfilia
      i_charg       = i_charg
      i_fundo       = abap_false
    IMPORTING
      e_algodao_alv = e_algodao_alv.

  CHECK e_algodao_alv[] IS NOT INITIAL.

  DATA: i_ordem_venda	TYPE zde_zsdt0001ov_alv.

  LOOP AT e_algodao_alv INTO DATA(wa_algodao_alv).

    CLEAR: i_ordem_venda.
    i_ordem_venda-nr_ordem_venda = wa_algodao_alv-nr_ordem_venda.
    i_ordem_venda-qt_fardos      = wa_algodao_alv-qt_fardos.

    TRY .
        objeto->add_ordem_venda( EXPORTING i_ordem_venda = i_ordem_venda
             )->set_volume_ordem_venda( EXPORTING i_vbeln = i_ordem_venda-nr_ordem_venda i_volume = CONV #( i_ordem_venda-qt_fardos )
             ).
      CATCH zcx_carga INTO ex_carga.
*-CS2022000332-#78064-07.06.2022-JT-inicio
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = ex_carga->msgid
            lang      = sy-langu
            no        = ex_carga->msgno
            v1        = ex_carga->msgv1
            v2        = ex_carga->msgv2
            v3        = ex_carga->msgv3
            v4        = ex_carga->msgv4
          IMPORTING
            msg       = l_message
          EXCEPTIONS
            not_found = 01
            OTHERS    = 02.

        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
          EXPORTING
            titel        = 'Mensagem'
            textline1    = l_message(50)
            textline2    = l_message+50(50)
            textline3    = l_message+100(50)
            start_column = 25
            start_row    = 6.

        EXIT.
*       ex_carga->published_erro( EXPORTING i_msgty = 'W' i_msgty_display = 'E' ).
*-CS2022000332-#78064-07.06.2022-JT-fim
    ENDTRY.

  ENDLOOP.

  PERFORM atualizar_dados_tela.

  wa_stable_0101b-row = abap_true.
  wa_stable_0101b-col = abap_true.
  CALL METHOD ctl_alv_0101b->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0101b.

  wa_stable_0101a-row = abap_true.
  wa_stable_0101a-col = abap_true.
  CALL METHOD ctl_alv_0101a->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0101a.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_SORT_0101A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_sort_0101a .

  DATA: wa_sort_0101a TYPE lvc_s_sort.

  CLEAR: it_sort_0101a[], it_sort_0101a.
  wa_sort_0101a-spos       = 1.     "first sorting key
  wa_sort_0101a-fieldname  = 'DS_PONTO_C'. "fieldname for sort
  wa_sort_0101a-up         = ' '. "sort ascending
  wa_sort_0101a-subtot     = ' '. "do subtotal
  wa_sort_0101a-obligatory = 'X'. "sort is obligatory
  INSERT wa_sort_0101a INTO TABLE it_sort_0101a. "insert to sort table

  wa_sort_0101a-spos       = 2.     "first sorting key
  wa_sort_0101a-fieldname  = 'NR_ORDEM_VENDA'. "fieldname for sort
  wa_sort_0101a-up         = ' '. "sort ascending
  wa_sort_0101a-subtot     = 'X'. "do subtotal
  wa_sort_0101a-obligatory = 'X'. "sort is obligatory
  INSERT wa_sort_0101a INTO TABLE it_sort_0101a. "insert to sort table

ENDFORM.
