*&---------------------------------------------------------------------*
*& Report  ZFIR056
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zlesr0092.

TYPE-POOLS: slis.

RANGES:  p_bukrs    FOR ekko-bukrs,  "Empresa
         p_werks    FOR zsdt_depara_cen-centro_real,  "Filial
         p_charg    FOR eket-charg,  "Safra/Lote
         p_lifnr    FOR ekko-lifnr,  "Fornecedor
         p_ebeln    FOR ekko-ebeln, "Pedido
         p_xblnr    FOR ekes-xblnr. "Nro NFe  "*-IR 185421-03.07.2024-#144903-JT

RANGES:  it_bukrs    FOR ekko-bukrs,  "Empresa
         it_werks    FOR zsdt_depara_cen-centro_real,  "Filial
         it_charg    FOR eket-charg,  "Safra/Lote
         it_lifnr    FOR ekko-lifnr,  "Fornecedor
         it_ebeln    FOR ekko-ebeln, "Pedido
         it_xblnr    FOR ekes-xblnr. "Nro NFe  "*-IR 185421-03.07.2024-#144903-JT

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*-------------------------------------------------------------------
* Alv's e Custom Container.
*------------------------------------------------------------------
DATA: wa_cont_ped         TYPE REF TO cl_gui_custom_container,
      wa_alv_ped          TYPE REF TO cl_gui_alv_grid,

      wa_cont_aviso_rec   TYPE REF TO cl_gui_custom_container,
      wa_alv_aviso_rec    TYPE REF TO cl_gui_alv_grid,

      wa_cont_dados_aviso TYPE REF TO cl_gui_custom_container,
      wa_alv_dados_aviso  TYPE REF TO cl_gui_alv_grid,

      wa_cont_dados_veic  TYPE REF TO cl_gui_custom_container,
      wa_alv_dados_veic   TYPE REF TO cl_gui_alv_grid.

DATA: wa_stable   TYPE lvc_s_stbl,
      wa_layout   TYPE lvc_s_layo,
      wa_variante TYPE disvariant,
      it_fcat     TYPE lvc_t_fcat,
      wa_fcat     TYPE lvc_s_fcat.

DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

CLASS: lcl_alv_toolbar             DEFINITION DEFERRED,
       lcl_alv_toolbar_aviso       DEFINITION DEFERRED,
       lcl_alv_toolbar_dados_aviso DEFINITION DEFERRED,
       lcl_alv_toolbar_dados_veic  DEFINITION DEFERRED.

DATA: ty_toolbar   TYPE stb_button,
      wl_desactive.

DATA: c_alv_toolbarmanager       TYPE REF TO cl_alv_grid_toolbar_manager,
      obj_toolbar                TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager_aviso TYPE REF TO cl_alv_grid_toolbar_manager,
      obj_toolbar_aviso          TYPE REF TO lcl_alv_toolbar_aviso,
      c_alv_toolbar_dados_aviso  TYPE REF TO cl_alv_grid_toolbar_manager,
      obj_toolbar_dados_aviso    TYPE REF TO lcl_alv_toolbar_dados_aviso,
      c_alv_toolbar_dados_veic   TYPE REF TO cl_alv_grid_toolbar_manager,
      obj_toolbar_dados_veic     TYPE REF TO lcl_alv_toolbar_dados_veic.

DATA: vg_tipo_frete(3),
      tg_selectedcell  TYPE lvc_t_cell,
      wg_selectedcell  TYPE lvc_s_cell.

*-CS2022001149-29.03.2023-#103469-JT-inicio
DATA: vg_eindt_ini TYPE ekes-eindt,
      vg_eindt_fim TYPE ekes-eindt,
      t_dynpfields TYPE TABLE OF dynpread,
      w_dynpfields TYPE dynpread.
RANGES: r_eindt     FOR ekes-eindt.
*-CS2022001149-29.03.2023-#103469-JT-fim

DATA: r_gerar,
      r_atrib.

INITIALIZATION.

  CALL SCREEN 0100.

  INCLUDE zlesr0092_top.
  INCLUDE zlesr0092_class.
  INCLUDE zlesr0092_form.



*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: msg_exibir TYPE string.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'SEA'.

      PERFORM limpar_dados.
      PERFORM config_ranges.

      IF p_bukrs-low IS INITIAL.
        MESSAGE 'Empresa é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF p_werks-low IS INITIAL.
        MESSAGE 'Filial é um campo obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( p_charg-low  IS INITIAL ) AND
         ( p_lifnr-low  IS INITIAL ) AND
         ( p_ebeln-low  IS INITIAL ).
        MESSAGE 'Informe pelo menos um dos campos a seguir: Safra/Lote - Fornecedor - Pedido' TYPE 'S'.
        EXIT.
      ENDIF.

      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
        ID 'WERKS' FIELD  p_werks-low
        ID 'ACTVT' FIELD '03'.    "Alteração

      IF sy-subrc IS NOT INITIAL.
        CONCATENATE 'Usuário sem permissão para o centro:' p_werks-low
                INTO msg_exibir SEPARATED BY space.

        MESSAGE msg_exibir  TYPE 'S'.
        SET CURSOR FIELD 'P_WERKS'.
        EXIT.
      ENDIF.


      PERFORM seleciona_dados.
      PERFORM processa_dados.
      PERFORM imprimir_dados.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'TB0101'.

  PERFORM atualiza_aviso USING wa_saida.

ENDMODULE.                 " STATUS_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0101  INPUT

*-CS2022001149-29.03.2023-#103469-JT-inicio
*&---------------------------------------------------------------------*
*&      Module  F_VALIDAR_DATA
*&---------------------------------------------------------------------*
MODULE f_validar_data INPUT.

  IF   vg_eindt_ini       IS INITIAL AND vg_eindt_fim IS NOT INITIAL.
    MESSAGE e024(sd) WITH 'Intervalo de Datas Incorreto!'.
  ELSEIF vg_eindt_ini > vg_eindt_fim AND vg_eindt_fim IS NOT INITIAL.
    MESSAGE e024(sd) WITH 'Intervalo de Datas Incorreto!'.
  ENDIF.

ENDMODULE.
*-CS2022001149-29.03.2023-#103469-JT-fim

*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.

  IF r_atrib IS NOT INITIAL.
    SET PF-STATUS 'PF0109'.
  ELSE.
    SET PF-STATUS 'PF0102'.
  ENDIF.

  SET TITLEBAR 'Dados Aviso Recimento'.

  IF aviso_dynnr_000 IS INITIAL.
    aviso_dynnr_000 = aviso_rec_0103.
  ENDIF.

  vg_data_aviso = sy-datum.

ENDMODULE.                 " STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.

  DATA(_ok_code_tmp) = ok_code_0102. "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>

  CASE ok_code_0102.

    WHEN av_tb01.
      aviso_dynnr_000 = aviso_rec_0103.
      CLEAR: ok_code_0102.
    WHEN av_tb02.
      aviso_dynnr_000 = aviso_rec_0105.
      CLEAR: ok_code_0102.
    WHEN av_tb03.
      PERFORM preenche_dados_nf.
      aviso_dynnr_000 = aviso_rec_0104.
      CLEAR: ok_code_0102.
    WHEN av_tb04.
      aviso_dynnr_000 = aviso_rec_0106.
      CLEAR: ok_code_0102.
    WHEN 'GER_AVISO'.
      PERFORM gerar_aviso_recebimento.
      CLEAR: ok_code_0102.
    WHEN 'ATR_AVISO'.
      PERFORM atribuir_aviso_recebimento.
      CLEAR: ok_code_0102.
    WHEN 'CANCELAR'.
      LEAVE TO SCREEN 0.
      CLEAR: ok_code_0102.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0102  INPUT

MODULE tab_active_av_rec INPUT.

  CASE sy-ucomm.
    WHEN av_tb01.
      info_aviso_tab-activetab = av_tb01.
    WHEN av_tb02.
      info_aviso_tab-activetab = av_tb02.
    WHEN av_tb03.
      info_aviso_tab-activetab = av_tb03.
    WHEN av_tb04.
      info_aviso_tab-activetab = av_tb04.
  ENDCASE.

ENDMODULE.                 " TAB_ACTIVE_GET  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0104 OUTPUT.

  PERFORM atualiza_campos_nf.
  PERFORM atualiza_screen_0104.

ENDMODULE.                 " STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0105 OUTPUT.

  PERFORM atualiza_campos_parc.
  PERFORM atualiza_screen_0105.

ENDMODULE.                 " STATUS_0105  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0106 OUTPUT.
  PERFORM atualiza_campos_modal.
ENDMODULE.                 " STATUS_0106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0106 OUTPUT.

  IF wa_cont_dados_veic IS INITIAL.

    CLEAR: wa_layout, wa_variante.

    PERFORM criar_alv_dados_veic.

    wa_layout-zebra      = abap_true.
    wa_layout-stylefname = 'FIELD_STYLE'.
    wa_variante-report  = sy-repid.

    CREATE OBJECT wa_cont_dados_veic
      EXPORTING
        container_name = 'CC_DADOS_VEIC'.

    CREATE OBJECT wa_alv_dados_veic
      EXPORTING
        i_parent = wa_cont_dados_veic.

    "Create toolbar for grid.
    CREATE OBJECT obj_toolbar_dados_veic
      EXPORTING
        io_alv_grid = wa_alv_dados_veic.


    "Register event handler
    SET HANDLER: obj_toolbar_dados_veic->on_toolbar                FOR wa_alv_dados_veic,
                 obj_toolbar_dados_veic->handle_user_command       FOR wa_alv_dados_veic.
    "GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR WA_ALV.

    "Excluir Buttons Toolbar
    REFRESH: it_exclude_fcode.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD wa_alv_dados_veic->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = wa_variante
        i_save               = 'X'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_outtab            = it_dados_veic
        it_fieldcatalog      = it_fcat.

    CALL METHOD wa_alv_dados_veic->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler_0106=>on_data_changed          FOR wa_alv_dados_veic,
                 lcl_event_handler_0106=>on_data_changed_finished FOR wa_alv_dados_veic,
                 lcl_event_handler_0106=>user_command             FOR wa_alv_dados_veic.

    SELECT SINGLE *
    FROM lfa1
    INTO @DATA(wlfa1)
    WHERE lifnr = @wa_dados_transp-agente_frete.

    IF wlfa1-ktokk = 'ZFIC'.
      vg_tipo_frete = 'CIF'.
    ELSE.
      vg_tipo_frete = 'CPT'.
    ENDIF.

    IF r_atrib IS INITIAL.

      IF vg_tipo_frete NE 'CPT'.
        CALL METHOD wa_alv_dados_veic->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.
      ELSE.
        CALL METHOD wa_alv_dados_veic->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.
      ENDIF.

    ELSE.

      CALL METHOD wa_alv_dados_veic->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.

    ENDIF.

  ELSE.

    SELECT SINGLE *
       FROM lfa1
       INTO wlfa1
       WHERE lifnr = wa_dados_transp-agente_frete.

    IF vg_tipo_frete <> wlfa1-ktokk.

      PERFORM criar_alv_dados_veic.

      IF wlfa1-ktokk = 'ZFIC'.
        vg_tipo_frete = 'CIF'.
      ELSE.
        vg_tipo_frete = 'CPT'.
      ENDIF.

      IF r_atrib IS INITIAL.

        IF vg_tipo_frete NE 'CPT'.
          CALL METHOD wa_alv_dados_veic->set_ready_for_input
            EXPORTING
              i_ready_for_input = 0.
        ELSE.
          CALL METHOD wa_alv_dados_veic->set_ready_for_input
            EXPORTING
              i_ready_for_input = 1.
        ENDIF.

      ELSE.

        CALL METHOD wa_alv_dados_veic->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.

      ENDIF.

    ENDIF.

    CALL METHOD wa_alv_dados_veic->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDMODULE.                 " CRIAR_OBJETOS_0106  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0107  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0107 OUTPUT.
  SET PF-STATUS 'PF0107'.
*  SET TITLEBAR 'xxx'.

  IF vg_det_peso1 IS INITIAL.
    PERFORM atualiza_campos_peso1.
  ELSEIF vg_det_peso2 IS INITIAL.
    PERFORM atualiza_campos_peso2.
  ENDIF.

ENDMODULE.                 " STATUS_0107  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0107  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0107 INPUT.
  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF vg_det_peso1 IS INITIAL. " Se não informou peso e tara pela 1ª vez ainda

        IF wa_qtde_aviso-peso_bruto1 <= 0.
          MESSAGE 'Favor Informar Peso!' TYPE 'W'.
          EXIT.
        ENDIF.

        IF wa_qtde_aviso-peso_tara1 <= 0.
          MESSAGE 'Favor Informar Tara!' TYPE 'W'.
          EXIT.
        ENDIF.

        vg_det_peso1 = 'X'.

      ELSEIF vg_det_peso2 IS INITIAL. " Se não informou peso e tara pela 2ª vez ainda

        IF wa_qtde_aviso-peso_bruto2 <= 0.
          MESSAGE 'Favor Informar Peso!' TYPE 'W'.
          EXIT.
        ENDIF.

        IF wa_qtde_aviso-peso_tara2 <= 0.
          MESSAGE 'Favor Informar Tara!' TYPE 'W'.
          EXIT.
        ENDIF.

        vg_det_peso2 = 'X'.

        IF ( vg_det_peso1 IS NOT INITIAL ) AND
           ( vg_det_peso2 IS NOT INITIAL ). " Se já informou peso e tara nas duas vez

          wa_qtde_aviso-peso_total1 = wa_qtde_aviso-peso_bruto1 - wa_qtde_aviso-peso_tara1.
          wa_qtde_aviso-peso_total2 = wa_qtde_aviso-peso_bruto2 - wa_qtde_aviso-peso_tara2.

          IF wa_qtde_aviso-peso_bruto1 <>  wa_qtde_aviso-peso_bruto2 .
            CLEAR: vg_det_peso1, vg_det_peso2, wa_qtde_aviso.
            MESSAGE 'Pesos brutos informados não conferem!' TYPE 'W'.
            EXIT.
          ENDIF.

          IF wa_qtde_aviso-peso_tara1 <>  wa_qtde_aviso-peso_tara2.
            CLEAR: vg_det_peso1, vg_det_peso2, wa_qtde_aviso.
            MESSAGE 'Taras informadas não conferem!' TYPE 'W'.
            EXIT.
          ENDIF.

          IF wa_qtde_aviso-peso_total1 <>  wa_qtde_aviso-peso_total2.
            CLEAR: vg_det_peso1, vg_det_peso2, wa_qtde_aviso.
            MESSAGE 'Totais não conferem!' TYPE 'W'.
            EXIT.
          ENDIF.

          IF wa_qtde_aviso-peso_total1 < 0.
            CLEAR: vg_det_peso1, vg_det_peso2, wa_qtde_aviso.
            MESSAGE 'Peso Tara não pode ser superior ao Peso Bruto!' TYPE 'W'.
            EXIT.
          ENDIF.

          " Atualiza Quantidade do Aviso
          CLEAR: it_sel_rows[], wa_sel_rows.

          CALL METHOD wa_alv_dados_aviso->get_selected_rows
            IMPORTING
              et_index_rows = it_sel_rows.

          CHECK it_sel_rows IS NOT INITIAL.

          READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

          READ TABLE it_dados_aviso INTO wa_dados_aviso INDEX wa_sel_rows-index.

          wa_dados_aviso-qtde_aviso = wa_qtde_aviso-peso_total1.
          wa_dados_aviso-peso_bruto = wa_qtde_aviso-peso_bruto1.
          wa_dados_aviso-peso_tara  = wa_qtde_aviso-peso_tara1.

          MODIFY it_dados_aviso FROM wa_dados_aviso INDEX wa_sel_rows-index.

          CALL METHOD wa_alv_dados_aviso->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

          LEAVE TO SCREEN 0.

        ENDIF.

      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0107  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0106  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0106 INPUT.

  CASE sy-ucomm.
    WHEN 'ATUALIZA_MODAL'.

      PERFORM atualiza_campos_modal.
      CLEAR: sy-ucomm.

    WHEN 'CLEAR_MODAL'.

      IF vg_view_transp IS NOT INITIAL.
        MESSAGE 'Modo de visualização! Operação não permitida!' TYPE  'S'.
        EXIT.
      ENDIF.

      REFRESH: it_dados_veic[].

      CLEAR: wa_dados_transp-motorista,
             wa_dados_transp-safra_ordem_car,
             wa_dados_transp-nro_ordem_car.

      CALL METHOD wa_alv_dados_veic->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      CLEAR: sy-ucomm.

    WHEN 'NTER'.
      PERFORM atualiza_campos_modal.
      CLEAR: sy-ucomm.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0106  INPUT
