*&---------------------------------------------------------------------*
*& Report  ZSDR0103
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0103.

TYPE-POOLS: slis.

TABLES: zsdt0192, zsdt0193, zsdt0194, zsdt0196.

TYPES: BEGIN OF ty_saida,
         matnr             TYPE zsdt0192-matnr,
         maktx             TYPE makt-maktx,
         total_estq        TYPE zsdt0192-qtd_n,
         ov_carteira       TYPE zsdt0192-qtd_n,
         entrega_efetivada TYPE zsdt0192-qtd_n,
         saldo_entrega     TYPE zsdt0192-qtd_n,
         estoque_geren     TYPE zsdt0192-qtd_n,
         mtbez             TYPE t134t-mtbez,
         grupo             TYPE zsdt0196-grupo,
         mtart             TYPE mara-mtart,
         cor               TYPE lvc_t_scol,
         celltab           TYPE lvc_t_styl,
       END OF ty_saida.

TYPES: BEGIN OF ty_saida_0101,
         ebeln           TYPE zsdt0193-ebeln,
         ebelp           TYPE zsdt0193-ebelp,
         menge           TYPE ekpo-menge, "QUANTIDADE DO PEDIDO
         qtd_fatur       TYPE ekpo-menge,
         qtd_saldo       TYPE ekpo-menge,
         meins           TYPE ekpo-meins, "Unidade de medida do pedido
         matnr           TYPE zsdt0192-matnr,
         aedar           TYPE ekko-aedat,
         lifnr           TYPE ekko-lifnr,
         werks           TYPE ekpo-werks,
         netpr           TYPE ekpo-netpr,
         bprme           TYPE ekpo-bprme,
         vlr_item_pedido TYPE komp-netwr,
         tp_ped          TYPE zsdt0193-tp_ped,
         name1           TYPE lfa1-name1,
         lgort           TYPE ekpo-lgort, "--Deposito
         lgobe           TYPE t001l-lgobe, "--Desc Deposito
         verkf           TYPE ekko-verkf, "--Navio
         qtd_safra       TYPE ekpo-menge,
         qtd_safrinha    TYPE ekpo-menge,
         cor             TYPE lvc_t_scol,
         celltab         TYPE lvc_t_styl,
       END OF ty_saida_0101.

TYPES: BEGIN OF ty_saida_0102,
         ebeln    TYPE  zsdt0194-ebeln,
         ebelp    TYPE  zsdt0194-ebelp,
         safra    TYPE  zsdt0194-safra,
         tp_safra TYPE  zsdt0194-tp_safra,
         qtd      TYPE  zsdt0194-qtd,
         celltab  TYPE lvc_t_styl,
       END OF ty_saida_0102.

TYPES: BEGIN OF ty_matnr,
         matnr TYPE zsdt0192-matnr,
       END OF ty_matnr.

TYPES: BEGIN OF ty_aux,
         matnr TYPE zsdt0192-matnr,
         qtd_t TYPE zsdt0192-qtd_t,
       END OF ty_aux.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
TYPES: BEGIN OF ty_saida_0110,
         matnr_pa TYPE matnr,
         maktx    TYPE ojtxb,
         stlnr    TYPE stnum,
         werks    TYPE werks_d,
         bmeng    TYPE basmn,
         bmein    TYPE basme,
         matnr_mp TYPE matnr,
         maktx_mp TYPE maktx,
         menge    TYPE kmpmg,
         meins    TYPE kmpme,
       END OF ty_saida_0110.

TYPES: BEGIN OF ty_saida_0111,
         item_no    TYPE sposn,
         compone    TYPE idnrk,
         maktx      TYPE maktx,
         comp_qty   TYPE kmpmg_bi,
         comp_unit  TYPE kmpme,
         valid_from TYPE datuv_bi,
         valid_to   TYPE datub_bi,
       END OF ty_saida_0111.


TYPES: BEGIN OF ty_saida_0120,
         safra     TYPE ajahr,
         cultura   TYPE acc_txtlg,
         cliente   TYPE name1,
         municipio TYPE ort01,
         vbeln     TYPE vbeln,
         posnr     TYPE posnr,
         matnr     TYPE matnr,
         maktx     TYPE ojtxb,
         werks     TYPE werks_d,
         qtd_ov    TYPE zqtde_t,
         qtd_sol   TYPE zqtde_t, " -US 136655-18-06-2024-#136655-RJF
         qtd_sald	 TYPE zqtde_t,
         um_ov     TYPE vrkme,
         matnr_mp  TYPE matnr,
         maktx_mp  TYPE maktx,
         fator_mp	 TYPE zde_percentual_10_4,
         qtd_cons	 TYPE zqtde_t,
         qtd_ac	   TYPE zqtde_t,
       END OF ty_saida_0120.

TYPES: BEGIN OF ty_cli,
         kunnr TYPE kunnr,
         name1 TYPE name1_gp,
         ort01 TYPE ort01,
       END OF ty_cli.
* Fim - CS2019001220 - Sara Oikawa - Jun/2020

DATA: it_saida      TYPE TABLE OF ty_saida,
      wa_saida      TYPE ty_saida,
      it_saida_0101 TYPE TABLE OF ty_saida_0101,
      wa_saida_0101 TYPE ty_saida_0101,
      it_sd_0101    TYPE TABLE OF ty_saida_0101,
      wa_sd_0101    TYPE ty_saida_0101,
      it_saida_0102 TYPE TABLE OF ty_saida_0102,
      wa_saida_0102 TYPE ty_saida_0102,
      it_saida_02   TYPE TABLE OF ty_saida_0102,
      wa_saida_02   TYPE ty_saida_0102,
      it_aux_0101   TYPE TABLE OF ty_saida_0101,
      wa_aux_0101   TYPE ty_saida_0101,
      it_zsdt0192   TYPE TABLE OF zsdt0192,
      wa_zsdt0192   TYPE zsdt0192,
      it_compra     TYPE TABLE OF zsdt0192,
      wa_compra     TYPE zsdt0192,
      it_compra_aux TYPE TABLE OF ty_aux,
      wa_compra_aux TYPE ty_aux,
      it_venda      TYPE TABLE OF zsdt0192,
      wa_venda      TYPE zsdt0192,
      it_zsdt0193   TYPE TABLE OF zsdt0193,
      wa_zsdt0193   TYPE zsdt0193,
      it_193_aux    TYPE TABLE OF zsdt0193,
      wa_193_aux    TYPE  zsdt0193,
      it_zsdt0194   TYPE TABLE OF zsdt0194,
      wa_zsdt0194   TYPE zsdt0194,
      it_zsdt0196   TYPE TABLE OF zsdt0196,
      wa_zsdt0196   TYPE zsdt0196,
      it_grupo      TYPE TABLE OF zsdt0196,
      wa_grupo      TYPE zsdt0196,
      it_excluir    TYPE TABLE OF zsdt0196,
      wa_excluir    TYPE  zsdt0196,
      it_z194       TYPE TABLE OF zsdt0194,
      wa_z194       TYPE zsdt0194,
      it_zs194      TYPE TABLE OF zsdt0194,
      wa_zs194      TYPE zsdt0194,
      it_zs194_aux  TYPE TABLE OF zsdt0194,
      wa_zs194_aux  TYPE zsdt0194,
      it_makt       TYPE TABLE OF makt,
      wa_makt       TYPE makt,
      it_materiais  TYPE TABLE OF ty_matnr,
      wa_materiais  TYPE ty_matnr.

DATA: it_ov         TYPE TABLE OF zsdt0256,
      wa_ov         TYPE zsdt0256,

      it_zsdt0038   TYPE TABLE OF zsdt0038,
      wa_zsdt0038   TYPE zsdt0038,

      it_cli        TYPE TABLE OF ty_cli,
      wa_cli        TYPE ty_cli,

      it_saida_0110 TYPE TABLE OF ty_saida_0110,
      wa_saida_0110 TYPE ty_saida_0110,

      it_saida_0111 TYPE TABLE OF ty_saida_0111,
      wa_saida_0111 TYPE ty_saida_0111,

      it_saida_0120 TYPE TABLE OF ty_saida_0120,
      wa_saida_0120 TYPE ty_saida_0120,

      it_zsdt0286   TYPE TABLE OF zsdt0286,
      wa_zsdt0286   TYPE zsdt0286.

DATA: g_grid                 TYPE REF TO cl_gui_alv_grid,
      g_grid_01              TYPE REF TO cl_gui_alv_grid,
      g_grid_02              TYPE REF TO cl_gui_alv_grid,
      g_grid_03              TYPE REF TO cl_gui_alv_grid,
      g_grid_04              TYPE REF TO cl_gui_alv_grid,
      g_grid_05              TYPE REF TO cl_gui_alv_grid,
      g_grid_cad_user        TYPE REF TO cl_gui_alv_grid,
      g_custom_container     TYPE REF TO cl_gui_custom_container,
      g_custom_container_01  TYPE REF TO cl_gui_custom_container,
      g_custom_container_02  TYPE REF TO cl_gui_custom_container,
      g_custom_container_03  TYPE REF TO cl_gui_custom_container,
      g_custom_container_04  TYPE REF TO cl_gui_custom_container,
      g_custom_container_05  TYPE REF TO cl_gui_custom_container,
      g_custom_container_cad TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager   TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function            TYPE ui_functions,
      wl_function            LIKE tl_function WITH HEADER LINE,
      tg_selectedrow         TYPE lvc_t_row,
      wg_selectedrow         TYPE lvc_s_row,
      dg_splitter_1          TYPE REF TO cl_gui_splitter_container,
      dg_splitter_1_cad      TYPE REF TO cl_gui_splitter_container,
      dg_parent_1            TYPE REF TO cl_gui_container,
      dg_parent_1_cad        TYPE REF TO cl_gui_container,
      dg_splitter_2          TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2_cad      TYPE REF TO cl_gui_splitter_container,
      dg_parent_2            TYPE REF TO cl_gui_container,
      dg_parent_2_cad        TYPE REF TO cl_gui_container,
      dg_parent_2a           TYPE REF TO cl_gui_container,
      dg_parent_2a_cad       TYPE REF TO cl_gui_container,
      dg_parent_alv          TYPE REF TO cl_gui_container,
      dg_parent_alv_cad      TYPE REF TO cl_gui_container,
      docking                TYPE REF TO cl_gui_docking_container,
      document               TYPE REF TO cl_dd_document,
      dg_dyndoc_id           TYPE REF TO cl_dd_document,
      table_element          TYPE REF TO cl_dd_table_element,
      column                 TYPE REF TO cl_dd_area,
      table_element2         TYPE REF TO cl_dd_table_element,
      column_1               TYPE REF TO cl_dd_area,
      column_2               TYPE REF TO cl_dd_area,
      column_3               TYPE REF TO cl_dd_area.

DATA: it_fcat       TYPE TABLE OF lvc_s_fcat,
      wl_fcat       TYPE lvc_s_fcat,
      it_fcat_01    TYPE TABLE OF lvc_s_fcat,
      wl_fcat_01    TYPE lvc_s_fcat,
      it_fcat_02    TYPE TABLE OF lvc_s_fcat,
      wl_fcat_02    TYPE lvc_s_fcat,
      it_fcat_cad   TYPE TABLE OF lvc_s_fcat,
      wl_fcat_cad   TYPE lvc_s_fcat,

      it_fcat_03    TYPE TABLE OF lvc_s_fcat,

      wa_layout     TYPE lvc_s_layo,
      wa_layout_01  TYPE lvc_s_layo,
      wa_layout_02  TYPE lvc_s_layo,
      wa_layout_03  TYPE lvc_s_layo,
      wa_layout_cad TYPE lvc_s_layo,

      gt_estilo_02  TYPE lvc_t_styl,
      t_sort        TYPE lvc_t_sort,
      fs_sort       TYPE lvc_s_sort,
      t_sort_cad    TYPE lvc_t_sort,
      fs_sort_cad   TYPE lvc_s_sort,

      ty_toolbar    TYPE stb_button,
      gt_estilo     TYPE lvc_t_styl.

DATA: lt_coltab    TYPE lvc_t_scol,
      ls_col       TYPE lvc_s_scol,
      lt_coltab_01 TYPE lvc_t_scol,
      ls_col_01    TYPE lvc_s_scol,
      wa_stable    TYPE lvc_s_stbl VALUE 'XX',
      wa_stable_01 TYPE lvc_s_stbl VALUE 'XX',
      wa_stable_02 TYPE lvc_s_stbl VALUE 'XX'.

DATA: click_alv01 TYPE c.
DATA: edit TYPE c.
DATA: tp_safra TYPE  zsdt0192-tp_safra.
DATA visao_total TYPE c.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_werks  FOR zsdt0192-werks,
                  p_gsafra FOR zsdt0192-safra.
*                P_DATA   FOR ZSDT0192-DATA  OBLIGATORY.
  PARAMETERS:      p_data   LIKE zsdt0192-data.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_safra  RADIOBUTTON GROUP g1,
              p_safrin RADIOBUTTON GROUP g1,
              p_todos  RADIOBUTTON GROUP g1,
              p_cadast RADIOBUTTON GROUP g1 MODIF ID g2.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.

  SELECT
      'I' AS sign,
      'EQ' AS option,
      valfrom AS low,
      valfrom AS high
   INTO TABLE @DATA(r_set_user)
   FROM setleaf
   WHERE setname = 'ZSDT0151_USER' AND valfrom EQ @sy-uname
   ORDER BY low ASCENDING.

  IF sy-subrc = 0.

    PERFORM modifica_tela_cadastro USING abap_true.
  ELSE.
    PERFORM modifica_tela_cadastro USING abap_false.
  ENDIF.


INITIALIZATION.

  SELECT * FROM zsdt0192 INTO TABLE @DATA(it_t192).

  SORT it_t192 BY data DESCENDING.

  READ TABLE it_t192 INTO DATA(w192) INDEX 1.
  IF sy-subrc = 0.
*    P_DATA-SIGN    = 'I'.
*    P_DATA-OPTION  = 'EQ'.
*    P_DATA-LOW     = W192-DATA.
*    P_DATA-HIGH    = W192-DATA.
*    APPEND P_DATA.
    p_data = w192-data.
  ENDIF.

  edit = abap_false.


START-OF-SELECTION.

  IF p_cadast IS INITIAL.
    IF p_werks IS INITIAL.
      MESSAGE 'A filial deve ser informada!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF p_gsafra IS INITIAL.
      MESSAGE 'O Ano Sara deve ser informado!' TYPE  'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CLEAR tp_safra.

    IF p_safra IS NOT INITIAL.
      tp_safra = 1.
    ELSEIF p_safrin IS NOT INITIAL .
      tp_safra = 2.
    ENDIF.

    PERFORM z_busca_dados.
    CALL SCREEN 0100.
  ELSE.
    "PERFORM z_busca_dados_cadastro.
    CALL TRANSACTION 'ZSDT0190'.
  ENDIF.


END-OF-SELECTION.


CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS: zm_handle_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_column e_row es_row_no.

    METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD zm_handle_click.
    PERFORM z_handler_double_click USING e_row e_column es_row_no.
  ENDMETHOD.

*Início - CS2019001220 - Sara Oikawa - Jun/2020
  METHOD catch_hotspot.
    PERFORM z_handler_click_hotspot USING e_row_id e_column_id es_row_no.
  ENDMETHOD.
*Fim - CS2019001220 - Sara Oikawa - Jun/2020

ENDCLASS.


CLASS lcl_event_receiver_01 DEFINITION.

  PUBLIC SECTION.
    METHODS: zm_handle_click_01 FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_column e_row es_row_no.


*    CLASS-METHODS:
    METHODS:
      catch_hotspot2 FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.

ENDCLASS.

CLASS lcl_event_receiver_01 IMPLEMENTATION.
  METHOD zm_handle_click_01.
    PERFORM z_handler_double_click_01 USING e_row e_column es_row_no.
  ENDMETHOD.

*Início - CS2019001220 - Sara Oikawa - Jun/2020
  METHOD catch_hotspot2.
    PERFORM z_handler_click_hotspot USING e_row_id e_column_id es_row_no.
  ENDMETHOD.
*Fim - CS2019001220 - Sara Oikawa - Jun/2020

ENDCLASS.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_p FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed,

      on_toolbar_cad FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.

*Início - CS2019001220 - Sara Oikawa - Jun/2020

*    CLASS-METHODS:
*      CATCH_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
*        IMPORTING E_ROW_ID
*                  E_COLUMN_ID
*                  ES_ROW_NO.
*Fim - CS2019001220 - Sara Oikawa - Jun/2020

ENDCLASS.



CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    SORT it_saida_02 BY ebeln ebelp safra tp_safra.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
       WHERE fieldname EQ 'SAFRA' OR fieldname EQ 'TP_SAFRA' OR
             fieldname EQ 'QTD'.

      LOOP AT it_saida_0102 INTO wa_saida_0102.
        CHECK wa_good_cells-row_id EQ sy-tabix.

        wa_saida_0102-ebeln =  wa_sd_0101-ebeln.
        wa_saida_0102-ebelp =  wa_sd_0101-ebelp.

        CASE wa_good_cells-fieldname.
          WHEN 'SAFRA'.
            wa_saida_0102-safra =   wa_good_cells-value.
            MODIFY it_saida_0102 FROM wa_saida_0102 INDEX wa_good_cells-row_id.
          WHEN 'TP_SAFRA'.

            wa_saida_0102-tp_safra =   wa_good_cells-value.
            MODIFY it_saida_0102 FROM wa_saida_0102 INDEX wa_good_cells-row_id.
          WHEN 'QTD'.
            wa_saida_0102-qtd   = wa_good_cells-value.
            IF wa_saida_0102-qtd > 0 AND wa_sd_0101-menge < 0.
              MESSAGE 'Não é possível classificar com valor positivo, valor do pedido é negativo!' TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
            MODIFY it_saida_0102 FROM wa_saida_0102 INDEX wa_good_cells-row_id.
        ENDCASE.
        IF wa_saida_0102-safra IS NOT INITIAL  AND  wa_saida_0102-tp_safra IS NOT INITIAL AND
           wa_saida_0102-qtd IS NOT INITIAL.
          READ TABLE it_saida_02 TRANSPORTING NO FIELDS
          WITH KEY ebeln    = wa_saida_0102-ebeln
                   ebelp    = wa_saida_0102-ebelp
                   safra    = wa_saida_0102-safra
                   tp_safra = wa_saida_0102-tp_safra
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            DELETE it_saida_02 INDEX sy-tabix.
          ENDIF.

          APPEND wa_saida_0102 TO it_saida_02.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD on_data_changed_finished.

    CALL METHOD g_grid_02->get_selected_rows
      IMPORTING
        et_index_rows = tg_selectedrow.

    IF tg_selectedrow[] IS  INITIAL.
      CALL METHOD g_grid_02->refresh_table_display
        EXPORTING
          is_stable = wa_stable_02.
    ENDIF.
  ENDMETHOD.

  METHOD on_data_changed_p.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'GRUPO'.

      LOOP AT it_saida INTO DATA(wsaida).

        CHECK wa_good_cells-row_id EQ sy-tabix.

        IF wsaida-grupo IS NOT INITIAL.
          wa_excluir-grupo = wsaida-grupo.
          wa_excluir-matnr = wsaida-matnr.
          APPEND wa_excluir TO it_excluir.
          CLEAR wa_excluir.
        ENDIF.

        wsaida-grupo = wa_good_cells-value.
        MODIFY it_saida FROM wsaida INDEX wa_good_cells-row_id.

        IF wa_good_cells-value IS NOT INITIAL.
          wa_grupo-mandt       = sy-mandt.
          wa_grupo-grupo       = wsaida-grupo.
          wa_grupo-matnr       = wsaida-matnr.
          wa_grupo-data_atual  = sy-datum.
          wa_grupo-hora_atual  = sy-uzeit.
          wa_grupo-usname      = sy-uname.
          APPEND wa_grupo TO it_grupo.
          CLEAR wa_grupo.
        ENDIF.

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_toolbar_cad.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'ADD_ROW'.
    wa_tool-icon     = '@17@'.
    wa_tool-quickinfo = 'Adicionar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function = 'DEL_ROW'.
    wa_tool-icon     = '@18@'.
    wa_tool-quickinfo = 'Eliminar Linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

  ENDMETHOD.             "DISPLAY

*Início - CS2019001220 - Sara Oikawa - Jun/2020
*  METHOD CATCH_HOTSPOT.
*    PERFORM Z_HANDLER_CLICK_HOTSPOT USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
*  ENDMETHOD.
*Fim - CS2019001220 - Sara Oikawa - Jun/2020

ENDCLASS.


CLASS lcl_alv_toolbar IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.

  METHOD on_toolbar.

    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'INSERIR'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'EXCLUIR'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.

  METHOD handle_user_command.
    DATA: it_aux TYPE TABLE OF ty_saida_0102,
          wa_aux TYPE ty_saida_0102.
    DATA: valor TYPE zsdt0194-qtd,
          soma  TYPE zsdt0194-qtd.

    REFRESH it_aux.
    CLEAR wa_aux-celltab.

    CASE e_ucomm.
      WHEN 'INSERIR'.
        IF click_alv01 EQ abap_true.
*          IF wa_sd_0101-tp_ped = 'T'.
*            MESSAGE 'Pedido de transferência! Não pode ser classificado!' TYPE 'S'.
*          ELSE.

          CLEAR: wa_aux_0101, valor,  wa_saida_0102.

          READ TABLE it_aux_0101 INTO wa_aux_0101 INDEX 1.

          IF it_saida_0102 IS INITIAL.
            wa_saida_0102-qtd = wa_aux_0101-menge.
            APPEND wa_saida_0102 TO it_saida_0102.

          ELSE.
            LOOP AT it_saida_0102 INTO DATA(w0102).
              valor = valor + w0102-qtd.
              CLEAR w0102.
            ENDLOOP.

            wa_saida_0102-qtd = wa_aux_0101-menge - valor.
            IF wa_saida_0102-qtd < 0.
              wa_saida_0102-qtd = wa_saida_0102-qtd * -1.
            ENDIF.
            APPEND wa_saida_0102 TO it_saida_0102.
          ENDIF.
*          ENDIF.
        ELSE.
          MESSAGE 'Favor selecione um pedido para fazer a classificação!' TYPE 'S'.
        ENDIF.

      WHEN 'EXCLUIR'.
        CALL METHOD g_grid_02->get_selected_rows
          IMPORTING
            et_index_rows = tg_selectedrow.

        IF tg_selectedrow IS INITIAL.
          MESSAGE ' Favor selecione uma linha!' TYPE 'S'.
          EXIT.
        ENDIF.

        LOOP AT tg_selectedrow INTO wg_selectedrow.
          READ TABLE it_saida_0102 INTO DATA(wa_s0102) INDEX  wg_selectedrow-index.
          IF sy-subrc = 0.
            DELETE FROM zsdt0194 WHERE ebeln     = wa_s0102-ebeln AND
                                       ebelp     = wa_s0102-ebelp AND
                                       safra     = wa_s0102-safra AND
                                       tp_safra  = wa_s0102-tp_safra AND
                                       werks     = wa_sd_0101-werks.
            APPEND wa_s0102 TO it_aux.

            IF wa_s0102-tp_safra EQ '1'.
              wa_sd_0101-qtd_safra = wa_sd_0101-qtd_safra - wa_s0102-qtd.
            ENDIF.

            IF wa_s0102-tp_safra EQ '2'.
              wa_sd_0101-qtd_safrinha = wa_sd_0101-qtd_safrinha - wa_s0102-qtd.
            ENDIF.

          ENDIF.
          CLEAR: wa_s0102.
        ENDLOOP.

        LOOP AT it_aux INTO wa_aux.
          DELETE it_saida_0102  WHERE ebeln     = wa_aux-ebeln AND
                                      ebelp     = wa_aux-ebelp AND
                                      safra     = wa_aux-safra AND
                                      tp_safra  = wa_aux-tp_safra.
          CLEAR  wa_aux.
        ENDLOOP.

        PERFORM z_clear_variavel.
        PERFORM z_busca_dados.
        MESSAGE 'Registro Excluido com Sucesso!' TYPE 'S'.


        SELECT * FROM zsdt0194 INTO TABLE @DATA(t194)
         WHERE ebeln EQ @wa_sd_0101-ebeln
         AND   ebelp EQ @wa_sd_0101-ebelp.

        LOOP AT t194 INTO DATA(w194).
          soma = soma + w194-qtd.
        ENDLOOP.

        IF soma <> wa_sd_0101-menge.

          CLEAR: wa_sd_0101-cor, ls_col_01.
          REFRESH lt_coltab_01.

          ls_col_01-fname      = 'EBELN'.
          ls_col_01-color-col  = '6'.
          APPEND ls_col_01 TO lt_coltab_01.
          wa_sd_0101-cor = lt_coltab_01.

        ELSE.

          CLEAR: wa_sd_0101-cor, ls_col_01.
          REFRESH lt_coltab_01.
        ENDIF.

        DELETE it_saida_0101 WHERE ebeln = wa_sd_0101-ebeln AND
                                   ebelp = wa_sd_0101-ebelp.
        APPEND wa_sd_0101 TO it_saida_0101.

        CALL METHOD g_grid_01->refresh_table_display
          EXPORTING
            is_stable = wa_stable_01.

        CALL METHOD g_grid->refresh_table_display.
    ENDCASE.

    CALL METHOD g_grid_02->refresh_table_display.

  ENDMETHOD.
ENDCLASS.

FORM z_clear_variavel.

  REFRESH: it_saida, it_compra, it_compra_aux, it_venda,  it_makt, it_materiais, it_zsdt0192, it_zsdt0193, it_zsdt0194, lt_coltab, it_193_aux.

  CLEAR: wa_saida, wa_compra, wa_compra_aux, wa_venda, wa_materiais, wa_makt, wa_zsdt0192, wa_zsdt0193, wa_zsdt0194, wa_zsdt0196, ls_col, wa_193_aux.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_busca_dados.

  TYPES:
    BEGIN OF ty_agrup,
      ebeln TYPE zsdt0194-ebeln,
      ebelp TYPE zsdt0194-ebelp,
      matnr TYPE zsdt0194-matnr,
      qtd_t TYPE zsdt0194-qtd,
    END OF ty_agrup.

  DATA: vqtd         TYPE zsdt0194-qtd,
        vqtd2        TYPE zsdt0194-qtd,
        vqtd3        TYPE zsdt0194-qtd,
        soma         TYPE zsdt0194-qtd,
        classificado TYPE c,
        lt_agrup     TYPE TABLE OF ty_agrup,
        lt_agrup2    TYPE TABLE OF ty_agrup,
        ls_agrup     TYPE ty_agrup.

  SELECT SINGLE usname INTO @DATA(user_acesso) FROM zsdt0286 WHERE usname_acesso EQ @sy-uname.

  IF sy-subrc = '0'.
    visao_total = 'X'.
  ENDIF.


  IF p_todos IS NOT INITIAL.

    SELECT *  FROM zsdt0192 INTO TABLE it_venda
    WHERE data     EQ p_data     "IN P_DATA
    AND   werks    IN p_werks
    AND   tipo     EQ 'V'
    AND   safra    IN p_gsafra.

  ELSE.
    SELECT * FROM zsdt0192 INTO TABLE it_venda
    WHERE data     EQ p_data    "IN P_DATA
    AND   werks    IN p_werks
    AND   tipo     EQ 'V'
    AND   safra    IN p_gsafra
    AND   tp_safra EQ tp_safra.
  ENDIF.

  SELECT *
   FROM zsdt0192 INTO TABLE it_compra
   WHERE data     EQ p_data     "IN P_DATA
   AND   werks    IN p_werks
   AND   tipo     EQ 'C'.



  SORT it_venda BY matnr.
  MOVE-CORRESPONDING it_venda TO it_materiais.

  LOOP AT it_compra INTO wa_compra.
    wa_materiais-matnr = wa_compra-matnr.
    APPEND wa_materiais TO it_materiais.
    CLEAR: wa_compra, wa_materiais.
  ENDLOOP.

  SORT it_compra BY matnr.

  CHECK it_materiais IS NOT INITIAL.

  SELECT *
    FROM makt INTO TABLE it_makt
    FOR ALL ENTRIES IN it_materiais
  WHERE matnr EQ it_materiais-matnr.

  SELECT *
    FROM mara INTO TABLE @DATA(it_mara)
   FOR ALL ENTRIES IN  @it_materiais
    WHERE matnr EQ @it_materiais-matnr.

  IF it_mara[] IS NOT INITIAL.
    SELECT *
      FROM t134t INTO TABLE @DATA(it_t134t)
     FOR ALL ENTRIES IN @it_mara
    WHERE mtart EQ @it_mara-mtart
      AND spras EQ @sy-langu.
  ENDIF.

  SELECT *
  FROM zsdt0193 INTO TABLE it_zsdt0193
    FOR ALL ENTRIES IN it_materiais
    WHERE matnr EQ it_materiais-matnr
    AND   werks IN p_werks.

*  CHECK it_zsdt0193 IS NOT INITIAL.
* BUG 83994 inicio
  IF it_zsdt0193 IS NOT INITIAL.
    IF p_todos IS NOT INITIAL.

      SELECT *
          FROM zsdt0194 INTO TABLE it_zsdt0194
          FOR ALL ENTRIES IN it_zsdt0193
            WHERE ebeln     EQ it_zsdt0193-ebeln
            AND   ebelp     EQ it_zsdt0193-ebelp
            AND   matnr     EQ it_zsdt0193-matnr
            AND   werks     EQ it_zsdt0193-werks
            AND   safra     IN p_gsafra.

    ELSE.

      SELECT *
          FROM zsdt0194 INTO TABLE it_zsdt0194
          FOR ALL ENTRIES IN it_zsdt0193
            WHERE ebeln     EQ it_zsdt0193-ebeln
            AND   ebelp     EQ it_zsdt0193-ebelp
            AND   matnr     EQ it_zsdt0193-matnr
            AND   werks     EQ it_zsdt0193-werks
            AND   safra     IN p_gsafra
            AND   tp_safra  EQ tp_safra.
    ENDIF.

    IF it_zsdt0194 IS NOT INITIAL.
      LOOP AT it_zsdt0194 ASSIGNING FIELD-SYMBOL(<fs_zsdt0194>).
        ls_agrup-ebeln = <fs_zsdt0194>-ebeln.
        ls_agrup-ebelp = <fs_zsdt0194>-ebelp.
        ls_agrup-matnr = <fs_zsdt0194>-matnr.
        ls_agrup-qtd_t = <fs_zsdt0194>-qtd.
        COLLECT ls_agrup INTO lt_agrup2.
        CLEAR ls_agrup.

      ENDLOOP.

      SORT lt_agrup2 BY ebeln ebelp.
    ENDIF.

    SELECT * FROM zsdt0194 INTO TABLE it_z194
        FOR ALL ENTRIES IN it_zsdt0193
          WHERE ebeln     EQ it_zsdt0193-ebeln
          AND   ebelp     EQ it_zsdt0193-ebelp
          AND   matnr     EQ it_zsdt0193-matnr
          AND   werks     EQ it_zsdt0193-werks.
    IF sy-subrc IS INITIAL.
      LOOP AT it_z194 ASSIGNING FIELD-SYMBOL(<fs_z194>).
        ls_agrup-ebeln = <fs_z194>-ebeln.
        ls_agrup-ebelp = <fs_z194>-ebelp.
        ls_agrup-matnr = <fs_z194>-matnr.
        ls_agrup-qtd_t = <fs_z194>-qtd.
        COLLECT ls_agrup INTO lt_agrup.
        CLEAR ls_agrup.

      ENDLOOP.

      SORT lt_agrup BY ebeln ebelp.
    ENDIF.

    DATA(lt_0193) = it_zsdt0193.
    SORT lt_0193 BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM lt_0193 COMPARING ebeln ebelp.

    SELECT *
      FROM ekpo
      INTO TABLE @DATA(lt_ekpo)
      FOR ALL ENTRIES IN @lt_0193
      WHERE ebeln = @lt_0193-ebeln
        AND ebelp = @lt_0193-ebelp.
    IF sy-subrc IS INITIAL.
      SORT lt_ekpo BY ebeln ebelp.

      LOOP AT lt_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>).

        wa_compra_aux-matnr = <fs_ekpo>-matnr.
        wa_compra_aux-qtd_t = <fs_ekpo>-menge.
        COLLECT wa_compra_aux INTO it_compra_aux.
        CLEAR: wa_compra, wa_compra_aux.

      ENDLOOP.
    ENDIF.
  ENDIF.
* BUG 83994 fim

  ""TRATA DADOS.
  SORT it_materiais BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_materiais COMPARING matnr.

  IF visao_total EQ 'X'.

    LOOP AT it_materiais INTO wa_materiais.

      READ TABLE it_mara  INTO DATA(wa_mara) WITH KEY matnr =  wa_materiais-matnr.


      READ TABLE it_t134t INTO DATA(wa_t134t) WITH KEY mtart = wa_mara-mtart.
      IF sy-subrc = 0.
        wa_saida-mtbez = wa_t134t-mtbez.
        wa_saida-mtart = wa_mara-mtart.
      ENDIF.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_materiais-matnr.
      IF sy-subrc = 0.
        wa_saida-matnr = wa_makt-matnr.
        wa_saida-maktx = wa_makt-maktx.
      ENDIF.



      LOOP AT it_venda INTO wa_venda
        WHERE matnr = wa_materiais-matnr.
        wa_saida-entrega_efetivada   = wa_saida-entrega_efetivada  +  wa_venda-qtd_u.
        wa_saida-saldo_entrega       = wa_saida-saldo_entrega      +  wa_venda-qtd_n.
      ENDLOOP.

      CLEAR vqtd3.

*      LOOP AT it_compra INTO wa_compra  WHERE  matnr = wa_materiais-matnr.
*        wa_compra_aux-matnr = wa_compra-matnr.
*        wa_compra_aux-qtd_t = wa_compra-qtd_t.
*        COLLECT wa_compra_aux INTO it_compra_aux.
*        CLEAR: wa_compra, wa_compra_aux.
*      ENDLOOP.

      READ TABLE it_compra_aux INTO wa_compra_aux WITH KEY matnr = wa_materiais-matnr.

      CLEAR: wa_zsdt0194, wa_z194, vqtd, vqtd2.

      LOOP AT it_zsdt0193 INTO wa_zsdt0193 WHERE matnr = wa_compra_aux-matnr.

        LOOP AT it_z194 INTO wa_z194
        WHERE  ebeln = wa_zsdt0193-ebeln AND  ebelp = wa_zsdt0193-ebelp AND
               matnr = wa_zsdt0193-matnr AND  werks = wa_zsdt0193-werks.
          vqtd2  = vqtd2  + wa_z194-qtd.
          CLEAR wa_z194.
        ENDLOOP.


        LOOP AT it_zsdt0194 INTO wa_zsdt0194
         WHERE  ebeln = wa_zsdt0193-ebeln AND ebelp = wa_zsdt0193-ebelp AND
                matnr = wa_zsdt0193-matnr AND werks = wa_zsdt0193-werks.
          vqtd  = vqtd  + wa_zsdt0194-qtd.
          CLEAR wa_zsdt0194.
        ENDLOOP.
        CLEAR wa_zsdt0193.
      ENDLOOP.

      READ TABLE it_zsdt0193 INTO wa_zsdt0193 WITH KEY matnr = wa_compra_aux-matnr.
      IF sy-subrc IS INITIAL.
        READ TABLE it_zsdt0194 INTO wa_zsdt0194 WITH KEY  matnr = wa_zsdt0193-matnr
                                                          werks = wa_zsdt0193-werks.
        IF sy-subrc IS INITIAL.

          DATA(lt_zsdt0193) = it_zsdt0193.
          SORT lt_zsdt0193 BY matnr werks.
          READ TABLE lt_zsdt0193 TRANSPORTING NO FIELDS
          WITH KEY matnr = wa_zsdt0193-matnr
                   werks = wa_zsdt0193-werks
          BINARY SEARCH.

          CLEAR ls_col.
          LOOP AT lt_zsdt0193 ASSIGNING FIELD-SYMBOL(<fs_zsdt0193>) FROM sy-tabix.
            IF wa_zsdt0193-werks <> <fs_zsdt0193>-werks OR
               wa_zsdt0193-matnr <> <fs_zsdt0193>-matnr.
              EXIT.
            ENDIF.

            READ TABLE it_zsdt0194 INTO wa_z194 WITH KEY  ebeln = <fs_zsdt0193>-ebeln
                                                      ebelp = <fs_zsdt0193>-ebelp.
            IF sy-subrc IS INITIAL.

              READ TABLE lt_ekpo ASSIGNING <fs_ekpo>
              WITH KEY ebeln = <fs_zsdt0193>-ebeln
                       ebelp = <fs_zsdt0193>-ebelp
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                IF <fs_ekpo>-menge < 1.
                  CONTINUE.
                ENDIF.

                READ TABLE lt_agrup2 ASSIGNING FIELD-SYMBOL(<fs_agrup>)
                WITH KEY ebeln = <fs_zsdt0193>-ebeln
                         ebelp = <fs_zsdt0193>-ebelp
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.

                  IF <fs_ekpo>-menge <= <fs_agrup>-qtd_t.

                    ls_col-fname     = 'TOTAL_ESTQ'. "VERDE
                    ls_col-color-col = '5'.
**                APPEND ls_col TO lt_coltab.
*                wa_saida-cor = lt_coltab.

                  ELSE.
                    ls_col-fname      = 'TOTAL_ESTQ'. "AMARELO
                    ls_col-color-col  = '3'.
                    EXIT.
*                APPEND ls_col TO lt_coltab.
*                wa_saida-cor = lt_coltab.
*                wa_saida-total_estq =  wa_compra_aux-qtd_t.
                  ENDIF.

                ENDIF.

              ENDIF.

            ELSE.

              READ TABLE it_z194 INTO wa_z194 WITH KEY  ebeln = <fs_zsdt0193>-ebeln
                                                        ebelp = <fs_zsdt0193>-ebelp.
              IF sy-subrc IS INITIAL.

                READ TABLE lt_ekpo ASSIGNING <fs_ekpo>
                WITH KEY ebeln = <fs_zsdt0193>-ebeln
                         ebelp = <fs_zsdt0193>-ebelp
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.

                  IF <fs_ekpo>-menge < 1.
                    CONTINUE.
                  ENDIF.

                  READ TABLE lt_agrup ASSIGNING <fs_agrup>
                  WITH KEY ebeln = <fs_zsdt0193>-ebeln
                           ebelp = <fs_zsdt0193>-ebelp
                  BINARY SEARCH.
                  IF sy-subrc IS INITIAL.

                    IF <fs_ekpo>-menge > <fs_agrup>-qtd_t.

                      ls_col-fname      = 'TOTAL_ESTQ'. "AMARELO
                      ls_col-color-col  = '3'.
                      EXIT.
*                APPEND ls_col TO lt_coltab.
*                wa_saida-cor = lt_coltab.
*                wa_saida-total_estq =  wa_compra_aux-qtd_t.
                    ENDIF.

                  ENDIF.

                ENDIF.

              ELSE.

                ls_col-fname      = 'TOTAL_ESTQ'. "AMARELO
                ls_col-color-col  = '3'.
                EXIT.

              ENDIF.

            ENDIF.

          ENDLOOP.

          APPEND ls_col TO lt_coltab.
          wa_saida-cor = lt_coltab.

        ELSE.
          lt_zsdt0193 = it_zsdt0193.
          SORT lt_zsdt0193 BY matnr werks.
          READ TABLE lt_zsdt0193 TRANSPORTING NO FIELDS
          WITH KEY matnr = wa_zsdt0193-matnr
                   werks = wa_zsdt0193-werks
          BINARY SEARCH.

          CLEAR ls_col.
          LOOP AT lt_zsdt0193 ASSIGNING <fs_zsdt0193> FROM sy-tabix.
            IF wa_zsdt0193-werks <> <fs_zsdt0193>-werks OR
               wa_zsdt0193-matnr <> <fs_zsdt0193>-matnr.
              EXIT.
            ENDIF.

            READ TABLE lt_ekpo ASSIGNING <fs_ekpo>
            WITH KEY ebeln = <fs_zsdt0193>-ebeln
                     ebelp = <fs_zsdt0193>-ebelp
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              IF <fs_ekpo>-menge <= 1.
                CONTINUE.
              ENDIF.

              READ TABLE it_z194 INTO wa_z194 WITH KEY  ebeln = <fs_zsdt0193>-ebeln
                                                        ebelp = <fs_zsdt0193>-ebelp.
              IF sy-subrc IS INITIAL.

                READ TABLE lt_agrup ASSIGNING <fs_agrup>
                WITH KEY ebeln = <fs_zsdt0193>-ebeln
                         ebelp = <fs_zsdt0193>-ebelp
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.

                  IF <fs_ekpo>-menge <= <fs_agrup>-qtd_t.

                    IF  ls_col-color-col <> '6'.

                      ls_col-fname     = 'TOTAL_ESTQ'. "VERDE
                      ls_col-color-col = '5'.
**                APPEND ls_col TO lt_coltab.
*                wa_saida-cor = lt_coltab.
                    ENDIF.

                  ELSE.
                    ls_col-fname      = 'TOTAL_ESTQ'. "AMARELO
                    ls_col-color-col  = '3'.
                    EXIT.
*                APPEND ls_col TO lt_coltab.
*                wa_saida-cor = lt_coltab.
*                wa_saida-total_estq =  wa_compra_aux-qtd_t.
                  ENDIF.

                ENDIF.


              ELSE.

                ls_col-fname      = 'TOTAL_ESTQ'. "VERMELHO
                ls_col-color-col  = '6'.

              ENDIF.

            ENDIF.

          ENDLOOP.

          APPEND ls_col TO lt_coltab.
          wa_saida-cor = lt_coltab.

        ENDIF.
      ENDIF.

      READ TABLE it_zsdt0193 INTO wa_zsdt0193 WITH KEY matnr = wa_compra_aux-matnr.

      IF sy-subrc = 0.
        READ TABLE it_z194 INTO wa_z194 WITH KEY  matnr = wa_zsdt0193-matnr
                                                  werks = wa_zsdt0193-werks.
        IF sy-subrc = 0.

          IF wa_compra_aux-qtd_t EQ vqtd2 .

*            ls_col-fname     = 'TOTAL_ESTQ'. "VERDE
*            ls_col-color-col = '5'.
*            APPEND ls_col TO lt_coltab.
*            wa_saida-cor = lt_coltab.

            READ TABLE it_zsdt0194 INTO wa_zsdt0194 WITH KEY  matnr = wa_zsdt0193-matnr.
            IF sy-subrc <> 0.
              wa_saida-total_estq = '0.00'.
            ELSE.
              wa_saida-total_estq =   vqtd.
            ENDIF.

          ELSE.
            IF wa_zsdt0193-tp_ped = 'T'.

              LOOP AT it_zsdt0193  INTO wa_193_aux WHERE matnr = wa_zsdt0193-matnr AND
                                                         werks  = wa_zsdt0193-werks.
                IF wa_193_aux-tp_ped <> 'T'.

                  SELECT SINGLE * FROM ekpo INTO  @DATA(wa_ekpo)
                  WHERE ebeln EQ @wa_193_aux-ebeln
                   AND  ebelp EQ @wa_193_aux-ebelp.

                  SELECT  *  FROM zsdt0194 INTO TABLE @DATA(t194)
                    WHERE ebeln EQ @wa_193_aux-ebeln
                    AND   ebelp EQ @wa_193_aux-ebelp.

                  LOOP AT t194 INTO DATA(w194).
                    soma = soma + w194-qtd.
                  ENDLOOP.

                  IF soma = wa_ekpo-menge.
                    classificado = 'X'.
                  ELSE.
                    classificado = ' '.
                    EXIT.
                  ENDIF.
                ENDIF.

                CLEAR: soma, wa_193_aux, wa_ekpo, w194.
              ENDLOOP.

              IF classificado = 'X'.
*                ls_col-fname     = 'TOTAL_ESTQ'. "VERDE
*                ls_col-color-col = '5'.
*                APPEND ls_col TO lt_coltab.
*                wa_saida-cor = lt_coltab.
              ELSE.
*                ls_col-fname     = 'TOTAL_ESTQ'. "AMARELO
*                ls_col-color-col = '3'.
*                ls_col-color-inv = '0'.
*                APPEND ls_col TO lt_coltab.
*                wa_saida-cor = lt_coltab.
              ENDIF.

              READ TABLE it_zsdt0194 INTO wa_zsdt0194 WITH KEY  matnr = wa_zsdt0193-matnr.
              IF sy-subrc <> 0.
                wa_saida-total_estq = '0.00'.
              ELSE.
                wa_saida-total_estq =   vqtd.
              ENDIF.

            ELSE.

*              ls_col-fname     = 'TOTAL_ESTQ'. "AMARELO
*              ls_col-color-col = '3'.
*              ls_col-color-inv = '0'.
*              APPEND ls_col TO lt_coltab.
*              wa_saida-cor = lt_coltab.

              READ TABLE it_zsdt0194 INTO wa_zsdt0194 WITH KEY  matnr = wa_zsdt0193-matnr.
              IF sy-subrc <> 0.
                wa_saida-total_estq = '0.00'.
              ELSE.
                wa_saida-total_estq =   vqtd.
              ENDIF.

            ENDIF.
          ENDIF.

        ELSE.

          IF wa_compra_aux-qtd_t < 0 .

*            ls_col-fname      = 'TOTAL_ESTQ'. "VERDE
*            ls_col-color-col  = '5'.
*            APPEND ls_col TO lt_coltab.
*            wa_saida-cor = lt_coltab.
            wa_saida-total_estq =  wa_compra_aux-qtd_t.
          ELSE.

            LOOP AT it_zsdt0193  INTO wa_193_aux WHERE matnr  = wa_zsdt0193-matnr AND
                                                       werks  = wa_zsdt0193-werks.
              IF wa_193_aux-tp_ped <> 'T'.

                SELECT  *  FROM zsdt0194 INTO TABLE t194
                  WHERE ebeln EQ wa_193_aux-ebeln
                  AND   ebelp EQ wa_193_aux-ebelp.

                IF sy-subrc = 0.
                  classificado = 'X'.
                ELSE.
                  classificado = ' '.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDLOOP.

            IF classificado = 'X'.
*              ls_col-fname      = 'TOTAL_ESTQ'. "VERMELHO
*              ls_col-color-col  = '5'.
*              APPEND ls_col TO lt_coltab.
*              wa_saida-cor = lt_coltab.
              wa_saida-total_estq =  wa_compra_aux-qtd_t.
            ELSE.
*              ls_col-fname      = 'TOTAL_ESTQ'. "VERMELHO
*              ls_col-color-col  = '6'.
*              APPEND ls_col TO lt_coltab.
*              wa_saida-cor = lt_coltab.
              wa_saida-total_estq =  wa_compra_aux-qtd_t.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


      CLEAR ls_col.
      REFRESH lt_coltab.

      wa_saida-estoque_geren  =  (  wa_saida-total_estq -  wa_saida-entrega_efetivada -  wa_saida-saldo_entrega ).
      wa_saida-ov_carteira    =  ( wa_saida-entrega_efetivada + wa_saida-saldo_entrega ).

      SELECT SINGLE *  FROM zsdt0196 INTO wa_zsdt0196  WHERE matnr EQ wa_saida-matnr.
      wa_saida-grupo = wa_zsdt0196-grupo.

      APPEND wa_saida TO it_saida.

      CLEAR: wa_saida, wa_materiais, wa_compra, wa_venda, wa_zsdt0193, wa_zsdt0194,wa_zsdt0196, wa_makt, ls_col, wa_zsdt0196, wa_compra_aux,wa_193_aux.
      REFRESH lt_coltab.


    ENDLOOP.
  ELSE.
    LOOP AT it_materiais INTO wa_materiais.

      READ TABLE it_mara  INTO DATA(wa_mara2) WITH KEY matnr =  wa_materiais-matnr.


      READ TABLE it_t134t INTO DATA(wa_t134t2) WITH KEY mtart = wa_mara2-mtart.
      IF sy-subrc = 0.
        wa_saida-mtbez = wa_t134t2-mtbez.
        wa_saida-mtart = wa_mara2-mtart.
      ENDIF.

      IF wa_saida-mtbez NE 'Produto de Revenda'.

        READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_materiais-matnr.
        IF sy-subrc = 0.
          wa_saida-matnr = wa_makt-matnr.
          wa_saida-maktx = wa_makt-maktx.
        ENDIF.



        LOOP AT it_venda INTO wa_venda
          WHERE matnr = wa_materiais-matnr.
          wa_saida-entrega_efetivada   = wa_saida-entrega_efetivada  +  wa_venda-qtd_u.
          wa_saida-saldo_entrega       = wa_saida-saldo_entrega      +  wa_venda-qtd_n.
        ENDLOOP.

        CLEAR vqtd3.

        LOOP AT it_compra INTO wa_compra  WHERE  matnr = wa_materiais-matnr.
          wa_compra_aux-matnr = wa_compra-matnr.
          wa_compra_aux-qtd_t = wa_compra-qtd_t.
          COLLECT wa_compra_aux INTO it_compra_aux.
          CLEAR: wa_compra, wa_compra_aux.
        ENDLOOP.

        READ TABLE it_compra_aux INTO wa_compra_aux WITH KEY matnr = wa_materiais-matnr.

        CLEAR: wa_zsdt0194, wa_z194, vqtd, vqtd2.

        LOOP AT it_zsdt0193 INTO wa_zsdt0193 WHERE matnr = wa_compra_aux-matnr.

          LOOP AT it_z194 INTO wa_z194
          WHERE  ebeln = wa_zsdt0193-ebeln AND  ebelp = wa_zsdt0193-ebelp AND
                 matnr = wa_zsdt0193-matnr AND  werks = wa_zsdt0193-werks.
            vqtd2  = vqtd2  + wa_z194-qtd.
            CLEAR wa_z194.
          ENDLOOP.


          LOOP AT it_zsdt0194 INTO wa_zsdt0194
           WHERE  ebeln = wa_zsdt0193-ebeln AND ebelp = wa_zsdt0193-ebelp AND
                  matnr = wa_zsdt0193-matnr AND werks = wa_zsdt0193-werks.
            vqtd  = vqtd  + wa_zsdt0194-qtd.
            CLEAR wa_zsdt0194.
          ENDLOOP.
          CLEAR wa_zsdt0193.
        ENDLOOP.

        READ TABLE it_zsdt0193 INTO wa_zsdt0193 WITH KEY matnr = wa_compra_aux-matnr.

        IF sy-subrc = 0.
          READ TABLE it_z194 INTO wa_z194 WITH KEY  matnr = wa_zsdt0193-matnr
                                                    werks = wa_zsdt0193-werks.
          IF sy-subrc = 0.

            IF wa_compra_aux-qtd_t EQ vqtd2 .

              ls_col-fname     = 'TOTAL_ESTQ'. "VERDE
              ls_col-color-col = '5'.
              APPEND ls_col TO lt_coltab.
              wa_saida-cor = lt_coltab.

              READ TABLE it_zsdt0194 INTO wa_zsdt0194 WITH KEY  matnr = wa_zsdt0193-matnr.
              IF sy-subrc <> 0.
                wa_saida-total_estq = '0.00'.
              ELSE.
                wa_saida-total_estq =   vqtd.
              ENDIF.

            ELSE.
              IF wa_zsdt0193-tp_ped = 'T'.

                LOOP AT it_zsdt0193  INTO wa_193_aux WHERE matnr = wa_zsdt0193-matnr AND
                                                           werks  = wa_zsdt0193-werks.
                  IF wa_193_aux-tp_ped <> 'T'.

                    SELECT SINGLE * FROM ekpo INTO  @DATA(wa_ekpo2)
                    WHERE ebeln EQ @wa_193_aux-ebeln
                     AND  ebelp EQ @wa_193_aux-ebelp.

                    SELECT  *  FROM zsdt0194 INTO TABLE @DATA(t1942)
                      WHERE ebeln EQ @wa_193_aux-ebeln
                      AND   ebelp EQ @wa_193_aux-ebelp.

                    LOOP AT t194 INTO DATA(w1942).
                      soma = soma + w1942-qtd.
                    ENDLOOP.

                    IF soma = wa_ekpo2-menge.
                      classificado = 'X'.
                    ELSE.
                      classificado = ' '.
                      EXIT.
                    ENDIF.
                  ENDIF.

                  CLEAR: soma, wa_193_aux, wa_ekpo2, w1942.
                ENDLOOP.

                IF classificado = 'X'.
                  ls_col-fname     = 'TOTAL_ESTQ'. "VERDE
                  ls_col-color-col = '5'.
                  APPEND ls_col TO lt_coltab.
                  wa_saida-cor = lt_coltab.
                ELSE.
                  ls_col-fname     = 'TOTAL_ESTQ'. "AMARELO
                  ls_col-color-col = '3'.
                  ls_col-color-inv = '0'.
                  APPEND ls_col TO lt_coltab.
                  wa_saida-cor = lt_coltab.
                ENDIF.

                READ TABLE it_zsdt0194 INTO wa_zsdt0194 WITH KEY  matnr = wa_zsdt0193-matnr.
                IF sy-subrc <> 0.
                  wa_saida-total_estq = '0.00'.
                ELSE.
                  wa_saida-total_estq =   vqtd.
                ENDIF.

              ELSE.

                ls_col-fname     = 'TOTAL_ESTQ'. "AMARELO
                ls_col-color-col = '3'.
                ls_col-color-inv = '0'.
                APPEND ls_col TO lt_coltab.
                wa_saida-cor = lt_coltab.

                READ TABLE it_zsdt0194 INTO wa_zsdt0194 WITH KEY  matnr = wa_zsdt0193-matnr.
                IF sy-subrc <> 0.
                  wa_saida-total_estq = '0.00'.
                ELSE.
                  wa_saida-total_estq =   vqtd.
                ENDIF.

              ENDIF.
            ENDIF.

          ELSE.

            IF wa_compra_aux-qtd_t < 0 .

              ls_col-fname      = 'TOTAL_ESTQ'. "VERDE
              ls_col-color-col  = '5'.
              APPEND ls_col TO lt_coltab.
              wa_saida-cor = lt_coltab.
              wa_saida-total_estq =  wa_compra_aux-qtd_t.
            ELSE.

              LOOP AT it_zsdt0193  INTO wa_193_aux WHERE matnr  = wa_zsdt0193-matnr AND
                                                         werks  = wa_zsdt0193-werks.
                IF wa_193_aux-tp_ped <> 'T'.

                  SELECT  *  FROM zsdt0194 INTO TABLE t194
                    WHERE ebeln EQ wa_193_aux-ebeln
                    AND   ebelp EQ wa_193_aux-ebelp.

                  IF sy-subrc = 0.
                    classificado = 'X'.
                  ELSE.
                    classificado = ' '.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              IF classificado = 'X'.
                ls_col-fname      = 'TOTAL_ESTQ'. "VERMELHO
                ls_col-color-col  = '5'.
                APPEND ls_col TO lt_coltab.
                wa_saida-cor = lt_coltab.
                wa_saida-total_estq =  wa_compra_aux-qtd_t.
              ELSE.
                ls_col-fname      = 'TOTAL_ESTQ'. "VERMELHO
                ls_col-color-col  = '6'.
                APPEND ls_col TO lt_coltab.
                wa_saida-cor = lt_coltab.
                wa_saida-total_estq =  wa_compra_aux-qtd_t.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


        CLEAR ls_col.
        REFRESH lt_coltab.

        wa_saida-estoque_geren  =  (  wa_saida-total_estq -  wa_saida-entrega_efetivada -  wa_saida-saldo_entrega ).
        wa_saida-ov_carteira    =  ( wa_saida-entrega_efetivada + wa_saida-saldo_entrega ).

        SELECT SINGLE *  FROM zsdt0196 INTO wa_zsdt0196  WHERE matnr EQ wa_saida-matnr.
        wa_saida-grupo = wa_zsdt0196-grupo.

        APPEND wa_saida TO it_saida.

        CLEAR: wa_saida, wa_materiais, wa_compra, wa_venda, wa_zsdt0193, wa_zsdt0194,wa_zsdt0196, wa_makt, ls_col, wa_zsdt0196, wa_compra_aux,wa_193_aux.
        REFRESH lt_coltab.

      ENDIF.
    ENDLOOP.

  ENDIF.

  SORT it_saida BY maktx.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  CASE visao_total.
    WHEN abap_false.
      SET PF-STATUS 'ST_0100' EXCLUDING '&EDIT'.
    WHEN OTHERS.
      SET PF-STATUS 'ST_0100'.
  ENDCASE.

  SET TITLEBAR  'TL_0100'.

  PERFORM z_cria_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&EDIT'.
      LOOP AT it_saida INTO wa_saida.
        FREE wa_saida-celltab.
        gt_estilo  = VALUE #( ( fieldname = 'GRUPO' style = cl_gui_alv_grid=>mc_style_enabled ) ).
        INSERT LINES OF gt_estilo  INTO TABLE wa_saida-celltab.
        MODIFY it_saida FROM wa_saida.
      ENDLOOP.
    WHEN 'SALVAR'.

      LOOP AT it_excluir INTO wa_excluir.
        DELETE FROM zsdt0196  WHERE grupo = wa_excluir-grupo AND
                                    matnr = wa_excluir-matnr.
        CLEAR wa_excluir.
      ENDLOOP.

      IF it_grupo IS NOT INITIAL.
        MODIFY zsdt0196 FROM TABLE it_grupo.
        REFRESH it_grupo.
      ENDIF.

      CHECK it_grupo IS  INITIAL.
      MESSAGE 'Lançamento gravado com Sucesso!' TYPE 'I'.

      LOOP AT it_saida INTO wa_saida.
        FREE wa_saida-celltab.
        gt_estilo  = VALUE #( ( fieldname = 'GRUPO' style = cl_gui_alv_grid=>mc_style_disabled ) ).
        INSERT LINES OF gt_estilo  INTO TABLE wa_saida-celltab.
        MODIFY it_saida FROM wa_saida.
      ENDLOOP.

* CS2019001220 - Sara Oikawa - Jun/2020
    WHEN 'LEGENDA'.

      CALL SCREEN '0900' STARTING AT 25 2
                         ENDING   AT 68 5.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_alv.
  CLEAR wl_fcat.
  REFRESH it_fcat[].

  PERFORM preenche_cat USING:
        'GRUPO'                 'Grupo'                  '20'      ''      ''     ''      ''    ''      '',
        'MTBEZ'                 'Tp.Material'            '20'      ''      ''     ''      ''    ''      '',
        'MATNR'                 'Cod.Material'           '10'      'X'     'X'    ''      ''    ''      '',
        'MAKTX'                 'Material'               '40'      ''      ''     ''      ''    ''      '',
        'OV_CARTEIRA'           'OV em Carteira'         '14'      ''      'X'    'X'     ''    ''      '',
        'TOTAL_ESTQ'            'Compras '               '14'      ''      'X'    'X'     ''    'C500'  '', "USER STORY 106316 / AOENING
        'ENTREGA_EFETIVADA'     'Entregas Efetivadas'    '20'      ''      ''     'X'     ''    'C700'  '', "VDS_LIBERADA
        'SALDO_ENTREGA'         'Saldo a Entregar'       '18'      ''      ''     'X'     ''    'C700'  '', "VDS_A_LIBERAR
        'ESTOQUE_GEREN'         'Estoque Gerencial'      '18'      ''      ''     'X'     ''    'C400'  '' . "PROJ_A_LIBERAR

ENDFORM.

FORM z_alv_cad.
  CLEAR wl_fcat_cad.
  REFRESH it_fcat_cad[].

  PERFORM preenche_cat_cad USING:
        'MANDT'               'MANDT'            '20'      ''      ''     ''      ''    ''      '',
        'USNAME_ACESSO'       'Criador'            '20'      ''      ''     ''      ''    ''      '',
        'USNAME'              'Usuário'            '20'      ''      ''     ''      ''    ''      '',
        'DATA'                'Data Registro'      '10'      ''      ''     ''      ''    ''      '',
        'HORA'                'Hora Registro'      '40'      ''      ''     ''      ''    ''      '' .

ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_cor)
                        VALUE(p_edit).

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-just      = p_just.
  wl_fcat-emphasize = p_cor.
  wl_fcat-edit      = p_edit.

  IF p_campo EQ 'TOTAL_ESTQ' AND visao_total IS INITIAL.
    CLEAR wl_fcat-hotspot.
  ENDIF.

  APPEND wl_fcat TO  it_fcat.

ENDFORM.

FORM preenche_cat_cad USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_cor)
                        VALUE(p_edit).

  wl_fcat_cad-fieldname = p_campo.
  wl_fcat_cad-scrtext_l = p_desc.
  wl_fcat_cad-scrtext_m = p_desc.
  wl_fcat_cad-scrtext_s = p_desc.
  wl_fcat_cad-outputlen = p_tam.
  wl_fcat_cad-hotspot   = p_hot.
  wl_fcat_cad-no_zero   = p_zero.
  wl_fcat_cad-do_sum    = p_sum.
  wl_fcat_cad-just      = p_just.
  wl_fcat_cad-emphasize = p_cor.
  wl_fcat_cad-edit      = p_edit.

  APPEND wl_fcat_cad TO  it_fcat_cad.

ENDFORM.

FORM z_cria_alv.

  DATA: wa_event    TYPE REF TO lcl_event_receiver.
  DATA(_risk) = zcl_risk_of_pesticide=>get_validity_risk( with_icon = abap_false ).

  DATA: url(255)                TYPE c,
        data_ini(10)            TYPE c,
        data_fim(10)            TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i.


  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 1. "2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1 "2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
*       HEIGHT = 18.
        height = 0.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 50.


    CREATE OBJECT document.
    document->initialize_document( ).

    wa_layout-ctab_fname = 'COR'.
    wa_layout-stylefname = 'CELLTAB'.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = dg_parent_alv.

    IF wa_event IS INITIAL.
      CREATE OBJECT wa_event.
      SET HANDLER wa_event->zm_handle_click FOR g_grid.
      SET HANDLER wa_event->catch_hotspot FOR g_grid.
    ENDIF.


    PERFORM z_alv.

    PERFORM z_sort.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        i_save                        = 'A'
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fcat
        it_sort                       = t_sort
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    SET HANDLER: lcl_event_handler=>on_data_changed_p FOR g_grid.

*Início - CS2019001220 - Sara Oikawa - Jun/2020
*  Remover a legenda das cores do cabeçalho do ALV
*  Criar Botão Legenda, e ao clicar nele abrir um pop-up exibindo
*  a legenda (Tela 0900)

*    SET HANDLER: LCL_EVENT_HANDLER=>CATCH_HOTSPOT FOR G_GRID.


*    DOCUMENT->NEW_LINE( ).
*    DOCUMENT->ADD_TEXT( TEXT = 'Legenda das cores:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_STYLE = CL_DD_AREA=>HEADING ).
*    DOCUMENT->NEW_LINE(  ). DOCUMENT->UNDERLINE( ).
*
*
*    CALL METHOD DOCUMENT->ADD_TABLE(
*      EXPORTING
*        NO_OF_COLUMNS = 1
*      IMPORTING
*        TABLEAREA     = DATA(_DOCTABLE2) ).
*
*
*    _DOCTABLE2->ADD_TEXT( TEXT = CONV #( '   ' ) SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_COLOR = CL_DD_AREA=>LIST_POSITIVE_INT FIX_LINES = '' ).
*    _DOCTABLE2->ADD_TEXT( TEXT = CONV #( 'Pedidos Totalmente Classificados' ) ).
*    _DOCTABLE2->NEW_ROW( ).
*
*    _DOCTABLE2->ADD_TEXT( TEXT = CONV #( '   ')  SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_COLOR = CL_DD_AREA=>LIST_TOTAL_INT FIX_LINES = '' ).
*    _DOCTABLE2->ADD_TEXT( TEXT = CONV #( 'Pedidos Parcialmente Classificados') ).
*    _DOCTABLE2->NEW_ROW( ).
*
*    _DOCTABLE2->ADD_TEXT( TEXT = CONV #( '   ' ) SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_COLOR = CL_DD_AREA=>LIST_NEGATIVE_INT FIX_LINES = '' ).
*    _DOCTABLE2->ADD_TEXT( TEXT = CONV #( 'Pedidos sem nenhuma Classificação' ) ).
*    _DOCTABLE2->NEW_ROW( ).
*
*
*    DOCUMENT->MERGE_DOCUMENT( ).
*    DOCUMENT->DISPLAY_DOCUMENT( PARENT = DG_PARENT_2 ).

*Fim - CS2019001220 - Sara Oikawa - Jun/2020

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.


FORM z_cria_alv_cad.

  DATA: wa_event TYPE REF TO lcl_event_receiver,
        _stable  TYPE lvc_s_stbl VALUE 'XX'.
*  DATA(_risk) = zcl_risk_of_pesticide=>get_validity_risk( with_icon = abap_false ).
*
*  DATA: url(255)                TYPE c,
*        data_ini(10)            TYPE c,
*        data_fim(10)            TYPE c,
*        p_text                  TYPE sdydo_text_element,
*        sdydo_text_element(255),
*        p_text_table            TYPE sdydo_text_table,
*        vl_cont                 TYPE i.


  IF g_custom_container_cad IS INITIAL.

    CREATE OBJECT g_custom_container_cad
      EXPORTING
        container_name              = 'C_CAD_USER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1_cad
      EXPORTING
        parent  = g_custom_container_cad
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1_cad->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1_cad.

    CALL METHOD dg_splitter_1_cad->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv_cad.

    CREATE OBJECT dg_splitter_2_cad
      EXPORTING
        parent  = dg_parent_1_cad
        rows    = 1
        columns = 1. "2.

    CALL METHOD dg_splitter_2_cad->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2_cad.

    CALL METHOD dg_splitter_2_cad->get_container
      EXPORTING
        row       = 1
        column    = 1 "2
      RECEIVING
        container = dg_parent_2a_cad.

    CALL METHOD dg_splitter_1_cad->set_row_height
      EXPORTING
        id     = 1
*       HEIGHT = 18.
        height = 0.

    CALL METHOD dg_splitter_2_cad->set_column_width
      EXPORTING
        id    = 1
        width = 50.


    "CREATE OBJECT document.
    "document->initialize_document( ).

    wa_layout-ctab_fname = 'COR'.
    wa_layout-stylefname = 'CELLTAB'.

    PERFORM z_alv_cad.

    PERFORM z_sort_cad.

    CREATE OBJECT g_grid_cad_user
      EXPORTING
        i_parent = dg_parent_alv.

    SET HANDLER: lcl_event_handler=>on_toolbar_cad FOR g_grid_cad_user.



    CALL METHOD g_grid_cad_user->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_cad
        i_save                        = 'A'
      CHANGING
        it_outtab                     = it_zsdt0286[]
        it_fieldcatalog               = it_fcat_cad
        it_sort                       = t_sort_cad
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.



    "lcl_event_handler=>on_data_changed_p FOR g_grid.
    CALL METHOD g_grid_cad_user->refresh_table_display
      EXPORTING
        is_stable = _stable.

*    CALL METHOD g_grid_cad_user->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*    CALL METHOD g_grid_cad_user->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*    CALL METHOD g_grid_cad_user->set_ready_for_input
*      EXPORTING
*        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLER_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMNES_ROW_NO  text
*----------------------------------------------------------------------*
FORM z_handler_double_click  USING     p_row
                                       p_column
                                       p_row_no.

  DATA valor LIKE wa_saida_0101-vlr_item_pedido.
  DATA soma  TYPE zsdt0194-qtd.

  CASE p_column.
    WHEN 'TOTAL_ESTQ'.
      IF visao_total EQ 'X'.
        IF NOT p_row  CA 'S' AND NOT p_row  CA 'T' .

          READ TABLE it_saida INTO wa_saida INDEX p_row.
          IF sy-subrc = 0.

            SELECT *
               FROM zsdt0193 INTO TABLE @DATA(it_zt0193)
              WHERE matnr EQ @wa_saida-matnr
               AND  werks IN @p_werks.

            CHECK it_zt0193 IS NOT INITIAL.

            SELECT *
              FROM ekpo INTO TABLE @DATA(it_ekpo)
              FOR ALL ENTRIES IN @it_zt0193
             WHERE ebeln EQ @it_zt0193-ebeln
              AND  ebelp EQ @it_zt0193-ebelp.

            SELECT *
              FROM ekko INTO TABLE @DATA(it_ekko)
              FOR ALL ENTRIES IN @it_zt0193
             WHERE ebeln EQ @it_zt0193-ebeln.


            LOOP AT it_zt0193 INTO  DATA(wa_zt0193).

              READ TABLE it_ekpo INTO DATA(w_ekpo) WITH KEY ebeln = wa_zt0193-ebeln
                                                            ebelp = wa_zt0193-ebelp.
              IF sy-subrc = 0.
                wa_saida_0101-ebeln  = wa_zt0193-ebeln.
                wa_saida_0101-ebelp  = wa_zt0193-ebelp.
                wa_saida_0101-matnr  = wa_zt0193-matnr.
                wa_saida_0101-tp_ped = wa_zt0193-tp_ped.
                wa_saida_0101-meins  = w_ekpo-meins.
                wa_saida_0101-werks  = wa_zt0193-werks.

                READ TABLE it_ekko INTO DATA(wekko) WITH KEY ebeln = wa_zt0193-ebeln.
                IF sy-subrc = 0.
                  wa_saida_0101-aedar  = wekko-aedat.
                ENDIF.

                PERFORM r_imposto_item USING  wekko-lifnr
                                              w_ekpo-werks
                                              w_ekpo-ebelp
                                              w_ekpo-ebeln
                                       CHANGING valor.

                wa_saida_0101-netpr  = w_ekpo-netpr.
                wa_saida_0101-bprme  = w_ekpo-bprme.


                IF wa_saida_0101-tp_ped = 'T'.
                  wa_saida_0101-menge           = w_ekpo-menge * -1.
                  wa_saida_0101-vlr_item_pedido = valor * -1.
                ELSE.
                  wa_saida_0101-menge           = w_ekpo-menge.
                  wa_saida_0101-vlr_item_pedido = valor.
                ENDIF.

                IF wekko-bsart = 'ZUB'.
                  wa_saida_0101-lifnr           = |{ wekko-reswk ALPHA = IN }|.
                ELSE.
                  wa_saida_0101-lifnr           = wekko-lifnr.
                ENDIF.

                SELECT SINGLE name1 FROM lfa1 INTO wa_saida_0101-name1
                  WHERE lifnr EQ wa_saida_0101-lifnr.

                IF wa_zt0193-tp_ped <> 'T'.

                  SELECT  *  FROM zsdt0194 INTO TABLE @DATA(t194)
                    WHERE ebeln EQ @wa_saida_0101-ebeln
                    AND   ebelp EQ @wa_saida_0101-ebelp.

                  LOOP AT t194 INTO DATA(w194).
                    soma = soma + w194-qtd.
                  ENDLOOP.

                  IF soma <> wa_saida_0101-menge.

                    CLEAR: wa_saida_0101-cor, ls_col_01.
                    REFRESH lt_coltab_01.

                    ls_col_01-fname      = 'EBELN'.
                    ls_col_01-color-col  = '6'.
                    APPEND ls_col_01 TO lt_coltab_01.
                    wa_saida_0101-cor = lt_coltab_01.
                  ENDIF.
                ENDIF.

                APPEND wa_saida_0101 TO it_saida_0101.
                CLEAR: wa_zt0193 , w_ekpo, wa_saida_0101, valor, ls_col_01, soma.
                REFRESH lt_coltab_01.

              ENDIF.
            ENDLOOP.

            CALL SCREEN 0101.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN 'GRUPO'.
      IF NOT p_row  CA 'S' AND  NOT p_row  CA 'T'.
        READ TABLE it_saida INTO wa_saida INDEX p_row.
        IF sy-subrc = 0.

          IF wa_saida-grupo IS NOT INITIAL.

            SELECT *
              FROM zsdt0196 INTO TABLE @DATA(it_zt0196)
              WHERE grupo EQ @wa_saida-grupo.

            SELECT *
              FROM zsdt0193 INTO TABLE it_zt0193
              FOR ALL ENTRIES IN it_zt0196
             WHERE matnr EQ it_zt0196-matnr.

            CHECK it_zt0193 IS NOT INITIAL.

            SELECT *
              FROM ekpo INTO TABLE it_ekpo
              FOR ALL ENTRIES IN it_zt0193
             WHERE ebeln EQ it_zt0193-ebeln
              AND  ebelp EQ it_zt0193-ebelp.

            SELECT *
              FROM ekko INTO TABLE it_ekko
              FOR ALL ENTRIES IN it_zt0193
             WHERE ebeln EQ it_zt0193-ebeln.


            LOOP AT it_zt0193 INTO wa_zt0193.

              READ TABLE it_ekpo INTO w_ekpo WITH KEY ebeln = wa_zt0193-ebeln
                                                      ebelp = wa_zt0193-ebelp.
              IF sy-subrc = 0.
                wa_saida_0101-ebeln  = wa_zt0193-ebeln.
                wa_saida_0101-ebelp  = wa_zt0193-ebelp.
                wa_saida_0101-matnr  = wa_zt0193-matnr.
                wa_saida_0101-tp_ped = wa_zt0193-tp_ped.
                wa_saida_0101-werks  = wa_zt0193-werks.
                wa_saida_0101-meins  = w_ekpo-meins.


                READ TABLE it_ekko INTO wekko WITH KEY ebeln = wa_zt0193-ebeln.
                IF sy-subrc = 0.
                  wa_saida_0101-aedar  = wekko-aedat.
                ENDIF.

                PERFORM r_imposto_item USING  wekko-lifnr
                                              w_ekpo-werks
                                              w_ekpo-ebelp
                                              w_ekpo-ebeln
                                       CHANGING valor.

                wa_saida_0101-netpr  = w_ekpo-netpr.
                wa_saida_0101-bprme  = w_ekpo-bprme.

                IF wa_saida_0101-tp_ped = 'T'.
                  wa_saida_0101-menge           = w_ekpo-menge * -1.
                  wa_saida_0101-vlr_item_pedido = valor * -1.
                ELSE.
                  wa_saida_0101-menge           = w_ekpo-menge.
                  wa_saida_0101-vlr_item_pedido = valor.
                ENDIF.

                IF wekko-bsart = 'ZUB'.
                  wa_saida_0101-lifnr           = |{ wekko-reswk ALPHA = IN }|.
                ELSE.
                  wa_saida_0101-lifnr           = wekko-lifnr.
                ENDIF.

                SELECT SINGLE name1 FROM lfa1 INTO wa_saida_0101-name1
                  WHERE lifnr EQ wa_saida_0101-lifnr.

                IF wa_zt0193-tp_ped <> 'T'.

                  SELECT  *  FROM zsdt0194 INTO TABLE t194
                    WHERE ebeln EQ wa_saida_0101-ebeln
                    AND   ebelp EQ wa_saida_0101-ebelp.

                  LOOP AT t194 INTO w194.
                    soma = soma + w194-qtd.
                  ENDLOOP.

                  IF  soma <> wa_saida_0101-menge.

                    CLEAR: wa_saida_0101-cor, ls_col_01.
                    REFRESH lt_coltab_01.

                    ls_col_01-fname      = 'EBELN'.
                    ls_col_01-color-col  = '6'.
                    APPEND ls_col_01 TO lt_coltab_01.
                    wa_saida_0101-cor = lt_coltab_01.
                  ENDIF.
                ENDIF.

                APPEND wa_saida_0101 TO it_saida_0101.
                CLEAR: wa_zt0193 , w_ekpo, wa_saida_0101, valor , ls_col_01, soma.
                REFRESH lt_coltab_01.
              ENDIF.
            ENDLOOP.
            CALL SCREEN 0101.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'ST_0101'.

  PERFORM z_alv_0101.
  PERFORM z_cria_alv_0101.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'SALVAR'.
      PERFORM z_salvar_dados.
    WHEN 'BACK'.
      REFRESH: it_saida_0101, it_saida_0102, it_saida_02.
      CLEAR click_alv01.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*Início - CS2019001220 - Sara Oikawa - Jun/2020
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0900 OUTPUT.
  SET PF-STATUS 'ST_0900'.
  SET TITLEBAR  'TL_0900'.

  PERFORM z_cria_legenda.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0900 INPUT.

  IF sy-ucomm = 'CANC'.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  SET PF-STATUS 'ST_0110'.
  SET TITLEBAR  'TL_0110'.

  PERFORM z_alv_0110.
  PERFORM z_cria_alv_0110.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  CASE sy-ucomm.

    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      REFRESH: it_saida_0110, it_saida_0111.
      CLEAR click_alv01.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0120 OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0120 OUTPUT.
  SET PF-STATUS 'ST_0110'.
  SET TITLEBAR  'TL_0120'.

  PERFORM z_alv_0120.
  PERFORM z_cria_alv_0120.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0120 INPUT.
  CASE sy-ucomm.

    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      REFRESH: it_saida_0120.
*      CLEAR CLICK_ALV01.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*Fim - CS2019001220 - Sara Oikawa - Jun/2020

*&---------------------------------------------------------------------*
*&      Form  Z_ALV_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_alv_0101.
  CLEAR wl_fcat_01.
  REFRESH it_fcat_01[].

  PERFORM preenche_cat_01 USING:
        'AEDAR'               'Data Pedido'         '10'      ''      ''     ''     ''    ''    '',
        'LIFNR'               'Fornecedor'          '08'      'X'     ''     ''     ''    ''    '',
        'NAME1'               'Descrição'           '20'      ''      ''     ''     ''    ''    '',
        'EBELN'               'Pedido'              '10'      ''      'X'    ''     ''    ''    '',
        'EBELP'               'Item'                '04'      ''      ''     ''     ''    ''    '',
        'MATNR'               'Material'            '08'      'X'     ''     ''     ''    ''    '',
        'WERKS'               'Centro'              '05'      ''      ''     ''     ''    ''    '',
        'LGORT'               'Depósito'            '08'      ''      ''     ''     ''    ''    '',
        'LGOBE'               'Desc. Depósito'      '10'      ''      ''     ''     ''    ''    '',
        'VERKF'               'Navio'               '10'      ''      ''     ''     ''    ''    '',
        'MENGE'               'Quantidade'          '15'      ''      ''     'X'    ''    ''    '',
        'QTD_FATUR'           'Qtd. Faturada'       '15'      ''      ''     'X'    ''    ''    '',
        'QTD_SALDO'           'Saldo Qtd.'          '15'      ''      ''     'X'    ''    ''    '',
        'MEINS'               'UM'                  '02'      ''      ''     ''     ''    ''    '',
        'VLR_ITEM_PEDIDO'     'Valor Item Pedido'   '15'      ''      ''     ' '    ''    ''    '',
        'NETPR'               'Preço'               '10'      ''      ''     ' '    ''    ''    '',
        'BPRME'               'UM Preço'            '03'      ''      ''     ' '    ''    ''    '',
        'QTD_SAFRA'           'Qtd. Safra Atrib.'   '15'      ''      ''     'X'    ''    ''    '',
        'QTD_SAFRINHA'        'Qtd. Safrinha Atrib.' '15'      ''      ''     'X'    ''    ''    ''.
ENDFORM.

FORM preenche_cat_01 USING VALUE(p_campo)
                           VALUE(p_desc)
                           VALUE(p_tam)
                           VALUE(p_zero)
                           VALUE(p_hot)
                           VALUE(p_sum)
                           VALUE(p_just)
                           VALUE(p_edit)
                           VALUE(p_cor).

  wl_fcat_01-fieldname = p_campo.
  wl_fcat_01-scrtext_l = p_desc.
  wl_fcat_01-scrtext_m = p_desc.
  wl_fcat_01-scrtext_s = p_desc.
  wl_fcat_01-outputlen = p_tam.
  wl_fcat_01-hotspot   = p_hot.
  wl_fcat_01-no_zero   = p_zero.
  wl_fcat_01-do_sum    = p_sum.
  wl_fcat_01-just      = p_just.
  wl_fcat_01-edit      = p_edit.
  wl_fcat_01-emphasize = p_cor.

  APPEND wl_fcat_01 TO  it_fcat_01.
  CLEAR: wl_fcat_01.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_ALV_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_cria_alv_0101.

  DATA: wa_event_01 TYPE REF TO lcl_event_receiver_01,
        obg_toolbar TYPE REF TO lcl_alv_toolbar.


  IF g_custom_container_01 IS INITIAL.

    CREATE OBJECT g_custom_container_01
      EXPORTING
        container_name              = 'CONTAINER01'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    IF g_grid_01 IS INITIAL AND  g_custom_container_01 IS NOT INITIAL.
      CREATE OBJECT g_grid_01
        EXPORTING
          i_parent          = g_custom_container_01
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    IF wa_event_01 IS INITIAL.
      CREATE OBJECT wa_event_01.
      SET HANDLER wa_event_01->zm_handle_click_01   FOR g_grid_01.
      SET HANDLER wa_event_01->catch_hotspot2       FOR g_grid_01.   "CS2019001220 - Sara Oikawa - Jun/2020
    ENDIF.


    wa_layout_01-ctab_fname = 'COR'.
    wa_layout_01-stylefname = 'CELLTAB'.


    CALL METHOD g_grid_01->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_01
      CHANGING
        it_outtab                     = it_saida_0101[]
        it_fieldcatalog               = it_fcat_01
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.

    CALL METHOD g_grid_01->refresh_table_display
      EXPORTING
        is_stable = wa_stable_01.
  ENDIF.



  IF g_custom_container_02 IS INITIAL.

    CREATE OBJECT g_custom_container_02
      EXPORTING
        container_name              = 'CONTAINER02'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    IF g_grid_02 IS INITIAL AND  g_custom_container_02 IS NOT INITIAL.
      CREATE OBJECT g_grid_02
        EXPORTING
          i_parent          = g_custom_container_02
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    IF obg_toolbar IS INITIAL.
      CREATE OBJECT obg_toolbar
        EXPORTING
          io_alv_grid = g_grid_02.

      SET HANDLER: obg_toolbar->on_toolbar FOR g_grid_02,
                   obg_toolbar->handle_user_command FOR g_grid_02.
    ENDIF.

    PERFORM z_alv_0102.

    wa_layout_02-stylefname = 'CELLTAB'.

    CALL METHOD g_grid_02->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_02
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = it_saida_0102[]
        it_fieldcatalog               = it_fcat_02
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER: lcl_event_handler=>on_data_changed_finished FOR g_grid_02,
                 lcl_event_handler=>on_data_changed FOR g_grid_02.

    CALL METHOD g_grid_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid_02->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid_02->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fcat_02.

    CALL METHOD g_grid_02->refresh_table_display
      EXPORTING
        is_stable = wa_stable_02.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ALV_0102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_alv_0102 .

  CLEAR wl_fcat_02.
  REFRESH it_fcat_02[].

  PERFORM preenche_cat_02 USING:
        'SAFRA'         'Ano Safra'          '10'     'X'     ''      ''     ''     ''     ''             '',
        'TP_SAFRA'      'Tipo Safra'         '10'     'X'     ''      ''     ''     ''     'ZSDT0192'     'TP_SAFRA',
        'QTD'           'Quantidade'         '15'     'X'     ''      ''     'X'     ''     ''             ''.
ENDFORM.


FORM preenche_cat_02 USING VALUE(p_campo)
                           VALUE(p_desc)
                           VALUE(p_tam)
                           VALUE(p_edit)
                           VALUE(p_zero)
                           VALUE(p_hot)
                           VALUE(p_sum)
                           VALUE(p_just)
                           VALUE(p_table)
                           VALUE(p_ref_field).

  wl_fcat_02-fieldname = p_campo.
  wl_fcat_02-scrtext_l = p_desc.
  wl_fcat_02-scrtext_m = p_desc.
  wl_fcat_02-scrtext_s = p_desc.
  wl_fcat_02-outputlen = p_tam.
  wl_fcat_02-edit      = p_edit.
  wl_fcat_02-hotspot   = p_hot.
  wl_fcat_02-no_zero   = p_zero.
  wl_fcat_02-do_sum    = p_sum.
  wl_fcat_02-just      = p_just.
  wl_fcat_02-ref_table = p_table.
  wl_fcat_02-ref_field = p_ref_field.


  APPEND wl_fcat_02 TO  it_fcat_02.
ENDFORM.

FORM z_handler_double_click_01  USING  p_row
                                       p_column
                                       p_row_no.

  DATA: lv_safra    TYPE zsdt0194-safra,
        lv_tp_safra TYPE zsdt0194-tp_safra.

  REFRESH: it_saida_0102, it_aux_0101.
  CLEAR  wa_sd_0101.

  IF p_safra IS NOT INITIAL.
    lv_tp_safra = '1'.
  ELSEIF p_safrin IS NOT INITIAL.
    lv_tp_safra = '2'.
  ENDIF.

  IF NOT p_row  CA 'T'.

    READ TABLE it_saida_0101 INTO wa_saida_0101 INDEX p_row.
    IF sy-subrc = 0.

      CLEAR: wa_zs194_aux, click_alv01.
      click_alv01 = 'X'.

      MOVE-CORRESPONDING wa_saida_0101 TO wa_sd_0101.
      APPEND wa_sd_0101 TO it_sd_0101.
      APPEND wa_saida_0101 TO it_aux_0101.

      wa_zs194_aux-ebeln  =  wa_saida_0101-ebeln.
      wa_zs194_aux-ebelp  =  wa_saida_0101-ebelp.
      wa_zs194_aux-matnr  =  wa_saida_0101-matnr.
      wa_zs194_aux-werks  =  wa_saida_0101-werks.

      SELECT *
        FROM zsdt0194 INTO TABLE it_zs194
       WHERE ebeln EQ wa_saida_0101-ebeln
       AND   ebelp EQ wa_saida_0101-ebelp
       AND   werks EQ wa_saida_0101-werks.

      IF it_zs194 IS NOT INITIAL.

        LOOP AT it_zs194 INTO wa_zs194.

          wa_saida_0102-ebeln     =   wa_zs194-ebeln.
          wa_saida_0102-ebelp     =   wa_zs194-ebelp.
          wa_saida_0102-safra     =   wa_zs194-safra.
          wa_saida_0102-tp_safra  =   wa_zs194-tp_safra.
          wa_saida_0102-qtd       =   wa_zs194-qtd.

          FREE wa_saida_0102-celltab.
          gt_estilo_02 =  VALUE #( ( fieldname = 'SAFRA'    style = cl_gui_alv_grid=>mc_style_disabled )
                                   ( fieldname = 'TP_SAFRA' style = cl_gui_alv_grid=>mc_style_disabled )
                                   ( fieldname = 'QTD'      style = cl_gui_alv_grid=>mc_style_disabled ) ).
          INSERT LINES OF gt_estilo_02 INTO TABLE wa_saida_0102-celltab.

          APPEND wa_saida_0102 TO it_saida_0102.
          CLEAR: wa_zs194, wa_saida_0102.
        ENDLOOP.

        LOOP AT  it_saida_02 INTO wa_saida_02 WHERE  ebeln = wa_saida_0101-ebeln
                                               AND   ebelp = wa_saida_0101-ebelp.
          wa_saida_0102-ebeln     =   wa_saida_02-ebeln.
          wa_saida_0102-ebelp     =   wa_saida_02-ebelp.
          wa_saida_0102-safra     =   wa_saida_02-safra.
          wa_saida_0102-tp_safra  =   wa_saida_02-tp_safra.
          wa_saida_0102-qtd       =   wa_saida_02-qtd.

          FREE wa_saida_0102-celltab.
          gt_estilo_02 =  VALUE #( ( fieldname = 'SAFRA'    style = cl_gui_alv_grid=>mc_style_disabled )
                                   ( fieldname = 'TP_SAFRA' style = cl_gui_alv_grid=>mc_style_disabled )
                                   ( fieldname = 'QTD'      style = cl_gui_alv_grid=>mc_style_disabled ) ).
          INSERT LINES OF gt_estilo_02 INTO TABLE wa_saida_0102-celltab.

          APPEND wa_saida_0102 TO it_saida_0102.
          CLEAR: wa_saida_02, wa_saida_0102.
        ENDLOOP.

      ELSE.

        LOOP AT  it_saida_02 INTO wa_saida_02 WHERE  ebeln = wa_saida_0101-ebeln
                                               AND   ebelp = wa_saida_0101-ebelp.
          wa_saida_0102-ebeln     =   wa_saida_02-ebeln.
          wa_saida_0102-ebelp     =   wa_saida_02-ebelp.
          wa_saida_0102-safra     =   wa_saida_02-safra.
          wa_saida_0102-tp_safra  =   wa_saida_02-tp_safra.
          wa_saida_0102-qtd       =   wa_saida_02-qtd.

          FREE wa_saida_0102-celltab.
          gt_estilo_02 =  VALUE #( ( fieldname = 'SAFRA'    style = cl_gui_alv_grid=>mc_style_disabled )
                                   ( fieldname = 'TP_SAFRA' style = cl_gui_alv_grid=>mc_style_disabled )
                                   ( fieldname = 'QTD'      style = cl_gui_alv_grid=>mc_style_disabled ) ).
          INSERT LINES OF gt_estilo_02 INTO TABLE wa_saida_0102-celltab.

          APPEND wa_saida_0102 TO it_saida_0102.
          CLEAR: wa_saida_02, wa_saida_0102.

        ENDLOOP.
      ENDIF.

      CALL METHOD g_grid_02->refresh_table_display.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SALVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_salvar_dados.
  DATA soma TYPE zsdt0194-qtd.
  DATA erro TYPE c.
  DATA w194 TYPE zsdt0194.
  DATA: lv_erro  TYPE c.

  erro = abap_false.
  CLEAR soma.

  DATA(lt_101) = it_saida_0101.
  SORT lt_101 BY ebeln ebelp.

  LOOP AT it_saida_02 INTO wa_saida_02.

    MOVE-CORRESPONDING wa_saida_02 TO wa_zs194_aux.

    wa_zs194_aux-mandt       = sy-mandt.
    wa_zs194_aux-usnam       = sy-uname.
    wa_zs194_aux-data_atual  = sy-datum.
    wa_zs194_aux-hora_atual  = sy-uzeit.

    DATA(lt_saida_02) = it_saida_02.
    SORT lt_saida_02 BY ebeln ebelp .
    DELETE ADJACENT DUPLICATES FROM lt_saida_02 COMPARING ebeln ebelp.
    IF lt_saida_02 IS NOT INITIAL.

      SELECT *
        FROM zsdt0194
        INTO TABLE @DATA(lt_0194)
        FOR ALL ENTRIES IN @lt_saida_02
        WHERE ebeln = @lt_saida_02-ebeln
          AND ebelp = @lt_saida_02-ebelp.
      IF sy-subrc IS INITIAL.
        SORT lt_0194 BY ebeln ebelp werks.
      ENDIF.

    ENDIF.

    LOOP AT it_saida_02 INTO DATA(wz194)
      WHERE ebeln = wa_saida_02-ebeln
      AND   ebelp = wa_saida_02-ebelp.

*      SELECT SINGLE * FROM zsdt0194 INTO @DATA(w194)
*      WHERE ebeln = @wa_saida_02-ebeln
*      AND   ebelp = @wa_saida_02-ebelp.
      READ TABLE lt_101 ASSIGNING FIELD-SYMBOL(<fs_101>)
      WITH KEY ebeln = wa_saida_02-ebeln
               ebelp = wa_saida_02-ebelp
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        IF wz194-qtd > 0 AND <fs_101>-menge < 0.
          MESSAGE 'Não é possível classificar com valor positivo, valor do pedido é negativo!' TYPE 'S' DISPLAY LIKE 'E'.
          erro = abap_true.
          EXIT.
        ENDIF.

        READ TABLE lt_0194 TRANSPORTING NO FIELDS
        WITH KEY ebeln = wa_saida_02-ebeln
                 ebelp = wa_saida_02-ebelp
                 werks = <fs_101>-werks
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_0194 ASSIGNING FIELD-SYMBOL(<fs_0194>) FROM sy-tabix.
            IF <fs_0194>-ebelp <> wa_saida_02-ebelp OR
               <fs_0194>-ebeln <>  wa_saida_02-ebeln OR
               <fs_0194>-werks <> <fs_101>-werks.
              EXIT.
            ENDIF.

*            soma = ( soma + ( wz194-qtd + <fs_0194>-qtd ) ).
            soma = soma + <fs_0194>-qtd.

          ENDLOOP.

          soma = soma + wz194-qtd.

        ELSE.

          soma =  soma + wz194-qtd.

        ENDIF.

      ENDIF.

*      IF sy-subrc = 0.
*        soma = ( soma + ( wz194-qtd + w194-qtd ) ).
*      ELSE.
*        soma =  wz194-qtd.
*      ENDIF.
    ENDLOOP.

    CHECK erro IS INITIAL.

    READ TABLE it_saida_0101 INTO wa_saida_0101 WITH KEY ebeln = wa_saida_02-ebeln
                                                         ebelp = wa_saida_02-ebelp.
    wa_zs194_aux-matnr = wa_saida_0101-matnr.
    wa_zs194_aux-werks = wa_saida_0101-werks.

    IF soma < 0.
      soma = soma * -1.
    ENDIF.

    IF wa_saida_0101-menge < 0.
      wa_saida_0101-menge = wa_saida_0101-menge * -1.
    ENDIF.

    IF soma  > wa_saida_0101-menge.
      erro = abap_true.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |Total da classificação superior a quantidade do |
                                             |Pedido { wa_saida_0101-ebeln } Item { wa_saida_0101-ebelp } ! | .
      EXIT.
    ELSE.
      APPEND wa_zs194_aux TO it_zs194_aux.
    ENDIF.

* -US 136655-18-06-2024-#136655-RJF-inicio
    IF wa_saida_02-tp_safra = '1'.
      wa_sd_0101-qtd_safra = wa_sd_0101-qtd_safra + wa_saida_02-qtd.
    ENDIF.

    IF wa_saida_02-tp_safra = '2'.
      wa_sd_0101-qtd_safrinha = wa_sd_0101-qtd_safrinha + wa_saida_02-qtd.
    ENDIF.
* -US 136655-18-06-2024-#136655-RJF-fim

    CLEAR:  wa_saida_0101, wa_zs194_aux, wa_saida_02, soma.
  ENDLOOP.

  IF erro = abap_false.

    MODIFY zsdt0194 FROM TABLE it_zs194_aux.
    MESSAGE 'Registro salvo com Sucesso!' TYPE 'S'.

    SELECT * FROM zsdt0194 INTO TABLE @DATA(t194)
     WHERE ebeln EQ @wa_sd_0101-ebeln
     AND   ebelp EQ @wa_sd_0101-ebelp
     AND   werks EQ @wa_sd_0101-werks.

    LOOP AT t194 INTO w194.
      soma = soma + w194-qtd.
    ENDLOOP.

    IF soma <> wa_sd_0101-menge.

      CLEAR: wa_sd_0101-cor, ls_col_01.
      REFRESH lt_coltab_01.

      ls_col_01-fname      = 'EBELN'.
      ls_col_01-color-col  = '6'.
      APPEND ls_col_01 TO lt_coltab_01.
      wa_sd_0101-cor = lt_coltab_01.

    ELSE.

      CLEAR: wa_sd_0101-cor, ls_col_01.
      REFRESH lt_coltab_01.
    ENDIF.

    DELETE it_saida_0101 WHERE ebeln = wa_sd_0101-ebeln AND
                               ebelp = wa_sd_0101-ebelp.
    APPEND wa_sd_0101 TO it_saida_0101.


    REFRESH: it_zs194_aux, it_zsdt0192, it_saida_0102, it_saida_02.

    PERFORM z_clear_variavel.
    PERFORM z_busca_dados.

    CALL METHOD g_grid_01->refresh_table_display
      EXPORTING
        is_stable = wa_stable_01.

    CALL METHOD g_grid->refresh_table_display.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_sort .
  CLEAR fs_sort.
  REFRESH t_sort.
  fs_sort-spos      ='1'.
  fs_sort-fieldname = 'GRUPO'.
  fs_sort-down      = 'X'.
  fs_sort-subtot    = 'X'.
  APPEND fs_sort TO t_sort.
  CLEAR fs_sort.
ENDFORM.

FORM z_sort_cad .
  CLEAR fs_sort.
  REFRESH t_sort.
  fs_sort-spos      ='1'.
  fs_sort-fieldname = 'USNAM'.
  fs_sort-down      = ' '.
  fs_sort-subtot    = ' '.
  APPEND fs_sort_cad TO t_sort_cad.
  CLEAR fs_sort_cad.
ENDFORM.


FORM r_imposto_item USING w_lifnr
                          w_werks
                          w_ebelp
                          w_ebeln
                   CHANGING w_valor.

  DATA: wa_ite  LIKE mepoitem.
  DATA: w_netwr  TYPE komp-netwr.
  CLEAR wa_ite.

  CALL FUNCTION 'MEPO_DOC_ITEM_GET'
    EXPORTING
      im_ebelp = w_ebelp                                    "'00010'
    IMPORTING
      ex_item  = wa_ite
    EXCEPTIONS
      failure  = 1
      OTHERS   = 2.

  DATA: BEGIN OF t_konv OCCURS 0.
          INCLUDE STRUCTURE konv.
  DATA: END OF t_konv.
  TYPES: ty_konv TYPE TABLE OF komv.

  FIELD-SYMBOLS: <wmwst> TYPE any,
                 <lfa1>  TYPE lfa1,
                 <ekpo>  TYPE ekpo,
                 <ek2>   TYPE ekpo,
                 <ekko>  TYPE ekko,
                 <vorga> TYPE any,
                 <konv>  TYPE ty_konv,
                 <cva>   TYPE any.

  ASSIGN ('(SAPLMEPO)ekpo') TO <ekpo>.
  ASSIGN ('(SAPLMEPO)ekko') TO <ekko>.
  ASSIGN ('(SAPLMEPO)lfa1') TO <lfa1>.

  SELECT SINGLE * FROM ekpo INTO <ekpo>
    WHERE ebeln = w_ebeln AND
          ebelp = w_ebelp.


  SELECT SINGLE * FROM ekko INTO <ekko>
    WHERE ebeln = w_ebeln.

  SELECT SINGLE * FROM lfa1 INTO <lfa1>
    WHERE lifnr = <ekko>-lifnr.

  TRY.


      cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
        EXPORTING it_selection_attribute = VALUE #(
       ( fieldname = 'KNUMV' value = <ekko>-knumv )
       )
        IMPORTING et_prc_element_classic_format = DATA(etl2648c2r9536) ).
      t_konv[] = etl2648c2r9536.
    CATCH cx_prc_result .
      sy-subrc = 4.
  ENDTRY.

  ASSIGN ('(SAPLMEPO)fc_vorga') TO <vorga>.
  ASSIGN ('(SAPLMEPO)cva_en') TO <cva>.
  ASSIGN ('(SAPLMEPO)tkomv[]') TO <konv>.

  <vorga> = <cva>.

  PERFORM kond_taxes(saplmepo) USING 'D' 'X'.

  ASSIGN ('(SAPLMEPO)taxcom-WMWST') TO <wmwst>.

  w_netwr = <ekpo>-netwr.

  w_valor  = ( w_netwr + <wmwst> ).

  CLEAR: <ekpo>,
         <ekko>,
         <lfa1>.

ENDFORM.


*Início - CS2019001220 - Sara Oikawa - Jun/2020
*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_LEGENDA
*&---------------------------------------------------------------------*
*  CS2019001220 - Sara Oikawa - Jun/2020
*  Remover o cabeçalho do ALV onde atualmente fica a legenda das cores
*  Criar Botão Legenda, e ao clicar nele abrir um pop-up exibindo
*  a legenda (Tela 0900)
*----------------------------------------------------------------------*
FORM z_cria_legenda .

* =====================================================================
* Instância
* =====================================================================
  DATA: v_docking     TYPE REF TO cl_gui_docking_container,
        v_splitter    TYPE REF TO cl_gui_splitter_container,
        v_container_1 TYPE REF TO cl_gui_container.
*     V_CONTAINER_2 TYPE REF TO CL_GUI_CONTAINER.
* =====================================================================

  CLEAR: v_docking, v_splitter, v_container_1.  " V_CONTAINER_2.
  CREATE OBJECT v_docking
    EXPORTING
      repid = sy-repid
      dynnr = sy-dynnr
      ratio = '95'.

* Create a splitter with 2 rows and 1 column
  CREATE OBJECT v_splitter
    EXPORTING
      parent  = v_docking
      rows    = 1
      columns = 1.

** Upper Container
  CALL METHOD v_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = v_container_1.

** Lower Container
*  CALL METHOD V_SPLITTER->GET_CONTAINER
*    EXPORTING
*      ROW       = 2
*      COLUMN    = 1
*    RECEIVING
*      CONTAINER = V_CONTAINER_2.

** Upper Container height

  CALL METHOD v_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 30.

  DATA: document              TYPE REF TO cl_dd_document.

  CREATE OBJECT document.
  document->initialize_document( ).

*  DOCUMENT->NEW_LINE( ).
*  DOCUMENT->ADD_TEXT( TEXT = 'Legenda das cores:' SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_STYLE = CL_DD_AREA=>HEADING ).
*  DOCUMENT->NEW_LINE(  ). DOCUMENT->UNDERLINE( ).


  CALL METHOD document->add_table(
    EXPORTING
      no_of_columns = 1
    IMPORTING
      tablearea     = DATA(_doctable2) ).


  _doctable2->add_text( text = CONV #( '   ' ) sap_fontsize = cl_dd_area=>large sap_color = cl_dd_area=>list_positive_int fix_lines = '' ).
  _doctable2->add_text( text = CONV #( 'Pedidos Totalmente Classificados' ) ).
  _doctable2->new_row( ).

  _doctable2->add_text( text = CONV #( '   ')  sap_fontsize = cl_dd_area=>large sap_color = cl_dd_area=>list_total_int fix_lines = '' ).
  _doctable2->add_text( text = CONV #( 'Pedidos Parcialmente Classificados') ).
  _doctable2->new_row( ).

  _doctable2->add_text( text = CONV #( '   ' ) sap_fontsize = cl_dd_area=>large sap_color = cl_dd_area=>list_negative_int fix_lines = '' ).
  _doctable2->add_text( text = CONV #( 'Pedidos sem nenhuma Classificação' ) ).
  _doctable2->new_row( ).


  document->merge_document( ).
  document->display_document( parent = v_container_1 ).


*  PERFORM ZF_PREPARAR_HEADER.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_ALV_0110
*&---------------------------------------------------------------------*
*  CS2019001220 - Sara Oikawa - Jun/2020
*  Ao Clicar sobre a Coluna “Código Material”  no ALV principal,
*  deverá abrir-se uma tela, listando todos os ‘Produtos Acabados’
*  que possuem a Matéria-prima em sua fórmula (lista técnica)
*  Só considerar os Produtos Acabado cujas listas técnicas estão ativas,
*  caso encontre mais de uma verificar qual é a vigente.
*  Layout da Tela (0110):
*  ALV Superior: Exibir as informações dos Produtos acabados que utilizam
*  a matéria-prima;
*  ALV Inferior: Exibir a lista técnica; (para exibir as informações
*  neste ALV o usuário deverá clicar sobre a linha do ALV superior);*
*----------------------------------------------------------------------*
FORM z_cria_alv_0110.

  DATA: wa_event_01 TYPE REF TO lcl_event_receiver_01,
        obg_toolbar TYPE REF TO lcl_alv_toolbar.


  IF g_custom_container_03 IS INITIAL.

    CREATE OBJECT g_custom_container_03
      EXPORTING
        container_name              = 'CONTAINER03'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    IF g_grid_03 IS INITIAL AND  g_custom_container_03 IS NOT INITIAL.
      CREATE OBJECT g_grid_03
        EXPORTING
          i_parent          = g_custom_container_03
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    IF wa_event_01 IS INITIAL.
      CREATE OBJECT wa_event_01.
      SET HANDLER wa_event_01->zm_handle_click_01   FOR g_grid_03.
      SET HANDLER wa_event_01->catch_hotspot2       FOR g_grid_03.
    ENDIF.

*    WA_LAYOUT_01-CTAB_FNAME = 'COR'.
*    WA_LAYOUT_01-STYLEFNAME = 'CELLTAB'.

    wa_layout_01-cwidth_opt = 'X'.


    CALL METHOD g_grid_03->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_01
      CHANGING
        it_outtab                     = it_saida_0110[]
        it_fieldcatalog               = it_fcat_01
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.

    CALL METHOD g_grid_03->refresh_table_display
      EXPORTING
        is_stable = wa_stable_01.
  ENDIF.


  IF g_custom_container_04 IS INITIAL.

    CREATE OBJECT g_custom_container_04
      EXPORTING
        container_name              = 'CONTAINER04'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.


    IF g_grid_04 IS INITIAL AND  g_custom_container_04 IS NOT INITIAL.
      CREATE OBJECT g_grid_04
        EXPORTING
          i_parent          = g_custom_container_04
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_CHECK.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_FC_REFRESH.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
*
*    IF OBG_TOOLBAR IS INITIAL.
*      CREATE OBJECT OBG_TOOLBAR
*        EXPORTING
*          IO_ALV_GRID = G_GRID_04.
*
*      SET HANDLER: OBG_TOOLBAR->ON_TOOLBAR FOR G_GRID_04,
*                   OBG_TOOLBAR->HANDLE_USER_COMMAND FOR G_GRID_04.
*    ENDIF.

    PERFORM z_alv_0111.

    wa_layout_03-stylefname = 'CELLTAB'.
    wa_layout_03-cwidth_opt = 'X'.


    CALL METHOD g_grid_04->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_03
      CHANGING
        it_outtab                     = it_saida_0111[]
        it_fieldcatalog               = it_fcat_03
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.

    CALL METHOD g_grid_04->refresh_table_display
      EXPORTING
        is_stable = wa_stable_01.
  ENDIF.

  SET HANDLER: lcl_event_handler=>on_data_changed_finished FOR g_grid_04,
               lcl_event_handler=>on_data_changed FOR g_grid_04.

*    CALL METHOD G_GRID_04->REGISTER_EDIT_EVENT

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_ALV_0120
*&---------------------------------------------------------------------*
*  CS2019001220 - Sara Oikawa - Jun/2020
* Ao Clicar sobre a Coluna “OV em Carteira”, será feita leitura da tabela
* alimentada pelo Programa ZSDR0102 (Job Necessidades de Matéria-Prima),
* listando todas as Ordens de Venda que foram consideradas na totalização
* das colunas “OV em Carteira” \ “Entregas Efetivadas” \ “Saldo a Entregar”
*----------------------------------------------------------------------*
FORM z_cria_alv_0120 .

  DATA: wa_event_01 TYPE REF TO lcl_event_receiver_01,
        obg_toolbar TYPE REF TO lcl_alv_toolbar.

  IF g_grid_05 IS BOUND.
    CALL METHOD g_grid_05->free.
  ENDIF.

  IF g_custom_container_05 IS BOUND. "NOT INITIAL.
    CALL METHOD g_custom_container_05->free.
  ENDIF.
*
****  IF G_CUSTOM_CONTAINER_05 IS BOUND.

  CREATE OBJECT g_custom_container_05
    EXPORTING
      container_name              = 'CONTAINER05'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

****    IF G_GRID_05 IS INITIAL AND  G_CUSTOM_CONTAINER_05 IS NOT INITIAL.
  CREATE OBJECT g_grid_05
    EXPORTING
      i_parent          = g_custom_container_05
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.
****    ENDIF.

  wa_layout_01-ctab_fname = 'COR'.
*    WA_LAYOUT_01-STYLEFNAME = 'CELLTAB'.

  wa_layout_01-cwidth_opt = 'X'.

  IF wa_event_01 IS INITIAL.

    CREATE OBJECT wa_event_01.

    SET HANDLER wa_event_01->zm_handle_click_01   FOR g_grid_05.
    SET HANDLER: wa_event_01->catch_hotspot2 FOR g_grid_05.

  ENDIF.

  CALL METHOD g_grid_05->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_01
      i_bypassing_buffer            = 'X'
    CHANGING
      it_outtab                     = it_saida_0120[]
      it_fieldcatalog               = it_fcat_01
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
***  ELSE.

*    CALL METHOD G_GRID_05->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = IT_FCAT_01.

***    CALL METHOD G_GRID_05->REFRESH_TABLE_DISPLAY
***      EXPORTING
***        IS_STABLE = WA_STABLE_01.
***
***  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLER_CLICK_HOTSPOT
*&---------------------------------------------------------------------*
*   Tratar Hotspot´s
*---------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMNES_ROW_NO  text
*----------------------------------------------------------------------*
FORM z_handler_click_hotspot USING  p_row
                               p_column
                               p_row_no.

  TYPES:
    BEGIN OF ty_agrup,
      ebeln TYPE zsdt0194-ebeln,
      ebelp TYPE zsdt0194-ebelp,
      matnr TYPE zsdt0194-matnr,
      werks TYPE zsdt0194-werks,
      qtd   TYPE zsdt0194-qtd,
    END OF ty_agrup,

    BEGIN OF ty_agrup_tp,
      ebeln TYPE zsdt0194-ebeln,
      ebelp TYPE zsdt0194-ebelp,
      matnr TYPE zsdt0194-matnr,
      werks TYPE zsdt0194-werks,
      tipo  TYPE zsdt0194-tp_safra,
      qtd   TYPE zsdt0194-qtd,
    END OF ty_agrup_tp.

  DATA valor LIKE wa_saida_0101-vlr_item_pedido.
  DATA: soma        TYPE zsdt0194-qtd,
        lv_tp_safra TYPE zsdt0194-tp_safra,
        lv_qtd_deb  TYPE ekbe-menge,
        lv_qtd_cred TYPE ekbe-menge,
        lt_agrup    TYPE TABLE OF ty_agrup,
        lt_agrup_tp TYPE TABLE OF ty_agrup_tp,
        ls_agrup    TYPE ty_agrup,
        ls_agrup_tp TYPE ty_agrup_tp,
        lv_menge    TYPE ekbe-menge,
        lv_soma     TYPE ekbe-menge,
        w_ekpo      TYPE ekpo.

  CASE p_column.

    WHEN 'TOTAL_ESTQ'.

      IF NOT p_row  CA 'S' AND NOT p_row  CA 'T' .

        READ TABLE it_saida INTO wa_saida INDEX p_row.
        IF sy-subrc = 0.

          SELECT *
             FROM zsdt0193 INTO TABLE @DATA(it_zt0193)
            WHERE matnr EQ @wa_saida-matnr
             AND  werks IN @p_werks.

          CHECK it_zt0193 IS NOT INITIAL.

          SELECT *
            FROM ekpo INTO TABLE @DATA(it_ekpo)
            FOR ALL ENTRIES IN @it_zt0193
           WHERE ebeln EQ @it_zt0193-ebeln
            AND  ebelp EQ @it_zt0193-ebelp.

          SELECT *
            FROM ekko INTO TABLE @DATA(it_ekko)
            FOR ALL ENTRIES IN @it_zt0193
           WHERE ebeln EQ @it_zt0193-ebeln.

          SELECT *
            FROM  t001l INTO TABLE @DATA(it_t001l)
            FOR ALL ENTRIES IN @it_ekpo
            WHERE lgort EQ @it_ekpo-lgort.

          DATA(lt_zt0193) = it_zt0193.
          SORT lt_zt0193 BY ebeln ebelp matnr werks.
          DELETE ADJACENT DUPLICATES FROM lt_zt0193 COMPARING ebeln ebelp matnr werks.
          IF lt_zt0193 IS NOT INITIAL.

            SELECT *
              FROM zsdt0194
              INTO TABLE @DATA(lt_zsdt0194)
              FOR ALL ENTRIES IN @lt_zt0193
              WHERE ebeln = @lt_zt0193-ebeln
                AND ebelp = @lt_zt0193-ebelp
                AND matnr = @lt_zt0193-matnr
                AND werks = @lt_zt0193-werks.
            IF sy-subrc IS INITIAL.
              SORT lt_zsdt0194 BY ebeln ebelp matnr werks.

              LOOP AT lt_zsdt0194 ASSIGNING FIELD-SYMBOL(<fs_zsdt0194>).

                ls_agrup-ebeln = <fs_zsdt0194>-ebeln.
                ls_agrup-ebelp = <fs_zsdt0194>-ebelp.
                ls_agrup-matnr = <fs_zsdt0194>-matnr.
                ls_agrup-werks = <fs_zsdt0194>-werks.
                ls_agrup-qtd   = <fs_zsdt0194>-qtd.
                COLLECT ls_agrup INTO lt_agrup.
                CLEAR ls_agrup.

                ls_agrup_tp-ebeln = <fs_zsdt0194>-ebeln.
                ls_agrup_tp-ebelp = <fs_zsdt0194>-ebelp.
                ls_agrup_tp-matnr = <fs_zsdt0194>-matnr.
                ls_agrup_tp-werks = <fs_zsdt0194>-werks.
                ls_agrup_tp-tipo  = <fs_zsdt0194>-tp_safra.
                ls_agrup_tp-qtd   = <fs_zsdt0194>-qtd.
                COLLECT ls_agrup_tp INTO lt_agrup_tp.
                CLEAR ls_agrup_tp.
              ENDLOOP.

              SORT lt_agrup BY ebeln ebelp matnr werks.
              SORT lt_agrup_tp BY ebeln ebelp matnr werks tipo.

            ENDIF.

          ENDIF.

          lt_zt0193 = it_zt0193.
          SORT lt_zt0193 BY ebeln.
          DELETE ADJACENT DUPLICATES FROM lt_zt0193 COMPARING ebeln.

          IF lt_zt0193 IS NOT INITIAL.

            SELECT *
              FROM ekbe
              INTO TABLE @DATA(lt_ekbe)
              FOR ALL ENTRIES IN @lt_zt0193
              WHERE ebeln = @lt_zt0193-ebeln
                AND vgabe = 2.
*                AND status = 'S'.
            IF sy-subrc IS INITIAL.
              SORT lt_ekbe BY ebeln.
            ENDIF.

          ENDIF.

          IF p_safra IS NOT INITIAL.
            lv_tp_safra = '1'.
          ELSEIF p_safrin IS NOT INITIAL.
            lv_tp_safra = '2'.
          ENDIF.
          LOOP AT it_zt0193 INTO  DATA(wa_zt0193).
            CLEAR: lv_menge.
            CLEAR: w_ekpo, wa_saida_0101, valor, ls_col_01, soma, lv_qtd_deb, lv_qtd_cred.
            REFRESH lt_coltab_01.

            READ TABLE it_ekpo INTO w_ekpo WITH KEY ebeln = wa_zt0193-ebeln
                                                    ebelp = wa_zt0193-ebelp.
            IF sy-subrc = 0.

              IF w_ekpo-menge > 1.

                DATA(lt_zsdt0194_aux) = lt_zsdt0194.
                IF lv_tp_safra IS NOT INITIAL.
                  DELETE lt_zsdt0194_aux WHERE ebeln <> wa_zt0193-ebeln OR ebelp <> wa_zt0193-ebelp OR safra NOT IN p_gsafra OR tp_safra NE lv_tp_safra.
                ELSE.
                  DELETE lt_zsdt0194_aux WHERE ebeln <> wa_zt0193-ebeln OR ebelp <> wa_zt0193-ebelp OR safra NOT IN p_gsafra.
                ENDIF.

                IF lt_zsdt0194_aux IS INITIAL.

                  READ TABLE lt_zsdt0194 TRANSPORTING NO FIELDS
                  WITH KEY ebeln = wa_zt0193-ebeln
                           ebelp = wa_zt0193-ebelp
                           matnr = wa_zt0193-matnr
                           werks = wa_zt0193-werks
                  BINARY SEARCH.
                  IF sy-subrc IS INITIAL.

                    READ TABLE lt_agrup ASSIGNING FIELD-SYMBOL(<fs_agrup>)
                    WITH KEY ebeln = wa_zt0193-ebeln
                             ebelp = wa_zt0193-ebelp
                             matnr = wa_zt0193-matnr
                             werks = wa_zt0193-werks
                    BINARY SEARCH.
                    IF sy-subrc IS INITIAL.

                      lv_menge = <fs_agrup>-qtd.
                      IF lv_menge < 0.
                        lv_menge = lv_menge * -1.
                      ENDIF.
                      IF lv_menge >= w_ekpo-menge.
                        CONTINUE.
                      ENDIF.

                    ENDIF.

                  ENDIF.

                ENDIF.

                READ TABLE lt_agrup_tp ASSIGNING FIELD-SYMBOL(<fs_agrup_tp>)
                WITH KEY ebeln = wa_zt0193-ebeln
                         ebelp = wa_zt0193-ebelp
                         tipo  = '1'.
                IF sy-subrc IS INITIAL.
                  wa_saida_0101-qtd_safra = <fs_agrup_tp>-qtd.
                ENDIF.

                READ TABLE lt_agrup_tp ASSIGNING <fs_agrup_tp>
                WITH KEY ebeln = wa_zt0193-ebeln
                         ebelp = wa_zt0193-ebelp
                         tipo  = '2'.
                IF sy-subrc IS INITIAL.
                  wa_saida_0101-qtd_safrinha = <fs_agrup_tp>-qtd.
                ENDIF.

                wa_saida_0101-ebeln  = wa_zt0193-ebeln.
                wa_saida_0101-ebelp  = wa_zt0193-ebelp.
                wa_saida_0101-matnr  = wa_zt0193-matnr.
                wa_saida_0101-tp_ped = wa_zt0193-tp_ped.
                wa_saida_0101-meins  = w_ekpo-meins.
                wa_saida_0101-werks  = wa_zt0193-werks.
                wa_saida_0101-lgort  = w_ekpo-lgort.

                READ TABLE it_t001l INTO DATA(wa_t001l) WITH KEY lgort = w_ekpo-lgort.
                IF sy-subrc = 0.
                  wa_saida_0101-lgobe = wa_t001l-lgobe.
                ENDIF.

                READ TABLE it_ekko INTO DATA(wekko) WITH KEY ebeln = wa_zt0193-ebeln.
                IF sy-subrc = 0.
                  wa_saida_0101-aedar  = wekko-aedat.
                  wa_saida_0101-verkf = wekko-verkf.
                ENDIF.

                PERFORM r_imposto_item USING  wekko-lifnr
                                              w_ekpo-werks
                                              w_ekpo-ebelp
                                              w_ekpo-ebeln
                                       CHANGING valor.

                wa_saida_0101-netpr  = w_ekpo-netpr.
                wa_saida_0101-bprme  = w_ekpo-bprme.


                IF wa_saida_0101-tp_ped = 'T'.
                  wa_saida_0101-menge           = w_ekpo-menge * -1.
                  wa_saida_0101-vlr_item_pedido = valor * -1.
                ELSE.
                  wa_saida_0101-menge           = w_ekpo-menge.
                  wa_saida_0101-vlr_item_pedido = valor.
                ENDIF.

                IF wekko-bsart = 'ZUB'.
                  wa_saida_0101-lifnr           = |{ wekko-reswk ALPHA = IN }|.
                ELSE.
                  wa_saida_0101-lifnr           = wekko-lifnr.
                ENDIF.

                SELECT SINGLE name1 FROM lfa1 INTO wa_saida_0101-name1
                  WHERE lifnr EQ wa_saida_0101-lifnr.

                IF wa_zt0193-tp_ped <> 'T'.

                  SELECT  *  FROM zsdt0194 INTO TABLE @DATA(t194)
                    WHERE ebeln EQ @wa_saida_0101-ebeln
                    AND   ebelp EQ @wa_saida_0101-ebelp
                    AND   werks EQ @wa_zt0193-werks.

                  LOOP AT t194 INTO DATA(w194).
                    soma = soma + w194-qtd.
                  ENDLOOP.

                  IF soma < wa_saida_0101-menge.

                    CLEAR: wa_saida_0101-cor, ls_col_01.
                    REFRESH lt_coltab_01.

                    ls_col_01-fname      = 'EBELN'.
                    ls_col_01-color-col  = '6'.
                    APPEND ls_col_01 TO lt_coltab_01.
                    wa_saida_0101-cor = lt_coltab_01.

                  ELSEIF soma > wa_saida_0101-menge.

                    CLEAR: wa_saida_0101-cor, ls_col_01.
                    REFRESH lt_coltab_01.

                    ls_col_01-fname      = 'EBELN'.
                    ls_col_01-color-col  = '3'.
                    APPEND ls_col_01 TO lt_coltab_01.
                    wa_saida_0101-cor = lt_coltab_01.
                  ENDIF.

                ELSE.

                  SELECT  *  FROM zsdt0194 INTO TABLE t194
                    WHERE ebeln EQ wa_saida_0101-ebeln
                    AND   ebelp EQ wa_saida_0101-ebelp
                    AND   werks EQ wa_zt0193-werks.

                  LOOP AT t194 INTO w194.
                    soma = soma + w194-qtd.
                  ENDLOOP.

                  IF wa_saida_0101-menge < 0.
                    lv_menge = wa_saida_0101-menge * -1.
                  ELSE.
                    lv_menge = wa_saida_0101-menge.
                  ENDIF.

                  IF soma < 0 .
                    lv_soma = soma * -1.
                  ELSE.
                    lv_soma = soma.
                  ENDIF.

                  IF lv_soma < lv_menge.

                    CLEAR: wa_saida_0101-cor, ls_col_01.
                    REFRESH lt_coltab_01.

                    ls_col_01-fname      = 'EBELN'.
                    ls_col_01-color-col  = '6'.
                    APPEND ls_col_01 TO lt_coltab_01.
                    wa_saida_0101-cor = lt_coltab_01.

                  ELSEIF lv_soma > lv_menge.

                    CLEAR: wa_saida_0101-cor, ls_col_01.
                    REFRESH lt_coltab_01.

                    ls_col_01-fname      = 'EBELN'.
                    ls_col_01-color-col  = '3'.
                    APPEND ls_col_01 TO lt_coltab_01.
                    wa_saida_0101-cor = lt_coltab_01.
                  ENDIF.

                ENDIF.

                READ TABLE lt_ekbe TRANSPORTING NO FIELDS
                WITH KEY ebeln = wa_zt0193-ebeln
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  LOOP AT lt_ekbe ASSIGNING FIELD-SYMBOL(<fs_ekbe>) FROM sy-tabix.
                    IF <fs_ekbe>-ebeln NE wa_zt0193-ebeln.
                      EXIT.
                    ENDIF.

                    IF <fs_ekbe>-shkzg EQ 'S'.
                      lv_qtd_deb = lv_qtd_deb + <fs_ekbe>-menge.
                    ELSEIF <fs_ekbe>-shkzg EQ 'H'.
                      lv_qtd_cred = lv_qtd_cred + <fs_ekbe>-menge.
                    ENDIF.

                  ENDLOOP.

                ENDIF.

                wa_saida_0101-qtd_fatur =  lv_qtd_deb - lv_qtd_cred.
                wa_saida_0101-qtd_saldo =  wa_saida_0101-menge - wa_saida_0101-qtd_fatur.

                APPEND wa_saida_0101 TO it_saida_0101.
                CLEAR: wa_zt0193.
*                CLEAR: wa_zt0193 , w_ekpo, wa_saida_0101, valor, ls_col_01, soma, lv_qtd_deb, lv_qtd_cred.
*                REFRESH lt_coltab_01.
              ENDIF.
            ENDIF.

          ENDLOOP.

          CALL SCREEN 0101.
        ENDIF.
      ENDIF.

    WHEN 'GRUPO'.
      IF NOT p_row  CA 'S' AND  NOT p_row  CA 'T'.
        READ TABLE it_saida INTO wa_saida INDEX p_row.
        IF sy-subrc = 0.

          IF wa_saida-grupo IS NOT INITIAL.

            SELECT *
              FROM zsdt0196 INTO TABLE @DATA(it_zt0196)
              WHERE grupo EQ @wa_saida-grupo.

            SELECT *
              FROM zsdt0193 INTO TABLE it_zt0193
              FOR ALL ENTRIES IN it_zt0196
             WHERE matnr EQ it_zt0196-matnr.

            CHECK it_zt0193 IS NOT INITIAL.

            SELECT *
              FROM ekpo INTO TABLE it_ekpo
              FOR ALL ENTRIES IN it_zt0193
             WHERE ebeln EQ it_zt0193-ebeln
              AND  ebelp EQ it_zt0193-ebelp.

            SELECT *
              FROM ekko INTO TABLE it_ekko
              FOR ALL ENTRIES IN it_zt0193
             WHERE ebeln EQ it_zt0193-ebeln.


            LOOP AT it_zt0193 INTO wa_zt0193.

              CLEAR: w_ekpo, wa_saida_0101, valor , ls_col_01, soma.
              REFRESH lt_coltab_01.

              READ TABLE it_ekpo INTO w_ekpo WITH KEY ebeln = wa_zt0193-ebeln
                                                      ebelp = wa_zt0193-ebelp.
              IF sy-subrc = 0.
                wa_saida_0101-ebeln  = wa_zt0193-ebeln.
                wa_saida_0101-ebelp  = wa_zt0193-ebelp.
                wa_saida_0101-matnr  = wa_zt0193-matnr.
                wa_saida_0101-tp_ped = wa_zt0193-tp_ped.
                wa_saida_0101-werks  = wa_zt0193-werks.
                wa_saida_0101-meins  = w_ekpo-meins.


                READ TABLE it_ekko INTO wekko WITH KEY ebeln = wa_zt0193-ebeln.
                IF sy-subrc = 0.
                  wa_saida_0101-aedar  = wekko-aedat.
                ENDIF.

                PERFORM r_imposto_item USING  wekko-lifnr
                                              w_ekpo-werks
                                              w_ekpo-ebelp
                                              w_ekpo-ebeln
                                       CHANGING valor.

                wa_saida_0101-netpr  = w_ekpo-netpr.
                wa_saida_0101-bprme  = w_ekpo-bprme.

                IF wa_saida_0101-tp_ped = 'T'.
                  wa_saida_0101-menge           = w_ekpo-menge * -1.
                  wa_saida_0101-vlr_item_pedido = valor * -1.
                ELSE.
                  wa_saida_0101-menge           = w_ekpo-menge.
                  wa_saida_0101-vlr_item_pedido = valor.
                ENDIF.

                IF wekko-bsart = 'ZUB'.
                  wa_saida_0101-lifnr           = |{ wekko-reswk ALPHA = IN }|.
                ELSE.
                  wa_saida_0101-lifnr           = wekko-lifnr.
                ENDIF.

                SELECT SINGLE name1 FROM lfa1 INTO wa_saida_0101-name1
                  WHERE lifnr EQ wa_saida_0101-lifnr.

                IF wa_zt0193-tp_ped <> 'T'.

                  SELECT  *  FROM zsdt0194 INTO TABLE t194
                    WHERE ebeln EQ wa_saida_0101-ebeln
                    AND   ebelp EQ wa_saida_0101-ebelp.

                  LOOP AT t194 INTO w194.
                    soma = soma + w194-qtd.
                  ENDLOOP.

                  IF  soma <> wa_saida_0101-menge.

                    CLEAR: wa_saida_0101-cor, ls_col_01.
                    REFRESH lt_coltab_01.

                    ls_col_01-fname      = 'EBELN'.
                    ls_col_01-color-col  = '6'.
                    APPEND ls_col_01 TO lt_coltab_01.
                    wa_saida_0101-cor = lt_coltab_01.
                  ENDIF.
                ENDIF.

                APPEND wa_saida_0101 TO it_saida_0101.
                CLEAR: wa_zt0193.
*                CLEAR: wa_zt0193 , w_ekpo, wa_saida_0101, valor , ls_col_01, soma.
*                REFRESH lt_coltab_01.
              ENDIF.
            ENDLOOP.
            CALL SCREEN 0101.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'MATNR'.
      " Exibe ALV 0110 Superior - Produto Acabado
      IF NOT p_row  CA 'S' AND NOT p_row  CA 'T' .

        READ TABLE it_saida INTO wa_saida INDEX p_row.

        IF sy-subrc = 0.
          IF  wa_saida-mtart NE 'ZHAW'.   " Somente exibir qdo Materia-Prima
            PERFORM zf_exibe_produto_acabado USING wa_saida-matnr.

            CALL SCREEN 0110.
          ENDIF.

        ENDIF.
      ENDIF.

    WHEN 'MATNR_PA'.
      " Exibe ALV 0110 Inferior - Lista Técnica
      IF NOT p_row  CA 'S' AND NOT p_row  CA 'T' .

        READ TABLE it_saida_0110 INTO wa_saida_0110 INDEX p_row.
        IF sy-subrc = 0.

          PERFORM zf_exibe_lista_tecnica USING wa_saida_0110-matnr_pa
                                               wa_saida_0110-werks.

        ENDIF.
      ENDIF.

    WHEN 'OV_CARTEIRA'.
      " Exibe ALV 0120 - OV´s em Carteira
      IF NOT p_row  CA 'S' AND NOT p_row  CA 'T' .

        READ TABLE it_saida INTO wa_saida INDEX p_row.
        IF sy-subrc = 0.
          PERFORM zf_busca_ordens USING wa_saida-matnr  wa_saida-maktx.
          CALL SCREEN 0120.
        ENDIF.
      ENDIF.

    WHEN 'EBELN'.
      "Chama transação ME23N
      READ TABLE it_saida_0101 INTO wa_saida_0101 INDEX p_row.
      IF sy-subrc = 0.
        SET PARAMETER ID 'BES' FIELD wa_saida_0101-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'VBELN'.
      "Chama transação VA03
      READ TABLE it_saida_0120 INTO wa_saida_0120 INDEX p_row.
      IF sy-subrc = 0.
        SET PARAMETER ID 'AUN' FIELD wa_saida_0120-vbeln.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.          "Z_HANDLER_CLICK_HOTSPOT.


*&---------------------------------------------------------------------*
*&      Form  Z_ALV_0110
*&---------------------------------------------------------------------*
* ALV Superior da tela 0110 (Produto Acabado)
*----------------------------------------------------------------------*
FORM z_alv_0110.
  CLEAR wl_fcat_01.
  REFRESH it_fcat_01[].

  PERFORM preenche_cat_01 USING:

        'MATNR_PA'            'Cód.Prod.Acabado'    '18'      'X'     'X'    ''     ''    ''    '',
        'MAKTX'               'Descrição'           '40'      ''      ''     ''     ''    ''    '',
        'STLNR'               'NºLista Técnica'     '20'      ''      ''     ''     ''    ''    '',
        'WERKS'               'Centro'              '04'      ''      ''     ''     ''    ''    '',
        'BMENG'               'Qtd.Básica'          '15'      ''      ''     ''     ''    ''    '',
        'BMEIN'               'UM Básica'           '03'      ''      ''     ''     ''    ''    '',
        'MATNR_MP'            'Cód.Mat.Prima'       '18'      'X'     ''     ''     ''    ''    '',
        'MAKTX_MP'            'Desc.Mat.Prima'      '40'      ''      ''     ''     ''    ''    '',
        'MENGE'               'Qtd.Mat.Prima'       '15'      ''      ''     ''     ''    ''    '',
        'MEINS'               'UM Mat.Prima'        '03'      ''      ''     ''     ''    ''    ''.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_ALV_0111
*&---------------------------------------------------------------------*
* ALV Inferior da tela 0110 (Produto Acabado)
*----------------------------------------------------------------------*
FORM z_alv_0111.
  CLEAR wl_fcat_01.
  REFRESH it_fcat_03[].

  PERFORM preenche_cat_03 USING:

        'ITEM_NO'             'Item'                '06'      'X'     ''     ''     ''    ''    '',
        'COMPONE'             'Cód.Componente'      '18'      'X'     ''     ''     ''    ''    '',
        'MAKTX'               'Descrição'           '40'      ''      ''     ''     ''    ''    '',
        'COMP_QTY'            'Qtd'                 '15'      ''      ''     ''     ''    ''    '',
        'COMP_UNIT'           'UM'                  '03'      ''      ''     ''     ''    ''    '',
        'VALID_FROM'          'Vál.Desde'           '10'      ''      ''     ''     ''    ''    '',
        'VALID_TO'            'Vál.Até'             '10'      ''      ''     ''     ''    ''    ''.

ENDFORM.

FORM preenche_cat_03 USING VALUE(p_campo)
                           VALUE(p_desc)
                           VALUE(p_tam)
                           VALUE(p_zero)
                           VALUE(p_hot)
                           VALUE(p_sum)
                           VALUE(p_just)
                           VALUE(p_edit)
                           VALUE(p_cor).

  wl_fcat_01-fieldname = p_campo.
  wl_fcat_01-scrtext_l = p_desc.
  wl_fcat_01-scrtext_m = p_desc.
  wl_fcat_01-scrtext_s = p_desc.
  wl_fcat_01-outputlen = p_tam.
  wl_fcat_01-hotspot   = p_hot.
  wl_fcat_01-no_zero   = p_zero.
  wl_fcat_01-do_sum    = p_sum.
  wl_fcat_01-just      = p_just.
  wl_fcat_01-edit      = p_edit.
  wl_fcat_01-emphasize = p_cor.

  APPEND wl_fcat_01 TO  it_fcat_03.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_ALV_0120
*&---------------------------------------------------------------------*
* ALV Ordens em Carteira
*----------------------------------------------------------------------*
FORM z_alv_0120.
  CLEAR wl_fcat_01.
  REFRESH it_fcat_01[].

  IF wa_saida-mtart NE 'ZHAW'.                 "Produto acabado

    PERFORM preenche_cat_01 USING:

        'SAFRA'             'Ano Safra'                '10'      ''     ''      ''     ''    ''    '',
        'CULTURA'           'Cultura'                  '20'      ''     ''      ''     ''    ''    '',
        'CLIENTE'           'Cliente'                  '40'      ''     ''      ''     ''    ''    '',
        'MUNICIPIO'         'Município'                '40'      ''     ''      ''     ''    ''    '',
        'VBELN'             'OV'                       '10'      'X'    'X'     ''     ''    ''    '',
        'POSNR'             'Item'                     '06'      'X'    ''      ''     ''    ''    '',
        'MATNR'             'Material'                 '18'      'X'    ''      ''    ''     ''    '',
        'MAKTX'             'Descrição Material'       '40'      ''     ''      ''     ''    ''    '',
        'WERKS'             'Centro Forn.'             '04'      ''     ''      ''     ''    ''    '',
        'QTD_OV'            'Qtd.OV'                   '15'      ''     ''      ''     ''    ''    '',
        'QTD_SOL'           'Qtd. Liberado Entrega'    '15'      ''     ''      'X'     ''    ''    '', "*-US 136655-18-06-2024-#136655-RJF
        'QTD_SALD'          'Saldo OV'                 '15'      ''     ''      ''     ''    ''    '',
        'UM_OV'             'UM'                       '03'      ''     ''      ''     ''    ''    '',
        'MATNR_MP'          'Matéria Prima'            '18'      'X'    ''      ''     ''    ''    'C400',
        'MAKTX_MP'          'Descrição Matéria Prima'  '40'      ''     ''      ''     ''    ''    'C400',
        'FATOR_MP'          'Fator Mat.Prima'          '10'      ''     ''      ''     ''    ''    'C400',
        'QTD_CONS'          'Qtd.Consumida'            '15'      ''     ''      'X'    ''    ''    'C400',
        'QTD_AC'            'Qtd à Consumir'           '15'      ''     ''      'X'    ''    ''    'C400'.
  ELSE.

    PERFORM preenche_cat_01 USING:
       'SAFRA'             'Ano Safra'                '10'      ''     ''      ''     ''    ''    '',
       'CULTURA'           'Cultura'                  '20'      ''     ''      ''     ''    ''    '',
       'CLIENTE'           'Cliente'                  '40'      ''     ''      ''     ''    ''    '',
       'MUNICIPIO'         'Município'                '40'      ''     ''      ''     ''    ''    '',
       'VBELN'             'OV'                       '10'      'X'    'X'     ''     ''    ''    '',
       'POSNR'             'Item'                     '06'      'X'    ''      ''     ''    ''    '',
       'MATNR'             'Material'                 '18'      'X'    ''      ''    ''     ''    '',
       'MAKTX'             'Descrição Material'       '40'      ''     ''      ''     ''    ''    '',
       'WERKS'             'Centro Forn.'             '04'      ''     ''      ''     ''    ''    '',
       'QTD_OV'            'Qtd.OV'                   '15'      ''     ''      ''     ''    ''    '',
       'QTD_SOL'           'Qtd. Liberado Entrega'    '15'      ''     ''      'X'     ''    ''    '',"*-US 136655-18-06-2024-#136655-RJF
       'QTD_SALD'          'Saldo OV'                 '15'      ''     ''      ''     ''    ''    '',
       'UM_OV'             'UM'                       '03'      ''     ''      ''     ''    ''    '',
*        'MATNR_MP'          'Matéria Prima'            '18'      'X'    ''      ''     ''    ''    'C400',     "Não exibe
*        'MAKTX_MP'          'Descrição Matéria Prima'  '40'      ''     ''      ''     ''    ''    'C400',
*        'FATOR_MP'          'Fator Mat.Prima'          '10'      ''     ''      ''     ''    ''    'C400',
       'QTD_CONS'          'Qtd.Consumida'            '15'      ''     ''      'X'    ''    ''    'C400',
       'QTD_AC'            'Qtd à Consumir'           '15'      ''     ''      'X'    ''    ''    'C400'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_PRODUTO_ACABADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_exibe_produto_acabado USING p_matnr.

  DATA: it_stpov   TYPE TABLE OF stpov,
        wa_stpov   TYPE stpov,

        it_equicat TYPE TABLE OF cscequi,
        it_kndcat  TYPE TABLE OF cscknd,
        it_matcat  TYPE TABLE OF cscmat,
        it_stdcat  TYPE TABLE OF cscstd,
        it_tplcat  TYPE TABLE OF csctpl,

        it_stpo    TYPE TABLE OF stpo_api02,
        wa_stpo    TYPE stpo_api02,

        vl_datuv   TYPE datuv_bi,

        it_makt_cp TYPE TABLE OF makt,
        wa_makt_cp TYPE makt.

*  REFRESH IT_SAIDA_0110.

  CALL FUNCTION 'CS_WHERE_USED_MAT'
    EXPORTING
      datub                      = '00000000'
      datuv                      = sy-datum
      matnr                      = p_matnr
*     POSTP                      = ' '
*     RETCODE_ONLY               = ' '
      stlan                      = '1'
      werks                      = '*'
*     MCLMT                      = ' '
*     MNSTL                      = ' '
*     MXSTL                      = ' '
*     STLTP                      = ' '
*     NEWSI                      = ' '
* IMPORTING
*     TOPMAT                     =
    TABLES
      wultb                      = it_stpov
      equicat                    = it_equicat
      kndcat                     = it_kndcat
      matcat                     = it_matcat
      stdcat                     = it_stdcat
      tplcat                     = it_tplcat
*     PRJCAT                     =
    EXCEPTIONS
      call_invalid               = 1
      material_not_found         = 2
      no_where_used_rec_found    = 3
      no_where_used_rec_selected = 4
      no_where_used_rec_valid    = 5
      OTHERS                     = 6.
  IF sy-subrc IS INITIAL.
* Implement suitable error handling here
*          ENDIF.

    DELETE it_stpov WHERE werks NOT IN p_werks.
    LOOP AT it_stpov INTO  wa_stpov.

      wa_saida_0110-matnr_pa  = wa_stpov-matnr.
      wa_saida_0110-maktx     = wa_stpov-ojtxb.
      wa_saida_0110-stlnr     = wa_stpov-stlnr.
      wa_saida_0110-werks     = wa_stpov-werks.
      wa_saida_0110-bmeng     = wa_stpov-bmeng.
      wa_saida_0110-bmein     = wa_stpov-bmein.
      wa_saida_0110-matnr_mp  = wa_saida-matnr.
      wa_saida_0110-maktx_mp  = wa_saida-maktx.
      wa_saida_0110-menge     = wa_stpov-menge.
      wa_saida_0110-meins     = wa_stpov-meins.

      APPEND wa_saida_0110  TO it_saida_0110.
      CLEAR wa_saida_0110.

    ENDLOOP.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBE_LISTA_TECNICA
*&---------------------------------------------------------------------*
*  Exibir Lista_Tecnica
*----------------------------------------------------------------------*
FORM zf_exibe_lista_tecnica USING p_matnr_pa p_werks.

  DATA: it_stpo    TYPE TABLE OF stpo_api02,
        wa_stpo    TYPE stpo_api02,

        vmatnr18   TYPE matnr18,

        it_makt_cp TYPE TABLE OF makt,
        wa_makt_cp TYPE makt,

        vl_datuv   TYPE datuv_bi.

  FIELD-SYMBOLS  <wa_stpo>  TYPE stpo_api02.

  WRITE sy-datum TO vl_datuv.

  REFRESH: it_stpo,
           it_saida_0111.

  CALL FUNCTION 'CSAP_MAT_BOM_READ'
    EXPORTING
      material   = p_matnr_pa
      plant      = p_werks
      bom_usage  = '1'
*     ALTERNATIVE          =
      valid_from = vl_datuv
*     VALID_TO   =
*     CHANGE_NO  =
*     REVISION_LEVEL       =
*     FL_DOC_LINKS         =
*     FL_DMU_TMX =
*            IMPORTING
*     FL_WARNING =
    TABLES
      t_stpo     = it_stpo
*     T_STKO     =
*     T_DEP_DATA =
*     T_DEP_DESCR          =
*     T_DEP_ORDER          =
*     T_DEP_SOURCE         =
*     T_DEP_DOC  =
*     T_DOC_LINK =
*     T_DMU_TMX  =
*     T_LTX_LINE =
*     T_STPU     =
*     T_SGT_BOMC =
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT it_stpo ASSIGNING <wa_stpo>.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <wa_stpo>-component
      IMPORTING
        output = vmatnr18.
    <wa_stpo>-component = vmatnr18.
  ENDLOOP.

  IF it_stpo[] IS NOT INITIAL.
    SELECT * FROM makt
      INTO TABLE it_makt_cp
      FOR ALL ENTRIES IN it_stpo
     WHERE matnr EQ  it_stpo-component
       AND spras EQ sy-langu.
  ENDIF.

  LOOP AT it_stpo INTO wa_stpo.

    wa_saida_0111-item_no    = wa_stpo-item_no.
    wa_saida_0111-compone    = wa_stpo-component.
    READ TABLE it_makt_cp INTO wa_makt_cp WITH KEY matnr = wa_stpo-component.
    wa_saida_0111-maktx      = wa_makt_cp-maktx.
    wa_saida_0111-comp_qty   = wa_stpo-comp_qty.
    wa_saida_0111-comp_unit  = wa_stpo-comp_unit.
    wa_saida_0111-valid_from = wa_stpo-valid_from.
    wa_saida_0111-valid_to   = wa_stpo-valid_to.

    APPEND wa_saida_0111 TO it_saida_0111.
    CLEAR wa_saida_0111.

  ENDLOOP.

  CALL METHOD g_grid_04->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout_03
    CHANGING
      it_outtab                     = it_saida_0111[]
      it_fieldcatalog               = it_fcat_03
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_ORDENS
*&---------------------------------------------------------------------*
*  Busca Ordens em Carteira (gravadas através do JOB programa ZSDR0102)
*----------------------------------------------------------------------*
FORM zf_busca_ordens USING p_matnr_mp p_maktx.

  REFRESH: it_ov, it_saida_0120, it_cli, it_zsdt0038, it_makt.
  CLEAR:   wa_ov, wa_saida_0120, wa_cli, wa_zsdt0038, wa_makt.

  IF p_todos IS NOT INITIAL.

    SELECT *  FROM zsdt0256 INTO TABLE it_ov
    WHERE data     EQ p_data      "IN P_DATA
    AND   matnr_mp EQ p_matnr_mp
    AND   werks    IN p_werks
    AND   tipo     EQ 'V'
    AND   safra    IN p_gsafra.

  ELSE.
    SELECT * FROM zsdt0256 INTO TABLE it_ov
    WHERE data     EQ p_data      "IN P_DATA
    AND   matnr_mp EQ p_matnr_mp
    AND   werks    IN p_werks
    AND   tipo     EQ 'V'
    AND   safra    IN p_gsafra
    AND   tp_safra EQ tp_safra.
  ENDIF.

  IF NOT it_ov IS INITIAL.

* -US 136655-18-06-2024-#136655-RJF-inicio
    SELECT vbeln, posnr, status, qte_sol
      INTO TABLE @DATA(it_zsdt0082)
      FROM zsdt0082
      FOR ALL ENTRIES IN @it_ov
      WHERE vbeln  EQ @it_ov-vbeln
        AND posnr  EQ @it_ov-posnr
        AND status EQ '1'.
    IF sy-subrc IS INITIAL.
      SORT it_zsdt0082 BY vbeln posnr.
    ENDIF.
* -US 136655-18-06-2024-#136655-RJF-fim

    SELECT kunnr
           name1
           ort01
      INTO TABLE it_cli
      FROM kna1
      FOR ALL ENTRIES IN it_ov
     WHERE kunnr EQ it_ov-kunnr.

    SELECT *
      INTO TABLE it_zsdt0038
      FROM zsdt0038
      FOR ALL ENTRIES IN it_ov
     WHERE cultura EQ it_ov-cultura.

    SELECT *
      FROM makt INTO TABLE it_makt
      FOR ALL ENTRIES IN it_ov
    WHERE matnr EQ it_ov-matnr.

  ENDIF.

  LOOP AT it_ov INTO wa_ov.
    CLEAR wa_saida_0120.
    MOVE-CORRESPONDING wa_ov TO wa_saida_0120.

* -US 136655-18-06-2024-#136655-RJF-inicio
    LOOP AT it_zsdt0082 INTO DATA(wa_zsdt0082) WHERE vbeln = wa_ov-vbeln
                                                AND posnr = wa_ov-posnr.

      wa_saida_0120-qtd_sol = wa_saida_0120-qtd_sol + wa_zsdt0082-qte_sol.
    ENDLOOP.
* -US 136655-18-06-2024-#136655-RJF-fim

    READ TABLE it_cli INTO wa_cli WITH KEY kunnr = wa_ov-kunnr.
    IF sy-subrc IS INITIAL.
      wa_saida_0120-cliente = wa_cli-name1.
      wa_saida_0120-municipio = wa_cli-ort01.
    ENDIF.

    READ TABLE it_zsdt0038 INTO wa_zsdt0038 WITH KEY cultura = wa_ov-cultura.
    IF sy-subrc IS INITIAL.
      wa_saida_0120-cultura = wa_zsdt0038-descricao.
    ENDIF.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_ov-matnr.
    IF sy-subrc IS INITIAL.
      wa_saida_0120-maktx = wa_makt-maktx.
    ENDIF.

    wa_saida_0120-maktx_mp = p_maktx.

    APPEND wa_saida_0120 TO it_saida_0120.

  ENDLOOP.

ENDFORM.

* CS2019001220 - Sara Oikawa - Jun/2020
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_TELA_CADASTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modifica_tela_cadastro USING habilitar TYPE c.

  LOOP AT SCREEN.
*     Esconde tela com Select Options*-------------------------------------
    IF screen-group1 = 'G2'.
      screen-invisible  = COND #( WHEN habilitar = abap_true THEN 0 ELSE 1 ).
      screen-input     = COND #( WHEN habilitar = abap_true THEN 1 ELSE 0 ).
      screen-active    = COND #( WHEN habilitar = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SALVAR_DADOS_CADASTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_salvar_dados_cadastro .
  DATA it_zsdt0286_aux TYPE TABLE OF zsdt0286.
  LOOP AT it_zsdt0286 INTO DATA(w_zsdt0286).
    SELECT * FROM zsdt0286 INTO TABLE @DATA(t_zsdt0286) WHERE usname = @w_zsdt0286-usname.
    IF sy-subrc  NE 0.
      APPEND w_zsdt0286 TO it_zsdt0286_aux.
    ENDIF.

  ENDLOOP.

  MODIFY zsdt0286 FROM TABLE it_zsdt0286_aux.

ENDFORM.
