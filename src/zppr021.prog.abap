*&---------------------------------------------------------------------*
*& Report  ZPPR021
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppr021.

**********************************************************************
* tabelas
**********************************************************************
TABLES: mseg, zppt0035, t001, icon, sscrfields.

*********************************************************************
* field symbols
**********************************************************************
FIELD-SYMBOLS: <fs_fld> TYPE any.

TYPES: BEGIN OF ty_saida,
         data_documento(10)  TYPE c,
         data_lancamento(10) TYPE c,
         movimento           TYPE mseg-bwart,
         centro              TYPE zppt0035-centro,
         deposito            TYPE zppt0035-deposito,
         material            TYPE zppt0035-material,
         qtde                TYPE char15,
         lote                TYPE zppt0035-lote,
         material_destino    TYPE zppt0035-material_destino,
         centro_destino      TYPE zppt0035-centro_destino,
         deposito_destino    TYPE zppt0035-deposito_destino,
         lote_destino        TYPE zppt0035-lote_destino,
         id_carga            TYPE zppt0035-id_carga,
         item_carga          TYPE zppt0035-item_carga,
         linha_planilha      TYPE zppt0035-linha_planilha,
         doc_material        TYPE zppt0035-doc_material,
         nserie              TYPE zppt0035-nserie, "  180771 -  CS2025000597  - RGA
         status(4),
         erro(1),
         autorizado(1).
TYPES: END   OF ty_saida.

TYPES: BEGIN OF ty_file,
         data_documento(10)  TYPE c,
         data_lancamento(10) TYPE c,
         movimento           TYPE mseg-bwart,
         centro              TYPE zppt0035-centro,
         deposito            TYPE zppt0035-deposito,
         material            TYPE zppt0035-material,
         qtde(20)            TYPE c,
         lote                TYPE zppt0035-lote,
         material_destino    TYPE zppt0035-material_destino,
         centro_destino      TYPE zppt0035-centro_destino,
         deposito_destino    TYPE zppt0035-deposito_destino,
         lote_destino        TYPE zppt0035-lote_destino,
         id_carga            TYPE zppt0035-id_carga,
         item_carga          TYPE zppt0035-item_carga,
         linha_planilha      TYPE zppt0035-linha_planilha,
         doc_material        TYPE zppt0035-doc_material,
         nserie              TYPE zppt0035-nserie, "180771 -  CS2025000597  - RGA
         status(4),
         log(60).
TYPES: END   OF ty_file.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_icon,
         id   TYPE icon-id,
         name TYPE icon-name.
TYPES: END   OF ty_icon.
*------------------------------------
*---- ALV
*------------------------------------
DATA: dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      obj_cont             TYPE REF TO cl_gui_custom_container,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      t_function           TYPE ui_functions,
      w_function           TYPE ui_func,
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: it_saida    TYPE STANDARD TABLE OF ty_saida,
      wa_saida    TYPE  ty_saida,
      it_zppt0035 TYPE TABLE OF zppt0035,
      wa_zppt0035 TYPE zppt0035,
      t_file      TYPE TABLE OF zppe0035, " zppe0035,
      wa_file     TYPE  zppe0035,
      t_tab       TYPE TABLE OF alsmex_tabline,
      w_tab       TYPE alsmex_tabline.

DATA: it_mchb TYPE TABLE OF mchb,
      wa_mchb TYPE mchb.

DATA:l_leave      TYPE syst_ucomm,
     l_sel_button TYPE smp_dyntxt,
     l_opcao      TYPE char1,
     l_nfps       TYPE znfnum,
     l_data_char  TYPE char10,
     l_tabix      TYPE sy-tabix,
     l_icon_name  TYPE icon-name.

DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      v_file(80)   ,
      c_x(1).

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.

DATA: sl_item     TYPE bapi2017_gm_item_create,
      sl_return   TYPE zfiwrs0002, "bapiret2,
      tl_item     TYPE TABLE OF bapi2017_gm_item_create,
      tl_return   TYPE TABLE OF zfiwrs0002,
      it_return   TYPE TABLE OF zfiwrs0002,
      msg_alv     TYPE char30,
      t_new_table TYPE REF TO data,
      t_new_line  TYPE REF TO data.


DATA: lv_rc               TYPE inri-returncode,
      lv_number_range(06) TYPE n.
DATA: v_tabix TYPE sy-tabix.
DATA : it_texto TYPE TABLE OF string.
DATA: v_erro(1),
      wg_acao(10).

FIELD-SYMBOLS: <saida>  TYPE ty_saida.

DATA: c_xx(1).

DATA: ty_toolbar TYPE stb_button.
CLASS: lcl_alv_toolbar    DEFINITION DEFERRED.
DATA:      obg_toolbar          TYPE REF TO lcl_alv_toolbar.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_add(3)   TYPE c VALUE 'ADD',
           c_del(3)   TYPE c VALUE 'DEL',
           c_modif(5) TYPE c VALUE 'MODIF'.

FIELD-SYMBOLS: <fs_table>    TYPE STANDARD TABLE,
               <fs_line>     TYPE any,
               <fs_line_aux> TYPE any,
               <fs_campo>    TYPE any.
**********************************************************************
* classes / implementacoes
**********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.



SELECT-OPTIONS: s_dt_doc         FOR sy-datum   NO-EXTENSION NO INTERVALS, "OBLIGATORY,
                s_dt_lan    FOR sy-datum      NO-EXTENSION NO INTERVALS." OBLIGATORY.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-010.
  SELECT-OPTIONS:
                  s_tp_mov    FOR mseg-bwart      NO-EXTENSION NO INTERVALS," OBLIGATORY,
                  s_centro FOR mseg-werks NO-EXTENSION NO INTERVALS." OBLIGATORY.
  PARAMETERS: p_form TYPE bktxt. "180771-Ajustes e melhorias na transação ZPP0024
SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:     p_file         LIKE rlgrap-filename MODIF ID t1.
SELECTION-SCREEN END   OF BLOCK b2.


SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'

**********************************************************************
*SELECTION-SCREEN p_file
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_select_file USING p_file.

**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  FREE MEMORY ID 'ZPPR021'.
  FREE: l_leave.

  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM gera_modelo_planillha.
  ENDCASE.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

  FREE MEMORY ID 'ZGLR072'.
  l_leave = 'LEAVE'.
  EXPORT l_leave FROM l_leave TO MEMORY ID 'ZPPR021'.

  l_opcao                = '1'.

  l_sel_button-icon_id   = icon_dangerous_goods.
  l_sel_button-icon_text = 'Download Planilha Modelo Carga'.
  sscrfields-functxt_01  = l_sel_button.
**********************************************************************
* START
**********************************************************************
START-OF-SELECTION.

  IF p_file IS NOT INITIAL.
    SPLIT p_file AT '\' INTO TABLE it_texto.

    DATA(v_cont) = lines( it_texto ).

    READ TABLE it_texto INTO DATA(wa_texto) INDEX v_cont.
    IF sy-subrc IS INITIAL.

      v_file = wa_texto.
    ENDIF.

* if sy-subrc is not INITIAL.
* MESSAGE   'Nome da Planilha não está no padrão estabelecido!' TYPE 'S' DISPLAY LIKE 'E'.
*      STOP.
* endif.

    DATA: p_index     TYPE i,
          p_length    TYPE i,
          p_character TYPE c,
          p_allowed   TYPE string,
          p_texto     TYPE string,
          p_erro(1).
    CONSTANTS p_ponto(1)   TYPE c VALUE '.'.

    TRANSLATE wa_texto TO LOWER CASE.

    p_allowed = '0123456789_'.
    p_length  = strlen( wa_texto ).
    p_index   = 0.

    WHILE p_length GT p_index.
      p_character = wa_texto+p_index(1).
      IF p_character NE p_ponto .
        CONCATENATE p_texto p_character INTO p_texto.
      ELSE.
        EXIT.
      ENDIF.

      IF p_index > 20.
        SEARCH p_allowed FOR p_character.
        IF sy-subrc NE '0'.
          p_erro = 'X'.
        ENDIF.
      ENDIF.

      p_index = p_index + 1.
    ENDWHILE.

    SEARCH wa_texto FOR 'PlanilhaCargaModelo_' AND MARK.

    IF sy-subrc IS NOT INITIAL OR strlen( p_texto ) NE 35 OR p_erro IS NOT INITIAL.
      MESSAGE   'Nome da Planilha não está no padrão estabelecido!' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.

    ENDIF.

    PERFORM f_carrega_arquivo.
    PERFORM f_processa_arquivo.
    PERFORM f_gravar_zppt0035.
    PERFORM transferir.
  ELSE.

    IF s_dt_doc-low IS INITIAL OR s_dt_lan-low IS INITIAL OR s_tp_mov-low IS INITIAL OR s_centro-low IS INITIAL
    OR p_form IS INITIAL.
      MESSAGE   'Informar campos obrigatórios!' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    IF s_tp_mov-low NE '309' AND s_tp_mov-low NE '311'.
      CONCATENATE 'Tipo Movimento' s_tp_mov-low 'não previsto!' INTO DATA(message) SEPARATED BY space.
      MESSAGE message TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.


    SELECT SINGLE * FROM mseg INTO @DATA(w_mseg) WHERE werks = @s_centro-low.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Centro não encontrado' TYPE 'S' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.
    PERFORM buscar_linhas_gravadas.
    PERFORM validar_linhas USING abap_false.
    IF it_saida[] IS INITIAL.
      DATA i  TYPE i.
      CLEAR: wa_saida.

      CONCATENATE s_dt_doc-low+6(2)'.' s_dt_doc-low+4(2)'.' s_dt_doc-low(4)'.' INTO wa_saida-data_documento.
      CONCATENATE s_dt_lan-low+6(2)'.' s_dt_lan-low+4(2)'.' s_dt_lan-low(4)'.' INTO wa_saida-data_lancamento.
      wa_saida-movimento      = s_tp_mov-low.
      wa_saida-centro         = s_centro-low.
      wa_saida-centro_destino      = s_centro-low.
      i = 1.

      DO 50 TIMES.
        wa_saida-item_carga = i.
        APPEND wa_saida TO it_saida.
        i = i + 1.
      ENDDO.
    ENDIF.
  ENDIF.
  CALL SCREEN 100.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

  ENDMETHOD.

  METHOD on_hotspot_click.

    TYPES: BEGIN OF ty_itab ,
             name(80) TYPE c,
           END OF ty_itab.

    DATA:
          itab_msg TYPE TABLE OF zfiwrs0002.

    DATA: wtab_msg TYPE  zfiwrs0002.
    REFRESH it_return.
    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      CASE e_column_id .
        WHEN 'STATUS'.
          IF wa_saida-status = icon_led_red OR wa_saida-status = icon_led_yellow.
            LOOP AT tl_return INTO DATA(w_retorn) WHERE tabix EQ e_row_id-index.
              APPEND VALUE #( msg = w_retorn-msg ) TO it_return.
            ENDLOOP.
            IF c_xx IS INITIAL.
              PERFORM f_verificar_erros USING abap_true.
            ELSE.
              PERFORM f_verificar_erros USING abap_false.
            ENDIF.
          ENDIF.

      ENDCASE.


    ENDIF.

  ENDMETHOD.

  METHOD data_changed_finished.

    DATA: wa_good_cells TYPE lvc_s_modi.
    DATA: v_tabix TYPE sy-tabix.
    CLEAR: it_sel_rows[], wa_sel_rows.

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = it_sel_rows.

    UNASSIGN <fs_line>.

    LOOP AT et_good_cells INTO wa_good_cells.
      READ TABLE it_saida INTO wa_saida INDEX wa_good_cells-row_id.

      v_tabix  = wa_good_cells-row_id.
      IF e_modified EQ abap_true.
        IF wa_good_cells-fieldname EQ 'DEPOSITO'.

          MOVE wa_good_cells-value TO wa_saida-deposito.

        ELSEIF wa_good_cells-fieldname EQ 'MATERIAL'.
*04 - VALIDAR SE O MATERIAL EXISTE

          SELECT SINGLE matnr
            FROM mara
            INTO @DATA(mat_existe)
            WHERE matnr = @wa_good_cells-value.

          IF sy-subrc IS NOT INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg = 'Material Informado não está cadastrado no SAP'.
            APPEND sl_return TO tl_return.
          ELSE.
            MOVE wa_good_cells-value TO wa_saida-material.
          ENDIF.

        ELSEIF wa_good_cells-fieldname EQ 'QTDE'.

          IF wa_good_cells-value < '0'.

            sl_return-tabix = v_tabix.
            sl_return-msg = 'Quantidade deve ser maior que 0'.
            APPEND sl_return TO tl_return.
          ELSE.
            PERFORM f_change_text CHANGING wa_saida-qtde.
            MOVE wa_good_cells-value TO wa_saida-qtde.
          ENDIF.

        ELSEIF wa_good_cells-fieldname EQ 'LOTE'.
*01 - VALIDAR SE O LOTE ORIGEM/DESTINO EXISTE

*origem
          SELECT SINGLE clabs
            FROM mchb
            INTO  @DATA(v_clabs)
            WHERE  matnr = @wa_saida-material AND
                   lgort = @wa_saida-deposito AND
                   charg = @wa_good_cells-value AND
                   werks = @wa_saida-centro.

          IF sy-subrc IS NOT INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg = 'Lote Origem informado não está cadastrado no SAP'.
            APPEND sl_return TO tl_return.
          ELSE.
            MOVE wa_good_cells-value TO wa_saida-lote.
          ENDIF.


        ELSEIF wa_good_cells-fieldname EQ 'MATERIAL_DESTINO'.
*04 - VALIDAR SE O MATERIAL EXISTE
          SELECT SINGLE matnr
            FROM mara
            INTO @DATA(mat_dest_existe)
            WHERE matnr = @wa_good_cells-value.

          IF sy-subrc IS NOT INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg = 'Material Informado não está cadastrado no SAP'.
            APPEND sl_return TO tl_return.
          ELSE.
            MOVE wa_good_cells-value TO wa_saida-material_destino.
          ENDIF.

        ELSEIF wa_good_cells-fieldname EQ 'DEPOSITO_DESTINO'.

          MOVE wa_good_cells-value TO wa_saida-deposito_destino.

        ELSEIF wa_good_cells-fieldname EQ 'LOTE_DESTINO'.
*Destino
          SELECT *
            FROM mchb
            INTO TABLE it_mchb
            WHERE  matnr = wa_saida-material AND
                   charg = wa_saida-lote_destino AND
                   werks = wa_saida-centro.

          IF sy-subrc IS NOT INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg = 'Lote Destino informado não está cadastrado no SAP'.
            APPEND sl_return TO tl_return.
          ELSE.
            MOVE wa_good_cells-value TO wa_saida-lote_destino.
          ENDIF.

        ENDIF.

        IF tl_return IS INITIAL.
          MOVE icon_led_green TO wa_saida-status.
          v_erro = ' '.
        ELSE.
          MOVE icon_led_red TO wa_saida-status.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF et_good_cells[] IS NOT INITIAL.
      PERFORM validar_linhas USING abap_false.
    ENDIF.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    IF p_file IS INITIAL.
      wg_acao = c_add.
    ELSE.
      wg_acao = c_del.
    ENDIF.
    e_object->mt_toolbar =
    VALUE #(
              ( icon = '@17@' function = c_add        butn_type = 0 disabled = SWITCH #( wg_acao WHEN c_add OR c_modif THEN space ELSE 1 ) )
              ( icon = '@18@' function = c_del        butn_type = 0 disabled = SWITCH #( wg_acao WHEN c_add OR c_modif THEN space ELSE 1 ) )
              (               function = '&&SEP04'    butn_type = 3 )
              ( icon = '@3E@' function = '&SORT_ASC'  butn_type = 0 )
              ( icon = '@3F@' function = '&SORT_DSC'  butn_type = 0 )
              (               function = '&&SEP04'    butn_type = 3 )
              ( icon = '@13@' function = '&FIND'      butn_type = 0 )
              ( icon = '@4E@' function = '&FIND_MORE' butn_type = 0 )
              (               function = '&&SEP04'    butn_type = 3 )
              ( icon = '@3Z@' function = '&MB_SUM'    butn_type = 1 )
              ( icon = '@5V@' function = '&MB_SUBTOT' butn_type = 1 )
           ).

    ty_toolbar-icon      =  icon_insert_row.



  ENDMETHOD.

  METHOD handle_user.

    CASE e_ucomm.
      WHEN c_add.
        DATA vi_lines TYPE i.
        CONCATENATE s_dt_doc-low+6(2)'.' s_dt_doc-low+4(2)'.' s_dt_doc-low(4)'.' INTO DATA(v_data_documento).
        CONCATENATE s_dt_lan-low+6(2)'.' s_dt_lan-low+4(2)'.' s_dt_lan-low(4)'.' INTO DATA(v_data_lancamento).
        DESCRIBE TABLE it_saida LINES vi_lines.
        APPEND VALUE #(
                        data_documento = v_data_documento
                        data_lancamento = v_data_lancamento
                        movimento = s_tp_mov-low
                        centro    = s_centro-low
                        centro_destino = s_centro-low
                        item_carga = vi_lines + 1
                      ) TO it_saida.

      WHEN c_del.

*        CALL METHOD g_grid->get_selected_cells
*          IMPORTING
*            et_cell = DATA(t_cell).

        LOOP AT it_sel_rows INTO DATA(_cell).
          "DATA(_linha) = it_saida[ _cell-index ].

          DELETE it_saida INDEX _cell-index.

        ENDLOOP.

    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.


ENDCLASS.


**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.


  SET TITLEBAR  'ZPPR021'.

  DATA: BEGIN OF tl_ucomm OCCURS 0,
          ucomm TYPE  sy-ucomm,
        END OF tl_ucomm.

  IF p_file IS INITIAL.
    APPEND VALUE #( ucomm = 'AUTH' ) TO tl_ucomm[].
  ENDIF.

  SET PF-STATUS 'ZPPR021' EXCLUDING tl_ucomm.

  PERFORM f_init_alv.

  "  PERFORM f_verificar_erros USING abap_true.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  FREE: t_rows[].

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

    WHEN 'TRANSF'.
      PERFORM validar_linhas USING abap_false.
      PERFORM f_gravar_zppt0035.
      PERFORM transferir.

    WHEN 'AUTH'.
      PERFORM f_autorizar.
    WHEN 'SAVE'.
*      PERFORM f_gravar_zppt0035 .
*      PERFORM validar_linhas.
*      MESSAGE 'Os dados foram salvos!' TYPE 'S' DISPLAY LIKE 'S'.
    WHEN 'CLOS_MSG'.
      c_xx = ' '.
  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

FORM gera_modelo_planillha .

  IF s_dt_doc-low IS INITIAL OR s_dt_lan-low IS INITIAL OR s_tp_mov-low IS INITIAL OR s_centro-low IS INITIAL.
    MESSAGE   'Informar campos obrigatórios!' TYPE 'E'.
    EXIT.
  ENDIF.


  IF s_tp_mov-low NE '309' AND s_tp_mov-low NE '311'.
    CONCATENATE 'Tipo Movimento' s_tp_mov-low 'não previsto' INTO DATA(message) SEPARATED BY space.
    MESSAGE   message TYPE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM mseg INTO @DATA(w_mseg) WHERE werks = @s_centro-low.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE  'Centro não encontrado'  TYPE 'E'.
    EXIT.
  ENDIF.

  "  180771 -  CS2025000597  - RGA
  IF p_form IS INITIAL.
    MESSAGE 'O campo Nº SRE é obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM cria_linhas.

  DATA valor(30).
  DATA: p_local   TYPE string,
        path(250).
  DATA: t_alvdata      TYPE REF TO data,
        v_nome_arquivo TYPE char50.

  DATA:
    BEGIN OF t_fieldnames OCCURS 0,
      name(50) TYPE c,
    END OF t_fieldnames.

  CONCATENATE 'PlanilhaCargaModelo_' sy-datum+6(2) sy-datum+4(2) sy-datum(4) '_' sy-uzeit INTO v_nome_arquivo.


  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = v_nome_arquivo
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    IMPORTING
      filename         = path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc IS INITIAL.

    CONCATENATE path '.xls' INTO p_local.

    t_fieldnames-name    = 'Data Documento'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Data Lançamento'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Movimento'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Centro'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Depósito'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Material'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Qtde'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Lote'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Material Destino'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Centro Destino'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Deposito destino'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Lote Destino'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Nº SRE'.
    APPEND t_fieldnames.


    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = p_local
        filetype              = 'ASC'
        write_field_separator = 'X'
        "CODEPAGE            = '8404'
      TABLES
        data_tab              = t_file
        fieldnames            = t_fieldnames
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        invalid_filesize      = 3
        invalid_table_width   = 4
        invalid_type          = 5
        no_batch              = 6
        unknown_error         = 7
        OTHERS                = 8.

    IF sy-subrc = 0.
      MESSAGE 'Arquivos gerados com sucesso' TYPE 'S'.
    ELSE.
      MESSAGE 'Arquivo processado com erro' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* seleciona arquivo
**********************************************************************
FORM f_select_file USING p_filename TYPE localfile.

  DATA: l_subrc     LIKE sy-subrc,
        t_filetable TYPE filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Selecione o arquivo .xls'
      default_filename = '*.xls'
      multiselection   = ' '
    CHANGING
      file_table       = t_filetable
      rc               = l_subrc.

  READ TABLE t_filetable INTO p_filename INDEX 1.

ENDFORM.

**********************************************************************
* carregar arquivo
**********************************************************************
FORM f_carrega_arquivo.

  DATA: l_erro TYPE char1,
        l_cols TYPE i.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = TEXT-130.

*----------------------------------------
* upload excel
*----------------------------------------
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 12
      i_end_row               = 30000
*     i_end_col               = 256
*     i_end_row               = 65536
    TABLES
      intern                  = t_tab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH TEXT-100 p_file DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*----------------------------------------
* carrega tabela interna
*----------------------------------------
  FREE: l_erro, l_cols.

  LOOP AT t_tab INTO w_tab.

    l_cols = l_cols + 1.
    ASSIGN COMPONENT w_tab-col OF STRUCTURE wa_file TO <fs_fld>.
    <fs_fld> = w_tab-value.
    AT END OF row.

      IF l_cols <> 12 AND  w_tab-row = 0001.
        l_erro = abap_true.
      ENDIF.
      APPEND wa_file TO t_file.
      CLEAR wa_file.
      FREE l_cols.
    ENDAT.
  ENDLOOP.


  IF l_erro = abap_true.
    MESSAGE s024(sd) WITH TEXT-115 TEXT-116 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

FORM f_processa_arquivo.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = TEXT-130.

  DELETE t_file INDEX 1.

*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      nr_range_nr = '01'
*      object      = 'ZMAN_MAT'
*    IMPORTING
*      number      = lv_number_range
*      returncode  = lv_rc.

  LOOP AT t_file INTO wa_file.

    l_tabix = sy-tabix.

    CONDENSE wa_file-dt_doc              NO-GAPS.
    CONDENSE wa_file-dt_lan              NO-GAPS.
    CONDENSE wa_file-movimento           NO-GAPS.
    CONDENSE wa_file-centro              NO-GAPS.
    CONDENSE wa_file-deposito            NO-GAPS.
    CONDENSE wa_file-material            NO-GAPS.
    CONDENSE wa_file-qtde                NO-GAPS.
    CONDENSE wa_file-lote                NO-GAPS.
    CONDENSE wa_file-mat_dest            NO-GAPS.
    CONDENSE wa_file-cen_dest            NO-GAPS.
    CONDENSE wa_file-dep_dest            NO-GAPS.
    CONDENSE wa_file-lot_dest            NO-GAPS.
    CONDENSE wa_file-nserie              NO-GAPS. "180771- RGARCIA


*------------------------------
*---material
*------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = wa_file-material
      IMPORTING
        output       = wa_file-material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = wa_file-mat_dest
      IMPORTING
        output       = wa_file-mat_dest
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

*------------------------------
*---qtde
*------------------------------
    PERFORM f_change_text CHANGING wa_file-qtde.


*------------------------------
*---CENTRO
*------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_file-centro
      IMPORTING
        output = wa_file-centro.

*------------------------------
*---CENTRO Destino
*------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_file-cen_dest
      IMPORTING
        output = wa_file-cen_dest.

*------------------------------
*---Deposito
*------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_file-deposito
      IMPORTING
        output = wa_file-deposito.

*------------------------------
*---Deposito Destino
*------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_file-dep_dest
      IMPORTING
        output = wa_file-dep_dest.

*------------------------------
*---Lote
*------------------------------
*--------/ Bloco comentado / MM - Alterar validação lote material #147296 / AOENNING V2
*    call function 'CONVERSION_EXIT_ALPHA_INPUT'
*      exporting
*        input  = wa_file-lote
*      importing
*        output = wa_file-lote.
*--------/ Bloco comentado / MM - Alterar validação lote material #147296 / AOENNING V2


    wa_saida-data_documento       = wa_file-dt_doc.
    wa_saida-data_lancamento      = wa_file-dt_lan.
    wa_saida-movimento            = wa_file-movimento.
    wa_saida-centro               = wa_file-centro.
    wa_saida-deposito             = wa_file-deposito.
    wa_saida-material             = wa_file-material.
    wa_saida-qtde                 = wa_file-qtde.
    wa_saida-lote                 = wa_file-lote.
    wa_saida-material_destino     = wa_file-mat_dest.
    wa_saida-centro_destino       = wa_file-cen_dest.
    wa_saida-deposito_destino     = wa_file-dep_dest.
    wa_saida-lote_destino         = wa_file-lot_dest.
    wa_saida-linha_planilha       =  l_tabix.
    wa_saida-nserie               =  wa_file-nserie. "180771 - RGARCIA


    "wa_saida-item_carga = l_tabix.
    "wa_saida-id_carga = lv_number_range.

    APPEND wa_saida TO it_saida.

    MODIFY t_file FROM wa_file  INDEX l_tabix.

  ENDLOOP.

  PERFORM validar_linhas USING abap_false.

ENDFORM.

FORM cria_linhas .
  DATA i  TYPE i.
  CLEAR: wa_saida.

  CONCATENATE s_dt_doc-low+6(2)'.' s_dt_doc-low+4(2)'.' s_dt_doc-low(4)'.' INTO wa_file-dt_doc.
  CONCATENATE s_dt_lan-low+6(2)'.' s_dt_lan-low+4(2)'.' s_dt_lan-low(4)'.' INTO wa_file-dt_lan.
  wa_file-movimento      = s_tp_mov-low.
  wa_file-centro         = s_centro-low.
  wa_file-cen_dest       = s_centro-low.
  wa_file-nserie         = p_form.
  i = 1.

  DO 50 TIMES.
    APPEND wa_file TO t_file.
    i = i + 1.
  ENDDO.

ENDFORM.

FORM validar_linhas USING p_autorizar .

  DATA: v_clabs    TYPE mchb-clabs,
        v_qtde     TYPE mchb-clabs,
        v_dtlan(8),
        v_dtdoc(8).

  "DATA: v_tabix TYPE sy-tabix.

  REFRESH: tl_return.
  CLEAR sl_return.
  FIELD-SYMBOLS: <saida> TYPE ty_saida.

  LOOP AT it_saida ASSIGNING  <saida>.

    IF  <saida>-status NE  icon_led_yellow.
      IF <saida>-status NE icon_checked.

        v_tabix  = sy-tabix.
        MOVE ' ' TO <saida>-status.

*Validações se ja foi processado a planilha informada.

        IF p_file  IS NOT INITIAL.
          IF <saida>-autorizado NE 'X'.
            SPLIT p_file AT '\' INTO TABLE it_texto.

            DATA(v_cont) = lines( it_texto ).

            READ TABLE it_texto INTO DATA(wa_texto) INDEX v_cont.
            IF sy-subrc IS INITIAL.

              v_file = wa_texto.
            ENDIF.

            IF v_file IS NOT INITIAL.

              SELECT * FROM zppt0035 INTO TABLE @DATA(it_zppt0035) WHERE nome_planilha = @v_file. "Planilha ja foi processada.

              IF sy-subrc IS INITIAL.

                CONCATENATE <saida>-data_lancamento+6(4) <saida>-data_lancamento+3(2) <saida>-data_lancamento(2) INTO v_dtlan.
                CONCATENATE <saida>-data_documento+6(4)  <saida>-data_documento+3(2)  <saida>-data_documento(2)  INTO v_dtdoc.
                READ TABLE it_zppt0035 INTO DATA(wa_zppt0035) WITH KEY data_documento   = v_dtdoc
                                                                       data_lancamento  = v_dtlan
                                                                       centro           = <saida>-centro
                                                                       deposito         = <saida>-deposito
                                                                       material         = <saida>-material
                                                                       qtde             = <saida>-qtde
                                                                       lote             = <saida>-lote
                                                                       material_destino = <saida>-material_destino
                                                                       centro_destino   = <saida>-centro_destino
                                                                       deposito_destino = <saida>-deposito_destino
                                                                       lote_destino     = <saida>-lote_destino.
                IF sy-subrc IS INITIAL AND wa_zppt0035-doc_material IS NOT INITIAL.
                  sl_return-tabix = v_tabix.
                  sl_return-msg = 'Planilha informada ja foi processada e documento material ja foi criado'.
                  APPEND sl_return TO tl_return.
                  MOVE icon_led_yellow TO <saida>-status.
                  MOVE 'X' TO <saida>-erro.
                  MOVE wa_zppt0035-id_carga TO <saida>-id_carga.
                  MOVE wa_zppt0035-item_carga TO <saida>-item_carga.
                ENDIF.

              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.
        IF <saida>-status NE icon_led_yellow.
*00 - VALIDAR SE OS CAMPOS ESTÃO PREENCHIDOS.
*Validar apenas se a linha estiver com pelo menos um dos campos que estão aberto estiver preenchido
*se nao houver, nao validar.
          IF <saida>-deposito IS INITIAL AND <saida>-material IS INITIAL
            AND <saida>-qtde IS INITIAL AND <saida>-lote IS INITIAL AND <saida>-material_destino IS INITIAL AND
            <saida>-deposito_destino IS INITIAL AND <saida>-lote_destino IS INITIAL.
            MOVE ' ' TO <saida>-erro.
            MOVE ' ' TO <saida>-status.
            CONTINUE.
          ENDIF.

          IF <saida>-deposito IS INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg = 'Necessário informar o Deposito'.
            APPEND sl_return TO tl_return.
          ENDIF.
          IF <saida>-material IS INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg  = 'Necessário informar o Material'.
            APPEND sl_return TO tl_return.
          ENDIF.
          IF <saida>-qtde IS INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg  = 'Necessário informar a Quantidade'.
            APPEND sl_return TO tl_return.
          ENDIF.


*        IF <saida>-lote IS INITIAL.
*          sl_return-tabix = v_tabix.
*          sl_return-msg  = 'Necessário informar o Lote'.
*          APPEND sl_return TO tl_return.
*        ENDIF.



          IF <saida>-material_destino IS INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg  = 'Necessário informar o Material Destino'.
            APPEND sl_return TO tl_return.
          ENDIF.
          IF <saida>-deposito_destino IS INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg  = 'Necessário informar o Deposito Destino'.
            APPEND sl_return TO tl_return.
          ENDIF.

*        IF <saida>-lote_destino IS INITIAL.
*          sl_return-tabix = v_tabix.
*          sl_return-msg  = 'Necessário informar o Lote Destino'.
*          APPEND sl_return TO tl_return.
*        ENDIF.


*04 - VALIDAR SE O MATERIAL EXISTE

          "origem
          SELECT SINGLE *
            FROM mara
            INTO @DATA(wa_mara)
            WHERE matnr = @<saida>-material.

          IF sy-subrc IS NOT INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg  = 'Material Informado não está cadastrado no SAP'.
            APPEND sl_return TO tl_return.
          ENDIF.

*&--------------Inicio BUG SOLTO 147296 / aoenning
          "Validar se o mateterial é controlado por lote.
          IF wa_mara-xchpf IS NOT INITIAL.
            IF <saida>-lote IS INITIAL.
              sl_return-tabix = v_tabix.
              sl_return-msg  = 'Necessário informar o Lote'.
              APPEND sl_return TO tl_return.
            ENDIF.


*01 - VALIDAR SE O LOTE ORIGEM/DESTINO EXISTE
*origem
            SELECT SINGLE *
            FROM mchb
            INTO  @DATA(wa_lote)
            WHERE  matnr = @<saida>-material AND
              lgort = @<saida>-deposito AND
              charg = @<saida>-lote .

            IF sy-subrc IS NOT INITIAL.

              sl_return-tabix = v_tabix.
              sl_return-msg  = 'Lote Origem informado não está cadastrado no SAP'.
              APPEND sl_return TO tl_return.
            ENDIF.

            PERFORM f_change_texto CHANGING <saida>-qtde.

            SELECT SINGLE clabs
              FROM mchb
              INTO  v_clabs
              WHERE  matnr = <saida>-material AND
                     lgort = <saida>-deposito AND
                     charg = <saida>-lote AND
                     werks = <saida>-centro.

            IF sy-subrc IS INITIAL.
              " 02 - NA MESMA TABELA VALIDAR EXISTENCIA NO SALDO ORIGEM PARA TRANSFERENCIA
              IF v_clabs < <saida>-qtde.
                sl_return-tabix = v_tabix.
                sl_return-msg  = 'Quantidade informada maior que a quantidade disponível'.
                APPEND sl_return TO tl_return.
              ENDIF.

            ENDIF.
          ENDIF.
*&--------------Fim BUG SOLTO 147296 / aoenning

          "destino
          SELECT SINGLE *
          FROM mara
          INTO @wa_mara
          WHERE matnr = @<saida>-material_destino.


          IF sy-subrc IS NOT INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg  = 'Material Destino Informado não está cadastrado no SAP'.
            APPEND sl_return TO tl_return.
          ENDIF.
*&--------------Inicio BUG SOLTO 147296 / aoenning
          "Validar se o mateterial é controlado por lote.
          IF wa_mara-xchpf IS NOT INITIAL.

            IF <saida>-lote_destino IS INITIAL.
              sl_return-tabix = v_tabix.
              sl_return-msg  = 'Necessário informar o Lote Destino'.
              APPEND sl_return TO tl_return.
            ENDIF.

*Destino
            SELECT *
            FROM mchb
            INTO TABLE it_mchb
            WHERE  charg = <saida>-lote_destino AND
            matnr = <saida>-material_destino AND
            lgort = <saida>-deposito_destino .

            IF sy-subrc IS NOT INITIAL.
              sl_return-tabix = v_tabix.
              sl_return-msg  = 'Lote Destino informado não está cadastrado no SAP'.
              APPEND sl_return TO tl_return.
            ENDIF.
          ENDIF.
*&--------------Fim BUG SOLTO 147296 / aoenning

*03 - VALIDAR SE O MATERIAL TEM EXPANSÃO PARA O DEPOSITO DESTINO INFORMADO
          SELECT SINGLE lgort
            FROM mard
            INTO @DATA(v_lgort)
            WHERE matnr  = @<saida>-material_destino AND
                  werks = @<saida>-centro_destino AND
                  lgort = @<saida>-deposito_destino.

          IF sy-subrc IS NOT INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg = 'Material Destino não foi expandido para o depósito destino Informado'.
            APPEND sl_return TO tl_return.
          ENDIF.



*VALIDAR SE O DEPOSITO EXISTE
          SELECT SINGLE * FROM mard INTO @DATA(wa_mard) WHERE matnr = @<saida>-material AND
                  werks = @<saida>-centro AND
                  lgort = @<saida>-deposito.

          IF sy-subrc IS NOT INITIAL.
            sl_return-tabix = v_tabix.
            sl_return-msg = 'Depósito Origem informado não está cadastrado no SAP'.
            APPEND sl_return TO tl_return.
          ENDIF.

          IF sl_return-msg IS INITIAL.
            MOVE icon_led_green TO <saida>-status.
            MOVE ' ' TO <saida>-erro.
          ELSEIF <saida>-status IS INITIAL.
            MOVE icon_led_red TO <saida>-status.
            MOVE 'X' TO <saida>-erro.
          ENDIF.

        ENDIF.
      ELSE.
        IF <saida>-deposito IS INITIAL AND <saida>-material IS INITIAL
        AND <saida>-qtde IS INITIAL AND <saida>-lote IS INITIAL AND <saida>-material_destino IS INITIAL AND
        <saida>-deposito_destino IS INITIAL AND <saida>-lote_destino IS INITIAL.
          MOVE ' ' TO <saida>-erro.
          MOVE ' ' TO <saida>-status.
        ENDIF.

      ENDIF.
    ENDIF.
    CLEAR: wa_saida, sl_return.

  ENDLOOP.

ENDFORM.

FORM transferir.
  CLEAR: wa_saida.
  DATA: v_erro_mat(1),
        v_status(1),
        v_autorizado(1),
        v_possui_doc(1).

  DATA: sl_header   TYPE bapi2017_gm_head_01,
        vl_code     TYPE bapi2017_gm_code,
        vl_material TYPE bapi2017_gm_head_ret-mat_doc,
        vl_year     TYPE bapi2017_gm_head_ret-doc_year,
        tl_item     TYPE TABLE OF bapi2017_gm_item_create,

        sl_item     TYPE bapi2017_gm_item_create,
        sl_zppt0035 TYPE zppt0035,
        vl_index    TYPE i,
        var_answer  TYPE c,
        t_return    TYPE TABLE OF bapiret2,
        s_return    TYPE bapiret2.
  CLEAR sl_header.
  CLEAR: v_erro,  v_status.
  LOOP AT it_saida INTO wa_saida.
    IF  wa_saida-status EQ icon_checked.
      v_status = 'X'.
    ELSEIF wa_saida-status EQ icon_led_yellow.
      v_possui_doc = 'X'.
      EXIT.
    ENDIF.
    IF wa_saida-autorizado IS NOT INITIAL.
      v_autorizado = 'X'.
    ENDIF.

    IF wa_saida-erro EQ 'X' .
      v_erro = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
*  IF p_file IS INITIAL.
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr = '01'
*        object      = 'ZMAN_MAT'
*      IMPORTING
*        number      = lv_number_range
*        returncode  = lv_rc.
*  ENDIF.

  IF v_status IS NOT INITIAL.
    MESSAGE 'Transferência ja realizada para os registros!' TYPE 'S' DISPLAY LIKE 'W'.
  ELSE.
    IF v_erro NE 'X'.

      IF ( v_possui_doc IS NOT INITIAL  AND v_autorizado IS NOT INITIAL ) OR  v_possui_doc IS INITIAL.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja realmente realizar a transferência de material?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        CHECK var_answer EQ '1'.

*    IF p_file IS INITIAL.
*      PERFORM f_gravar_zppt0035.
*    ENDIF.


        LOOP AT it_saida ASSIGNING  <saida>.
          IF  <saida>-status EQ icon_led_green.
            CLEAR: wa_zppt0035.
            v_tabix = sy-tabix.
            IF <saida>-status EQ icon_led_green.

              REFRESH: tl_item  ,
                       tl_return.


              vl_code = '06'.
              CONCATENATE <saida>-data_documento+6(4) <saida>-data_documento+3(2) <saida>-data_documento(2) INTO sl_header-doc_date.
              CONCATENATE <saida>-data_lancamento+6(4) <saida>-data_lancamento+3(2) <saida>-data_lancamento(2) INTO sl_header-pstng_date.

*-------------BUG SOLTO #147296 / AOENNING V2*
              sl_item-move_type  =  <saida>-movimento.
*-------------BUG SOLTO #147296 / AOENNING V2*

*--> 19.06.2023 - Migration S4 – MIGNOW - Start
              "              sl_item-material   = <saida>-material.
              DATA(v_len1) = strlen( <saida>-material ).
              IF v_len1 > 18.
                sl_item-material_long = <saida>-material .
              ELSE.
                sl_item-material = <saida>-material .
              ENDIF.
*<-- 19.06.2023 - Migration S4 – MIGNOW – End
              sl_item-plant      = <saida>-centro.
              sl_item-stge_loc   = <saida>-deposito.
              sl_item-batch      = <saida>-lote.
              sl_item-entry_qnt  = <saida>-qtde.
              sl_item-move_plant = <saida>-centro_destino.
              sl_item-move_stloc = <saida>-deposito_destino.
              sl_item-move_mat   = <saida>-material_destino.
              sl_item-move_batch = <saida>-lote_destino.

              APPEND sl_item TO tl_item.

              sl_header-header_txt = p_form. "180771 - RGARCIA

              CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
                EXPORTING
                  goodsmvt_header  = sl_header
                  goodsmvt_code    = vl_code
                IMPORTING
                  materialdocument = vl_material
                  matdocumentyear  = vl_year
                TABLES
                  goodsmvt_item    = tl_item
                  return           = t_return.

              IF  vl_material IS NOT INITIAL.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.
                REFRESH t_return.


                CONCATENATE
                            vl_material
                            '/'
                            vl_year
                       INTO sl_return-msg SEPARATED BY space.

                CONCATENATE sl_return-msg 'Transf. de bloco Gerado'  INTO sl_return-msg SEPARATED BY space.

                sl_return-tabix = v_tabix.
                APPEND sl_return TO tl_return.

                "APPEND s_return TO t_return.

                "it_return[] = tl_return[].

                MOVE icon_checked TO <saida>-status.

                UPDATE zppt0035
                       SET  doc_material = vl_material
                             ano_doc_material = vl_year
                             status_proc = '2'
                      WHERE id_carga  = <saida>-id_carga
                        AND item_carga   = <saida>-item_carga.
*            ENDIF.


                COMMIT WORK AND WAIT .

              ELSE.
                v_erro_mat = 'X'.
                MOVE icon_led_red TO <saida>-status.
*     Retorna Erro
                LOOP AT t_return INTO s_return.

                  sl_return-msg = s_return-message.
                  sl_return-tabix = v_tabix.
                  APPEND sl_return TO tl_return.
                ENDLOOP.
                CLEAR: sl_item .
              ENDIF.

            ENDIF.
          ENDIF.
        ENDLOOP.
        IF v_erro_mat = 'X'.
          MESSAGE 'Itens da planilha não foram Transfereridos !' TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE 'Transferência realizada com sucesso!' TYPE 'S' DISPLAY LIKE 'S'.
        ENDIF.
      ELSE.
        MESSAGE 'Existem erros no arquivo, favor corrigir para poder realizar a transferência!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE 'Existem erros no arquivo, favor corrigir para poder realizar a transferência!' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.
  ENDIF.


ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  DATA: wl_layout TYPE slis_layout_alv.

  PERFORM f_fieldcatalog.
  variante = VALUE #(
                      report = sy-repid
                    ).

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.

  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-zebra        = abap_false.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLSTYLES'.

  IF g_grid IS INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'ALV'.

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = g_grid.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_hotspot_click  FOR g_grid,
    lcl_event_handler=>on_data_changed  FOR g_grid,
    lcl_event_handler=>data_changed_finished FOR g_grid.

    SET HANDLER:
    obg_toolbar->on_toolbar          FOR g_grid,
    obg_toolbar->handle_user FOR g_grid.


    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = t_function
        is_variant                    = variante
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.


  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].
  PERFORM f_estrutura_alv USING:
*01  02      03       04           05                06                    07    08    09   10   11   12   13   14   15   16   17
 01  ''      ''       'IT_SAIDA'  'STATUS'           'Status'              '04'  ' '   ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''      ''       'IT_SAIDA'  'DATA_DOCUMENTO'   'Dt. Documento'       '14'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''      ''       'IT_SAIDA'  'DATA_LANCAMENTO'  'Data Lançamento'     '16'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''      ''       'IT_SAIDA'  'MOVIMENTO'        'Tipo Movimento'      '14'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''      ''       'IT_SAIDA'  'CENTRO'           'Centro'              '7'   ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''      ''       'IT_SAIDA'  'DEPOSITO'         'Depósito'            '8'   'X'   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''      ''       'IT_SAIDA'  'MATERIAL'         'Material'            '20'  'X'   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''      ''       'IT_SAIDA'  'QTDE'             'Quantidade'          '10'  'X'   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''      ''       ''          'LOTE'             'Lote'                '10'   'X'   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''      ''       'IT_SAIDA'  'MATERIAL_DESTINO' 'Material Destino'    '20'  'X'   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  ''      ''       'IT_SAIDA'  'CENTRO_DESTINO'   'Centro Destino'      '15'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 12  ''      ''       'IT_SAIDA'  'DEPOSITO_DESTINO' 'Depósito Destino'    '17'  'X'   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 12  ''      ''       'IT_SAIDA'  'LOTE_DESTINO'     'Lote Destino'        '10'  'X'   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "17

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
* tratamento valores
**********************************************************************
FORM f_change_text CHANGING p_out_text.

  DATA: l_index     TYPE i,
        l_length    TYPE i,
        l_character TYPE c,
        l_allowed   TYPE string.

  TRANSLATE p_out_text TO LOWER CASE.

  l_allowed = '1234567890,'.
  l_length  = strlen( p_out_text ).
  l_index   = 0.

  WHILE l_length GT l_index.
    l_character = p_out_text+l_index(1).
    SEARCH l_allowed FOR l_character.
    IF sy-subrc NE 0.
      REPLACE l_character WITH space INTO p_out_text.
    ENDIF.
    l_index = l_index + 1.
  ENDWHILE.

  REPLACE ALL OCCURRENCES OF REGEX ',' IN p_out_text WITH '.'.
  CONDENSE p_out_text NO-GAPS.

ENDFORM.

**********************************************************************
* tratamento valores
**********************************************************************
FORM f_change_texto CHANGING p_out_text.

  DATA: l_index     TYPE i,
        l_length    TYPE i,
        l_character TYPE c,
        l_allowed   TYPE string.

  TRANSLATE p_out_text TO LOWER CASE.

  l_allowed = '1234567890.'.
  l_length  = strlen( p_out_text ).
  l_index   = 0.

  WHILE l_length GT l_index.
    l_character = p_out_text+l_index(1).
    IF l_character EQ ','.
      REPLACE l_character WITH '.' INTO p_out_text.
    ENDIF.
*    SEARCH l_allowed FOR l_character.
*    IF sy-subrc NE 0.
*
*    ENDIF.
    l_index = l_index + 1.
  ENDWHILE.
  CONDENSE p_out_text NO-GAPS.

ENDFORM.



****************************************************************
*                                                              *
*                FORM f_gravar_zppt0035 .                      *
*                                                              *
****************************************************************
FORM f_gravar_zppt0035 .
  DATA vstatus_proc(1).
  "  gera o ID_CARGA se ja foi gerado um ID não gera novamente.
  IF lv_number_range IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZMAN_MAT'
      IMPORTING
        number      = lv_number_range
        returncode  = lv_rc.
  ENDIF.


  IF p_file IS INITIAL.

    SELECT *
      FROM zppt0035
      INTO TABLE it_zppt0035
      WHERE data_documento = s_dt_doc-low AND
            data_lancamento = s_dt_lan-low AND
            movimento = s_tp_mov-low AND
            centro = s_centro-low AND
            centro_destino = s_centro-low AND nome_planilha = ' '.
  ELSE.
    SPLIT p_file AT '\' INTO TABLE it_texto.

    DATA(v_cont) = lines( it_texto ).

    READ TABLE it_texto INTO DATA(wa_texto) INDEX v_cont.
    IF sy-subrc IS INITIAL.

      v_file = wa_texto.

      SELECT *
        FROM zppt0035
        INTO TABLE it_zppt0035
       WHERE  nome_planilha = wa_texto AND
              doc_material = ' '.

      IF sy-subrc IS INITIAL.

        DELETE FROM zppt0035 WHERE nome_planilha = wa_texto AND doc_material = ' '.
        REFRESH  it_zppt0035.
      ENDIF.

    ENDIF.

  ENDIF.

  IF it_zppt0035[] IS INITIAL.
    LOOP AT it_saida ASSIGNING  <saida>.
      v_tabix = sy-tabix.
      "não grava se as linhas estiverem vazias
      IF <saida>-deposito IS INITIAL AND <saida>-material IS INITIAL AND <saida>-qtde IS INITIAL AND
        <saida>-lote IS INITIAL AND <saida>-material_destino IS INITIAL AND <saida>-deposito_destino IS INITIAL
         AND <saida>-lote_destino IS INITIAL OR <saida>-status EQ icon_checked.

        CONTINUE.

      ELSE.

        IF p_file IS NOT INITIAL.
          wa_zppt0035-tipo_carga = '1'.
          wa_zppt0035-nome_planilha = v_file.
          wa_zppt0035-linha_planilha = <saida>-linha_planilha.
        ELSE.
          wa_zppt0035-tipo_carga = '2'.
        ENDIF.

        wa_zppt0035-id_carga = lv_number_range. "SNUM
        wa_zppt0035-item_carga =  v_tabix .

        IF <saida>-erro NE 'X'.
          wa_zppt0035-status_proc = '2'.
        ELSE.
          wa_zppt0035-status_proc = '1'.
        ENDIF.
        CONCATENATE <saida>-data_documento+6(4) <saida>-data_documento+3(2) <saida>-data_documento(2) INTO wa_zppt0035-data_documento.
        "wa_zppt0035-data_documento   = (  ). "s_dt_doc-low.
        CONCATENATE <saida>-data_lancamento+6(4) <saida>-data_lancamento+3(2) <saida>-data_lancamento(2) INTO wa_zppt0035-data_lancamento.

        " wa_zppt0035-data_lancamento  = <saida>-data_lancamento.
        wa_zppt0035-movimento        = <saida>-movimento.
        wa_zppt0035-centro           = <saida>-centro.
        wa_zppt0035-deposito         = <saida>-deposito.
        wa_zppt0035-material         = <saida>-material .
        wa_zppt0035-qtde             = <saida>-qtde.
        wa_zppt0035-lote             = <saida>-lote.
        wa_zppt0035-material_destino = <saida>-material_destino.
        wa_zppt0035-centro_destino   = <saida>-centro_destino.
        wa_zppt0035-deposito_destino = <saida>-deposito_destino.
        wa_zppt0035-lote_destino     = <saida>-lote_destino.
        wa_zppt0035-usuario          = sy-uname.
        wa_zppt0035-data             = sy-datum.
        wa_zppt0035-hora             = sy-uzeit.
        wa_zppt0035-nserie           = <saida>-nserie. "  180771 -  CS2025000597  - RGA

        MODIFY zppt0035 FROM wa_zppt0035.

        MOVE lv_number_range TO <saida>-id_carga.
        MOVE sy-tabix        TO <saida>-item_carga.

      ENDIF.

    ENDLOOP.

  ELSE.
    DATA vv_tabix TYPE sy-tabix.
    LOOP AT it_saida ASSIGNING  <saida>.
      vv_tabix = sy-tabix.
      READ TABLE it_zppt0035 INTO wa_zppt0035 WITH KEY id_carga = <saida>-id_carga
                                                       item_carga =  <saida>-item_carga.

      IF sy-subrc EQ '0' AND  wa_zppt0035-doc_material IS INITIAL.

        IF <saida>-erro IS INITIAL.
          vstatus_proc = '2'.
        ELSE.
          vstatus_proc = '1'.
        ENDIF.

        UPDATE zppt0035
        SET   deposito = <saida>-deposito
              material = <saida>-material
              qtde     = <saida>-qtde
              lote     = <saida>-lote
              status_proc = vstatus_proc
              material_destino =  <saida>-material_destino
              deposito_destino =  <saida>-deposito_destino
              lote_destino =  <saida>-lote_destino
              usuario = sy-uname
              data = sy-datum
              hora = sy-uzeit
        WHERE id_carga =  <saida>-id_carga AND
              item_carga =  <saida>-item_carga.
      ELSE.

        IF <saida>-deposito IS INITIAL AND <saida>-material IS INITIAL AND <saida>-qtde IS INITIAL AND
        <saida>-lote IS INITIAL AND <saida>-material_destino IS INITIAL AND <saida>-deposito_destino IS INITIAL
         AND <saida>-lote_destino IS INITIAL OR <saida>-status EQ icon_checked.

          CONTINUE.
        ELSE.

          CLEAR wa_zppt0035.
          wa_zppt0035-tipo_carga = '2'.
          wa_zppt0035-id_carga = lv_number_range. "SNUM
          wa_zppt0035-item_carga =  vv_tabix.

          IF <saida>-erro NE 'X'.
            wa_zppt0035-status_proc = '2'.
          ELSE.
            wa_zppt0035-status_proc = '1'.
          ENDIF.
          CONCATENATE <saida>-data_documento+6(4) <saida>-data_documento+3(2) <saida>-data_documento(2) INTO wa_zppt0035-data_documento.
          "wa_zppt0035-data_documento   = (  ). "s_dt_doc-low.
          CONCATENATE <saida>-data_lancamento+6(4) <saida>-data_lancamento+3(2) <saida>-data_lancamento(2) INTO wa_zppt0035-data_lancamento.

          " wa_zppt0035-data_lancamento  = <saida>-data_lancamento.
          wa_zppt0035-movimento        = <saida>-movimento.
          wa_zppt0035-centro           = <saida>-centro.
          wa_zppt0035-deposito         = <saida>-deposito.
          wa_zppt0035-material         = <saida>-material .
          wa_zppt0035-qtde             = <saida>-qtde.
          wa_zppt0035-lote             = <saida>-lote.
          wa_zppt0035-material_destino = <saida>-material_destino.
          wa_zppt0035-centro_destino   = <saida>-centro_destino.
          wa_zppt0035-deposito_destino = <saida>-deposito_destino.
          wa_zppt0035-lote_destino     = <saida>-lote_destino.
          wa_zppt0035-usuario          = sy-uname.
          wa_zppt0035-data             = sy-datum.
          wa_zppt0035-hora             = sy-uzeit.
          wa_zppt0035-nserie           = <saida>-nserie. "180771 RGARCIA



          MODIFY zppt0035 FROM wa_zppt0035.

          MOVE lv_number_range TO <saida>-id_carga.
          MOVE sy-tabix        TO <saida>-item_carga.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM f_verificar_erros USING p_view .


*    CALL FUNCTION 'Z_DOC_CHECK_NEW'
*    EXPORTING
*      i_screen    = '100'
*      i_repid     = sy-repid
*      i_show      = p_view
*      i_set_field = 'X_FIELD'
*    IMPORTING
*      e_messagem  = msg_alv
*    TABLES
*      it_msgs     = it_return.





  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
      i_repid     = sy-repid
      i_show      = p_view
      i_set_field = 'X_FIELD'
    IMPORTING
      e_messagem  = msg_alv
    TABLES
      it_msgs     = it_return.

  c_xx = 'X'.

ENDFORM.

FORM buscar_linhas_gravadas .
  SELECT *
  FROM zppt0035
  INTO TABLE it_zppt0035
  WHERE data_documento = s_dt_doc-low AND
        data_lancamento = s_dt_lan-low AND
        movimento = s_tp_mov-low AND
        centro = s_centro-low AND nome_planilha = ' '.

  LOOP AT it_zppt0035 INTO wa_zppt0035.
    IF wa_zppt0035-doc_material IS INITIAL.

      CONCATENATE wa_zppt0035-data_documento+6(2)'.' wa_zppt0035-data_documento+4(2)'.' s_dt_doc-low(4) INTO wa_saida-data_documento.
      CONCATENATE wa_zppt0035-data_lancamento+6(2)'.' wa_zppt0035-data_lancamento+4(2)'.' wa_zppt0035-data_lancamento(4) INTO wa_saida-data_lancamento.

      "wa_saida-data_documento   = wa_zppt0035-data_documento.
      "wa_saida-data_lancamento  = wa_zppt0035-data_lancamento.
      wa_saida-movimento        = wa_zppt0035-movimento.
      wa_saida-centro           = wa_zppt0035-centro.
      wa_saida-deposito         = wa_zppt0035-deposito.
      wa_saida-material         = wa_zppt0035-material.
      wa_saida-qtde             = wa_zppt0035-qtde.
      "            PERFORM f_change_texto CHANGING wa_saida-qtde.
      wa_saida-lote             = wa_zppt0035-lote.
      wa_saida-material_destino = wa_zppt0035-material_destino.
      wa_saida-centro_destino   = wa_zppt0035-centro_destino.
      wa_saida-deposito_destino = wa_zppt0035-deposito_destino.
      wa_saida-lote_destino     = wa_zppt0035-lote_destino.
      wa_saida-id_carga         = wa_zppt0035-id_carga.
      wa_saida-item_carga       = wa_zppt0035-item_carga.
      wa_saida-doc_material     = wa_zppt0035-doc_material.
      wa_saida-nserie           = wa_zppt0035-nserie. "  180771 -  CS2025000597  - RGA
      APPEND wa_saida TO it_saida.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_autorizar .



  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows IS NOT INITIAL.

  LOOP AT it_sel_rows INTO wa_sel_rows.
    READ TABLE it_saida ASSIGNING <saida> INDEX wa_sel_rows-index.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    IF <saida>-status EQ icon_led_yellow.
      MOVE ' ' TO <saida>-status.
      MOVE ' ' TO <saida>-erro.
      MOVE 'X' TO <saida>-autorizado.
    ENDIF.

  ENDLOOP.

  PERFORM validar_linhas USING abap_true.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_salvar .
  IF p_file IS INITIAL.
    LOOP AT it_saida INTO wa_saida.



    ENDLOOP.
  ENDIF.
ENDFORM.
