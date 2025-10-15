*&---------------------------------------------------------------------*
*& Report ZLESR0163
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlesr0163.
TABLES: j_1bnfdoc, j_1bnflin, zsdt0001, zlest0248, zlest0249.


TYPES:

  BEGIN OF ty_saida, "relatório
    id_mistura     TYPE zlest0248-id_mistura,
    id_item        TYPE zlest0249-id_item,
    chave_nfe      TYPE zde_chave_nfe,
    docnum         TYPE j_1bdocnum,
    chave_romaneio TYPE zlest0249-chave_romaneio,
    filial         TYPE j_1bnfdoc-branch,
    dt_mistura     TYPE j_1bnfdoc-docdat,
    doc_mat        TYPE zlest0249-mblnr,
    observacao     TYPE zlest0248-observacoes,
    ano_doc        TYPE zlest0249-mjahr,
  END OF ty_saida,

  BEGIN OF ty_saida_02, "cadastro
    chave_nfe      TYPE zde_chave_nfe,
    docnum         TYPE j_1bdocnum,
    chave_romaneio TYPE zsdt0001-ch_referencia,
    filial         TYPE j_1bnfdoc-branch,
    quantidade     TYPE j_1bnflin-menge,
    unidade        TYPE j_1bnflin-meins,
    numero         TYPE j_1bnfdoc-nfenum,
    dt_emissao     TYPE j_1bnfdoc-docdat,
    matnr          TYPE j_1bnflin-matnr,
    doc_mat        TYPE zlest0249-mblnr,
    id_mistura     TYPE zlest0249-id_mistura,
    id_item        TYPE zlest0249-id_item,
    status(4),
    erro(200),
  END OF ty_saida_02,



  BEGIN OF ty_editor,
    line(255),
  END   OF ty_editor.

DATA: wa_dt_mistura TYPE zlest0248-data,
      wa_id_mistura TYPE zlest0248-id_mistura,
      wa_obs        TYPE string,
      v_operacao    TYPE string,
      v_salvo(1),
      v_erros(1).

DATA: it_saida    TYPE STANDARD TABLE OF ty_saida,
      wa_saida    TYPE  ty_saida,
      it_saida_02 TYPE STANDARD TABLE OF ty_saida_02,
      wa_saida_02 TYPE  ty_saida_02.

DATA: it_zlest0248 TYPE STANDARD TABLE OF zlest0248,
      it_zlest0249 TYPE STANDARD TABLE OF zlest0249,

      wa_zlest0248 TYPE zlest0248,
      wa_zlest0249 TYPE zlest0249,
      wa_depara    TYPE zsdt_depara_depo.

DATA: dg_splitter_1         TYPE REF TO cl_gui_splitter_container,
      g_grid                TYPE REF TO cl_gui_alv_grid,
      g_grid_10             TYPE REF TO cl_gui_alv_grid,
      g_custom_container    TYPE REF TO cl_gui_custom_container,
      g_custom_container_10 TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager  TYPE REF TO cl_alv_grid_toolbar_manager,
      obj_cont              TYPE REF TO cl_gui_custom_container,
      container_1           TYPE REF TO cl_gui_container,
      cl_container_95       TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id         TYPE REF TO cl_dd_document,
      picture               TYPE REF TO cl_gui_picture,
      l_graphic_conv        TYPE i,
      l_graphic_offs        TYPE i,
      graphic_size          TYPE i,
      l_graphic_xstr        TYPE xstring,
      url(255)              TYPE c,
      graphic_url(255),
      t_function            TYPE ui_functions,
      w_function            TYPE ui_func,
      t_fieldcat            TYPE lvc_t_fcat,
      w_fieldcat            TYPE lvc_s_fcat,
      t_colorcell           TYPE TABLE OF lvc_s_scol,
      w_colorcell           TYPE lvc_s_scol,
      t_exctab              TYPE slis_t_extab,
      w_exctab              TYPE slis_extab,
      w_layout              TYPE lvc_s_layo,
      w_stable              TYPE lvc_s_stbl,
      t_style               TYPE lvc_t_styl,
      w_style               TYPE lvc_s_styl,
      t_rows                TYPE lvc_t_row,
      w_rows                TYPE lvc_s_row,
      ok_code               LIKE sy-ucomm,
      it_sel_rows           TYPE lvc_t_row,
      wa_sel_rows           TYPE lvc_s_row,
      variante              LIKE disvariant,
      gs_variant_c          TYPE disvariant,
      txtopen               TYPE c,
      editor                TYPE REF TO cl_gui_textedit,
      c_editor              TYPE REF TO cl_gui_custom_container,
      it_editor             TYPE TABLE OF ty_editor,
      wa_editor             TYPE ty_editor.

CONSTANTS: c_del(3)  TYPE c VALUE 'DEL'.

FIELD-SYMBOLS: <fs_table>    TYPE STANDARD TABLE,
               <fs_line>     TYPE any,
               <fs_line_aux> TYPE any,
               <fs_campo>    TYPE any,
               <saida>       TYPE ty_saida_02,
               <saida_princ> TYPE ty_saida.

DATA: ty_toolbar TYPE stb_button.
CLASS: lcl_alv_toolbar    DEFINITION DEFERRED.
DATA: obg_toolbar    TYPE REF TO lcl_alv_toolbar,
      obg_toolbar_10 TYPE REF TO lcl_alv_toolbar.
DATA: v_number  TYPE i,
      e_vbeln   TYPE zdoc_rem,
      e_ov      TYPE lips-vgbel,
      e_auart   TYPE vbak-auart,
      e_vstel   TYPE vbap-vstel,
      e_lifnr   TYPE lifnr,
      v_len1    TYPE i,
      p_visu(1).

DATA: zcl_util       TYPE REF TO zcl_util.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-010.
  SELECT-OPTIONS: p_data   FOR zlest0248-data,
              p_ch_nfe FOR zlest0249-chave_nfe,
              p_docnum FOR zlest0249-docnum,
              p_mblnr  FOR zlest0249-mblnr.

SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM: fm_seleciona_dados,
           fm_processa_dados,
           f_init_alv_10.

  CALL SCREEN 0010.

***********************************************************************
** classes / implementacoes
***********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
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


      on_toolbar_10 FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_10 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
    CREATE OBJECT zcl_util.
    DATA: wa_good_cells TYPE lvc_s_modi.
    DATA: v_tabix       TYPE sy-tabix,
          it_documentos TYPE j_1b_tt_nfe_active.
    CLEAR: it_sel_rows[], wa_sel_rows.

    LOOP AT er_data_changed->mt_good_cells INTO wa_good_cells WHERE fieldname EQ 'CHAVE_NFE'.

      READ TABLE it_saida_02 ASSIGNING <saida> INDEX wa_good_cells-row_id.
      CHECK sy-subrc EQ 0.

      v_tabix  = wa_good_cells-row_id.

      IF wa_good_cells-value IS INITIAL.
        CLEAR: <saida>.

        CALL METHOD g_grid->refresh_table_display
          EXPORTING
            is_stable = w_stable.

        RETURN.
      ENDIF.

      IF <saida>-chave_nfe IS INITIAL.
        <saida>-chave_nfe = wa_good_cells-value.
      ENDIF.

      CALL METHOD zcl_util->get_docnum
        EXPORTING
          i_chave_nfe  = <saida>-chave_nfe
        RECEIVING
          t_documentos = it_documentos[].

      DELETE it_documentos WHERE NOT ( form NE space AND docsta EQ '1' AND scssta NE '2'  AND direct EQ '2' AND cancel EQ space ).
      READ TABLE it_documentos INTO DATA(wa_documento) INDEX 1.
      CHECK sy-subrc EQ 0 AND it_documentos[] IS NOT INITIAL.

      <saida>-docnum =   wa_documento-docnum.
      DATA(wa_romaneio) = zcl_les_utils=>get_romaneio_documento_fiscal( i_docnum = <saida>-docnum ).

      IF wa_romaneio IS NOT INITIAL.
        <saida>-chave_romaneio = wa_romaneio-ch_referencia.
      ENDIF.

      SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_1bnfdoc) WHERE docnum = @<saida>-docnum.
      IF sy-subrc IS INITIAL.
        <saida>-filial     = wa_1bnfdoc-branch.
        <saida>-numero     = wa_1bnfdoc-nfenum.
        <saida>-dt_emissao = wa_1bnfdoc-docdat.
      ENDIF.

      SELECT SINGLE * FROM j_1bnflin INTO @DATA(wa_1bnflin) WHERE docnum EQ @wa_documento-docnum.

      IF sy-subrc IS INITIAL.
        <saida>-quantidade = wa_1bnflin-menge.
        <saida>-unidade    = wa_1bnflin-meins.
        <saida>-matnr      = wa_1bnflin-matnr.
      ENDIF.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = w_stable.


    ENDLOOP.


  ENDMETHOD.

  METHOD data_changed_finished.


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
    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'MOVIMENTO'.
    wa_tool-icon      = icon_relation.

    IF  ( v_salvo = abap_true OR p_visu EQ abap_true ).
      wa_tool-disabled = abap_false.
    ELSE.
      wa_tool-disabled = abap_true.
    ENDIF.

    wa_tool-quickinfo = 'Movimento'.
    wa_tool-text      = 'Gerar Movimento'.
    APPEND wa_tool TO e_object->mt_toolbar.

    CLEAR wa_tool.

  ENDMETHOD.

  METHOD on_toolbar_10.
    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'VISUALIZAR'.
    wa_tool-icon      = icon_display.
    wa_tool-quickinfo = 'Visualizar Lançamento'.
    wa_tool-text      = 'Visualizar'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'NOVO'.
    wa_tool-icon      = icon_create.
    wa_tool-quickinfo = 'Novo'.
    wa_tool-text      = 'Novo'.
    APPEND wa_tool TO e_object->mt_toolbar.

    CLEAR wa_tool.
    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-icon      = icon_delete_row.
    wa_tool-function  = 'DEL'.
    wa_tool-text      = 'Excluir'.
    wa_tool-butn_type = 0.
    APPEND wa_tool TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_10.

    CLEAR: v_operacao.

    CASE e_ucomm.
      WHEN 'NOVO'.
        txtopen = 'X'.

        v_operacao = e_ucomm.

        CLEAR: wa_dt_mistura, wa_obs, p_visu, v_salvo, it_editor[].
        FREE it_saida_02.
        CALL SCREEN 0100.

        PERFORM: fm_seleciona_dados,
                 fm_processa_dados.

      WHEN 'VISUALIZAR'.

        v_operacao = e_ucomm.

        PERFORM f_visualizar.
      WHEN 'DEL'.
        PERFORM f_deletar.

    ENDCASE.

    CALL METHOD g_grid_10->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

  METHOD handle_user.

    CASE e_ucomm.
      WHEN 'MOVIMENTO'.

        IF v_salvo = abap_true OR p_visu EQ abap_true.

          PERFORM f_validar_lancamento.

          CHECK v_erros IS INITIAL.

          PERFORM f_gera_movimentacao USING wa_id_mistura.

        ELSE.
          MESSAGE   'Necessário gravar dados antes de Gerar movimentação' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      WHEN c_del.

        LOOP AT it_sel_rows INTO DATA(_cell).
          DELETE it_saida_02 INDEX _cell-index.
        ENDLOOP.

    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.


  ENDMETHOD.




ENDCLASS.

MODULE user_command_0100 INPUT.
  FREE: t_rows[].
  DATA: v_index  TYPE string,
        v_numero TYPE string.
  DATA: w_text TYPE string.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.

      CALL METHOD c_editor->free( ).
      CALL METHOD editor->free( ).
      FREE: c_editor, editor.

      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.

      IF v_operacao = 'NOVO' AND v_salvo IS NOT INITIAL.
        MESSAGE 'Lançamento já foi salvo!' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF p_visu IS NOT INITIAL.
        MESSAGE 'Modo de visualização! Operação não permitida!' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      PERFORM f_validar_lancamento.
      IF v_erros IS NOT INITIAL.
        MESSAGE 'Impossivel salvar, verifique os erros!' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'Z_MIST_EUD'
        IMPORTING
          number      = v_number.

      CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).

      CLEAR: wa_zlest0248-observacoes,
             wa_editor.

      LOOP AT it_editor INTO wa_editor.
        wa_zlest0248-observacoes = |{ wa_zlest0248-observacoes } { wa_editor-line }|.
      ENDLOOP.

      wa_id_mistura            = v_number.

      wa_zlest0248-id_mistura  = v_number.
      wa_zlest0248-data        = wa_dt_mistura.
      wa_zlest0248-dt_registro = sy-datum.
      wa_zlest0248-hr_registro = sy-uzeit.
      wa_zlest0248-us_registro = sy-uname.
      MODIFY zlest0248 FROM  wa_zlest0248.

      LOOP AT it_saida_02 ASSIGNING FIELD-SYMBOL(<fs_saida_02>) .
        v_index = sy-index.

        <fs_saida_02>-id_mistura    = v_number.
        <fs_saida_02>-id_item       = v_index.

        wa_zlest0249-id_mistura     = <fs_saida_02>-id_mistura.
        wa_zlest0249-id_item        = <fs_saida_02>-id_item.
        wa_zlest0249-chave_nfe      = <fs_saida_02>-chave_nfe.
        wa_zlest0249-chave_romaneio = <fs_saida_02>-chave_romaneio.
        wa_zlest0249-docnum         = <fs_saida_02>-docnum.
        wa_zlest0249-filial         = <fs_saida_02>-filial.

        MODIFY zlest0249 FROM  wa_zlest0249.
      ENDLOOP.

      v_salvo = 'X'.
      MESSAGE 'Registros foram salvos!' TYPE 'S' .

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.
ENDMODULE.

MODULE user_command_0010 INPUT.
  FREE: t_rows[].

  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  FREE ok_code.
ENDMODULE.

MODULE status_0010 OUTPUT.
  SET PF-STATUS 'ST0010'.
  SET TITLEBAR 'TL0010'.
ENDMODULE.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF v_salvo IS NOT INITIAL.
    txtopen = abap_false.
  ENDIF.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'WA_DT_MISTURA'.

        IF p_visu IS NOT INITIAL OR v_salvo IS NOT INITIAL.
          screen-input = 0. "CAMPO FECHADO
          MODIFY SCREEN.
        ENDIF.

      WHEN 'WA_ID_MISTURA' OR 'TXT_ID_MISTURA'.

        IF p_visu IS INITIAL AND v_salvo IS INITIAL.
          screen-invisible = 1.
          screen-output = 0.
        ENDIF.

        screen-input = 0. "CAMPO FECHADO

        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
  PERFORM f_init_alv.
ENDMODULE.

MODULE status_0200 OUTPUT.
  PERFORM f_caixa_txt_obs.
ENDMODULE.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].
  PERFORM f_estrutura_alv USING:
*01  02      03       04             05               06                    07    08    09   10   11   12   13   14   15   16   17
 01  ''      ''       'IT_SAIDA_02'  'CHAVE_NFE'      'Chave NFE'           '45'  'X'   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''      ''       'IT_SAIDA_02'  'DOCNUM'         'Docnum'              '08'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''      ''       'IT_SAIDA_02'  'FILIAL'         'Filial'              '06'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''      ''       'IT_SAIDA_02'  'QUANTIDADE'     'Quantidade'          '10'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''      ''       'IT_SAIDA_02'  'UNIDADE'        'Unidade'             '07'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''      ''       'IT_SAIDA_02'  'NUMERO'         'Número'              '10'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''      ''       'IT_SAIDA_02'  'DT_EMISSAO'     'Data Emissão'        '12'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''      ''       'IT_SAIDA_02'  'DOC_MAT'        'Doc.Mat.Mistura'     '15'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''      ''       'IT_SAIDA_02'  'ERRO'           'Erros'               '45'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.

FORM f_fieldcatalog_rel.

  FREE t_fieldcat[].
  PERFORM f_estrutura_alv USING:
*01  02      03       04           05                06                    07    08    09   10   11   12   13   14   15   16   17
 01  ''      ''       'IT_SAIDA'  'ID_MISTURA'       'ID Mistura'          '09'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''      ''       'IT_SAIDA'  'CHAVE_NFE'        'Chave NFE'           '44'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''      ''       'IT_SAIDA'  'DOCNUM'           'Docnum'              '08'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''      ''       'IT_SAIDA'  'FILIAL'           'Filial'              '06'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''      ''       'IT_SAIDA'  'DOC_MAT'          'Doc. Material'       '13'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''      ''       'IT_SAIDA'  'DT_MISTURA'       'Data Mistura'        '12'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''      ''       'IT_SAIDA'  'OBSERVACAO'       'Observações'         '20'  ' '   ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.
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
  IF w_fieldcat-fieldname EQ 'CHAVE_NFE' AND ( v_salvo IS NOT INITIAL OR p_visu IS NOT INITIAL ).
    w_fieldcat-edit        = ' '.
  ELSE.
    w_fieldcat-edit        = p_edit.
  ENDIF.
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
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  DATA: gr_event_handler TYPE REF TO lcl_event_handler.

  DATA: wl_layout TYPE slis_layout_alv.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'CONTAINER'.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = g_custom_container.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = g_grid.

    SET HANDLER: lcl_event_handler=>on_data_changed  FOR g_grid,
                 lcl_event_handler=>data_changed_finished FOR g_grid,
                 obg_toolbar->on_toolbar          FOR g_grid,
                 obg_toolbar->handle_user FOR g_grid.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  PERFORM f_fieldcatalog.

  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      "it_toolbar_excluding          = t_fun
      is_layout                     = w_layout
      is_variant                    = gs_variant_c
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida_02[]
      it_fieldcatalog               = t_fieldcat[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

ENDFORM.

FORM f_init_alv_10.

  DATA: gr_event_handler TYPE REF TO lcl_event_handler.

  DATA: wl_layout TYPE slis_layout_alv.

  IF g_custom_container_10 IS INITIAL.
    CREATE OBJECT g_custom_container_10
      EXPORTING
        container_name = 'CONTAINER_01'.

    CREATE OBJECT g_grid_10
      EXPORTING
        i_parent = g_custom_container_10.

    CREATE OBJECT obg_toolbar_10
      EXPORTING
        io_alv_grid = g_grid_10.

    SET HANDLER:
    obg_toolbar_10->on_toolbar_10  FOR g_grid_10,
    obg_toolbar_10->handle_user_10 FOR g_grid_10.

    w_layout-sel_mode   = 'A'.

    PERFORM f_fieldcatalog_rel.

    CALL METHOD g_grid_10->set_table_for_first_display
      EXPORTING
        "it_toolbar_excluding          = t_fun
        is_layout                     = w_layout
        is_variant                    = gs_variant_c
        i_save                        = 'A'
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = t_fieldcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD g_grid_10->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

FORM f_validar_lancamento .

  CLEAR: v_erros.

  IF it_saida_02[] IS INITIAL.
    MESSAGE 'Nenhuma Nota fiscal informada!' TYPE 'I'.
    v_erros = 'X'.
    RETURN.
  ENDIF.

  LOOP AT it_saida_02 ASSIGNING <saida>.

    CLEAR: <saida>-erro.

    IF <saida>-chave_nfe IS INITIAL.
      <saida>-erro = 'Chave NF-e não informada!'.
      v_erros = 'X'.
      CONTINUE.
    ENDIF.

    IF <saida>-docnum IS INITIAL.
      <saida>-erro = |Documento para Chave NF-e { <saida>-chave_nfe } não encontrado!|.
      v_erros = 'X'.
      CONTINUE.
    ENDIF.

    "A nota precisa ser eudr
    IF zcl_eudr_utils=>check_doc_fiscal_eudr( i_docnum = <saida>-docnum ) NE 'S'.
      <saida>-erro = 'Nota informada não é EUDR!'.
      v_erros = 'X'.
      CONTINUE.
    ENDIF.

    "Não pode estar vinculada a um retorno simbolico -> Existir na tabela ZSDT_RETLOTE-docnum
    SELECT SINGLE * FROM zsdt_retlote INTO @DATA(wa_ret_simbolico) WHERE docnum EQ @<saida>-docnum.
    IF sy-subrc IS INITIAL.
      CONCATENATE <saida>-erro ' Nota vinculada a um retorno simbólico!' INTO <saida>-erro SEPARATED BY space  .
      v_erros = 'X'.
      CONTINUE.
    ENDIF.

    IF <saida>-chave_romaneio IS INITIAL.
      CONCATENATE <saida>-erro 'Nota não possui Romaneio vinculado!' INTO <saida>-erro SEPARATED BY  space  .
      v_erros = 'X'.
      CONTINUE.
    ENDIF.

    IF v_operacao = 'NOVO' AND v_salvo IS INITIAL.
      "não podemos ter movimentações duplicadas para a mesma NF.
      SELECT SINGLE * FROM zlest0249 INTO @DATA(wa_zlest0249) WHERE chave_nfe EQ @<saida>-chave_nfe.
      IF sy-subrc IS INITIAL.
        <saida>-erro = |A NF informada ja esta no informativo mistura Id:{ wa_zlest0249-id_mistura }!|.
        v_erros = 'X'.
        CONTINUE.
      ENDIF.
    ENDIF.

    "se a remessa não estiver na ZSDT0023 tbm dar erro
    CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
      EXPORTING
        i_docnum = <saida>-docnum
        i_direct = '2'
      IMPORTING
        e_vbeln  = e_vbeln.

    IF e_vbeln IS INITIAL.
      CONCATENATE <saida>-erro 'NF não possui remessa!' INTO <saida>-erro SEPARATED BY  space  .
      v_erros = 'X'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE * FROM zsdt0023 INTO @DATA(wa_zsdt0023) WHERE vbeln EQ @e_vbeln.
    IF sy-subrc NE 0.
      CONCATENATE <saida>-erro 'Remessa não vinculada ao processo EUDR!' INTO <saida>-erro SEPARATED BY  space  .
      v_erros = 'X'.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDFORM.

FORM f_caixa_txt_obs .

  IF c_editor IS NOT INITIAL AND editor IS NOT INITIAL.

    CASE txtopen.
      WHEN 'X'.
        CALL METHOD editor->set_readonly_mode( readonly_mode = editor->false ).
      WHEN OTHERS.
        CALL METHOD editor->set_readonly_mode( readonly_mode = editor->true ).
    ENDCASE.

    EXIT.
  ENDIF.

  CREATE OBJECT: c_editor EXPORTING container_name = 'C_EDITOR', "CONTAINER
                   editor EXPORTING parent         = c_editor.

  CALL METHOD editor->set_toolbar_mode( toolbar_mode = editor->false ).
  CALL METHOD editor->set_statusbar_mode( statusbar_mode = editor->false ).

  CASE txtopen.
    WHEN 'X'.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->false ).
    WHEN OTHERS.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->true ).
  ENDCASE.

  CALL METHOD editor->set_text_as_r3table
    EXPORTING
      table = it_editor.

ENDFORM.

FORM fm_seleciona_dados .

  SELECT *
    FROM zlest0248
    INTO TABLE it_zlest0248
    WHERE data IN p_data.

  IF  it_zlest0248[] IS NOT INITIAL.

    SELECT *
      FROM zlest0249
      INTO TABLE it_zlest0249
      FOR ALL ENTRIES IN it_zlest0248
      WHERE id_mistura EQ it_zlest0248-id_mistura
        AND chave_nfe IN p_ch_nfe
        AND docnum IN p_docnum
        AND mblnr IN p_mblnr.

  ENDIF.

ENDFORM.

FORM fm_processa_dados .

  CLEAR: it_saida[].

  LOOP AT it_zlest0249[] INTO DATA(wa_zlest0249).

    READ TABLE it_zlest0248 INTO DATA(wa_zlest0248) WITH KEY id_mistura = wa_zlest0249-id_mistura.

    IF sy-subrc IS INITIAL.
      wa_saida-dt_mistura = wa_zlest0248-data.
      wa_saida-observacao = wa_zlest0248-observacoes.
      wa_saida-id_mistura = wa_zlest0248-id_mistura.
    ENDIF.

    wa_saida-chave_nfe      = wa_zlest0249-chave_nfe.
    wa_saida-doc_mat        = wa_zlest0249-mblnr.
    wa_saida-docnum         = wa_zlest0249-docnum.
    wa_saida-filial         = wa_zlest0249-filial.
    wa_saida-chave_romaneio = wa_zlest0249-chave_romaneio.
    wa_saida-id_item        = wa_zlest0249-id_item.
    wa_saida-doc_mat        = wa_zlest0249-mblnr.
    wa_saida-ano_doc        = wa_zlest0249-mjahr.

    APPEND wa_saida TO it_saida.

  ENDLOOP.

ENDFORM.

FORM f_gera_movimentacao USING p_id_mistura TYPE zlest0249-id_mistura.

  DATA: v_erro_mat(1),
        v_status(1),
        v_autorizado(1),
        v_possui_doc(1),
        v_qtd           TYPE j_1bnflin-menge,
        v_matnr         TYPE j_1bnflin-matnr,
        v_filial        TYPE j_1bnfdoc-branch.

  DATA: sl_header   TYPE bapi2017_gm_head_01,
        lwa_code    TYPE bapi2017_gm_code,
        vl_material TYPE bapi2017_gm_head_ret-mat_doc,
        vl_year     TYPE bapi2017_gm_head_ret-doc_year,
        tl_item     TYPE TABLE OF bapi2017_gm_item_create,

        sl_item     TYPE bapi2017_gm_item_create,
        sl_zppt0035 TYPE zppt0035,
        vl_index    TYPE i,
        var_answer  TYPE c,
        t_return    TYPE TABLE OF bapiret2,
        s_return    TYPE bapiret2,
        v_tabix     TYPE sy-tabix,
        sl_return   TYPE zfiwrs0002, "bapiret2,
        tl_return   TYPE TABLE OF zfiwrs0002,
        it_return   TYPE TABLE OF zfiwrs0002.
  CLEAR sl_header.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente realizar a mistura de produto?'
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

  CHECK p_id_mistura IS NOT INITIAL.

  SELECT SINGLE *
    FROM zlest0248 INTO @DATA(lwa_zlest0248)
   WHERE id_mistura EQ @p_id_mistura.

  SELECT *
    FROM zlest0249 INTO TABLE @DATA(lit_zlest0249)
   WHERE id_mistura EQ @p_id_mistura.

  LOOP AT lit_zlest0249 ASSIGNING FIELD-SYMBOL(<fs_zlest0249>).

    READ TABLE it_saida_02 ASSIGNING <saida> WITH KEY id_mistura = <fs_zlest0249>-id_mistura
                                                      id_item    = <fs_zlest0249>-id_item.
    CHECK sy-subrc EQ 0.

    v_tabix = sy-tabix.

    IF <fs_zlest0249>-docnum IS INITIAL.
      MESSAGE |Docnumento Fiscal Item { <fs_zlest0249>-id_item } não encontrado!| TYPE 'I'.
      RETURN.
    ENDIF.

    IF <fs_zlest0249>-mblnr IS NOT INITIAL.
      MESSAGE |Mov. Estoque já gerado para o documento Fiscal { <fs_zlest0249>-docnum }!| TYPE 'I'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0001 INTO @DATA(lwa_zsdt0001_e)
     WHERE chave_nfe    EQ @<fs_zlest0249>-chave_nfe
       AND tp_movimento EQ 'E'.

    IF sy-subrc ne 0.
      MESSAGE |Nenhum Romaneio de entrada não localizado para NFe: { <fs_zlest0249>-chave_nfe }!| TYPE 'I'.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(lwa_lin)
     WHERE docnum EQ @<fs_zlest0249>-docnum.

    IF sy-subrc NE 0.
      MESSAGE |Item Documento Fiscal { <fs_zlest0249>-docnum } não encontrado!| TYPE 'I'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(lwa_doc)
     WHERE docnum EQ @<fs_zlest0249>-docnum.

    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZDOCUMENTO_NF_REMESSA'
      EXPORTING
        i_docnum = <fs_zlest0249>-docnum
        i_direct = '2'
      IMPORTING
        e_vbeln  = e_vbeln.

    IF e_vbeln IS INITIAL.
      MESSAGE |Remessa para docnumento { <fs_zlest0249>-docnum } não encontrado!| TYPE 'I'.
      RETURN.
    ENDIF.

    SELECT SINGLE vgbel FROM lips INTO e_ov WHERE vbeln EQ e_vbeln.

    IF sy-subrc NE 0 OR e_ov IS INITIAL.
      MESSAGE |OV para a remessa { e_vbeln } não encontrado!| TYPE 'I'.
      RETURN.
    ENDIF.

    SELECT SINGLE vbeln ,auart, kvgr3 FROM vbak INTO @DATA(lwa_vbak) WHERE vbeln EQ @e_ov.
    CHECK sy-subrc EQ 0.

    IF lwa_vbak-auart EQ 'ZIND'.
      MESSAGE |Nota fiscais de industrialização não podem ser misturadas! Docnum: { <fs_zlest0249>-docnum }!| TYPE 'I'.
      RETURN.
    ENDIF.

    SELECT SINGLE vbeln ,vstel FROM vbap INTO @DATA(lwa_vbap) WHERE vbeln EQ @e_ov.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE vbeln ,lifnr FROM vbpa INTO @DATA(lwa_vbpa_z1) WHERE vbeln EQ @e_ov AND parvw EQ 'Z1'..
    IF sy-subrc NE 0.
      MESSAGE |Parceiro Z1 para a OV { e_ov } não encontrado!| TYPE 'I'.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zsdt0023 INTO @DATA(wa_zsdt0023) WHERE vbeln EQ @e_vbeln.
    IF sy-subrc NE 0.
      MESSAGE |Movimentação porto não encontrada para a Remessa { e_vbeln }! Table ZSDT0023!| TYPE 'I'.
      RETURN.
    ENDIF.

    zcl_depara_centro_fixo_virtual=>get_dados_depara(
        EXPORTING
          i_werks        = lwa_vbap-vstel
          i_lifnr        = lwa_vbpa_z1-lifnr
          i_operacao     = 'RF'
          i_eudr         = 'N'
        IMPORTING
         e_single_depara = wa_depara ).

    IF wa_depara IS INITIAL .
      MESSAGE |Parametro ZSDT0020 Centro { lwa_vbap-vstel } Terminal { lwa_vbpa_z1-lifnr } EUDR = N, não foi encontrado!| TYPE 'I'.
      RETURN.
    ENDIF.


*---------------------------------------------------------------------------------------------*
*   Montagem Cabeçalho BAPI
*---------------------------------------------------------------------------------------------*
    CLEAR: sl_header.

    sl_header-pstng_date = wa_dt_mistura.
    sl_header-doc_date   = wa_dt_mistura.

    lwa_code-gm_code = '06'.

*---------------------------------------------------------------------------------------------*
*   Montagem Item BAPI
*---------------- -----------------------------------------------------------------------------*

    CLEAR: sl_item, tl_item[].

    sl_item-material   = lwa_lin-matnr.
    sl_item-move_type  = 'ZA5'.
    sl_item-entry_qnt  = lwa_lin-menge.
    sl_item-batch      = lwa_lin-charg.
    sl_item-entry_uom  = lwa_lin-meins.

    "Dados Origem
    sl_item-plant      = wa_zsdt0023-werks_v.
    sl_item-stge_loc   = wa_zsdt0023-lgort_v.

    "Dados Destino
    sl_item-move_stloc = wa_depara-lgort.
    IF lwa_vbak-kvgr3 = 'C' AND wa_depara-lgort_t IS NOT INITIAL.
      sl_item-move_stloc  = wa_depara-lgort_t.
    ELSEIF lwa_vbak-kvgr3 = 'F' AND wa_depara-lgort_f IS NOT INITIAL.
      sl_item-move_stloc = wa_depara-lgort_f.
    ELSE.
      sl_item-move_stloc = wa_depara-lgort.
    ENDIF.

    APPEND sl_item TO tl_item.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = sl_header
        goodsmvt_code    = lwa_code
      IMPORTING
        materialdocument = vl_material
        matdocumentyear  = vl_year
      TABLES
        goodsmvt_item    = tl_item
        return           = t_return.

    IF  vl_material IS NOT INITIAL.

      <saida>-doc_mat = vl_material.

      UPDATE zlest0249
        SET  mblnr = vl_material
             mjahr = vl_year
       WHERE id_mistura EQ <saida>-id_mistura
         AND id_item    EQ <saida>-id_item.

      UPDATE zsdt0001 SET eudr            = 'N'
                          id_mistura_eudr = <saida>-id_mistura
       WHERE chave_nfe    EQ <fs_zlest0249>-chave_nfe
         AND tp_movimento EQ 'E'.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ELSE.
      <saida>-erro = 'Não foi possivel gerar o movimento de estoque'.

      LOOP AT t_return INTO s_return WHERE type = 'E'.
        <saida>-erro = s_return-message.
        EXIT.
      ENDLOOP.

    ENDIF.


  ENDLOOP.


ENDFORM.

FORM f_visualizar.

  DATA: t_rows               TYPE lvc_t_row.
  TYPES text   TYPE c LENGTH 85.
  DATA  itab   TYPE TABLE OF text.

  DATA: l_text_table LIKE tline OCCURS 3,
        l_tdline     LIKE LINE OF l_text_table.

  CLEAR: v_salvo, it_editor[].
  p_visu = 'X'.

  FREE it_saida_02.

  CALL METHOD g_grid_10->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE 'Selecione uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF lines( t_rows ) NE 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE t_rows INTO DATA(w_rows) INDEX 1.


  READ TABLE it_saida INTO DATA(w_saida) INDEX w_rows-index.

  wa_dt_mistura = w_saida-dt_mistura.
  wa_id_mistura = w_saida-id_mistura.

  wa_editor-line = w_saida-observacao.
  APPEND wa_editor TO  it_editor.
  "TXTOPEN =
  PERFORM f_caixa_txt_obs.

  LOOP AT it_saida INTO DATA(wa_saida) WHERE id_mistura EQ w_saida-id_mistura.

    CLEAR wa_saida_02.

    wa_saida_02-id_mistura      = w_saida-id_mistura.
    wa_saida_02-id_item         = w_saida-id_item.
    wa_saida_02-chave_nfe       = w_saida-chave_nfe.
    wa_saida_02-docnum          = w_saida-docnum.
    wa_saida_02-dt_emissao      = w_saida-dt_mistura.
    wa_saida_02-filial          = w_saida-filial.
    wa_saida_02-chave_romaneio  = w_saida-chave_romaneio.
    wa_saida_02-doc_mat         = w_saida-doc_mat.

    SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_1bnfdoc) WHERE docnum = @w_saida-docnum.
    IF sy-subrc IS INITIAL.
      wa_saida_02-numero =  wa_1bnfdoc-nfenum.
      wa_saida_02-dt_emissao = wa_1bnfdoc-docdat.
    ENDIF.

    SELECT SINGLE * FROM j_1bnflin INTO @DATA(wa_1bnflin) WHERE docnum EQ @w_saida-docnum.

    IF sy-subrc IS INITIAL.
      wa_saida_02-quantidade = wa_1bnflin-menge.
      wa_saida_02-unidade    = wa_1bnflin-meins.
      wa_saida_02-matnr      = wa_1bnflin-matnr.
    ENDIF.

    APPEND wa_saida_02  TO it_saida_02.

  ENDLOOP.

  CALL SCREEN 0100.

ENDFORM.


FORM f_deletar .

  DATA: var_answer TYPE c.
  DATA: t_rows               TYPE lvc_t_row.
  TYPES text   TYPE c LENGTH 85.
  DATA  itab   TYPE TABLE OF text.

  DATA: l_text_table LIKE tline OCCURS 3,
        l_tdline     LIKE LINE OF l_text_table.
  FREE it_saida_02.

  CALL METHOD g_grid_10->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE 'Selecione uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF lines( t_rows ) NE 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente realizar a exclusão do registro?'
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

  READ TABLE t_rows INTO DATA(w_rows) INDEX 1.

  READ TABLE it_saida INTO DATA(w_saida) INDEX w_rows-index.
  CHECK sy-subrc EQ 0 AND w_saida-id_mistura IS NOT INITIAL.

  SELECT SINGLE *
    FROM zlest0249 AS a INTO @DATA(lwa_zlest0249)
   WHERE id_mistura EQ @w_saida-id_mistura
     AND mblnr      NE @space
     AND NOT EXISTS ( SELECT mblnr
                        FROM mseg AS b
                       WHERE b~smbln = a~mblnr  ).

  IF sy-subrc EQ 0.
    MESSAGE 'Registro já possui movimentações! Operação não permitida!' TYPE 'I'.
    RETURN.
  ENDIF.

  DELETE FROM zlest0248 WHERE id_mistura = w_saida-id_mistura.
  DELETE FROM zlest0249 WHERE id_mistura = w_saida-id_mistura.

  COMMIT WORK.

  DELETE it_saida WHERE id_mistura = w_saida-id_mistura.

  CALL METHOD g_grid_10->refresh_table_display
    EXPORTING
      is_stable = w_stable.


ENDFORM.


FORM editor_text .
  CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
  LOOP AT it_editor ASSIGNING FIELD-SYMBOL(<w_text>).
    w_text = |{ w_text } { <w_text>-line }|.
  ENDLOOP.
ENDFORM.
