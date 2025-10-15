REPORT zfir0077_filter_amaggi.

TABLES: zfit0091, sscrfields.

DATA: ex_vl_augdt        TYPE zfit0091-dt_pgto,
      ex_vl_data(10)     TYPE c,
      ex_vl_trin         TYPE zfit0091-dt_pgto,
      ex_vl_assunto(150) TYPE c,
      ex_vl_ano          TYPE gjahr.

TYPES: BEGIN OF tY_list_send,
         lifnr     TYPE lfa1-lifnr,
         name1     TYPE lfa1-name1,
         adrnr     TYPE lfa1-adrnr,
         smtp_addr TYPE adr6-smtp_addr,
       END OF ty_list_send.

DATA: it_list_send TYPE STANDARD TABLE OF ty_list_send INITIAL SIZE 0.
DATA: wa_list_send TYPE ty_list_send.
DATA: ex_lista TYPE string.
DATA: ex_cdfor TYPE lifnr.
DATA: ex_executa TYPE btcjob.
DATA: ex_cdcomp TYPE zfit0091-augbl.
DATA: icon_proc TYPE string.
DATA: p_executa(1) TYPE c.

DATA tg_envia_salv TYPE STANDARD TABLE OF zfit0077_monitor WITH HEADER LINE.

DATA container_main TYPE REF TO cl_gui_custom_container.
DATA painel_control  TYPE REF TO cl_gui_splitter_container.
DATA painel_1        TYPE REF TO cl_gui_container.
DATA painel_2        TYPE REF TO cl_gui_container.
DATA container       TYPE REF TO cl_gui_container.
DATA gr_table TYPE REF TO cl_salv_table.
DATA: lr_functions TYPE REF TO cl_salv_functions_list.
DATA lr_columns           TYPE REF TO cl_salv_columns.
DATA lr_columns_TB        TYPE REF TO cl_salv_columns_table.
DATA lr_column            TYPE REF TO cl_salv_column.
DATA lr_column_TB         TYPE REF TO cl_salv_column_table.
DATA lr_events TYPE REF TO cl_salv_events_table.

CLASS lcl_handle_events DEFINITION DEFERRED.
DATA: gr_events TYPE REF TO lcl_handle_events.
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function,
      on_before_user_command FOR EVENT before_salv_function OF cl_salv_events IMPORTING e_salv_function,
      on_after_user_command FOR EVENT after_salv_function OF cl_salv_events IMPORTING e_salv_function,
      "on_hotspot_click FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING e_row_id e_column_id es_row_no,
      on_hotspot_click FOR EVENT link_click OF cl_salv_events_table IMPORTING column row sender,
      make_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_interactive e_object,
*      on_handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING
*          er_data_changed
*          e_onf4
*          e_onf4_before
*          e_onf4_after
*          e_ucomm
*          sender,
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

ENDCLASS.
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i08.
  ENDMETHOD.
  METHOD on_before_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i09.
  ENDMETHOD.
  METHOD on_after_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i10.
  ENDMETHOD.                 " on_link_click2

  METHOD on_hotspot_click.
    PERFORM hotspot USING row column.
  ENDMETHOD.
  METHOD make_toolbar.
    DATA mt_toolbar TYPE stb_button.
    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   " separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
      " 3 DESABILITA E 0 HABILITA
      IF <fs_tollbar>-function = '&LOCAL&COPY_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&CREATE_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&APPEND'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&INSERT_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&LOCAL&DELETE_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function = '&REFRESH'.
        <fs_tollbar>-butn_type = '3'.
      ENDIF.
*      IF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
*        <fs_tollbar>-function = 'INSERT_ROW'.
*      ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
*        <fs_tollbar>-function = 'DELETE_ROW'.
*      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished.

    CHECK e_modified = 'X'.

  ENDMETHOD.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE tit_a .

  SELECT-OPTIONS: p_cdfor FOR zfit0091-lifnr NO-EXTENSION NO INTERVALS. "OBLIGATORY
  SELECT-OPTIONS: p_dtpgto FOR zfit0091-dt_pgto NO-EXTENSION NO INTERVALS."OBLIGATORY
  SELECT-OPTIONS: p_cdcomp FOR zfit0091-augbl NO INTERVALS.
  SELECT-OPTIONS: p_email FOR (string) NO INTERVALS VISIBLE LENGTH 150.

SELECTION-SCREEN END OF BLOCK a.

*selection-screen begin of block b with frame title tit_b .
*
*  selection-screen skip 1.
*  selection-screen: begin of line.
*    selection-screen pushbutton 5(10) but1 user-command pb01.
*  selection-screen: end of line.
*  selection-screen skip 1.
*
*selection-screen end of block b.
SELECTION-SCREEN FUNCTION KEY 1.


INITIALIZATION.

  p_executa = abap_false.
  " but1 = 'Monitor'.
  tit_a = 'Filtros'.
  "tit_b = 'Cockpit'.

  icon_proc = icon_query && 'Monitor'.
  sscrfields-functxt_01 = icon_proc .

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      PERFORM make_alv.
      p_executa = abap_false.
    WHEN 'ONLI'.
      IF p_dtpgto IS INITIAL.
        MESSAGE 'O campo data é obrigatório!' TYPE 'I' DISPLAY LIKE 'I'.
        EXIT.
      ELSE.
        p_executa = abap_true.
      ENDIF.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.


START-OF-SELECTION.

  IF p_executa = abap_true.
    FREE:it_list_send.
    CLEAR: ex_vl_augdt, ex_vl_data, ex_vl_trin, ex_vl_assunto, ex_vl_ano.
    ex_vl_augdt   = p_dtpgto-low.
    ex_vl_data    = |{ ex_vl_augdt+6(2) }.{ ex_vl_augdt+4(2) }.{ ex_vl_augdt+0(4) }|.
    ex_vl_ano     = |{ ex_vl_augdt+0(4) }|.
    ex_executa = |INF_PGTO_FORNEC_MANUAL_MAGGI|.
    ex_cdfor = p_cdfor-low.
    ex_cdcomp = p_cdcomp-low.
    IF p_dtpgto-high IS NOT INITIAL.
      ex_vl_trin    = p_dtpgto-high.
    ELSE.
      ex_vl_trin    = ex_vl_augdt   + 30.
    ENDIF.

    DESCRIBE TABLE p_email[] LINES DATA(lv_tab_lines).
    LOOP AT p_email[] ASSIGNING FIELD-SYMBOL(<add_receivers>).
      CONDENSE <add_receivers>-low NO-GAPS.
      IF sy-tabix = lv_tab_lines.
        ex_lista = |{ ex_lista }{ <add_receivers>-low }|.
      ELSE.
        CONCATENATE <add_receivers>-low ex_lista INTO ex_lista SEPARATED BY ';'.
      ENDIF.
    ENDLOOP.

    EXPORT ex_executa TO MEMORY ID 'ZFIR0077_0'.
    EXPORT ex_lista TO MEMORY ID 'ZFIR0077_1'.
    EXPORT ex_vl_augdt TO MEMORY ID 'ZFIR0077_2'.
    EXPORT ex_vl_data TO MEMORY ID 'ZFIR0077_3'.
    EXPORT ex_vl_ano TO MEMORY ID 'ZFIR0077_4'.
    EXPORT ex_vl_trin TO MEMORY ID 'ZFIR0077_5'.
    EXPORT ex_cdfor TO MEMORY ID 'ZFIR0077_6'.
    EXPORT ex_cdcomp TO MEMORY ID 'ZFIR0077_7'.


    SUBMIT zfir0077_maggi AND RETURN.
  ENDIF.



FORM make_alv.

  SELECT * FROM zfit0077_monitor INTO TABLE tg_envia_salv[].

  TRY.
      cl_salv_table=>factory(
*        EXPORTING
*          r_container    = painel_1
*          container_name = 'CONTAINER'
        IMPORTING
          r_salv_table   = gr_table
        CHANGING
          t_table        = tg_envia_salv[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( abap_true ).


  lr_columns = gr_table->get_columns( ).


  lr_columns_tb = gr_table->get_columns( ).



  "... §3.2 include own functions
*  DATA l_text_send TYPE string.
*  DATA l_icon_send TYPE string.
*  l_text_send = 'Enviar'.
*  l_icon_send =  icon_envelope_closed.
*  TRY.
*      lr_functions->add_function( name     = 'ENVIAR'
*                                  icon     = l_icon_send
*                                  text     = l_text_send
*                                  tooltip  = 'Enviar'
*                                  position = if_salv_c_function_position=>right_of_salv_functions ).
*    CATCH cx_root.
*  ENDTRY.

    TRY.
      "***MOSTRA
      lr_column = lr_columns->get_column( 'EMPRESA' ).
      lr_column->set_short_text( 'Empresa' ).
      lr_column->set_medium_text( 'Empresa' ).
      lr_column->set_long_text( 'Empresa' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'LIFNR' ).
      lr_column->set_short_text( 'Fornecedor' ).
      lr_column->set_medium_text( 'Fornecedor' ).
      lr_column->set_long_text( 'Fornecedor' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'DOC_COMPENSA' ).
      lr_column->set_short_text( 'Doc.' ).
      lr_column->set_medium_text( 'Doc.Comp.' ).
      lr_column->set_long_text( 'Doc.Comp.' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'TIPO' ).
      lr_column->set_short_text( 'Tipo' ).
      lr_column->set_medium_text( 'Tipo' ).
      lr_column->set_long_text( 'Tipo' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '7' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'RECEIVER_QTD' ).
      lr_column->set_short_text( 'QtdDest.' ).
      lr_column->set_medium_text( 'QtdDest.' ).
      lr_column->set_long_text( 'QtdDest.' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'ASSUNTO' ).
      lr_column->set_short_text( 'Assunto' ).
      lr_column->set_medium_text( 'Assunto' ).
      lr_column->set_long_text( 'Assunto' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '51' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'USUARIO' ).
      lr_column->set_short_text( 'Usuario' ).
      lr_column->set_medium_text( 'Usuario' ).
      lr_column->set_long_text( 'Usuario' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'DT_SEND' ).
      lr_column->set_short_text( 'Dt Envio' ).
      lr_column->set_medium_text( 'Dt Envio' ).
      lr_column->set_long_text( 'Dt Envio' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '10' ).
      lr_column->set_edit_mask( value = '__/__/____' ).
      lr_column = lr_columns->get_column( 'HR_SEND' ).
      lr_column->set_short_text( 'Hr Envio' ).
      lr_column->set_medium_text( 'Hr Envio' ).
      lr_column->set_long_text( 'Hr Envio' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '10' ).
      lr_column->set_edit_mask( value = '__:__:__' ).
      lr_column = lr_columns->get_column( 'MOEDA' ).
      lr_column->set_short_text( 'Moeda' ).
      lr_column->set_medium_text( 'Moeda' ).
      lr_column->set_long_text( 'Moeda' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      "lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'EXECUCAO' ).
      lr_column->set_short_text( 'Execucao' ).
      lr_column->set_medium_text( 'Execucao' ).
      lr_column->set_long_text( 'Execucao' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_alignment( if_salv_c_alignment=>left ).
      lr_column->set_output_length( '26' ).
      " lr_column->set_edit_mask( value = '__________' ).
      lr_column = lr_columns->get_column( 'DT_COMP' ).
      lr_column->set_short_text( 'Dt Comp.' ).
      lr_column->set_medium_text( 'Dt Compensação' ).
      lr_column->set_long_text( 'Dt Compensação' ).
      lr_column->set_optimized( abap_true ).

      "***ESCONDE
      lr_column = lr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'BUKRS' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'FORNECEDOR' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'PDF' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'HTML' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'RECEIVER_EMAIL' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'COMPENSACAO' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found.
  ENDTRY.

  lr_columns->set_column_position( columnname = 'EMPRESA'         position   = 01 ).
  lr_columns->set_column_position( columnname = 'LIFNR'           position   = 02 ).
  lr_columns->set_column_position( columnname = 'NMFOR'           position   = 03 ).
  lr_columns->set_column_position( columnname = 'DOC_COMPENSA'    position   = 04 ).
  lr_columns->set_column_position( columnname = 'DT_COMP'         position   = 05 ).
  lr_columns->set_column_position( columnname = 'ASSUNTO'         position   = 06 ).
  lr_columns->set_column_position( columnname = 'TIPO'            position   = 07 ).
  lr_columns->set_column_position( columnname = 'RECEIVER_QTD'    position   = 08 ).
  lr_columns->set_column_position( columnname = 'EXECUCAO'        position   = 09 ).
  lr_columns->set_column_position( columnname = 'COMPENSACAO'     position   = 10 ).
  lr_columns->set_column_position( columnname = 'MOEDA'           position   = 11 ).
  lr_columns->set_column_position( columnname = 'USUARIO'         position   = 12 ).
  lr_columns->set_column_position( columnname = 'DT_SEND'         position   = 13 ).
  lr_columns->set_column_position( columnname = 'HR_SEND'         position   = 14 ).

  TRY.
      lr_column_tb ?= lr_columns_tb->get_column( 'RECEIVER_QTD' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).

      lr_column_tb ?= lr_columns_tb->get_column( 'ASSUNTO' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).

      lr_column_tb ?= lr_columns_tb->get_column( 'DOC_COMPENSA' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found  .                          "#EC NO_HANDLER
  ENDTRY.

  lr_events = gr_table->get_event( ).

  CREATE OBJECT gr_events.

  SET HANDLER gr_events->on_user_command  FOR lr_events.
  SET HANDLER gr_events->on_before_user_command FOR lr_events.
  SET HANDLER gr_events->on_after_user_command FOR lr_events.
  SET HANDLER gr_events->on_hotspot_click FOR lr_events.
  SET HANDLER gr_events->make_toolbar FOR ALL INSTANCES.
  SET HANDLER gr_events->on_data_changed_finished FOR ALL INSTANCES ACTIVATION 'X'.

  gr_table->display( ).

ENDFORM.

FORM hotspot USING p_row p_column.

  READ TABLE tg_envia_salv ASSIGNING FIELD-SYMBOL(<_get>) INDEX p_row.

  CHECK <_get> IS NOT INITIAL.

  CASE p_column.
    WHEN 'ASSUNTO'.
      cl_abap_browser=>show_html( html_string = <_get>-html title = 'Html' ).
    WHEN 'DOC_COMPENSA'.

      IF <_get>-pdf IS NOT INITIAL.
      IF <_get>-pdf IS NOT INITIAL.

        DATA: lo_html_viewer TYPE REF TO cl_gui_html_viewer,
              lt_pdf_content TYPE TABLE OF char255,
              lv_html        TYPE string,
              lv_base64_pdf  TYPE string.


        CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
          EXPORTING
            input  = <_get>-pdf
          IMPORTING
            output = lv_base64_pdf.

        lv_html = '<html><body><embed src="data:application/pdf;base64,' && lv_base64_pdf && '" width="100%" height="100%"></embed></body></html>'.

        cl_abap_browser=>show_html( html_string = lv_html title = 'PDF' ).
      ELSE.
        MESSAGE 'Comprovante não Encontrado!' TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.
      ELSE.
        MESSAGE 'Comprovante não Encontrado!' TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.

    WHEN 'RECEIVER_QTD'.

      TYPES: BEGIN OF ty_email,
               email TYPE adr6-smtp_addr,
             END OF ty_email.
      DATA: lt_destinatarios  TYPE TABLE OF ty_email INITIAL SIZE 0.
      FREE: lt_destinatarios.
      SPLIT <_get>-receiver_email AT ';' INTO TABLE lt_destinatarios.


      DATA go_alv TYPE REF TO cl_salv_table.

      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = go_alv
            CHANGING
              t_table      = lt_destinatarios[] ).
        CATCH cx_salv_msg.

      ENDTRY.


      TRY.
          "***MOSTRA
          DATA(go_column) = go_alv->get_columns( )->get_column( 'EMAIL' ).
          go_column->set_short_text( 'Email' ).
          go_column->set_medium_text( 'Email' ).
          go_column->set_long_text( 'Email' ).
          go_column->set_optimized( abap_false ).
        CATCH cx_salv_not_found.

      ENDTRY.



      DATA go_title              TYPE lvc_title.
      go_title = |Destinatários|.
      DATA(go_display_settings) = go_alv->get_display_settings( ).
      go_display_settings->set_list_header_size( '10' ). " 0=l, 1=s, 2=m
      go_display_settings->set_list_header( go_title ).

      go_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
      go_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
      go_display_settings->set_list_header( go_title ).

      DATA(ltot) = lines( lt_destinatarios[] ).
      DATA total TYPE i.

      total = ltot + 10.

      go_alv->set_screen_popup(
        start_column = 1
        end_column   = 100
        start_line   = 1
        end_line     = total ).

      go_alv->display( ).
      "WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM show_function_info USING i_function TYPE sy-ucomm  i_text TYPE string.

  IF i_function IS INITIAL.
    RETURN.
  ENDIF.
  " Botões da Grid
  CASE i_function.
    WHEN 'ATUALIZAR_ALV'.
      gr_table->refresh( ).
  ENDCASE.

ENDFORM.

FORM action_process.
  " Botões do Programa Cabeçalho
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 1000.
      SET SCREEN 0.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDFORM.

FORM docker.
  CREATE OBJECT container_main
    EXPORTING
      container_name = 'CONTAINER'
      lifetime       = container_main->lifetime_dynpro.

* Cria Splitter Container
  CREATE OBJECT painel_control
    EXPORTING
      parent  = container_main
      rows    = 1
      columns = 1
      align   = 15.

* Exibe Painel 1
  CALL METHOD painel_control->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = painel_1.

ENDFORM.

MODULE status_0100 OUTPUT.
  "PERFORM docker.
  SET PF-STATUS 'STATUS_0100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  PERFORM action_process.
ENDMODULE.
