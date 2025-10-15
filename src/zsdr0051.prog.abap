*&---------------------------------------------------------------------*
*& Report ZSDR0051
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0051.
**********************************************************************

TABLES:zsdt0042.

DATA: gr_table       TYPE REF TO cl_salv_table,
      it_saida       TYPE STANDARD TABLE OF zsdt0042 WITH HEADER LINE,
      wa_mod         TYPE zsdt0042,
      ls_api         TYPE REF TO if_salv_gui_om_extend_grid_api,
      okcode         TYPE sy-ucomm,
      container_main TYPE REF TO cl_gui_custom_container,
      painel         TYPE REF TO cl_gui_container,
      painel_control TYPE REF TO cl_gui_splitter_container,
      painel_main    TYPE REF TO cl_gui_container,
      ls_edit        TYPE REF TO if_salv_gui_om_edit_restricted,
      lr_functions   TYPE REF TO cl_salv_functions,
      gr_salv_func   TYPE REF TO cl_salv_functions,
      lo_alv_toolbar TYPE REF TO cl_salv_functions_list,
      l_text_ref     TYPE string,
      l_icon_ref     TYPE string,
      l_text_del     TYPE string,
      l_icon_del     TYPE string,
      l_text_edit    TYPE string,
      l_icon_edit    TYPE string,
      l_text_nov     TYPE string,
      l_icon_nov     TYPE string.

DATA: msg_text TYPE char45.

DATA: lo_selections TYPE REF TO cl_salv_selections.
DATA lt_rows TYPE salv_t_row.
DATA ls_row TYPE int4.

DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.
DATA: ivals  TYPE STANDARD TABLE OF sval WITH HEADER LINE.
DATA: xvals  TYPE sval.
DATA: WA_zsdt0042 TYPE zsdt0042.
DATA w_retn TYPE string.
DATA: var_click TYPE sy-ucomm.


**********************************************************************
"CLASSE
CLASS lcl_handle_events DEFINITION DEFERRED.
DATA gr_events TYPE REF TO lcl_handle_events.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_before_user_command FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_after_user_command FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING e_salv_function.
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
  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t_block1.
  SELECT-OPTIONS: s_safra FOR zsdt0042-safra,
                  s_imp FOR zsdt0042-witht,
                  s_cult FOR zsdt0042-cultura,
                  s_moe FOR zsdt0042-waerk,
                  s_reg FOR zsdt0042-estado.

SELECTION-SCREEN END   OF BLOCK b1.

INITIALIZATION.
  t_block1 = 'Filtro'.

FORM show_function_info USING i_function TYPE salv_de_function
                              i_text     TYPE string.
  DATA: l_string TYPE string.
  "concatenate i_text i_function into l_string separated by space.
  "message i000(0k) with l_string.

  CASE i_function.
*     Make ALV as Editable ALV
*    WHEN 'EDIT'.
*      ls_api = gr_table->extended_grid_api( ).
*      ls_edit = ls_api->editable_restricted( ).
*
*      TRY.
*
*          ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'VAL_DE'
*all_cells_input_enabled = abap_true ).
*          ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'VAL_ATE'
*all_cells_input_enabled = abap_true ).
*          ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'SAFRA'
*all_cells_input_enabled = abap_true ).
*
*
*
*          ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'ESTADO'
*all_cells_input_enabled = abap_true ).
*
*          ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'WAERK'
*all_cells_input_enabled = abap_true ).
*
*          ls_edit->set_attributes_for_columnname(
*  EXPORTING
*    columnname = 'CULTURA'
*    all_cells_input_enabled = abap_true ).
*
*          ls_edit->set_attributes_for_columnname(
*      EXPORTING
*        columnname = 'WITHT'
*        all_cells_input_enabled = abap_true ).
*
*
*          ls_edit->set_attributes_for_columnname(
*          EXPORTING
*            columnname = 'VLR_PERC'
*            all_cells_input_enabled = abap_true ).
*
*          ls_edit->set_attributes_for_columnname(
*          EXPORTING
*          columnname = 'VLR_ALIQ'
*          all_cells_input_enabled = abap_true ).
*
*          ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'VLR_PERC1'
*all_cells_input_enabled = abap_true ).
*        CATCH cx_salv_not_found.
*      ENDTRY.
*
*      ls_edit->validate_changed_data(
*    ).
*      PERFORM atuliza.

    WHEN 'NOVO'. "'GRAVAR' "PSA
      CLEAR: var_click.
      var_click = 'NOVO'.

      CLEAR:w_retn.
      FREE: ivals[].

      PERFORM popup_cadastro.


    WHEN 'ALTERAR'.
      CLEAR: var_click.
      var_click = 'ALTERAR'.

      CLEAR:w_retn.
      FREE: ivals[].

      lt_rows = gr_table->get_selections( )->get_selected_rows( ).

      IF lt_rows IS INITIAL.
        CLEAR: msg_text.
        msg_text = 'Selecione ao menos uma linha!'.
        MESSAGE msg_text TYPE 'I'.

      ELSE.

        LOOP AT lt_rows INTO ls_row.

          CLEAR: wa_mod.
          READ TABLE it_saida[] INTO wa_mod INDEX ls_row.

          PERFORM popup_cadastro.
          IF w_retn <> 'A'.
            MODIFY zsdt0042 FROM wa_mod.
            COMMIT WORK.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CLEAR:lt_fieldcat,ivals,xvals,WA_zsdt0042,w_retn.
      FREE: ivals[].
      PERFORM atualiza.

    WHEN 'REFRESH'.
      PERFORM atualiza.


    WHEN 'DELETE'.

      "CLEAR: lo_selections,lt_rows,ls_row.

      lt_rows = gr_table->get_selections( )->get_selected_rows( ).

      IF lt_rows IS INITIAL.
        CLEAR: msg_text.
        msg_text = 'Selecione ao menos uma linha!'.
        MESSAGE msg_text TYPE 'I'.

      ELSE.

        LOOP AT lt_rows INTO ls_row.

          READ TABLE it_saida[] INTO DATA(wa_del) INDEX ls_row.

          DELETE FROM zsdt0042 WHERE witht = wa_del-witht
          AND cultura = wa_del-cultura
          AND waerk = wa_del-waerk
          AND estado = wa_del-estado
          AND val_de = wa_del-val_de
          AND val_ate = wa_del-val_ate
          AND safra = wa_del-safra.

          COMMIT WORK.

        ENDLOOP.

      ENDIF.
      PERFORM atualiza.

    WHEN 'REFRESH'.
      PERFORM atualiza.
  ENDCASE.



ENDFORM.

FORM main.
  PERFORM seleciona_dados.

  "... §2 create an ALV table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
      r_container = painel
      container_name = 'PAINEL_MAIN'
        IMPORTING
          r_salv_table   = gr_table
        CHANGING
          t_table        = it_saida[] ).
    CATCH cx_salv_msg.
      "lcl_util=>x_error( ).                                 "#EC NO_HANDLER
  ENDTRY.

  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( abap_true ).

  "lr_functions->enable_function( name = '&DETAIL' boolean = ' ' ).


  "... §3.2 include own functions
  l_text_edit = 'Editar'.
  l_icon_edit = icon_edit_file.
  l_text_edit = 'Alterar'.
  l_icon_edit = icon_edit_file.
  l_text_nov = 'Novo'.
  l_icon_nov = icon_add_row.
  l_text_del = 'Remover'.
  l_icon_del = icon_delete.
  l_text_ref = 'Atualizar'.
  l_icon_ref = icon_refresh.
  TRY.
*      lr_functions->add_function(
*        name     = 'EDIT'
*        icon     = l_icon_edit
*        text     = l_text_edit
*        tooltip  = 'Edit Item'
*        position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
       name     = 'NOVO'
       icon     = l_icon_nov
       text     = l_text_nov
       tooltip  = 'Novo Item'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
       name     = 'DELETE'
       icon     = l_icon_del
       text     = l_text_del
       tooltip  = 'Deleta Dados'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
 name     = 'ALTERAR'
  icon     = l_icon_edit
  text     = l_text_edit
 tooltip  = 'Alterar Dados'
 position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->remove_function(
name     = '&DETAIL' ).

    CATCH cx_root.
  ENDTRY.

  "... set the columns technical
  DATA: lr_columns    TYPE REF TO cl_salv_columns,
        lr_column     TYPE REF TO cl_salv_column_table,
        lr_selections TYPE REF TO cl_salv_selections.

  DATA: lr_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  TRY.
*
*      lr_column ?= lr_columns->get_column( 'VLR_PERC1' ).
*      lr_column->set_short_text( 'Vl.Per.Exc' ).
*      lr_column->set_medium_text( 'Vlr. Perc. Exceção' ).
*      lr_column->set_long_text( 'Vlr. Perc. Exceção' ).
*
*      lr_column = ir_columns->get_column( 'NM_INDIC' ).
*      lr_column->set_short_text( 'NM IND' ).
*      lr_column->set_medium_text( 'NM IND' ).
*      lr_column->set_long_text( 'NM IND' ).
*
*      lr_column = ir_columns->get_column( 'CD_ITEM' ).
*      lr_column->set_short_text( 'COD ITEN' ).
*      lr_column->set_medium_text( 'COD ITEN' ).
*      lr_column->set_long_text( 'COD ITEN' ).
*
*      lr_column = ir_columns->get_column( 'NM_ITEM' ).
*      lr_column->set_short_text( 'NM ITEM' ).
*      lr_column->set_medium_text( 'NM ITEM' ).
*      lr_column->set_long_text( 'NM ITEM' ).

      lr_column ?= lr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( abap_false ).

    CATCH cx_root.
      "write some error handling
  ENDTRY.

  "..Posição Colunas

*  lr_columns->set_column_position(  columnname = 'CD_INDIC' position = 1 ).
*  lr_columns->set_column_position(  columnname = 'NM_INDIC' position = 2 ).
*  lr_columns->set_column_position(  columnname = 'CD_ITEM' position = 3 ).
*  lr_columns->set_column_position(  columnname = 'NM_ITEM' position = 4 ).


  "... F4 DDIC

*  DATA lv_ddic TYPE salv_s_ddic_reference.
*
*  TRY.
*      lr_column ?= lr_columns->get_column( columnname = 'NM_INDIC' ).
*      lv_ddic = VALUE #( table = 'ZFIT0201' field = 'NM_INDIC').
*      lr_column->set_ddic_reference( lv_ddic  ). "EXPORTING value = lv_ddic
*      lr_column->set_f4(  if_salv_c_bool_sap=>true ).
*    CATCH cx_salv_not_found.
*  ENDTRY.


  "... §4 set hotspot column
  " TRY.
*      lr_column ?= lr_columns->get_column( 'MATNR' ).
*      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  "ENDTRY.

  "... §6 register to the events of cl_salv_table
  DATA lr_events TYPE REF TO cl_salv_events_table.
  lr_events = gr_table->get_event( ).

  CREATE OBJECT gr_events.

*"... §6.1 register to the event USER_COMMAND

  SET HANDLER gr_events->on_user_command FOR ALL INSTANCES. "lr_events.
  SET HANDLER gr_events->on_before_user_command FOR ALL INSTANCES. "lr_events.
  SET HANDLER gr_events->on_after_user_command FOR ALL INSTANCES. "lr_events.


*"... set list title
  DATA: lr_display_settings TYPE REF TO cl_salv_display_settings,
        l_title             TYPE lvc_title.

  l_title = 'Modificar visão "Simulador de vendas - tabela de formulas.": síntese'.
  lr_display_settings = gr_table->get_display_settings( ).
  lr_display_settings->set_list_header_size( '1' ).
  lr_display_settings->set_list_header( l_title ).


  "...Enable cell selection mode
  lr_selections = gr_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  "... Enable the save layout buttons
  lv_key-report = sy-repid.
  lr_layout = gr_table->get_layout( ).
  lr_layout->set_key( lv_key ).
  lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lr_layout->set_default( abap_true ).


  "... just to triger handler
  gr_table->refresh( ).

  "... §7 display the table
  gr_table->display( ).

ENDFORM.

FORM atualiza.
  REFRESH it_saida.
  PERFORM seleciona_dados.
  gr_table->refresh( ).
  gr_table->display( ).
ENDFORM.

FORM seleciona_dados.
  SELECT DISTINCT * FROM zsdt0042
    WHERE safra IN @s_safra
    AND witht IN @s_imp
    AND cultura IN @s_cult
    AND waerk IN @s_moe
    AND estado IN @s_reg
  INTO TABLE @it_saida[].
ENDFORM.

MODULE status_0100_pai INPUT.
  PERFORM status_0100_pai.
ENDMODULE.

MODULE status_0100_pbo OUTPUT.
  PERFORM status_0100_pbo.
ENDMODULE.

FORM status_0100_pbo .
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.
  PERFORM main.
ENDFORM.

FORM status_0100_pai .
  okcode = sy-ucomm.
  CASE okcode.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDFORM.

START-OF-SELECTION.

  CALL SCREEN 0100.

FORM popup_cadastro .

  CLEAR: xvals, ivals[],ivals.

  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'WITHT'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'CULTURA'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'WAERK'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'ESTADO'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'VAL_DE'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'VAL_ATE'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'SAFRA'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'VLR_PERC'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'VLR_ALIQ'.
  APPEND xvals TO ivals[].
  xvals-tabname   = 'ZSDT0042'.
  xvals-fieldname = 'VLR_PERC1'.
  xvals-fieldtext = 'Vlr. Perc. Exceção'.
  APPEND xvals TO ivals[].

  IF  var_click = 'ALTERAR'.

    LOOP AT ivals[] ASSIGNING FIELD-SYMBOL(<val>) GROUP BY <val>-fieldname .

      IF <val>-fieldname = 'WITHT'.
        <val>-value = wa_mod-witht.
      ELSEIF <val>-fieldname = 'CULTURA'.
        <val>-value = wa_mod-cultura.
      ELSEIF <val>-fieldname = 'WAERK'.
        <val>-value = wa_mod-waerk.
      ELSEIF <val>-fieldname = 'ESTADO'.
        <val>-value = wa_mod-estado.
      ELSEIF <val>-fieldname = 'VAL_DE'.
        <val>-value = wa_mod-val_de.
      ELSEIF <val>-fieldname = 'VAL_ATE'.
        <val>-value = wa_mod-val_ate.
      ELSEIF <val>-fieldname = 'SAFRA'.
        <val>-value = wa_mod-safra.
      ELSEIF <val>-fieldname = 'VLR_PERC'.
        <val>-value = wa_mod-vlr_perc.
      ELSEIF <val>-fieldname = 'VLR_ALIQ'.
        <val>-value = wa_mod-vlr_aliq.
      ELSEIF <val>-fieldname = 'VLR_PERC1'.
        <val>-value = wa_mod-vlr_perc1.
      ENDIF.

    ENDLOOP.


  ELSEIF var_click = 'NOVO'.
    LOOP AT ivals[] ASSIGNING FIELD-SYMBOL(<obrig>) GROUP BY <obrig>-fieldname .

      CASE <obrig>-fieldname.
        WHEN 'WITHT'.
          <obrig>-field_obl = 'X'.
        WHEN 'CULTURA'.
          <obrig>-field_obl = 'X'.
        WHEN 'WAERK'.
          <obrig>-field_obl = 'X'.
        WHEN 'ESTADO'.
          <obrig>-field_obl = 'X'.
        WHEN 'VAL_DE'.
          <obrig>-field_obl = 'X'.
        WHEN 'VAL_ATE'.
          <obrig>-field_obl = 'X'.
        WHEN 'SAFRA'.
          <obrig>-field_obl = 'X'.
        WHEN 'VLR_PERC'.
          <obrig>-field_obl = ' '.
        WHEN 'VLR_ALIQ'.
          <obrig>-field_obl = ' '.
        WHEN 'VLR_PERC1'.
          <obrig>-field_obl = ' '.
      ENDCASE.
    ENDLOOP.
  ENDIF.





  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Cadastro'
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = w_retn
    TABLES
      fields          = ivals[]
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  "dEPOIS DO CLICK

  IF  var_click = 'NOVO' .

    LOOP AT ivals[] ASSIGNING FIELD-SYMBOL(<field>) GROUP BY <field>-fieldname .

      CASE <field>-fieldname.
        WHEN 'WITHT'.
          wa_zsdt0042-witht = <field>-value.
        WHEN 'CULTURA'.
          wa_zsdt0042-cultura = <field>-value.
        WHEN 'WAERK'.
          wa_zsdt0042-waerk = <field>-value.
        WHEN 'ESTADO'.
          wa_zsdt0042-estado = <field>-value.
        WHEN 'VAL_DE'.
          wa_zsdt0042-val_de = <field>-value.
        WHEN 'VAL_ATE'.
          wa_zsdt0042-val_ate = <field>-value.
        WHEN 'SAFRA'.
          wa_zsdt0042-safra = <field>-value.
        WHEN 'VLR_PERC'.

          CONDENSE <field>-value NO-GAPS.
          REPLACE ALL OCCURRENCES OF ',' IN <field>-value WITH '.'.

          wa_zsdt0042-Vlr_perc = <field>-value.
        WHEN 'VLR_ALIQ'.

          CONDENSE <field>-value NO-GAPS.
          REPLACE ALL OCCURRENCES OF ',' IN <field>-value WITH '.'.

          wa_zsdt0042-vlr_aliq = <field>-value.
        WHEN 'VLR_PERC1'.

          CONDENSE <field>-value NO-GAPS.
          REPLACE ALL OCCURRENCES OF ',' IN <field>-value WITH '.'.

          wa_zsdt0042-vlr_perc1 = <field>-value.
      ENDCASE.
    ENDLOOP.

    IF sy-subrc = 0.

      TRY.

          MODIFY zsdt0042 FROM wa_zsdt0042.
          COMMIT WORK.
        CATCH cx_salv_not_found.
      ENDTRY.

      PERFORM atualiza.

    SELECT SINGLE * FROM zsdt0042
  WHERE 1 = 1
  AND witht = @wa_zsdt0042-witht
  AND cultura = @wa_zsdt0042-cultura
  AND waerk = @wa_zsdt0042-waerk
  AND estado = @wa_zsdt0042-estado
  AND val_de = @wa_zsdt0042-val_de
  AND val_ate = @wa_zsdt0042-val_ate
  AND safra = @wa_zsdt0042-safra
      INTO @DATA(AUX_zsdt0042).


    ELSE.

      IF w_retn <> 'A'.
        CLEAR: msg_text.
        msg_text = 'Os dados informados já estão cadastrados!'.
        MESSAGE msg_text TYPE 'I'.

      ELSE.

      ENDIF.

    ENDIF.

    "CLEAR:lt_fieldcat,ivals,xvals,WA_zsdt0042,w_retn.
    FREE: ivals[].

  ELSEIF var_click = 'ALTERAR'.

    LOOP AT ivals[] ASSIGNING FIELD-SYMBOL(<field2>) GROUP BY <field2>-fieldname .

      CASE <field2>-fieldname.
        WHEN 'WITHT'.
          wa_zsdt0042-witht = <field2>-value.
        WHEN 'CULTURA'.
          wa_zsdt0042-cultura = <field2>-value.
        WHEN 'WAERK'.
          wa_zsdt0042-waerk = <field2>-value.
        WHEN 'ESTADO'.
          wa_zsdt0042-estado = <field2>-value.
        WHEN 'VAL_DE'.
          wa_zsdt0042-val_de = <field2>-value.
        WHEN 'VAL_ATE'.
          wa_zsdt0042-val_ate = <field2>-value.
        WHEN 'SAFRA'.
          wa_zsdt0042-safra = <field2>-value.
        WHEN 'VLR_PERC'.

          CONDENSE <field2>-value NO-GAPS.
          REPLACE ALL OCCURRENCES OF ',' IN <field2>-value WITH '.'.

          wa_zsdt0042-Vlr_perc = <field2>-value.
        WHEN 'VLR_ALIQ'.

          CONDENSE <field2>-value NO-GAPS.
          REPLACE ALL OCCURRENCES OF ',' IN <field2>-value WITH '.'.

          wa_zsdt0042-vlr_aliq = <field2>-value.
        WHEN 'VLR_PERC1'.

          CONDENSE <field2>-value NO-GAPS.
          REPLACE ALL OCCURRENCES OF ',' IN <field2>-value WITH '.'.

          wa_zsdt0042-vlr_perc1 = <field2>-value.
      ENDCASE.
    ENDLOOP.


    TRY.

        MODIFY zsdt0042 FROM wa_zsdt0042.
        COMMIT WORK.
      CATCH cx_salv_not_found.
    ENDTRY.

    PERFORM atualiza.

    FREE: ivals[].

  ENDIF.


ENDFORM .
