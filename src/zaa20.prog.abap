*&---------------------------------------------------------------------*
*& Report ZAA20
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZAA20.


TABLES: SKB1.

*&---------------------------------------------------------------------*
*&  TABELAS INTERNAS E WORKAREAS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_dados,
         bukrs TYPE skb1-bukrs,
         saknr TYPE skb1-saknr,
         mitkz TYPE skb1-mitkz,
         butxt TYPE t001-butxt,
         txt50 TYPE skat-txt50,
       END OF ty_dados.

DATA: it_saida TYPE TABLE OF ty_dados.


*&---------------------------------------------------------------------*
*&  ALV
*&---------------------------------------------------------------------*
types: begin of ty_estrutura.
         include TYPE slis_fieldcat_main.
         include TYPE slis_fieldcat_alv_spec.
types: end of ty_estrutura.

DATA: dg_splitter_1        TYPE ref TO cl_gui_splitter_container,
      g_grid               TYPE ref TO cl_gui_alv_grid,
      g_custom_container   TYPE ref TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE ref TO cl_alv_grid_toolbar_manager,
      container_1          TYPE ref TO cl_gui_container,
      cl_container_95      TYPE ref TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE ref TO cl_dd_document,
      tl_function          TYPE ui_functions,
      wl_function          TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE table of lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm,
*
      zcl_util             TYPE ref TO zcl_util.

DATA: variante         LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.

DATA: it_return TYPE table of ddshretval,
      it_T028G  TYPE table of t028g.


*&---------------------------------------------------------------------*
*&  CLASSES
*&---------------------------------------------------------------------*
class lcl_event_handler definition.
  public section.


    class-methods:
      on_double_click FOR event double_click of cl_gui_alv_grid
        importing e_row e_column.

*    class-methods:
*      on_hotspot_click FOR event hotspot_click of cl_gui_alv_grid
*        importing e_row_id e_column_id.

*    CLASS-METHODS:
*      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
*        IMPORTING e_object.

*    CLASS-METHODS:
*      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
*        IMPORTING e_ucomm.

endclass.                    "LCL_EVENT_HANDLER DEFINITION


class lcl_event_handler implementation.
  method on_double_click.

  endmethod.
endclass.


*&---------------------------------------------------------------------*
*&  SELECT-OPTIONS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS: s_bukrs FOR skb1-bukrs NO INTERVALS,
                  s_saknr FOR skb1-saknr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.


*&---------------------------------------------------------------------*
*&  INICIO
*&---------------------------------------------------------------------*
initialization.


*&---------------------------------------------------------------------*
*&  START
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_selecao_dados.

*  PERFORM f_manipula_dados.

  PERFORM f_exibir_dados.


*&---------------------------------------------------------------------*
*&  SELEÇÃO DE DADOS
*&---------------------------------------------------------------------*
FORM f_selecao_dados.
  SELECT a~bukrs, a~saknr, a~mitkz, b~butxt, c~txt50
    INTO TABLE @it_saida
    FROM skb1 AS a
    INNER JOIN t001 AS b
    ON b~bukrs = a~bukrs
    INNER JOIN skat AS c
    ON c~saknr = a~saknr
    AND c~ktopl = '0050'
    AND c~spras = 'PT'
    WHERE a~bukrs IN @s_bukrs
    AND a~saknr IN @s_saknr.
ENDFORM.


*&---------------------------------------------------------------------*
*&  MANIPULAÇÃO DE DADOS
*&---------------------------------------------------------------------*
FORM f_manipula_dados.
  DATA: resposta TYPE c.

  CLEAR resposta.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmar'
      TEXT_QUESTION         = 'Deseja confirmar a alteração?'
      TEXT_BUTTON_1         = 'Sim'
      TEXT_BUTTON_2         = 'Não'
      DEFAULT_BUTTON        = '2'
      DISPLAY_CANCEL_BUTTON = ''
    IMPORTING
      ANSWER                = RESPOSTA
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.

  IF RESPOSTA = '1'.

    DATA: lt_tab TYPE esp1_message_tab_type.
    DATA: ls_tab TYPE esp1_message_wa_type.
    CLEAR: lt_tab,ls_tab.

    ls_tab-msgid  = 'E4'.
    ls_tab-msgno  = '000'.

    IF it_saida IS NOT INITIAL.

      LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_ajuste>).

        SELECT SINGLE bukrs, saknr, mitkz FROM skb1 WHERE bukrs = @<fs_ajuste>-bukrs AND saknr = @<fs_ajuste>-saknr INTO @DATA(lr_skb1).

        IF lr_skb1-mitkz NE <fs_ajuste>-mitkz.

          UPDATE skb1 SET mitkz = @<fs_ajuste>-mitkz WHERE bukrs = @<fs_ajuste>-bukrs AND saknr = @<fs_ajuste>-saknr.

          COMMIT WORK.

          IF sy-subrc = 0.
            ls_tab-msgty  = 'S'.
            ls_tab-msgv1  = |Conta: { <fs_ajuste>-saknr } - Empresa: { <fs_ajuste>-bukrs } - Atualizado!|.
            ls_tab-lineno = 3.
            APPEND ls_tab TO lt_tab.
          ELSE.
            ls_tab-msgty  = 'E'.
            ls_tab-msgv1  = |Conta: { <fs_ajuste>-saknr } - Empresa: { <fs_ajuste>-bukrs } - Falhou!|.
            ls_tab-lineno = 1.
            APPEND ls_tab TO lt_tab.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
            TABLES
              i_message_tab = lt_tab.
  ELSE.

    EXIT.

  ENDIF.

  CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).

ENDFORM.


*&---------------------------------------------------------------------*
*&  EXIBIR DADOS
*&---------------------------------------------------------------------*
FORM f_exibir_dados.
  CALL SCREEN 0100.
ENDFORM.


*&---------------------------------------------------------------------*
*&  IMPRIMIR DADOS
*&---------------------------------------------------------------------*
FORM f_init_alv .

  DATA: wl_layout TYPE slis_layout_alv.
  DATA:
    p_text      TYPE sdydo_text_element,
    filtros	    TYPE zif_screen_linha_filtro,
    i_filtros	  TYPE zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) TYPE c,
    v_uzeit(10) TYPE c.


  PERFORM f_fieldcatalog.

  variante = VALUE #( report = sy-repid ).



  IF g_grid is initial.

    CLEAR: i_filtros.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO v_datum.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit.
    DESCRIBE TABLE it_saida LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'DATA:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros.

    p_text = 'Abertura de contas de Imobilizado'.
  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
        i_titulo  = CONV #( p_text )
        i_filtros = i_filtros
      CHANGING
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.


    w_layout-sel_mode = 'A'.
    w_layout-col_opt  = abap_true.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.

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

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = t_fieldcat
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


    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ELSE.
    CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).
  ENDIF.

  wl_layout-colwidth_optimize = 'X'.


ENDFORM.


*&---------------------------------------------------------------------*
*& FORM montar_layout
*&---------------------------------------------------------------------*
FORM f_fieldcatalog .

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
 01  ''   ''   'IT_SAIDA'   'BUKRS'  'Empresa                 '  '08'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'   'BUTXT'  'Descrição Empresa       '  '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'   'SAKNR'  'Conta                   '  '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'   'TXT50'  'Descrição Conta         '  '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'   'MITKZ'  'Marcado para Imolibizado'  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.


*&---------------------------------------------------------------------*
*& FORM  f_estrutura_alv
*&---------------------------------------------------------------------*
FORM f_estrutura_alv  USING VALUE(p_col_pos)       TYPE i                    "1
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
  w_fieldcat-colddictxt  = 'L'.
  w_fieldcat-selddictxt  = 'L'.
  w_fieldcat-tipddictxt  = 'L'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  IF p_field = 'MITKZ'.
    w_fieldcat-edit = abap_true.
  ENDIF.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.

  PERFORM f_init_alv.
ENDMODULE.


*&---------------------------------------------------------------------*
*&  Module  USER_COMMAND_100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      PERFORM f_manipula_dados.
  ENDCASE.
ENDMODULE.
