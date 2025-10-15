*&---------------------------------------------------------------------*
*& Report ZHCMR_SU001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcmr_su001.

TABLES:pa0465,pa0001,usr21.

TYPES:

  BEGIN OF ty_saida,
    email  TYPE string, "mail,
    cpf    TYPE char15, "pa0465-cpf_nr,
    custo  TYPE char10, "kostl,
    emp    TYPE char4, "bukrs,
    fil    TYPE char4, "werKS,
    logsap TYPE string, "bname,
    nome   TYPE string, "cname,
    nmsnc  TYPE string, "SNC Name,
    logad  TYPE string,
    domain TYPE string,
    filter TYPE string,
  END OF ty_saida.


DATA g_okcode TYPE sy-ucomm.
DATA lr_column TYPE REF TO cl_salv_column.
DATA ir_columns           TYPE REF TO cl_salv_columns_table.
DATA lex_not_found        TYPE REF TO cx_salv_not_found.
DATA gr_table   TYPE REF TO cl_salv_table.
DATA lr_display_settings TYPE REF TO cl_salv_display_settings.
DATA l_title             TYPE lvc_title.
DATA ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api.
DATA ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.

DATA: msg_text TYPE char45.

DATA lr_functions TYPE REF TO cl_salv_functions.
DATA l_text_RF   TYPE string.
DATA l_icon_RF   TYPE string.
DATA l_text_UPSNC  TYPE string.
DATA l_icon_UPSNC  TYPE string.
DATA l_text_CLSNC  TYPE string.
DATA l_icon_CLSNC  TYPE string.


DATA container_main TYPE REF TO cl_gui_custom_container.
DATA painel_control  TYPE REF TO cl_gui_splitter_container.
DATA painel_1        TYPE REF TO cl_gui_container.
DATA painel_2        TYPE REF TO cl_gui_container.
DATA container       TYPE REF TO cl_gui_container.

DATA lr_columns    TYPE REF TO cl_salv_columns.
DATA lR_selections TYPE REF TO cl_salv_selections.
DATA lv_key        TYPE salv_s_layout_key.

DATA: lR_layout TYPE REF TO cl_salv_layout.

DATA it_SAIDA TYPE STANDARD TABLE OF ty_saida WITH HEADER LINE.

**********************************************************************
DATA  user_bapi LIKE bapibname-bapibname.
DATA  snc TYPE bapisncu.
DATA  sncx TYPE bapisncux.

DATA  logondatax LIKE bapilogonx.
DATA  defaultsx LIKE bapidefax.
DATA  addressx LIKE usaddressx.

DATA: logondata LIKE uslogond,
      address   LIKE usaddress,
      defaults  LIKE usdefaults.

DATA: return TYPE TABLE OF bapiret2 WITH HEADER LINE.

DATA bapilogond LIKE bapilogond.

DATA: lo_selections TYPE REF TO cl_salv_selections.

DATA lt_rows TYPE salv_t_row.

DATA ls_row TYPE int4.
**********************************************************************

**********************************************************************
* PARAMETROS DE SELECAO
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS: s_MAT FOR pa0465-pernr,
  s_CPF FOR pa0465-cpf_nr,
  s_kostl FOR pa0001-kostl,
  s_bukrs FOR pa0001-bukrs,
  s_werks FOR pa0001-werks,
  s_bname FOR usr21-bname.
SELECTION-SCREEN END   OF BLOCK b1.

CLASS lcl_handle_events DEFINITION DEFERRED.
DATA: gr_events TYPE REF TO lcl_handle_events.

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
*          ls_edit->set_attributes_for_columnname(
*      EXPORTING
*        columnname = 'CD_INDIC'
*        all_cells_input_enabled = abap_true
*      ).
*          ls_edit->set_attributes_for_columnname(
*            EXPORTING
*              columnname = 'NM_INDIC'
*              all_cells_input_enabled = abap_true
*              ).
*          ls_edit->set_attributes_for_columnname(
*          EXPORTING
*            columnname = 'CD_ITEM'
*            all_cells_input_enabled = abap_true
*            ).
*          ls_edit->set_attributes_for_columnname(
*          EXPORTING
*            columnname = 'NM_ITEM'
*            all_cells_input_enabled = abap_true
*            ).
*        CATCH cx_salv_not_found.
*      ENDTRY.
*
*      ls_edit->validate_changed_data(
*    ).
*      gr_table->refresh( ).

*    WHEN 'SAVE'.
*      TRY.
*          ls_edit->validate_changed_data(
*                IMPORTING
*                  is_input_data_valid = DATA(s)
*                ).
*
*          gr_table->refresh( ).
*        CATCH cx_salv_not_found.
*      ENDTRY.
** Input is valid, we can save the data
*      IF s = 'X'.
** You will need to implement your own save method, based on your own data
*        " save( ).
*      ENDIF.

    WHEN 'UPSNC'.


      lo_selections = gr_table->get_selections( ).
      lt_rows = lo_selections->get_selected_rows( ).

      IF lt_rows IS INITIAL.
        msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE msg_text TYPE 'I'.
      ELSE.

        LOOP AT lt_rows INTO ls_row.

          READ TABLE it_saida INDEX ls_row.


          CLEAR bapilogond.
          MOVE-CORRESPONDING logondata TO bapilogond.

          user_bapi = it_saida-logsap.

          snc = VALUE #( pname = |{ it_saida-filter }{ it_saida-logad }@{ it_saida-domain }| ).
          sncx = VALUE #( pname = abap_true ).

          TRY.
              CALL FUNCTION 'SUSR_BAPI_USER_CHANGE'
                EXPORTING
                  username   = user_bapi
                  snc        = snc
                  sncx       = sncx
                  logondata  = bapilogond
                  logondatax = logondatax
                TABLES
                  return     = return.
            CATCH cx_salv_not_found.
          ENDTRY.

        ENDLOOP.

        PERFORM atualiza_grid.

      ENDIF.

    WHEN 'CLSNC'.


      lo_selections = gr_table->get_selections( ).
      lt_rows = lo_selections->get_selected_rows( ).


      IF lt_rows IS INITIAL.
        msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE msg_text TYPE 'I'.
      ELSE.

        LOOP AT lt_rows INTO ls_row.

          READ TABLE it_saida INDEX ls_row.


          CLEAR bapilogond.
          MOVE-CORRESPONDING logondata TO bapilogond.

          user_bapi = it_saida-logsap.

          snc = VALUE #( pname = || ).
          sncx = VALUE #( pname = abap_true ).

          TRY.
              CALL FUNCTION 'SUSR_BAPI_USER_CHANGE'
                EXPORTING
                  username   = user_bapi
                  snc        = snc
                  sncx       = sncx
                  logondata  = bapilogond
                  logondatax = logondatax
                TABLES
                  return     = return.
            CATCH cx_salv_not_found.
          ENDTRY.

        ENDLOOP.

        PERFORM atualiza_grid.

      ENDIF.


    WHEN 'REFRESH'.

      PERFORM atualiza_grid.

  ENDCASE.

ENDFORM.

FORM container1.
*  create object mr_container type cl_gui_custom_container
*  exporting
*  container_name = m_cont_name.

*... create an ALV table
  TRY.
      cl_salv_table=>factory(
          EXPORTING
        r_container = painel_1
        container_name = 'PAINEL_1'
        IMPORTING
          r_salv_table   = gr_table
        CHANGING
          t_table        = it_saida[] ).
    CATCH cx_salv_msg.
      "lcl_util=>x_error( ).                                 "#EC NO_HANDLER
  ENDTRY.

*...Add DDIC reference for F4 generating
  DATA gr_salv_func TYPE REF TO cl_salv_functions .
  gr_salv_func = gr_table->get_functions( ).
  gr_salv_func->set_all( abap_true ).

*... §3.1 activate ALV generic Functions
  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( abap_false ).

  "lr_functions->set_group_aggregation( space ).
  "lr_functions->set_group_layout( 'X' ).

**... §3.2 include own functions
  l_text_UPSNC = 'Associar'.
  l_icon_UPSNC = icon_wf_link.
  l_text_CLSNC = 'Desassociar'.
  l_icon_CLSNC = icon_wf_unlink.
  l_text_RF = 'Atualizar'.
  l_icon_RF = icon_refresh.

  TRY.
      lr_functions->add_function(
        name     = 'UPSNC'
        icon     = l_icon_UPSNC
        text     = l_text_UPSNC
        tooltip  = l_text_UPSNC
        position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
  name     = 'CLSNC'
  icon     = l_icon_CLSNC
  text     = l_text_CLSNC
  tooltip  = l_text_CLSNC
  position = if_salv_c_function_position=>right_of_salv_functions ).


      lr_functions->add_function(
 name     = 'REFRESH'
 icon     = l_icon_RF
 text     = l_text_RF
 tooltip  = l_text_RF
 position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

  lr_columns = gr_table->get_columns( ).
  PERFORM organizar_colunas.
  lr_columns->set_optimize( abap_true ).
  PERFORM configurar_colunas USING lr_columns.

*      " F4 DDIC

*   DATA lv_ddic TYPE salv_s_ddic_reference.
*
**   TRY.
**       lr_column ?= lr_columns->get_column( columnname = 'NM_INDIC' ).
**       lv_ddic = VALUE #( table = 'ZFIT0201' field = 'NM_INDIC').
**       lr_column->set_ddic_reference( lv_ddic  ). "EXPORTING value = lv_ddic
**       lr_column->set_f4(  if_salv_c_bool_sap=>true ).
**     CATCH cx_salv_not_found.
**   ENDTRY.
*
**... §4 set hotspot column
**    TRY.
**        lr_column ?= lr_columns->get_column( 'MATNR' ).
**        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
**      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
**    ENDTRY.




**... §6 register to the events of cl_salv_table
*   DATA lr_events TYPE REF TO cl_salv_events_table.
*
*   lr_events = gr_table->get_event( ).
*
**   CREATE OBJECT gr_events.
**
***... §6.1 register to the event USER_COMMAND
**   SET HANDLER gr_events->on_user_command FOR lr_events.
**
**   SET HANDLER gr_events->on_before_user_command FOR lr_events.
**
**   SET HANDLER gr_events->on_after_user_command FOR lr_events.
**
************************************************************************
**     SET HANDLER gr_events->on_data_change FOR ALL INSTANCES.
**  SET HANDLER gr_events->on_ucomm FOR ALL INSTANCES.
***...Event handler for the button.....................................
***  "Event handler
**  SET HANDLER gr_events->on_after_refresh
**    FOR ALL INSTANCES
**    ACTIVATION 'X'.

  "... §6 register to the events of cl_salv_table
  DATA: lr_events TYPE REF TO cl_salv_events_table.

  lr_events = gr_table->get_event( ).

  CREATE OBJECT gr_events.

*... §6.1 register to the event USER_COMMAND
  SET HANDLER gr_events->on_user_command FOR lr_events.

  SET HANDLER gr_events->on_before_user_command FOR lr_events.

  SET HANDLER gr_events->on_after_user_command FOR lr_events.


*... set list title

  l_title = TEXT-t01.
  lr_display_settings = gr_table->get_display_settings( ).
  lr_display_settings->set_list_header_size( '1' ). "0=l, 1=s, 2=m
  lr_display_settings->set_list_header( l_title ).

  "Enable Zebra Layout
  lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable cell selection mode
  lR_selections = gr_table->get_selections( ).
  lR_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  lv_key-report = sy-repid.
  lR_layout = gr_table->get_layout( ).
  lR_layout->set_key( lv_key ).
  lR_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lR_layout->set_default( abap_true ).

*... §7 display the table
  gr_table->display( ).
  "alv_status = co_alv_ok.
ENDFORM.

FORM configurar_colunas USING ir_columns TYPE REF TO cl_salv_columns.

  ".. Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)lr_column
  TRY.

      lr_column = ir_columns->get_column( 'EMAIL' ).
      lr_column->set_short_text( 'EMAIL' ).
      lr_column->set_medium_text( 'EMAIL' ).
      lr_column->set_long_text( 'EMAIL' ).

      lr_column = ir_columns->get_column( 'CPF' ).
      lr_column->set_short_text( 'CPF' ).
      lr_column->set_medium_text( 'CPF' ).
      lr_column->set_long_text( 'CPF' ).

      lr_column = ir_columns->get_column( 'CUSTO' ).
      lr_column->set_short_text( 'CUSTO' ).
      lr_column->set_medium_text( 'CENT. CUSTO' ).
      lr_column->set_long_text( 'CENTRO DE CUSTO' ).

      lr_column = ir_columns->get_column( 'EMP' ).
      lr_column->set_short_text( 'EMP' ).
      lr_column->set_medium_text( 'EMPRESA' ).
      lr_column->set_long_text( 'EMPRESA' ).

      lr_column = ir_columns->get_column( 'FIL' ).
      lr_column->set_short_text( 'FIL' ).
      lr_column->set_medium_text( 'FILIAL' ).
      lr_column->set_long_text( 'FILIAL' ).

      lr_column = ir_columns->get_column( 'LOGSAP' ).
      lr_column->set_short_text( 'LOG' ).
      lr_column->set_medium_text( 'LOGIN' ).
      lr_column->set_long_text( 'LOGIN' ).

      lr_column = ir_columns->get_column( 'NOME' ).
      lr_column->set_short_text( 'NOME' ).
      lr_column->set_medium_text( 'NOME' ).
      lr_column->set_long_text( 'NOME' ).

      lr_column = ir_columns->get_column( 'NMSNC' ).
      "lr_column->set_short_text( 'SNC' ).
      lr_column->set_medium_text( 'SNC Name' ).
      lr_column->set_long_text( 'SNC Name' ).

      lr_column = ir_columns->get_column( 'LOGAD' ).
      lr_column->set_short_text( 'LOGAD' ).
      lr_column->set_medium_text( 'LOGIN AD' ).
      lr_column->set_long_text( 'LOGIN AD' ).

      lr_column = ir_columns->get_column( 'DOMAIN' ).
      lr_column->set_short_text( 'DOM' ).
      lr_column->set_medium_text( 'DOMINIO' ).
      lr_column->set_long_text( 'DOMINIO' ).

      lr_column = ir_columns->get_column( 'FILTER' ).
      lr_column->set_short_text( 'FILTRO' ).
      lr_column->set_medium_text( 'FILTRO' ).
      lr_column->set_long_text( 'FILTRO' ).

      " Esconde Coluna
      lr_column = ir_columns->get_column( 'MANDT' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO lex_not_found.
      "write some error handling
  ENDTRY.

ENDFORM.

FORM organizar_colunas.
  lr_columns->set_column_position(  columnname = 'EMAIL' position = 1 ).
  lr_columns->set_column_position(  columnname = 'CPF' position = 2 ).
  lr_columns->set_column_position(  columnname = 'CUSTO' position = 3 ).
  lr_columns->set_column_position(  columnname = 'EMP' position = 4 ).
  lr_columns->set_column_position(  columnname = 'FIL' position = 5 ).
  lr_columns->set_column_position(  columnname = 'LOGSAP' position = 6 ).
  lr_columns->set_column_position(  columnname = 'NOME' position = 7 ).
  lr_columns->set_column_position(  columnname = 'NMSNC' position = 8 ).
  lr_columns->set_column_position(  columnname = 'LOGAD' position = 9 ).
  lr_columns->set_column_position(  columnname = 'DOMAIN' position = 10 ).
  lr_columns->set_column_position(  columnname = 'FILTER' position = 11 ).


ENDFORM.

FORM selecao_dados.

  DATA hj TYPE sy-datum.
  DATA empty TYPE char1.
  DATA p_cpf TYPE char11.
  hj = sy-datum.
  empty = ''.


  LOOP AT s_CPF ASSIGNING FIELD-SYMBOL(<s_CPF>).
    REPLACE ALL OCCURRENCES OF '-' IN <s_CPF> WITH empty.
    REPLACE ALL OCCURRENCES OF '.' IN <s_CPF> WITH empty.
  ENDLOOP.

  SELECT DISTINCT
    e~usrid_long AS email,
    c~cpf_nr AS cpf,
    d~kostl AS custo,
    d~bukrs AS emp,
    d~werks AS fil,
    b~bname AS logSAP,
    d~ename AS nome,
    f~pname AS nmsnc,
    rtrim( ltrim( upper( substring( e~usrid_long,1, ( instr( e~usrid_long,'@' ) * 1  ) - 1 ) ), ' ' ) ,' ' ) AS loGAD,
    'AROEIRA.CORP' AS domain,
    'p:CN=' AS filter

  FROM adcp AS a
  LEFT JOIN usr21 AS b ON b~persnumber = a~persnumber AND b~addrnumber = a~addrnumber
  LEFT JOIN pa0465 AS c ON a~fax_number = replace( replace( c~cpf_nr, '-', @empty ), '.', @empty )
  LEFT JOIN pa0001 AS d ON d~pernr = c~pernr
  LEFT JOIN pa0105 AS e ON e~pernr = d~pernr
  LEFT JOIN usracl AS f ON f~bname = b~bname
  WHERE 1 = 1
  AND a~fax_number IN @s_cpf
  AND b~bname IN @s_bname
  AND c~pernr IN @s_mat
  AND c~endda >= @hj
  AND d~endda >= c~endda
  AND e~endda >= c~endda
  AND d~plans <> '99999999'
  AND c~subty = '0001'
  AND d~kostl IN @s_kostl
  AND d~bukrs IN @s_bukrs
  AND d~werks IN @s_werks
  AND b~bname IS NOT NULL
  AND e~subty = 'MAIL'
  INTO TABLE @it_saida.

  SORT it_saida BY nome emp fil custo ASCENDING.

ENDFORM.

FORM atualiza_grid.

  REFRESH it_saida.

  PERFORM selecao_dados.
  gr_table->refresh( ).
  gr_table->display( ).

ENDFORM.

FORM selecione_linha.


  DATA:
    lv_titel TYPE string, "
    lv_txt1	 TYPE string, "
    lv_txt2	 TYPE string, "
    lv_txt3	 TYPE string, "   SPACE
    lv_txt4	 TYPE string. "   SPACE

  lv_titel = 'Alerta!'.
  lv_txt1 = 'Selecione ao menos uma linha!'.
  CALL FUNCTION 'POPUP_TO_INFORM'  "Dialog Box to Display a Message
    EXPORTING
      titel = lv_titel
      txt1  = lv_txt1
      "TXT2 = lv_txt2
      "TXT3 = lv_txt3
      "TXT4 = lv_txt4
    . " POPUP_TO_INFORM

ENDFORM.


FORM docker.
  CREATE OBJECT container_main
    EXPORTING
      container_name = 'CONTAINER1'
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

** Exibe Painel 2
*    CALL METHOD painel_control->get_container
*      EXPORTING
*        row       = 2
*        column    = 1
*      RECEIVING
*        container = painel_2.

ENDFORM.


*&---------------------------------------------------------------------*
*& Include          ZMMR189_PBO
*&---------------------------------------------------------------------*
FORM display_grid.

  CALL SCREEN 0100.

ENDFORM.                    "display_grid

*&---------------------------------------------------------------------*
*&      Module  d0100_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0100_pbo OUTPUT.
  PERFORM d0100_pbo.
ENDMODULE.                 " d0100_pbo  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  d0100_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d0100_pai INPUT.
  PERFORM d0100_pai.
ENDMODULE.                 " d0100_pai  INPUT

*&---------------------------------------------------------------------*
*&      Form  d0100_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM d0100_pbo .

  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.
  PERFORM dockER.
  PERFORM container1.
ENDFORM.                                                    " d0100_pbo

*&---------------------------------------------------------------------*
*&      Form  d0100_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM d0100_pai .
  g_okcode = sy-ucomm.
  CASE g_okcode.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'FILTER'.
      SET SCREEN 0..
  ENDCASE.

ENDFORM.


START-OF-SELECTION.
*  cl_demo_output=>display( it_saida ).
  PERFORM selecao_dados.
  PERFORM display_grid.
