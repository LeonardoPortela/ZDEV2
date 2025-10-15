*&---------------------------------------------------------------------*
*& Report  ZPMR0063
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

*&                 AMAGGI - Projeto
*&---------------------------------------------------------------------*
*& Abap         :  Anderson Oenning ( AO ) - Amaggi
*& Data         : 20/05/2020
*& Especialista : Cleudo Ferreira
*& Chamado/Descrição : CS2019001804 - Interface recebimento de dados referente abastecimento de combustivel frota Amaggi - Autotrac
*& Tela para analise de logs.
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------
*&
*&--------------------------------------------------------------------
REPORT zpmr0063.

TABLES: aufk, zpmt0035, equi.

TYPES: BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: lcl_alv           TYPE REF TO cl_gui_alv_grid,
      lcl_container_alv TYPE REF TO cl_gui_custom_container.


DATA: t_fcat             TYPE lvc_t_fcat,
      dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
      dg_parent_1        TYPE REF TO cl_gui_container,
      dg_parent_alv      TYPE REF TO cl_gui_container,
      table_element      TYPE REF TO cl_dd_table_element,
      column             TYPE REF TO cl_dd_area,
      p_text             TYPE sdydo_text_element,
      obj_custom         TYPE REF TO cl_gui_custom_container,
      dg_dyndoc_id       TYPE REF TO cl_dd_document,
      dg_parent_2a       TYPE REF TO cl_gui_container,
      dg_parent_2        TYPE REF TO cl_gui_container,
      gs_layout          TYPE lvc_s_layo,
      gs_variant         TYPE disvariant,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      picture            TYPE REF TO cl_gui_picture,
      url(255)           TYPE c,
      ctl_alv            TYPE REF TO cl_gui_alv_grid,
      wa_toolbar         TYPE stb_button.
"t_print            TYPE slis_print_alv,
"events             TYPE slis_t_event,
" t_sort             TYPE slis_t_sortinfo_alv WITH HEADER LINE,
"it_fcat            TYPE TABLE OF ty_estrutura,
"gd_layout          TYPE slis_layout_alv.

DATA:
  table_element2          TYPE REF TO cl_dd_table_element,
  sdydo_text_element(255),
  p_text_table            TYPE sdydo_text_table,
  ls_stable               TYPE lvc_s_stbl,
  dg_html_cntrl           TYPE REF TO cl_gui_html_viewer,
  column_1                TYPE REF TO cl_dd_area,
  it_selected_rows        TYPE lvc_t_row,
  wa_selected_rows        TYPE lvc_s_row,
  lines                   TYPE sy-tabix.

DATA: editor   TYPE REF TO cl_gui_textedit,
      c_editor TYPE REF TO cl_gui_custom_container,
      txtopen  TYPE c,
      ok_code  LIKE sy-ucomm.

DATA: block     TYPE c,
      it_editor TYPE TABLE OF ty_editor,
      wa_editor TYPE ty_editor.

DATA: t_log TYPE TABLE OF zpme0060,
      t_cob TYPE TABLE OF zpme0065.
DATA: w_zpme0060 TYPE zpme0060.

DATA: r_placa TYPE zrsdsselopts.
DATA: r_equnr TYPE zrsdsselopts.
DATA: r_frota TYPE zrsdsselopts.
DATA: r_werks TYPE zrsdsselopts.
DATA: r_stat  TYPE zrsdsselopts..
DATA: r_suc TYPE RANGE OF char1.
DATA: r_err TYPE RANGE OF char1.
DATA: p_sim     TYPE char1,
      vl_dates1 TYPE char10,
      vl_dates2 TYPE char10,
      p_nao     TYPE char1.

**********************************************************************

* Layout Variant - - 115303 CS2023000352 Inclusão do "Salvar Variantes" - PSA
**********************************************************************
DATA: ls_variant TYPE disvariant,
      l_exit     TYPE char1,
      variante   LIKE disvariant.
**********************************************************************

DATA: odometro TYPE imrc_cntrc.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-004.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_r1 RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND abc. "Autotrac
    SELECTION-SCREEN COMMENT 4(10) TEXT-005 FOR FIELD r_r1.


    PARAMETERS: r_r2 RADIOBUTTON GROUP 1. "Posto Mirian
    SELECTION-SCREEN COMMENT 25(12) TEXT-006 FOR FIELD r_r2.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

"Montando tela de parametros de seleção.
SELECTION-SCREEN: BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-009.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1 .
    PARAMETER: p_err AS CHECKBOX DEFAULT 'X' USER-COMMAND abc.
    SELECTION-SCREEN COMMENT 05(12) TEXT-010 FOR FIELD p_err.
    SELECTION-SCREEN POSITION 20.
    PARAMETER: p_suc AS CHECKBOX DEFAULT 'X' USER-COMMAND abc.
    SELECTION-SCREEN COMMENT 25(14) TEXT-011 FOR FIELD p_suc.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b6.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
*                 P_BUKRS FOR AUFK-BUKRS NO-EXTENSION NO INTERVALS,
                   p_werks FOR aufk-werks NO-EXTENSION NO INTERVALS,
                   p_equnr FOR equi-equnr ,
                   p_frota FOR zpmt0035-frota,
                   p_placa FOR zpmt0035-placa,
                   p_erdat FOR aufk-erdat OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-012.
  PARAMETERS: p_var TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b3.

**********************************************************************
* Function List Variant Report - 115303 CS2023000352 Inclusão do "Salvar Variantes" - PSA
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.

  PERFORM f4_variant.

**********************************************************************

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN '%B009008_BLOCK_1000'
        OR 'P_ERR'
        OR '%F010012_1000'
        OR 'P_SUC'
        OR '%F011015_1000 '.
*
*
        IF r_r2 IS INITIAL.
          screen-active = '1'.         "show parameters     "n921165
        ELSE.                                               "n921165
          screen-active = '0'.         "Hide parameters     "n921165

        ENDIF.
        MODIFY SCREEN.


    ENDCASE.
  ENDLOOP.

CLASS lcl_eventhandler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:

      handle_node_double_click
        FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key,

      handle_item_double_click
        FOR EVENT item_double_click OF cl_gui_alv_tree
        IMPORTING node_key
                  fieldname.

ENDCLASS.






















*
CLASS lcl_grid_event DEFINITION.
* seção publica
  PUBLIC SECTION.
*...Barra de Ferramentas
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.
*...User Command
    METHODS handle_command_grid
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

ENDCLASS. "LCL_GRID_EVENT DEFINITION

CLASS lcl_eventhandler IMPLEMENTATION.

  METHOD handle_node_double_click.


  ENDMETHOD.


  METHOD handle_item_double_click.


  ENDMETHOD.



ENDCLASS.

CLASS lcl_grid_event IMPLEMENTATION.

  "Method Handle_Toolbar.
  METHOD handle_toolbar.

    IF p_suc IS INITIAL.
      ty_toolbar-icon      = icon_refresh.
      ty_toolbar-function  = 'REFRESH'.
      ty_toolbar-text      = 'Atualizar'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

*    IF p_suc IS INITIAL.
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'DELETE'.
    ty_toolbar-text      = 'Excluir'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*    ENDIF.

  ENDMETHOD.


  "Method onClick
  METHOD on_double_click.

    DATA: lc_binario TYPE xstring,
          lt_pdf     TYPE TABLE OF char80.

    DATA: placa TYPE char7.
    DATA: text TYPE string.

    CLEAR:  text ,
            placa.

    CHECK e_row-rowtype IS INITIAL.

    TRY .
        w_zpme0060 = t_log[ e_row ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    CASE e_column.
      WHEN 'STATUS'.
        FREE: it_editor.

        IF w_zpme0060-status EQ icon_change.

          "Seleciona ponto de medição do equpamento.
          IF w_zpme0060-equnr IS NOT INITIAL.
            zcl_exc_apont_med=>select_pont_medicao(
                   EXPORTING
               i_equnr =   w_zpme0060-equnr
                  RECEIVING
              t_dimpt = DATA(t_dimpt) ).
          ENDIF.

          LOOP AT t_dimpt  ASSIGNING FIELD-SYMBOL(<w_dimpt>) WHERE atnam EQ 'HORIMETRO' OR atnam EQ 'ODOMETRO' AND indtr = ' '.
            IF sy-subrc EQ 0.
              zcl_int_sappm_autotrac=>m_check_pont_med(
                EXPORTING
                  i_date  = w_zpme0060-dt_cupom_fisc
                  i_time  = w_zpme0060-hr_cupom_fisc
                  i_point = <w_dimpt>-point    " Ponto de medição
                IMPORTING
                  e_value = w_zpme0060-odometro_ant   " Unidade de medida ao entrar documento
              ).

            ENDIF.
          ENDLOOP.
          CONDENSE w_zpme0060-odometro_ant.
          REPLACE '.' IN w_zpme0060-odometro_ant WITH ',' .
          REPLACE '.' IN w_zpme0060-odometro WITH ',' .

          CLEAR: odometro.
          odometro = w_zpme0060-odometro.

          CALL SCREEN 0200 STARTING AT 5 5 ENDING AT 100 23.

        ENDIF.

      WHEN 'EQUNR'.
        SET PARAMETER ID 'EQN' FIELD w_zpme0060-equnr.
        CALL TRANSACTION 'IE02' AND SKIP FIRST SCREEN.
    ENDCASE.

  ENDMETHOD.

  METHOD handle_command_grid.

    DATA: i_equnr TYPE equnr.

    CASE e_ucomm.
      WHEN 'DELETE'.
        PERFORM f_delete_doc.

      WHEN 'REFRESH'.
        CLEAR: i_equnr.
        LOOP AT t_log ASSIGNING FIELD-SYMBOL(<l_log>).
          "Seleciona equipamento.
          CLEAR: i_equnr.
          zcl_exc_apont_med=>get_equipamento( EXPORTING i_placa = <l_log>-placa  IMPORTING e_equnr = i_equnr ).

          IF i_equnr IS NOT INITIAL.
            IF <l_log>-status EQ icon_change OR <l_log>-status EQ icon_display_text.

              "Limpa log da tabela.
              zcl_int_sappm_autotrac=>m_clear_id_item( w_zpme0060 = <l_log> ).

              "Processa documento.
              zcl_int_sappm_autotrac=>m_reproc_inform(
              w_zpme0060 =  <l_log>
              i_equnr = i_equnr
              p_sub_cont = ' ').
            ENDIF.
          ENDIF.
        ENDLOOP.


*  "Seleção de dados.
        zcl_int_sappm_autotrac=>i_cons_logs(
            EXPORTING
            i_frota       = r_frota        " Numero da frota
            i_equnr       = r_equnr        " Nº equipamento
            i_centro      = r_werks        " Centro
            i_perido_inic = p_erdat-low    " Campo do sistema ABAP: data atual do servidor de aplicação
            i_perido_fim  = p_erdat-high   " Campo do sistema ABAP: data atual do servidor de aplicação
            i_status      = r_stat         " Campo de texto do comprimento 1
          IMPORTING
            t_logs        =  t_log   " Dados de log de processamento integração SAP PM x Autotrac
        ).

*        CHECK  T_LOG IS NOT INITIAL.

        CALL METHOD ctl_alv->refresh_table_display
          EXPORTING
            is_stable = ls_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.

*        LEAVE TO SCREEN 0.



    ENDCASE.


  ENDMETHOD.

ENDCLASS.

DATA: lcl_event         TYPE REF TO lcl_grid_event.

START-OF-SELECTION.



  IF r_r2 IS INITIAL.

    r_equnr  =  VALUE #( FOR l IN  p_equnr   ( sign = 'I' option = 'EQ' low = l-low high = l-high ) ).
    r_frota  =  VALUE #( FOR s IN  p_frota   ( sign = 'I' option = 'EQ' low = s-low ) ).
    r_werks  =  VALUE #( FOR t IN  p_werks   ( sign = 'I' option = 'EQ' low = t-low ) ).

    IF p_err EQ abap_true.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'E' ) TO r_stat.
    ENDIF.

    IF p_suc EQ abap_true.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = 'S') TO r_stat.
    ENDIF.

*  "Seleção de dados.
    zcl_int_sappm_autotrac=>i_cons_logs(
        EXPORTING
        i_frota       = r_frota        " Numero da frota
        i_equnr       = r_equnr        " Nº equipamento
        i_centro      = r_werks        " Centro
        i_perido_inic = p_erdat-low    " Campo do sistema ABAP: data atual do servidor de aplicação
        i_perido_fim  = p_erdat-high   " Campo do sistema ABAP: data atual do servidor de aplicação
        i_status      = r_stat         " Campo de texto do comprimento 1
      IMPORTING
        t_logs        =  t_log   " Dados de log de processamento integração SAP PM x Autotrac
    ).

    CHECK  t_log IS NOT INITIAL.

    CALL SCREEN 0100.
  ELSE.
    r_placa  =  VALUE #( FOR w IN  p_placa   ( sign = 'I' option = 'EQ' low = w-low high = w-high ) ).
    "Seleção de dados.
    zcl_int_sappm_autotrac=>get_dados_p_mirian(
      EXPORTING
        i_placa       = r_placa
        i_perido_inic = p_erdat-low    " Campo do sistema ABAP: data atual do servidor de aplicação
        i_perido_fim  = p_erdat-high   " Campo do sistema ABAP: data atual do servidor de aplicação
      IMPORTING
        t_comb        = t_cob  " Estrutura de dados combustivél Posto Mirian
    ).

    CHECK  t_cob IS NOT INITIAL.

    CALL SCREEN 0500.


  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS0100'.
  SET TITLEBAR 'TITLE0100'.

*  LOOP AT SCREEN.
*    CASE SCREEN-NAME.
*      WHEN 'OBS' .
*        SCREEN-INPUT = 0. "Campo Fechado
*        MODIFY SCREEN.
*    ENDCASE.
*  ENDLOOP.


  IF t_log[] IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-021 DISPLAY LIKE 'S'.
    LEAVE TO SCREEN 0.
  ENDIF.

  PERFORM f_exibe_alv.



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

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibe_alv .
  DATA: ls_variant TYPE disvariant.

  ls_variant =  sy-repid.
  gs_variant =  sy-repid.


  FREE: t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZPME0060'
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  gs_layout-zebra = abap_true.       "Código Zebrado
  gs_layout-no_rowmark = abap_true. "Exclui barra standard de flag a esquerda
  gs_layout-cwidth_opt = abap_true. "Ajusta tamanho na coluna
  gs_layout-box_fname = abap_true. "

  IF ctl_alv IS INITIAL.
    CREATE OBJECT obj_custom
      EXPORTING
        container_name              = 'ALV_LOG'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = obj_custom
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
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 15.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 60.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent          = dg_parent_alv
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.


    CREATE OBJECT lcl_event.
*      SET HANDLER LCL_EVENT-> FOR CTL_ALV.
    SET HANDLER lcl_event->handle_toolbar FOR ctl_alv.
    SET HANDLER lcl_event->handle_command_grid FOR ctl_alv.
    SET HANDLER lcl_event->on_double_click  FOR ctl_alv.


  ENDIF.

**********************************************************************
*Caso o Parametro da Variante estiver preenchido -  115303 CS2023000352 Inclusão do "Salvar Variantes" - PSA
**********************************************************************
  IF p_var IS NOT INITIAL.
    gs_variant-variant = p_var.
  ENDIF.
**********************************************************************

  CALL METHOD ctl_alv->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout
      is_variant      = gs_variant
      i_save          = 'A'
    CHANGING
      it_fieldcatalog = t_fcat
      it_outtab       = t_log.

***Cabecalho.
  CREATE OBJECT dg_dyndoc_id
    EXPORTING
      style = 'ALV_GRID'.


  CALL METHOD dg_dyndoc_id->initialize_document.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 1
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element.

  CALL METHOD table_element->add_column
    IMPORTING
      column = column.

  CALL METHOD table_element->set_column_style
    EXPORTING
      col_no    = 1
      "SAP_ALIGN = 'CENTER'
      sap_style = cl_dd_document=>heading.

  IF r_r1 IS NOT INITIAL.
    p_text = TEXT-002.
  ELSE.
    p_text = TEXT-022.
  ENDIF.

  CALL METHOD column->add_text
    EXPORTING
      text      = p_text
      sap_style = 'HEADING'.


  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element2.


  CALL METHOD table_element2->add_column
    EXPORTING
      sap_style   = 'SAP_BOLD'
      style_class = 'SAP_BOLD'
    IMPORTING
      column      = column_1.

  PERFORM cabecario.

*  ------------------
  CALL METHOD column_1->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  CALL METHOD dg_dyndoc_id->merge_document.

  CREATE OBJECT dg_html_cntrl
    EXPORTING
      parent = dg_parent_2.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_2
    EXCEPTIONS
      html_display_error = 1.


  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0563   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                  CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.

  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.

  WHILE l_graphic_conv > 255.

    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.

  ENDWHILE.

  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CABECARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cabecario .

  READ TABLE t_log ASSIGNING FIELD-SYMBOL(<_log>) INDEX 1.

  CONCATENATE 'Centro:' p_werks-low INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  "------------------
  LOOP AT p_erdat.
    IF p_erdat-option EQ 'BT'.
      CONCATENATE p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) INTO vl_dates1.
      CONCATENATE p_erdat-high+6(2) '.' p_erdat-high+4(2) '.' p_erdat-high(4) INTO vl_dates2.
      CONCATENATE 'Período:' vl_dates1 '-' vl_dates2 INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
      EXIT.
    ELSE.
      CONCATENATE p_erdat-low+6(2) '.' p_erdat-low+4(2) '.' p_erdat-low(4) INTO vl_dates1.
      CONCATENATE 'Período:' vl_dates1 INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.
    CLEAR: sdydo_text_element.
  ENDLOOP.
*
*  CONCATENATE 'Qtde registro:' SY- INTO SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
*  APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.
*  CLEAR: SDYDO_TEXT_ELEMENT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'ST0200'.
  SET TITLEBAR 'TIT0200'.

*  LOOP AT SCREEN.
*    CASE SCREEN-NAME.
*      WHEN '%#AUTOTEXT001'.
*        SCREEN-INPUT = 1. "Campo Aberto
*
**          SCREEN-INPUT = 0. "Campo Fechado*
*        MODIFY SCREEN.
*    ENDCASE.
*  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.

      DATA: w_text TYPE char72.


***      Verifica se o campo observação foi preenchida.
      CLEAR wa_editor.
      CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
      IF it_editor IS INITIAL.
        MESSAGE TEXT-003 TYPE 'I' DISPLAY LIKE 'E'.
        txtopen = abap_true.
      ELSE.

        LOOP AT it_editor ASSIGNING FIELD-SYMBOL(<w_text>).
          w_text = |{ w_text } { <w_text>-line }|.

        ENDLOOP.

        DATA(p_sub_cont) = ' '.
        IF p_sim EQ abap_true.
          p_sub_cont = abap_true.
        ELSE.
          p_sub_cont = ' '.
        ENDIF.

        "Processa documento.
        zcl_int_sappm_autotrac=>m_reproc_inform(
        w_zpme0060 =  w_zpme0060 i_equnr = w_zpme0060-equnr  p_sub_cont = p_sub_cont ).
        zcl_int_sappm_autotrac=>m_regist_justif(
        i_line = w_text w_zpme0060 = w_zpme0060 i_odometro = odometro ).
        CLEAR: p_sub_cont.

*  "Seleção de dados.
        zcl_int_sappm_autotrac=>i_cons_logs(
            EXPORTING
            i_frota       = r_frota        " Numero da frota
            i_equnr       = r_equnr        " Nº equipamento
            i_centro      = r_werks        " Centro
            i_perido_inic = p_erdat-low    " Campo do sistema ABAP: data atual do servidor de aplicação
            i_perido_fim  = p_erdat-high   " Campo do sistema ABAP: data atual do servidor de aplicação
            i_status      = r_stat         " Campo de texto do comprimento 1
          IMPORTING
            t_logs        =  t_log   " Dados de log de processamento integração SAP PM x Autotrac
        ).

*        CHECK  T_LOG IS NOT INITIAL.

        CALL METHOD ctl_alv->refresh_table_display
          EXPORTING
            is_stable = ls_stable
          EXCEPTIONS
            finished  = 1
            OTHERS    = 2.

        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  PERFORM caixa_txt_obs.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CAIXA_TXT_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM caixa_txt_obs .

  txtopen = abap_true.

  IF NOT c_editor IS INITIAL AND NOT editor IS INITIAL.
    CALL METHOD c_editor->free( ).
    CALL METHOD editor->free( ).
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

  CALL METHOD editor->set_text_as_stream
    EXPORTING
      text = it_editor.


  FREE: txtopen, it_editor.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'STATUS0400'.
  SET TITLEBAR 'TITLE0400'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_DELETE_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_delete_doc .
*  *        T_LOG

  REFRESH: it_selected_rows.
  CLEAR: wa_selected_rows.

  CALL METHOD ctl_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  DESCRIBE TABLE it_selected_rows LINES lines.

  IF ( lines IS INITIAL ).
    MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE t_log INTO DATA(w_log) INDEX wa_selected_rows-index.
      IF sy-subrc EQ 0.
        zcl_int_sappm_autotrac=>m_estorno_doc_med(
          EXPORTING
            e_doc_med = w_log-mdocm    " Doc.medição
          IMPORTING
            r_return  = DATA(r_rturn)
            e_msgty   = DATA(e_msgty)    " Campo do sistema: tipo de mensagem
        ).

        IF e_msgty EQ 'S'.
          SELECT SINGLE * FROM zpmt0035 INTO @DATA(w_zpmt0035) WHERE id = @w_log-id AND item EQ @w_log-item.
          IF sy-subrc EQ 0.
            w_zpmt0035-estorno = 'X'.
            w_zpmt0035-aenam = sy-uname.
            MODIFY  zpmt0035 FROM w_zpmt0035.
            COMMIT WORK.
          ENDIF.
        ELSE.
          SELECT SINGLE * FROM zpmt0035 INTO w_zpmt0035 WHERE id = w_log-id AND item EQ w_log-item.
          IF sy-subrc EQ 0.
            w_zpmt0035-msgty = 'E'.
            w_zpmt0035-msgv1 = 'Não foi possivél estornar o documento -' && w_log-mdocm.
            w_zpmt0035-aenam = sy-uname.
            MODIFY  zpmt0035 FROM w_zpmt0035.
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: w_zpmt0035.
    ENDLOOP.
  ENDIF.

  IF p_err IS NOT INITIAL.
    DATA(s_err) = 'E'.
  ENDIF.

  IF p_suc IS NOT INITIAL.
    DATA(s_suc) = 'S'.
  ENDIF.

  "Selecionar logs.
  FREE: t_log.
  SELECT * FROM zpmt0035 INTO TABLE @DATA(t_zpmt0035)
  WHERE frota IN @p_frota
  AND   equnr IN @p_equnr
  AND   estorno NE 'X'
  AND  dt_cupom_fisc IN @p_erdat
  AND  msgty IN ( @s_suc, @s_err ).

  CHECK t_zpmt0035 IS NOT INITIAL.

  "Selecionando descrição frota.
  SELECT * FROM eqkt INTO TABLE @DATA(t_eqkt) FOR ALL ENTRIES IN @t_zpmt0035 WHERE equnr EQ @t_zpmt0035-equnr.

  "Selecionando descrição frota.
  MOVE-CORRESPONDING t_zpmt0035 TO t_log.

  LOOP AT t_log ASSIGNING FIELD-SYMBOL(<w_logs>).

    "Iserindo o nome do item.
    CASE <w_logs>-item.
      WHEN '10'.
        <w_logs>-desc_item = 'Odometro'.

      WHEN '20'.
        <w_logs>-desc_item = 'Combustível'.
      WHEN OTHERS.
    ENDCASE.

    READ TABLE t_eqkt ASSIGNING FIELD-SYMBOL(<w_eqkt>) WITH KEY equnr = <w_logs>-equnr.
    IF sy-subrc EQ 0.
      <w_logs>-equtx = <w_eqkt>-eqktx.
    ENDIF.

    DATA(icon_erro)   = icon_red_light.
    DATA(icon_exibir) = icon_display_text.
    DATA(icon_modif)  = icon_change.


    IF <w_logs>-msgty EQ 'E'.
      IF <w_logs>-item < '50'.
        <w_logs>-status = |{ icon_modif }|.
      ELSE.

        <w_logs>-status = |{ icon_exibir }|.
      ENDIF.
    ELSE.
      <w_logs>-status = icon_green_light.
    ENDIF.
  ENDLOOP.

  "Atualiza ALV.
  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS 'ST0500'.
  SET TITLEBAR 'TI0500'.

  IF t_cob[] IS INITIAL.
    MESSAGE s000(z_les) WITH TEXT-021 DISPLAY LIKE 'S'.
    LEAVE TO SCREEN 0.
  ENDIF.

  PERFORM f_exibe_alv_0500.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV_0500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibe_alv_0500 .

**********************************************************************

  FREE: t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZPME0065'
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.


  gs_layout-zebra = abap_true.       "Código Zebrado
  gs_layout-no_rowmark = abap_true. "Exclui barra standard de flag a esquerda
  gs_layout-cwidth_opt = abap_true. "Ajusta tamanho na coluna
  gs_layout-box_fname = abap_true. "

  IF ctl_alv IS INITIAL.
    CREATE OBJECT obj_custom
      EXPORTING
        container_name              = 'ALV_COMB'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.


    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = obj_custom
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
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 15.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 60.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent          = dg_parent_alv
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.


*    CREATE OBJECT lcl_event.
*      SET HANDLER LCL_EVENT-> FOR CTL_ALV.
*    SET HANDLER lcl_event->handle_toolbar FOR ctl_alv.
*    SET HANDLER lcl_event->handle_command_grid FOR ctl_alv.
*    SET HANDLER lcl_event->on_double_click  FOR ctl_alv.




  ENDIF.
**********************************************************************
*CONSTROI ALV - 115303 CS2023000352 Inclusão do "Salvar Variantes" - PSA
**********************************************************************
*CLEAR:ls_variant-variant.
*ls_variant-variant = p_var. "Usa Variante do Param - 115303 CS2023000352 Inclusão do "Salvar Variantes" - PSA
**********************************************************************
  CALL METHOD ctl_alv->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout
**********************************************************************
      is_variant      = ls_variant    "PERMITE SALVAR VARIANT - 115303 CS2023000352 Inclusão do "Salvar Variantes" - PSA
**********************************************************************
      i_save          = 'A'
    CHANGING
      it_fieldcatalog = t_fcat
      it_outtab       = t_cob.

***Cabecalho.
  CREATE OBJECT dg_dyndoc_id
    EXPORTING
      style = 'ALV_GRID'.


  CALL METHOD dg_dyndoc_id->initialize_document.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 1
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element.

  CALL METHOD table_element->add_column
    IMPORTING
      column = column.

  CALL METHOD table_element->set_column_style
    EXPORTING
      col_no    = 1
      "SAP_ALIGN = 'CENTER'
      sap_style = cl_dd_document=>heading.

  p_text = TEXT-002.

  CALL METHOD column->add_text
    EXPORTING
      text      = p_text
      sap_style = 'HEADING'.


  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element2.


  CALL METHOD table_element2->add_column
    EXPORTING
      sap_style   = 'SAP_BOLD'
      style_class = 'SAP_BOLD'
    IMPORTING
      column      = column_1.

  PERFORM cabecario.

*  ------------------
  CALL METHOD column_1->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  CALL METHOD dg_dyndoc_id->merge_document.

  CREATE OBJECT dg_html_cntrl
    EXPORTING
      parent = dg_parent_2.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_2
    EXCEPTIONS
      html_display_error = 1.


  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDFORM.

**********************************************************************
*BUSCA VARIANT PARAMETRO - 115303 CS2023000352 Inclusão do "Salvar Variantes" - PSA
**********************************************************************
FORM f4_variant.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_variant
      i_save        = 'A'
    IMPORTING
      e_exit        = l_exit
      es_variant    = ls_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  p_var =  ls_variant-variant.

ENDFORM.
**********************************************************************
