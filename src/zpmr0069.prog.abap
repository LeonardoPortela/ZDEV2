*&---------------------------------------------------------------------*
*& Report  ZPMR0069
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

*&                 AMAGGI - Projeto
*&---------------------------------------------------------------------*
*& Abap         : Jaime Tassoni
*& Data         : 20/05/2020
*& Especialista : Anderson Oenning
*& Chamado/Descrição : CS2020001309 Relatório de Visão Geral  de Ordens encerradas e abertas
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------
*&
*&--------------------------------------------------------------------
REPORT zpmr0069.

************************************************************************
* tabelas
************************************************************************
TABLES: viaufkst.

************************************************************************
* types
************************************************************************
TYPES: BEGIN OF ty_viaufk,
         aufpl TYPE viaufkst-aufpl,
         iwerk TYPE viaufkst-iwerk,
         auart TYPE viaufkst-auart,
         aufnr TYPE viaufkst-aufnr,
         tplnr TYPE viaufkst-tplnr,
         equnr TYPE viaufkst-equnr,
         ktext TYPE viaufkst-ktext,
         vaplz TYPE viaufkst-vaplz,
         objnr TYPE viaufkst-objnr,
         ilart TYPE viaufkst-ilart,
         qmnum TYPE viaufkst-qmnum,
         gstrp TYPE viaufkst-gstrp,
         gltrp TYPE viaufkst-gltrp,
       END   OF ty_viaufk,

       BEGIN OF ty_afvc,
         aufpl TYPE afvc-aufpl,
         aplzl TYPE afvc-aplzl,
         objnr TYPE afvc-objnr,
       END   OF ty_afvc,

       BEGIN OF ty_afvv,
         aufpl TYPE afvv-aufpl,
         aplzl TYPE afvv-aplzl,
         arbeh TYPE afvv-arbeh,
         arbei TYPE afvv-arbei,
         ofmnw TYPE afvv-ofmnw,
       END   OF ty_afvv,

       BEGIN OF ty_afru,
         rueck TYPE afru-rueck,
         rmzhl TYPE afru-rmzhl,
         aufnr TYPE afru-aufnr,
         ismne TYPE afru-ismne,
         ismnw TYPE afru-ismnw,
         pernr TYPE afru-pernr,
       END   OF ty_afru,

       BEGIN OF ty_coep,
         kokrs  TYPE coep-kokrs,
         belnr  TYPE coep-belnr,
         buzei  TYPE coep-buzei,
         objnr  TYPE coep-objnr,
         wtgbtr TYPE coep-wtgbtr,
       END   OF ty_coep,

       BEGIN OF ty_func,
         pernr TYPE afru-pernr,
       END   OF ty_func.

************************************************************************
* variavels / tabelas
************************************************************************
DATA: t_viaufk                TYPE TABLE OF ty_viaufk,
      t_tj02t                 TYPE TABLE OF tj02t,
      t_afvc                  TYPE TABLE OF ty_afvc,
      t_afvv                  TYPE TABLE OF ty_afvv,
      t_afru                  TYPE TABLE OF ty_afru,
      t_coep                  TYPE TABLE OF ty_coep,
      t_func                  TYPE TABLE OF ty_func,
      t_saida                 TYPE TABLE OF zpme0063,
*
      w_viaufk                TYPE ty_viaufk,
      w_tj02t                 TYPE tj02t,
      w_afvc                  TYPE ty_afvc,
      w_afvv                  TYPE ty_afvv,
      w_afru                  TYPE ty_afru,
      w_coep                  TYPE ty_coep,
      w_func                  TYPE ty_func,
      w_saida                 TYPE zpme0063,
*
      lcl_alv                 TYPE REF TO cl_gui_alv_grid,
      lcl_container_alv       TYPE REF TO cl_gui_custom_container,
*
      t_fcat                  TYPE lvc_t_fcat,
      w_fcat                  TYPE lvc_s_fcat,
      dg_splitter_1           TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2           TYPE REF TO cl_gui_splitter_container,
      dg_parent_1             TYPE REF TO cl_gui_container,
      dg_parent_alv           TYPE REF TO cl_gui_container,
      table_element           TYPE REF TO cl_dd_table_element,
      column                  TYPE REF TO cl_dd_area,
      p_text                  TYPE sdydo_text_element,
      obj_custom              TYPE REF TO cl_gui_custom_container,
      dg_dyndoc_id            TYPE REF TO cl_dd_document,
      dg_parent_2a            TYPE REF TO cl_gui_container,
      dg_parent_2             TYPE REF TO cl_gui_container,
      gs_layout               TYPE lvc_s_layo,
      g_custom_container      TYPE REF TO cl_gui_custom_container,
      picture                 TYPE REF TO cl_gui_picture,
      url(255)                TYPE c,
      ctl_alv                 TYPE REF TO cl_gui_alv_grid,
*
      table_element2          TYPE REF TO cl_dd_table_element,
      sdydo_text_element(255),
      p_text_table            TYPE sdydo_text_table,
      ls_stable               TYPE lvc_s_stbl,
      dg_html_cntrl           TYPE REF TO cl_gui_html_viewer,
      column_1                TYPE REF TO cl_dd_area,
      l_tabix                 TYPE sy-tabix,
      l_tot_func              TYPE i,
      l_arbei                 TYPE zpme0063-arbei,
      l_ismnw                 TYPE zpme0063-ismnw,
*
      editor                  TYPE REF TO cl_gui_textedit,
      c_editor                TYPE REF TO cl_gui_custom_container,
      txtopen                 TYPE c,
      ok_code                 LIKE sy-ucomm,
*
      vl_dates1               TYPE char10,
      vl_dates2               TYPE char10,

      c_alv_toolbarmanager    TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar              TYPE stb_button.

************************************************************************
* ranges
************************************************************************
RANGES:
      r_stat                  FOR jest-stat,
      r_txt04                 FOR tj02t-txt04.

************************************************************************
* tela selecao
************************************************************************
SELECTION-SCREEN: BEGIN OF   BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS  : s_aufnr      FOR viaufkst-aufnr ,
                  s_auart      FOR viaufkst-auart ,
                  s_tplnr      FOR viaufkst-tplnr ,
                  s_equnr      FOR viaufkst-equnr ,
                  s_kostl      FOR viaufkst-kostl ,
                  s_iwerk      FOR viaufkst-iwerk ,
                  s_erdat      FOR viaufkst-erdat  OBLIGATORY DEFAULT sy-datum TO sy-datum.
SELECTION-SCREEN:END OF      BLOCK b1.

SELECTION-SCREEN: BEGIN OF   BLOCK b2 WITH FRAME TITLE text-009.
PARAMETERS      : p_libe AS CHECKBOX     DEFAULT abap_true,
                  p_ence AS CHECKBOX     DEFAULT abap_true.
SELECTION-SCREEN:END OF      BLOCK b2.

************************************************************************
* classes
************************************************************************
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:
*Double-click Controle
      handle_hotspot_click FOR EVENT hotspot_click
                  OF        cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no,
*Para ser acionado antes dos comandos do usuário
      handle_before_user_command  FOR EVENT before_user_command
                  OF        cl_gui_alv_grid
        IMPORTING e_ucomm .
  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS lcl_event_handler IMPLEMENTATION .

  METHOD handle_before_user_command .
  ENDMETHOD .                    "handle_before_user_command

  METHOD handle_hotspot_click .

    READ TABLE t_saida INTO w_saida INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    CASE e_column_id-fieldname.
      WHEN 'AUFNR'.
        CHECK w_saida-aufnr IS NOT INITIAL.

        SET PARAMETER ID 'ANR' FIELD w_saida-aufnr.
        CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.

      WHEN 'EQUNR'.
        CHECK w_saida-equnr IS NOT INITIAL.

        SET PARAMETER ID 'EQN' FIELD w_saida-equnr.
        CALL TRANSACTION 'IE03' AND SKIP FIRST SCREEN.
    ENDCASE.

  ENDMETHOD .                    "handle_double_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: lcl_event         TYPE REF TO lcl_event_handler.

************************************************************************
* INICIO PROG
************************************************************************
START-OF-SELECTION.

*-Selecao dados
  PERFORM f_selecao_dados.

  IF t_viaufk[] IS INITIAL.
    MESSAGE s000(z_les) WITH text-021.
    STOP.
  ENDIF.

*-Monta saida
  PERFORM f_monta_saida.

  IF t_saida[] IS INITIAL.
    MESSAGE s000(z_les) WITH text-021.
    STOP.
  ENDIF.

  CALL SCREEN 0100.

************************************************************************
* selecao dados
************************************************************************
FORM f_selecao_dados.

*-monta range
  FREE: r_txt04,
        r_stat.

  CHECK p_libe = abap_true OR
        p_ence = abap_true.

  IF p_libe = abap_true.
    r_txt04-sign   = 'I'.
    r_txt04-option = 'EQ'.
    r_txt04-low    = 'LIB'.
    APPEND r_txt04.
  ENDIF.

  IF p_ence = abap_true.
    r_txt04-sign   = 'I'.
    r_txt04-option = 'EQ'.
    r_txt04-low    = 'ENCE'.
    APPEND r_txt04.
  ENDIF.

*-status
  SELECT *
    FROM tj02t
    INTO TABLE t_tj02t
   WHERE txt04 IN r_txt04
     AND spras  = sy-langu.

  LOOP AT t_tj02t INTO w_tj02t.
    r_stat-sign   = 'I'.
    r_stat-option = 'EQ'.
    r_stat-low    = w_tj02t-istat.
    APPEND r_stat.
  ENDLOOP.

*-SELECAO viaufk
  SELECT viaufkst~aufpl  viaufkst~iwerk  viaufkst~auart
         viaufkst~aufnr  viaufkst~tplnr
         viaufkst~equnr  viaufkst~ktext
         viaufkst~vaplz  viaufkst~objnr
         viaufkst~ilart  viaufkst~qmnum
         viaufkst~gstrp  viaufkst~gltrp
    FROM viaufkst
    INNER JOIN jest ON jest~objnr = viaufkst~objnr
                   AND jest~inact = abap_false
    INTO TABLE t_viaufk
   WHERE viaufkst~aufnr IN s_aufnr
     AND viaufkst~auart IN s_auart
     AND viaufkst~tplnr IN s_tplnr
     AND viaufkst~equnr IN s_equnr
     AND viaufkst~kostl IN s_kostl
     AND viaufkst~iwerk IN s_iwerk
     AND viaufkst~erdat IN s_erdat
     AND jest~stat      IN r_stat.

  CHECK t_viaufk[] IS NOT INITIAL.

*-operacaom ordem
  SELECT aufpl aplzl objnr
    FROM afvc
    INTO TABLE t_afvc
     FOR ALL ENTRIES IN t_viaufk
   WHERE aufpl = t_viaufk-aufpl.

  IF t_afvc[] IS NOT INITIAL.

*---total de horas previsto
    SELECT aufpl aplzl arbeh arbei ofmnw
      FROM afvv
      INTO TABLE t_afvv
       FOR ALL ENTRIES IN t_afvc
     WHERE aufpl = t_afvc-aufpl
       AND aplzl = t_afvc-aplzl.

*---custo lancado por operacao
    SELECT kokrs belnr buzei objnr wtgbtr
      FROM coep
      INTO TABLE t_coep
       FOR ALL ENTRIES IN t_afvc
     WHERE objnr = t_afvc-objnr
       AND vrgng = 'COIN'.
  ENDIF.

*-total de horas realiazado
  SELECT rueck rmzhl aufnr ismne ismnw pernr
    FROM afru
    INTO TABLE t_afru
     FOR ALL ENTRIES IN t_viaufk
   WHERE aufnr = t_viaufk-aufnr.

ENDFORM.

************************************************************************
* montar relatorio
************************************************************************
FORM f_monta_saida.

  LOOP AT t_viaufk INTO w_viaufk.

    CLEAR w_saida.

    w_saida-aufnr  = w_viaufk-aufnr.
    w_saida-tplnr  = w_viaufk-tplnr.
    w_saida-equnr  = w_viaufk-equnr.
    w_saida-ktext  = w_viaufk-ktext.
    w_saida-vaplz  = w_viaufk-vaplz.
    w_saida-auart  = w_viaufk-auart.
    w_saida-ilart  = w_viaufk-ilart.
    w_saida-qmnum  = w_viaufk-qmnum.
    w_saida-gstri  = w_viaufk-gstrp.
    w_saida-gltri  = w_viaufk-gltrp.
    w_saida-arbeh  = 'H'.
    w_saida-ismne  = 'H'.

    LOOP AT t_afvc INTO w_afvc WHERE aufpl = w_viaufk-aufpl.
      LOOP AT t_afvv INTO w_afvv WHERE aufpl = w_afvc-aufpl
                                   AND aplzl = w_afvc-aplzl.
        IF w_afvv-arbeh = 'MIN'.
          l_arbei  = w_afvv-ofmnw / 60.
        ELSE.
          l_arbei  = w_afvv-ofmnw.
        ENDIF.
        w_saida-arbei  = w_saida-arbei + l_arbei.
      ENDLOOP.

      LOOP AT t_coep INTO w_coep WHERE objnr = w_afvc-objnr.
        w_saida-wtgbtr = w_saida-wtgbtr + w_coep-wtgbtr.
      ENDLOOP.
    ENDLOOP.

    FREE: t_func, l_tot_func.

    LOOP AT t_afru   INTO w_afru WHERE aufnr = w_viaufk-aufnr.
      IF w_afru-ismne = 'MIN'.
        l_ismnw    = w_afru-ismnw / 60.
      ELSE.
        l_ismnw    = w_afru-ismnw.
      ENDIF.
      w_saida-ismnw     = w_saida-ismnw + l_ismnw.

      w_func-pernr      = w_afru-pernr.
      COLLECT w_func INTO t_func.
    ENDLOOP.

    DESCRIBE TABLE t_func LINES l_tot_func.

    w_saida-pernr_tot = l_tot_func.

    APPEND w_saida  TO t_saida.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS0100'.
  SET TITLEBAR 'TITLE0100'.

  PERFORM f_exibe_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
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

  FREE: t_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZPME0063'
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = t_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT t_fcat INTO w_fcat.
    l_tabix = sy-tabix.

    CASE w_fcat-fieldname.
      WHEN 'AUFNR'.
        w_fcat-coltext   = 'Ordem'.
        w_fcat-outputlen = 15.
        w_fcat-hotspot   = abap_true.
      WHEN 'EQUNR'.
        w_fcat-hotspot   = abap_true.
      WHEN 'KTEXT'.
        w_fcat-coltext   = 'Descrição Ordem'.
        w_fcat-outputlen = 40.
      WHEN 'VAPLZ'.
        w_fcat-coltext   = 'Centr.Trabalho'.
        w_fcat-outputlen = 20.
      WHEN 'ARBEI'.
        w_fcat-coltext   = 'Total Horas Planejadas'.
        w_fcat-outputlen = 20.
      WHEN 'ARBEH'.
        w_fcat-coltext   = 'UNID'.
        w_fcat-outputlen = 10.
      WHEN 'ISMNW'.
        w_fcat-coltext   = 'Total Horas Apontadas'.
        w_fcat-outputlen = 20.
      WHEN 'ISMNE'.
        w_fcat-coltext   = 'UNID'.
        w_fcat-outputlen = 10.
      WHEN 'PERNR_TOT'.
        w_fcat-coltext   = 'Total Funcionários'.
        w_fcat-outputlen = 20.
      WHEN 'WTGBTR'.
        w_fcat-coltext   = 'Custo Total Real'.
        w_fcat-outputlen = 20.
      WHEN 'AUART'.
        w_fcat-coltext   = 'Tipo de Ordem'.
        w_fcat-outputlen = 15.
      WHEN 'ILART'.
        w_fcat-coltext   = 'Tipo de Atividade'.
        w_fcat-outputlen = 10.
      WHEN 'QMNUM'.
        w_fcat-coltext   = 'Nro.da Nota'.
        w_fcat-outputlen = 15.
      WHEN 'GSTRI'.
        w_fcat-coltext   = 'Inicio Atividade'.
        w_fcat-outputlen = 15.
      WHEN 'GLTRI'.
        w_fcat-coltext   = 'Fim Atividade'.
        w_fcat-outputlen = 15.
    ENDCASE.
    MODIFY t_fcat FROM w_fcat INDEX l_tabix.
  ENDLOOP.

  gs_layout-zebra = abap_true.       "Código Zebrado
  gs_layout-no_rowmark = abap_true. "Exclui barra standard de flag a esquerda
  gs_layout-cwidth_opt = abap_false. "Ajusta tamanho na coluna
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
*   SET HANDLER LCL_EVENT-> FOR CTL_ALV.
    SET HANDLER lcl_event->handle_hotspot_click FOR ctl_alv.
  ENDIF.

  CALL METHOD ctl_alv->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout
      i_save          = 'A'
    CHANGING
      it_fieldcatalog = t_fcat
      it_outtab       = t_saida.

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

  p_text = text-002.

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

  CONCATENATE 'Centro:' s_iwerk-low INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  "------------------
  LOOP AT s_erdat.
    IF s_erdat-option EQ 'BT'.
      CONCATENATE s_erdat-low+6(2)  '.' s_erdat-low+4(2)  '.' s_erdat-low(4)  INTO vl_dates1.
      CONCATENATE s_erdat-high+6(2) '.' s_erdat-high+4(2) '.' s_erdat-high(4) INTO vl_dates2.
      CONCATENATE 'Período:' vl_dates1 '-' vl_dates2 INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
      EXIT.
    ELSE.
      CONCATENATE s_erdat-low+6(2) '.' s_erdat-low+4(2) '.' s_erdat-low(4) INTO vl_dates1.
      CONCATENATE 'Período:' vl_dates1 INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.
    CLEAR: sdydo_text_element.
  ENDLOOP.

ENDFORM.
