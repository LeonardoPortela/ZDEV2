*--------------------------------------------------------------------------------------------------------*
*&                          AMAGGI                                                                       *
*& Elaboração de Batch input para criar planos de manutenção atraves da transação IP41.                  *
*--------------------------------------------------------------------------------------------------------*
*& REPORT ZPMR0038.                                                                                      *
*& Data           : 01/11/2018                                                                           *
*& Especificado   : Anderson Oenning                                                                     *
*& Desenvolvimento: Anderson Oenning                                                                     *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                                                  *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*&             |            |               |                                                            *
*--------------------------------------------------------------------------------------------------------*

REPORT zpmr0038 MESSAGE-ID zpmmsg.

TABLES: mpla, mpos, mmpt, equi, zpmt005, rmipm .


TYPES: BEGIN OF ty_status_bapis,
         txt_status TYPE char100,
       END OF ty_status_bapis.

TYPES: BEGIN OF ty_mhis.
         INCLUDE STRUCTURE mhis.
         INCLUDE STRUCTURE mhis_addition.
TYPES: END   OF ty_mhis.

TYPES: BEGIN OF ty_mpos.
         INCLUDE STRUCTURE mpos.
         INCLUDE STRUCTURE mpos_addition.
TYPES: END OF ty_mpos.

DATA: BEGIN OF wa_impt.
        INCLUDE STRUCTURE impt.
DATA: END OF wa_impt.

DATA: BEGIN OF wa_imrg.
        INCLUDE STRUCTURE imrg.
DATA: END OF wa_imrg.

TYPES: BEGIN OF ty_mpla.
         INCLUDE STRUCTURE mpla.
         INCLUDE STRUCTURE mpla_addition.
TYPES: END OF ty_mpla.

TYPES: BEGIN OF ty_mmpt.
         INCLUDE STRUCTURE mmpt.
         INCLUDE STRUCTURE mmpt_addition.
TYPES: END OF ty_mmpt.

*DATA:
*  R_VALUE TYPE RLGRAP-FILENAME,
*  INPUT   TYPE CHAR128.

TYPES: BEGIN OF ty_zpmmpla.
         INCLUDE TYPE zpmmpla.
TYPES:   marc         TYPE c,
         pos_tot_cont TYPE char10,
         pos_tot      TYPE p DECIMALS 2,
         pos_inic     TYPE p DECIMALS 2,
       END OF ty_zpmmpla.

TYPES: BEGIN OF ty_ip10,
         warpl  TYPE mpla-warpl,
         szaeh  TYPE mpla-szaeh,
         status TYPE char5,
       END OF ty_ip10.

TYPES: BEGIN OF ty_change,
         marc         TYPE c,
         status       TYPE char5,
         tstatus      TYPE ztexstatus,
         warpl        TYPE mpos-warpl,
         wptxt        TYPE mpla-wptxt,
         pstxt        TYPE mpos-pstxt,
         zykl1        TYPE zpmmpla-zykl1, "mmpt-zykl1,
         zeieh        TYPE mmpt-zeieh,
         pak_text     TYPE mmpt-pak_text,
         equnr        TYPE mpos-equnr,
         bautl        TYPE mpos-bautl,
         iwerk        TYPE mpos-iwerk,
         auart        TYPE mpos-auart,
         gewrk        TYPE rmipm-gewerk,
         wpgrp        TYPE mpos-wpgrp,
         ilart        TYPE mpos-ilart,
         gsber        TYPE mpos-gsber,
         priok        TYPE mpos-priok,
         plnty        TYPE mpos-plnty,
         plnnr        TYPE mpos-plnnr,
         plnal        TYPE mpos-plnal,
         vspos        TYPE mpla-vspos,
         topos        TYPE mpla-topos,
         vsneg        TYPE mpla-vsneg,
         toneg        TYPE mpla-toneg,
         horiz        TYPE mpla-horiz,
*---> CS1097277 / IR138862 ---->
         call_confirm TYPE mpla-call_confirm,
*<--- CS1097277 / IR138862 <----
       END OF ty_change.

DATA:
  gt_ip10                 TYPE TABLE OF ty_ip10 WITH HEADER LINE,
  gw_ip10                 TYPE ty_ip10,
  gt_change               TYPE TABLE OF ty_change WITH HEADER LINE,
  gw_change               TYPE ty_change,
  it_import               TYPE TABLE OF ty_zpmmpla WITH HEADER LINE,
  it_status               TYPE TABLE OF ty_zpmmpla WITH HEADER LINE,
  it_status2              TYPE TABLE OF ty_change  WITH HEADER LINE,
  it_sel_pl               TYPE TABLE OF ty_zpmmpla WITH HEADER LINE,
  it_sel_p2               TYPE TABLE OF ty_change  WITH HEADER LINE,
  it_ip10                 TYPE TABLE OF ty_zpmmpla WITH HEADER LINE,
  p_ip10                  TYPE  char5,
  p_change                TYPE  char5,
  p_novo                  TYPE  char5,
  wa_import               TYPE zpmmpla,
  gt_diimpt               TYPE TABLE OF diimpt WITH HEADER LINE,
  p_text                  TYPE sdydo_text_element,
  sdydo_text_element(255),
  p_text_table            TYPE sdydo_text_table,
  point_txt(40)           TYPE c,
  no_beleg                LIKE sy-subrc,
  fltp_char               TYPE imrc_totac,
  l_row                   TYPE kcd_ex_row_n,
  l_first                 TYPE c,
  l_plnal(2)              TYPE n,
  cc_x                    TYPE c VALUE 'X',
  w_mpla                  TYPE mpla,
  w_zpmt005               TYPE zpmt005,
  wa_return               TYPE bapiret2.

TYPES: BEGIN OF ty_ucomm,
         ucomm TYPE  sy-ucomm,
       END OF ty_ucomm.

DATA:return_code TYPE          sy-subrc.
DATA: it_ucomm TYPE TABLE OF ty_ucomm.

DATA: BEGIN OF ijstat OCCURS   0.
        INCLUDE STRUCTURE jstat.
DATA: END OF ijstat.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA: p_resp, check, p_erro(1).
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata,
      wa_bdcdata LIKE LINE OF ti_bdcdata.

*&---------------------------------------------------------------------*
*& TABELAS-INTERNAS
*&---------------------------------------------------------------------*

DATA:
  it_system_status TYPE STANDARD TABLE OF bapi_itob_status,
  it_user_status   TYPE STANDARD TABLE OF bapi_itob_status,
  it_status_bapis  TYPE TABLE OF ty_status_bapis.


DATA:
  it_imhis         TYPE ty_mhis OCCURS 0 WITH HEADER LINE,
  it_impos         TYPE ty_mpos OCCURS 0 WITH HEADER LINE,
  it_impla         TYPE ty_mpla OCCURS 0 WITH HEADER LINE,
  it_immpt         TYPE ty_mmpt OCCURS 0 WITH HEADER LINE,
  wa_imhis         TYPE ty_mhis,
  wa_impos         TYPE ty_mpos,
  wa_impla         TYPE ty_mpla,
  wa_immpt         TYPE ty_mmpt,
  wa_system_status TYPE bapi_itob_status,
  wa_status_bapis  TYPE ty_status_bapis.


TYPES: BEGIN OF ty_alsmex_tabline,
         row   TYPE kcd_ex_row_n,
         col   TYPE kcd_ex_col_n,
         value TYPE char8000,
       END OF ty_alsmex_tabline.

TYPES: BEGIN OF ty_s_senderline,
         line(5120) TYPE c,
       END OF ty_s_senderline,
       ty_t_sender TYPE ty_s_senderline.

DATA: it_dados TYPE TABLE OF char8000.

DATA: excel_tab    TYPE TABLE OF ty_t_sender.

DATA:
  it_index TYPE lvc_t_row,
  it_excel TYPE TABLE OF ty_alsmex_tabline,
  it_aux   TYPE TABLE OF ty_alsmex_tabline WITH DEFAULT KEY,
  cont_col TYPE kcd_ex_col_n,
  t_excel  TYPE hrcnex_tab.


DATA: ld_separator TYPE c,
      application  TYPE ole2_object,
      workbook     TYPE ole2_object,
      range        TYPE ole2_object,
      worksheet    TYPE ole2_object,
      h_cell       TYPE ole2_object,
      h_cell1      TYPE ole2_object,
      ld_rc        TYPE i.


DEFINE m_message.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    WHEN OTHERS.
  ENDCASE.
END-OF-DEFINITION.


*Estruturas para ALV:
*=================================================================================
DATA:
  "Informações para 1º ALV.
  g_container      TYPE REF TO cl_gui_custom_container, "Conteiner ALV
  ctl_alv          TYPE REF TO cl_gui_alv_grid,
  g_container_2    TYPE REF TO cl_gui_custom_container, "Conteiner ALV
  ctl_alv_2        TYPE REF TO cl_gui_alv_grid,
  obj_custom_0110  TYPE REF TO cl_gui_custom_container,
  obj_custom_0120  TYPE REF TO cl_gui_custom_container,
  obj_custom_0130  TYPE REF TO cl_gui_custom_container,
  obj_alv_0110     TYPE REF TO cl_gui_alv_grid,
  obj_alv_0120     TYPE REF TO cl_gui_alv_grid,
  obj_alv_0130     TYPE REF TO cl_gui_alv_grid,

  gs_variant       TYPE disvariant,
  it_exclude_fcode TYPE ui_functions,
  wa_exclude_fcode LIKE LINE OF it_exclude_fcode,
  gs_layout        TYPE lvc_s_layo, "Layout da ALV
  it_fieldcatalog  TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas
  wa_fieldcatalog  TYPE lvc_s_fcat, "Controle VLA: catálogo de campos
  it_sort          TYPE lvc_t_sort,
  ls_stable        TYPE lvc_s_stbl,
  ls_stable2       TYPE lvc_s_stbl.


*-------------------------------------------------------------------------------------------
* Paramentro de seleção.
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS: P_MPTYP FOR MPLA-MPTYP,
*                P_WARPL FOR MPLA-WARPL OBLIGATORY.
*SELECTION-SCREEN: END OF BLOCK B1.


CLASS zcl_instrucao DEFINITION.

  PUBLIC SECTION.
    METHODS:

      get_excel
        RETURNING VALUE(r_value) TYPE rlgrap-filename,

      set_excel
        IMPORTING input TYPE char128,
      exc_excel,

      col RETURNING VALUE(r_value) TYPE kcd_ex_col_n.
ENDCLASS.

DATA(obj_inst) = NEW zcl_instrucao( ).

CLASS zcl_instrucao IMPLEMENTATION.

  METHOD get_excel.

    DATA: it_file TYPE filetable,
          l_subrc TYPE i.

    cl_gui_frontend_services=>file_open_dialog(  EXPORTING default_filename = ' '
                                                           file_filter      = '*.xls'
                                                 CHANGING  file_table       = it_file
                                                           rc               = l_subrc
                                               ).
    TRY .
        r_value = it_file[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.
  ENDMETHOD.

  METHOD set_excel.

    CHECK input IS NOT INITIAL.

    FREE: it_excel[], excel_tab[].

    CLASS cl_abap_char_utilities DEFINITION LOAD.

    ld_separator = cl_abap_char_utilities=>horizontal_tab.

    IF application-header = space OR application-handle = -1.
      CREATE OBJECT application 'Excel.Application'.
      m_message.
    ENDIF.

    CALL METHOD OF application 'Workbooks' = workbook.
    m_message.
    CALL METHOD OF workbook 'Open' EXPORTING #1 = input.
    m_message.
    GET PROPERTY OF  application 'ACTIVESHEET' = worksheet.
    m_message.
    CALL METHOD OF worksheet 'Cells' = h_cell
        EXPORTING #1 = 1 #2 = 1.
    m_message.
    CALL METHOD OF worksheet 'Cells' = h_cell1
        EXPORTING #1 = 10000 #2 = 38.
    m_message.

    CALL METHOD  OF worksheet 'RANGE' = range
                   EXPORTING #1 = h_cell #2 = h_cell1.
    m_message.
    CALL METHOD OF range 'SELECT'.
    m_message.
    CALL METHOD OF range 'COPY'.
    m_message.

    cl_gui_frontend_services=>clipboard_import( IMPORTING data = excel_tab ).
    IF sy-subrc <> 0.
      MESSAGE a037(alsmex).
    ENDIF.

    LOOP AT excel_tab INTO DATA(wa).

      DATA(cont) = sy-tabix.
      SPLIT wa AT ld_separator INTO TABLE it_dados.
      it_aux = VALUE #( FOR ls IN it_dados (
                         row   = cont
                         col   = col( )
                         value = ls
                        ) ).
      CLEAR cont_col.
      APPEND LINES OF it_aux TO it_excel.
    ENDLOOP.

    FREE excel_tab.
    cl_gui_frontend_services=>clipboard_export(  IMPORTING data = excel_tab
                                                 CHANGING  rc   = ld_rc
                                               ).

    CALL METHOD OF application 'QUIT'.
    m_message.

    FREE OBJECT h_cell.       m_message.
    FREE OBJECT h_cell1.      m_message.
    FREE OBJECT range.        m_message.
    FREE OBJECT worksheet.    m_message.
    FREE OBJECT workbook.     m_message.
    FREE OBJECT application.  m_message.

  ENDMETHOD.

  METHOD exc_excel.

    CLEAR: gw_ip10.
    FREE: gt_ip10,
          gt_change.

    IF p_ip10 IS NOT INITIAL.
      FREE: it_import[].

      obj_inst->set_excel( obj_inst->get_excel( ) ).

      LOOP AT it_excel INTO DATA(wa_excel).
        CHECK wa_excel-row NE 1 AND wa_excel-row NE 2.
        CASE wa_excel-col.
          WHEN 1.   gw_ip10-warpl        = wa_excel-value.
          WHEN 2.   gw_ip10-szaeh        = wa_excel-value.
*          WHEN 3.
        ENDCASE.

        AT END OF row.
          APPEND gw_ip10 TO gt_ip10.
          CLEAR: gw_ip10, wa_excel.
        ENDAT.

      ENDLOOP.

    ELSEIF p_change IS NOT INITIAL.
      FREE: it_import[].

      obj_inst->set_excel( obj_inst->get_excel( ) ).

      SORT it_excel BY row col.
      CLEAR: gw_change.
      LOOP AT it_excel INTO DATA(wa_excel2).
        CHECK wa_excel2-row NE 1 AND wa_excel2-row NE 1.
        CASE wa_excel2-col.
          WHEN 1.   gw_change-warpl      = wa_excel2-value.
          WHEN 2.   gw_change-wptxt      = wa_excel2-value.
          WHEN 3.   gw_change-pstxt      = wa_excel2-value.
          WHEN 4.   gw_change-zykl1      = wa_excel2-value.
          WHEN 5.   gw_change-zeieh      = wa_excel2-value.
          WHEN 6.   gw_change-pak_text   = wa_excel2-value.
          WHEN 7.   gw_change-equnr      = wa_excel2-value.
          WHEN 8.   gw_change-bautl      = wa_excel2-value.
          WHEN 9.   gw_change-iwerk      = wa_excel2-value.
          WHEN 10.  gw_change-auart      = wa_excel2-value.
          WHEN 11.  gw_change-gewrk      = wa_excel2-value.
          WHEN 12.  gw_change-wpgrp      = wa_excel2-value.
          WHEN 13.  gw_change-ilart      = wa_excel2-value.
          WHEN 14.  gw_change-gsber      = wa_excel2-value.
          WHEN 15.  gw_change-priok      = wa_excel2-value.
          WHEN 16.  gw_change-plnty      = wa_excel2-value.
          WHEN 17.  gw_change-plnnr      = wa_excel2-value.
          WHEN 18.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_excel2-value
              IMPORTING
                output = gw_change-plnal.

*          WHEN 17.
**           gw_change-plnal      = |{ wa_excel2-value ALPHA = OUT }|.
*            gw_change-plnal      = wa_excel2-value.

          WHEN 19.  gw_change-vspos      = wa_excel2-value.
          WHEN 20.  gw_change-topos      = wa_excel2-value.
          WHEN 21.  gw_change-vsneg      = wa_excel2-value.
          WHEN 22.  gw_change-toneg      = wa_excel2-value.
          WHEN 23.  gw_change-horiz      = wa_excel2-value.
*---> CS1097277 / IR138862 ---->
          WHEN 24.  gw_change-call_confirm = wa_excel2-value.
*<--- CS1097277 / IR138862 <----
*          WHEN 23.  APPEND gw_change TO gt_change.
        ENDCASE.

        AT END OF row.
          APPEND gw_change              TO gt_change.
          CLEAR: gw_change, wa_excel2.
        ENDAT.
      ENDLOOP.

    ELSE.

      FREE: it_import[].

      obj_inst->set_excel( obj_inst->get_excel( ) ).

      FREE: it_import.
      CLEAR: wa_excel, wa_import.
      LOOP AT it_excel INTO wa_excel.
        CHECK wa_excel-row NE 1 AND wa_excel-row NE 2.
        CASE wa_excel-col.
          WHEN 1.   wa_import-mptyp        = wa_excel-value.
          WHEN 2.   wa_import-wptxt        = wa_excel-value.
          WHEN 3.   wa_import-zykl1        = wa_excel-value.
          WHEN 4.   wa_import-zeieh        = wa_excel-value.
          WHEN 5.   wa_import-pak_text     = wa_excel-value.
          WHEN 6.   wa_import-point        = wa_excel-value.
          WHEN 7.   wa_import-tplnr        = wa_excel-value.
          WHEN 8.   wa_import-equnr        = wa_excel-value.
          WHEN 9.   wa_import-bautl        = wa_excel-value.
          WHEN 10.  wa_import-iwerk        = wa_excel-value.
          WHEN 11.  wa_import-auart        = wa_excel-value.
          WHEN 12.  wa_import-gewerk       = wa_excel-value.
          WHEN 13.  wa_import-wergw        = wa_excel-value.
          WHEN 14.  wa_import-wpgrp        = wa_excel-value.
          WHEN 15.  wa_import-ilart        = wa_excel-value.
          WHEN 16.  wa_import-gsber        = wa_excel-value.
          WHEN 17.  wa_import-priok        = wa_excel-value.
          WHEN 18.  wa_import-plnty        = wa_excel-value.
          WHEN 19.  wa_import-plnnr        = wa_excel-value.
          WHEN 20.  wa_import-plnal        = wa_excel-value.
          WHEN 21.  wa_import-vspos        = wa_excel-value.
          WHEN 22.  wa_import-topos        = wa_excel-value.
          WHEN 23.  wa_import-vsneg        = wa_excel-value.
          WHEN 24.  wa_import-toneg        = wa_excel-value.
          WHEN 25.  wa_import-horiz        = wa_excel-value.
          WHEN 26.  wa_import-abrho        = wa_excel-value.
          WHEN 27.  wa_import-hunit        = wa_excel-value.
          WHEN 28.  wa_import-szaeh        = wa_excel-value.
          WHEN 29.  wa_import-call_confirm = wa_excel-value.
          WHEN 30.  APPEND wa_import TO it_import.
        ENDCASE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD col.
    ADD 1 TO cont_col.
    r_value = cont_col.
  ENDMETHOD.

ENDCLASS.


*-------------------------------------------------------------------------------------------
*Tela de entrada para importação.
START-OF-SELECTION.
  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

*  PERFORM MODIF_SCREEN.

  DATA: p_erro_ TYPE char5.

  CLEAR p_erro_.
  DATA: w_ucomm TYPE sy-ucomm.
  FREE it_ucomm.

  IF p_ip10 IS INITIAL.
    IF it_import[] IS INITIAL AND
       gt_change[] IS INITIAL.

      w_ucomm = 'BNT_EXEC'.
      APPEND w_ucomm TO it_ucomm.
      CLEAR w_ucomm.

      w_ucomm = 'BNT_VERIF'.
      APPEND w_ucomm TO it_ucomm.
      CLEAR w_ucomm.

      w_ucomm = 'BTN_LIMPAR'.
      APPEND w_ucomm TO it_ucomm.
      CLEAR w_ucomm.

      w_ucomm = 'BTN_DEL'.
      APPEND w_ucomm TO it_ucomm.
      CLEAR w_ucomm.

    ELSE.
      w_ucomm = 'BNT_EXCEL'.
      APPEND w_ucomm TO it_ucomm.
      CLEAR w_ucomm.


*      w_ucomm = 'BNT_EXEC'.
*      APPEND w_ucomm TO it_ucomm.
*      CLEAR w_ucomm.
    ENDIF.

    IF it_status[] IS NOT INITIAL.
      LOOP AT it_status.
        IF it_status-status = '@0A@'.
          ADD 1 TO p_erro_.
        ENDIF.
      ENDLOOP.

      IF p_erro_ IS NOT INITIAL.
        APPEND w_ucomm TO it_ucomm.
        CLEAR w_ucomm.
      ELSE.
        DELETE it_ucomm WHERE ucomm = 'BNT_EXEC'.
      ENDIF.
    ENDIF.

    IF it_status2[] IS NOT INITIAL.
      LOOP AT it_status2.
        IF it_status2-status = '@0A@'.
          ADD 1 TO p_erro_.
        ENDIF.
      ENDLOOP.

      IF p_erro_ IS NOT INITIAL.
        APPEND w_ucomm TO it_ucomm.
        CLEAR w_ucomm.
      ELSE.
        DELETE it_ucomm WHERE ucomm = 'BNT_EXEC'.
      ENDIF.
    ENDIF.

  ELSE.
    w_ucomm = 'BNT_VERIF'.
    APPEND w_ucomm TO it_ucomm.
    CLEAR w_ucomm.

    w_ucomm = 'BTN_LIMPAR'.
    APPEND w_ucomm TO it_ucomm.
    CLEAR w_ucomm.

    w_ucomm = 'BTN_DEL'.
    APPEND w_ucomm TO it_ucomm.
    CLEAR w_ucomm.
  ENDIF.


  SET PF-STATUS 'PF-001' EXCLUDING it_ucomm.
  SET TITLEBAR 'T-001'.

  CLEAR: it_fieldcatalog.
  CLEAR: gs_layout.

  IF p_ip10 IS NOT INITIAL.

    PERFORM fill_it_fieldcatalog USING:
       01 'WARPL       '     ' '    '25'  ' '     ' '   ' '   'Plano            '  ''  '' ,
       02 'SZAEH       '     ' '    '70'  ' '     ' '   ' '   'Km/Hr            '  ''  '' ,
       03 'STATUS'     '     '      '30'  ' '     ' '   ' '   'Status               '  ''  '' .

    IF obj_custom_0120 IS INITIAL.

      CREATE OBJECT obj_custom_0110
        EXPORTING
          container_name              = 'CONTAINER'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      CREATE OBJECT obj_alv_0120
        EXPORTING
          i_parent = obj_custom_0120.

    ENDIF.

    CALL METHOD obj_alv_0120->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = gt_ip10[]
        it_sort              = it_sort.

  ELSEIF p_change IS NOT INITIAL.
    PERFORM fill_it_fieldcatalog USING:
        01 'STATUS'     '     '      '30'  ' '     ' '   ' '   'Status               '  ''  '' ,
        02 'TSTATUS'    '     '      '40'  ' '     ' '   ' '   'Texto do status      '  ''  '' ,
        03 'WARPL       '     ' '    '35'  ' '     ' '   ' '   'Plano               '  ''  '' ,
        04 'WPTXT       '     ' '    '40'  ' '     ' '   ' '   'Descrição Plano     '  ''  '' ,
        05 'PSTXT       '     ' '    '40'  ' '     ' '   ' '   'Descrição item      '  ''  '' ,
        06 'ZYKL1       '     ' '    '20'  ' '     ' '   ' '   'Ciclo Manut.        '  ''  '' ,
        07 'ZEIEH       '     ' '    '20'  ' '     ' '   ' '   'Unidade             '  ''  '' ,
        08 'PAK_TEXT    '     ' '    '35'  ' '     ' '   ' '   'Texto ciclo         '  ''  '' ,
        09 'EQUNR       '     ' '    '35'  ' '     ' '   ' '   'Equipamento         '  ''  '' ,
        10 'BAUTL       '     ' '    '35'  ' '     ' '   ' '   'Conjunto            '  ''  '' ,
        11 'IWERK       '     ' '    '35'  ' '     ' '   ' '   'Centro Planej       '  ''  '' ,
        12 'AUART       '     ' '    '35'  ' '     ' '   ' '   'Tp.Ordem            '  ''  '' ,
        13 'GEWERK      '     ' '    '35'  ' '     ' '   ' '   'Centro Trabalho     '  ''  '' ,
        14 'WPGRP       '     ' '    '35'  ' '     ' '   ' '   'Grp.Planej          '  ''  '' ,
        15 'ILART       '     ' '    '35'  ' '     ' '   ' '   'Tp.Atv              '  ''  '' ,
        16 'GSBER       '     ' '    '35'  ' '     ' '   ' '   'Centro              '  ''  '' ,
        17 'PRIOK       '     ' '    '35'  ' '     ' '   ' '   'Prioridade          '  ''  '' ,
        18 'PLNTY       '     ' '    '35'  ' '     ' '   ' '   'Tipo lista          '  ''  '' ,
        19 'PLNNR       '     ' '    '35'  ' '     ' '   ' '   'Chave grupo         '  ''  '' ,
        20 'PLNAL       '     ' '    '35'  ' '     ' '   ' '   'Numerador           '  ''  '' ,
        21 'VSPOS       '     ' '    '35'  ' '     ' '   ' '   'FD Atras            '  ''  '' ,
        22 'TOPOS       '     ' '    '35'  ' '     ' '   ' '   'Tolerância atras    '  ''  '' ,
        23 'VSNEG       '     ' '    '35'  ' '     ' '   ' '   'FD Antec            '  ''  '' ,
        24 'TONEG       '     ' '    '35'  ' '     ' '   ' '   'Tolerância antec    '  ''  '' ,
        25 'HORIZ       '     ' '    '35'  ' '     ' '   ' '   'Horiz Abertura      '  ''  '' ,
*---> CS1097277 / IR138862 --->
        26 'CALL_CONFIRM       '     ' '    '35'  ' '     ' '   ' '   'Conf.Obrigat.      '  ''  '' .
*<--- CS1097277 / IR138862 <---

    IF obj_custom_0120 IS INITIAL.
      CREATE OBJECT obj_custom_0130
        EXPORTING
          container_name              = 'CONTAINER'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      CREATE OBJECT obj_alv_0120
        EXPORTING
          i_parent = obj_custom_0120.
    ENDIF.

    CALL METHOD obj_alv_0120->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = gt_change[]
        it_sort              = it_sort.

  ELSE.
    PERFORM fill_it_fieldcatalog USING:
        01 'MPTYP       '     ' '    '25'  ' '     ' '   ' '   'Ctg.plano            '  ''  '' ,
        06 'WPTXT       '     ' '    '70'  ' '     ' '   ' '   'Descrição do plano de manutneção     '  ''  '' ,
        07 'ZYKL1       '     ' '    '20'  ' '     ' '   ' '   'Ciclo                '  ''  '' ,
        08 'ZEIEH       '     ' '    '30'  ' '     ' '   ' '   'Und medida           '  ''  '' ,
        09 'PAK_TEXT    '     ' '    '40'  ' '     ' '   ' '   'Txt p/ pacote        '  ''  '' ,
        10 'POINT       '     ' '    '30'  ' '     ' '   ' '   'P medição            '  ''  '' ,
        11 'TPLNR       '     ' '    '40'  ' '     ' '   ' '   'Local Instalação     '  ''  '' ,
        12 'EQUNR       '     ' '    '30'  ' '     ' '   ' '   'Equipamento          '  ''  '' ,
        13 'BAUTL       '     ' '    '30'  ' '     ' '   ' '   'Conjunto             '  ''  '' ,
        14 'IWERK       '     ' '    '30'  ' '     ' '   ' '   'Centro               '  ''  '' ,
        15 'AUART       '     ' '    '30'  ' '     ' '   ' '   'T. Ordem             '  ''  '' ,
        16 'GEWERK      '     ' '    '30'  ' '     ' '   ' '   'Ctro Trab            '  ''  '' ,
        17 'WERGW       '     ' '    '30'  ' '     ' '   ' '   'C. relativo          '  ''  '' ,
        18 'WPGRP       '     ' '    '30'  ' '     ' '   ' '   'Grp planj            '  ''  '' ,
        19 'ILART       '     ' '    '25'  ' '     ' '   ' '   'T. atividade         '  ''  '' ,
        20 'GSBER       '     ' '    '25'  ' '     ' '   ' '   'Divisão              '  ''  '' ,
        21 'PRIOK       '     ' '    '30'  ' '     ' '   ' '   'Prioridade           '  ''  '' ,
        22 'PLNTY       '     ' '    '30'  ' '     ' '   ' '   'T. roteiro           '  ''  '' ,
        23 'PLNNR       '     ' '    '30'  ' '     ' '   ' '   'Chv Grupo            '  ''  '' ,
        24 'PLNAL       '     ' '    '30'  ' '     ' '   ' '   'N.grup               '  ''  '' ,
        25 'VSPOS       '     ' '    '30'  ' '     ' '   ' '   'Ft cf atrasada       '  ''  '' ,
        26 'TOPOS       '     ' '    '30'  ' '     ' '   ' '   'T. cf atrasada       '  ''  '' ,
        27 'VSNEG       '     ' '    '30'  ' '     ' '   ' '   'Ft cf antecipada     '  ''  '' ,
        28 'TONEG       '     ' '    '30'  ' '     ' '   ' '   'T cf antecipada      '  ''  '' ,
        29 'HORIZ       '     ' '    '30'  ' '     ' '   ' '   'Hz abertura          '  ''  '' ,
        30 'ABRHO       '     ' '    '30'  ' '     ' '   ' '   'Int solic            '  ''  '' ,
        31 'HUNIT       '     ' '    '30'  ' '     ' '   ' '   'Unid                 '  ''  '' ,
        32 'SZAEH       '     ' '    '30'  ' '     ' '   ' '   'Dta/h/km inicio      '  ''  '' ,
        33 'CALL_CONFIRM'    ' '     '30'  ' '     ' '   ' '   'Cf. obrigatório      '  ''  '' ,
        04 'WARPL'      '     '      '30'  ' '     ' '   ' '   'Nº do plano          '  ''  '' ,
        05 'IP10 '      '     '      '10'  ' '     ' '   ' '   'Exec IP10            '  ''  '' ,
        02 'STATUS'     '     '      '30'  ' '     ' '   ' '   'Status               '  ''  '' ,
        03 'TSTATUS'    '     '      '40'  ' '     ' '   ' '   'Texto do status      '  ''  '' ,
        34 'UNAME  '    '     '      '30'  ' '     ' '   ' '   'Nome do usuário      '  ''  '' ,
        35 'ERDAT  '    '     '      '40'  ' '     ' '   ' '   'Data de criação      '  ''  '' .


    IF obj_custom_0120 IS INITIAL.
      CREATE OBJECT obj_custom_0120
        EXPORTING
          container_name              = 'CONTAINER'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      CREATE OBJECT obj_alv_0120
        EXPORTING
          i_parent = obj_custom_0120.

*    SET HANDLER: OBJ_EVEN->HANDLE_DOUBLE_CLICK FOR CTL_ALV.
**                OBJ_EVEN->ON_HOTSPOT_CLICK    FOR CTL_ALV.

    ENDIF.

    CALL METHOD obj_alv_0120->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_import[]
        it_sort              = it_sort.


  ENDIF.

  ls_stable-row = 'X'.
  ls_stable-col = 'X'.

  CALL METHOD obj_alv_0120->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BNT_EXCEL'.
*      LV_ACAO = SY-UCOMM.
      obj_inst->exc_excel( ).
    WHEN 'BNT_EXEC'.
      IF p_change = abap_true.
        PERFORM modificar_plan.
      ELSE.
        PERFORM criar_plan.
      ENDIF.
    WHEN 'BNT_VERIF'.
      IF p_ip10 IS INITIAL.
        IF p_change = abap_true.
          PERFORM verificar_change.
        ELSE.
          PERFORM verificar_list.
        ENDIF.
      ENDIF.
    WHEN 'BTN_LIMPAR'.
      PERFORM limpar_planilha.
    WHEN 'BTN_DEL'.
      PERFORM excluir_linha.
  ENDCASE.
ENDMODULE.


FORM fill_it_fieldcatalog  USING    VALUE(p_colnum)
                                    VALUE(p_fieldname)
                                    VALUE(p_tabname)
                                    VALUE(p_len)
                                    VALUE(p_edit)
                                    VALUE(p_icon)
                                    VALUE(p_do_sum)
                                    VALUE(p_header)
                                    VALUE(p_emphasize)
                                    VALUE(p_hotspot).


  DATA:  wa_fieldcatalog  TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos     = p_colnum.
  wa_fieldcatalog-fieldname   = p_fieldname.
  wa_fieldcatalog-tabname     = p_tabname.
  wa_fieldcatalog-outputlen   = p_len.
  wa_fieldcatalog-coltext     = p_header.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-ref_table   = p_tabname.
  wa_fieldcatalog-checktable  = p_tabname.
  wa_fieldcatalog-do_sum      = p_do_sum.
  wa_fieldcatalog-emphasize   = p_emphasize.
  wa_fieldcatalog-hotspot     = p_hotspot.

  gs_layout-ctab_fname    = 'CELL_COLOR'.
  gs_layout-excp_conds    = 'X'.
  gs_layout-zebra         = 'X'.
  gs_layout-sel_mode      = 'A'.
  gs_layout-cwidth_opt    = 'X'.     "  Otimizar colunas na tela
  gs_layout-totals_bef    = ''.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modificar_plan .
  DATA: p_resp,
        lv_msg   TYPE bapi_msg,
        l_wapos  TYPE mpos-wapos,
        l_nummer TYPE mmpt-nummer.

  DATA: p_resp_,
        lv_msg_        TYPE bapi_msg,
        it_select_rows TYPE lvc_t_row,
        contnum        TYPE p DECIMALS 2,
        wa_select_rows TYPE lvc_s_row.


  CHECK gt_change[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING        "TITLEBAR = 'Confirmar'
      text_question         = 'Deseja realmente alterar plano em massa?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ' '
    IMPORTING
      answer                = p_resp.

  IF p_resp = 2.
    EXIT.
  ENDIF.

* =====================================================================

  MOVE-CORRESPONDING gt_change[] TO it_sel_p2[].

  CLEAR: it_select_rows[], wa_select_rows.
  CALL METHOD obj_alv_0120->get_selected_rows
    IMPORTING
      et_index_rows = it_select_rows.

  IF it_select_rows[] IS NOT INITIAL.
    LOOP AT it_select_rows INTO wa_select_rows.
      LOOP AT it_sel_p2 ASSIGNING FIELD-SYMBOL(<wa_sel_p2>).
        IF sy-tabix = wa_select_rows-index.
          <wa_sel_p2>-marc = 'X'.
        ENDIF.
      ENDLOOP.

      LOOP AT gt_change ASSIGNING FIELD-SYMBOL(<w_sel_p2>).
        IF sy-tabix = wa_select_rows-index.
          <w_sel_p2>-marc = 'X'.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  DELETE it_sel_p2[] WHERE marc NE abap_true.

  LOOP AT gt_change ASSIGNING FIELD-SYMBOL(<wa_change>) WHERE marc  EQ abap_true.

    FREE: it_impos,
          it_impla,
          it_immpt,
          wa_impos,
          wa_impla,
          wa_immpt,
          l_wapos,
          l_nummer.



    SELECT SINGLE aktyp
  FROM t370
  INTO @DATA(ld_x_xaktyp).

    <wa_change>-warpl = |{ <wa_change>-warpl ALPHA = IN }|.
    <wa_change>-equnr = |{ <wa_change>-equnr ALPHA = IN }|.
    SELECT wapos
      INTO l_wapos
      FROM mpos
        UP TO 1 ROWS
      WHERE warpl = <wa_change>-warpl.
    ENDSELECT.
    IF sy-subrc <> 0.
      CLEAR sdydo_text_element.
      CONCATENATE sdydo_text_element'Não encontrou plano manutenção!' INTO sdydo_text_element SEPARATED BY space.
      <wa_change>-tstatus = sdydo_text_element.
      <wa_change>-status = '@0A@'.
      CONTINUE.
    ENDIF.

    SELECT nummer
      INTO l_nummer
      FROM mmpt
        UP TO 1 ROWS
      WHERE warpl = <wa_change>-warpl.
    ENDSELECT.
    IF sy-subrc <> 0.
      CLEAR sdydo_text_element.
      CONCATENATE sdydo_text_element'Não encontrou plano manutenção!' INTO sdydo_text_element SEPARATED BY space.
      <wa_change>-tstatus = sdydo_text_element.
      <wa_change>-status = '@0A@'.
      CONTINUE.
    ENDIF.

    "Verificar se existe equipamento.
    IF <wa_change>-equnr IS NOT INITIAL.
      SELECT SINGLE equnr
      INTO @DATA(equnr)
      FROM eqkt
      WHERE equnr  = @<wa_change>-equnr.
      IF sy-subrc <> 0.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Equipamento não existe!' INTO sdydo_text_element SEPARATED BY space.
        <wa_change>-tstatus = sdydo_text_element.
        <wa_change>-status = '@0A@'.
        CONTINUE.
      ENDIF.
    ENDIF.


*---------------------------------------------------
*---buscar dados para atualizao
*---------------------------------------------------
    SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF wa_impos
    FROM mpos
    WHERE warpl = <wa_change>-warpl.

    IF <wa_change>-pstxt IS NOT INITIAL. MOVE  <wa_change>-pstxt TO wa_impos-pstxt. ENDIF.
    IF <wa_change>-equnr IS NOT INITIAL. MOVE  <wa_change>-equnr TO wa_impos-equnr. ENDIF.
    IF <wa_change>-bautl IS NOT INITIAL. MOVE  <wa_change>-bautl TO wa_impos-bautl. ENDIF.
    CONDENSE wa_impos-bautl.
    IF <wa_change>-iwerk IS NOT INITIAL. MOVE  <wa_change>-iwerk TO wa_impos-iwerk. ENDIF.
    CONDENSE wa_impos-iwerk.
    IF <wa_change>-auart IS NOT INITIAL. MOVE  <wa_change>-auart TO wa_impos-auart. ENDIF.
    CONDENSE wa_impos-auart.

    IF <wa_change>-gewrk IS NOT INITIAL.
      SELECT SINGLE * FROM crhd INTO @DATA(w_crhd) WHERE arbpl EQ @<wa_change>-gewrk AND werks EQ @<wa_change>-iwerk.
      MOVE  w_crhd-objid TO wa_impos-gewrk.
      CONDENSE wa_impos-gewrk.
    ENDIF.


    IF <wa_change>-wpgrp IS NOT INITIAL. MOVE  <wa_change>-wpgrp TO wa_impos-wpgrp. ENDIF.
    CONDENSE wa_impos-wpgrp.
    IF <wa_change>-ilart IS NOT INITIAL. MOVE  <wa_change>-ilart TO wa_impos-ilart. ENDIF.
    CONDENSE wa_impos-ilart.
    IF <wa_change>-gsber IS NOT INITIAL. MOVE  <wa_change>-gsber TO wa_impos-gsber. ENDIF.
    CONDENSE wa_impos-gsber.
    IF <wa_change>-priok IS NOT INITIAL. MOVE  <wa_change>-priok TO wa_impos-priok. ENDIF.
    CONDENSE wa_impos-priok.
    IF <wa_change>-plnty IS NOT INITIAL. MOVE  <wa_change>-plnty TO wa_impos-plnty. ENDIF.
    CONDENSE wa_impos-plnty.
    IF <wa_change>-plnnr IS NOT INITIAL. MOVE  <wa_change>-plnnr TO wa_impos-plnnr. ENDIF.
    CONDENSE wa_impos-plnnr.
    IF <wa_change>-plnal IS NOT INITIAL. MOVE  <wa_change>-plnal TO wa_impos-plnal. ENDIF.
    CONDENSE wa_impos-plnal.

    MOVE sy-datum              TO wa_impos-aedat.
    MOVE sy-uname              TO wa_impos-aenam.
    APPEND wa_impos            TO it_impos.


    SELECT SINGLE *
             INTO CORRESPONDING FIELDS OF wa_impla
             FROM mpla
            WHERE warpl = <wa_change>-warpl.

    IF <wa_change>-wptxt IS NOT INITIAL. MOVE  <wa_change>-wptxt TO wa_impla-wptxt. ENDIF.
    IF <wa_change>-vspos IS NOT INITIAL. MOVE  <wa_change>-vspos TO wa_impla-vspos. ENDIF.
    CONDENSE wa_impla-vspos.
    IF <wa_change>-topos IS NOT INITIAL. MOVE  <wa_change>-topos TO wa_impla-topos. ENDIF.
    CONDENSE wa_impla-topos.
    IF <wa_change>-vsneg IS NOT INITIAL. MOVE  <wa_change>-vsneg TO wa_impla-vsneg. ENDIF.
    CONDENSE wa_impla-vsneg.
    IF <wa_change>-toneg IS NOT INITIAL. MOVE  <wa_change>-toneg TO wa_impla-toneg. ENDIF.
    CONDENSE wa_impla-toneg.
    IF <wa_change>-horiz IS NOT INITIAL. MOVE  <wa_change>-horiz TO wa_impla-horiz. ENDIF.
    CONDENSE wa_impla-horiz.
*---> CS1097277 / IR138862 --->
    IF <wa_change>-call_confirm IS NOT INITIAL.
      IF <wa_change>-call_confirm EQ 'D'.
        MOVE  '' TO wa_impla-call_confirm.
      ELSE.
        MOVE  <wa_change>-call_confirm TO wa_impla-call_confirm.
      ENDIF.
    ENDIF.
    CONDENSE wa_impla-call_confirm.
*<--- CS1097277 / IR138862 <---

    MOVE sy-datum                     TO wa_impla-aedat.
    MOVE sy-uname                     TO wa_impla-aenam.
    APPEND wa_impla                   TO it_impla.

    SELECT SINGLE *
             INTO CORRESPONDING FIELDS OF wa_immpt
             FROM mmpt
            WHERE warpl  = <wa_change>-warpl
              AND nummer = l_nummer.

    DATA: y_masc_symbol__ VALUE '_'.
    DATA: s_recdv  TYPE imrg-recdv, s_cdiff TYPE imrg-cdiff, s_cdiffi TYPE imrg-cdiffi.
    IF <wa_change>-zykl1    IS NOT INITIAL.

      DATA(s_zykl1) = <wa_change>-zykl1.
      CONDENSE s_zykl1.
      CALL FUNCTION 'CHAR_FLTP_CONVERSION_TO_SI'
        EXPORTING
          char_value       = s_zykl1
          char_unit        = wa_immpt-zeieh
          unit_is_optional = ' '
          decimals_max     = '15'
          field_name       = ' '
          masc_symbol      = ' '
        IMPORTING
          fltp_value       = s_recdv  "valid for API!
          fltp_value_si    = s_cdiff  "only for debugging
          indicator_value  = s_cdiffi. "only for debugging

      IF NOT ( s_recdv IS INITIAL ).
        CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
          EXPORTING
            input   = s_recdv
            unit_in = wa_immpt-zeieh
          IMPORTING
            output  = wa_immpt-zykl1.

*        MOVE s_recdv    TO wa_immpt-zykl1.
      ENDIF.
*      CONDENSE <wa_change>-zykl1.
*      MOVE <wa_change>-zykl1    TO wa_immpt-zykl1.
    ENDIF.

*    IF <wa_change>-zykl1    IS NOT INITIAL.  MOVE <wa_change>-zykl1    TO wa_immpt-zykl1. ENDIF.
*    IF <wa_change>-zeieh    IS NOT INITIAL.  MOVE <wa_change>-zeieh    TO wa_immpt-zeieh   . ENDIF.
    IF <wa_change>-pak_text IS NOT INITIAL.  MOVE <wa_change>-pak_text TO wa_immpt-pak_text. ENDIF.
    MOVE 'U'                          TO wa_immpt-upd_knz.
    APPEND wa_immpt                   TO it_immpt.

*    SELECT *
*    INTO CORRESPONDING FIELDS OF TABLE it_imhis
*    FROM mmpt
*    WHERE warpl  = <wa_change>-warpl.


*----------------------------------------------------
*---BAPI de modificação Plano de manutenção.
*----------------------------------------------------
*   Update MPLA, MMPT, MPOS
    CALL FUNCTION 'MAINTENANCE_PLAN_POST' "IN UPDATE TASK
      EXPORTING
        x_xaktyp = 'V'
      TABLES
        imhis    = it_imhis
        impla    = it_impla
        impos    = it_impos
        immpt    = it_immpt.


    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.


    <wa_change>-status = '@08@'.
    CLEAR sdydo_text_element.
    CONCATENATE sdydo_text_element'Alteração efetuada' INTO sdydo_text_element SEPARATED BY space.
    <wa_change>-tstatus = sdydo_text_element.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CRIAR_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM criar_plan .
  DATA: p_resp,
          lv_msg TYPE bapi_msg.

  DATA: p_resp_,
        lv_msg_        TYPE bapi_msg,
        it_select_rows TYPE lvc_t_row,
        contnum        TYPE p DECIMALS 2,
        wa_select_rows TYPE lvc_s_row,
        lv_szaeh       TYPE string.


*  PERFORM VERIFICAR_LIST.
*  DELETE IT_IMPORT WHERE STATUS NE '@08@'.

  CHECK it_import[] IS NOT INITIAL OR gt_ip10[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING        "TITLEBAR = 'Confirmar'
      text_question         = 'Deseja realmente criar plano em massa?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      display_cancel_button = ' '
    IMPORTING
      answer                = p_resp.

  IF p_resp = 2.
    EXIT.
  ENDIF.

* =====================================================================

  MOVE-CORRESPONDING it_import[] TO it_sel_pl[].

  CLEAR: it_select_rows[], wa_select_rows.
  CALL METHOD obj_alv_0120->get_selected_rows
    IMPORTING
      et_index_rows = it_select_rows.

  IF it_select_rows[] IS NOT INITIAL.
    LOOP AT it_select_rows INTO wa_select_rows.
      LOOP AT it_sel_pl ASSIGNING FIELD-SYMBOL(<wa_sel_pl>).
        IF sy-tabix = wa_select_rows-index.
          <wa_sel_pl>-marc = 'X'.
        ENDIF.
      ENDLOOP.

      LOOP AT it_import ASSIGNING FIELD-SYMBOL(<w_sel_pl>).
        IF sy-tabix = wa_select_rows-index.
          <w_sel_pl>-marc = 'X'.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  DELETE it_sel_pl[] WHERE marc NE abap_true.

  IF p_ip10 IS INITIAL.

    IF it_sel_pl[] IS NOT INITIAL.
      LOOP AT it_import ASSIGNING FIELD-SYMBOL(<wa_import>) WHERE marc  EQ abap_true
                                                            AND   warpl EQ ' '.
        CLEAR w_mpla.
        CLEAR w_zpmt005.

        IF <wa_import>-equnr IS NOT INITIAL.
          <wa_import>-equnr = |{ <wa_import>-equnr ALPHA = IN }|.
          SELECT SINGLE *
          FROM zpmt005
          INTO w_zpmt005
            WHERE tplnr EQ <wa_import>-tplnr
             AND  equnr EQ <wa_import>-equnr
             AND  mptyp EQ <wa_import>-mptyp
             AND  iwerk EQ <wa_import>-iwerk
             AND  plnnr EQ <wa_import>-plnnr
             AND  plnal EQ <wa_import>-plnal.
        ELSE.
          SELECT SINGLE *
        FROM zpmt005
        INTO w_zpmt005
          WHERE tplnr EQ <wa_import>-tplnr
           AND  mptyp EQ <wa_import>-mptyp
           AND  iwerk EQ <wa_import>-iwerk
           AND  plnnr EQ <wa_import>-plnnr
           AND  plnal EQ <wa_import>-plnal.
        ENDIF.

        IF w_zpmt005 IS NOT INITIAL.
*        Verificar status do plano.
          w_zpmt005-warpl = |{ w_zpmt005-warpl ALPHA = IN }|.
          SELECT SINGLE *
          FROM mpla
          INTO w_mpla
            WHERE warpl EQ w_zpmt005-warpl.
          PERFORM status_check_plano USING w_mpla-objnr return_code CHANGING ijstat.

          IF ijstat EQ 'I0001'.
            CLEAR sdydo_text_element.
            CONCATENATE sdydo_text_element'Ja existe plano cadastrado' INTO sdydo_text_element SEPARATED BY space.
            <wa_import>-tstatus = sdydo_text_element.
            <wa_import>-warpl = |{ <wa_import>-warpl ALPHA = OUT }|.
            <wa_import>-equnr = |{ <wa_import>-equnr ALPHA = OUT }|.
            <wa_import>-point = |{ <wa_import>-point ALPHA = OUT }|.
            <wa_import>-status = '@0A@'.
            CLEAR w_zpmt005.
            CONTINUE.
          ENDIF.
        ENDIF.

        <wa_import>-point = |{ <wa_import>-point ALPHA = IN }|.
        IF <wa_import>-status NE '@08@'.
          CONTINUE.
        ENDIF.

        FREE ti_bdcdata.
        FREE it_msg.

* Verifica se unidade de medida do plano é por hora/km.
        IF <wa_import>-zeieh EQ 'H' OR <wa_import>-zeieh EQ 'KM'.
          PERFORM f_bdc_data USING:
      'SAPLIWP3' '0100'  'X' '                    '  '                                        ',
      '        ' '0000'  ' ' 'BDC_CURSOR          '  'RMIPM-MPTYP                                                                  ',
      '        ' '0000'  ' ' 'BDC_OKCODE          '  '/00                                                                          ',
      '        ' '0000'  ' ' 'RMIPM-MPTYP         '  <wa_import>-mptyp,
      'SAPLIWP3' '0201'  'X' '                    '  '                                                                             ',
      '        ' '0000'  ' ' 'BDC_OKCODE          '  '/00                                                                          ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'  ' ' 'RMIPM-WPTXT         '  <wa_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'  ' ' 'RMIPM-PSTXT         '  <wa_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'  ' ' 'RIWO1-TPLNR         '  <wa_import>-tplnr,
      '        ' '0000'  ' ' 'RIWO1-EQUNR         '  <wa_import>-equnr,
      '        ' '0000'  ' ' 'RIWO1-BAUTL         '  <wa_import>-bautl,
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
*'        ' '0000'  ' ' 'BDC_CURSOR          '  'RMIPM-PLNAL                                                                  ',
      '        ' '0000'  ' ' 'RMIPM-IWERK         '  <wa_import>-iwerk,
      '        ' '0000'  ' ' 'RMIPM-WPGRP         '  <wa_import>-wpgrp,
      '        ' '0000'  ' ' 'RMIPM-AUART         '  <wa_import>-auart,
      '        ' '0000'  ' ' 'RMIPM-ILART         '  <wa_import>-ilart,
      '        ' '0000'  ' ' 'RMIPM-GEWERK        '  <wa_import>-gewerk,
      '        ' '0000'  ' ' 'RMIPM-WERGW         '  <wa_import>-wergw,
      '        ' '0000'  ' ' 'RMIPM-GSBER         '  <wa_import>-gsber,
      '        ' '0000'  ' ' 'RMIPM-PLNTY         '  <wa_import>-plnty,
      '        ' '0000'  ' ' 'RMIPM-PLNNR         '  <wa_import>-plnnr,
      '        ' '0000'  ' ' 'RMIPM-PLNAL         '  <wa_import>-plnal,
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
      '        ' '0000'  ' ' 'RMIPM-ZYKL1         '  <wa_import>-zykl1,
      '        ' '0000'  ' ' 'RMIPM-ZEIEH         '  <wa_import>-zeieh,
      '        ' '0000'  ' ' 'RMIPM-PAK_TEXT      '  <wa_import>-pak_text,
      '        ' '0000'  ' ' 'RMIPM-OFFS1         '  '                                                                             ',
      '        ' '0000'  ' ' 'RMIPM-POINT         '  <wa_import>-point,
      'SAPLIWP3' '0201'  'X' '                    ' '                                                                             ',
      '        ' '0000'  ' ' 'BDC_OKCODE          ' '/00                                                                          ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'  ' ' 'RMIPM-WPTXT         '  <wa_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'  ' ' 'RMIPM-PSTXT         ' <wa_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'  ' ' 'RIWO1-TPLNR         ' <wa_import>-tplnr,
      '        ' '0000'  ' ' 'RIWO1-EQUNR         ' <wa_import>-equnr,
      '        ' '0000'  ' ' 'RIWO1-BAUTL         ' <wa_import>-bautl,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
*'        ' '0000'  ' ' 'BDC_CURSOR          ' 'RMIPM-PRIOK                                                                  ',
      '        ' '0000'  ' ' 'RMIPM-IWERK         ' <wa_import>-iwerk ,
      '        ' '0000'  ' ' 'RMIPM-WPGRP         ' <wa_import>-wpgrp ,
      '        ' '0000'  ' ' 'RMIPM-AUART         ' <wa_import>-auart ,
      '        ' '0000'  ' ' 'RMIPM-ILART         ' <wa_import>-ilart ,
      '        ' '0000'  ' ' 'RMIPM-GEWERK        ' <wa_import>-gewerk,
      '        ' '0000'  ' ' 'RMIPM-WERGW         ' <wa_import>-wergw ,
      '        ' '0000'  ' ' 'RMIPM-GSBER         ' <wa_import>-gsber ,
      '        ' '0000'  ' ' 'RMIPM-PRIOK         ' <wa_import>-priok ,
      '        ' '0000'  ' ' 'RMIPM-PLNTY         ' <wa_import>-plnty ,
      '        ' '0000'  ' ' 'RMIPM-PLNNR         ' <wa_import>-plnnr ,
      '        ' '0000'  ' ' 'RMIPM-PLNAL         ' <wa_import>-plnal ,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
      '        ' '0000'  ' ' 'RMIPM-ZYKL1         ' <wa_import>-zykl1    ,
      '        ' '0000'  ' ' 'RMIPM-ZEIEH         ' <wa_import>-zeieh    ,
      '        ' '0000'  ' ' 'RMIPM-PAK_TEXT      ' <wa_import>-pak_text ,
      '        ' '0000'  ' ' 'RMIPM-OFFS1         ' '                     0                                                       ',
      '        ' '0000'  ' ' 'RMIPM-POINT         ' <wa_import>-point,
      'SAPLIWP3' '0201'	'X'	'                    '  '                                                                             ',
      '        ' '0000'	' '	'BDC_OKCODE          '  '=T\02                                                                        ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'	' '	'RMIPM-WPTXT         '  <wa_import>-wptxt,
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'	' '	'RMIPM-PSTXT         '  <wa_import>-wptxt,
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'	' '	'RIWO1-TPLNR         '  <wa_import>-tplnr,
      '        ' '0000'	' '	'RIWO1-EQUNR         '  <wa_import>-equnr,
      '        ' '0000'	' '	'RIWO1-BAUTL         '  <wa_import>-bautl,
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
      '        ' '0000'	' '	'BDC_CURSOR          '  'RMIPM-PRIOK                                                                  ',
      '        ' '0000'	' '	'RMIPM-IWERK         '  <wa_import>-iwerk,
      '        ' '0000'	' '	'RMIPM-WPGRP         '  <wa_import>-wpgrp,
      '        ' '0000'	' '	'RMIPM-AUART         '  <wa_import>-auart,
      '        ' '0000'	' '	'RMIPM-ILART         '  <wa_import>-ilart,
      '        ' '0000'	' '	'RMIPM-GEWERK        '  <wa_import>-gewerk,
      '        ' '0000'	' '	'RMIPM-WERGW         '  <wa_import>-wergw,
      '        ' '0000'	' '	'RMIPM-GSBER         '  <wa_import>-gsber,
      '        ' '0000'	' '	'RMIPM-PRIOK         '  <wa_import>-priok,
      '        ' '0000'	' '	'RMIPM-PLNTY         '  <wa_import>-plnty,
      '        ' '0000'	' '	'RMIPM-PLNNR         '  <wa_import>-plnnr,
      '        ' '0000'	' '	'RMIPM-PLNAL         '  <wa_import>-plnal,
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
      '        ' '0000'	' '	'RMIPM-ZYKL1         '  <wa_import>-zykl1,
      '        ' '0000'	' '	'RMIPM-ZEIEH         '  <wa_import>-zeieh,
      '        ' '0000'	' '	'RMIPM-PAK_TEXT      '  <wa_import>-pak_text,
      '        ' '0000'	' '	'RMIPM-OFFS1         '  '                     0                                                       ',
      '        ' '0000'	' '	'RMIPM-POINT         '  <wa_import>-point,
      'SAPLIWP3' '0201'  'X' '                    ' '                                                                             ',
      '        ' '0000'  ' ' 'BDC_OKCODE          ' '/00                                                                          ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'  ' ' 'RMIPM-WPTXT         ' <wa_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'  ' ' 'RMIPM-PSTXT         ' <wa_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'  ' ' 'RIWO1-TPLNR         ' <wa_import>-tplnr,
      '        ' '0000'  ' ' 'RIWO1-EQUNR         ' <wa_import>-equnr,
      '        ' '0000'  ' ' 'RIWO1-BAUTL         ' <wa_import>-bautl,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
      '        ' '0000'  ' ' 'RMIPM-IWERK         ' <wa_import>-iwerk ,
      '        ' '0000'  ' ' 'RMIPM-WPGRP         ' <wa_import>-wpgrp ,
      '        ' '0000'  ' ' 'RMIPM-AUART         ' <wa_import>-auart ,
      '        ' '0000'  ' ' 'RMIPM-ILART         ' <wa_import>-ilart ,
      '        ' '0000'  ' ' 'RMIPM-GEWERK        ' <wa_import>-gewerk,
      '        ' '0000'  ' ' 'RMIPM-WERGW         ' <wa_import>-wergw ,
      '        ' '0000'  ' ' 'RMIPM-GSBER         ' <wa_import>-gsber ,
      '        ' '0000'  ' ' 'RMIPM-PRIOK         ' <wa_import>-priok ,
      '        ' '0000'  ' ' 'RMIPM-PLNTY         ' <wa_import>-plnty ,
      '        ' '0000'  ' ' 'RMIPM-PLNNR         ' <wa_import>-plnnr ,
      '        ' '0000'  ' ' 'RMIPM-PLNAL         ' <wa_import>-plnal ,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8012SUBSCREEN_BODY1                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0116SUBSCREEN_PARAMETER              ',
      '        ' '0000'  ' ' 'BDC_CURSOR          ' 'RMIPM-CALL_CONFIRM                                                           ',
      '        ' '0000'  ' ' 'RMIPM-VSPOS         ' <wa_import>-vspos,
      '        ' '0000'  ' ' 'RMIPM-HORIZ         ' <wa_import>-horiz,
      '        ' '0000'  ' ' 'RMIPM-TOPOS         ' <wa_import>-topos,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*      '        ' '0000'  ' ' 'RMIPM-ABRHO         ' ' ',
      '        ' '0000'  ' ' 'RMIPM-ABRHO         ' <wa_import>-abrho,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                                         ',
      '        ' '0000'  ' ' 'RMIPM-HUNIT         ' <wa_import>-hunit,
      '        ' '0000'  ' ' 'RMIPM-VSNEG         ' <wa_import>-vsneg,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' 'X',
      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' <wa_import>-call_confirm,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                                       ',
      '        ' '0000'  ' ' 'RMIPM-TONEG         ' <wa_import>-toneg,
*'        ' '0000'  ' ' 'RMIPM-SFAKT         ' 'RMIPM-SFAKT',
      '        ' '0000'  ' ' 'RMIPM-SZAEH         ' <wa_import>-szaeh,
      'SAPLIWP3' '0201'	'X'	'                    ' '                                                                             ',
      '        ' '0000'	' '	'BDC_OKCODE          ' '=BU                                                                          ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'	' '	'RMIPM-WPTXT         ' <wa_import>-wptxt,
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'	' '	'RMIPM-PSTXT         ' <wa_import>-wptxt,
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'	' '	'RIWO1-TPLNR         ' <wa_import>-tplnr,
      '        ' '0000'	' '	'RIWO1-EQUNR         ' <wa_import>-equnr,
      '        ' '0000'	' '	'RIWO1-BAUTL         ' <wa_import>-bautl,
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
      '        ' '0000'	' '	'RMIPM-IWERK         ' <wa_import>-iwerk ,
      '        ' '0000'	' '	'RMIPM-WPGRP         ' <wa_import>-wpgrp ,
      '        ' '0000'	' '	'RMIPM-AUART         ' <wa_import>-auart ,
      '        ' '0000'	' '	'RMIPM-ILART         ' <wa_import>-ilart ,
      '        ' '0000'	' '	'RMIPM-GEWERK        ' <wa_import>-gewerk,
      '        ' '0000'	' '	'RMIPM-WERGW         ' <wa_import>-wergw ,
      '        ' '0000'	' '	'RMIPM-GSBER         ' <wa_import>-gsber ,
      '        ' '0000'	' '	'RMIPM-PRIOK         ' <wa_import>-priok ,
      '        ' '0000'	' '	'RMIPM-PLNTY         ' <wa_import>-plnty ,
      '        ' '0000'	' '	'RMIPM-PLNNR         ' <wa_import>-plnnr ,
      '        ' '0000'	' '	'RMIPM-PLNAL         ' <wa_import>-plnal ,
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8012SUBSCREEN_BODY1                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                0116SUBSCREEN_PARAMETER              ',
      '        ' '0000'	' '	'BDC_CURSOR          ' 'RMIPM-CALL_CONFIRM                                                           ',
      '        ' '0000'	' '	'RMIPM-VSPOS         ' <wa_import>-vspos,
      '        ' '0000'	' '	'RMIPM-HORIZ         ' <wa_import>-horiz,
      '        ' '0000'	' '	'RMIPM-TOPOS         ' <wa_import>-topos,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
      '        ' '0000'	' '	'RMIPM-ABRHO         ' <wa_import>-abrho,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
      '        ' '0000'	' '	'RMIPM-HUNIT         ' <wa_import>-hunit,
      '        ' '0000'	' '	'RMIPM-VSNEG         ' <wa_import>-vsneg,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' 'X',
      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' <wa_import>-call_confirm,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
      '        ' '0000'	' '	'RMIPM-TONEG         ' <wa_import>-toneg,
*'        ' '0000'  ' ' 'RMIPM-SFAKT         ' 'RMIPM-SFAKT',
          '    ' '0000'  ' ' 'RMIPM-SZAEH        ' <wa_import>-szaeh.

        ELSE.
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
        lv_szaeh = <wa_import>-szaeh.
        REPLACE ALL OCCURRENCES OF '/' IN lv_szaeh WITH '.'.
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
          PERFORM f_bdc_data USING:

        'SAPLIWP3' '0100'  'X' '                    '  '                                        ',
        '        ' '0000'  ' ' 'BDC_CURSOR          '  'RMIPM-MPTYP                                                                  ',
        '        ' '0000'  ' ' 'BDC_OKCODE          '  '/00                                                                          ',
        '        ' '0000'  ' ' 'RMIPM-MPTYP         '  <wa_import>-mptyp,
        'SAPLIWP3' '0201'  'X' '                    '  '                                                                             ',
        '        ' '0000'  ' ' 'BDC_OKCODE          '  '/00                                                                          ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'  ' ' 'RMIPM-WPTXT         '  <wa_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'  ' ' 'RMIPM-PSTXT         '  <wa_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'  ' ' 'RIWO1-TPLNR         '  <wa_import>-tplnr,
        '        ' '0000'  ' ' 'RIWO1-EQUNR         '  <wa_import>-equnr,
        '        ' '0000'  ' ' 'RIWO1-BAUTL         '  <wa_import>-bautl,
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
*'        ' '0000'  ' ' 'BDC_CURSOR          '  'RMIPM-PLNAL                                                                  ',
        '        ' '0000'  ' ' 'RMIPM-IWERK         '  <wa_import>-iwerk,
        '        ' '0000'  ' ' 'RMIPM-WPGRP         '  <wa_import>-wpgrp,
        '        ' '0000'  ' ' 'RMIPM-AUART         '  <wa_import>-auart,
        '        ' '0000'  ' ' 'RMIPM-ILART         '  <wa_import>-ilart,
        '        ' '0000'  ' ' 'RMIPM-GEWERK        '  <wa_import>-gewerk,
        '        ' '0000'  ' ' 'RMIPM-WERGW         '  <wa_import>-wergw,
        '        ' '0000'  ' ' 'RMIPM-GSBER         '  <wa_import>-gsber,
        '        ' '0000'  ' ' 'RMIPM-PLNTY         '  <wa_import>-plnty,
        '        ' '0000'  ' ' 'RMIPM-PLNNR         '  <wa_import>-plnnr,
        '        ' '0000'  ' ' 'RMIPM-PLNAL         '  <wa_import>-plnal,
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
        '        ' '0000'  ' ' 'RMIPM-ZYKL1         '  <wa_import>-zykl1,
        '        ' '0000'  ' ' 'RMIPM-ZEIEH         '  <wa_import>-zeieh,
        '        ' '0000'  ' ' 'RMIPM-PAK_TEXT      '  <wa_import>-pak_text,
        '        ' '0000'  ' ' 'RMIPM-OFFS1         '  '                                                                             ',
        '        ' '0000'  ' ' 'RMIPM-POINT         '  <wa_import>-point,
        'SAPLIWP3' '0201'  'X' '                    ' '                                                                             ',
        '        ' '0000'  ' ' 'BDC_OKCODE          ' '/00                                                                          ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'  ' ' 'RMIPM-WPTXT         '  <wa_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'  ' ' 'RMIPM-PSTXT         ' <wa_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'  ' ' 'RIWO1-TPLNR         ' <wa_import>-tplnr,
        '        ' '0000'  ' ' 'RIWO1-EQUNR         ' <wa_import>-equnr,
        '        ' '0000'  ' ' 'RIWO1-BAUTL         ' <wa_import>-bautl,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
*'        ' '0000'  ' ' 'BDC_CURSOR          ' 'RMIPM-PRIOK                                                                  ',
        '        ' '0000'  ' ' 'RMIPM-IWERK         ' <wa_import>-iwerk ,
        '        ' '0000'  ' ' 'RMIPM-WPGRP         ' <wa_import>-wpgrp ,
        '        ' '0000'  ' ' 'RMIPM-AUART         ' <wa_import>-auart ,
        '        ' '0000'  ' ' 'RMIPM-ILART         ' <wa_import>-ilart ,
        '        ' '0000'  ' ' 'RMIPM-GEWERK        ' <wa_import>-gewerk,
        '        ' '0000'  ' ' 'RMIPM-WERGW         ' <wa_import>-wergw ,
        '        ' '0000'  ' ' 'RMIPM-GSBER         ' <wa_import>-gsber ,
        '        ' '0000'  ' ' 'RMIPM-PRIOK         ' <wa_import>-priok ,
        '        ' '0000'  ' ' 'RMIPM-PLNTY         ' <wa_import>-plnty ,
        '        ' '0000'  ' ' 'RMIPM-PLNNR         ' <wa_import>-plnnr ,
        '        ' '0000'  ' ' 'RMIPM-PLNAL         ' <wa_import>-plnal ,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
        '        ' '0000'  ' ' 'RMIPM-ZYKL1         ' <wa_import>-zykl1    ,
        '        ' '0000'  ' ' 'RMIPM-ZEIEH         ' <wa_import>-zeieh    ,
        '        ' '0000'  ' ' 'RMIPM-PAK_TEXT      ' <wa_import>-pak_text ,
        '        ' '0000'  ' ' 'RMIPM-OFFS1         ' '0',
*    '        ' '0000'  ' ' 'RMIPM-POINT         ' <wa_import>-point,
        'SAPLIWP3' '0201'	'X'	'                    '  '                                                                             ',
        '        ' '0000'	' '	'BDC_OKCODE          '  '=T\02                                                                        ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'	' '	'RMIPM-WPTXT         '  <wa_import>-wptxt,
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'	' '	'RMIPM-PSTXT         '  <wa_import>-wptxt,
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'	' '	'RIWO1-TPLNR         '  <wa_import>-tplnr,
        '        ' '0000'	' '	'RIWO1-EQUNR         '  <wa_import>-equnr,
        '        ' '0000'	' '	'RIWO1-BAUTL         '  <wa_import>-bautl,
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
        '        ' '0000'	' '	'BDC_CURSOR          '  'RMIPM-PRIOK                                                                  ',
        '        ' '0000'	' '	'RMIPM-IWERK         '  <wa_import>-iwerk,
        '        ' '0000'	' '	'RMIPM-WPGRP         '  <wa_import>-wpgrp,
        '        ' '0000'	' '	'RMIPM-AUART         '  <wa_import>-auart,
        '        ' '0000'	' '	'RMIPM-ILART         '  <wa_import>-ilart,
        '        ' '0000'	' '	'RMIPM-GEWERK        '  <wa_import>-gewerk,
        '        ' '0000'	' '	'RMIPM-WERGW         '  <wa_import>-wergw,
        '        ' '0000'	' '	'RMIPM-GSBER         '  <wa_import>-gsber,
        '        ' '0000'	' '	'RMIPM-PRIOK         '  <wa_import>-priok,
        '        ' '0000'	' '	'RMIPM-PLNTY         '  <wa_import>-plnty,
        '        ' '0000'	' '	'RMIPM-PLNNR         '  <wa_import>-plnnr,
        '        ' '0000'	' '	'RMIPM-PLNAL         '  <wa_import>-plnal,
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
        '        ' '0000'	' '	'RMIPM-ZYKL1         '  <wa_import>-zykl1,
        '        ' '0000'	' '	'RMIPM-ZEIEH         '  <wa_import>-zeieh,
        '        ' '0000'	' '	'RMIPM-PAK_TEXT      '  <wa_import>-pak_text,
        '        ' '0000'	' '	'RMIPM-OFFS1         '  ' 0 ',
*    '        ' '0000'  ' ' 'RMIPM-POINT         '  <wa_import>-point,
        'SAPLIWP3' '0201'  'X' '                    ' '                                                                             ',
        '        ' '0000'  ' ' 'BDC_OKCODE          ' '/00                                                                          ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'  ' ' 'RMIPM-WPTXT         ' <wa_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'  ' ' 'RMIPM-PSTXT         ' <wa_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'  ' ' 'RIWO1-TPLNR         ' <wa_import>-tplnr,
        '        ' '0000'  ' ' 'RIWO1-EQUNR         ' <wa_import>-equnr,
        '        ' '0000'  ' ' 'RIWO1-BAUTL         ' <wa_import>-bautl,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
        '        ' '0000'  ' ' 'RMIPM-IWERK         ' <wa_import>-iwerk ,
        '        ' '0000'  ' ' 'RMIPM-WPGRP         ' <wa_import>-wpgrp ,
        '        ' '0000'  ' ' 'RMIPM-AUART         ' <wa_import>-auart ,
        '        ' '0000'  ' ' 'RMIPM-ILART         ' <wa_import>-ilart ,
        '        ' '0000'  ' ' 'RMIPM-GEWERK        ' <wa_import>-gewerk,
        '        ' '0000'  ' ' 'RMIPM-WERGW         ' <wa_import>-wergw ,
        '        ' '0000'  ' ' 'RMIPM-GSBER         ' <wa_import>-gsber ,
        '        ' '0000'  ' ' 'RMIPM-PRIOK         ' <wa_import>-priok ,
        '        ' '0000'  ' ' 'RMIPM-PLNTY         ' <wa_import>-plnty ,
        '        ' '0000'  ' ' 'RMIPM-PLNNR         ' <wa_import>-plnnr ,
        '        ' '0000'  ' ' 'RMIPM-PLNAL         ' <wa_import>-plnal ,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8012SUBSCREEN_BODY1                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0116SUBSCREEN_PARAMETER              ',
        '        ' '0000'  ' ' 'BDC_CURSOR          ' 'RMIPM-CALL_CONFIRM                                                           ',
        '        ' '0000'  ' ' 'RMIPM-VSPOS         ' <wa_import>-vspos,
        '        ' '0000'  ' ' 'RMIPM-HORIZ         ' <wa_import>-horiz,
        '        ' '0000'  ' ' 'RMIPM-TOPOS         ' <wa_import>-topos,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*        '        ' '0000'  ' ' 'RMIPM-ABRHO         ' '
        '        ' '0000'  ' ' 'RMIPM-ABRHO         ' <wa_import>-abrho,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                                      ',
        '        ' '0000'  ' ' 'RMIPM-HUNIT         ' <wa_import>-hunit,
        '        ' '0000'  ' ' 'RMIPM-VSNEG         ' <wa_import>-vsneg,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*        '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' 'X',
        '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' <wa_import>-call_confirm,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                                          ',
        '        ' '0000'  ' ' 'RMIPM-TONEG         ' <wa_import>-toneg,
*'        ' '0000'  ' ' 'RMIPM-SFAKT         ' 'RMIPM-SFAKT',
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*        '        ' '0000'  ' ' 'RMIPM-STADT         ' <wa_import>-stadt,
        '        ' '0000'  ' ' 'RMIPM-STADT         ' <wa_import>-szaeh,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
        'SAPLIWP3' '0201'	'X'	'                    ' '                                                                             ',
        '        ' '0000'	' '	'BDC_OKCODE          ' '=BU                                                                          ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'	' '	'RMIPM-WPTXT         ' <wa_import>-wptxt,
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'	' '	'RMIPM-PSTXT         ' <wa_import>-wptxt,
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'	' '	'RIWO1-TPLNR         ' <wa_import>-tplnr,
        '        ' '0000'	' '	'RIWO1-EQUNR         ' <wa_import>-equnr,
        '        ' '0000'	' '	'RIWO1-BAUTL         ' <wa_import>-bautl,
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
        '        ' '0000'	' '	'RMIPM-IWERK         ' <wa_import>-iwerk ,
        '        ' '0000'	' '	'RMIPM-WPGRP         ' <wa_import>-wpgrp ,
        '        ' '0000'	' '	'RMIPM-AUART         ' <wa_import>-auart ,
        '        ' '0000'	' '	'RMIPM-ILART         ' <wa_import>-ilart ,
        '        ' '0000'	' '	'RMIPM-GEWERK        ' <wa_import>-gewerk,
        '        ' '0000'	' '	'RMIPM-WERGW         ' <wa_import>-wergw ,
        '        ' '0000'	' '	'RMIPM-GSBER         ' <wa_import>-gsber ,
        '        ' '0000'	' '	'RMIPM-PRIOK         ' <wa_import>-priok ,
        '        ' '0000'	' '	'RMIPM-PLNTY         ' <wa_import>-plnty ,
        '        ' '0000'	' '	'RMIPM-PLNNR         ' <wa_import>-plnnr ,
        '        ' '0000'	' '	'RMIPM-PLNAL         ' <wa_import>-plnal ,
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8012SUBSCREEN_BODY1                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                0116SUBSCREEN_PARAMETER              ',
        '        ' '0000'	' '	'BDC_CURSOR          ' 'RMIPM-CALL_CONFIRM                                                           ',
        '        ' '0000'	' '	'RMIPM-VSPOS         ' <wa_import>-vspos,
        '        ' '0000'	' '	'RMIPM-HORIZ         ' <wa_import>-horiz,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
        '        ' '0000'	' '	'RMIPM-ABRHO         ' <wa_import>-abrho,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
        '        ' '0000'	' '	'RMIPM-TOPOS         ' <wa_import>-topos,
        '        ' '0000'	' '	'RMIPM-HUNIT         ' <wa_import>-hunit,
        '        ' '0000'	' '	'RMIPM-VSNEG         ' <wa_import>-vsneg,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' 'X',
       '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' <wa_import>-call_confirm,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                                         ',
        '        ' '0000'	' '	'RMIPM-TONEG         ' <wa_import>-toneg,
*'        ' '0000'  ' ' 'RMIPM-SFAKT         ' 'RMIPM-SFAKT',
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
*            '    ' '0000'  ' ' 'RMIPM-STADT        ' <wa_import>-szaeh.
            '    ' '0000'  ' ' 'RMIPM-STADT        ' lv_szaeh.
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
        ENDIF.

        CLEAR p_erro.
        PERFORM zf_call_transaction USING 'IP41' CHANGING p_erro.

        IF p_erro IS INITIAL.
          READ TABLE it_msg INTO DATA(_w_msg) INDEX 1.
          <wa_import>-warpl = _w_msg-msgv1.
          <wa_import>-uname = sy-uname.
          <wa_import>-erdat = sy-datum.

          IF <wa_import>-zeieh EQ 'H' OR <wa_import>-zeieh EQ 'KM'.
          ELSE.
            <wa_import>-szaeh = |{ <wa_import>-szaeh+6(4) }{ <wa_import>-szaeh+3(2) }{ <wa_import>-szaeh(2) }|.
            <wa_import>-stadt = <wa_import>-szaeh.
          ENDIF.
          MOVE-CORRESPONDING <wa_import> TO zpmt005.
          MODIFY zpmt005.

          <wa_import>-equnr = |{ <wa_import>-equnr ALPHA = OUT }|.
          <wa_import>-point = |{ <wa_import>-point ALPHA = OUT }|.
          CLEAR it_sel_pl.
        ENDIF.
      ENDLOOP.

      CLEAR contnum.
      LOOP AT it_import[] ASSIGNING FIELD-SYMBOL(<z_import>) WHERE marc  = abap_true
                                                              AND  ip10  = ' '.

        IF <z_import>-warpl IS NOT INITIAL AND <z_import>-status = '@08@'.

          <z_import>-warpl = |{ <z_import>-warpl ALPHA = IN }|.
          DO.
            SELECT COUNT(*)
              FROM mpla
              WHERE warpl EQ <z_import>-warpl.
            IF sy-subrc IS INITIAL.
*              PERFORM SHDB_IP10 USING <Z_IMPORT>-WARPL <Z_IMPORT>-SZAEH <Z_IMPORT>-STADT <Z_IMPORT>-ZEIEH CHANGING P_IP10.
              EXIT.
            ELSE.
              ADD 1 TO contnum.
              IF contnum EQ 100.
                EXIT.
              ENDIF.
            ENDIF.
          ENDDO.

          IF p_ip10 IS NOT INITIAL.
            <z_import>-warpl = |{ <z_import>-warpl ALPHA = OUT }|.
            <z_import>-ip10 = p_ip10.
            APPEND <z_import> TO it_ip10.
          ENDIF.
        ELSE.
          <z_import>-warpl = |{ <z_import>-warpl ALPHA = OUT }|.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ELSE.
      LOOP AT it_import ASSIGNING FIELD-SYMBOL(<w_import>).

        CLEAR w_mpla.
        CLEAR w_zpmt005.
        IF <w_import>-equnr IS NOT INITIAL.
          <w_import>-equnr = |{ <w_import>-equnr ALPHA = IN }|.
          SELECT SINGLE *
          FROM zpmt005
          INTO w_zpmt005
            WHERE tplnr EQ <w_import>-tplnr
             AND  equnr EQ <w_import>-equnr
             AND  mptyp EQ <w_import>-mptyp
             AND  iwerk EQ <w_import>-iwerk
             AND  plnnr EQ <w_import>-plnnr
             AND  plnal EQ <w_import>-plnal.
        ELSE.
          SELECT SINGLE *
        FROM zpmt005
        INTO w_zpmt005
          WHERE tplnr EQ <w_import>-tplnr
           AND  mptyp EQ <w_import>-mptyp
           AND  iwerk EQ <w_import>-iwerk
           AND  plnnr EQ <w_import>-plnnr
           AND  plnal EQ <w_import>-plnal.
        ENDIF.

        IF w_zpmt005 IS NOT INITIAL.
*      VERIFICAR STATUS DO PLANO.
          w_zpmt005-warpl = |{ w_zpmt005-warpl ALPHA = IN }|.
          SELECT SINGLE *
          FROM mpla
          INTO w_mpla
            WHERE warpl EQ w_zpmt005-warpl.
          PERFORM status_check_plano USING w_mpla-objnr return_code CHANGING ijstat.
          IF ijstat EQ 'I0001'.
            CLEAR sdydo_text_element.
            CONCATENATE sdydo_text_element'Ja existe plano cadastrado' INTO sdydo_text_element SEPARATED BY space.
            <w_import>-tstatus = sdydo_text_element.
            <w_import>-status = '@0A@'.
            <w_import>-warpl = |{ <w_import>-warpl ALPHA = OUT }|.
            <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
            <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
            FREE w_zpmt005.
            CONTINUE.
          ENDIF.
        ENDIF.

        <w_import>-point = |{ <w_import>-point ALPHA = IN }|.
        IF <w_import>-status NE '@08@'.
          CONTINUE.
        ENDIF.

        FREE ti_bdcdata.
        FREE it_msg.

* Verifica se unidade de medida do plano é por hora/km.
        IF <w_import>-zeieh EQ 'H' OR <w_import>-zeieh EQ 'KM'.
          PERFORM f_bdc_data USING:
      'SAPLIWP3' '0100'  'X' '                    '  '                                        ',
      '        ' '0000'  ' ' 'BDC_CURSOR          '  'RMIPM-MPTYP                                                                  ',
      '        ' '0000'  ' ' 'BDC_OKCODE          '  '/00                                                                          ',
      '        ' '0000'  ' ' 'RMIPM-MPTYP         '  <w_import>-mptyp,
      'SAPLIWP3' '0201'  'X' '                    '  '                                                                             ',
      '        ' '0000'  ' ' 'BDC_OKCODE          '  '/00                                                                          ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'  ' ' 'RMIPM-WPTXT         '  <w_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'  ' ' 'RMIPM-PSTXT         '  <w_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'  ' ' 'RIWO1-TPLNR         '  <w_import>-tplnr,
      '        ' '0000'  ' ' 'RIWO1-EQUNR         '  <w_import>-equnr,
      '        ' '0000'  ' ' 'RIWO1-BAUTL         '  <w_import>-bautl,
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
*'        ' '0000'  ' ' 'BDC_CURSOR          '  'RMIPM-PLNAL                                                                  ',
      '        ' '0000'  ' ' 'RMIPM-IWERK         '  <w_import>-iwerk,
      '        ' '0000'  ' ' 'RMIPM-WPGRP         '  <w_import>-wpgrp,
      '        ' '0000'  ' ' 'RMIPM-AUART         '  <w_import>-auart,
      '        ' '0000'  ' ' 'RMIPM-ILART         '  <w_import>-ilart,
      '        ' '0000'  ' ' 'RMIPM-GEWERK        '  <w_import>-gewerk,
      '        ' '0000'  ' ' 'RMIPM-WERGW         '  <w_import>-wergw,
      '        ' '0000'  ' ' 'RMIPM-GSBER         '  <w_import>-gsber,
      '        ' '0000'  ' ' 'RMIPM-PLNTY         '  <w_import>-plnty,
      '        ' '0000'  ' ' 'RMIPM-PLNNR         '  <w_import>-plnnr,
      '        ' '0000'  ' ' 'RMIPM-PLNAL         '  <w_import>-plnal,
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
      '        ' '0000'  ' ' 'RMIPM-ZYKL1         '  <w_import>-zykl1,
      '        ' '0000'  ' ' 'RMIPM-ZEIEH         '  <w_import>-zeieh,
      '        ' '0000'  ' ' 'RMIPM-PAK_TEXT      '  <w_import>-pak_text,
      '        ' '0000'  ' ' 'RMIPM-OFFS1         '  '                                                                             ',
      '        ' '0000'  ' ' 'RMIPM-POINT         '  <w_import>-point,
      'SAPLIWP3' '0201'  'X' '                    ' '                                                                             ',
      '        ' '0000'  ' ' 'BDC_OKCODE          ' '/00                                                                          ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'  ' ' 'RMIPM-WPTXT         '  <w_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'  ' ' 'RMIPM-PSTXT         ' <w_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'  ' ' 'RIWO1-TPLNR         ' <w_import>-tplnr,
      '        ' '0000'  ' ' 'RIWO1-EQUNR         ' <w_import>-equnr,
      '        ' '0000'  ' ' 'RIWO1-BAUTL         ' <w_import>-bautl,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
*'        ' '0000'  ' ' 'BDC_CURSOR          ' 'RMIPM-PRIOK                                                                  ',
      '        ' '0000'  ' ' 'RMIPM-IWERK         ' <w_import>-iwerk ,
      '        ' '0000'  ' ' 'RMIPM-WPGRP         ' <w_import>-wpgrp ,
      '        ' '0000'  ' ' 'RMIPM-AUART         ' <w_import>-auart ,
      '        ' '0000'  ' ' 'RMIPM-ILART         ' <w_import>-ilart ,
      '        ' '0000'  ' ' 'RMIPM-GEWERK        ' <w_import>-gewerk,
      '        ' '0000'  ' ' 'RMIPM-WERGW         ' <w_import>-wergw ,
      '        ' '0000'  ' ' 'RMIPM-GSBER         ' <w_import>-gsber ,
      '        ' '0000'  ' ' 'RMIPM-PRIOK         ' <w_import>-priok ,
      '        ' '0000'  ' ' 'RMIPM-PLNTY         ' <w_import>-plnty ,
      '        ' '0000'  ' ' 'RMIPM-PLNNR         ' <w_import>-plnnr ,
      '        ' '0000'  ' ' 'RMIPM-PLNAL         ' <w_import>-plnal ,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
      '        ' '0000'  ' ' 'RMIPM-ZYKL1         ' <w_import>-zykl1    ,
      '        ' '0000'  ' ' 'RMIPM-ZEIEH         ' <w_import>-zeieh    ,
      '        ' '0000'  ' ' 'RMIPM-PAK_TEXT      ' <w_import>-pak_text ,
      '        ' '0000'  ' ' 'RMIPM-OFFS1         ' '                     0                                                       ',
      '        ' '0000'  ' ' 'RMIPM-POINT         ' <w_import>-point,
      'SAPLIWP3' '0201'	'X'	'                    '  '                                                                             ',
      '        ' '0000'	' '	'BDC_OKCODE          '  '=T\02                                                                        ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'	' '	'RMIPM-WPTXT         '  <w_import>-wptxt,
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'	' '	'RMIPM-PSTXT         '  <w_import>-wptxt,
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'	' '	'RIWO1-TPLNR         '  <w_import>-tplnr,
      '        ' '0000'	' '	'RIWO1-EQUNR         '  <w_import>-equnr,
      '        ' '0000'	' '	'RIWO1-BAUTL         '  <w_import>-bautl,
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
      '        ' '0000'	' '	'BDC_CURSOR          '  'RMIPM-PRIOK                                                                  ',
      '        ' '0000'	' '	'RMIPM-IWERK         '  <w_import>-iwerk,
      '        ' '0000'	' '	'RMIPM-WPGRP         '  <w_import>-wpgrp,
      '        ' '0000'	' '	'RMIPM-AUART         '  <w_import>-auart,
      '        ' '0000'	' '	'RMIPM-ILART         '  <w_import>-ilart,
      '        ' '0000'	' '	'RMIPM-GEWERK        '  <w_import>-gewerk,
      '        ' '0000'	' '	'RMIPM-WERGW         '  <w_import>-wergw,
      '        ' '0000'	' '	'RMIPM-GSBER         '  <w_import>-gsber,
      '        ' '0000'	' '	'RMIPM-PRIOK         '  <w_import>-priok,
      '        ' '0000'	' '	'RMIPM-PLNTY         '  <w_import>-plnty,
      '        ' '0000'	' '	'RMIPM-PLNNR         '  <w_import>-plnnr,
      '        ' '0000'	' '	'RMIPM-PLNAL         '  <w_import>-plnal,
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
      '        ' '0000'	' '	'RMIPM-ZYKL1         '  <w_import>-zykl1,
      '        ' '0000'	' '	'RMIPM-ZEIEH         '  <w_import>-zeieh,
      '        ' '0000'	' '	'RMIPM-PAK_TEXT      '  <w_import>-pak_text,
      '        ' '0000'	' '	'RMIPM-OFFS1         '  '                     0                                                       ',
      '        ' '0000'	' '	'RMIPM-POINT         '  <w_import>-point,
      'SAPLIWP3' '0201'  'X' '                    ' '                                                                             ',
      '        ' '0000'  ' ' 'BDC_OKCODE          ' '/00                                                                          ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'  ' ' 'RMIPM-WPTXT         ' <w_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'  ' ' 'RMIPM-PSTXT         ' <w_import>-wptxt,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'  ' ' 'RIWO1-TPLNR         ' <w_import>-tplnr,
      '        ' '0000'  ' ' 'RIWO1-EQUNR         ' <w_import>-equnr,
      '        ' '0000'  ' ' 'RIWO1-BAUTL         ' <w_import>-bautl,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
      '        ' '0000'  ' ' 'RMIPM-IWERK         ' <w_import>-iwerk ,
      '        ' '0000'  ' ' 'RMIPM-WPGRP         ' <w_import>-wpgrp ,
      '        ' '0000'  ' ' 'RMIPM-AUART         ' <w_import>-auart ,
      '        ' '0000'  ' ' 'RMIPM-ILART         ' <w_import>-ilart ,
      '        ' '0000'  ' ' 'RMIPM-GEWERK        ' <w_import>-gewerk,
      '        ' '0000'  ' ' 'RMIPM-WERGW         ' <w_import>-wergw ,
      '        ' '0000'  ' ' 'RMIPM-GSBER         ' <w_import>-gsber ,
      '        ' '0000'  ' ' 'RMIPM-PRIOK         ' <w_import>-priok ,
      '        ' '0000'  ' ' 'RMIPM-PLNTY         ' <w_import>-plnty ,
      '        ' '0000'  ' ' 'RMIPM-PLNNR         ' <w_import>-plnnr ,
      '        ' '0000'  ' ' 'RMIPM-PLNAL         ' <w_import>-plnal ,
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8012SUBSCREEN_BODY1                  ',
      '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0116SUBSCREEN_PARAMETER              ',
      '        ' '0000'  ' ' 'BDC_CURSOR          ' 'RMIPM-CALL_CONFIRM                                                           ',
      '        ' '0000'  ' ' 'RMIPM-VSPOS         ' <w_import>-vspos,
      '        ' '0000'  ' ' 'RMIPM-HORIZ         ' <w_import>-horiz,
      '        ' '0000'  ' ' 'RMIPM-TOPOS         ' <w_import>-topos,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*      '        ' '0000'  ' ' 'RMIPM-ABRHO         ' '
      '        ' '0000'  ' ' 'RMIPM-ABRHO         ' <w_import>-abrho,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
      '        ' '0000'  ' ' 'RMIPM-ABRHO         ' '                                                                             ',
      '        ' '0000'  ' ' 'RMIPM-HUNIT         ' <w_import>-hunit,
      '        ' '0000'  ' ' 'RMIPM-VSNEG         ' <w_import>-vsneg,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' 'X',
      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' <w_import>-call_confirm,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                                  ',
      '        ' '0000'  ' ' 'RMIPM-TONEG         ' <w_import>-toneg,
*'        ' '0000'  ' ' 'RMIPM-SFAKT         ' 'RMIPM-SFAKT',
*    '        ' '0000'  ' ' 'RMIPM-SZAEH         ' <W_IMPORT>-SZAEH,
      'SAPLIWP3' '0201'	'X'	'                    ' '                                                                             ',
      '        ' '0000'	' '	'BDC_OKCODE          ' '=BU                                                                          ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
      '        ' '0000'	' '	'RMIPM-WPTXT         ' <w_import>-wptxt,
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
      '        ' '0000'	' '	'RMIPM-PSTXT         ' <w_import>-wptxt,
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
      '        ' '0000'	' '	'RIWO1-TPLNR         ' <w_import>-tplnr,
      '        ' '0000'	' '	'RIWO1-EQUNR         ' <w_import>-equnr,
      '        ' '0000'	' '	'RIWO1-BAUTL         ' <w_import>-bautl,
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
      '        ' '0000'	' '	'RMIPM-IWERK         ' <w_import>-iwerk ,
      '        ' '0000'	' '	'RMIPM-WPGRP         ' <w_import>-wpgrp ,
      '        ' '0000'	' '	'RMIPM-AUART         ' <w_import>-auart ,
      '        ' '0000'	' '	'RMIPM-ILART         ' <w_import>-ilart ,
      '        ' '0000'	' '	'RMIPM-GEWERK        ' <w_import>-gewerk,
      '        ' '0000'	' '	'RMIPM-WERGW         ' <w_import>-wergw ,
      '        ' '0000'	' '	'RMIPM-GSBER         ' <w_import>-gsber ,
      '        ' '0000'	' '	'RMIPM-PRIOK         ' <w_import>-priok ,
      '        ' '0000'	' '	'RMIPM-PLNTY         ' <w_import>-plnty ,
      '        ' '0000'	' '	'RMIPM-PLNNR         ' <w_import>-plnnr ,
      '        ' '0000'	' '	'RMIPM-PLNAL         ' <w_import>-plnal ,
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8012SUBSCREEN_BODY1                  ',
      '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                0116SUBSCREEN_PARAMETER              ',
      '        ' '0000'	' '	'BDC_CURSOR          ' 'RMIPM-CALL_CONFIRM                                                           ',
      '        ' '0000'	' '	'RMIPM-VSPOS         ' <w_import>-vspos,
      '        ' '0000'	' '	'RMIPM-HORIZ         ' <w_import>-horiz,
      '        ' '0000'	' '	'RMIPM-TOPOS         ' <w_import>-topos,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
*      '        ' '0000'	' '	'RMIPM-ABRHO         ' <wa_import>-abrho,
      '        ' '0000'	' '	'RMIPM-ABRHO         ' <w_import>-abrho,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
      '        ' '0000'	' '	'RMIPM-HUNIT         ' <w_import>-hunit,
      '        ' '0000'	' '	'RMIPM-VSNEG         ' <w_import>-vsneg,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' 'X'
      '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM ' <w_import>-call_confirm,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                                        ',
      '        ' '0000'	' '	'RMIPM-TONEG         ' <w_import>-toneg,
*'        ' '0000'  ' ' 'RMIPM-SFAKT         ' 'RMIPM-SFAKT',
          '    ' '0000'  ' ' 'RMIPM-SZAEH        ' <w_import>-szaeh.

        ELSE.
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
        lv_szaeh = <w_import>-szaeh.
        REPLACE ALL OCCURRENCES OF '/' IN lv_szaeh WITH '.'.
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024

          PERFORM f_bdc_data USING:

        'SAPLIWP3' '0100'  'X' '                    '  '                                        ',
        '        ' '0000'  ' ' 'BDC_CURSOR          '  'RMIPM-MPTYP                                                                  ',
        '        ' '0000'  ' ' 'BDC_OKCODE          '  '/00                                                                          ',
        '        ' '0000'  ' ' 'RMIPM-MPTYP         '  <w_import>-mptyp,
        'SAPLIWP3' '0201'  'X' '                    '  '                                                                             ',
        '        ' '0000'  ' ' 'BDC_OKCODE          '  '/00                                                                          ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'  ' ' 'RMIPM-WPTXT         '  <w_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'  ' ' 'RMIPM-PSTXT         '  <w_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'  ' ' 'RIWO1-TPLNR         '  <w_import>-tplnr,
        '        ' '0000'  ' ' 'RIWO1-EQUNR         '  <w_import>-equnr,
        '        ' '0000'  ' ' 'RIWO1-BAUTL         '  <w_import>-bautl,
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
*'        ' '0000'  ' ' 'BDC_CURSOR          '  'RMIPM-PLNAL                                                                  ',
        '        ' '0000'  ' ' 'RMIPM-IWERK         '  <w_import>-iwerk,
        '        ' '0000'  ' ' 'RMIPM-WPGRP         '  <w_import>-wpgrp,
        '        ' '0000'  ' ' 'RMIPM-AUART         '  <w_import>-auart,
        '        ' '0000'  ' ' 'RMIPM-ILART         '  <w_import>-ilart,
        '        ' '0000'  ' ' 'RMIPM-GEWERK        '  <w_import>-gewerk,
        '        ' '0000'  ' ' 'RMIPM-WERGW         '  <w_import>-wergw,
        '        ' '0000'  ' ' 'RMIPM-GSBER         '  <w_import>-gsber,
        '        ' '0000'  ' ' 'RMIPM-PLNTY         '  <w_import>-plnty,
        '        ' '0000'  ' ' 'RMIPM-PLNNR         '  <w_import>-plnnr,
        '        ' '0000'  ' ' 'RMIPM-PLNAL         '  <w_import>-plnal,
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          '  'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
        '        ' '0000'  ' ' 'RMIPM-ZYKL1         '  <w_import>-zykl1,
        '        ' '0000'  ' ' 'RMIPM-ZEIEH         '  <w_import>-zeieh,
        '        ' '0000'  ' ' 'RMIPM-PAK_TEXT      '  <w_import>-pak_text,
        '        ' '0000'  ' ' 'RMIPM-OFFS1         '  '                                                                             ',
        '        ' '0000'  ' ' 'RMIPM-POINT         '  <w_import>-point,
        'SAPLIWP3' '0201'  'X' '                    ' '                                                                             ',
        '        ' '0000'  ' ' 'BDC_OKCODE          ' '/00                                                                          ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'  ' ' 'RMIPM-WPTXT         '  <w_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'  ' ' 'RMIPM-PSTXT         ' <w_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'  ' ' 'RIWO1-TPLNR         ' <w_import>-tplnr,
        '        ' '0000'  ' ' 'RIWO1-EQUNR         ' <w_import>-equnr,
        '        ' '0000'  ' ' 'RIWO1-BAUTL         ' <w_import>-bautl,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
*'        ' '0000'  ' ' 'BDC_CURSOR          ' 'RMIPM-PRIOK                                                                  ',
        '        ' '0000'  ' ' 'RMIPM-IWERK         ' <w_import>-iwerk ,
        '        ' '0000'  ' ' 'RMIPM-WPGRP         ' <w_import>-wpgrp ,
        '        ' '0000'  ' ' 'RMIPM-AUART         ' <w_import>-auart ,
        '        ' '0000'  ' ' 'RMIPM-ILART         ' <w_import>-ilart ,
        '        ' '0000'  ' ' 'RMIPM-GEWERK        ' <w_import>-gewerk,
        '        ' '0000'  ' ' 'RMIPM-WERGW         ' <w_import>-wergw ,
        '        ' '0000'  ' ' 'RMIPM-GSBER         ' <w_import>-gsber ,
        '        ' '0000'  ' ' 'RMIPM-PRIOK         ' <w_import>-priok ,
        '        ' '0000'  ' ' 'RMIPM-PLNTY         ' <w_import>-plnty ,
        '        ' '0000'  ' ' 'RMIPM-PLNNR         ' <w_import>-plnnr ,
        '        ' '0000'  ' ' 'RMIPM-PLNAL         ' <w_import>-plnal ,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
        '        ' '0000'  ' ' 'RMIPM-ZYKL1         ' <w_import>-zykl1    ,
        '        ' '0000'  ' ' 'RMIPM-ZEIEH         ' <w_import>-zeieh    ,
        '        ' '0000'  ' ' 'RMIPM-PAK_TEXT      ' <w_import>-pak_text ,
        '        ' '0000'  ' ' 'RMIPM-OFFS1         ' '0',
*    '        ' '0000'  ' ' 'RMIPM-POINT         ' <wa_import>-point,
        'SAPLIWP3' '0201'	'X'	'                    '  '                                                                             ',
        '        ' '0000'	' '	'BDC_OKCODE          '  '=T\02                                                                        ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'	' '	'RMIPM-WPTXT         '  <w_import>-wptxt,
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'	' '	'RMIPM-PSTXT         '  <w_import>-wptxt,
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'	' '	'RIWO1-TPLNR         '  <w_import>-tplnr,
        '        ' '0000'	' '	'RIWO1-EQUNR         '  <w_import>-equnr,
        '        ' '0000'	' '	'RIWO1-BAUTL         '  <w_import>-bautl,
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
        '        ' '0000'	' '	'BDC_CURSOR          '  'RMIPM-PRIOK                                                                  ',
        '        ' '0000'	' '	'RMIPM-IWERK         '  <w_import>-iwerk,
        '        ' '0000'	' '	'RMIPM-WPGRP         '  <w_import>-wpgrp,
        '        ' '0000'	' '	'RMIPM-AUART         '  <w_import>-auart,
        '        ' '0000'	' '	'RMIPM-ILART         '  <w_import>-ilart,
        '        ' '0000'	' '	'RMIPM-GEWERK        '  <w_import>-gewerk,
        '        ' '0000'	' '	'RMIPM-WERGW         '  <w_import>-wergw,
        '        ' '0000'	' '	'RMIPM-GSBER         '  <w_import>-gsber,
        '        ' '0000'	' '	'RMIPM-PRIOK         '  <w_import>-priok,
        '        ' '0000'	' '	'RMIPM-PLNTY         '  <w_import>-plnty,
        '        ' '0000'	' '	'RMIPM-PLNNR         '  <w_import>-plnnr,
        '        ' '0000'	' '	'RMIPM-PLNAL         '  <w_import>-plnal,
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                8011SUBSCREEN_BODY1                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          '  'SAPLIWP3                                0205SUBSCREEN_CYCLE                  ',
        '        ' '0000'	' '	'RMIPM-ZYKL1         '  <w_import>-zykl1,
        '        ' '0000'	' '	'RMIPM-ZEIEH         '  <w_import>-zeieh,
        '        ' '0000'	' '	'RMIPM-PAK_TEXT      '  <w_import>-pak_text,
        '        ' '0000'	' '	'RMIPM-OFFS1         '  ' 0 ',
*    '        ' '0000'  ' ' 'RMIPM-POINT         '  <wa_import>-point,
        'SAPLIWP3' '0201'  'X' '                    ' '                                                                             ',
        '        ' '0000'  ' ' 'BDC_OKCODE          ' '/00                                                                          ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'  ' ' 'RMIPM-WPTXT         ' <w_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'  ' ' 'RMIPM-PSTXT         ' <w_import>-wptxt,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'  ' ' 'RIWO1-TPLNR         ' <w_import>-tplnr,
        '        ' '0000'  ' ' 'RIWO1-EQUNR         ' <w_import>-equnr,
        '        ' '0000'  ' ' 'RIWO1-BAUTL         ' <w_import>-bautl,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
        '        ' '0000'  ' ' 'RMIPM-IWERK         ' <w_import>-iwerk ,
        '        ' '0000'  ' ' 'RMIPM-WPGRP         ' <w_import>-wpgrp ,
        '        ' '0000'  ' ' 'RMIPM-AUART         ' <w_import>-auart ,
        '        ' '0000'  ' ' 'RMIPM-ILART         ' <w_import>-ilart ,
        '        ' '0000'  ' ' 'RMIPM-GEWERK        ' <w_import>-gewerk,
        '        ' '0000'  ' ' 'RMIPM-WERGW         ' <w_import>-wergw ,
        '        ' '0000'  ' ' 'RMIPM-GSBER         ' <w_import>-gsber ,
        '        ' '0000'  ' ' 'RMIPM-PRIOK         ' <w_import>-priok ,
        '        ' '0000'  ' ' 'RMIPM-PLNTY         ' <w_import>-plnty ,
        '        ' '0000'  ' ' 'RMIPM-PLNNR         ' <w_import>-plnnr ,
        '        ' '0000'  ' ' 'RMIPM-PLNAL         ' <w_import>-plnal ,
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                8012SUBSCREEN_BODY1                  ',
        '        ' '0000'  ' ' 'BDC_SUBSCR          ' 'SAPLIWP3                                0116SUBSCREEN_PARAMETER              ',
        '        ' '0000'  ' ' 'BDC_CURSOR          ' 'RMIPM-CALL_CONFIRM                                                           ',
        '        ' '0000'  ' ' 'RMIPM-VSPOS         ' <w_import>-vspos,
        '        ' '0000'  ' ' 'RMIPM-HORIZ         ' <w_import>-horiz,
        '        ' '0000'  ' ' 'RMIPM-TOPOS         ' <w_import>-topos,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*        '        ' '0000'  ' ' 'RMIPM-ABRHO         ' '
        '        ' '0000'  ' ' 'RMIPM-ABRHO         ' <w_import>-abrho,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                            ',
        '        ' '0000'  ' ' 'RMIPM-HUNIT         ' <w_import>-hunit,
        '        ' '0000'  ' ' 'RMIPM-VSNEG         ' <w_import>-vsneg,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*        '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' 'X',
        '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' <w_import>-call_confirm,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )                                                                    ',
        '        ' '0000'  ' ' 'RMIPM-TONEG         ' <w_import>-toneg,
*'        ' '0000'  ' ' 'RMIPM-SFAKT         ' 'RMIPM-SFAKT',
*      '        ' '0000'  ' ' 'RMIPM-STADT         ' <W_IMPORT>-STADT,
        'SAPLIWP3' '0201'	'X'	'                    ' '                                                                             ',
        '        ' '0000'	' '	'BDC_OKCODE          ' '=BU                                                                          ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6000SUBSCREEN_HEAD                   ',
        '        ' '0000'	' '	'RMIPM-WPTXT         ' <w_import>-wptxt,
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8002SUBSCREEN_MITEM                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8022SUBSCREEN_BODY2                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6005SUBSCREEN_MAINT_ITEM_TEXT        ',
        '        ' '0000'	' '	'RMIPM-PSTXT         ' <w_import>-wptxt,
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWO1                                0100SUBSCREEN_ITEM_1                 ',
        '        ' '0000'	' '	'RIWO1-TPLNR         ' <w_import>-tplnr,
        '        ' '0000'	' '	'RIWO1-EQUNR         ' <w_import>-equnr,
        '        ' '0000'	' '	'RIWO1-BAUTL         ' <w_import>-bautl,
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                0500SUBSCREEN_ITEM_2                 ',
        '        ' '0000'	' '	'RMIPM-IWERK         ' <w_import>-iwerk ,
        '        ' '0000'	' '	'RMIPM-WPGRP         ' <w_import>-wpgrp ,
        '        ' '0000'	' '	'RMIPM-AUART         ' <w_import>-auart ,
        '        ' '0000'	' '	'RMIPM-ILART         ' <w_import>-ilart ,
        '        ' '0000'	' '	'RMIPM-GEWERK        ' <w_import>-gewerk,
        '        ' '0000'	' '	'RMIPM-WERGW         ' <w_import>-wergw ,
        '        ' '0000'	' '	'RMIPM-GSBER         ' <w_import>-gsber ,
        '        ' '0000'	' '	'RMIPM-PRIOK         ' <w_import>-priok ,
        '        ' '0000'	' '	'RMIPM-PLNTY         ' <w_import>-plnty ,
        '        ' '0000'	' '	'RMIPM-PLNNR         ' <w_import>-plnnr ,
        '        ' '0000'	' '	'RMIPM-PLNAL         ' <w_import>-plnal ,
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                6003SUBSCREEN_MLAN_ITEM              ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8001SUBSCREEN_MPLAN                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                8012SUBSCREEN_BODY1                  ',
        '        ' '0000'	' '	'BDC_SUBSCR          ' 'SAPLIWP3                                0116SUBSCREEN_PARAMETER              ',
        '        ' '0000'	' '	'BDC_CURSOR          ' 'RMIPM-CALL_CONFIRM                                                           ',
        '        ' '0000'	' '	'RMIPM-VSPOS         ' <w_import>-vspos,
        '        ' '0000'	' '	'RMIPM-HORIZ         ' <w_import>-horiz,
        '        ' '0000'	' '	'RMIPM-TOPOS         ' <w_import>-topos,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
*        '        ' '0000'	' '	'RMIPM-ABRHO         ' <wa_import>-abrho,
        '        ' '0000'	' '	'RMIPM-ABRHO         ' <w_import>-abrho,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
        '        ' '0000'	' '	'RMIPM-HUNIT         ' <w_import>-hunit,
        '        ' '0000'	' '	'RMIPM-VSNEG         ' <w_import>-vsneg,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*        '        ' '0000'  ' ' 'RMIPM-CALL_CONFIRM  ' 'X',
        '        ' '0000' ' ' 'RMIPM-CALL_CONFIRM  ' <w_import>-call_confirm,
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
        '        ' '0000'	' '	'RMIPM-TONEG         ' <w_import>-toneg,
*'        ' '0000'  ' ' 'RMIPM-SFAKT         ' 'RMIPM-SFAKT',
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
*        '        ' '0000'  ' ' 'RMIPM-STADT         ' <w_import>-stadt,
** Início - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
*        '        ' '0000' ' ' 'RMIPM-STADT         ' <w_import>-szaeh.
        '        ' '0000' ' ' 'RMIPM-STADT         ' lv_szaeh.
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC ) - RGS - 24.07.2024
** Fim - IR174667 - 3000006158 ( Ajuste no Mapeamento BDC )
        ENDIF.

        CLEAR p_erro.
        PERFORM zf_call_transaction USING 'IP41' CHANGING p_erro.

        IF p_erro IS INITIAL.
          READ TABLE it_msg INTO DATA(w_msg) INDEX 1.
          <w_import>-warpl = w_msg-msgv1.
          <w_import>-uname = sy-uname.
          <w_import>-erdat = sy-datum.

          IF <w_import>-zeieh EQ 'H' OR <w_import>-zeieh EQ 'KM'.
          ELSE.
            <w_import>-szaeh = |{ <w_import>-szaeh+6(4) }{ <w_import>-szaeh+3(2) }{ <w_import>-szaeh(2) }|.
            <w_import>-stadt = <w_import>-szaeh.
          ENDIF.
          MOVE-CORRESPONDING <w_import> TO zpmt005.
          MODIFY zpmt005.
          <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
          <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        ENDIF.
      ENDLOOP.

      CLEAR contnum.
      LOOP AT it_import ASSIGNING FIELD-SYMBOL(<_import>).
        IF <_import>-warpl IS NOT INITIAL AND <_import>-status = '@08@'.

          <_import>-warpl = |{ <_import>-warpl ALPHA = IN }|.
          DO.
            SELECT COUNT(*)
              FROM mpla
              WHERE warpl EQ <_import>-warpl.
            IF sy-subrc IS INITIAL.
*              PERFORM SHDB_IP10 USING <_IMPORT>-WARPL <_IMPORT>-SZAEH <_IMPORT>-STADT <_IMPORT>-ZEIEH CHANGING P_IP10.
              EXIT.
              ADD 1 TO contnum.
              IF contnum EQ 40.
                EXIT.
              ENDIF.
            ENDIF.
          ENDDO.

          IF p_ip10 IS NOT INITIAL.
            <_import>-warpl = |{ <_import>-warpl ALPHA = OUT }|.
            <_import>-ip10 = p_ip10.
            APPEND <_import> TO it_ip10.
          ENDIF.
        ELSE.
          <_import>-warpl = |{ <_import>-warpl ALPHA = OUT }|.
          CONTINUE.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ELSE.

    CLEAR contnum.
    LOOP AT gt_ip10 ASSIGNING FIELD-SYMBOL(<s_ip10>).
      <s_ip10>-warpl = |{ <s_ip10>-warpl ALPHA = IN }|.

      PERFORM shdb_ip10_hr USING <s_ip10>-warpl <s_ip10>-szaeh CHANGING p_ip10.

      IF p_ip10 IS NOT INITIAL.
        <s_ip10>-warpl = |{ <s_ip10>-warpl ALPHA = OUT }|.
        <s_ip10>-status = p_ip10.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1397   text
*      -->P_1398   text
*      -->P_1399   text
*      -->P_1400   text
*      -->P_1401   text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  APPEND VALUE #(
                program   = p_program
                dynpro    = p_dynpro
                dynbegin  = p_start
                fnam      = p_fnam
                fval      = p_fval
  ) TO ti_bdcdata.

ENDFORM.

FORM zf_call_transaction USING p_trans CHANGING p_erro.

  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  DATA: wl_cont    TYPE sy-tabix,
        wl_mode(1).

*  FREE IT_MSG .
  CLEAR wl_mode.
  CLEAR wl_cont.
  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
                           MODE wl_mode
                           MESSAGES INTO it_msg.

  CLEAR: wl_cont.

  IF line_exists( it_msg[ msgtyp = 'A' ] ).
    p_erro = abap_true.
  ELSE.
    IF line_exists( it_msg[ msgtyp = 'E' ] ).
      p_erro = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.
*&-------

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verificar_change.

  DATA: gv_equnr        TYPE equi-equnr.
  DATA: wl_diimpt TYPE diimpt.

  LOOP AT gt_change ASSIGNING FIELD-SYMBOL(<w_change>).

    IF <w_change>-wptxt IS INITIAL.
      CLEAR sdydo_text_element.
      CONCATENATE sdydo_text_element'Informar texto do plano' INTO sdydo_text_element SEPARATED BY space.
      <w_change>-tstatus = sdydo_text_element.
      <w_change>-status = '@0A@'.
      <w_change>-equnr = |{ <w_change>-equnr ALPHA = OUT }|.
      CONTINUE.
    ENDIF.

    IF <w_change>-warpl IS NOT INITIAL.
      <w_change>-warpl = |{ <w_change>-warpl ALPHA = IN }|.

      SELECT SINGLE *
      FROM mpos
      INTO @DATA(w_mpos)
        WHERE warpl EQ @<w_change>-warpl.

      IF  w_mpos IS INITIAL.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Plano não existe' INTO sdydo_text_element SEPARATED BY space.
        <w_change>-warpl = |{ <w_change>-warpl ALPHA = OUT }|.
        <w_change>-tstatus = sdydo_text_element.
        <w_change>-status = '@0A@'.
        <w_change>-warpl = |{ <w_change>-warpl ALPHA = OUT }|.
        CLEAR w_mpos.
        CONTINUE.
      ELSE.
        CLEAR w_mpos.
      ENDIF.
    ENDIF.

    IF <w_change>-equnr IS NOT INITIAL.
      <w_change>-equnr = |{ <w_change>-equnr ALPHA = IN }|.

      SELECT SINGLE *
      FROM equi
      INTO @DATA(w_equi)
        WHERE equnr EQ @<w_change>-equnr.

      IF  w_equi IS INITIAL.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Equipamento não existe' INTO sdydo_text_element SEPARATED BY space.
        <w_change>-equnr = |{ <w_change>-equnr ALPHA = OUT }|.
        <w_change>-tstatus = sdydo_text_element.
        <w_change>-status = '@0A@'.
        <w_change>-equnr = |{ <w_change>-equnr ALPHA = OUT }|.
        CLEAR w_equi.
        CONTINUE.
      ELSE.
        CLEAR w_equi.
      ENDIF.
    ENDIF.

*  Verificar se a lista de tarefas existe.
    IF <w_change>-plnnr IS NOT INITIAL AND <w_change>-plnal IS NOT INITIAL.
      SELECT SINGLE *
      FROM plko
      INTO @DATA(w_plko)
       WHERE plnty EQ @<w_change>-plnty
        AND  plnnr EQ @<w_change>-plnnr
        AND  plnal EQ @<w_change>-plnal.

      IF w_plko IS INITIAL.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Lista não cadastrada' INTO sdydo_text_element SEPARATED BY space.
        <w_change>-tstatus = sdydo_text_element.
        <w_change>-status = '@0A@'.
        <w_change>-equnr = |{ <w_change>-equnr ALPHA = OUT }|.
        CLEAR w_plko.
        CONTINUE.
      ELSE.
        CLEAR w_plko.
      ENDIF.
    ENDIF.

    <w_change>-equnr = |{ <w_change>-equnr ALPHA = OUT }|.
    <w_change>-status = '@08@'.
    CLEAR sdydo_text_element.
    CONCATENATE sdydo_text_element'Erro não encontrado' INTO sdydo_text_element SEPARATED BY space.
    <w_change>-tstatus = sdydo_text_element.

  ENDLOOP.

  MOVE-CORRESPONDING  gt_change[] TO it_status2[].

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verificar_list.
*  Verificar inconsistencia das informações.

  DATA: gv_equnr        TYPE equi-equnr.
  DATA: wl_diimpt TYPE diimpt.

  LOOP AT it_import ASSIGNING FIELD-SYMBOL(<w_import>).
    CLEAR w_mpla.
    CLEAR w_zpmt005.
    CLEAR sdydo_text_element.
    <w_import>-plnal = |{ <w_import>-plnal ALPHA = IN }|.
*    Verificar se o plano ja existe com a mesmas caracteristicas.
    IF <w_import>-equnr IS NOT INITIAL.
      <w_import>-equnr = |{ <w_import>-equnr ALPHA = IN }|.

      SELECT SINGLE *
      FROM zpmt005
      INTO w_zpmt005
        WHERE tplnr EQ <w_import>-tplnr
         AND  equnr EQ <w_import>-equnr
         AND  mptyp EQ <w_import>-mptyp
         AND  iwerk EQ <w_import>-iwerk
         AND  plnnr EQ <w_import>-plnnr
         AND  plnal EQ <w_import>-plnal.
    ELSE.
      SELECT SINGLE *
    FROM zpmt005
    INTO  w_zpmt005
      WHERE tplnr EQ <w_import>-tplnr
       AND  mptyp EQ <w_import>-mptyp
       AND  iwerk EQ <w_import>-iwerk
       AND  plnnr EQ <w_import>-plnnr
       AND  plnal EQ <w_import>-plnal.
    ENDIF.

    IF w_zpmt005 IS NOT INITIAL.

*      VERIFICAR STATUS DO PLANO.
      w_zpmt005-warpl = |{ w_zpmt005-warpl ALPHA = IN }|.
      SELECT SINGLE *
      FROM mpla
      INTO w_mpla
        WHERE warpl EQ w_zpmt005-warpl.
      PERFORM status_check_plano USING w_mpla-objnr return_code CHANGING ijstat.
      IF ijstat EQ 'I0001'.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Ja existe plano cadastrado' INTO sdydo_text_element SEPARATED BY space.
        <w_import>-tstatus = sdydo_text_element.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        FREE w_zpmt005.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Verificar categoria do plano.
    IF <w_import>-mptyp IS NOT INITIAL AND <w_import>-mptyp EQ 'PM' OR <w_import>-mptyp EQ 'MO' OR <w_import>-mptyp EQ 'MI'.
      SELECT SINGLE *
      FROM t399w
      INTO @DATA(w_t399w)
      WHERE mptyp EQ @<w_import>-mptyp.
      IF w_t399w IS INITIAL.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Categoria não existe' INTO sdydo_text_element SEPARATED BY space.
        <w_import>-tstatus = sdydo_text_element.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        CLEAR w_t399w.
        CONTINUE.
      ELSE.
        CLEAR w_t399w.
      ENDIF.
    ELSE.
      CLEAR sdydo_text_element.
      CONCATENATE sdydo_text_element'Informar a categoria do plano' INTO sdydo_text_element SEPARATED BY space.
      <w_import>-tstatus = sdydo_text_element.
      <w_import>-status = '@0A@'.
      <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
      <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
      CONTINUE.
    ENDIF.

    IF <w_import>-wptxt IS INITIAL.
      CLEAR sdydo_text_element.
      CONCATENATE sdydo_text_element'Informar texto do plano' INTO sdydo_text_element SEPARATED BY space.
      <w_import>-tstatus = sdydo_text_element.
      <w_import>-status = '@0A@'.
      <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
      <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
      CONTINUE.
    ENDIF.

    IF <w_import>-tplnr IS NOT INITIAL.
*  Verificar local instalação existe.
      SELECT SINGLE *
      FROM iloa
      INTO @DATA(w_iloa)
        WHERE tplnr EQ @<w_import>-tplnr.

      IF w_iloa IS INITIAL.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Local Inst não existe' INTO sdydo_text_element SEPARATED BY space.
        <w_import>-tstatus = sdydo_text_element.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        CLEAR w_iloa.
        CONTINUE.
      ELSE.
        CLEAR w_iloa.
      ENDIF.
    ELSE.
      CLEAR sdydo_text_element.
      CONCATENATE sdydo_text_element'Informar local de instalação' INTO sdydo_text_element SEPARATED BY space.
      <w_import>-tstatus = sdydo_text_element.
      <w_import>-status = '@0A@'.
      <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
      <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
      CONTINUE.
    ENDIF.

*    Verificar equipamento existe.
    IF <w_import>-equnr IS NOT INITIAL.
      <w_import>-equnr = |{ <w_import>-equnr ALPHA = IN }|.

      SELECT SINGLE *
      FROM equi
      INTO @DATA(w_equi)
        WHERE equnr EQ @<w_import>-equnr.

      IF  w_equi IS INITIAL.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Equipamento não existe' INTO sdydo_text_element SEPARATED BY space.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-tstatus = sdydo_text_element.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        CLEAR w_equi.
        CONTINUE.
      ELSE.
        CLEAR w_equi.
      ENDIF.
*    ELSE.
*      CLEAR sdydo_text_element.
*      CONCATENATE sdydo_text_element'Informar o equipamento' INTO sdydo_text_element SEPARATED BY space.
*      <w_import>-tstatus = sdydo_text_element.
*      <w_import>-status = '@0A@'.
*      <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
*      <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
*      CONTINUE.


*    Verificar se o local pertence ao equipamento.
      SELECT *
      FROM equz
      INTO TABLE @DATA(t_equz)
       WHERE equnr EQ @<w_import>-equnr.

      IF  t_equz IS NOT INITIAL.
        SORT t_equz DESCENDING BY equnr datbi.
        READ TABLE t_equz ASSIGNING FIELD-SYMBOL(<w_equz>) INDEX 1.

        SELECT SINGLE *
        FROM iloa
        INTO @DATA(wa_iloa)
         WHERE iloan EQ @<w_equz>-iloan.
        IF wa_iloa-tplnr NE <w_import>-tplnr.
          CLEAR sdydo_text_element.
          CONCATENATE sdydo_text_element'Local inform não pertence ao equipamento' INTO sdydo_text_element SEPARATED BY space.
          <w_import>-tstatus = sdydo_text_element.
          <w_import>-status = '@0A@'.
          <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
          <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
          FREE t_equz.
          CLEAR wa_iloa.
          CONTINUE.
        ELSE.
          CLEAR wa_iloa.
        ENDIF.
      ELSE.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        CONTINUE.
      ENDIF.
*  Verificar se o ponto de medição existe
      IF <w_import>-zeieh IS NOT INITIAL.
        IF <w_import>-zeieh EQ 'H' OR <w_import>-zeieh EQ 'KM'.
          IF <w_import>-point IS NOT INITIAL.
            FREE gt_diimpt.
            CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
              EXPORTING
                i_equnr   = <w_import>-equnr
              TABLES
                et_diimpt = gt_diimpt.
            SORT gt_diimpt BY: point mptyp atnam indtr.

            DELETE gt_diimpt[] WHERE atnam NE 'ODOMETRO'
                                AND  atnam NE 'HORIMETRO'
                                 OR  indtr NE abap_true
                                 OR  inact EQ abap_true.

            <w_import>-point = |{ <w_import>-point ALPHA = IN }|.


            READ TABLE gt_diimpt ASSIGNING FIELD-SYMBOL(<w_diimpt>) WITH KEY point = <w_import>-point.
            IF sy-subrc NE 0.
              <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
              <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
              CLEAR sdydo_text_element.
              CONCATENATE sdydo_text_element'Ponto de medição não existe' INTO sdydo_text_element SEPARATED BY space.
              <w_import>-tstatus = sdydo_text_element.
              <w_import>-status = '@0A@'.
              CONTINUE.
            ENDIF.
          ELSE.
            CLEAR sdydo_text_element.
            CONCATENATE sdydo_text_element'Informar o ponto de medição' INTO sdydo_text_element SEPARATED BY space.
            <w_import>-tstatus = sdydo_text_element.
            <w_import>-status = '@0A@'.
            <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
            <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
            CONTINUE.
          ENDIF.

        ENDIF.
      ELSE.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Informar unidade do ponto medição' INTO sdydo_text_element SEPARATED BY space.
        <w_import>-tstatus = sdydo_text_element.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        CONTINUE.
      ENDIF.
    ENDIF.

*  Verificar se a lista de tarefas existe.
    IF <w_import>-plnnr IS NOT INITIAL AND <w_import>-plnal IS NOT INITIAL.
      SELECT SINGLE *
      FROM plko
      INTO @DATA(w_plko)
       WHERE plnnr EQ @<w_import>-plnnr
        AND  plnal EQ @<w_import>-plnal.

      IF w_plko IS INITIAL.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Lista não cadastrada' INTO sdydo_text_element SEPARATED BY space.
        <w_import>-tstatus = sdydo_text_element.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        CLEAR w_plko.
        CONTINUE.
      ELSE.
        CLEAR w_plko.
      ENDIF.

    ELSE.
      CLEAR sdydo_text_element.
      CONCATENATE sdydo_text_element'Informar a lista de tarefas e numerador do grupo de lista' INTO sdydo_text_element SEPARATED BY space.
      <w_import>-tstatus = sdydo_text_element.
      <w_import>-status = '@0A@'.
      <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
      <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
      CONTINUE.
    ENDIF.

*    Verificar se posição do contador / Data Inicio esta preenchida.
    IF <w_import>-szaeh IS NOT INITIAL.
      CONDENSE <w_import>-szaeh.
      IF <w_import>-zeieh EQ 'H' OR <w_import>-zeieh EQ 'KM'.
      ELSE.
        IF <w_import>-zeieh EQ 'MÊS' OR <w_import>-zeieh EQ 'MêS' OR <w_import>-zeieh EQ 'mês'.
          <w_import>-zeieh = 'mes'.
        ENDIF.

*        <w_import>-szaeh = |{ <w_import>-szaeh+6(4) }{ <w_import>-szaeh+3(2) }{ <w_import>-szaeh(2) }|.
*        <w_import>-stadt = <w_import>-szaeh.
      ENDIF.
    ELSE.
      IF <w_import>-zeieh EQ 'H' OR <w_import>-zeieh EQ 'KM'.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Informar a posição inicial' INTO sdydo_text_element SEPARATED BY space.
        <w_import>-tstatus = sdydo_text_element.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
        CONTINUE.
      ELSE.
        CLEAR sdydo_text_element.
        CONCATENATE sdydo_text_element'Informar da data de inicio' INTO sdydo_text_element SEPARATED BY space.
        <w_import>-tstatus = sdydo_text_element.
        <w_import>-status = '@0A@'.
        <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
        CONTINUE.
      ENDIF.
    ENDIF.

*    Pegando a posição total do contador para verificar se esta correto.
    IF <w_import>-point IS NOT INITIAL.
      DATA : no_beleg LIKE sy-subrc,
             fltp     TYPE imrc_totac.

      CALL FUNCTION 'MEASUREM_POINT_LAST_VALUE'
        EXPORTING
          i_point           = <w_import>-point
        IMPORTING
          e_wa_point        = wa_impt
          e_point_txt       = point_txt
          e_wa_value        = wa_imrg
          e_no_value        = no_beleg
        EXCEPTIONS
          pointer_not_found = 01.

      CASE sy-subrc.
        WHEN 0.
          IF no_beleg IS INITIAL.
            IF NOT wa_imrg-readg IS INITIAL.
              PERFORM fltp_char_conversion_pak_f40
                 USING fltp_char
                       wa_imrg-readg
                       wa_impt-msehi.
              <w_import>-pos_tot_cont  =  fltp_char+14(8).
              CONDENSE <w_import>-pos_tot_cont.
              CONDENSE <w_import>-szaeh.
              <w_import>-pos_tot = <w_import>-pos_tot_cont.
              <w_import>-pos_inic = <w_import>-szaeh.

              IF <w_import>-pos_inic > <w_import>-pos_tot.
                CLEAR sdydo_text_element.
                CONCATENATE sdydo_text_element'Posição cont maior q a posição anterior' INTO sdydo_text_element SEPARATED BY space.
                <w_import>-tstatus = sdydo_text_element.
                <w_import>-status = '@0A@'.
                <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
                <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDIF.

    <w_import>-point = |{ <w_import>-point ALPHA = OUT }|.
    <w_import>-equnr = |{ <w_import>-equnr ALPHA = OUT }|.
    <w_import>-status = '@08@'.
    CLEAR sdydo_text_element.
    CONCATENATE sdydo_text_element'Erro não encontrado' INTO sdydo_text_element SEPARATED BY space.
    <w_import>-tstatus = sdydo_text_element.
  ENDLOOP.

  MOVE-CORRESPONDING  it_import[] TO it_status[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIF_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modif_screen .

  LOOP AT SCREEN.
    CASE  screen-name.

      WHEN 'BNT_EXCEL'.
        IF it_import[] IS NOT INITIAL.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'BNT_VERIF'.
        IF it_import[] IS INITIAL.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'BTN_LIMPAR'.
        IF it_import[] IS INITIAL.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'BTN_DEL'.
        IF it_import[] IS INITIAL.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.


      WHEN 'BNT_EXEC'.

        DATA: p_erro TYPE char5.
        CLEAR p_erro.

        IF it_status[] IS NOT INITIAL.
          LOOP AT it_status.
            IF it_status-status = '@0A@'.
              ADD 1 TO p_erro.
            ENDIF.
          ENDLOOP.

          IF p_erro IS NOT INITIAL. "OR IT_IP10[] IS NOT INITIAL.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSE.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.

*        IF IT_IP10[] IS NOT INITIAL.
*          SCREEN-INPUT = 0.
*          MODIFY SCREEN.
*        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIMPAR_PLANILHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_planilha .

  FREE: it_import[], gt_ip10[], gt_change[].
  CALL METHOD obj_alv_0120->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_LINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excluir_linha .

  DATA: p_resp,
        lv_msg         TYPE bapi_msg,
        it_select_rows TYPE lvc_t_row,
        wa_select_rows TYPE lvc_s_row.

  CLEAR: it_select_rows[], wa_select_rows.

  CALL METHOD obj_alv_0120->get_selected_rows
    IMPORTING
      et_index_rows = it_select_rows.

  IF it_select_rows[] IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING        "TITLEBAR = 'Confirmar'
        text_question         = 'Deseja realmente excluir a linha?'
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = p_resp.

    IF p_resp = 1.

      IF p_novo = abap_true.
        LOOP AT it_select_rows INTO wa_select_rows.
          LOOP AT it_import ASSIGNING FIELD-SYMBOL(<w_import>).
            IF sy-tabix = wa_select_rows-index.
              <w_import>-marc = 'X'.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        DELETE it_import[] WHERE marc EQ 'X'.
        IF sy-subrc = 0.
          MESSAGE s000(o0) WITH 'Informação excluida com sucesso' DISPLAY LIKE 'S'.
        ENDIF.

        CALL METHOD obj_alv_0120->refresh_table_display.
      ENDIF.

      IF p_change = abap_true.
        LOOP AT it_select_rows INTO wa_select_rows.
          LOOP AT gt_change ASSIGNING FIELD-SYMBOL(<w_change>).
            IF sy-tabix = wa_select_rows-index.
              <w_change>-marc = 'X'.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        DELETE gt_change[] WHERE marc EQ 'X'.
        IF sy-subrc = 0.
          MESSAGE s000(o0) WITH 'Informação excluida com sucesso' DISPLAY LIKE 'S'.
        ENDIF.

        CALL METHOD obj_alv_0120->refresh_table_display.
      ENDIF.

    ELSE.
      MESSAGE i026(sv)." WITH 'Selecione uma linha para excluir'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHDB_IP10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<WA_IMPORT>_WARPL  text
*      -->P_<WA_IMPORT>_SZAEH  text
*      -->P_<WA_IMPORT>_STADT  text
*----------------------------------------------------------------------*
FORM shdb_ip10  USING    p_warpl
                         p_szaeh
                         p_stadt
                         p_zeieh
                          CHANGING p_ip10.

  FREE ti_bdcdata[].
  FREE it_msg[].

  IF p_zeieh EQ 'H' OR p_zeieh EQ 'KM'.
    PERFORM f_bdc_data USING:

'        '      '    '      'T '     'IP10       '      '                                                                  ',
'SAPLIWP3'      '0140'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_CURSOR '      'RMIPM-WARPL'                                                     ,
'        '      '    '      '  '     'BDC_OKCODE '      ' /00                                                              ',
'        '      '    '      '  '     'RMIPM-WARPL'      p_warpl                                                            ,
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_OKCODE '      ' =NS                                                           ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                6000WPLANKOPF            ',
'        '      '    '      '  '     'BDC_CURSOR '      'RMIPM-WARPL'                                                       ,
'SAPLIWP3'      '7001'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_CURSOR '      'RMIPM-SZAEH'                                                            ,
'        '      '    '      '  '     'BDC_OKCODE '      ' =ENTR                                                            ',
'        '      '    '      '  '     'RMIPM-SZAEH'       p_szaeh                                                            ,
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8003SUBSCREEN_SCHEDULING ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8031SUBSCREEN_BODY       ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                0121SUBSCREEN_CALL       ',
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_OKCODE '      ' =BU                                                              ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                6000WPLANKOPF            ',
'        '      '    '      '  '     'BDC_CURSOR '      'RMIPM-WARPL'                                                       ,
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8003SUBSCREEN_SCHEDULING ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8031SUBSCREEN_BODY       ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                0121SUBSCREEN_CALL       '.

  ELSE.
    p_szaeh = |{ p_szaeh+6(2) }.{ p_szaeh+4(2) }.{ p_szaeh(4) }|.
    p_stadt = p_szaeh.

    PERFORM f_bdc_data USING:

'        '      '    '      'T '     'IP10       '      '                                                                  ',
'SAPLIWP3'      '0140'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_CURSOR '      'RMIPM-WARPL',
'        '      '    '      '  '     'BDC_OKCODE '      ' /00                                                              ',
'        '      '    '      '  '     'RMIPM-WARPL'      p_warpl                                                             ,
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_OKCODE '      ' =ST                                                              ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                6000WPLANKOPF            ',
'        '      '    '      '  '     'BDC_CURSOR '      ' RMIPM-WARPL                                                      ',
'SAPLIWP3'      '7001'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_CURSOR '      'RMIPM-SZAEH',
'        '      '    '      '  '     'BDC_OKCODE '      ' =ENTR                                                            ',
'        '      '    '      '  '     'RMIPM-SZAEH'       p_stadt                                                            ,
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8003SUBSCREEN_SCHEDULING ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8031SUBSCREEN_BODY       ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                0121SUBSCREEN_CALL       ',
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_OKCODE '      ' =BU                                                              ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                6000WPLANKOPF            ',
'        '      '    '      '  '     'BDC_CURSOR '      ' RMIPM-WARPL                                                      ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8003SUBSCREEN_SCHEDULING ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8031SUBSCREEN_BODY       ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                0121SUBSCREEN_CALL       '.

  ENDIF.

  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'IP10' CHANGING p_erro.

  IF p_erro IS INITIAL.
    CLEAR p_ip10.
    p_ip10 = '@08@'.
  ELSE.
    CLEAR p_ip10.
    p_ip10 = '@0A@'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FLTP_CHAR_CONVERSION_PAK_F40
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLTP_CHAR  text
*      -->P_WA_IMRG_READG  text
*      -->P_WA_IMPT_MSEHI  text
*----------------------------------------------------------------------*
FORM fltp_char_conversion_pak_f40 USING char_wert
                                        fltp_wert
                                        einheit.
  CLEAR char_wert.
  CHECK NOT einheit IS INITIAL.

  CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
    EXPORTING
      char_unit       = einheit
      decimals        = 0
      exponent        = 0
      fltp_value_si   = fltp_wert
      indicator_value = cc_x
      masc_symbol     = ' '
    IMPORTING
      char_value      = char_wert.

ENDFORM.                    "fltp_char_conversion_pak_f40
*&---------------------------------------------------------------------*
*&      Form  STATUS_CHECK_PLANO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_MPLA_OBJNR  text
*      -->P_RETURN_CODE  text
*      <--P_IJSTAT  text
*----------------------------------------------------------------------*
FORM status_check_plano  USING mpla_objnr return_code CHANGING z_status.

  CONSTANTS: y_i0013_lokz LIKE jest-stat  VALUE 'I0013',
             y_i0076_lovm LIKE jest-stat  VALUE 'I0076',
             y_i0320_inak LIKE jest-stat  VALUE 'I0320'.

  CLEAR z_status.

  CALL FUNCTION 'STATUS_READ'
    EXPORTING
      client      = sy-mandt
      objnr       = mpla_objnr
      only_active = 'X'
    TABLES
      status      = ijstat.

  z_status = ijstat.

  LOOP AT ijstat.
    CASE ijstat-stat.
      WHEN y_i0013_lokz.
        return_code = 1.
      WHEN y_i0076_lovm.
        return_code = 2.
        EXIT.
      WHEN y_i0320_inak.               "Inativo
        return_code = 3.
      WHEN OTHERS.
        return_code = 0.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHDB_IP10_HR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<S_IP10>_WARPL  text
*      -->P_<S_IP10>_SZAEH  text
*      <--P_P_IP10  text
*----------------------------------------------------------------------*
FORM shdb_ip10_hr  USING    p_warpl
                            p_szaeh
                   CHANGING p_ip10.


  FREE ti_bdcdata[].
  FREE it_msg[].


  PERFORM f_bdc_data USING:

'        '      '    '      'T '     'IP10       '      '                                                                  ',
'SAPLIWP3'      '0140'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_CURSOR '      p_warpl                                                      ,
'        '      '    '      '  '     'BDC_OKCODE '      ' /00                                                              ',
'        '      '    '      '  '     'RMIPM-WARPL'      p_warpl                                                            ,
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_OKCODE '      ' =NS                                                           ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                6000WPLANKOPF            ',
'        '      '    '      '  '     'BDC_CURSOR '      'RMIPM-WARPL'                                                       ,
'SAPLIWP3'      '7001'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_CURSOR '      p_szaeh                                                             ,
'        '      '    '      '  '     'BDC_OKCODE '      ' =ENTR                                                            ',
'        '      '    '      '  '     'RMIPM-SZAEH'       p_szaeh                                                            ,
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8003SUBSCREEN_SCHEDULING ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8031SUBSCREEN_BODY       ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                0121SUBSCREEN_CALL       ',
'SAPLIWP3'      '0103'      'X '     '           '      '                                                                  ',
'        '      '    '      '  '     '           '      '                                                                  ',
'        '      '    '      '  '     'BDC_OKCODE '      ' =BU                                                              ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                6000WPLANKOPF            ',
'        '      '    '      '  '     'BDC_CURSOR '      'RMIPM-WARPL'                                                       ,
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8003SUBSCREEN_SCHEDULING ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                8031SUBSCREEN_BODY       ',
'        '      '    '      '  '     'BDC_SUBSCR '      ' SAPLIWP3                                0121SUBSCREEN_CALL       '.
*

  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'IP10' CHANGING p_erro.

  IF p_erro IS INITIAL.
    CLEAR p_ip10.
    p_ip10 = '@08@'.
  ELSE.
    CLEAR p_ip10.
    p_ip10 = '@0A@'.
  ENDIF.

ENDFORM.
