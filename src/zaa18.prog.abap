*&---------------------------------------------------------------------*
*& Report  ZAA18
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zaa18.


*&--------------------------------------------------------------------&*
*& Tabelas                                                            &*
*&--------------------------------------------------------------------&*

TABLES: anlc, anla.

TYPES: BEGIN OF ty_anla.
         INCLUDE TYPE anla.
       TYPES:
                vzugdt  TYPE char10,
                vaktiv  TYPE char10,
                icon    TYPE char05,
                message TYPE char50,
                return  TYPE char10,
              END OF ty_anla.

TYPES: BEGIN OF ty_anlc.
         INCLUDE TYPE anlc.
       TYPES:
                icon    TYPE char05,
                message TYPE char50,
                return  TYPE char30,
              END OF ty_anlc.

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
  case sy-subrc.
    when 0.
    when 1.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    when others.
  endcase.
END-OF-DEFINITION.



DATA:
  git_fcat         TYPE lvc_t_fcat,
  gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid,
  it_anla          TYPE TABLE OF ty_anla WITH HEADER LINE,
  it_anlc          TYPE TABLE OF ty_anlc WITH HEADER LINE,
  ws_anla          TYPE ty_anla,
  ws_anlc          TYPE ty_anlc.


*&--------------------------------------------------------------------&*
*& Parametro seleção                                                          &*
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: rb_anla RADIOBUTTON GROUP g1 USER-COMMAND abc DEFAULT 'X'. "Modificação ANLA
SELECTION-SCREEN COMMENT 05(08) text-003 FOR FIELD rb_anla.

SELECTION-SCREEN POSITION 15.
SELECTION-SCREEN COMMENT 18(08) text-002 FOR FIELD rb_anlc. "Modificar ANLC
PARAMETERS: rb_anlc RADIOBUTTON GROUP g1 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

*SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
*SELECT-OPTIONS: p_bukrs  FOR anla-bukrs OBLIGATORY, "Empresa
*                p_ano    FOR anlc-gjahr OBLIGATORY, "Ano
*                p_anln1  FOR anla-anln1 OBLIGATORY, "Imobilizado
*                p_anln2  FOR anla-anln2 OBLIGATORY, "Subnumero
*                p_afabe  FOR anlc-afabe OBLIGATORY, "Area Deprec
*                p_aktiv  FOR anla-aktiv, "Data Incorporação
*                p_kansw  FOR anlc-kansw, "Valor Aquisição
*                p_knafa  FOR anlc-knafa, "Valor Deprec.Inciio Exerc
*                p_nafag  FOR anlc-nafag. "Valor Deprec.Ano
*SELECTION-SCREEN: END OF BLOCK b3.

CLASS zcl_instrucao DEFINITION.

  PUBLIC SECTION.
    METHODS:

      get_excel
        RETURNING VALUE(r_value) TYPE rlgrap-filename,

      set_excel
        IMPORTING input TYPE char128,
      exc_excel,

      col RETURNING VALUE(r_value) TYPE kcd_ex_col_n,

      update_alna
        IMPORTING t_anla LIKE it_anla,

      update_alnc
        IMPORTING t_anlc LIKE  it_anlc.

ENDCLASS.

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

*    DATA(obj_inst) = NEW zcl_instrucao( ).

    CLEAR: ws_anla.
    FREE: it_anla,
          it_anlc.

    me->set_excel( me->get_excel( ) ).

    SORT it_excel BY row col.

    IF rb_anla IS NOT INITIAL. "Importar dados Arquivo ALNA
      CLEAR: ws_anlc.
      LOOP AT it_excel INTO DATA(wa_exce).
        CHECK wa_exce-row NE 1 AND wa_exce-row NE 1.
        CASE wa_exce-col.
          WHEN 1.   ws_anla-bukrs      = wa_exce-value.
          WHEN 2.   ws_anla-anln1      = wa_exce-value.
          WHEN 3.   ws_anla-anln2      = wa_exce-value.
          WHEN 4.   ws_anla-zujhr      = wa_exce-value.
          WHEN 5.   ws_anla-zuper      = wa_exce-value.
          WHEN 6.   ws_anla-vzugdt      = wa_exce-value.
          WHEN 7.   ws_anla-vaktiv      = wa_exce-value.


        ENDCASE.

        AT END OF row.
          APPEND ws_anla              TO it_anla.
          CLEAR: ws_anla, wa_exce.
        ENDAT.
      ENDLOOP.
    ENDIF.


    IF rb_anlc IS NOT INITIAL.  "Importar dados Arquivo ALNC
      CLEAR: ws_anlc.
      LOOP AT it_excel INTO wa_exce.
        CHECK wa_exce-row NE 1 AND wa_exce-row NE 1.
        CASE wa_exce-col.
          WHEN 1.   ws_anlc-bukrs      = wa_exce-value.
          WHEN 2.   ws_anlc-anln1      = wa_exce-value.
          WHEN 3.   ws_anlc-anln2      = wa_exce-value.
          WHEN 4.   ws_anlc-gjahr      = wa_exce-value.
          WHEN 5.   ws_anlc-afabe      = wa_exce-value.
          WHEN 6.
            REPLACE '.' IN wa_exce-value WITH ''.
            CONDENSE wa_exce-value NO-GAPS.

            REPLACE ',' IN wa_exce-value WITH '.'.
            CONDENSE wa_exce-value NO-GAPS.
            ws_anlc-kansw      = wa_exce-value.

          WHEN 7.
            REPLACE '.' IN wa_exce-value WITH ''.
            CONDENSE wa_exce-value NO-GAPS.

            REPLACE ',' IN wa_exce-value WITH '.'.
            CONDENSE wa_exce-value NO-GAPS.
            ws_anlc-knafa      = wa_exce-value.
          WHEN 8.
            REPLACE '.' IN wa_exce-value WITH ''.
            CONDENSE wa_exce-value NO-GAPS.

            REPLACE ',' IN wa_exce-value WITH '.'.
            CONDENSE wa_exce-value NO-GAPS.
            ws_anlc-nafag      = wa_exce-value.

        ENDCASE.

        AT END OF row.
          APPEND ws_anlc              TO it_anlc.
          CLEAR: ws_anlc, wa_exce.
        ENDAT.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD col.
    ADD 1 TO cont_col.
    r_value = cont_col.
  ENDMETHOD.


  METHOD update_alna.

    DATA: ws_anla TYPE ty_anla.
    DATA: ws_anlc  TYPE ty_anlc,
          zvg_date TYPE anla-zugdt.

    CHECK it_anla[] IS NOT INITIAL.

    LOOP AT it_anla ASSIGNING FIELD-SYMBOL(<ls_anla>).

      PERFORM f_lupa USING 'Iniciando modificação imbolizado' <ls_anla>-anln1 space.

      "Modificar informação ANLA.
      MOVE-CORRESPONDING <ls_anla> TO ws_anla.

      "Check empresa.
      IF <ls_anla>-bukrs IS INITIAL.
        <ls_anla>-icon = icon_red_light.
        <ls_anla>-message = |É obrigatório preenchimento empresa |.
        <ls_anla>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anla>-anln1 IS INITIAL.
        <ls_anla>-icon = icon_red_light.
        <ls_anla>-message = |É obrigatório preenchimento imobilizado|.
        <ls_anla>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anla>-anln2 IS INITIAL.
        <ls_anla>-icon = icon_red_light.
        <ls_anla>-message = |É obrigatório preenchimento subnumero|.
        <ls_anla>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anla>-zujhr IS INITIAL.
        <ls_anla>-icon = icon_red_light.
        <ls_anla>-message = |É obrigatório preenchimento ano|.
        <ls_anla>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anla>-zuper IS INITIAL.
        <ls_anla>-icon = icon_red_light.
        <ls_anla>-message = |É obrigatório preenchimento período lançamento de aquisição|.
        <ls_anla>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anla>-vzugdt IS INITIAL.
        <ls_anla>-icon = icon_red_light.
        <ls_anla>-message = |É obrigatório preenchimento data de referência|.
        <ls_anla>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anla>-vaktiv IS INITIAL.
        <ls_anla>-icon = icon_red_light.
        <ls_anla>-message = |É obrigatório preenchimento data de incorporação|.
        <ls_anla>-return = |E|.
        CONTINUE.
      ENDIF.


      CLEAR: zvg_date.
      zvg_date = |{ ws_anla-vzugdt+6(4) }{ ws_anla-vzugdt+3(2) }{ ws_anla-vzugdt(2) }|.
      <ls_anla>-zugdt = zvg_date.
      ws_anla-zugdt = zvg_date.

      CLEAR: zvg_date.
      zvg_date = |{ ws_anla-vaktiv+6(4) }{ ws_anla-vaktiv+3(2) }{ ws_anla-vaktiv(2) }|.
      <ls_anla>-aktiv = zvg_date.
      ws_anla-aktiv = zvg_date.

      IF ws_anla IS NOT INITIAL.
        MODIFY anla FROM ws_anla.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        IF sy-subrc EQ 0.
          <ls_anla>-icon = icon_green_light.
          <ls_anla>-message = |Imobilizado { <ls_anla>-anln1 } alterado com sucesso|.
          <ls_anla>-return = |S|.
        ELSE.
          <ls_anla>-icon = icon_red_light.
          <ls_anla>-message = |Erro ao modificar { <ls_anla>-anln1 }|.
          <ls_anla>-return = |E|.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD update_alnc.
    DATA: ws_anlc  TYPE anlc,
          zvg_date TYPE anla-zugdt.

    CHECK it_anlc[] IS NOT INITIAL.

    LOOP AT it_anlc ASSIGNING FIELD-SYMBOL(<ls_anlc>).
      "Modificar informação ANLC.
      MOVE-CORRESPONDING <ls_anlc> TO ws_anlc.

      "Check empresa.
      IF <ls_anlc>-bukrs IS INITIAL.
        <ls_anlc>-icon = icon_red_light.
        <ls_anlc>-message = |É obrigatório preenchimento empresa |.
        <ls_anlc>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anlc>-anln1 IS INITIAL.
        <ls_anlc>-icon = icon_red_light.
        <ls_anlc>-message = |É obrigatório preenchimento imobilizado|.
        <ls_anlc>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anlc>-anln2 IS INITIAL.
        <ls_anlc>-icon = icon_red_light.
        <ls_anlc>-message = |É obrigatório preenchimento subnumero|.
        <ls_anlc>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anlc>-gjahr IS INITIAL.
        <ls_anlc>-icon = icon_red_light.
        <ls_anlc>-message = |É obrigatório preenchimento ano|.
        <ls_anlc>-return = |E|.
        CONTINUE.
      ENDIF.

      IF <ls_anlc>-afabe IS INITIAL.
        <ls_anlc>-icon = icon_red_light.
        <ls_anlc>-message = |É obrigatório preenchimento area de avaliação|.
        <ls_anlc>-return = |E|.
        CONTINUE.
      ENDIF.

*      REPLACE '.' IN <ls_anlc>-kansw WITH ''.
*      CONDENSE <ls_anlc>-kansw NO-GAPS.
*
*      REPLACE ',' IN <ls_anlc>-kansw WITH '.'.
*      CONDENSE <ls_anlc>-kansw NO-GAPS.



      IF ws_anlc IS NOT INITIAL.
        MODIFY anlc FROM ws_anlc.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        IF sy-subrc EQ 0.
          <ls_anlc>-icon = icon_green_light.
          <ls_anlc>-message = |Imobilizado { <ls_anlc>-anln1 } alterado com sucesso|.
          <ls_anlc>-return = |S|.
        ELSE.
          <ls_anlc>-icon = icon_red_light.
          <ls_anlc>-message = |Erro ao modificar { <ls_anlc>-anln1 }|.
          <ls_anlc>-return = |E|.
        ENDIF.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


ENDCLASS.


START-OF-SELECTION.
  DATA(obj_inst) = NEW zcl_instrucao( ).
  obj_inst->exc_excel( ).
  obj_inst->update_alna( t_anla = it_anla ).
  obj_inst->update_alnc( t_anlc = it_anlc ).
  PERFORM fm_alv.
*&---------------------------------------------------------------------*
*&      Form  FM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_alv .

  CALL SCREEN 0100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.

  PERFORM fm_criar_objetos.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos .


  DATA: lva_data(22) TYPE c,
        w_layout     TYPE lvc_s_layo.

  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.
  FREE: git_fcat.

  PERFORM fm_cria_fieldcat.


  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Modificar dados imbolizado'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
*    SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.

    w_layout-cwidth_opt = abap_true.
    w_layout-zebra      = 'X'.
    w_layout-sel_mode   = 'A'.
    w_layout-col_opt    = abap_true.


    IF rb_anla IS NOT INITIAL.


      CALL METHOD gob_gui_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout                     = w_layout
          i_save                        = 'A'
          is_variant                    = gs_variant
        CHANGING
          it_outtab                     = it_anla[]
          it_fieldcatalog               = git_fcat
*         IT_SORT                       =
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

    ELSE.
      CALL METHOD gob_gui_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout                     = w_layout
          i_save                        = 'A'
          is_variant                    = gs_variant
        CHANGING
          it_outtab                     = it_anlc[]
          it_fieldcatalog               = git_fcat
*         IT_SORT                       =
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  IF rb_anla IS NOT INITIAL.

    TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
    git_fcat = VALUE lit_fieldcat_aux(
  ( fieldname ='BUKRS'        coltext = 'Empresa'                             col_opt = 'X' no_zero = '' hotspot = '' )
  ( fieldname ='ANLN1'        coltext = 'Imobilizado'                         col_opt = 'X' no_zero = '' )
  ( fieldname ='ANLN2'        coltext = 'Sub-num           '                  col_opt = 'X' no_zero = '' )
  ( fieldname ='ZUJHR'        coltext = 'Ano           '                      col_opt = 'X' no_zero = '' )
  ( fieldname ='ZUPER'        coltext = 'Período lançamento de aquisição'     col_opt = 'X' no_zero = '' )
  ( fieldname ='ZUGDT'        coltext = 'Data de referência'                  col_opt = 'X' no_zero = '' )
  ( fieldname ='AKTIV'        coltext = 'Data de incorporação'                col_opt = 'X' no_zero = '' )
  ( fieldname ='ICON'         coltext = 'Status'                              col_opt = 'X' no_zero = '' )
  ( fieldname ='MESSAGE'      coltext = 'Messagem'                            col_opt = 'X' no_zero = '' )
  ( fieldname ='RETURN'       coltext = 'Tipo msg'                            col_opt = 'X' no_zero = '' )
  ).

  ELSE.
    TYPES: lit_fieldcat TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
    git_fcat = VALUE lit_fieldcat(
  ( fieldname ='BUKRS'         coltext = 'Empresa'                             col_opt = 'X' no_zero = '' hotspot = '' )
  ( fieldname ='ANLN1'         coltext = 'Imobilizado'                         col_opt = 'X' no_zero = '' )
  ( fieldname ='ANLN2'         coltext = 'Sub-num           '                  col_opt = 'X' no_zero = '' )
  ( fieldname ='GJAHR'         coltext = 'Ano           '                      col_opt = 'X' no_zero = '' )
  ( fieldname ='AFABE'         coltext = 'Área de avaliação'                   col_opt = 'X' no_zero = '' )
  ( fieldname ='KANSW'         coltext = 'Custos acumulados'                   col_opt = 'X' no_zero = '' )
  ( fieldname ='KNAFA'         coltext = 'Depreciação normal acumulada'        col_opt = 'X' no_zero = '' )
  ( fieldname ='NAFAG'         coltext = 'Depreciação normal lançada do ano'   col_opt = 'X' no_zero = '' )
  ( fieldname ='ICON'          coltext = 'Status'                              col_opt = 'X' no_zero = '' )
  ( fieldname ='MESSAGE'       coltext = 'Messagem'                            col_opt = 'X' no_zero = '' )
  ( fieldname ='RETURN'        coltext = 'Tipo msg'                            col_opt = 'X' no_zero = '' )
  ).

  ENDIF.

ENDFORM.
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
*&      Form  F_LUPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0839   text
*      -->P_<LS_ANLA>_ANLN1  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM f_lupa USING p_msg1 p_msg2 p_msg3.
  DATA: vl_message(150) TYPE c.
  CLEAR vl_message.

  p_msg2 = |{ p_msg2 ALPHA = OUT }|.
  CONCATENATE p_msg1 p_msg2 p_msg3 INTO vl_message SEPARATED BY space.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 99
      text       = vl_message.

ENDFORM.
