*&---------------------------------------------------------------------*
*& Report ZGLR081
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Paulo Ferraz                                            &*
*& Data.....: 19.05.2024                                              &*
*& Descrição: MEP - Conciliação Investimento                          &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*
REPORT zglr081.

INCLUDE zglr081_top.
INCLUDE zglr081_mod.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_monat TYPE monat OBLIGATORY,
              p_exer  TYPE gjahr OBLIGATORY,
              p_ivtd  TYPE bukrs OBLIGATORY,
              p_ivda  TYPE bukrs OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM get_dados.
  PERFORM alv.

  CALL SCREEN 0100.


*&---------------------------------------------------------------------*
*& Form get_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_dados .

  DATA: go_mep TYPE REF TO zclfi_conc_eqpatr.
  DATA: gs_mep TYPE zsfi_mep_process.

  CLEAR: gs_mep.
  gs_mep-mes = p_monat.
  gs_mep-ano = p_exer.
  gs_mep-empresa_investida = p_ivda.
  gs_mep-empresa_investidora = p_ivtd.

  go_mep = NEW zclfi_conc_eqpatr( gs_mep ).
  go_mep->processa(
    IMPORTING
      et_alv1 = gt_alv1
      et_alv2 = gt_alv2  ).

ENDFORM.

FORM alv.


** Cria Splitter Container
  painel_control = NEW cl_gui_splitter_container( parent = cl_gui_container=>screen0
                                               no_autodef_progid_dynnr = abap_true
                                               rows = 1
                                               columns = 2 ).

  CREATE OBJECT dd.
*  EXPORTING
*    STYLE  =
*    BACKGROUND_COLOR =
*    BDS_STYLESHEET =
*    NO_MARGINS =

* marking container
  painel1 = painel_control->get_container( row = 1 column = 1 ).
  painel2 = painel_control->get_container( row = 1 column = 2 ).

  PERFORM alv1.
  PERFORM alv2.
  PERFORM heading.


ENDFORM.

FORM alv1.

  DATA l_title1              TYPE lvc_title.
  DATA lo_functions1        TYPE REF TO cl_salv_functions.
  DATA lr_columns1           TYPE REF TO cl_salv_columns.
  DATA lr_column1            TYPE REF TO cl_salv_column.
  DATA lr_display_settings1  TYPE REF TO cl_salv_display_settings.


  TRY.
      cl_salv_table=>factory( EXPORTING
                                  r_container    = painel1 "spl_right
                                IMPORTING
                                  r_salv_table   = go_alv1
                                CHANGING
                                  t_table        = gt_alv1 ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  lo_functions1 = go_alv1->get_functions( ).
  lo_functions1->set_all( abap_true ).

  lr_columns1 = go_alv1->get_columns( ).

  TRY.

      lr_column1 = lr_columns1->get_column( 'PARAMETRO' ).
      lr_column1->set_short_text( 'Agrup' ).
      lr_column1->set_medium_text( 'Agrupamento' ).
      lr_column1->set_long_text( 'Agrupamento' ).
      lr_column1->set_optimized( abap_true ).

    CATCH cx_salv_not_found.
      "handle exception
  ENDTRY.

  TRY.

      lr_column1 = lr_columns1->get_column( 'CONC_BRL' ).
      lr_column1->set_short_text( 'BRL' ).
      lr_column1->set_medium_text( 'BRL' ).
      lr_column1->set_long_text( 'BRL' ).
      lr_column1->set_optimized( abap_true ).

    CATCH cx_salv_not_found.
      "handle exception
  ENDTRY.

  TRY.

      lr_column1 = lr_columns1->get_column( 'CONC_USD' ).
      lr_column1->set_short_text( 'USD' ).
      lr_column1->set_medium_text( 'USD' ).
      lr_column1->set_long_text( 'USD' ).
      lr_column1->set_optimized( abap_true ).

    CATCH cx_salv_not_found.
      "handle exception
  ENDTRY.



  l_title1 = |Composição da equivalência sobre o Patrimonio Liquido|.
  lr_display_settings1 = go_alv1->get_display_settings( ).
  lr_display_settings1->set_list_header_size( '0' ). "0=l, 1=s, 2=m
  lr_display_settings1->set_list_header( l_title1 ).

  go_alv1->display( ).


ENDFORM.

FORM alv2.

  DATA: lo_functions2         TYPE REF TO cl_salv_functions.
  DATA l_title1              TYPE lvc_title.
  DATA lr_display_settings1  TYPE REF TO cl_salv_display_settings.
  DATA lr_columns1           TYPE REF TO cl_salv_columns.
  DATA lr_column1            TYPE REF TO cl_salv_column.


  TRY.
      cl_salv_table=>factory( EXPORTING
                                  r_container    = painel2 "spl_right
                                IMPORTING
                                  r_salv_table   = go_alv2
                                CHANGING
                                  t_table        = gt_alv2 ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  lo_functions2 = go_alv2->get_functions( ).
  lo_functions2->set_all( abap_true ).

  lr_columns1 = go_alv2->get_columns( ).

  TRY.

      lr_column1 = lr_columns1->get_column( 'PARAMETRO' ).
      lr_column1->set_short_text( 'Agrup' ).
      lr_column1->set_medium_text( 'Agrupamento' ).
      lr_column1->set_long_text( 'Agrupamento' ).
      lr_column1->set_optimized( abap_true ).

    CATCH cx_salv_not_found.
      "handle exception
  ENDTRY.

  TRY.

      lr_column1 = lr_columns1->get_column( 'CONC_BRL' ).
      lr_column1->set_short_text( 'BRL' ).
      lr_column1->set_medium_text( 'BRL' ).
      lr_column1->set_long_text( 'BRL' ).
      lr_column1->set_optimized( abap_true ).

    CATCH cx_salv_not_found.
      "handle exception
  ENDTRY.

  TRY.

      lr_column1 = lr_columns1->get_column( 'CONC_USD' ).
      lr_column1->set_short_text( 'USD' ).
      lr_column1->set_medium_text( 'USD' ).
      lr_column1->set_long_text( 'USD' ).
      lr_column1->set_optimized( abap_true ).

    CATCH cx_salv_not_found.
      "handle exception
  ENDTRY.


  l_title1 = |Conciliação Investimento|.
  lr_display_settings1 = go_alv2->get_display_settings( ).
  lr_display_settings1->set_list_header_size( '0' ). "0=l, 1=s, 2=m
  lr_display_settings1->set_list_header( l_title1 ).

  go_alv2->display( ).


ENDFORM.


FORM heading.

  DATA : head TYPE  sdydo_attribute
         .
  head = cl_dd_document=>heading .

  CALL METHOD dd->add_text
    EXPORTING
      text      = 'MATERIAL MASTER REPORT'
      sap_style = head.

  CALL METHOD dd->display_document
    EXPORTING
      container = 'HEADING'.

ENDFORM.
