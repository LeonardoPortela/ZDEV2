*&---------------------------------------------------------------------*
*& Report  ZPROGRAMA_PADRAO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir084 MESSAGE-ID zcarga.

"Tabelas
TABLES: zfit0164.

"Tipos
TYPES: t_fieldcat TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY,

       BEGIN OF ty_saida,
         icon(5)                   TYPE c,
         lifnr                     TYPE bsak-lifnr,
         lifnr_name                TYPE lfa1-name1,
         kunnr                     TYPE kna1-kunnr,
         kunnr_name                TYPE kna1-name1,
         transno                   TYPE zfit0164-transno,
         entrydate                 TYPE zfit0164-entrydate,
         status                    TYPE  zfit0164-status,
         seqitem                   TYPE zfit0164-seqitem,
         vendorexternalref         TYPE zfit0164-vendorexternalref,
         vendorreferencecode       TYPE zfit0164-vendorreferencecode,
         invoiceno                 TYPE zfit0164-invoiceno,
         invoicedate               TYPE zfit0164-invoicedate,
         actdate                   TYPE zfit0164-actdate,
         duedate                   TYPE zfit0164-duedate,
         exchangeratedate          TYPE zfit0164-exchangeratedate,
         exchangerate              TYPE zfit0164-exchangerate,
         oprbillsource             TYPE zfit0164-oprbillsource,
         currencyamount            TYPE zfit0164-currencyamount,
         currency                  TYPE zfit0164-currency,
         basecurrencyamount        TYPE zfit0164-basecurrencyamount,
         detail_transtype          TYPE zfit0164-detail_transtype,
         detail_companycode        TYPE zfit0164-detail_companycode,
         detail_oprbillcode        TYPE zfit0164-detail_oprbillcode,
         detail_lobcode            TYPE zfit0164-detail_lobcode,
         detail_ledgercode         TYPE zfit0164-detail_ledgercode,
         detail_aparcode           TYPE zfit0164-detail_aparcode,
         detail_currencyamount     TYPE zfit0164-detail_currencyamount,
         detail_memo               TYPE zfit0164-detail_memo,
         detail_basecurrencyamount TYPE zfit0164-detail_basecurrencyamount,
         detail_voyageno           TYPE zfit0164-detail_voyageno,
         detail_vesselcode         TYPE zfit0164-detail_vesselcode,
         augbl                     TYPE zfit0164-augbl,
         augdt                     TYPE zfit0164-augdt,
         dmbtr                     TYPE zfit0164-dmbtr,
         dmbe2                     TYPE zfit0164-dmbe2,
         hkont                     TYPE zfit0164-hkont,
         belnr                     TYPE zib_contabil_chv-belnr,
         belnr_estorno             TYPE zib_contabil_chv-belnr,
         belnr_erro(5)             TYPE c,
         ck_compensado             TYPE zfit0164-ck_compensado , "CBS - US64799
         kursf                     TYPE zfit0164-kursf , "CBS - US64799
         "type zfit0164-CK_ZIB_CONTABIL
         "type zfit0164-CK_STATUS_ENVIADO
         "type zfit0164-CK_ESTORNADO
         "type zfit0164-BELNR_ESTORNO
         dt_envio_sap              TYPE zfit0164-dt_envio_sap,
         hr_envio_sap              TYPE zfit0164-hr_envio_sap,
       END OF ty_saida.


"Contantes
CONSTANTS: gco_bukrs TYPE bukrs VALUE '0001'.

"Tabela Interna Global
DATA: git_0164  TYPE TABLE OF zfit0164,
      git_saida TYPE TABLE OF ty_saida.

"Variáveis
DATA: gva_bukrs TYPE bukrs.

"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,

      git_fcat                    TYPE lvc_t_fcat,
      "git_filtro                  TYPE zif_screen_linha_filtro_t,

      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid.

"Ranges
RANGES:                  gra_bukrs FOR t001-bukrs.

"Field-Symbol
FIELD-SYMBOLS: <gfs_t001> TYPE t001.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_dat     FOR zfit0164-dt_envio_sap OBLIGATORY, "Data de envio para o SAP
                  p_invoic  FOR zfit0164-invoiceno,               "Número da Invoice
                  p_trans   FOR zfit0164-transno.
SELECTION-SCREEN: END OF BLOCK b1.

*selection-screen: begin of block B2 with frame title TEXT-002.
*parameters: R_ALL radiobutton group RAD1 default 'X', "Todos
*            R_PEN radiobutton group RAD1.             "Pendentes
*selection-screen: end of block B2.

*AT SELECTION-SCREEN ON P_BUKRS.
*  PERFORM FM_AT_SELECTION_SCREEN_P_BUKRS.

*AT SELECTION-SCREEN.
*  PERFORM FM_AT_SELECTION_SCREEN.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_report
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: zm_handle_hotspot_report.

    PERFORM user_command_0100 USING e_row_id e_column_id es_row_no.

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_EMPRESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_seleciona_empresa .


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  "perform FM_FILTROS.
  CALL SCREEN 0100.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

  TYPES: ty_rg_lifnr TYPE RANGE OF lfa1-lifnr,
         ty_rg_kunnr TYPE RANGE OF kna1-kunnr.

  DATA: lva_kunnr TYPE kunnr,
        lva_lifnr TYPE lifnr.

  IF ( git_0164[] IS NOT INITIAL ).

    DATA(rg_lifnr) = VALUE ty_rg_lifnr( FOR _0164 IN git_0164[] (
        sign = 'I' option = 'EQ' low = |{ _0164-vendorexternalref ALPHA = IN }| ) ).
    SORT rg_lifnr[] BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rg_lifnr[] COMPARING low.
    DELETE rg_lifnr[] WHERE low IS INITIAL.

    DATA(rg_kunnr) = VALUE ty_rg_kunnr( FOR _0164 IN git_0164[] (
      sign = 'I' option = 'EQ' low = |{ _0164-vendorreferencecode ALPHA = IN }| ) ).
    SORT rg_kunnr[] BY low ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rg_kunnr[] COMPARING low.
    DELETE rg_kunnr[] WHERE low IS INITIAL.

    IF ( rg_lifnr[] IS NOT INITIAL ).

      SELECT la~lifnr, la~name1, lb~bukrs
        FROM lfa1 AS la
        LEFT JOIN lfb1 AS lb
          ON la~lifnr = lb~lifnr
          AND lb~bukrs = '0200'
        INTO TABLE @DATA(lit_lfa1)
        WHERE la~lifnr IN @rg_lifnr[].

      SORT lit_lfa1[] BY lifnr ASCENDING.

    ENDIF.

    IF ( rg_kunnr[] IS NOT INITIAL ).

      SELECT ka~kunnr, ka~name1, kb~bukrs
        FROM kna1 AS ka
        LEFT JOIN knb1 AS kb
        ON ka~kunnr = kb~kunnr
        AND kb~bukrs = '0200'
        INTO TABLE @DATA(lit_kna1)
        WHERE ka~kunnr IN @rg_kunnr[].
      SORT lit_kna1[] BY kunnr ASCENDING.

    ENDIF.

    "ICON_GREEN_LIGHT
    "ICON_YELLOW_LIGHT

    LOOP AT git_0164[] INTO DATA(lwa_0164).

      DATA(lva_chave_zib) = |{ lwa_0164-transno }{ lwa_0164-actdate+0(4) }|.

      APPEND INITIAL LINE TO git_saida[] ASSIGNING FIELD-SYMBOL(<lfs_saida>).

      SELECT SINGLE * FROM zib_contabil
        INTO @DATA(lwa_zib)
        WHERE obj_key = @lva_chave_zib.

      IF ( sy-subrc = 0 ).

        SELECT SINGLE * FROM zib_contabil_chv
          INTO @DATA(lwa_zib_chv)
          WHERE obj_key = @lva_chave_zib.

        IF ( sy-subrc = 0 ).

          <lfs_saida>-belnr = lwa_zib_chv-belnr.
          <lfs_saida>-icon  = icon_complete.

        ELSE.

          SELECT SINGLE * FROM zib_contabil_err
            INTO @DATA(lwa_zib_err)
            WHERE obj_key = @lva_chave_zib.
          IF ( sy-subrc = 0 ).
            <lfs_saida>-icon = icon_defect.
            <lfs_saida>-belnr_erro = icon_history.
          ELSE.
            <lfs_saida>-icon = icon_time_ina.
          ENDIF.

        ENDIF.

      ENDIF.

      IF ( ( lwa_0164-status = 'R' ) OR ( lwa_0164-status = 'X' ) ) AND ( lwa_0164-belnr_estorno IS NOT INITIAL ).
        <lfs_saida>-icon  = icon_complete.
      ELSEIF ( ( lwa_0164-status = 'R' ) OR ( lwa_0164-status = 'X' ) ) AND ( lwa_0164-belnr_estorno IS INITIAL ).
        <lfs_saida>-icon = icon_time_ina.
      ENDIF.

      lva_kunnr = |{ lwa_0164-vendorreferencecode ALPHA = IN }|.
      lva_lifnr = |{ lwa_0164-vendorexternalref ALPHA = IN }|.

      READ TABLE lit_kna1 INTO DATA(lwa_kna1) WITH KEY kunnr = lva_kunnr.
      IF ( lwa_kna1 IS NOT INITIAL ) AND ( lwa_kna1-bukrs IS NOT INITIAL ).
        <lfs_saida>-kunnr = lwa_kna1-kunnr.
        <lfs_saida>-kunnr_name = lwa_kna1-name1.
      ELSEIF ( lwa_kna1 IS NOT INITIAL ) AND ( lwa_kna1-bukrs IS INITIAL ).
        <lfs_saida>-kunnr = lwa_kna1-kunnr.
        <lfs_saida>-kunnr_name = 'Customer not found for Company 0200'.
      ENDIF.

      READ TABLE lit_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = lva_lifnr.
      IF ( lwa_lfa1 IS NOT INITIAL ) AND ( lwa_lfa1-bukrs IS NOT INITIAL ).
        <lfs_saida>-lifnr = lwa_lfa1-lifnr.
        <lfs_saida>-lifnr_name = lwa_lfa1-name1.
      ELSEIF ( lwa_lfa1 IS NOT INITIAL ) AND ( lwa_lfa1-bukrs IS INITIAL ).
        <lfs_saida>-lifnr = lwa_lfa1-lifnr.
        <lfs_saida>-lifnr_name = 'Supplier not found for Company 0200'.
      ENDIF.

      MOVE-CORRESPONDING lwa_0164 TO <lfs_saida>.

      CLEAR: lva_chave_zib, lwa_zib, lwa_zib_chv, lwa_zib_err, lva_kunnr, lva_lifnr,
             lwa_kna1, lwa_lfa1.

    ENDLOOP.

  ELSE.

    MESSAGE TEXT-003 TYPE 'I'.
    EXIT.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
FORM fm_dados_seleciona .
  SELECT * FROM zfit0164 INTO TABLE git_0164[]
    WHERE transno       IN p_trans[]
      AND invoiceno     IN p_invoic[]
      AND dt_envio_sap  IN p_dat[].
  SORT git_0164[] BY transno ASCENDING.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM fm_at_selection_screen .

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN_P_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen_p_bukrs .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'. " WITH 'Padrao'.

  PERFORM fm_criar_objetos.

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
    WHEN 'SEND'.
      PERFORM fm_send_vip.
    WHEN 'RESEND'.
      PERFORM fm_resend_vip.
    WHEN 'REFRESH'.
      CLEAR: git_saida[], git_0164[].
      PERFORM fm_start_of_selection.
      "PERFORM fm_end_of_selection.
      PERFORM fm_criar_objetos.
  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FM_ALV
*&---------------------------------------------------------------------*
FORM fm_criar_objetos.

  DATA : lva_layout TYPE lvc_s_layo.

  lva_layout-sel_mode = 'A'.


  PERFORM fm_cria_fieldcat.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'VIP Integration Report'
       "i_filtros = git_filtro "VALUE zif_screen_linha_filtro_t( ( parametro = 'Nome' valor = 'Marcus Luciano Costa Bárbara' ) ( parametro = 'Data Posição' valor = '22/07/2020' ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.

    SET HANDLER lcl_event_receiver=>zm_handle_hotspot_report FOR gob_gui_alv_grid.

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = lva_layout
      CHANGING
        it_outtab                     = git_saida
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


  ELSE.

    gob_gui_alv_grid->refresh_table_display( ).

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_FIELDCAT
*&---------------------------------------------------------------------*
FORM fm_cria_fieldcat.

  DATA(git_fieldcat_dados) = VALUE t_fieldcat(
      ( fieldname = 'icon'                      coltext ='Status'                  col_opt = 'X' just = 'C' )
      "( fieldname = 'lifnr'                     coltext ='Vendor'                  col_opt = 'X' just = 'C' )
      ( fieldname = 'vendorexternalref'         coltext ='Vendor Externalref'      col_opt = 'X' just = 'C' )
      ( fieldname = 'lifnr_name'                coltext ='Vendor Name'             col_opt = 'X' just = 'C' )
      "( fieldname = 'kunnr'                     coltext ='Customer'                col_opt = 'X' just = 'C' )
      ( fieldname = 'vendorreferencecode'       coltext ='Vendor Referencecode'    col_opt = 'X' just = 'C' )
      ( fieldname = 'kunnr_name'                coltext ='Customer Name'           col_opt = 'X' just = 'C' )
      ( fieldname = 'transno'                   coltext ='Transaction N.'          outputlen = '15' just = 'C' ) "col_opt = 'X'
      ( fieldname = 'belnr'                     coltext ='Accounting Document'     col_opt = 'X' just = 'C' hotspot = 'X' )
      ( fieldname = 'belnr_erro'                coltext ='Accounting Error'        col_opt = 'X' just = 'C' hotspot = 'X' )
      ( fieldname = 'belnr_estorno'             coltext ='Acc. Reversal Document'  col_opt = 'X' just = 'C' hotspot = 'X' )
      ( fieldname = 'invoiceno'                 coltext = 'Invoice N.'             col_opt = 'X' just = 'C' )
      ( fieldname = 'invoicedate'               coltext ='Invoice Date'            col_opt = 'X' just = 'C' )
      ( fieldname = 'entrydate'                 coltext ='Entry Date'              col_opt = 'X' just = 'C' )
      ( fieldname = 'seqitem'                   coltext ='Seqitem'                 col_opt = 'X' just = 'C' )
      ( fieldname = 'actdate'                   coltext ='Actdate'                 col_opt = 'X' just = 'C' )
      ( fieldname = 'duedate'                   coltext ='Duedate'                 col_opt = 'X' just = 'C' )
      ( fieldname = 'exchangeratedate'          coltext ='Exchange R.Date'         col_opt = 'X' just = 'C' )
      ( fieldname = 'exchangerate'              coltext ='Exchange Rate'           col_opt = 'X' just = 'C' )
      ( fieldname = 'oprbillsource'             coltext ='Opr Bill Source'         col_opt = 'X' just = 'C' )
      ( fieldname = 'currencyamount'            coltext ='Currency Amount'         col_opt = 'X' just = 'C' )
      ( fieldname = 'currency'                  coltext ='Currency'                col_opt = 'X' just = 'C' )
      ( fieldname = 'basecurrencyamount'        coltext ='Base Currency'           col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_transtype'          coltext ='Det. Transtype'          col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_companycode'        coltext ='Det. Companycode'        col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_oprbillcode'        coltext ='Det. Oprbillcode'        col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_lobcode'            coltext ='Det. Lobcode'            col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_ledgercode'         coltext ='Det. Ledgercode'         col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_aparcode'           coltext ='Det. Aparcode'           col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_currencyamount'     coltext ='Det. CurrencyAmount'     col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_memo'               coltext ='Det. Memo'               col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_basecurrencyamount' coltext ='Det. Basecurrencyamount' col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_voyageno'           coltext ='detail_voyageno'         col_opt = 'X' just = 'C' )
      ( fieldname = 'detail_vesselcode'         coltext ='Det. Vesselcode'         col_opt = 'X' just = 'C' )
      ( fieldname = 'augbl'                     coltext ='Compesation Doc'         col_opt = 'X' just = 'C' hotspot = 'X' )
      ( fieldname = 'augdt'                     coltext ='Compesation Doc Date'    col_opt = 'X' just = 'C' )
      ( fieldname = 'dmbtr'                     coltext ='Amount local Curr.'      col_opt = 'X' just = 'C' )
      ( fieldname = 'dmbe2'                     coltext ='Amount local Curr.2'     col_opt = 'X' just = 'C' )
      ( fieldname = 'hkont'                     coltext ='Gen. ledger Count'       col_opt = 'X' just = 'C' )

      ( fieldname = 'dt_envio_sap'              coltext ='Received in SAP'         col_opt = 'X' just = 'C' )
      ( fieldname = 'hr_envio_sap'              coltext ='Hr in SAP'               col_opt = 'X' just = 'C' ) ).

  git_fcat[] = git_fieldcat_dados[].

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*FORM fm_filtros.
*
*  FREE: git_filtro.
*
*  LOOP AT SCREEN.
*    git_filtro = VALUE #(
*      ( parametro = 'Date:' valor = |{ p_dat-low }{ p_dat-high }| )
*      ( parametro = 'Invoice:' valor = p_invoic-low )
*    ).
*  ENDLOOP.
*ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0100
*&---------------------------------------------------------------------*
FORM user_command_0100 USING e_row_id TYPE lvc_s_row
                             p_e_column_id TYPE lvc_s_col
                             p_es_eow_no TYPE lvc_s_roid.

  DATA: lva_bukrs TYPE bukrs VALUE '0200',
        lva_gjahr TYPE gjahr.

  READ TABLE git_saida[] INTO DATA(lwa_saida) INDEX e_row_id-index.

  CHECK ( sy-subrc = 0 ).

  CASE p_e_column_id-fieldname.
    WHEN 'belnr'.
      IF lwa_saida-belnr IS NOT INITIAL.

        lva_gjahr = lwa_saida-actdate+0(4).

        SET PARAMETER ID 'BLN' FIELD lwa_saida-belnr.
        SET PARAMETER ID 'BUK' FIELD lva_bukrs.
        SET PARAMETER ID 'GJR' FIELD lva_gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'belnr_estorno'.
      IF lwa_saida-belnr_estorno IS NOT INITIAL.

        lva_gjahr = lwa_saida-actdate+0(4).

        SET PARAMETER ID 'BLN' FIELD lwa_saida-belnr_estorno.
        SET PARAMETER ID 'BUK' FIELD lva_bukrs.
        SET PARAMETER ID 'GJR' FIELD lva_gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'augbl'.
      IF lwa_saida-augbl IS NOT INITIAL.

        lva_gjahr = lwa_saida-actdate+0(4).

        SET PARAMETER ID 'BLN' FIELD lwa_saida-augbl.
        SET PARAMETER ID 'BUK' FIELD lva_bukrs.
        "SET PARAMETER ID 'GJR' FIELD lva_gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'belnr_erro'.
      IF lwa_saida-belnr_erro IS NOT INITIAL.
        DATA(lva_chave_zib) = |{ lwa_saida-transno }{ lwa_saida-actdate+0(4) }|.
        PERFORM f_exibe_erro_zib USING lva_chave_zib.
      ENDIF.
  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ERRO_ZIB
*&---------------------------------------------------------------------*
FORM f_exibe_erro_zib  USING    p_lva_chave_zib.

  TYPES: BEGIN OF ty_zib_err,
           obj_key        TYPE zib_contabil_err-obj_key,
           dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
           hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
           message        TYPE zib_contabil_err-message,
         END OF ty_zib_err.
  DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

  CHECK ( p_lva_chave_zib IS NOT INITIAL ).

  SELECT obj_key, dt_atualizacao, hr_atualizacao, message
     FROM zib_contabil_err INTO TABLE @lit_zib_err
    WHERE obj_key = @p_lva_chave_zib.
  IF ( sy-subrc = 0 ).

    cl_demo_output=>new(
      )->begin_section( `ZIB_CONTABIL_ERR:`
      )->write_text( |Errors found when creating the accounting document: \n|
      ")->WRITE_DATA( SY-DATUM
      )->write_data( lit_zib_err[]
      )->end_section(
      )->display( ).

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_SEND_VIP
*&---------------------------------------------------------------------*
FORM fm_send_vip.
*** BUG - 91927 - CBRAND
* Retornar o código original

  gob_gui_alv_grid->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(lit_rows)
  ).

  CHECK ( lit_rows[] IS NOT INITIAL ).

  LOOP AT lit_rows[] INTO DATA(lwa_rows).

    READ TABLE git_saida[] INTO DATA(lwa_saida) INDEX lwa_rows-index.

    UPDATE zfit0164
       SET ck_status_enviado = ''
       WHERE transno    = lwa_saida-transno
         AND entrydate  = lwa_saida-entrydate
         AND status     = lwa_saida-status
         AND seqitem    = lwa_saida-seqitem.

  ENDLOOP.

  MESSAGE 'The record will be resent.' TYPE 'S'.
  EXIT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_RESEND_VIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_resend_vip .

  DATA: gva_xml_result        TYPE xstring,
        gva_xml_string        TYPE string,
        lva_pagamento_parcial TYPE abap_bool,
        lva_data              TYPE zfit0164-augbl.


  DATA(obj_vip) = NEW zcl_integra_vip( ).

  gob_gui_alv_grid->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(lit_rows)
  ).

  CHECK ( lit_rows[] IS NOT INITIAL ).

  LOOP AT lit_rows[] INTO DATA(lwa_rows).

    READ TABLE git_saida[] INTO DATA(lwa_saida_aux) INDEX lwa_rows-index.

    LOOP AT git_saida[] INTO DATA(lwa_saida) WHERE transno   = lwa_saida_aux-transno
                                               AND entrydate = lwa_saida_aux-entrydate
                                               AND status    = lwa_saida_aux-status
                                               AND dmbtr     <> 0.

      "IF lwa_saida-currencyamount <> 0. "CBRAND - 18.05.2023

      DATA(lva_date) = |{ lwa_saida-augdt+0(4) }-{ lwa_saida-augdt+4(2) }-{ lwa_saida-augdt+6(2) }|.
      DATA(lva_date_ex) = |{ lwa_saida-exchangeratedate+0(4) }-{ lwa_saida-exchangeratedate+4(2) }-{ lwa_saida-exchangeratedate+6(2) }|.

      SHIFT lwa_saida-hkont LEFT DELETING LEADING '0'.

      DATA(lwa_simple_payment) = VALUE zfis_vip_simple_payment(
                        invoicetransno     = lwa_saida-transno
                        entrydate          = lva_date
                        actdate            = lva_date
                        externalrefid      = lwa_saida-augbl
                        bankcode           = lwa_saida-hkont
                        bankxcrate         = lwa_saida-kursf
                        bankcurr           = lwa_saida-currency
                        currencyamount     = lwa_saida-dmbtr "COND #( WHEN lva_pagamento_parcial = abap_true THEN lwa_saida-dmbtr ELSE lwa_saida-currencyamount )
                        currency           = lwa_saida-currency
                        basecurrencyamount = lwa_saida-dmbtr "COND #( WHEN lva_pagamento_parcial = abap_true THEN lwa_saida-dmbtr ELSE lwa_saida-currencyamount )
                        exchangeratedate   = lva_date_ex "lwa_saida-exchangeratedate
                        paymode            = 'WT'    ).

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lwa_simple_payment-currencyamount.

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lwa_simple_payment-basecurrencyamount.


      IF ( lwa_simple_payment IS NOT INITIAL ).

        CALL TRANSFORMATION zfi_vip_simple_payment
        SOURCE simplepayment = lwa_simple_payment
          RESULT XML gva_xml_result.

        CALL FUNCTION 'CRM_IC_XML_XSTRING2STRING'
          EXPORTING
            inxstring = gva_xml_result
          IMPORTING
            outstring = gva_xml_string.

        IF ( gva_xml_string IS NOT INITIAL ).
          obj_vip->send_xml_to_api( i_xml = gva_xml_string i_metodo = 'GET_SIMPLE_PAYMENT' ).
        ENDIF.
      ENDIF.

      CLEAR: lwa_saida.
      "      ENDIF.
    ENDLOOP.

*** US - 64790 - Inicio - CBRAND
*    UPDATE zfit0164
*       SET ck_status_enviado = ''
*       WHERE transno    = lwa_saida-transno
*         AND entrydate  = lwa_saida-entrydate
*         AND status     = lwa_saida-status
*         AND seqitem    = lwa_saida-seqitem.
*** US - 64790 - Fim - CBRAND

  ENDLOOP.

  "MESSAGE 'The record will be resent.' TYPE 'S'.
  MESSAGE 'Registros enviados > Document Sent' TYPE 'I'.
  EXIT.

*  DATA(obj_vip) = NEW zcl_integra_vip( ).
*
*  gob_gui_alv_grid->get_selected_rows(
*    IMPORTING
*      et_index_rows = DATA(lit_rows)
*  ).
*
*  CHECK ( lit_rows[] IS NOT INITIAL ).
*
*  LOOP AT lit_rows[] INTO DATA(lwa_rows).
*    READ TABLE git_saida[] INTO DATA(lwa_saida) INDEX lwa_rows-index.
*
*    SELECT *
*     FROM zfit0164
*         INTO TABLE @DATA(lit_zfit0164)
*    WHERE transno = @lwa_saida-transno
*      AND augbl <> ''.
*
*    IF sy-subrc = 0.
*
*
*
*        "obj_vip->get_simple_payment( i_transno = lwa_saida-transno  ).
*      ENDIF.
*
*      MESSAGE 'The record will be resent.' TYPE 'S'.
*      EXIT.
*
*    ENDLOOP.

ENDFORM.
