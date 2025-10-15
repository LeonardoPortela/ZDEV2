*----------------------------------------------------------------------*
***INCLUDE LZGFSD001F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  PERFORM montar_estrutura USING:
   1  'ZSDT0041'   'INCO1'            'TL_SAIDA_EXEC' 'INCO1'     ' '  ' ',
   2  'ZSDT0041'   'SPART'            'TL_SAIDA_EXEC' 'SPART'     ' '  ' ',
   3  'ZSDT0041'   'AUART'            'TL_SAIDA_EXEC' 'AUART'     ' '  ' ',
   4  'ZSDT0041'   'WERKS'            'TL_SAIDA_EXEC' 'WERKS'     ' '  ' ',
   4  'ZSDT0041'   'VBELN'            'TL_SAIDA_EXEC' 'VBELN'     ' '  ' ',
   4  'ZSDT0041'   'MSG'              'TL_SAIDA_EXEC' 'MSG'       'Msg de bapi'   '170'.
ENDFORM.                    " MONTAR_LAYOUT

FORM montar_layout_1 .
  PERFORM montar_estrutura USING:
   1  'ZSDT0041'   'INCO1'            'TL_SAIDA_ALV' 'INCO1'          ' '             '04',
   2  'ZSDT0041'   'SPART'            'TL_SAIDA_ALV' 'SPART'          ' '             '03',
   3  'ZSDT0041'   'AUART'            'TL_SAIDA_ALV' 'AUART'          ' '             '05',
   4  'ZSDT0041'   'WERKS'            'TL_SAIDA_ALV' 'WERKS'          ' '             '15',
   5  'ZSDT0041'   'VBELN'            'TL_SAIDA_ALV' 'VBELN'          ' '             '06',
   6  'ZSDT0041'   'DESC_ABSOLUTO'    'TL_SAIDA_ALV' 'DESC_ABSOLUTO'  'Desc. Abs'     '',
   7  'ZSDT0041'   'VLRTOT'           'TL_SAIDA_ALV' 'VLRTOT'         'Valor Total'   ' ',
   8  'ZSDT0041'   'MSG'              'TL_SAIDA_ALV' 'MSG'            'Msg de bapi'   '170'.
ENDFORM.                    " MONTAR_LAYOUT

FORM montar_layout_.
  PERFORM montar_estrutura USING:
   1  'ZSDT0041'   'VBELN'            'TL_SAIDA_ALV' 'VBELN'          ' '             '10',
   2  'ZSDT0041'   'POSNR'            'TL_SAIDA_ALV' 'POSNR'          ' '             '06',
   3  'ZSDT0041'   'MSG'              'TL_SAIDA_ALV' 'MSG'            'Msg de bapi'   '170'.
ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen     = x_contador.
  ELSE.
    wa_estrutura-outputlen     =  p_outputlen.
  ENDIF.

  CASE p_field.
    WHEN 'BELNR'
      OR 'AWKEY'.
      wa_estrutura-hotspot = 'X'.
      wa_estrutura-key = 'X'.

  ENDCASE.


  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura

* Início - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
*&---------------------------------------------------------------------*
*&      Form  ZF_ESTORNO_ERRO_ZRFU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_estorno_erro_zrfu USING p_documento.

  DATA: vl_docfat    TYPE bapivbrk-ref_doc,
        is_cancelled TYPE bapivbrkout-cancelled,
        t_success    TYPE STANDARD TABLE OF bapivbrksuccess WITH HEADER LINE,
        t_return     TYPE STANDARD TABLE OF bapireturn1 WITH HEADER LINE,
        vl_docnum    TYPE j_1bnflin-docnum,
        v_docnum     TYPE j_1bnfe_active-docnum,
        vcandat      TYPE j_1bnfdoc-candat,
        vdocnum_est  TYPE j_1bdocnum,
        w_erro(1),
        fatura(1).

  vl_docfat = p_documento.

  "//Cancela fatura
  CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED' "#EC CI_USAGE_OK[2228098]
    EXPORTING
      billingdoc_number       = vl_docfat
    IMPORTING
      billingdoc_is_cancelled = is_cancelled.

  IF ( is_cancelled IS INITIAL ).
    "Cancela fatura
    CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
      EXPORTING
        billingdocument = vl_docfat
      TABLES
        return          = t_return         " bapireturn1 Table of Error Messages Entered
        success         = t_success.       " bapivbrksuccess Table of Successfully Processed Documents
  ENDIF.

  IF ( t_success[] IS NOT INITIAL ) OR ( is_cancelled IS NOT INITIAL ).

    SELECT SINGLE docnum
      FROM j_1bnflin
      INTO vl_docnum
      WHERE refkey = vl_docfat.

    CLEAR vcandat.
    SELECT SINGLE  candat
      FROM j_1bnfdoc
      INTO  vcandat
     WHERE docnum     = vl_docnum.

    IF vcandat IS INITIAL. "Documento Fiscal não está estornado ainda....
*
      "Verificar se documento esta autorizado na SEFAZ
      SELECT SINGLE docnum
       FROM j_1bnfe_active
       INTO v_docnum
       WHERE docnum     = vl_docnum
         AND docsta     = '1'
         AND cancel     = ''.

      IF sy-subrc NE 0. "Caso não esteja, forçar o cancelamento do documento fiscal, serviço que a bapi deveria ter feito e não fez.
        CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
          EXPORTING
            doc_number               = vl_docnum
            ref_type                 = space
            ref_key                  = space
            can_dat                  = sy-datum
          IMPORTING
            doc_number               = vdocnum_est
          EXCEPTIONS
            document_not_found       = 1
            cancel_not_possible      = 2
            nf_cancel_type_not_found = 3
            database_problem         = 4
            docum_lock               = 5
            nfe_cancel_simulation    = 6
            OTHERS                   = 7.

        IF sy-subrc EQ 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          w_erro = abap_true.
        ENDIF.
      ELSE.
        w_erro = abap_true.
      ENDIF.

      CHECK w_erro IS NOT INITIAL. "Não houve êxito na tentativa do cancelamento do Doc. Fiscal, e prosseguir para gravar o log. de erro.

*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      MESSAGE 'Impossível estorno de fatura. Danfe, não estornada!' TYPE  'I'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ZERA_QTDE_OV_ZFUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_zera_qtde_ov_zfut  USING p_vbeln.

  DATA: tl_schedule_lines  TYPE TABLE OF bapischdl  WITH HEADER LINE,
        tl_schedule_linesx TYPE TABLE OF bapischdlx WITH HEADER LINE,
        tl_bapisditm       TYPE TABLE OF bapisditm WITH HEADER LINE,
        tl_bapisditmx      TYPE TABLE OF bapisditmx WITH HEADER LINE,
        tl_return          TYPE TABLE OF bapiret2 WITH HEADER LINE,
        wl_orderheaderinx  TYPE bapisdh1x,
        wl_logic_switch    TYPE bapisdls,
        wl_vbeln           TYPE vbeln.

  wl_vbeln = p_vbeln.

  SELECT *
    FROM vbap
    INTO TABLE @DATA(vbap)
    WHERE vbeln EQ @wl_vbeln.

  SELECT *
    FROM vbep
    INTO TABLE @DATA(vbep)
    FOR  ALL ENTRIES IN @vbap
    WHERE vbeln EQ @vbap-vbeln.

  wl_orderheaderinx-updateflag = 'U'.

  LOOP AT vbap INTO DATA(wvbap).

    MOVE: 'U'            TO tl_bapisditmx-updateflag,
          wvbap-posnr    TO tl_bapisditmx-itm_number,
          'X'            TO tl_bapisditmx-target_qty,
          wvbap-posnr    TO tl_bapisditm-itm_number,
          0              TO tl_bapisditm-target_qty.

    APPEND tl_bapisditmx.
    APPEND tl_bapisditm.
    CLEAR: tl_bapisditm, tl_bapisditmx.

    LOOP AT vbep INTO DATA(w_vbep).

      MOVE: 'U'           TO tl_schedule_linesx-updateflag,

            wvbap-posnr   TO tl_schedule_linesx-itm_number,
            w_vbep-etenr  TO tl_schedule_linesx-sched_line,
            'X'           TO tl_schedule_linesx-req_qty,

            wvbap-posnr   TO tl_schedule_lines-itm_number,
            w_vbep-etenr  TO tl_schedule_lines-sched_line,
            0             TO tl_schedule_lines-req_qty.

      APPEND tl_schedule_linesx.
      APPEND tl_schedule_lines.
      CLEAR: tl_schedule_lines, tl_schedule_linesx.

    ENDLOOP.

"*---> 12/07/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = wvbap-vbeln
        order_header_inx = wl_orderheaderinx
      TABLES
        order_item_in    = tl_bapisditm
        order_item_inx   = tl_bapisditmx
        schedule_lines   = tl_schedule_lines
        schedule_linesx  = tl_schedule_linesx
        return           = tl_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ENDLOOP.

ENDFORM.

* Fim - Sara Oikawa - 14.10.2020 - 39781 - Melhorias Pacote 5
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_VLR_REAL
*&---------------------------------------------------------------------*
FORM f_update_vlr_real .

  IF zsde013-vlr_brl IS INITIAL.

    zsde013-vlr_brl = zsde013-vlr_dolar_proj * zsde013-taxa_curva.

  ENDIF.

  MODIFY gt_alv_9000
    FROM zsde013
      INDEX tc_alv-current_line
        TRANSPORTING vlr_brl.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REMOVE_UNICODE
*&---------------------------------------------------------------------*
FORM f_remove_unicode USING p_string TYPE string
                   CHANGING p_ret TYPE string.

  p_ret = p_string.

  REPLACE ALL OCCURRENCES OF 'u00C0' IN p_ret WITH 'À'.
  REPLACE ALL OCCURRENCES OF 'u00C1' IN p_ret WITH 'Á'.
  REPLACE ALL OCCURRENCES OF 'u00C2' IN p_ret WITH 'Â'.
  REPLACE ALL OCCURRENCES OF 'u00C3' IN p_ret WITH 'Ã'.
  REPLACE ALL OCCURRENCES OF 'u00C7' IN p_ret WITH 'Ç'.
  REPLACE ALL OCCURRENCES OF 'u00C8' IN p_ret WITH 'È'.
  REPLACE ALL OCCURRENCES OF 'u00C9' IN p_ret WITH 'É'.
  REPLACE ALL OCCURRENCES OF 'u00CA' IN p_ret WITH 'Ê'.
  REPLACE ALL OCCURRENCES OF 'u00CC' IN p_ret WITH 'Ì'.
  REPLACE ALL OCCURRENCES OF 'u00CD' IN p_ret WITH 'Í'.
  REPLACE ALL OCCURRENCES OF 'u00CE' IN p_ret WITH 'Î'.
  REPLACE ALL OCCURRENCES OF 'u00E0' IN p_ret WITH 'à'.
  REPLACE ALL OCCURRENCES OF 'u00E1' IN p_ret WITH 'á'.
  REPLACE ALL OCCURRENCES OF 'u00E2' IN p_ret WITH 'â'.
  REPLACE ALL OCCURRENCES OF 'u00E3' IN p_ret WITH 'ã'.
  REPLACE ALL OCCURRENCES OF 'u00E4' IN p_ret WITH 'ä'.
  REPLACE ALL OCCURRENCES OF 'u00E7' IN p_ret WITH 'ç'.
  REPLACE ALL OCCURRENCES OF 'u00E8' IN p_ret WITH 'è'.
  REPLACE ALL OCCURRENCES OF 'u00E9' IN p_ret WITH 'é'.
  REPLACE ALL OCCURRENCES OF 'u00EA' IN p_ret WITH 'ê'.
  REPLACE ALL OCCURRENCES OF 'u00EB' IN p_ret WITH 'ë'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FORMATA_BUKRS_BOLETA
*&---------------------------------------------------------------------*
FORM f_formata_bukrs_boleta USING p_bukrs TYPE bukrs
                         CHANGING p_cod TYPE string.

  DATA lv_ret TYPE c LENGTH 2.

  CHECK p_bukrs IS NOT INITIAL.

  lv_ret = p_bukrs+2(2).

  IF lv_ret(1) = '0'.

    lv_ret(1) = space.

  ENDIF.

  p_cod = lv_ret.

ENDFORM.
