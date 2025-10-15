*&---------------------------------------------------------------------*
*& Report  Z_SHDB_FB02
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_shdb_fb02.

TYPE-POOLS: pmst.

" SHDB
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF ti_bdcdata.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: wl_mode(1).

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs   TYPE bsid-bukrs,
              p_belnr   TYPE bsid-belnr,
              p_gjahr   TYPE bsid-gjahr,
              p_xref2   TYPE bseg-xref2,
              p_xref3   TYPE bseg-xref3,
              p_tipo(1),
              p_hbkid   TYPE bseg-hbkid. "BUG - 83955 - CBRAND
SELECTION-SCREEN: END OF BLOCK b1.

DATA:  p_erro(1).


START-OF-SELECTION.
*Inicio Alteração - Leandro Valentim Ferreira - 16.05.23 - 111042
  PERFORM altera_doc.
***  PERFORM executa_shdb.
*Fim Alteração - Leandro Valentim Ferreira - 16.05.23 - 111042

*&---------------------------------------------------------------------*
*&      Form  EXECUTA_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM executa_shdb .

*** BUG - 83955 - Inicio - CBRAND
* Deixei fixo caso exista outros processos que chamam esse JOB
* Anteriormente a AMAGGI só gerava boleto BBRA
  IF p_hbkid IS INITIAL.
    p_hbkid = 'BBRA'.
  ENDIF.
*** BUG - 83955 - Fim - CBRAND

  REFRESH ti_bdcdata.

  IF p_tipo = 'O'.
    PERFORM f_bdc_data USING:
          'SAPMF05L'  '0100'  'X'  ''               ' ',
          ''          ''      ''   'BDC_CURSOR'	    'RF05L-BELNR',
          ''          ''      ''   'BDC_OKCODE'	    '=AZ',
          ''          ''      ''   'RF05L-BELNR'    p_belnr,
          ''          ''      ''   'RF05L-BUKRS'    p_bukrs,
          ''          ''      ''   'RF05L-GJAHR'    p_gjahr,
          'SAPMF05L'  '0304'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '=ZK',
          'SAPMF05L'  '1303'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_CURSOR'	      'BSEG-XREF3',
          ''          ''      ''   'BDC_OKCODE'	      '=ENTR',
          ''          ''      ''   'BSEG-HBKID'	      p_hbkid, "'BBRA', " BUG - 83955
          ''          ''      ''   'BSEG-XREF2'	      p_xref2,
          ''          ''      ''   'BSEG-XREF3'	      p_xref3,
          'SAPMF05L'  '0304'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '=AE'.
  ELSE.
    PERFORM f_bdc_data USING:
        'SAPMF05L'  '0100'  'X'  ''               ' ',
        ''          ''      ''   'BDC_CURSOR'	    'RF05L-BELNR',
        ''          ''      ''   'BDC_OKCODE'	    '=AZ',
        ''          ''      ''   'RF05L-BELNR'    p_belnr,
        ''          ''      ''   'RF05L-BUKRS'    p_bukrs,
        ''          ''      ''   'RF05L-GJAHR'    p_gjahr,
        'SAPMF05L'  '0301'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '=ZK',
        'SAPMF05L'  '1301'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_CURSOR'	      'BSEG-XREF3',
        ''          ''      ''   'BDC_OKCODE'	      '=ENTR',
        ''          ''      ''   'BSEG-HBKID'	      p_hbkid," 'BBRA',  "BUG - 83955
        ''          ''      ''   'BSEG-XREF2'	      p_xref2,
        ''          ''      ''   'BSEG-XREF3'	      p_xref3,
        'SAPMF05L'  '0301'  'X'  ''                 ' ',
        ''          ''      ''   'BDC_OKCODE'	      '=AE'.
  ENDIF.
  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'FB02' CHANGING p_erro.
ENDFORM.                    " EXECUTA_SHDB


*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_ERRO     text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  REFRESH it_msg.

  wl_mode = 'E'.
  CALL TRANSACTION p_trans USING ti_bdcdata
       MODE wl_mode
       MESSAGES INTO it_msg.


  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    "ZF_CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  ALTERA_DOC
*&---------------------------------------------------------------------*
*       Busca documento Contábil e altera
*----------------------------------------------------------------------*
FORM altera_doc .

  DATA: t_bseg   TYPE TABLE OF bseg,
        t_accchg TYPE TABLE OF accchg,
        v_buzei	 TYPE	 bseg-buzei,
        v_bukrs  TYPE  accit-bukrs,
        v_belnr  TYPE  bseg-belnr,
        v_gjahr  TYPE  bseg-gjahr.

  v_bukrs = p_bukrs.
  v_belnr = p_belnr.
  v_gjahr = p_gjahr.

  CALL FUNCTION 'FI_DOCUMENT_READ'
    EXPORTING
      i_bukrs     = v_bukrs
      i_belnr     = v_belnr
      i_gjahr     = v_gjahr
    TABLES
      t_bseg      = t_bseg
    EXCEPTIONS
      wrong_input = 1
      not_found   = 2
      OTHERS      = 3.

  READ TABLE t_bseg INTO DATA(wl_bseg) WITH KEY bschl = '01'.
  IF sy-subrc NE 0.
    READ TABLE t_bseg INTO wl_bseg WITH KEY bschl = '09'.
  ENDIF.
  IF sy-subrc EQ 0.
    APPEND INITIAL LINE TO t_accchg ASSIGNING FIELD-SYMBOL(<fs_accchg>).
    <fs_accchg>-fdname = 'XREF2'.
    <fs_accchg>-newval = p_xref2.

    APPEND INITIAL LINE TO t_accchg ASSIGNING <fs_accchg>.
    <fs_accchg>-fdname = 'XREF3'.
    <fs_accchg>-newval = p_xref3.

    APPEND INITIAL LINE TO t_accchg ASSIGNING <fs_accchg>.
    <fs_accchg>-fdname = 'HBKID'.
    <fs_accchg>-newval = p_hbkid.

    v_buzei = wl_bseg-buzei.

    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_buzei              = v_buzei
        i_bukrs              = v_bukrs
        i_belnr              = v_belnr
        i_gjahr              = v_gjahr
      TABLES
        t_accchg             = t_accchg
      EXCEPTIONS
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        OTHERS               = 6.

    IF sy-subrc NE 0.
      p_erro = 'X'.
    ENDIF.
    CLEAR: t_accchg[],t_bseg[],v_bukrs,v_belnr,v_gjahr,v_buzei.
  ENDIF.
ENDFORM.
