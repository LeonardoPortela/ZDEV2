*&---------------------------------------------------------------------*
*& Report  ZSDR0008
*&
*&---------------------------------------------------------------------*
*& Impressão de Ordem de Venda
*&
*&---------------------------------------------------------------------*
*& Histórico de alterações
*& Autor          Request         Data
*& Marcos Faneli  DEVK937399      08.05.2014
*&---------------------------------------------------------------------*

REPORT  zsdr0008.

SELECTION-SCREEN: BEGIN OF SCREEN 999.
  PARAMETERS p_vlr AS CHECKBOX.
SELECTION-SCREEN: END OF SCREEN 999.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_vbeln LIKE vbak-vbeln OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK a1.

DATA: wa_vbak       TYPE vbak,
      wa_tvakt      TYPE tvakt,
      wa_tvkbt      TYPE tvkbt,
      it_vbap       TYPE TABLE OF zsds004,
      it_vbap2      TYPE TABLE OF zsds004,
      wa_vbap2      TYPE zsds004,
      wa_vbap       TYPE zsds004,
      wa_t052       TYPE t052,
      wa_t052u      TYPE t052u,
      it_vbkd       TYPE TABLE OF vbkd INITIAL SIZE 0 WITH HEADER LINE,
      it_ztext      TYPE TABLE OF ttext INITIAL SIZE 0 WITH HEADER LINE,
      wa_ztext      TYPE ttext,
      wa_vbkd       TYPE vbkd,
      wa_info_c     TYPE kna1,
      wa_tvko       TYPE tvko,
      wa_tvtwt      TYPE tvtwt,
      wa_tspat      TYPE tspat,
      wa_t001       TYPE t001,
      wa_werks      TYPE t001w,
      wa_vstel      TYPE t001w,
      wa_tcurt      TYPE tcurt,
      vl_formname   TYPE tdsfname,
      vl_name       TYPE rs38l_fnam,
      ls_options    TYPE ssfcompop,
      vg_cpf        TYPE c LENGTH 14,
      vg_cnpj       TYPE c LENGTH 18,
      cnpj          TYPE c LENGTH 18,
      tx_date       TYPE string,
      vg_name       LIKE thead-tdname,
      vg_language   LIKE thead-tdspras,
      vg_object     LIKE thead-tdobject,
      vg_line       TYPE TABLE OF tline INITIAL SIZE 0 WITH HEADER LINE,
      vg_linez      TYPE TABLE OF tline INITIAL SIZE 0 WITH HEADER LINE, "ztline initial size 0 with header line,
      it_matnr      TYPE TABLE OF tline INITIAL SIZE 0 WITH HEADER LINE,
      wa_linez      TYPE ztline,
      vg_tabix      TYPE sy-tabix,
      v_matant      TYPE vbap-matnr,
      it_konv       TYPE TABLE OF konv INITIAL SIZE 0 WITH HEADER LINE,
      wa_konv       TYPE konv,
      vl_total      TYPE vbap-netwr,
      vl_quantidade TYPE vbap-kwmeng,
      vl_unidade    TYPE konv-kmein,
      vl_preco_unit TYPE vbap-netpr,
      wekrs         TYPE kna1-kunnr.

START-OF-SELECTION.


******************************************************************"
*VERSÃO NOVA DO SMARTFORM SOMENTE PARA O INSUMOS                  "
******************************************************************"
  SELECT COUNT(*) FROM zsdt0041 WHERE vbeln EQ p_vbeln.           "
  IF sy-subrc IS INITIAL.                                         "
    DATA(ok_) = abap_true.                                        "
  ENDIF.                                                          "
  "
  SELECT COUNT(*) FROM zsdt0090 WHERE vbeln EQ p_vbeln.           "
  IF sy-subrc IS INITIAL.                                         "
    ok_ = abap_true.                                              "
  ENDIF.                                                          "
  "
  IF ok_ IS NOT INITIAL.                                          "
    CALL  SELECTION-SCREEN 999 ENDING AT 51 8 STARTING AT 3 3.    "
    "
    CALL FUNCTION 'Z_GERA_OV_CONTRATO'                            "
      EXPORTING                                                   "
        i_vbeln = p_vbeln                                         "
        i_vlr   = p_vlr.                                          "
    EXIT.                                                         "
  ENDIF.                                                          "
******************************************************************"
*VERSÃO NOVA DO SMARTFORM SOMENTE PARA O INSUMOS                  "
******************************************************************"



  SELECT SINGLE * INTO wa_vbak
    FROM vbak
   WHERE vbeln EQ p_vbeln.

  IF wa_vbak IS INITIAL.
    MESSAGE 'Ordem de Venda não encontrada!' TYPE 'S'.
    EXIT.
  ENDIF.

*---> 05/07/2022 - Migração S4 - DG
*  SELECT  * INTO TABLE IT_KONV
*    FROM KONV
*    WHERE KSCHL EQ 'PR00'
*    AND   KNUMV  EQ WA_VBAK-KNUMV.

  SELECT  * INTO TABLE @DATA(it_konv_aux)
    FROM v_konv
    WHERE kschl  EQ 'PR00'
    AND   knumv  EQ @wa_vbak-knumv.

  MOVE-CORRESPONDING it_konv_aux[] TO it_konv[].
*<--- 05/07/2022 - Migração S4 - DG


  SELECT SINGLE * INTO wa_tvkbt
    FROM tvkbt
    WHERE vkbur EQ wa_vbak-vkbur.


  SELECT SINGLE * INTO wa_tvakt
    FROM tvakt
    WHERE spras EQ 'PT'
    AND   auart EQ wa_vbak-auart.

  SELECT *
   FROM vbap
   INTO CORRESPONDING FIELDS OF TABLE it_vbap
 WHERE vbeln EQ p_vbeln.


  SELECT * INTO TABLE it_vbkd
    FROM vbkd
   WHERE vbeln EQ wa_vbak-vbeln.

  SELECT SINGLE * INTO wa_tcurt
    FROM tcurt
   WHERE spras EQ sy-langu
     AND waers EQ wa_vbak-waerk.

  IF NOT it_vbkd[] IS INITIAL.
    READ TABLE it_vbkd INTO wa_vbkd INDEX 1.

    IF NOT wa_vbkd-zterm IS INITIAL.

      SELECT SINGLE * INTO wa_t052u
        FROM t052u
       WHERE zterm EQ wa_vbkd-zterm
        AND  spras EQ 'PT' .


      IF NOT wa_t052u IS INITIAL.
        MOVE-CORRESPONDING wa_t052u TO wa_ztext.
      ENDIF.

    ENDIF.

  ENDIF.

  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      p_parceiro = wa_vbak-kunnr
      p_partype  = 'C'
    CHANGING
      wa_info_c  = wa_info_c.

  IF wa_info_c-stkzn IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
      EXPORTING
        input  = wa_info_c-stcd1
      IMPORTING
        output = vg_cnpj.

    wa_info_c-knurl = vg_cnpj.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = wa_info_c-stcd2
      IMPORTING
        output = vg_cpf.
    wa_info_c-knurl = vg_cpf.
  ENDIF.

  wa_info_c-txjcd = wa_info_c-txjcd(2).

  "Org. de Vendas
  SELECT SINGLE * INTO wa_tvko
    FROM tvko
   WHERE vkorg EQ wa_vbak-vkorg.

  SELECT SINGLE * INTO wa_t001
    FROM t001
   WHERE bukrs  EQ wa_tvko-bukrs.

  "Canal de Distribuição
  SELECT SINGLE * INTO wa_tvtwt
    FROM tvtwt
   WHERE spras EQ sy-langu
     AND vtweg EQ wa_vbak-vtweg.

  "Setor de atividade
  SELECT SINGLE * INTO wa_tspat
    FROM tspat
   WHERE spras EQ sy-langu
     AND spart EQ wa_vbak-spart.

  it_vbap2[] = it_vbap[].

  SORT: it_vbap  BY matnr,
        it_vbap2 BY matnr,
        it_konv  BY kposn.


  DELETE ADJACENT DUPLICATES FROM it_vbap COMPARING matnr.

  LOOP AT it_vbap INTO wa_vbap.

    vg_tabix = sy-tabix.

    CLEAR: wa_vbap-netwr ,
           wa_vbap-kwmeng,
           vl_total      ,
           vl_quantidade ,
           vl_unidade    ,
           vl_preco_unit .

    LOOP AT it_vbap2 INTO wa_vbap2 WHERE matnr = wa_vbap-matnr .

      READ TABLE it_konv INTO wa_konv WITH KEY kposn = wa_vbap2-posnr BINARY SEARCH.

      vl_total       =  vl_total       + wa_vbap2-netwr.   " Valor Total
      vl_quantidade  =  vl_quantidade  + wa_vbap2-kwmeng.  " Quantidade
      vl_preco_unit  =   wa_konv-kbetr.    " Preco unitário

    ENDLOOP.

    wa_vbap-netwr  =  vl_total.      " Valor Total
    wa_vbap-kwmeng =  vl_quantidade. " Quantidade
    wa_vbap-vrkme  =  vl_unidade.    " Unidade
    wa_vbap-kbetr  =  vl_preco_unit. " Preco unitário
    wa_vbap-kmein  =  wa_konv-kmein. " Unidade

    wa_vbap-matnr = |{ wa_vbap-matnr ALPHA = OUT }|.

    "Escritório de Vendas
    SELECT SINGLE * INTO wa_werks
      FROM t001w
     WHERE werks EQ wa_vbap-werks.


    "Centro fornecedor
    SELECT SINGLE * INTO wa_vstel
      FROM t001w
     WHERE werks EQ wa_vbap-vstel.


    MODIFY it_vbap INDEX vg_tabix FROM wa_vbap TRANSPORTING matnr netwr kwmeng netpr kbetr kmein.

  ENDLOOP.


  IF ( NOT wa_vbak-audat IS INITIAL ) AND ( NOT wa_werks IS INITIAL ).

    CALL FUNCTION 'Z_DATA_CIDADE_TEXTO'
      EXPORTING
        pais    = wa_werks-land1
        date    = wa_vbak-audat
        txjcd   = wa_werks-txjcd
      IMPORTING
        tx_date = tx_date.

  ENDIF.

  vl_formname = 'ZSDP0001'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  vg_name     = wa_vbak-vbeln.
  vg_language = sy-langu.
  vg_object   = 'VBBK'.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = '0002' " Para pegar Nota de cabeçalho 1
      language                = vg_language
      name                    = vg_name
      object                  = vg_object
    TABLES
      lines                   = vg_line
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF NOT vg_line[] IS INITIAL.
    wa_linez-tdformat = 'N'.
    wa_linez-tdline   = 'Observações:'.
    APPEND wa_linez TO vg_linez.
  ENDIF.

  CALL FUNCTION 'ZFORMATA_LINES'
    TABLES
      lines   = vg_line
      lines_s = vg_linez.



  IF NOT vg_line[] IS INITIAL.
    wa_linez-tdformat = '*'.
    wa_linez-tdline   = ''.
    APPEND wa_linez TO vg_linez.
  ENDIF.

  CALL  SELECTION-SCREEN 999 ENDING AT 51 8 STARTING AT 3 3.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION vl_name
      EXPORTING
        wa_t001          = wa_t001
        wa_werks         = wa_werks
        wa_vstel         = wa_vstel
        wa_vbak          = wa_vbak
        tt_vbap          = wa_vbap
        wa_vbkd          = wa_vbkd
        wa_info_c        = wa_info_c
        wa_tvtwt         = wa_tvtwt
        wa_tspat         = wa_tspat
        wa_ztext         = wa_ztext
        wa_tcurt         = wa_tcurt
        tx_date          = tx_date
        wa_tvakt         = wa_tvakt
        wa_tvkbt         = wa_tvkbt
        wg_preco_unit    = p_vlr
      TABLES
        it_vbap          = it_vbap
        it_lines         = vg_linez
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
