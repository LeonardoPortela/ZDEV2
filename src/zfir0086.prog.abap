*******************************************************************************************
*            	                     +------------------+                                  	*
*                                  | Grupo | WAYON    |                                   *
*                                  +------------------+                                   *
*-----------------------------------------------------------------------------------------*
*** Dados do Programa                                                                     *
*-----------------------------------------------------------------------------------------*
* Nome            : ZFIR0086
* Título          : Efetua estorno do documento de pagamento
* Autor           : Jaime Tassoni                                                         *
* Data            : 01.09.2020                                                            *
*-----------------------------------------------------------------------------------------*
*** Histórico das modificações                                                            *
*-----------------------------------------------------------------------------------------*
* Seq | Data         | Autor              | COD/ Descrição Da Modificação                 *
*-----------------------------------------------------------------------------------------*
*                                                                                         *
*******************************************************************************************
REPORT zfir0086 MESSAGE-ID zfi.

************************************************************************
* tabelas
************************************************************************
TABLES: reguh.

************************************************************************
* Selection-Screen
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-005.
PARAMETER       : p_zbukr  LIKE reguh-zbukr,
                  p_laufd  LIKE reguh-laufd,
                  p_laufi  LIKE reguh-laufi,
                  p_vblnr  LIKE reguh-vblnr,
                  p_zaldt  LIKE reguh-zaldt.
SELECTION-SCREEN: END   OF BLOCK b1.

************************************************************************
* Variaveis locais
************************************************************************
DATA: l_message      TYPE bapiret2-message,
      l_subrc        TYPE sy-subrc,
      l_mode         TYPE c,
      l_gjahr        TYPE bkpf-gjahr,
      l_erro         TYPE c,
      l_msgv1        TYPE sy-msgv1,
      l_motivo       TYPE bkpf-stgrd,
      l_fb08_belnr   TYPE bkpf-belnr,
      l_fb08_message TYPE bapiret2-message,
      w_bkpf         TYPE bkpf,
      w_zfit0167     TYPE zfit0167.

************************************************************************
* START
************************************************************************
START-OF-SELECTION.

  PERFORM f_cancela_pagamento.

************************************************************************
* CAncela pagamento
************************************************************************
FORM f_cancela_pagamento.

  CLEAR: l_subrc,
         l_erro,
         l_fb08_belnr,
         l_fb08_message,
         l_mode,
         w_bkpf,
         w_zfit0167.

  l_mode  = 'N'.
  l_gjahr = p_zaldt(4).

*-----------------------------
*-doc.contabil
*-----------------------------
  SELECT SINGLE *
           FROM bkpf
           INTO w_bkpf
          WHERE bukrs = p_zbukr
            AND belnr = p_vblnr
            AND gjahr = l_gjahr.

  CHECK sy-subrc = 0.
  CHECK w_bkpf-stblg IS INITIAL.

*-----------------------------
*---FBRA
*-----------------------------
  CALL FUNCTION 'CALL_FBRA'
    EXPORTING
      i_bukrs      = p_zbukr
      i_augbl      = p_vblnr
      i_gjahr      = l_gjahr
      i_mode       = l_mode
*     i_stomo      = '01'
*     i_no_auth    = 'X'
    EXCEPTIONS
      not_possible = 1
      OTHERS       = 2.

  l_subrc     = sy-subrc.

  CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
    EXPORTING
      id         = sy-msgid
      number     = sy-msgno
      language   = sy-langu
      textformat = 'ASC'
      message_v1 = sy-msgv1
      message_v2 = sy-msgv2
      message_v3 = sy-msgv3
      message_v4 = sy-msgv4
    IMPORTING
      message    = l_message.

  IF l_subrc = 0 OR
     l_subrc = 1.

*-----------------------------
*------ FB08
*-----------------------------
    IF w_bkpf-gjahr = sy-datum(4) AND
       w_bkpf-monat = sy-datum+4(2).
      l_motivo = '01'.
    ELSE.
      l_motivo = '02'.
    ENDIF.

    CALL FUNCTION 'CALL_FB08'
      EXPORTING
        i_bukrs      = p_zbukr
        i_belnr      = p_vblnr
        i_gjahr      = l_gjahr
        i_stgrd      = l_motivo
      EXCEPTIONS
        not_possible = 1
        OTHERS       = 2.

    l_subrc      = sy-subrc.
    l_msgv1      = sy-msgv1.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = sy-msgid
        number     = sy-msgno
        language   = sy-langu
        textformat = 'ASC'
        message_v1 = sy-msgv1
        message_v2 = sy-msgv2
        message_v3 = sy-msgv3
        message_v4 = sy-msgv4
      IMPORTING
        message    = l_message.

    l_fb08_belnr   = l_msgv1.
    l_fb08_message = l_message.
  ENDIF.

  IF l_fb08_belnr IS INITIAL.
    l_erro = 'E'.
  ELSE.
    l_erro = 'S'.
  ENDIF.
*------------------------------
* atualiza tabela de logs
*------------------------------
  SELECT SINGLE *
           FROM zfit0167
           INTO w_zfit0167
          WHERE laufd = p_laufd
            AND laufi = p_laufi
            AND zbukr = p_zbukr
            AND vblnr = p_vblnr.

  CHECK w_zfit0167-belnr_fb08 IS INITIAL.

  w_zfit0167-mandt       = sy-mandt.
  w_zfit0167-laufd       = p_laufd.
  w_zfit0167-laufi       = p_laufi.
  w_zfit0167-zbukr       = p_zbukr.
  w_zfit0167-vblnr       = p_vblnr.
  w_zfit0167-belnr_fb08  = l_fb08_belnr.
  w_zfit0167-message     = l_message.
  w_zfit0167-erro        = l_erro.

  MODIFY zfit0167     FROM w_zfit0167.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.

*******************************************************************************************
*******************************************************************************************
