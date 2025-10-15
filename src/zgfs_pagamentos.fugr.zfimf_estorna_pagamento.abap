FUNCTION zfimf_estorna_pagamento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ZBUKR) TYPE  DZBUKR OPTIONAL
*"     VALUE(I_LAUFD) TYPE  LAUFD OPTIONAL
*"     VALUE(I_LAUFI) TYPE  LAUFI OPTIONAL
*"     VALUE(I_VBLNR) TYPE  VBLNR OPTIONAL
*"     VALUE(I_ZALDT) TYPE  DZALDT_ZHL OPTIONAL
*"----------------------------------------------------------------------

  CLEAR: l_subrc,
         l_erro,
         l_fb08_belnr,
         l_message,
         l_fb08_message,
         l_fbra_message,
         l_mode,
         w_bkpf,
         w_zfit0167.

  l_mode  = 'N'.
  l_gjahr = i_zaldt(4).

*-----------------------------
*-doc.contabil
*-----------------------------
  SELECT SINGLE *
           FROM bkpf
           INTO w_bkpf
          WHERE bukrs = i_zbukr
            AND belnr = i_vblnr
            AND gjahr = l_gjahr.

  CHECK sy-subrc = 0.
  CHECK w_bkpf-stblg IS INITIAL.

*-----------------------------
*---FBRA
*-----------------------------
  CALL FUNCTION 'CALL_FBRA'
    EXPORTING
      i_bukrs      = i_zbukr
      i_augbl      = i_vblnr
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

  l_fbra_message = l_message.

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
        i_bukrs      = i_zbukr
        i_belnr      = i_vblnr
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

    IF l_subrc = 0.
      l_fb08_belnr = l_msgv1.
    ENDIF.
    l_fb08_message = l_message.
  ENDIF.

  IF l_subrc = 0.
    l_erro = 'S'.
  ELSE.
    l_erro = 'E'.
  ENDIF.

  CONCATENATE l_fbra_message '|' l_fb08_message
         INTO l_message
         SEPARATED BY space.

*------------------------------
* atualiza tabela de logs
*------------------------------
  SELECT SINGLE *
           FROM zfit0167
           INTO w_zfit0167
          WHERE laufd = i_laufd
            AND laufi = i_laufi
            AND zbukr = i_zbukr
            AND vblnr = i_vblnr.

  CHECK w_zfit0167-belnr_fb08 IS INITIAL.

  w_zfit0167-mandt       = sy-mandt.
  w_zfit0167-laufd       = i_laufd.
  w_zfit0167-laufi       = i_laufi.
  w_zfit0167-zbukr       = i_zbukr.
  w_zfit0167-vblnr       = i_vblnr.
  w_zfit0167-belnr_fb08  = l_fb08_belnr.
  w_zfit0167-message     = l_message.
  w_zfit0167-erro        = l_erro.

  MODIFY zfit0167     FROM w_zfit0167.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFUNCTION.
