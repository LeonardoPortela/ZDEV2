*----------------------------------------------------------------------*
***INCLUDE ZXM02F02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_POSNR  text
*      -->P_IT_EBKN2_BNFPO  text
*----------------------------------------------------------------------*
FORM verifica_status  TABLES ex_messages TYPE  mereq_t_bapiret2
                      USING    p_posnr
                               p_item.

  DATA: wl_status TYPE zim01_sol_ap_inv-status_aprov.
*  DATA: messages LIKE BAPIRET2 WITH HEADER LINE.

  DATA: BEGIN OF messages OCCURS 0.
          INCLUDE STRUCTURE bapiret2.
  DATA: END OF messages.

  SELECT SINGLE status_aprov FROM zim01_sol_ap_inv INTO wl_status
       WHERE posnr = p_posnr.

  IF sy-subrc = 0.
    IF wl_status NE '1'.

      CLEAR ex_messages.
      messages-type = 'E'.
      messages-id = 'Z01'.
      messages-number = '000'.
      messages-message_v1 = 'Para os objetos informados Imobilizado'.
      messages-message_v2 = '/Ordem Interna não está aprovado a '.
      messages-message_v3 = 'Solicitação de Investimento'.
      CONCATENATE  p_posnr '- Item:' p_item INTO messages-message_v4 SEPARATED BY space.
*       = p_item.
      APPEND messages TO ex_messages.
    ENDIF.
  ENDIF.

ENDFORM.                    " VERIFICA_STATUS
