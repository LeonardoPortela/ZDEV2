FUNCTION z_pfe_msg_contab.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_NM_LOTE) TYPE  ZPFE_NUMERO_LOTE
*"     REFERENCE(P_NM_LOTE_ITEM) TYPE  ZPFE_NUMERO_LOTE
*"  TABLES
*"      IT_MSG_LOTE STRUCTURE  ZPFE_LOTE_MSG OPTIONAL
*"----------------------------------------------------------------------

  DATA: it_retorno   TYPE TABLE OF bapiret2 with header line,
        wa_retorno   TYPE bapiret2,
        wa_lote_item TYPE zpfe_lote_item,
        wa_msg_lote  TYPE zpfe_lote_msg.

  SELECT SINGLE * INTO wa_lote_item
    FROM zpfe_lote_item
   WHERE nm_lote      EQ p_nm_lote
     AND nm_lote_item EQ p_nm_lote_item.

  CHECK ( sy-subrc IS INITIAL ) AND ( wa_lote_item-obj_key IS NOT INITIAL ).

  SELECT * INTO TABLE it_msg_lote
    FROM zpfe_lote_msg
   WHERE obj_key EQ wa_lote_item-obj_key.

  clear: it_retorno[].

  LOOP AT it_msg_lote INTO wa_msg_lote.
    wa_retorno-type       = wa_msg_lote-type.
    wa_retorno-id         = wa_msg_lote-id.
    wa_retorno-number     = wa_msg_lote-numero.
    wa_retorno-message    = wa_msg_lote-message.
    wa_retorno-log_no     = wa_msg_lote-log_no.
    wa_retorno-log_msg_no = wa_msg_lote-log_msg_no.
    wa_retorno-message_v1 = wa_msg_lote-message_v1.
    wa_retorno-message_v2 = wa_msg_lote-message_v2.
    wa_retorno-message_v3 = wa_msg_lote-message_v3.
    wa_retorno-message_v4 = wa_msg_lote-message_v4.
    APPEND wa_retorno TO it_retorno.
  ENDLOOP.

  CALL FUNCTION 'Z_VISUALIZA_MSG_TELA'
    EXPORTING
      p_popup    = c_x
    TABLES
      it_retorno = it_retorno.

ENDFUNCTION.
