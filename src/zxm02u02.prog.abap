*&---------------------------------------------------------------------*
*&  Include           ZXM02U02
*&---------------------------------------------------------------------*
CONSTANTS: c_banfn(28) VALUE '(SAPLMEGUI)MEPO_SELECT-BANFN'.
CONSTANTS: c_banfn2(34) VALUE '(SAPLMEGUI)MEREQ_TOPLINE-BANFN_EXT'.
* Busca Requisição da tela
FIELD-SYMBOLS: <fs_banfn> TYPE any.

DATA : l_mereq_item   TYPE mereq_item,
       v_banfn        TYPE eban-banfn,
       l_account_list TYPE mmpur_accounting_list,
       l_account_line TYPE mmpur_accounting_type,
       ls_exkn        TYPE exkn,
       i_erro(1).

CLEAR v_banfn.
"
IF NOT im_req_item IS INITIAL.
  l_mereq_item = im_req_item->get_data( ).
ENDIF.
IF im_ucomm = 'BTN_TRE'.
  ASSIGN (c_banfn2) TO <fs_banfn>.
  IF <fs_banfn> IS ASSIGNED .
    v_banfn = <fs_banfn>.
  ENDIF.
  IF v_banfn IS INITIAL.
    ASSIGN (c_banfn) TO <fs_banfn>.
    IF <fs_banfn> IS ASSIGNED .
      v_banfn = <fs_banfn>.
    ENDIF.
  ENDIF.

  CALL METHOD im_req_item->if_acct_container_mm~get_items
    RECEIVING
      re_items = l_account_list.
  CHECK l_account_list IS NOT INITIAL.

  LOOP AT l_account_list INTO l_account_line.
    CALL METHOD l_account_line-model->get_exkn
      RECEIVING
        re_exkn = ls_exkn.

*    CALL FUNCTION 'Z_MM_TREINA_COMPRAS'
*      EXPORTING
*        I_EBELN = V_BANFN
*        I_EBELP = L_MEREQ_ITEM-BNFPO
*        I_KOSTL = LS_EXKN-KOSTL
*        I_SAKNR = LS_EXKN-SAKTO
*        I_MATNR = L_MEREQ_ITEM-MATNR
*        I_BTN   = 'X'
*        I_TIPO  = 'R'
*      IMPORTING
*        I_ERRO  = I_ERRO.

  ENDLOOP.

ELSEIF im_ucomm = 'MELEAV' OR im_ucomm = 'MEBACK' OR im_ucomm = 'MECANC'.
  DELETE FROM zmmt0105
          WHERE ebeln   = ' '
          AND   ebelp   = l_mereq_item-bnfpo
          AND   tipo    = 'R'.
ENDIF.
