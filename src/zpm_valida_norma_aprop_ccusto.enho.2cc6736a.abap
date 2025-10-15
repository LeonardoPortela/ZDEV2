"Name: \FU:IWO1_CHECK_USER_INGRP\SE:BEGIN\EI
ENHANCEMENT 0 ZPM_VALIDA_NORMA_APROP_CCUSTO.
"FF - 16.01.2024 - inicio

IF sy-ucomm = 'BUCH'. "Salvar

  DATA(lv_data) = '(SAPLKOBS)GT_COBRB_BUF[]'.
  DATA: msg01  TYPE string.
  FIELD-SYMBOLS: <fs_gt_cobrb_buf> TYPE ANY TABLE,
                 <fs> type any.


  TYPES: BEGIN OF ty_cobrb_buf.
           INCLUDE STRUCTURE cobrb.
  TYPES:     uflag LIKE dkobr-upd_flag,
         END OF ty_cobrb_buf.

  TYPES: ty_t_cobrb_buf TYPE ty_cobrb_buf OCCURS 10.


  data wa_cobrb TYPE ty_cobrb_buf.

  ASSIGN (lv_data) TO <fs_gt_cobrb_buf>.

  IF <fs_gt_cobrb_buf> IS ASSIGNED.

    LOOP AT <fs_gt_cobrb_buf> INTO wa_cobrb.

      IF i_viqmel-kostl <> wa_cobrb-kostl.
        msg01 = 'Não é possível salvar. Norma de apropriação na ordem divergentes!'.
        MESSAGE  msg01 TYPE 'E' RAISING no_authority.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDIF.
"FF - 16.01.2024 - fim
ENDENHANCEMENT.
