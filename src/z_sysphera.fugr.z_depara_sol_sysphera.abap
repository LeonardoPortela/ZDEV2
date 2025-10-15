FUNCTION z_depara_sol_sysphera.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_ANO) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      RESULTADO STRUCTURE  ZSYS_DEPARA_SOL OPTIONAL
*"      IT_EMPRESA STRUCTURE  FIN_CFIN_S_APAR_BURKS
*"----------------------------------------------------------------------
  DATA: lr_bukrs TYPE RANGE OF bukrs,
        lr_gjahr TYPE RANGE OF gjahr.

  IF i_ano IS INITIAL.
    i_ano = sy-datum(4).
  ENDIF.

  lr_gjahr = VALUE #( sign = 'I' option = 'BT' ( low = i_ano - 1 high = i_ano ) ).

* USER STORY 160299 - MMSILVA - 04.12.2024 - INICIO
IF IT_EMPRESA[] IS NOT INITIAL.
  LOOP AT IT_EMPRESA INTO DATA(wa_bukrs).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_bukrs-bukrs ) to lr_bukrs.
  ENDLOOP.
  ELSE.
lr_bukrs = VALUE #( sign = 'I' option = 'BT' ( low = '0001' high = '0055' ) ).
    ENDIF.
* USER STORY 160299 - MMSILVA - 04.12.2024 - FIM

  SELECT posnr solicitacao_invest INTO TABLE resultado
    FROM zim01_sol_ap_inv
    WHERE bukrs IN lr_bukrs
      AND ano   IN lr_gjahr.

ENDFUNCTION.
