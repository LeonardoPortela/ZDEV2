"Name: \PR:SAPMV45A\FO:USEREXIT_SAVE_DOCUMENT\SE:BEGIN\EI
ENHANCEMENT 0 ZVA02_PEDIDO_IMPORTACAO.
*
*-CS2022000686-12.01.2023-#100837-JT-inicio
  DATA: l_seq.

  IF sy-tcode(4) = 'VA02'.
    SELECT id_seq
      INTO l_seq
      FROM zsdt0225
        UP TO 1 ROWS
     WHERE nr_ov = vbak-vbeln.
    ENDSELECT.

    IF sy-subrc = 0.
      UPDATE zsdt0225 SET navio          = zsdt0225-navio
                          local_operacao = zsdt0225-local_operacao
                    WHERE nr_ov = vbak-vbeln.
    ENDIF.
  ENDIF.
*-CS2022000686-12.01.2023-#100837-JT-inicio
*
ENDENHANCEMENT.
