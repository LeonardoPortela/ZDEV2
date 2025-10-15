"Name: \PR:SAPMV45A\FO:USEREXIT_READ_DOCUMENT\SE:BEGIN\EI
ENHANCEMENT 0 ZVA02_PEDIDO_IMPORTACAO.
*
*-CS2022000686-12.01.2023-#100837-JT-inicio
  CLEAR zsdt0225.

  SELECT            navio           local_operacao
    INTO ( zsdt0225-navio, zsdt0225-local_operacao )
    FROM zsdt0225
      UP TO 1 ROWS
   WHERE nr_ov = vbak-vbeln.
  ENDSELECT.
*-CS2022000686-12.01.2023-#100837-JT-inicio
*
ENDENHANCEMENT.
