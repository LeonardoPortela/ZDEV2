DATA: taxa_conversao TYPE p DECIMALS 2,
      taxa_fixa      TYPE p DECIMALS 4 VALUE '2.2046'.

* Inclusão - RIM-SKM - IR146244 - Inicio
DATA: v_bukrs TYPE bukrs.
SELECT SINGLE valfrom
  FROM setleaf INTO @v_bukrs
 WHERE setname = 'ZSDF0015_NAO_PRODUTORA'
 AND   valfrom = @wa_itens-empresa.
IF sy-subrc EQ 0.
  SELECT SINGLE *
    FROM zppt0002 AS a
    INNER JOIN zppt0004 AS b ON b~verid EQ a~verid
                            AND b~werks EQ a~werks
    INTO @DATA(wa_00021)
      WHERE a~werks    EQ @wa_itens-algodoeira(4)
        AND cd_safra EQ @wa_itens-safra
        AND lgort    EQ @wa_itens-lote.
  taxa_conversao = wa_00021-b-tara.
ELSE.
* Inclusão - RIM-SKM - IR146244 - Fim
  SELECT SINGLE *
    FROM zppt0002 AS a
    INNER JOIN zppt0004 AS b ON b~verid EQ a~verid
                            AND b~werks EQ a~werks
    INTO @DATA(wa_0002)
      WHERE a~werks    EQ @wa_itens-werks
        AND cd_safra EQ @wa_itens-safra
        AND lgort    EQ @wa_itens-lote.

*** Stefanini - IR251652 - 05/08/2025 - LAZAROSR - Início de Alteração
    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE *
        FROM zppt0002 AS a
        INNER JOIN zppt0004 AS b
                            ON  b~verid EQ a~verid
                            AND b~werks EQ a~werks
       INTO @wa_0002
      WHERE a~werks       EQ @wa_itens-werks
        AND cd_safra      EQ @wa_itens-safra
        AND bloco_destino EQ @wa_itens-lote.

    ENDIF.
*** Stefanini - IR251652 - 05/08/2025 - LAZAROSR - Fim de Alteração

  taxa_conversao = wa_0002-b-tara.

ENDIF.



wa_grid = VALUE #(
                   lot       = wa_itens-lote
                   shipping  = wa_itens-tipo
                   bales     = wa_itens-qtd_fardos
                   gross     = ( wa_itens-qtd_fardos * taxa_conversao ) + wa_itens-peso_lote
                   tare      = wa_itens-qtd_fardos * taxa_conversao
                   net       = wa_itens-peso_lote
                   price_ctr = wa_itens-preco_final
                   price     = wa_itens-preco_final * taxa_fixa
                   total     = wa_itens-preco_final * wa_itens-peso_lote * taxa_fixa
                 ).
"Mota totais do Radapé
ADD wa_grid-bales TO wa_total-bales.
ADD wa_grid-gross TO wa_total-gross.
ADD wa_grid-net   TO wa_total-net.
ADD wa_grid-total TO wa_total-total.

WRITE wa_grid-tare TO wa_grid-tare NO-ZERO.
WRITE wa_total-bales TO wa_total-bales NO-ZERO.
CONDENSE wa_grid-tare NO-GAPS.
CONDENSE wa_total-bales NO-GAPS.

wa_total-prepayment = wa_total-total * ( wa_143-pctgem_ant / 100 ).
