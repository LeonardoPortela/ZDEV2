FUNCTION z_sd_info_cte_moto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"     REFERENCE(P_CTE_GERA) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      IT_CTE_MOTO STRUCTURE  ZCTE_MOTORISTA
*"  EXCEPTIONS
*"      MOTO_NAO_PF
*"----------------------------------------------------------------------

  DATA: wa_cte_item       TYPE j_1bnflin,
        wa_fatura_servico TYPE vbrp,
        wa_ordem_venda    TYPE vbak,
        wa_info_part      TYPE lfa1,
        wa_vtpa           TYPE vtpa,
        wa_zcte_motorista TYPE zcte_motorista.

  IF p_cte_gera IS INITIAL.

    SELECT * INTO TABLE it_cte_moto
      FROM zcte_motorista
     WHERE docnum EQ p_cte_avulso.

  ELSE.

    "" Documento Normal
    SELECT SINGLE * INTO wa_cte_item
      FROM j_1bnflin
     WHERE docnum EQ p_cte_avulso.

    CHECK sy-subrc IS INITIAL.

    "Fatura do Serviço
    SELECT SINGLE * INTO wa_fatura_servico
      FROM vbrp
     WHERE vbeln = wa_cte_item-refkey(10)
       AND posnr = wa_cte_item-refitm.

    CHECK sy-subrc IS INITIAL.

    "Ordem de Venda
    SELECT SINGLE * INTO wa_ordem_venda
      FROM vbak
     WHERE vbeln = wa_fatura_servico-aubel.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE * INTO wa_vtpa
      FROM vtpa
     WHERE vbeln = wa_ordem_venda-tknum
       AND parvw = c_mt.

    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = wa_vtpa-lifnr
        p_partype    = 'V'
      CHANGING
        wa_info_part = wa_info_part.

    IF wa_info_part IS NOT INITIAL.

      IF wa_info_part-stkzn IS INITIAL.
        MESSAGE e014(zciot) WITH wa_vtpa-lifnr RAISING moto_nao_pf.
      ENDIF.

      wa_zcte_motorista-docnum = p_cte_avulso.
      PERFORM lct USING wa_info_part-name1.
      wa_zcte_motorista-xnome  = vg_limpo.
      wa_zcte_motorista-cpf    = wa_info_part-stcd2.
      wa_zcte_motorista-lifnr  = wa_info_part-lifnr.
      APPEND wa_zcte_motorista TO it_cte_moto.
    ENDIF.

  ENDIF.

ENDFUNCTION.
