FUNCTION z_sd_info_cte_trans.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"     REFERENCE(P_CTE_GERA) TYPE  CHAR01 OPTIONAL
*"  EXPORTING
*"     REFERENCE(P_DOC_TRANSP) TYPE  VTTK
*"  TABLES
*"      IT_CTE_TRANS STRUCTURE  ZCTE_TRANS OPTIONAL
*"  EXCEPTIONS
*"      N_PLACA_CAD
*"----------------------------------------------------------------------

  DATA: wa_cte_item       TYPE j_1bnflin,
        wa_fatura_servico TYPE vbrp,
        wa_ordem_venda    TYPE vbak,
        vl_placa(7)       TYPE c,
        wa_zlest0002      TYPE zlest0002,
        wa_zcte_trans     TYPE zcte_trans,
        wa_info_part      TYPE lfa1,
        wa_adr6           TYPE adr6,
        wa_zlest0026      TYPE zlest0026.

  IF p_cte_gera IS INITIAL.

    SELECT * INTO TABLE it_cte_trans
      FROM zcte_trans
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

    SELECT SINGLE * INTO p_doc_transp
      FROM vttk
     WHERE tknum = wa_ordem_venda-tknum.

    SELECT SINGLE * INTO wa_zlest0026
      FROM zlest0026
     WHERE tknum EQ p_doc_transp-tknum.

    DO 4 TIMES.

      CASE sy-index.
        WHEN 1.
          vl_placa = p_doc_transp-text1(7).
        WHEN 2.
          vl_placa = p_doc_transp-text2(7).
        WHEN 3.
          vl_placa = p_doc_transp-text3(7).
        WHEN 4.
          vl_placa = p_doc_transp-text4(7).
      ENDCASE.

      CHECK vl_placa IS NOT INITIAL.

      SELECT SINGLE * INTO wa_zlest0002 FROM zlest0002 WHERE pc_veiculo = vl_placa.

      IF sy-subrc IS INITIAL.
        CLEAR: wa_zcte_trans.
        wa_zcte_trans-docnum         = p_cte_avulso.
        wa_zcte_trans-pc_veiculo     = wa_zlest0002-pc_veiculo.
        wa_zcte_trans-qtd_eixo       = wa_zlest0002-qt_eixo.
        wa_zcte_trans-proprietario   = wa_zlest0002-proprietario.
        wa_zcte_trans-country        = wa_zlest0002-country.
        wa_zcte_trans-taxjurcode     = wa_zlest0002-taxjurcode.
        wa_zcte_trans-spras          = wa_zlest0002-spras.
        wa_zcte_trans-cd_cidade      = wa_zlest0002-cd_cidade.
        wa_zcte_trans-cd_uf          = wa_zlest0002-cd_uf.
        wa_zcte_trans-agregado       = wa_zlest0002-agregado.
        wa_zcte_trans-cd_renavam     = wa_zlest0002-cd_renavam.
        wa_zcte_trans-tp_veiculo     = wa_zlest0002-tp_veiculo.
        wa_zcte_trans-tp_rodado      = wa_zlest0002-tp_rodado.
        wa_zcte_trans-tp_carroceria2 = wa_zlest0002-tp_carroceria2.
        wa_zcte_trans-tara           = wa_zlest0002-tara.
        wa_zcte_trans-cap_kg         = wa_zlest0002-cap_kg.
        wa_zcte_trans-cap_m3         = wa_zlest0002-cap_m3.
        wa_zcte_trans-nr_tag_strada  = wa_zlest0002-nr_tag_strada.  "*-CS2024001181-16.12.2024-#160717-JT

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            p_parceiro   = wa_zlest0002-proprietario
            p_partype    = 'V'
            p_endereco   = 'X'
          CHANGING
            wa_info_part = wa_info_part
            wa_adr6      = wa_adr6.

        wa_zcte_trans-prop_cnpj  = wa_info_part-stcd1.
        wa_zcte_trans-prop_cpf   = wa_info_part-stcd2.
        wa_zcte_trans-pfisica    = wa_info_part-stkzn.
        wa_zcte_trans-prop_nome  = wa_info_part-name1.
        wa_zcte_trans-prop_rntrc = wa_info_part-bahns.
        wa_zcte_trans-prop_uf    = wa_info_part-regio.
        wa_zcte_trans-prop_ie    = wa_info_part-stcd3.
        wa_zcte_trans-telefone   = wa_info_part-telf1.
        wa_zcte_trans-e_mail     = wa_adr6-smtp_addr.

        IF wa_zlest0026-nr_card_ped IS NOT INITIAL.
          wa_zcte_trans-tp_card_ped = wa_zlest0026-tp_card_ped.
          wa_zcte_trans-nr_card_ped = wa_zlest0026-nr_card_ped.
        ENDIF.

        APPEND wa_zcte_trans TO it_cte_trans.
      ELSE.
        MESSAGE e026 WITH vl_placa RAISING n_placa_cad.
      ENDIF.
    ENDDO.

  ENDIF.

ENDFUNCTION.
