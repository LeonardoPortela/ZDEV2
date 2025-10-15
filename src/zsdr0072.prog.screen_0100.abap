PROCESS BEFORE OUTPUT.
  MODULE pbo_0100.
  MODULE create_fcat.
  MODULE popula_obj.

PROCESS AFTER INPUT.

  CHAIN.
    FIELD it_new-bukrs.
    FIELD it_new-werks.
    FIELD it_new-matnr.
    FIELD it_new-terminal.
    FIELD it_new-ponto_c.
    FIELD it_new-terminal_estuf.
    FIELD it_new-controladora.
    FIELD it_new-cod_despach.
    FIELD it_new-cod_transp.
    FIELD it_new-cod_armz.
*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
    FIELD it_new-local_entrega.
    FIELD it_new-oper_log.
*** Fim - Rubenilson Pereira - 12.02.25 - US164130
    MODULE crl_reg_modify ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD it_new-instrucao.
    FIELD it_new-contrato.
    FIELD it_new-data_instr.
    FIELD it_new-data_in_porto.
    FIELD it_new-deadline_draft.
    FIELD it_new-porto_embarque..
    FIELD it_new-safra.
    FIELD it_new-data_retirada.
    FIELD it_new-data_porto.
    FIELD it_new-deadline_documen.
*    FIELD IT_NEW-QUANTIDADE.
    FIELD it_new-voleh.
    FIELD it_new-dmbtr.
    FIELD it_new-pmein.
*    FIELD IT_NEW-BTGEW.
    FIELD it_new-gewei.
    FIELD it_new-charg.
    FIELD it_new-booking.
    FIELD it_new-armador.
    FIELD it_new-qtd_ctners.
    FIELD it_new-mapa.
    FIELD it_new-fumigacao.
    FIELD it_new-hrs_fgacao.
    FIELD it_new-vlr_frete.
    FIELD it_new-free_time.
    FIELD it_new-cod_despach.
    FIELD it_new-cod_transp.
*    FIELD it_new-observacao.
    FIELD it_new-data_eta.
    FIELD it_new-pais_des.
    FIELD it_new-terminal_estuf.
    FIELD it_new-controladora.
    FIELD it_new-navio.
    FIELD it_new-tamanho_fardo.
    FIELD it_new-data_container.
    FIELD it_new-limite_peso.
    FIELD it_new-peso_max.
    FIELD it_new-pais_des.
    FIELD it_new-data_container.
    FIELD it_new-armazenagem.
    MODULE check_obrigatorio ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD it_new-peso_max.
    MODULE peso_max ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD it_new-instrucao.
    FIELD it_new-safra_sel.  "*-CS2023000189-06.04.2023-#108697-JT
    MODULE instrucao ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD it_new-quantidade.
    MODULE check_qtd ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD it_new-btgew.
    MODULE check_peso ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE pai_0100.
