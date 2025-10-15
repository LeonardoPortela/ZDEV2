PROCESS BEFORE OUTPUT.
  MODULE status_0300.
*
PROCESS AFTER INPUT.
  MODULE user_command_0300.

  CHAIN.
    FIELD w_instrucao-instrucao.
    MODULE instrucao ON CHAIN-REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD w_instrucao-objek.
*   FIELD w_instrucao-instrucao. "*-CS2021000532-#84613-12.08.2022-JT
    FIELD w_instrucao-data_instr.
    FIELD w_instrucao-data_retirada.
    FIELD w_instrucao-deadline_draft.
    FIELD w_instrucao-deadline_documen.
    FIELD w_instrucao-porto_embarque.
    FIELD w_instrucao-data_in_porto.
    FIELD w_instrucao-data_porto.
    FIELD w_instrucao-terminal.
*    FIELD W_INSTRUCAO-PONTO_C.
    FIELD w_instrucao-booking.
    FIELD w_instrucao-mapa.
    FIELD w_instrucao-fumigacao.
    FIELD w_instrucao-hrs_fgacao.
    FIELD w_instrucao-armador.
    FIELD w_instrucao-free_time.
    FIELD w_instrucao-qtd_ctners.
    FIELD w_instrucao-cod_despach.
    FIELD w_instrucao-cod_transp.
    FIELD w_instrucao-vlr_frete.
    FIELD w_instrucao-observacao.
    FIELD w_instrucao-data_eta.
    FIELD w_instrucao-limite_peso.
    FIELD w_instrucao-peso_max.
    FIELD w_instrucao-pais_des.
    FIELD w_instrucao-data_container.
    FIELD w_instrucao-armazenagem.
    FIELD w_instrucao-lote_armz.
    FIELD w_instrucao-cod_armz.
*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
    FIELD w_instrucao-local_entrega.
    FIELD w_instrucao-oper_log.
*** Fim - Rubenilson Pereira - 12.02.25 - US164130
    "*-CS2021000532-#84613-12.08.2022-JT-inicio
*   MODULE instrucao ON CHAIN-REQUEST.
    MODULE peso_max  ON CHAIN-REQUEST.
    "*-CS2021000532-#84613-12.08.2022-JT-fim
    MODULE crl_reg_modify ON CHAIN-REQUEST.
  ENDCHAIN.

*-CS2021000532-#84613-12.08.2022-JT-inicio
* CHAIN.
*   FIELD w_instrucao-instrucao.
*   MODULE instrucao ON CHAIN-REQUEST.
* ENDCHAIN.

*  CHAIN.
*    FIELD w_instrucao-peso_max.
*    MODULE peso_max ON CHAIN-REQUEST.
*  ENDCHAIN.
*-CS2021000532-#84613-12.08.2022-JT-fim

PROCESS ON VALUE-REQUEST.
  FIELD w_instrucao-objek MODULE search_doc.
