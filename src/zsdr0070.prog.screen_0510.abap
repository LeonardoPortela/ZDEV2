PROCESS BEFORE OUTPUT.
  MODULE pbo_0510.
  MODULE screen_0510."
  MODULE style.
  MODULE trata_fields_01.

  CALL SUBSCREEN sub1 INCLUDING sy-repid '0520'.
PROCESS AFTER INPUT.

  CHAIN.
    FIELD w_contrato-empresa.
    FIELD w_contrato-cliente.
    FIELD w_contrato-cliente_final.
    FIELD w_contrato-tp_venda.
    FIELD w_contrato-vendedor.
    FIELD w_contrato-corretor.
    FIELD w_contrato-tipo_padrao.
    MODULE atualiza_dados_contrato ON CHAIN-REQUEST.
  ENDCHAIN.

  MODULE pai_0510.

CALL SUBSCREEN sub1.

PROCESS ON VALUE-REQUEST.
  FIELD w_contrato-tp_venda MODULE search_venda.
  FIELD w_contrato-vendedor MODULE search_vendedor.
  FIELD w_contrato-visao    MODULE create_dropdown_box_01.
  FIELD w_contrato-id_contrato_referencia MODULE search_id_cont_ref_01.
