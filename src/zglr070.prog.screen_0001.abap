PROCESS BEFORE OUTPUT.
  MODULE tab_set.
  MODULE screen.
  CALL SUBSCREEN tab_strip_sca
       INCLUDING g_tab_strip-prog g_tab_strip-subscreen.

  MODULE main_pbo.

PROCESS AFTER INPUT.

  MODULE main_pai_exit AT EXIT-COMMAND.

  CHAIN.
*    FIELD WA_092-BUKRS.
*    FIELD WA_092-FILIAL.
    FIELD wa_092-tx_usd_futuro.
    FIELD wa_092-tx_contrato.
    MODULE set_modelagem ON CHAIN-INPUT.
  ENDCHAIN.

  CALL SUBSCREEN tab_strip_sca.

  FIELD wa_092-bukrs  MODULE val_empresa ON REQUEST.
  FIELD wa_092-bukrsp  MODULE val_empresa ON REQUEST.
  FIELD wa_092-filial MODULE val_filial ON REQUEST.
  FIELD wa_092-kostl MODULE val_kostl ON REQUEST.
  FIELD wa_092-fornecedor MODULE val_forne ON REQUEST.
  FIELD wa_092-cliente MODULE val_cliente ON REQUEST.
  FIELD wa_092-moeda MODULE val_moeda ON REQUEST.

  MODULE tab_get.

  MODULE main_pai.

PROCESS ON VALUE-REQUEST.
*  FIELD wa_092-filial MODULE search_filial.
  FIELD wa_092-kostl MODULE search_kostl.
*  FIELD it_096-cli_for MODULE search_cli_for.
  FIELD wa_092-municipio MODULE helpm.
  FIELD wa_092-cod_contrato MODULE helpc.
