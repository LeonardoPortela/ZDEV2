
PROCESS BEFORE OUTPUT.
* MODULE STATUS_0110.
  MODULE trata_fields.

  MODULE criar_objetos_0213.
*
PROCESS AFTER INPUT.
* MODULE USER_COMMAND_0110.
  FIELD wg_fiscal-docref  MODULE get_itens ON REQUEST.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  CHAIN.
    FIELD wg_direitos-taxlw1 MODULE valida_taxlw1.
    FIELD wg_direitos-taxlw2 MODULE valida_taxlw2.
    FIELD wg_direitos-taxlw4 MODULE valida_taxlw4.
    FIELD wg_direitos-taxlw5 MODULE valida_taxlw5.
  ENDCHAIN.

  CHAIN.
    FIELD wg_fiscal-ebeln.
    MODULE valida_parametros2 ON CHAIN-REQUEST.
  ENDCHAIN.

*-CS2023000043-09.02.2023-#102019-JT-fim

*  CHAIN.
*    FIELD WG_FISCAL-INCO1 MODULE ENTER ON INPUT.
*  ENDCHAIN.

PROCESS ON VALUE-REQUEST.
  FIELD wg_fiscal-docref MODULE matchcode_docref.
