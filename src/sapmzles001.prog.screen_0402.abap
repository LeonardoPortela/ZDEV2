
PROCESS BEFORE OUTPUT.

  MODULE visibilidade_edicao_402.

  MODULE status_402.

PROCESS AFTER INPUT.

  FIELD snlancto_confer_400-chvid
        MODULE check_chvid_400 ON REQUEST.

  FIELD snlancto_confer_400-valor
        MODULE check_valor_400 ON REQUEST.

  MODULE check_okcode_402 AT EXIT-COMMAND.
  MODULE check_okcode_402.

PROCESS ON VALUE-REQUEST.

  FIELD snlancto_confer_400-chvid
    MODULE match_code_chvid_400.
