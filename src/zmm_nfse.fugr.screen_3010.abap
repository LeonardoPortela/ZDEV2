PROCESS BEFORE OUTPUT.
  MODULE status_3010.

  LOOP AT gt_service INTO zibs_nfse_po_serv
                      WITH CONTROL tc_service
                            CURSOR tc_service-current_line.

  ENDLOOP.

PROCESS AFTER INPUT.

  LOOP AT gt_service.

    CHAIN.
      FIELD: zibs_nfse_po_serv-menge
             MODULE screen_change_3010
      ON CHAIN-REQUEST.
    ENDCHAIN.

  ENDLOOP.

  MODULE user_command_3010.
