PROCESS BEFORE OUTPUT.
  MODULE status_9000.

  LOOP AT gt_saldos INTO zsds_tela_calculo_alv
    WITH CONTROL grd_9000
                            CURSOR grd_9000-current_line.
    MODULE screen_alv_9000.
  ENDLOOP.



PROCESS AFTER INPUT.

  LOOP AT gt_saldos.

    CHAIN.
      FIELD zsds_tela_calculo_alv-saldo_usado
        MODULE update_fields ON CHAIN-REQUEST.

    ENDCHAIN.


  ENDLOOP.

  MODULE user_command_9000.
