PROCESS BEFORE OUTPUT.
  MODULE status_9000.
  MODULE init_9000.
  MODULE modify_screen_9000.

  LOOP AT gt_alv_9000 INTO zsde013
                            WITH CONTROL tc_alv
                            CURSOR tc_alv-current_line.
    MODULE tc_lines_9000.

  ENDLOOP.


PROCESS AFTER INPUT.

  LOOP AT gt_alv_9000.

    CHAIN.
      FIELD zsde013-selec
        MODULE checkbox_search ON CHAIN-REQUEST.

    ENDCHAIN.


    CHAIN.
      FIELD zsde013-vlr_brl MODULE m_update ON REQUEST.
    ENDCHAIN.


  ENDLOOP.


  MODULE user_command_9000.
