PROCESS BEFORE OUTPUT.
  MODULE status_9000.
  MODULE init_9000.
  "MODULE tc_lines_9000.
  MODULE screen_9000.


  LOOP AT gt_alv_9000 INTO zsds079
                            WITH CONTROL tc_alv
                            CURSOR tc_alv-current_line.
    MODULE screen_alv_9000.
  ENDLOOP.



PROCESS AFTER INPUT.

  CHAIN.
    FIELD zsds078-dtprevpag_n.
    FIELD zsds078-vlr_prev.
    MODULE check_field_9000 ON CHAIN-REQUEST.
  ENDCHAIN.

  LOOP AT gt_alv_9000.

    CHAIN.
      FIELD zsds079-selec
        MODULE checkbox_search ON CHAIN-REQUEST.

    ENDCHAIN.

  ENDLOOP.

  MODULE user_command_9000.
