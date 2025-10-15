*&---------------------------------------------------------------------*
*&  Include           ZXM02TOP
*&---------------------------------------------------------------------*
DATA : cur_activity TYPE aktvt.

DATA : flag_input TYPE flag.

DATA : ssj(1) TYPE c.

DATA : department(20) TYPE c.

DATA : header_changed(1) TYPE c.

" 02.02.2022 - RAMON - REQ DE COMPRA - INTEGRAÇÃO -->
DATA status_coupa TYPE zemm_status_coupa.
DATA id_coupa TYPE zemm_id_coupa.
" 02.02.2022 - RAMON - REQ DE COMPRA - INTEGRAÇÃO --<

DATA lifnr_coupa TYPE lifnr.
DATA konnr_coupa TYPE konnr.

*6) Draw the subscreen 111 in the Project. Place the fields using F6 button. Type ci_ebanbd (because we defined it in ZXM02U01
*
*) and click "Get from Dictionary".
*
*7) Add the PBO module to the screen. It will control the ability to edit fields.

MODULE loop_screen OUTPUT.

  "   CHECK flag_input IS INITIAL.

  LOOP AT SCREEN.

    "      if  sy-tcode = 'ME51N' or sy-tcode = 'ME52N' or ( sy-tcode = 'ME53N' and flag_input = 'X' ).

    IF flag_input = 'X'.

      screen-input = 1.

    ELSE.

      screen-input = 0.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.
