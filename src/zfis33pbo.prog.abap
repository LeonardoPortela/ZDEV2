*&---------------------------------------------------------------------*
*&  Include           ZFIS33PBO
*&---------------------------------------------------------------------*

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST_001'.
  SET TITLEBAR 'T_001'.
ENDMODULE.


MODULE USER_COMMAND_0100 OUTPUT.

   IF G_CUSTOM_CONTAINER IS INITIAL.
      PERFORM CREATE_AND_INIT_ALV CHANGING GT_FIELDCAT.
    ENDIF.

ENDMODULE.
