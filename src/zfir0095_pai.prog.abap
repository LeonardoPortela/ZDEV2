*----------------------------------------------------------------------*
***INCLUDE ZFIR0095_PAI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0102 INPUT.
CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF g_tree IS NOT INITIAL.
        CALL METHOD g_tree->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE: g_tree.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN: 'PSQ_V1'.
      PERFORM pesq_versao USING '1'.
    WHEN: 'PSQ_V2'.
      PERFORM pesq_versao USING '2'.
  ENDCASE.
ENDMODULE.
