*----------------------------------------------------------------------*
***INCLUDE ZFIR0125_PAI_0200.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.

    WHEN 'BT_SAIR' OR 'EXIT'.

      CALL METHOD go_textedit->delete_text.

      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'BT_CONF'.

      PERFORM processa_justificativa.

      CALL METHOD go_textedit->delete_text. "SMC 189279 - 10-09-2025

      SET SCREEN 0.
      LEAVE SCREEN.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form processa_justificativa
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processa_justificativa .


  CALL METHOD go_textedit->get_text_as_stream
    EXPORTING
      only_when_modified     = cl_gui_textedit=>true
    IMPORTING
      text                   = gt_text
    EXCEPTIONS
      error_dp               = 1
      error_cntl_call_method = 2
      OTHERS                 = 3.


  IF gt_text IS INITIAL.
    CASE sy-ucomm.
        WHEN 'LIBERAR'.

           MESSAGE 'É obrigatório o preenchimento do Motivo da Liberação' TYPE 'S' DISPLAY LIKE 'E'.
           RETURN.

        WHEN 'CANCSOL'.
          MESSAGE 'É obrigatório o preenchimento do Motivo do Cancelamento' TYPE 'S' DISPLAY LIKE 'E'.
           RETURN.
        WHEN OTHERS.

    ENDCASE.



  ENDIF.


ENDFORM.
