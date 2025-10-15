*&---------------------------------------------------------------------*
*& Report  ZIM20
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zim20.
TABLES: zim01_sol_ap_inv.

DATA: tg_zim01 TYPE TABLE OF zim01_sol_ap_inv WITH HEADER LINE.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_final FOR zim01_sol_ap_inv-finalidade NO INTERVALS NO-EXTENSION,
                s_gpo   FOR zim01_sol_ap_inv-cod_gpo  NO INTERVALS NO-EXTENSION,
                s_item  FOR zim01_sol_ap_inv-cod_item  NO INTERVALS NO-EXTENSION.

SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM seleciona_dados.
  PERFORM organiza_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .
  SELECT *
    FROM zim01_sol_ap_inv
    INTO TABLE tg_zim01
     WHERE finalidade EQ space
       OR  cod_gpo    EQ space
       OR  cod_item   EQ space.

if tg_zim01[] is initial.
  message i836(sd) display like 'E' with 'Não foram encontrados registros para atualização'.
  stop.
endif.
ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_dados .
  DATA: x_cont TYPE sy-tabix,
        x_cont_aux(6),
        msg(255),
        resposta.

  LOOP AT tg_zim01.
    IF tg_zim01-finalidade IS INITIAL.
      MOVE: s_final-low TO tg_zim01-finalidade.
    ENDIF.

    IF tg_zim01-cod_gpo IS INITIAL.
      MOVE: s_gpo-low TO tg_zim01-cod_gpo.
    ENDIF.

    IF tg_zim01-cod_item IS INITIAL.
      MOVE: s_item-low TO tg_zim01-cod_item.
    ENDIF.
    MODIFY tg_zim01.
  ENDLOOP.

  DESCRIBE TABLE tg_zim01 LINES x_cont.
  x_cont_aux = x_cont.
  shift x_cont_aux left deleting leading '0'.
  condense x_cont_aux no-gaps.
  CONCATENATE 'Serão atualizados' x_cont_aux 'registros' INTO msg SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = text-002
      text_question         = msg
      text_button_1         = 'Sim'
      icon_button_1         = '@0V@'
      text_button_2         = 'Não'
      icon_button_2         = '@0W@'
      default_button        = '1'
      display_cancel_button = ' '
    IMPORTING
      answer                = resposta.
  IF resposta EQ '1'.
    MODIFY zim01_sol_ap_inv FROM TABLE tg_zim01.
    COMMIT WORK AND WAIT.
  ELSE.
    STOP.
  ENDIF.
endform.                    " ORGANIZA_DADOS
