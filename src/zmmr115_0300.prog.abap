*----------------------------------------------------------------------*
***INCLUDE ZMMR115_0300.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  FREE: ok_code_0300.

  SET PF-STATUS 'ZMMR115_0300'.
  SET TITLEBAR 'ZMMR115_0300'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  DATA: lv_erro TYPE char01.

  lv_conf_ped_massa = abap_false.

  CASE ok_code_0300.
    WHEN 'CONFIRMAR'.
      PERFORM f_valida_tela_0300 CHANGING lv_erro.
      IF lv_erro = abap_false.
        lv_conf_ped_massa = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CLEAR ok_code_0300.

ENDMODULE.

************************************************************
* validar informacoes
************************************************************
FORM f_valida_tela_0300 CHANGING p_erro.

  FREE: p_erro.

  SELECT SINGLE ebeln
    INTO @DATA(_ebeln)
    FROM ekpo
   WHERE ebeln = @lv_ebeln
     AND ebelp = @lv_ebelp.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Pedido/Item não Encontrado!' DISPLAY LIKE 'E'.
    p_erro = abap_true.
  ENDIF.

ENDFORM.
