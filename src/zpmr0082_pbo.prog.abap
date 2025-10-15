*&---------------------------------------------------------------------*
*&  Include           ZPMR0028_PBO
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  PERFORM f_create_long_text.
  CHECK sy-ucomm <> 'SAVE'.
  obj_main->pbo( ).
  PERFORM modif_screen.

  DATA(fieldname) = 'V_ORDEM'.  "Nome do campo onde você deseja definir o cursor
  SET CURSOR FIELD fieldname .

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIF_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modif_screen OUTPUT.
*  PERFORM MODIF_SCREEN.

ENDMODULE.

MODULE status_0001 OUTPUT.
  SET PF-STATUS '1'.
*  SET TITLEBAR 'xxx'.
  LOOP AT SCREEN.
    IF screen-name = 'ZTPM_D_USUARIO-LOGIN'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_APONTA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc_aponta_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_aponta LINES tc_aponta-lines.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_APONTA'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc_aponta_get_lines OUTPUT.
  g_tc_aponta_lines = sy-loopc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PREENCHE_USUARIO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE preenche_usuario OUTPUT.

  IF sy-uname = 'FFONSECA' OR
     sy-uname = 'ABAP'.

    IF sy-sysid = 'QAS'.
      ztpm_d_usuario-cpf_nr = '593.199.251-00'.
    ELSEIF sy-sysid = 'DEV'.
      ztpm_d_usuario-cpf_nr = ''.
    ENDIF.

    SELECT SINGLE login
      FROM ztpm_d_usuario
      INTO ztpm_d_usuario-login
      WHERE cpf_nr = ztpm_d_usuario-cpf_nr.

    IF sy-subrc = 0.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '=ENTER' "ENTER
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.
    ENDIF.

  ENDIF.

ENDMODULE.
