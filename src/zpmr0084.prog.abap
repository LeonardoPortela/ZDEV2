*&---------------------------------------------------------------------*
*& Report zpmr0084
*&---------------------------------------------------------------------*
*& Gestão de equipamentos locados
*&---------------------------------------------------------------------*
REPORT zpmr0084.

INCLUDE zpmr0084_top.

* Declaração de eventos
DATA: BEGIN OF eventos OCCURS 0,
        evento TYPE string,
      END OF eventos.
DATA: p_screen1000(1) TYPE c.
* Início do programa
START-OF-SELECTION.
  CALL SCREEN 100.

* Definição da tela
MODULE status_0100 OUTPUT.

  CLEAR: p_screen1000.
  IMPORT p_screen1000 FROM MEMORY ID 'ZSCREEN1000'.

  IF p_screen1000 = 'X'.
    SUBMIT zpmr0086 AND RETURN .
  ENDIF.

  SET PF-STATUS '0100'. " Define o status da tela
  SET TITLEBAR 'TITULO'. " Define o título da tela

ENDMODULE.

* Tratamento de eventos
MODULE user_command_0100 INPUT.

  PERFORM get_acesso.

  IF acesso IS NOT INITIAL.
    CASE sy-ucomm.
      WHEN 'BTN_1'.
        CALL TRANSACTION 'ZPM0103'.
      WHEN 'BTN_2'.
        SUBMIT zpmr0086 AND RETURN .
      WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ELSE.

    MESSAGE 'Seu usuário não tem permissão, favor consultar o Gestor ou Abrir uma SA!' TYPE 'S' DISPLAY LIKE 'E'.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*      EXPORTING
*        titel = 'Information'
*        txt1  = 'Seu usário não tem permissão, favor consultar o Gestor'
*        txt2  = 'ou Abrir uma SA!'
*        "txt3  = 'Information line 3'
*        "txt4  = 'Information line 4'
*      .
    EXIT.
  ENDIF.

ENDMODULE.

INCLUDE zpmr0084_i01.

FORM get_acesso.

  DATA: it_param TYPE  ustyp_t_parameters.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = it_param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.


  IF it_param IS NOT INITIAL.
    LOOP AT it_param ASSIGNING FIELD-SYMBOL(<_get>) WHERE parva = 'X' AND parid+0(7) = 'ZPM0104'.
      CLEAR: acesso.
      CASE <_get>-parid.
        WHEN 'ZPM0104_EDIT'.
          acesso = 'E'.
        WHEN 'ZPM0104_FULL'.
          acesso = 'F'.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ELSE.
  ENDIF.

ENDFORM.
