*&---------------------------------------------------------------------*
*&  Include           ZSDT0084PBO
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA lv_title_gui TYPE c LENGTH 30.

  CONTROLS: main_tab TYPE TABSTRIP.

*** CS2020000143 inicio
  DATA: fcode    TYPE TABLE OF sy-ucomm,
        wa_fcode TYPE sy-ucomm.

  " 22.09.2022 - RAMON 19450 -->
  IF gv_bukrs IS INITIAL AND gv_matnr IS INITIAL.

    GET PARAMETER ID 'BKR' FIELD gv_bukrs.
    GET PARAMETER ID 'MTN' FIELD gv_matnr.

    " após recuperar limpa memoria
    SET PARAMETER ID 'BKR' FIELD space.
    SET PARAMETER ID 'MTN' FIELD space.

  ENDIF.
  " 22.09.2022 - RAMON 19450 --<

  IF gv_bukrs IS NOT INITIAL.
    lv_title_gui = 'SOLFORMLOTE'.
  ELSE.
    lv_title_gui = 'SOLFORMLOTE2'.
  ENDIF.

  IF wa_zsdt0158_saida-mensagem IS INITIAL.
    wa_fcode = 'MESSAGE'.
    APPEND wa_fcode TO fcode.
    SET PF-STATUS 'STATUS3' EXCLUDING fcode.
    SET TITLEBAR lv_title_gui WITH gv_bukrs gv_matnr.
  ELSE.
    SET PF-STATUS 'STATUS3'.
    SET TITLEBAR lv_title_gui.
  ENDIF.
*** CS2020000143 Fim

  main_tab-activetab = i_main_tab-pressed_tab.

  " 23.09.2022 - RAMON 19450 -->
  i_main_tab-subscreen = '0102'.

*  i_main_tab-subscreen = COND #( WHEN i_main_tab-pressed_tab = c_main_tab-tab1 THEN '0102'
*                                 WHEN i_main_tab-pressed_tab = c_main_tab-tab2 THEN '0103' ).
  " 22.09.2022 - RAMON 19450 --<
ENDMODULE.

CLASS custom_screen DEFINITION.
  PUBLIC SECTION.
    METHODS set_screen_cadastro
      IMPORTING
        screen_a TYPE sy-repid.
ENDCLASS.

CLASS custom_screen IMPLEMENTATION.
  METHOD set_screen_cadastro.
    MOVE screen_a TO screen_cadastro.
  ENDMETHOD.
ENDCLASS.


MODULE status_0102 OUTPUT.

  PERFORM f_controle_tela_0102.

  "PERFORM f_screen_0102_control.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0104 OUTPUT.
  SET PF-STATUS 'STATUS2'.
  SET TITLEBAR 'T02'.
ENDMODULE.
