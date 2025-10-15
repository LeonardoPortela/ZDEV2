*----------------------------------------------------------------------*
***INCLUDE ZV45A_ZM_CHECK_TKNUMI01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  ZM_TELA_8309  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
TABLES: zsdt0225."
MODULE zm_tela_8309 OUTPUT.
  IF vbak-auart NE 'ZTRO'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GR1'.
        screen-required = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF sy-tcode NE 'VA01'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GR2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ZM_CHECK_TKNUM_8309  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_check_tknum_8309 INPUT.

  CHECK vbak-tknum IS NOT INITIAL.

  SELECT SINGLE tknum
    FROM vttk
    INTO vbak-tknum
  WHERE  tknum EQ vbak-tknum.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e836(sd) WITH 'Nº Transporte Inexistente.'.
  ENDIF.

ENDMODULE.

*-CS2022000686-12.01.2023-#100837-JT-inicio
MODULE zm_check_navio INPUT.

  DATA: l_seq,
        xtcode   TYPE tcode.

  FREE: xtcode.
  IMPORT xtcode TO xtcode FROM MEMORY ID 'ZSDR0147'.

  IF sy-subrc = 0 AND xtcode = 'ZSDT0158'.
    SELECT id_seq
      INTO l_seq
      FROM zsdt0225
        UP TO 1 ROWS
     WHERE nr_ov  = vbak-vbeln
       AND origem = 'PI'.
    ENDSELECT.

    CHECK sy-subrc = 0.

    IF zsdt0225-navio IS INITIAL.
      MESSAGE e836(sd) WITH 'Informar o Navio!'.
    ENDIF.
  ENDIF.

ENDMODULE.

MODULE zm_check_locoper INPUT.

  DATA: l_seq2.

  FREE: xtcode.
  IMPORT xtcode TO xtcode FROM MEMORY ID 'ZSDR0147'.

  IF sy-subrc = 0 AND xtcode = 'ZSDT0158'.
    SELECT id_seq
      INTO l_seq2
      FROM zsdt0225
        UP TO 1 ROWS
     WHERE nr_ov  = vbak-vbeln
       AND origem = 'PI'.
    ENDSELECT.

    CHECK sy-subrc = 0.

    IF zsdt0225-local_operacao IS INITIAL.
      MESSAGE e836(sd) WITH 'Informar o Local de Operação!'.
    ENDIF.
  ENDIF.

ENDMODULE.
*-CS2022000686-12.01.2023-#100837-JT-fim

*&---------------------------------------------------------------------*
*&      Module  Z_CAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_campos OUTPUT.
  IF sy-tcode EQ 'VA03'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GP1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " Z_CAMPOS  OUTPUT

*-CS2019001753-09.01.2023-#84936-JT-inicio
MODULE zm_outros_dados OUTPUT.

  DATA: l_seq3.

  LOOP AT SCREEN.
    IF screen-name = 'ZSDT0225-NAVIO' OR
       screen-name = 'ZSDT0225-LOCAL_OPERACAO'.
      IF sy-tcode(4) = 'VA03'.
        screen-input = 0.
      ELSE.
        SELECT id_seq
          INTO l_seq3
          FROM zsdt0225
            UP TO 1 ROWS
         WHERE nr_ov  = vbak-vbeln
           AND origem = 'PI'.
        ENDSELECT.
        IF sy-subrc = 0.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.
*-CS2019001753-09.01.2023-#84936-JT-fim
*&---------------------------------------------------------------------*
*& Module ZM_CHECK_NAVIO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE zm_check_navio OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
