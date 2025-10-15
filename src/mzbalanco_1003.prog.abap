*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1003 .
*----------------------------------------------------------------------*

  DATA: VG_DESC_SAKNR_LUCRO    LIKE SKAT-TXT50,
        VG_DESC_SAKNR_PREJUIZO LIKE SKAT-TXT50.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1003 OUTPUT.

  DATA: WA_FCODE TYPE SY-UCOMM,
        IT_FCODE LIKE TABLE OF WA_FCODE.

  CLEAR: VG_DESC_SAKNR_LUCRO,
         VG_DESC_SAKNR_PREJUIZO.

  wa_fcode = OK_SELALL.
  APPEND wa_fcode TO it_fcode.

  wa_fcode = OK_DSELALL.
  APPEND wa_fcode TO it_fcode.

  SET PF-STATUS 'PF1003' EXCLUDING it_fcode.
  SET TITLEBAR 'TL1003'.

  IF IT_ZGLT046-KTOPL IS NOT INITIAL.

    IF IT_ZGLT046-SAKNR_LUCRO IS NOT INITIAL.
      SELECT SINGLE TXT50 INTO VG_DESC_SAKNR_LUCRO
        FROM SKAT
       WHERE SPRAS EQ SY-LANGU
         AND KTOPL EQ IT_ZGLT046-KTOPL
         AND SAKNR EQ IT_ZGLT046-SAKNR_LUCRO.
    ENDIF.

    IF IT_ZGLT046-SAKNR_PREJUIZO IS NOT INITIAL.
      SELECT SINGLE TXT50 INTO VG_DESC_SAKNR_PREJUIZO
        FROM SKAT
       WHERE SPRAS EQ SY-LANGU
         AND KTOPL EQ IT_ZGLT046-KTOPL
         AND SAKNR EQ IT_ZGLT046-SAKNR_PREJUIZO.
    ENDIF.

  ENDIF.

  IF CK_EDITAR EQ ABAP_TRUE.
    LOOP AT SCREEN.
      IF ( SCREEN-NAME EQ 'IT_ZGLT046-VERSN' ).
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_1003  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1003 INPUT.

  DATA: WA_ZGLT046 TYPE ZGLT046.

  IF OK_CODE_1003 EQ OK_CONF.
    IF CK_EDITAR EQ ABAP_FALSE.
      SELECT SINGLE * INTO WA_ZGLT046
        FROM ZGLT046
       WHERE VERSN EQ IT_ZGLT046-VERSN.

      IF SY-SUBRC IS INITIAL.
        MESSAGE W001 WITH IT_ZGLT046-VERSN DISPLAY LIKE 'I'.
        EXIT.
      ENDIF.
    ENDIF.

    MODIFY ZGLT046 FROM IT_ZGLT046.
    PERFORM ATUALIZA_ALV_ESTRUTURAS.
    CALL METHOD BALANCO_ALV_1002->REFRESH_TABLE_DISPLAY.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1003  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1003_EXIT INPUT.
  CLEAR: IT_ZGLT046.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_1003_EXIT  INPUT
