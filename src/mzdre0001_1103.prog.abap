*----------------------------------------------------------------------*
***INCLUDE MZDRE0001_1103 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1103 OUTPUT.

  SET PF-STATUS 'PF1103'.
  SET TITLEBAR 'TL1103'.

  LOOP AT SCREEN.
    IF ( SCREEN-NAME EQ 'ZGL020_DRE_DADOS-BUKRS' ) AND ( WA_DRE_DADOS_ALV-BUKRS IS NOT INITIAL ).
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF ( SCREEN-NAME EQ 'ZGL020_DRE_DADOS-VERSN' ) AND ( WA_DRE_DADOS_ALV-VERSN IS NOT INITIAL ).
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF ( SCREEN-NAME EQ 'ZGL020_DRE_DADOS-MONAT' ) AND ( WA_DRE_DADOS_ALV-MONAT IS NOT INITIAL ).
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF ( SCREEN-NAME EQ 'ZGL020_DRE_DADOS-GJAHR' ) AND ( WA_DRE_DADOS_ALV-GJAHR IS NOT INITIAL ).
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF NOT ZGL020_DRE_DADOS-BUKRS IS INITIAL.
    SELECT SINGLE BUTXT INTO WA_DRE_DADOS_ALV-BUTXT
      FROM T001
     WHERE BUKRS EQ ZGL020_DRE_DADOS-BUKRS.

    IF NOT ZGL020_DRE_DADOS-VERSN IS INITIAL.
      SELECT SINGLE VSTXT INTO WA_DRE_DADOS_ALV-VSTXT
        FROM ZGL015_DRE_EST01
       WHERE VERSN EQ ZGL020_DRE_DADOS-VERSN.
    ENDIF.
  ENDIF.

  ZGL020_DRE_DADOS-DRE_NEW = 'X'.

ENDMODULE.                 " STATUS_1103  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1103_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1103_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_1103_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1103 INPUT.

  DATA: WA_DADOS    TYPE ZGL020_DRE_DADOS,
        WA_DRE_EST  TYPE ZGL015_DRE_EST01,
        WA_DRE_EST8 TYPE ZGL015_DRE_EST08.

  CASE OK_CODE_1103.
    WHEN OK_SAVE.
      IF ZGL020_DRE_DADOS-MONAT GT 12 OR ZGL020_DRE_DADOS-MONAT LE 0.
        MESSAGE S013 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD ZGL020_DRE_DADOS-BUKRS.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S091(8B) DISPLAY LIKE 'E' WITH ZGL020_DRE_DADOS-BUKRS.
      ENDIF.

      SELECT SINGLE * INTO WA_DRE_EST
        FROM ZGL015_DRE_EST01
       WHERE BUKRS EQ ZGL020_DRE_DADOS-BUKRS
         AND VERSN EQ ZGL020_DRE_DADOS-VERSN.

      IF NOT SY-SUBRC IS INITIAL.

        SELECT SINGLE * INTO WA_DRE_EST8
          FROM ZGL015_DRE_EST08
         WHERE BUKRS_B EQ ZGL020_DRE_DADOS-BUKRS
           AND VERSN   EQ ZGL020_DRE_DADOS-VERSN.

        IF NOT SY-SUBRC IS INITIAL.
          MESSAGE S002 WITH WA_DADOS-VERSN WA_DADOS-BUKRS DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      ENDIF.

      SELECT SINGLE * INTO WA_DADOS
        FROM ZGL020_DRE_DADOS
       WHERE BUKRS EQ ZGL020_DRE_DADOS-BUKRS
         AND VERSN EQ ZGL020_DRE_DADOS-VERSN
         AND MONAT EQ ZGL020_DRE_DADOS-MONAT
         AND GJAHR EQ ZGL020_DRE_DADOS-GJAHR.

      IF SY-SUBRC IS INITIAL.
        MESSAGE S001 WITH WA_DADOS-BUKRS WA_DADOS-VERSN WA_DADOS-GJAHR WA_DADOS-MONAT DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      ZGL020_DRE_DADOS-STATUS_PROC = 0.
      ZGL020_DRE_DADOS-UNAME = SY-UNAME.
      ZGL020_DRE_DADOS-DATUM = SY-DATUM.
      ZGL020_DRE_DADOS-UZEIT = SY-UZEIT.
      MODIFY ZGL020_DRE_DADOS.
      PERFORM CONSULTAR_DRES.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1103  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1104 OUTPUT.

  SET PF-STATUS 'PF1104'.
  SET TITLEBAR 'TL1103'.

  IF NOT ZGL020_DRE_DADOS-BUKRS IS INITIAL.

    SELECT SINGLE BUTXT INTO WA_DRE_DADOS_ALV-BUTXT
      FROM T001
     WHERE BUKRS EQ ZGL020_DRE_DADOS-BUKRS.

    IF NOT ZGL020_DRE_DADOS-VERSN IS INITIAL.

      SELECT SINGLE VSTXT INTO WA_DRE_DADOS_ALV-VSTXT
        FROM ZGL015_DRE_EST01
       WHERE BUKRS EQ ZGL020_DRE_DADOS-BUKRS
         AND VERSN EQ ZGL020_DRE_DADOS-VERSN.

      IF SY-SUBRC IS NOT INITIAL.
        SELECT SINGLE * INTO WA_ZGL015_DRE_EST08
          FROM ZGL015_DRE_EST08
         WHERE BUKRS_B EQ ZGL020_DRE_DADOS-BUKRS
           AND VERSN   EQ ZGL020_DRE_DADOS-VERSN.

        SELECT SINGLE VSTXT INTO WA_DRE_DADOS_ALV-VSTXT
          FROM ZGL015_DRE_EST01
         WHERE BUKRS EQ WA_ZGL015_DRE_EST08-BUKRS
           AND VERSN EQ WA_ZGL015_DRE_EST08-VERSN.
      ENDIF.

    ENDIF.

  ENDIF.

  ZGL020_DRE_DADOS-DRE_NEW = 'X'.

ENDMODULE.                 " STATUS_1104  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1104 INPUT.

  DATA: WA_DADOS_P   TYPE ZGL020_DRE_DADOS.

  CASE OK_CODE_1103.
    WHEN OK_LIBERAR.

      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD ZGL020_DRE_DADOS-BUKRS.
      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE S091(8B) DISPLAY LIKE 'E' WITH ZGL020_DRE_DADOS-BUKRS.
      ENDIF.

      SELECT SINGLE * INTO WA_DADOS_P
        FROM ZGL020_DRE_DADOS
       WHERE BUKRS EQ ZGL020_DRE_DADOS-BUKRS
         AND VERSN EQ ZGL020_DRE_DADOS-VERSN
         AND MONAT EQ ZGL020_DRE_DADOS-MONAT
         AND GJAHR EQ ZGL020_DRE_DADOS-GJAHR.

      IF NOT SY-SUBRC IS INITIAL.
        MESSAGE S006 WITH WA_DADOS-BUKRS WA_DADOS-VERSN WA_DADOS-GJAHR WA_DADOS-MONAT DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      ZGL020_DRE_DADOS-UNAME_PROG = SY-UNAME.
      MODIFY ZGL020_DRE_DADOS.
      PERFORM CONSULTAR_DRES.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1104  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDA_PARAMETROS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDA_PARAMETROS INPUT.
  IF ZGL020_DRE_DADOS-MONAT GT 12 OR ZGL020_DRE_DADOS-MONAT LE 0.
    MESSAGE S013 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
ENDMODULE.                 " VALIDA_PARAMETROS  INPUT

*&---------------------------------------------------------------------*
*&      Module  VALIDA_EMPRESA_PERFIL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDA_EMPRESA_PERFIL INPUT.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' ID 'BUKRS' FIELD ZGL020_DRE_DADOS-BUKRS.
  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE E091(8B) WITH ZGL020_DRE_DADOS-BUKRS.
  ENDIF.
ENDMODULE.                 " VALIDA_EMPRESA_PERFIL  INPUT
