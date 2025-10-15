*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_PAI
*&---------------------------------------------------------------------*



MODULE USER_COMMAND_0100 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM: F_SELECIONAR_DADOS,
               F_PROCESSA_DADOS,
               F_REFRESH_ALV USING '0100'.
  ENDCASE.

ENDMODULE.

MODULE USER_COMMAND_0110 INPUT.

  CASE SY-UCOMM.
    WHEN 'CONFIRM'.

      ZLEST0153-LAND1 = 'BR'.

      IF ZLEST0153-LZONE IS INITIAL.
        MESSAGE 'Zona não informada!' TYPE 'S'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM TZONE INTO @DATA(_WL_TZONE)
       WHERE LAND1 = @ZLEST0153-LAND1
         AND ZONE1 = @ZLEST0153-LZONE.

      IF SY-SUBRC NE 0.
        MESSAGE 'Zona inválida!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZLEST0153-LIFNR IS INITIAL ) AND ( ZLEST0153-KUNNR IS INITIAL ).
        MESSAGE 'Informe Cliente ou Fornecedor!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ( ZLEST0153-LIFNR IS NOT INITIAL ) AND ( ZLEST0153-KUNNR IS NOT INITIAL ).
        MESSAGE 'Informe Cliente ou Fornecedor!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF ZLEST0153-LIFNR IS NOT INITIAL.
        SELECT SINGLE *
          FROM LFA1 INTO @DATA(_WL_LFA1)
         WHERE LIFNR = @ZLEST0153-LIFNR.

        IF SY-SUBRC NE 0.
          MESSAGE 'Fornecedor inválido!' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      IF ZLEST0153-KUNNR IS NOT INITIAL.
        SELECT SINGLE *
          FROM KNA1 INTO @DATA(_WL_KNA1)
         WHERE KUNNR = @ZLEST0153-KUNNR.

        IF SY-SUBRC NE 0.
          MESSAGE 'Cliente inválido!' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.


      IF VG_OPERACAO = C_NOVO.
        SELECT SINGLE *
          FROM ZLEST0153 INTO @DATA(_WL_0153)
         WHERE LAND1 = @ZLEST0153-LAND1
           AND LZONE = @ZLEST0153-LZONE
           AND LIFNR = @ZLEST0153-LIFNR
           AND KUNNR = @ZLEST0153-KUNNR.
        IF SY-SUBRC EQ 0.
          MESSAGE 'Já existe um registro com esses dados!' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = 'Confirmação'
          TEXT_QUESTION         = 'Deseja realmente gravar o registro?'
          TEXT_BUTTON_1         = 'Sim'
          TEXT_BUTTON_2         = 'Não'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ''
        IMPORTING
          ANSWER                = VAR_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      CHECK VAR_ANSWER EQ '1'.

      ZLEST0153-US_REGISTRO = SY-UNAME.
      ZLEST0153-DT_REGISTRO = SY-DATUM.
      ZLEST0153-HR_REGISTRO = SY-UZEIT.

      MODIFY ZLEST0153 FROM ZLEST0153.

      IF SY-SUBRC = 0.
        MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Houve um erro ao gravar o registro!' TYPE 'S'.
      ENDIF.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

MODULE HELP_LZONE INPUT.

  DATA: GT_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        GT_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF GT_TZONT OCCURS 0,
    ZONE1    TYPE TZONT-ZONE1,
    VTEXT    TYPE TZONT-VTEXT,
  END OF GT_TZONT.

  CLEAR: GT_TZONT[].

  SELECT *
    FROM TZONT INTO CORRESPONDING FIELDS OF TABLE GT_TZONT
   WHERE SPRAS = SY-LANGU
     AND LAND1 = 'BR'.

  SORT GT_TZONT BY ZONE1.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ZONE1'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZLEST0153-LZONE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = GT_TZONT
      RETURN_TAB      = GT_RETURN_TAB
      DYNPFLD_MAPPING = GT_DSELC.

ENDMODULE.
