*&---------------------------------------------------------------------*
*&  Include           ZPM_VIEW_10_SAAFI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE STATUS_0100 OUTPUT.
*  SET PF-STATUS '0100'.
*  SET TITLEBAR 'xxx'.
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'SAVE'. PERFORM ZF_SAVE.
    WHEN 'DEFF'. PERFORM ZF_ELIMINA.
    WHEN 'INSE'. PERFORM ZF_INSERE.
    WHEN 'BACK'. LEAVE PROGRAM.
    WHEN 'EXIT' OR 'CANC'. LEAVE TO SCREEN 0.
  ENDCASE.
*
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_FOR_CODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALUE_FOR_CODE INPUT.
* MatchCode dos Codes
  SELECT CODE KURZTEXT INTO TABLE IT_CODE
    FROM QPCT
    WHERE KATALOGART = LC_S
      AND CODEGRUPPE = LC_FPN
      AND SPRACHE    = LC_PT.
*
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      RETFIELD        = 'CODE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_CODE
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  LOOP AT IT_RETURN INTO WA_RETURN.
    "The selected field needs to be passed to the screen field
*      z_equi_equnr = wa_return-fieldval.
    READ TABLE IT_CODE WITH KEY CODE = WA_RETURN-FIELDVAL.
    IF SY-SUBRC IS INITIAL.
      V_CODE = IT_CODE-CODE.
      V_DESC_CODE = IT_CODE-KURZTEXT.
    ENDIF.
  ENDLOOP.
*
*
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_FOR_EQART  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALUE_FOR_EQART INPUT.
* MatchCode das Operações
  SELECT EQART EARTX INTO TABLE IT_EQART
    FROM T370K_T
    WHERE SPRAS   = LC_PT.
*
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      RETFIELD        = 'EQART'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_EQART
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  LOOP AT IT_RETURN INTO WA_RETURN.
    "The selected field needs to be passed to the screen field
*      z_equi_equnr = wa_return-fieldval.
    READ TABLE IT_EQART WITH KEY EQART = WA_RETURN-FIELDVAL.
    IF SY-SUBRC IS INITIAL.
      V_EQART = IT_EQART-EQART.
      V_DESC_EQART = IT_EQART-EARTX.
    ENDIF.
  ENDLOOP.
*
*
ENDMODULE.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VALUE_FOR_TYPBZ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALUE_FOR_TYPBZ INPUT.
* MatchCode dos Mocelos
  CLEAR IT_ZVAL[].
  SELECT ZVAL CONST
    FROM ZTPARAM
    INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
    WHERE PARAM EQ LC_TP_OBJ
    AND ABASTEC EQ LC_X.
  LOOP AT IT_PARAM.
    IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
    APPEND IT_ZVAL.
  ENDLOOP.
*
  SELECT TYPBZ INTO TABLE IT_TYPBZ
    FROM EQUI FOR ALL ENTRIES IN IT_ZVAL
    WHERE EQTYP   = IT_ZVAL-ZVAL
      AND EQART   = V_EQART.
*
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      RETFIELD        = 'TYPBZ'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_TYPBZ
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  LOOP AT IT_RETURN INTO WA_RETURN.
    "The selected field needs to be passed to the screen field
*      z_equi_equnr = wa_return-fieldval.
    READ TABLE IT_TYPBZ WITH KEY TYPBZ = WA_RETURN-FIELDVAL.
    IF SY-SUBRC IS INITIAL.
      V_TYPBZ = IT_TYPBZ-TYPBZ.
    ENDIF.
  ENDLOOP.
*
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_CODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICA_CODE INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'. LEAVE TO SCREEN 0.
  ENDCASE.
*
  IF NOT V_CODE IS INITIAL.
    SELECT CODE KURZTEXT INTO TABLE IT_CODE
      FROM QPCT UP TO 1 ROWS
      WHERE KATALOGART = LC_S
        AND CODEGRUPPE = LC_FPN
        AND CODE       = V_CODE
        AND SPRACHE    = LC_PT.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_CODE INDEX 1.
      V_CODE = IT_CODE-CODE.
      V_DESC_CODE = IT_CODE-KURZTEXT.
    ELSE.
      MESSAGE E000(ZPPM001) WITH 'Operação não cadastrada'.
    ENDIF.
  ELSE.
    MESSAGE S000(ZPPM001) DISPLAY LIKE 'E'
          WITH 'Operação não cadastrada'.
*        MESSAGE E000(ZPPM001) WITH 'Operação não cadastrada'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_EQART  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICA_EQART INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'. LEAVE TO SCREEN 0.
  ENDCASE.
*
  IF NOT V_EQART IS INITIAL.
    SELECT EQART EARTX INTO TABLE IT_EQART
    FROM T370K_T
    WHERE EQART  = V_EQART
      AND SPRAS  = LC_PT.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_EQART INDEX 1.
      V_EQART = IT_EQART-EQART.
      V_DESC_EQART = IT_EQART-EARTX.
    ELSE.
      MESSAGE E000(ZPPM001) WITH 'Classe não cadastrada'.
    ENDIF.
  ELSE.
    MESSAGE S000(ZPPM001) DISPLAY LIKE 'E'
         WITH 'Classe não cadastrada'.
*    MESSAGE E000(ZPPM001) WITH 'Classe não cadastrada'.
  ENDIF.
*
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_TYPBZ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICA_TYPBZ INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'. LEAVE TO SCREEN 0.
    WHEN 'SAVE' OR 'INSE'.
      CLEAR: V_TYPBZ, V_CONSUMO, V_DESVIO.
      CLEAR SY-UCOMM.
  ENDCASE.
*
  CLEAR IT_ZVAL[].
  IF NOT V_TYPBZ IS INITIAL.
    SELECT ZVAL CONST
     FROM ZTPARAM
     INTO CORRESPONDING FIELDS OF TABLE IT_PARAM
     WHERE PARAM EQ LC_TP_OBJ
     AND ABASTEC EQ LC_X.
    LOOP AT IT_PARAM.
      IT_ZVAL-ZVAL = IT_PARAM-ZVAL.
      APPEND IT_ZVAL.
    ENDLOOP.
*
    SELECT TYPBZ INTO TABLE IT_TYPBZ
      FROM EQUI FOR ALL ENTRIES IN IT_ZVAL
      WHERE EQTYP   = IT_ZVAL-ZVAL
        AND EQART   = V_EQART
        AND TYPBZ   = V_TYPBZ.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_TYPBZ INDEX 1.
      V_TYPBZ = IT_TYPBZ-TYPBZ.
    ELSE.
      MESSAGE E000(ZPPM001) WITH 'Modelo não cadastrado'.
    ENDIF.
  ELSE.
*    MESSAGE S000(ZPPM001) DISPLAY LIKE 'E'
*         WITH 'Modelo não cadastrado'.
*    MESSAGE E000(ZPPM001) WITH 'Modelo não cadastrado'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
