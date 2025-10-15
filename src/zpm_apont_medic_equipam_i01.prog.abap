*&---------------------------------------------------------------------*
*&  Include           ZPM_APONT_MEDIC_EQUIPAM_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN LC_BACK OR LC_EXIT OR LC_CANC.
      PERFORM ZF_LIMPAR.
      LEAVE PROGRAM.
    WHEN 'BTN_CANCELAR'.     PERFORM ZF_LIMPAR.
    WHEN LC_CRIA.            PERFORM ZF_CRIAR.
    WHEN LC_SAVEP .          PERFORM ZF_SALVAR.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICA_DATA INPUT.

  IF SY-UCOMM EQ LC_BACK OR
     SY-UCOMM EQ LC_EXIT OR
     SY-UCOMM EQ LC_CANC.
    LEAVE PROGRAM.
  ENDIF.
*

  IF MHIO-ADDAT IS INITIAL.
    MESSAGE E000(ZPPM001)  DISPLAY LIKE 'E'
      WITH 'Digite a data do apontamento'.
  ELSE.
    SET CURSOR FIELD 'MHIO-ADTIME'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_HORA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICA_HORA INPUT.

  IF MHIO-ADTIME IS INITIAL.
    MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
      WITH 'Digite a hora do apontamento'.
  ELSE.
    SET CURSOR FIELD 'MHIO-WARPL'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_PLANO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE VERIFICA_PLANO INPUT.
*  IF MHIO-QMNUM IS INITIAL.
*    IF MHIO-WARPL IS INITIAL.
*      MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
*        WITH 'Digite o Plano do apontamento'.
*    ELSE.
*      SET CURSOR FIELD 'V_EQUNR'.
*    ENDIF.
*  ELSE.
*    SET CURSOR FIELD 'V_EQUNR'.
*  ENDIF.
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_NOTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICA_NOTA INPUT.
*
  IF SY-UCOMM EQ 'BTN_CANCELAR'.
    PERFORM ZF_LIMPAR.
  ELSEIF SY-UCOMM EQ 'BTN_APONT'.
    IF MHIO-QMNUM IS INITIAL.
      EXIT.
    ENDIF.
  ENDIF.
*
  IF MHIO-WARPL IS INITIAL.
    IF MHIO-QMNUM IS INITIAL.
      CLEAR V_EQUNR.
*      SET SCREEN 100.
      SET CURSOR FIELD 'MHIO-WARPL'.
      MESSAGE S000(ZPPM001) DISPLAY LIKE 'E'
        WITH 'Digite a Nota ou o Plano do apontamento'.
    ELSE.
* Lista equipamentos da Nota para escolha
      PERFORM ZF_SELECT_NOTAS.
      SET CURSOR FIELD 'V_EQUNR'.
    ENDIF.
  ELSE.
* Lista equipamentos do Plano para escolha
    IF MHIO-QMNUM IS INITIAL.
      PERFORM ZF_SELECT_PLANO.
      SET CURSOR FIELD 'V_EQUNR'.
    ENDIF.
  ENDIF.
  IF NOT V_EQUNR IS INITIAL.
    SELECT EQART INTO V_TIPOBJ
      FROM EQUI UP TO 1 ROWS
      WHERE EQUNR = V_EQUNR.
    ENDSELECT.
*
    SELECT EARTX INTO V_DESCRSAP
      FROM T370K_T UP TO 1 ROWS
     WHERE EQART EQ V_TIPOBJ
       AND SPRAS EQ SY-LANGU.
    ENDSELECT.

    SELECT SINGLE TPLNR
      FROM VIQMEL
      INTO @DATA(V_TPLNR)
      WHERE EQUNR EQ @V_EQUNR.

    SELECT SINGLE PLTXT
      FROM IFLO
      INTO V_PLTXT
      WHERE TPLNR EQ V_TPLNR.
*
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_EQUIP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VERIFICA_EQUIP INPUT.
*  IF V_EQUNR IS INITIAL.
*    MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
*      WITH 'Digite o Equipamento do apontamento'.
*  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SELECT_NOTAS .
*
  SELECT M~WARPL M~QMNUM V~EQUNR E~EQKTX V~OBJNR V~IWERK INTO TABLE IT_NOTAS_P
    FROM MHIO AS M
    INNER JOIN VIQMEL AS V ON M~QMNUM EQ V~QMNUM
    INNER JOIN ITOB   AS F ON F~EQUNR EQ V~EQUNR
    LEFT OUTER JOIN EQKT AS E
           ON V~EQUNR EQ E~EQUNR
        WHERE M~QMNUM = MHIO-QMNUM.
  SORT IT_NOTAS_P ASCENDING BY EQUNR.
*
  SELECT WAPOS WARPL BAUTL QMNUM INTO TABLE IT_MPOS
    FROM MPOS FOR ALL ENTRIES IN IT_NOTAS_P
    WHERE WARPL EQ IT_NOTAS_P-WARPL
      AND QMNUM EQ IT_NOTAS_P-QMNUM.
  SORT IT_MPOS ASCENDING BY QMNUM.
*
  SELECT V~EQUNR E~EQKTX V~OBJNR V~IWERK INTO TABLE IT_EQUIP
  FROM VIQMEL AS V INNER JOIN EQKT AS E
         ON V~EQUNR EQ E~EQUNR
  WHERE QMNUM = MHIO-QMNUM.
  IF SY-SUBRC IS INITIAL.
*
    READ TABLE IT_EQUIP INDEX 1.
*
    READ TABLE IT_NOTAS_P WITH KEY EQUNR = IT_EQUIP-EQUNR
       BINARY SEARCH.
    IF NOT SY-SUBRC IS INITIAL.
      MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
        WITH 'Nota não encontrada'.
    ENDIF.
*
    READ TABLE IT_MPOS WITH KEY QMNUM = IT_NOTAS_P-QMNUM
       BINARY SEARCH.
    IF NOT SY-SUBRC IS INITIAL.
      MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
        WITH 'Plano não encontrado para a Nota'.
    ENDIF.
*
    LV_OBJNR = IT_EQUIP-OBJNR.
    PERFORM ZF_STATUS_NOTA.

    V_QMNUM  = MHIO-QMNUM.
    V_EQUNR  = IT_EQUIP-EQUNR.
    V_IWERK  = IT_EQUIP-IWERK.
  ENDIF.
*
*  CHECK SY-UCOMM IS INITIAL.
*
*  CALL SCREEN 300  STARTING AT 30 3
*                   ENDING   AT 130 17.
**
*  CASE SY-UCOMM.
*    WHEN 'VOLTAR'.
*      READ TABLE IT_EQUIP WITH KEY MARC = 'X'.
*      IF SY-SUBRC IS INITIAL.
*        V_EQUNR = IT_EQUIP-EQUNR.
*      ENDIF.
*  ENDCASE.
*  CLEAR SY-UCOMM.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECT_PLANO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_SELECT_PLANO .


  SELECT MP~WAPOS MP~WARPL MP~BAUTL MP~QMNUM MP~GEWRK CR~ARBPL
    FROM MPOS AS MP
    LEFT JOIN CRHD AS CR ON CR~OBJID EQ MP~GEWRK
    INTO TABLE IT_MPOS
    WHERE WARPL EQ MHIO-WARPL
    ORDER BY QMNUM.

  SELECT M~QMNUM V~EQUNR E~EQKTX V~OBJNR V~IWERK
    INTO TABLE IT_NOTAS
    FROM MHIO AS M INNER JOIN VIQMEL AS V
           ON M~QMNUM EQ V~QMNUM
    LEFT OUTER JOIN EQKT AS E
           ON V~EQUNR EQ E~EQUNR
    FOR ALL ENTRIES IN IT_MPOS
    WHERE M~WARPL EQ IT_MPOS-WARPL
      AND M~WPPOS EQ IT_MPOS-WAPOS
      AND M~QMNUM EQ IT_MPOS-QMNUM.

  SELECT OBJNR STAT INTO TABLE IT_JEST
    FROM JEST FOR ALL ENTRIES IN IT_NOTAS
    WHERE OBJNR EQ IT_NOTAS-OBJNR
      AND INACT EQ ''.
  SORT IT_JEST ASCENDING BY OBJNR.
*
  LOOP AT IT_NOTAS.
    READ TABLE IT_JEST WITH KEY OBJNR = IT_NOTAS-OBJNR
      BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IF IT_JEST-STAT EQ LC_I0068 OR    "MSPN
         IT_JEST-STAT EQ LC_I0070.      "MSPR
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = IT_NOTAS-QMNUM
          IMPORTING
            OUTPUT = IT_NOTAS-QMNUM.
        MODIFY IT_NOTAS.
        CONTINUE.
      ELSE.
        DELETE IT_NOTAS.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SORT IT_NOTAS ASCENDING BY QMNUM.
*
  CHECK SY-UCOMM IS INITIAL.
*
  IF IT_NOTAS[] IS INITIAL.
    MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
      WITH 'Não há notas para este Plano'.
  ELSE.

    IF LINES( IT_NOTAS[] ) EQ 1.
      IT_NOTAS = IT_NOTAS[ 1 ].
      SY-UCOMM = LC_VOLT.
    ELSE.
      CALL SCREEN 200  STARTING AT 30 3
                     ENDING   AT 130 17.
      READ TABLE IT_NOTAS WITH KEY MARC = LC_X.
    ENDIF.

    SELECT SINGLE OBJNR
      FROM EQUI INTO @DATA(V_OBJ)
      WHERE EQUNR EQ @IT_NOTAS-EQUNR.

    SELECT SINGLE LOCAS
      FROM IMPTT
      INTO @DATA(V_LOCAS)
      WHERE MPOBJ EQ @V_OBJ.

    LOOP AT IT_MPOS ASSIGNING FIELD-SYMBOL(<MPOS>).
      <MPOS>-BAUTL = V_LOCAS.
    ENDLOOP.

    CASE SY-UCOMM.
      WHEN LC_VOLT.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = IT_NOTAS-QMNUM
          IMPORTING
            OUTPUT = IT_NOTAS-QMNUM.
*
        READ TABLE IT_MPOS WITH KEY QMNUM = IT_NOTAS-QMNUM
        BINARY SEARCH.

        IF NOT SY-SUBRC IS INITIAL OR
          IT_MPOS-BAUTL IS INITIAL.
          MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
            WITH 'Código de Atividade não encontrado'.
        ENDIF.

        V_EQUNR = IT_NOTAS-EQUNR.

        MHIO-QMNUM   = IT_NOTAS-QMNUM.
*
        V_QMNUM = IT_NOTAS-QMNUM.
        LV_OBJNR = IT_NOTAS-OBJNR.
        V_IWERK  = IT_NOTAS-IWERK.

        PERFORM ZF_STATUS_NOTA.
*
*        ENDIF.
    ENDCASE.
  ENDIF.
  CLEAR SY-UCOMM.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  VALUE_FOR_QMNUM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALUE_FOR_QMNUM INPUT.
* MatchCode dos Codes
* A tabela interna IT_NOTAS é usada na tela 200.
* A IT_NOTAS_P está sendo usada para pegar o WARPL
  SELECT M~WARPL M~QMNUM V~EQUNR E~EQKTX V~OBJNR V~IWERK INTO TABLE IT_NOTAS_P
    FROM MHIO AS M INNER JOIN VIQMEL AS V
           ON M~QMNUM EQ V~QMNUM
    LEFT OUTER JOIN EQKT AS E
           ON V~EQUNR EQ E~EQUNR.
*
  SELECT WAPOS WARPL BAUTL QMNUM INTO TABLE IT_MPOS
    FROM MPOS FOR ALL ENTRIES IN IT_NOTAS_P
    WHERE WARPL EQ IT_NOTAS_P-WARPL
      AND QMNUM EQ IT_NOTAS_P-QMNUM.
  SORT IT_MPOS ASCENDING BY QMNUM.
*
  SELECT OBJNR STAT INTO TABLE IT_JEST
    FROM JEST FOR ALL ENTRIES IN IT_NOTAS_P
    WHERE OBJNR EQ IT_NOTAS_P-OBJNR
      AND INACT EQ ''.
  SORT IT_JEST ASCENDING BY OBJNR.
*
  LOOP AT IT_NOTAS_P.
*
    READ TABLE IT_MPOS WITH KEY QMNUM = IT_NOTAS_P-QMNUM
       BINARY SEARCH.
    IF NOT SY-SUBRC IS INITIAL.
      CONTINUE.
    ENDIF.
*
    READ TABLE IT_JEST WITH KEY OBJNR = IT_NOTAS_P-OBJNR
      BINARY SEARCH.
    IF SY-SUBRC IS INITIAL.
      IF IT_JEST-STAT EQ LC_I0068 OR    "MSPN
         IT_JEST-STAT EQ LC_I0070.      "MSPR

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = IT_NOTAS_P-QMNUM
          IMPORTING
            OUTPUT = IT_NOTAS_P-QMNUM.
        IT_NOTAS = IT_NOTAS_P.
        APPEND IT_NOTAS.
        CONTINUE.
      ENDIF.
    ENDIF.

*    CLEAR LV_STATUS.
*    CALL FUNCTION 'STATUS_TEXT_EDIT'
*      EXPORTING
*        CLIENT      = SY-MANDT
*        OBJNR       = IT_NOTAS-OBJNR
*        ONLY_ACTIVE = 'X'
*        SPRAS       = SY-LANGU
**       BYPASS_BUFFER           = ' '
*      IMPORTING
*        LINE        = LV_STATUS.
*    IF LV_STATUS CS LC_MSPN OR
*       LV_STATUS CS LC_MSPR.
*      DELETE IT_NOTAS.
*    ELSE.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          INPUT  = IT_NOTAS-QMNUM
*        IMPORTING
*          OUTPUT = IT_NOTAS-QMNUM.
*      MODIFY IT_NOTAS.
*      CONTINUE.
*    ENDIF.
  ENDLOOP.
  REFRESH IT_NOTAS_P.
  SORT IT_NOTAS ASCENDING BY QMNUM.
*
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      RETFIELD        = 'QMNUM'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_NOTAS
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.
  LOOP AT IT_RETURN INTO WA_RETURN.
    "The selected field needs to be passed to the screen field
*      z_equi_equnr = wa_return-fieldval.
    READ TABLE IT_NOTAS WITH KEY QMNUM = WA_RETURN-FIELDVAL.
    IF SY-SUBRC IS INITIAL.
      READ TABLE IT_MPOS WITH KEY QMNUM = IT_NOTAS-QMNUM
          BINARY SEARCH.
      IF NOT SY-SUBRC IS INITIAL OR
        IT_MPOS-BAUTL IS INITIAL.
        MESSAGE E000(ZPPM001) DISPLAY LIKE 'E'
          WITH 'Código de Atividade não encontrado'.
      ENDIF.
*
      MHIO-QMNUM  = IT_NOTAS-QMNUM.
      V_EQUNR  = IT_NOTAS-EQUNR.
    ENDIF.
  ENDLOOP.
*
  IF NOT V_EQUNR IS INITIAL.
    SELECT EQART INTO V_TIPOBJ
      FROM EQUI UP TO 1 ROWS
      WHERE EQUNR = V_EQUNR.
    ENDSELECT.
*
    SELECT EARTX INTO V_DESCRSAP
      FROM T370K_T UP TO 1 ROWS
     WHERE EQART EQ V_TIPOBJ
       AND SPRAS EQ SY-LANGU.
    ENDSELECT.
*
  ENDIF.

  CLEAR MHIO-WARPL.
  LEAVE TO SCREEN 100.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  LIMPA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LIMPA INPUT.
  IF SY-UCOMM EQ 'BTN_CANCELAR'.
    PERFORM ZF_LIMPAR.
  ENDIF.
  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'MHIO-WARPL'.
        SCREEN-INPUT = 1.
      WHEN 'MHIO-QMNUM'.
        SCREEN-INPUT = 1.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ADD_ITENS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ADD_ITENS INPUT.
  IF IT_REG[] IS INITIAL.
    PERFORM ZF_CRIAR.
  ENDIF.
ENDMODULE.
