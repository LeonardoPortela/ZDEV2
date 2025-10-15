*&---------------------------------------------------------------------*
*& Report  ZPMR0021
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZPMR0021.

*/===========================================================================\*
*| Tabelas                                                                   |*
*/===========================================================================\*
TABLES: VIQMEL, V_EQUI.
*/===========================================================================\*
*| Types                                                                     |*
*/===========================================================================\*
TYPES: BEGIN OF TY_SAIDA,
         EQUNR   TYPE VIQMEL-EQUNR,          "Equipamento
         IWERK   TYPE VIQMEL-IWERK,          "Centro
         AUFNR   TYPE VIQMEL-AUFNR,          "Ordem
         KTEXT   TYPE VIQMEL-QMTXT,          "Serviço
         DESCR   TYPE CHAR30,                "Custos
         TOTCC   TYPE P DECIMALS 2,          "Valor Total
         ZPERC_F TYPE P DECIMALS 2,          "% Forncedor
         ZPERC_C TYPE P DECIMALS 2,          "% Cliente
         VLR_F   TYPE P DECIMALS 2,          "Valor Forncedor
         VLR_C   TYPE P DECIMALS 2.          "Valor Cliente
TYPES: END OF TY_SAIDA,

BEGIN OF TY_COEP,
  OBJNR  TYPE COEP-OBJNR,
  KSTAR  TYPE COEP-KSTAR,
  WTGBTR TYPE COEP-WTGBTR,
END OF TY_COEP.

*/===========================================================================\*
*| Data                                                                      |*
*/===========================================================================\*
DATA: IT_SAIDA    TYPE STANDARD TABLE OF TY_SAIDA,
      IT_VIQMEL   TYPE STANDARD TABLE OF VIQMEL,
      IT_V_EQUI   TYPE STANDARD TABLE OF V_EQUI,
      IT_ZPMT003  TYPE STANDARD TABLE OF ZPMT003 WITH HEADER LINE,
      IT_AUFK     TYPE STANDARD TABLE OF AUFK,
      IT_AUFK_AUX TYPE STANDARD TABLE OF AUFK WITH HEADER LINE,
      IT_COEP     TYPE STANDARD TABLE OF TY_COEP,
      IT_COEP_AUX TYPE STANDARD TABLE OF TY_COEP,
      WA_SAIDA    TYPE TY_SAIDA,
      WA_VIQMEL   TYPE VIQMEL,
      WA_ZPMT003  TYPE ZPMT003,
      WA_AUFK     TYPE AUFK,
      WA_COEP     TYPE TY_COEP.

DATA: T_SERVICO  TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
      T_MATERIAL TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
      R_CONTA    TYPE RANGE OF COSP-KSTAR WITH HEADER LINE,
      R_SERVICO  TYPE RANGE OF COSP-KSTAR WITH HEADER LINE,
      R_MATERIAL TYPE RANGE OF COSP-KSTAR WITH HEADER LINE,
      R_QMNUM    TYPE RANGE OF VIQMEL-QMNUM WITH HEADER LINE.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENTOS DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ON_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID.

ENDCLASS.                    "LCL_EVENT DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_evento IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENTOS IMPLEMENTATION.
  METHOD ON_HOTSPOT_CLICK.

    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX E_ROW_ID-INDEX.
    SET PARAMETER ID 'ANR' FIELD WA_SAIDA-AUFNR.
    CLEAR: WA_SAIDA.
    CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.

  ENDMETHOD.                    "ON_HOTSPOT_CLICK
ENDCLASS.                    "lcl_evento IMPLEMENTATION

*/===========================================================================\*
*| Tela de Seleção                                                           |*
*/===========================================================================\*
SELECTION-SCREEN BEGIN OF BLOCK FILTROS WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_QMNUM FOR VIQMEL-QMNUM,                 "/Nota
                P_AUFNR FOR VIQMEL-AUFNR,                 "/Ordem
                P_EQUNR FOR VIQMEL-EQUNR,                 "/Equipamento
                P_EQART FOR V_EQUI-EQART,                 "/Tipo de Objeto
                P_BAUTL FOR VIQMEL-BAUTL,                 "/Conjunto
                P_IWERK FOR VIQMEL-IWERK,                 "/Centro
                P_QMDAT FOR VIQMEL-QMDAT OBLIGATORY.      "/Data
SELECTION-SCREEN END OF BLOCK FILTROS.

*/===========================================================================\*
*| Start of Selection                                                        |*
*/===========================================================================\*
START-OF-SELECTION.

  IF P_QMNUM IS INITIAL AND
     P_AUFNR IS INITIAL AND
     P_EQUNR IS INITIAL AND
     P_EQART IS INITIAL AND
     P_BAUTL IS INITIAL AND
     P_IWERK IS INITIAL.

    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM SELECIONA_DADOS.
  PERFORM MANIPULA_DADOS.

  CALL SCREEN 0100.

  INCLUDE ZPMR0021_0100.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  PERFORM GET_SETS.

  "/Apenas P_EQART preenchido
  IF P_EQART IS NOT INITIAL AND
     P_QMNUM IS INITIAL AND
     P_AUFNR IS INITIAL AND
     P_EQUNR IS INITIAL AND
     P_BAUTL IS INITIAL AND
     P_IWERK IS INITIAL.

    SELECT *
      FROM V_EQUI
      INTO TABLE IT_V_EQUI
      WHERE EQART IN P_EQART.

    IF IT_V_EQUI IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL
        FROM VIQMEL
        FOR ALL ENTRIES IN IT_V_EQUI
        WHERE EQUNR EQ IT_V_EQUI-EQUNR.
    ENDIF.

  ELSE.
    "/Se algum outro parâmetro preenchido
    SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL
        FROM VIQMEL
        INNER JOIN AFKO ON AFKO~AUFNR EQ VIQMEL~AUFNR "/Modificação CS2017001263
        WHERE VIQMEL~QMNUM IN P_QMNUM
          AND VIQMEL~AUFNR IN P_AUFNR
          AND VIQMEL~EQUNR IN P_EQUNR
          AND VIQMEL~BAUTL IN P_BAUTL
          AND VIQMEL~IWERK IN P_IWERK
          AND AFKO~GSTRP   IN P_QMDAT.                "/Modificação CS2017001263

    IF P_EQART IS NOT INITIAL.
      SELECT *
        FROM V_EQUI
        INTO TABLE IT_V_EQUI
        FOR ALL ENTRIES IN IT_VIQMEL
        WHERE EQUNR EQ IT_VIQMEL-EQUNR
          AND EQART IN P_EQART.
    ENDIF.
  ENDIF.

  SELECT *
    FROM ZPMT003
    INTO TABLE IT_ZPMT003
    FOR ALL ENTRIES IN IT_VIQMEL
    WHERE QMNUM EQ IT_VIQMEL-QMNUM.

  LOOP AT IT_ZPMT003. APPEND VALUE #( OPTION = 'EQ' SIGN = 'I' LOW = IT_ZPMT003-QMNUM ) TO R_QMNUM. ENDLOOP.

  DELETE IT_VIQMEL WHERE QMNUM NOT IN R_QMNUM.

  FREE IT_COEP.

  IF IT_VIQMEL IS NOT INITIAL.

    SELECT *
        FROM AUFK
        INTO TABLE IT_AUFK_AUX
        FOR ALL ENTRIES IN IT_VIQMEL
        WHERE AUFNR EQ IT_VIQMEL-AUFNR.


    SELECT *
    FROM AFKO AS O
    INNER JOIN AFVC AS B ON B~AUFPL EQ O~AUFPL
    INNER JOIN COEP AS C ON C~OBJNR EQ B~OBJNR
    INTO CORRESPONDING FIELDS OF TABLE IT_AUFK
          FOR ALL ENTRIES IN IT_VIQMEL
  WHERE O~AUFNR EQ IT_VIQMEL-AUFNR
    AND C~KSTAR IN R_CONTA
    AND C~VRGNG EQ 'COIN'.


    LOOP AT IT_AUFK_AUX ASSIGNING FIELD-SYMBOL(<AUFK>).
      IF LINE_EXISTS( IT_AUFK[ OBJNR = <AUFK>-OBJNR ] ).
        CLEAR <AUFK>.
      ELSE.
        APPEND <AUFK> TO IT_AUFK.
      ENDIF.
    ENDLOOP.

    SORT IT_AUFK BY AUFNR.

    IF IT_AUFK IS NOT INITIAL.

      SELECT OBJNR KSTAR WTGBTR
          FROM COEP
          APPENDING TABLE IT_COEP
          FOR ALL ENTRIES IN IT_AUFK
          WHERE OBJNR EQ IT_AUFK-OBJNR
            AND KSTAR IN R_CONTA
            AND VRGNG EQ 'COIN'.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MANIPULA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MANIPULA_DADOS .

  LOOP AT IT_VIQMEL INTO WA_VIQMEL.
    LOOP AT IT_ZPMT003 INTO WA_ZPMT003 WHERE QMNUM EQ WA_VIQMEL-QMNUM.

      WA_SAIDA-EQUNR = |{ WA_VIQMEL-EQUNR ALPHA = OUT }|.

      READ TABLE IT_V_EQUI WITH KEY EQUNR = WA_VIQMEL-EQUNR TRANSPORTING NO FIELDS.
      IF SY-SUBRC IS NOT INITIAL AND P_EQART IS NOT INITIAL.
        EXIT.
      ENDIF.

      WA_SAIDA-IWERK = WA_VIQMEL-IWERK.
      WA_SAIDA-AUFNR = |{ WA_VIQMEL-AUFNR ALPHA = OUT }|.

      SORT IT_AUFK BY AUFNR OBJNR.
      DELETE ADJACENT DUPLICATES FROM IT_AUFK COMPARING AUFNR OBJNR.

      LOOP AT IT_AUFK INTO WA_AUFK WHERE AUFNR EQ WA_VIQMEL-AUFNR.

*      READ TABLE IT_AUFK INTO WA_AUFK WITH KEY AUFNR = WA_VIQMEL-AUFNR.
*      IF SY-SUBRC IS INITIAL.
**        WA_SAIDA-KTEXT = WA_AUFK-KTEXT.
*      ENDIF.

        WA_SAIDA-KTEXT = WA_VIQMEL-QMTXT. "/Modificação CS2017001263
        CASE WA_ZPMT003-ZTP_MS.
          WHEN 'M'.
            WA_SAIDA-DESCR = TEXT-003.
            IT_COEP_AUX = IT_COEP.
            DELETE IT_COEP_AUX WHERE OBJNR NE WA_AUFK-OBJNR OR KSTAR IN R_SERVICO[].
          WHEN 'S'.
            WA_SAIDA-DESCR = TEXT-004.
            IT_COEP_AUX = IT_COEP.
            DELETE IT_COEP_AUX WHERE OBJNR NE WA_AUFK-OBJNR OR KSTAR IN R_MATERIAL[].
        ENDCASE.

        LOOP AT IT_COEP_AUX INTO WA_COEP.
          ADD WA_COEP-WTGBTR TO WA_SAIDA-TOTCC.
        ENDLOOP.

      ENDLOOP.

      WA_SAIDA-ZPERC_C = WA_ZPMT003-ZPERC_C.

      IF WA_SAIDA-ZPERC_C IS INITIAL.
        WA_SAIDA-ZPERC_F = 0.
      ELSE.
        WA_SAIDA-ZPERC_F = 100 - WA_SAIDA-ZPERC_C.
      ENDIF.

      TRY .
          WA_SAIDA-VLR_F = ( WA_SAIDA-TOTCC * WA_SAIDA-ZPERC_F ) / WA_SAIDA-ZPERC_C.
        CATCH CX_SY_ZERODIVIDE.
      ENDTRY.

      WA_SAIDA-VLR_C = WA_SAIDA-TOTCC + WA_SAIDA-VLR_F.

      APPEND WA_SAIDA TO IT_SAIDA.
      CLEAR: WA_AUFK, WA_SAIDA.
    ENDLOOP.
  ENDLOOP.

  IF IT_SAIDA IS INITIAL.
    MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_SETS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SETS .

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'Z_SERVICO'
    TABLES
      SET_VALUES    = T_SERVICO
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.

  FREE R_CONTA.

  LOOP AT T_SERVICO.  APPEND VALUE #( OPTION = 'EQ' SIGN = 'I' LOW = T_SERVICO-FROM ) TO R_SERVICO. ENDLOOP.
  APPEND LINES OF R_SERVICO TO R_CONTA.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'Z_MATERIAL'
    TABLES
      SET_VALUES    = T_MATERIAL
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.

  LOOP AT T_MATERIAL. APPEND VALUE #( OPTION = 'EQ' SIGN = 'I' LOW = T_MATERIAL-FROM ) TO R_MATERIAL. ENDLOOP.
  APPEND LINES OF R_MATERIAL TO R_CONTA.

ENDFORM.
