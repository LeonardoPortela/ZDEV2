*&---------------------------------------------------------------------*
*&  Include           ZPMR0020_0100
*&---------------------------------------------------------------------*

TYPES: BEGIN OF TY_SAIDA,
         AUFNR   TYPE VIQMEL-AUFNR,
         EQUNR   TYPE VIQMEL-EQUNR,
         EQKTX   TYPE EQKT-EQKTX,
         KTEXT   TYPE VIQMEL-QMTXT,
         ZTP_MS  TYPE ZPMT003-ZTP_MS,
         DESCR   TYPE CHAR30,
         TOTCC   TYPE COSP-WTG001,
         ZPERC_F TYPE ZPMT003-ZPERC_C,
         VLR_F   TYPE COSP-WTG001,
         ZPERC_C TYPE ZPMT003-ZPERC_C,
         VLR_C   TYPE COSP-WTG001.
TYPES: END OF TY_SAIDA.

DATA: IT_SAIDA TYPE STANDARD TABLE OF TY_SAIDA,
      WA_SAIDA TYPE TY_SAIDA.

DATA: VL_QMNUM    TYPE VIQMEL-QMNUM,
      CHECK_QMNUM TYPE VIQMEL-QMNUM.

DATA: T_SERVICO  TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
      T_MATERIAL TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
      R_CONTA    TYPE RANGE OF COEP-KSTAR WITH HEADER LINE,
      R_SERVICO  TYPE RANGE OF COEP-KSTAR WITH HEADER LINE,
      R_MATERIAL TYPE RANGE OF COEP-KSTAR WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  IF VL_QMNUM EQ CHECK_QMNUM AND SY-UCOMM NE 'SAVE'.
    CLEAR SY-UCOMM.
  ELSE.
    CHECK_QMNUM = VL_QMNUM.
  ENDIF.

  CASE SY-UCOMM.
    WHEN 'ENTER'.
      PERFORM BUSCA_NOTA.
    WHEN 'SAVE'.
      PERFORM SALVAR_NOTA.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUSCA_NOTA .

  DATA: IT_ZPMT003  TYPE STANDARD TABLE OF ZPMT003,
        IT_VIQMEL   TYPE STANDARD TABLE OF VIQMEL,
        IT_EQKT     TYPE STANDARD TABLE OF EQKT,
        IT_AUFK     TYPE STANDARD TABLE OF AUFK,
        IT_AFKO     TYPE STANDARD TABLE OF AFKO,
        IT_AFVC     TYPE STANDARD TABLE OF AFVC,
        IT_COSP     TYPE STANDARD TABLE OF COSP,
        IT_COSP_AUX TYPE STANDARD TABLE OF COSP,
        IT_COEP     TYPE STANDARD TABLE OF COEP,
        IT_COEP_AUX TYPE STANDARD TABLE OF COEP,
        WA_ZPMT003  TYPE ZPMT003,
        WA_VIQMEL   TYPE VIQMEL,
        WA_EQKT     TYPE EQKT,
        WA_AUFK     TYPE AUFK,
        WA_COEP     TYPE COEP.

  DATA: VL_CONT TYPE I,
        VL_TCC  TYPE COSP-WTG001,           "/Varíavel local para guardar total
        VL_TPF  TYPE COSP-WTG001,           "/Varíavel local para guardar total pago pelo forn.
        VL_TPC  TYPE COSP-WTG001.           "/Varíavel local para guardar total pago pelo cliente

  PERFORM GET_SETS.

  SELECT  *
     FROM ZPMT003
     INTO TABLE IT_ZPMT003
     WHERE QMNUM EQ VL_QMNUM.

  IF NOT LINE_EXISTS( IT_ZPMT003[ QMNUM = VL_QMNUM ZTP_MS = 'M' ] ).
    APPEND VALUE #( QMNUM = VL_QMNUM ZTP_MS = 'M' ZPERC_C = 0 ) TO IT_ZPMT003.
  ENDIF.

  IF NOT LINE_EXISTS( IT_ZPMT003[ QMNUM = VL_QMNUM ZTP_MS = 'S' ] ).
    APPEND VALUE #( QMNUM = VL_QMNUM ZTP_MS = 'S' ZPERC_C = 0 ) TO IT_ZPMT003.
  ENDIF.

  SELECT *
      FROM VIQMEL
      INTO TABLE IT_VIQMEL
      FOR ALL ENTRIES IN IT_ZPMT003
      WHERE QMNUM EQ IT_ZPMT003-QMNUM
        AND QMCOD EQ '0090'.

  IF IT_VIQMEL IS NOT INITIAL.

    SELECT *
        FROM EQKT
        INTO TABLE IT_EQKT
        FOR ALL ENTRIES IN IT_VIQMEL
        WHERE EQUNR EQ IT_VIQMEL-EQUNR
          AND SPRAS EQ SY-LANGU.

    SELECT *
      FROM AUFK
      INTO TABLE IT_AUFK
      FOR ALL ENTRIES IN IT_VIQMEL
      WHERE AUFNR EQ IT_VIQMEL-AUFNR.

    IF IT_AUFK IS NOT INITIAL.

      SELECT *
      FROM CAUFV AS A
      INNER JOIN AFVC AS B ON A~AUFPL EQ B~AUFPL
      INNER JOIN COEP AS C ON C~OBJNR EQ B~OBJNR
      APPENDING CORRESPONDING FIELDS OF TABLE IT_AUFK
      FOR ALL ENTRIES IN IT_VIQMEL
      WHERE A~AUFNR EQ IT_VIQMEL-AUFNR
      AND C~KSTAR IN R_CONTA
      AND C~VRGNG EQ 'COIN'.

      SORT IT_AUFK BY AUFNR.

      SELECT *
       FROM COEP
       INTO TABLE IT_COEP
       FOR ALL ENTRIES IN IT_AUFK
       WHERE OBJNR EQ IT_AUFK-OBJNR
       AND KSTAR IN R_CONTA
       AND VRGNG EQ 'COIN'.

    ELSE.
      MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE TO SCREEN 0100.
    ENDIF.

  ELSE.
    MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0100.
  ENDIF.

  CLEAR: IT_SAIDA, WA_SAIDA.

  LOOP AT IT_ZPMT003 INTO WA_ZPMT003.

    READ TABLE IT_VIQMEL INTO WA_VIQMEL WITH KEY QMNUM = WA_ZPMT003-QMNUM.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-AUFNR = WA_VIQMEL-AUFNR.                                 "/Ordem
      WA_SAIDA-EQUNR = WA_VIQMEL-EQUNR.
    ENDIF.

    READ TABLE IT_EQKT INTO WA_EQKT WITH KEY EQUNR = WA_SAIDA-EQUNR.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-EQKTX = WA_EQKT-EQKTX.                                   "/Desc. Equipamento
    ENDIF.

    READ TABLE IT_AUFK INTO WA_AUFK WITH KEY AUFNR = WA_SAIDA-AUFNR.
    IF SY-SUBRC IS INITIAL.
      WA_SAIDA-KTEXT = WA_VIQMEL-QMTXT.                                   "/Título da Ordem
    ENDIF.

    WA_SAIDA-ZTP_MS = WA_ZPMT003-ZTP_MS.                                  "/Classe de Custo

    CASE WA_SAIDA-ZTP_MS.
      WHEN 'M'.
        MOVE TEXT-003 TO WA_SAIDA-DESCR.                                "/Material/Serviço
        IT_COEP_AUX = IT_COEP.
        "Deleta o que é serviço
        DELETE IT_COEP_AUX WHERE KSTAR IN R_SERVICO.

      WHEN 'S'.
        MOVE TEXT-004 TO WA_SAIDA-DESCR.                                "/Material/Serviço
        IT_COEP_AUX = IT_COEP.
        "Deleta o que é material
        DELETE IT_COEP_AUX WHERE KSTAR IN R_MATERIAL.

    ENDCASE.

    LOOP AT IT_COEP_AUX INTO WA_COEP.
      ADD WA_COEP-WTGBTR TO WA_SAIDA-TOTCC.
    ENDLOOP.

    WA_SAIDA-ZPERC_C = WA_ZPMT003-ZPERC_C.                                          "/% do Cliente

    IF WA_SAIDA-ZPERC_C IS NOT INITIAL.
      WA_SAIDA-ZPERC_F = 100 - WA_SAIDA-ZPERC_C.                                    "/% do Forncedor
      WA_SAIDA-VLR_F = WA_SAIDA-TOTCC * WA_SAIDA-ZPERC_F / WA_SAIDA-ZPERC_C .       "/Vlr. do Forncedor
    ENDIF.

    WA_SAIDA-VLR_C = WA_SAIDA-VLR_F + WA_SAIDA-TOTCC.                               "/Vlr. do Cliente

    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR: WA_SAIDA.

  ENDLOOP.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    ADD WA_SAIDA-TOTCC TO VL_TCC.
    ADD WA_SAIDA-VLR_F TO VL_TPF.
    ADD WA_SAIDA-VLR_C TO VL_TPC.
  ENDLOOP.

  CLEAR: WA_SAIDA.
  WA_SAIDA-DESCR = 'Total'.
  WA_SAIDA-TOTCC = VL_TCC.
  WA_SAIDA-VLR_F = VL_TPF.
  WA_SAIDA-VLR_C = VL_TPC.
  APPEND WA_SAIDA TO IT_SAIDA.

  CLEAR: WA_SAIDA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVAR_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALVAR_NOTA .

  DATA: WA_ZPMT003_SALVAR TYPE ZPMT003.

  LOOP AT IT_SAIDA INTO WA_SAIDA.
    IF WA_SAIDA-DESCR NE 'Total'.

      IF ( WA_SAIDA-TOTCC   NE 0 AND WA_SAIDA-ZPERC_F + WA_SAIDA-ZPERC_C NE 100 ) OR
           WA_SAIDA-ZPERC_F LT 0 OR
           WA_SAIDA-ZPERC_C LT 0.

        MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0100.

      ENDIF.

      IF  WA_SAIDA-TOTCC EQ 0 AND WA_SAIDA-ZPERC_C NE 0.

        MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0100.

      ENDIF.

      WA_ZPMT003_SALVAR-QMNUM = VL_QMNUM.
      WA_ZPMT003_SALVAR-ZTP_MS = WA_SAIDA-ZTP_MS.
      WA_ZPMT003_SALVAR-ZPERC_C = WA_SAIDA-ZPERC_C.
      MODIFY ZPMT003 FROM WA_ZPMT003_SALVAR.
      CLEAR: WA_ZPMT003_SALVAR.
    ENDIF.
  ENDLOOP.

  MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'S'.

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
