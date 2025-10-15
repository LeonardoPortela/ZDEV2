*&---------------------------------------------------------------------*
*& Report  ZPMR0018
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZPMR0018.
*&---------------------------------------------------------------------*
*&      TABELAS
*&---------------------------------------------------------------------*
TABLES: VIQMEL, IFLO, ITOB.
*&---------------------------------------------------------------------*
*&      TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF TY_IFLO_LOC_CAT,
         TPLNR TYPE IFLO-TPLNR,
         IWERK TYPE IFLO-IWERK,
         FLTYP TYPE IFLO-FLTYP,
         BUKRS TYPE IFLO-BUKRS.
TYPES:END OF TY_IFLO_LOC_CAT.

TYPES: BEGIN OF TY_V_EQUI_EQUIP_OBJ,
         EQUNR TYPE V_EQUI-EQUNR.
TYPES:END OF TY_V_EQUI_EQUIP_OBJ.

TYPES:  BEGIN OF TY_JEST ,         " Status da Ordem
          OBJNR TYPE JEST-OBJNR,   " Objeto
          STAT  TYPE JEST-STAT,    " Nº do objeto
          TXT04 TYPE TJ02T-TXT04,  " Descrição dos status
        END OF TY_JEST.

TYPES: BEGIN OF TY_TJ02T,
         ISTAT TYPE TJ02T-ISTAT,
         TXT04 TYPE TJ02T-TXT04,
       END OF TY_TJ02T.


TYPES: BEGIN OF TY_SAIDA.
TYPES:    BUKRS TYPE VIQMEL-BUKRS,    "Empresa
          SWERK TYPE VIQMEL-SWERK,    "Centro
          TPLNR TYPE IFLO-TPLNR,      "Número do Local
          PLTXT TYPE IFLO-PLTXT,      "Descrição do Local
          TPLMA TYPE IFLO-TPLMA,      "Número do Local Superior
          PLTX2 TYPE IFLO-PLTXT,      "Descrição do Local Superior
          EQUNR TYPE V_EQUI-EQUNR,    "Nº Equipamento
          EQKTX TYPE V_EQUI-EQKTX,    "Descrição Equipamento
          EQART TYPE V_EQUI-EQART,    "Tipo de Objeto
          EARTX TYPE T370K_T-EARTX,   "Descrição Tipo de Objeto
          OPERA TYPE I,               "Quantidade de horas de operação
          PARAD TYPE I,               "Qtde de horas paradas
          DISPO TYPE I,               "Qtde de horas disponíveis
          %DISP TYPE P DECIMALS 2.    "% de disponibilidade
TYPES:END OF TY_SAIDA.
*&---------------------------------------------------------------------*
*&      TYPES
*&---------------------------------------------------------------------*
DATA: IT_IFLO_LOC_CAT     TYPE STANDARD TABLE OF TY_IFLO_LOC_CAT,
      IT_V_EQUI_EQUIP_OBJ TYPE STANDARD TABLE OF TY_V_EQUI_EQUIP_OBJ,
      IT_VIQMEL           TYPE STANDARD TABLE OF VIQMEL WITH HEADER LINE,
      IT_NOTAS            TYPE TABLE OF BAPI2080_1,
      IT_JEST             TYPE TABLE OF TY_JEST WITH HEADER LINE,
      IT_TJ02T            TYPE TABLE OF TY_TJ02T,
      IT_SAIDA            TYPE STANDARD TABLE OF TY_SAIDA.

DATA: WA_IFLO_LOC_CAT     TYPE TY_IFLO_LOC_CAT,
      WA_V_EQUI_EQUIP_OBJ TYPE TY_V_EQUI_EQUIP_OBJ,
      WA_VIQMEL           TYPE VIQMEL,
      WA_NOTAS            TYPE BAPI2080_1,
      WA_SAIDA            TYPE TY_SAIDA.

DATA:
     V_TABIX     TYPE SY-TABIX. " guardar o indice
*&---------------------------------------------------------------------*
*&      TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUKRS FOR VIQMEL-BUKRS OBLIGATORY,                  "Empresa
                P_QMART FOR VIQMEL-QMART,                             "Tipo de Nota
                P_SWERK FOR VIQMEL-SWERK,                             "Centro
                P_DATES FOR VIQMEL-AEDAT NO-EXTENSION OBLIGATORY,     "Período
                P_TPLNR FOR VIQMEL-TPLNR,                             "Local de Instalação
                P_FLTYP FOR IFLO-FLTYP OBLIGATORY DEFAULT 'E'.        "Categoria
*                P_EQUNR FOR VIQMEL-EQUNR,                             "Equipamento
*                P_EQART FOR ITOB-EQART.                               "Tipo de Objeto
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_ANALI RADIOBUTTON GROUP G1 DEFAULT 'X',
            P_SINTE RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B2.
*&---------------------------------------------------------------------*
*&      SELEÇÂO
*&---------------------------------------------------------------------*
START-OF-SELECTION.

*  IF P_TPLNR IS NOT INITIAL AND ( P_FLTYP IS NOT INITIAL ) OR
*     P_FLTYP IS NOT INITIAL AND ( P_TPLNR IS NOT INITIAL ). "OR
**     P_EQUNR IS NOT INITIAL AND ( P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL OR P_EQART IS NOT INITIAL ) OR
**     P_EQART IS NOT INITIAL AND ( P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL ) OR
**     P_EQART IS INITIAL AND P_TPLNR IS INITIAL AND P_FLTYP IS INITIAL." AND P_EQUNR IS INITIAL.
*    MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.


  IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.
    PERFORM SELECIONA_DADOS_LOC_CAT.                "Grupo de Filtros de Localização/Categoria(Dominante sobre Equip/Obj)
  ELSE.
*    PERFORM SELECIONA_DADOS_EQUIP_OBJ.              "Grupo de Filtros de Equipamento/Objeto
  ENDIF.

  PERFORM SELECIONA_NOTAS.

  IF P_ANALI IS NOT INITIAL.
    PERFORM ORGANIZA_DADOS_ANALITICO.
  ELSE.
    PERFORM ORGANIZA_DADOS_SINTETICO.
  ENDIF.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_LOC_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS_LOC_CAT .

  IF P_FLTYP IS NOT INITIAL.

    SELECT IFLO~TPLNR IFLO~IWERK IFLO~BUKRS IFLO~FLTYP
      INTO CORRESPONDING FIELDS OF TABLE IT_IFLO_LOC_CAT
      FROM IFLO
     INNER JOIN J_1BBRANCH ON ( J_1BBRANCH~BRANCH EQ IFLO~SWERK )
     WHERE IFLO~SWERK IN P_SWERK
       AND IFLO~FLTYP IN P_FLTYP
       AND IFLO~TPLNR IN P_TPLNR
    AND J_1BBRANCH~BUKRS IN P_BUKRS.

  ELSE.

    SELECT IFLO~TPLNR IFLO~IWERK IFLO~BUKRS IFLO~FLTYP
        INTO CORRESPONDING FIELDS OF TABLE IT_IFLO_LOC_CAT
        FROM IFLO
       INNER JOIN J_1BBRANCH ON ( J_1BBRANCH~BRANCH EQ IFLO~SWERK )
       WHERE IFLO~SWERK IN P_SWERK
         AND IFLO~TPLNR IN P_TPLNR
    AND J_1BBRANCH~BUKRS IN P_BUKRS.

  ENDIF.

  SORT IT_IFLO_LOC_CAT BY TPLNR.
  DELETE ADJACENT DUPLICATES FROM IT_IFLO_LOC_CAT COMPARING TPLNR.

  IF IT_IFLO_LOC_CAT IS INITIAL.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_EQUIP_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS_EQUIP_OBJ .

*  IF P_EQART IS NOT INITIAL.
*
*    SELECT V_EQUI~EQUNR
*      INTO CORRESPONDING FIELDS OF TABLE IT_V_EQUI_EQUIP_OBJ
*      FROM V_EQUI
*     INNER JOIN J_1BBRANCH ON ( J_1BBRANCH~BRANCH EQ V_EQUI~SWERK )
*     WHERE V_EQUI~SWERK IN P_SWERK
*       AND V_EQUI~EQART IN P_EQART
*    AND J_1BBRANCH~BUKRS IN P_BUKRS.
*
*  ELSE.

  SELECT V_EQUI~EQUNR
      INTO CORRESPONDING FIELDS OF TABLE IT_V_EQUI_EQUIP_OBJ
      FROM V_EQUI
     INNER JOIN J_1BBRANCH ON ( J_1BBRANCH~BRANCH EQ V_EQUI~SWERK )
     WHERE V_EQUI~SWERK IN P_SWERK
*         AND V_EQUI~EQUNR IN P_EQUNR
  AND J_1BBRANCH~BUKRS IN P_BUKRS.

*  ENDIF.

  SORT IT_V_EQUI_EQUIP_OBJ BY EQUNR.
  DELETE ADJACENT DUPLICATES FROM IT_V_EQUI_EQUIP_OBJ COMPARING EQUNR.

  IF IT_V_EQUI_EQUIP_OBJ IS INITIAL.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_NOTAS .



  DATA: FUNCLOC_INT   TYPE BAPI_ITOB_PARMS-FUNCLOC_INT,
        EQUIPMENT     TYPE BAPI_ITOB_PARMS-EQUIPMENT,
        WA_BAPIRETURN TYPE BAPIRETURN,
        NOTIFICATION  TYPE TABLE OF BAPI2080_1.

  IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.

    SELECT *
      FROM VIQMEL
      INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL
      FOR ALL ENTRIES IN IT_IFLO_LOC_CAT
      WHERE TPLNR EQ IT_IFLO_LOC_CAT-TPLNR
        AND SWERK IN P_SWERK
        AND BUKRS IN P_BUKRS
        AND QMART IN P_QMART
    AND AUSZT NE SPACE.

  ELSE.

    SELECT *
      FROM VIQMEL
      INTO CORRESPONDING FIELDS OF TABLE IT_VIQMEL
      FOR ALL ENTRIES IN IT_V_EQUI_EQUIP_OBJ
      WHERE EQUNR EQ IT_V_EQUI_EQUIP_OBJ-EQUNR
        AND SWERK IN P_SWERK
        AND BUKRS IN P_BUKRS
        AND QMART IN P_QMART
    AND AUSZT NE SPACE.

  ENDIF.

  "Deleta os registros que não contém o período selecionado
  DELETE IT_VIQMEL WHERE ( AUSVN LT P_DATES-LOW AND AUSBS LT P_DATES-LOW )
    OR ( AUSVN GT P_DATES-HIGH AND AUSBS GT P_DATES-HIGH ).

*  IF IT_VIQMEL[] IS INITIAL.
*    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.

  CHECK IT_VIQMEL[] IS NOT INITIAL.
  "Ajusta extremos fora da range do parâmetro p_dates
  LOOP AT IT_VIQMEL INTO WA_VIQMEL.
    IF WA_VIQMEL-AUSVN LT P_DATES-LOW.
      MOVE P_DATES-LOW TO WA_VIQMEL-AUSVN.
      MOVE '000000' TO WA_VIQMEL-AUZTV.
    ENDIF.
    IF WA_VIQMEL-AUSBS GT P_DATES-HIGH.
      MOVE P_DATES-HIGH TO WA_VIQMEL-AUSBS.
      MOVE '235959' TO WA_VIQMEL-AUZTB.
    ENDIF.
    MODIFY IT_VIQMEL FROM WA_VIQMEL INDEX SY-TABIX.
  ENDLOOP.
*
* Busca os status
  SELECT JEST~OBJNR JEST~STAT TJ02T~TXT04
    INTO TABLE IT_JEST
    FROM JEST
      INNER JOIN TJ02T ON TJ02T~ISTAT = JEST~STAT
        FOR ALL ENTRIES IN IT_VIQMEL
          WHERE JEST~OBJNR  EQ IT_VIQMEL-OBJNR
            AND JEST~INACT  EQ ABAP_FALSE   " Somente os ativos
            AND TJ02T~SPRAS EQ SY-LANGU.

  LOOP AT IT_JEST.
    READ TABLE IT_VIQMEL WITH KEY OBJNR = IT_JEST-OBJNR.

    "Deleta as ordens com status eliminado.
    IF IT_JEST-OBJNR = IT_VIQMEL-OBJNR AND IT_JEST-TXT04 = 'MREL'.
      DELETE IT_VIQMEL WHERE OBJNR EQ IT_JEST-OBJNR.
    ENDIF.
  ENDLOOP.

  SORT IT_VIQMEL ASCENDING BY QMNUM.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS_SINTETICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZA_DADOS_SINTETICO.

  DATA: VL_DURATION TYPE SYTABIX,
        VL_TPLMA    TYPE IFLO-TPLMA,
        VL_SHTXT    TYPE ITOB-SHTXT,
        VL_EQART    TYPE V_EQUI-EQART.

  DATA: IT_SAIDA_AUX TYPE STANDARD TABLE OF TY_SAIDA,
        WA_SAIDA_AUX TYPE TY_SAIDA.

  IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.
*    IF IT_VIQMEL[] IS INITIAL.
      LOOP AT IT_IFLO_LOC_CAT INTO WA_IFLO_LOC_CAT .
        WA_SAIDA_AUX-BUKRS = WA_IFLO_LOC_CAT-BUKRS.
        WA_SAIDA_AUX-SWERK = WA_IFLO_LOC_CAT-IWERK.

        SELECT SINGLE TPLMA
          FROM IFLO
          INTO VL_TPLMA
        WHERE TPLNR EQ WA_IFLO_LOC_CAT-TPLNR.

        IF SY-SUBRC IS INITIAL.

          WA_SAIDA_AUX-TPLMA = VL_TPLMA.                                           "Nº do Local Superior

          SELECT SINGLE PLTXT
            FROM IFLO
            INTO WA_SAIDA_AUX-PLTX2                                                "Local Pai
          WHERE TPLNR EQ VL_TPLMA.
        ENDIF.

        LOOP AT IT_VIQMEL INTO WA_VIQMEL WHERE TPLNR = WA_IFLO_LOC_CAT-TPLNR.
          IF WA_IFLO_LOC_CAT-TPLNR = WA_VIQMEL-TPLNR.
            CALL FUNCTION 'SWI_DURATION_DETERMINE'
              EXPORTING
                START_DATE = WA_VIQMEL-AUSVN
                END_DATE   = WA_VIQMEL-AUSBS
                START_TIME = WA_VIQMEL-AUZTV
                END_TIME   = WA_VIQMEL-AUZTB
              IMPORTING
                DURATION   = VL_DURATION.

            VL_DURATION = ( VL_DURATION / 60 ).
            ADD VL_DURATION TO WA_SAIDA_AUX-PARAD .
            CLEAR WA_VIQMEL.
          ENDIF.
        ENDLOOP.

        APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
        CLEAR: WA_SAIDA_AUX.
      ENDLOOP.

      LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
        COLLECT WA_SAIDA_AUX INTO IT_SAIDA.
      ENDLOOP.

      SORT IT_SAIDA BY BUKRS ASCENDING
                       SWERK ASCENDING
                       TPLMA ASCENDING.

      LOOP AT IT_SAIDA INTO WA_SAIDA.
        WA_SAIDA-OPERA = ( ( P_DATES-HIGH - P_DATES-LOW ) + 1 ) * 24 * 60.         "Tempo de Operação em Minutos
        WA_SAIDA-DISPO = WA_SAIDA-OPERA - WA_SAIDA-PARAD.                          "Tempo Disponível em Segundos
        WA_SAIDA-%DISP = ( WA_SAIDA-DISPO / WA_SAIDA-OPERA ) * 100.                "% de Disponibilidade
        MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX.
      ENDLOOP.

*    ELSE.
*
*      LOOP AT IT_VIQMEL INTO WA_VIQMEL.
*
*        WA_SAIDA_AUX-BUKRS = WA_VIQMEL-BUKRS.
*        WA_SAIDA_AUX-SWERK = WA_VIQMEL-SWERK.
*
*        SELECT SINGLE TPLMA
*          FROM IFLO
*          INTO VL_TPLMA
*        WHERE TPLNR EQ WA_VIQMEL-TPLNR.
*
*        IF SY-SUBRC IS INITIAL.
*
*          WA_SAIDA_AUX-TPLMA = VL_TPLMA.                                           "Nº do Local Superior
*
*          SELECT SINGLE PLTXT
*            FROM IFLO
*            INTO WA_SAIDA_AUX-PLTX2                                                "Local Pai
*          WHERE TPLNR EQ VL_TPLMA.
*
*        ENDIF.
*
*        CALL FUNCTION 'SWI_DURATION_DETERMINE'
*          EXPORTING
*            START_DATE = WA_VIQMEL-AUSVN
*            END_DATE   = WA_VIQMEL-AUSBS
*            START_TIME = WA_VIQMEL-AUZTV
*            END_TIME   = WA_VIQMEL-AUZTB
*          IMPORTING
*            DURATION   = VL_DURATION.
*
*        WA_SAIDA_AUX-PARAD = VL_DURATION / 60.                                      "Tempo de Parada em Segundos
*
*        APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
*        CLEAR: WA_SAIDA_AUX.
*      ENDLOOP.
*
*      LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
*        COLLECT WA_SAIDA_AUX INTO IT_SAIDA.
*      ENDLOOP.
*
*      SORT IT_SAIDA BY BUKRS ASCENDING
*                       SWERK ASCENDING
*                       TPLMA ASCENDING.
*
*      LOOP AT IT_SAIDA INTO WA_SAIDA.
*        WA_SAIDA-OPERA = ( ( P_DATES-HIGH - P_DATES-LOW ) + 1 ) * 24 * 60.         "Tempo de Operação em Minutos
*        WA_SAIDA-DISPO = WA_SAIDA-OPERA - WA_SAIDA-PARAD.                          "Tempo Disponível em Segundos
*        WA_SAIDA-%DISP = ( WA_SAIDA-DISPO / WA_SAIDA-OPERA ) * 100.                "% de Disponibilidade
*        MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX.
*      ENDLOOP.
*    ENDIF.
*  ELSE.

*    LOOP AT IT_VIQMEL INTO WA_VIQMEL.
*      WA_SAIDA_AUX-BUKRS = WA_VIQMEL-BUKRS.
*      WA_SAIDA_AUX-SWERK = WA_VIQMEL-SWERK.
*      WA_SAIDA_AUX-TPLNR = WA_VIQMEL-TPLNR.
*
*      SELECT SINGLE PLTXT
*        FROM IFLO
*        INTO WA_SAIDA_AUX-PLTXT
*        WHERE TPLNR EQ WA_VIQMEL-TPLNR                                           "Descrição do Local
*          AND SWERK EQ WA_VIQMEL-SWERK.
*
**      IF P_EQUNR IS NOT INITIAL.
**
**        WA_SAIDA_AUX-EQUNR = WA_VIQMEL-EQUNR.
**        SELECT SINGLE EQKTX
**          FROM V_EQUI
**          INTO WA_SAIDA_AUX-EQKTX                                                  "Descrição do Equipamento
**          WHERE EQUNR EQ WA_VIQMEL-EQUNR
**            AND SWERK EQ WA_VIQMEL-SWERK.
**
**      ELSE.
*
*      SELECT SINGLE EQART
*         FROM V_EQUI
*         INTO VL_EQART
*         WHERE EQUNR EQ WA_VIQMEL-EQUNR
*           AND SWERK EQ WA_VIQMEL-SWERK.
*
*      SELECT SINGLE EARTX
*        FROM T370K_T
*        INTO WA_SAIDA_AUX-EARTX                                                  "Descrição do Tipo de Objeto
*        WHERE EQART EQ VL_EQART
*          AND SPRAS EQ SY-LANGU.
*
**      ENDIF.
*
*      CALL FUNCTION 'SWI_DURATION_DETERMINE'
*        EXPORTING
*          START_DATE = WA_VIQMEL-AUSVN
*          END_DATE   = WA_VIQMEL-AUSBS
*          START_TIME = WA_VIQMEL-AUZTV
*          END_TIME   = WA_VIQMEL-AUZTB
*        IMPORTING
*          DURATION   = VL_DURATION.
*
*      WA_SAIDA_AUX-PARAD = VL_DURATION / 60.                                      "Tempo de Parada em Segundos
*
*      APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
*      CLEAR: WA_SAIDA_AUX, VL_EQART.
*    ENDLOOP.
*
*    LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
*      COLLECT WA_SAIDA_AUX INTO IT_SAIDA.
*    ENDLOOP.
*
*    SORT IT_SAIDA BY BUKRS ASCENDING
*                     SWERK ASCENDING
*                     TPLMA ASCENDING.
*
*    LOOP AT IT_SAIDA INTO WA_SAIDA.
*      WA_SAIDA-OPERA = ( ( P_DATES-HIGH - P_DATES-LOW ) + 1 ) * 24 * 60.         "Tempo de Operação em Minutos
*      WA_SAIDA-DISPO = WA_SAIDA-OPERA - WA_SAIDA-PARAD.                          "Tempo Disponível em Segundos
*      WA_SAIDA-%DISP = ( WA_SAIDA-DISPO / WA_SAIDA-OPERA ) * 100.                "% de Disponibilidade
*      MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX.
*    ENDLOOP.

*    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS_ANALITICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZA_DADOS_ANALITICO.

  DATA: VL_DURATION TYPE SYTABIX,
        VL_TPLMA    TYPE IFLO-TPLMA,
        VL_SHTXT    TYPE ITOB-SHTXT,
        VL_EQART    TYPE V_EQUI-EQART.

  DATA: IT_SAIDA_AUX TYPE STANDARD TABLE OF TY_SAIDA,
        WA_SAIDA_AUX TYPE TY_SAIDA.
  CLEAR VL_DURATION.
  IF P_TPLNR IS NOT INITIAL OR P_FLTYP IS NOT INITIAL.
*    IF IT_VIQMEL[] IS INITIAL.
    LOOP AT IT_IFLO_LOC_CAT INTO WA_IFLO_LOC_CAT .
      WA_SAIDA_AUX-BUKRS = WA_IFLO_LOC_CAT-BUKRS.
      WA_SAIDA_AUX-SWERK = WA_IFLO_LOC_CAT-IWERK.
      WA_SAIDA_AUX-TPLNR = WA_IFLO_LOC_CAT-TPLNR.

      SELECT SINGLE PLTXT
        FROM IFLO
        INTO WA_SAIDA_AUX-PLTXT                                                  "Descrição do Local
        WHERE TPLNR EQ WA_IFLO_LOC_CAT-TPLNR.

      SELECT SINGLE TPLMA
        FROM IFLO
        INTO VL_TPLMA
      WHERE TPLNR EQ WA_IFLO_LOC_CAT-TPLNR.

      IF SY-SUBRC IS INITIAL.

        WA_SAIDA_AUX-TPLMA = VL_TPLMA.                                           "Nº do Local Superior

        SELECT SINGLE PLTXT
          FROM IFLO
          INTO WA_SAIDA_AUX-PLTX2                                                "Descrição do Local Superior
        WHERE TPLNR EQ VL_TPLMA.
      ENDIF.

      LOOP AT IT_VIQMEL INTO WA_VIQMEL WHERE TPLNR = WA_IFLO_LOC_CAT-TPLNR.

        IF WA_IFLO_LOC_CAT-TPLNR = WA_VIQMEL-TPLNR.
          CALL FUNCTION 'SWI_DURATION_DETERMINE'
            EXPORTING
              START_DATE = WA_VIQMEL-AUSVN
              END_DATE   = WA_VIQMEL-AUSBS
              START_TIME = WA_VIQMEL-AUZTV
              END_TIME   = WA_VIQMEL-AUZTB
            IMPORTING
              DURATION   = VL_DURATION.

          VL_DURATION = ( VL_DURATION / 60 ).
          ADD VL_DURATION TO WA_SAIDA_AUX-PARAD .
          CLEAR WA_VIQMEL.
        ENDIF.
      ENDLOOP.

      APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
      CLEAR: WA_SAIDA_AUX.
    ENDLOOP.

    LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
      COLLECT WA_SAIDA_AUX INTO IT_SAIDA.
    ENDLOOP.

    SORT IT_SAIDA BY BUKRS ASCENDING
                     SWERK ASCENDING
                     TPLMA ASCENDING
                     TPLNR ASCENDING.

    LOOP AT IT_SAIDA INTO WA_SAIDA.
      WA_SAIDA-OPERA = ( ( P_DATES-HIGH - P_DATES-LOW ) + 1 ) * 24 * 60.         "Tempo de Operação em Minutos
      WA_SAIDA-DISPO = WA_SAIDA-OPERA - WA_SAIDA-PARAD.                          "Tempo Disponível em Segundos
      WA_SAIDA-%DISP = ( WA_SAIDA-DISPO / WA_SAIDA-OPERA ) * 100.                "% de Disponibilidade
      MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX.
    ENDLOOP.

*    ELSE.
*      LOOP AT IT_VIQMEL INTO WA_VIQMEL.
*
*        WA_SAIDA_AUX-BUKRS = WA_VIQMEL-BUKRS.
*        WA_SAIDA_AUX-SWERK = WA_VIQMEL-SWERK.
*
*        SELECT SINGLE TPLMA
*          FROM IFLO
*          INTO VL_TPLMA
*        WHERE TPLNR EQ WA_VIQMEL-TPLNR.
*
*        IF SY-SUBRC IS INITIAL.
*
*          WA_SAIDA_AUX-TPLMA = VL_TPLMA.                                           "Nº do Local Superior
*
*          SELECT SINGLE PLTXT
*            FROM IFLO
*            INTO WA_SAIDA_AUX-PLTX2                                                "Local Pai
*          WHERE TPLNR EQ VL_TPLMA.
*
*        ENDIF.
*
*        CALL FUNCTION 'SWI_DURATION_DETERMINE'
*          EXPORTING
*            START_DATE = WA_VIQMEL-AUSVN
*            END_DATE   = WA_VIQMEL-AUSBS
*            START_TIME = WA_VIQMEL-AUZTV
*            END_TIME   = WA_VIQMEL-AUZTB
*          IMPORTING
*            DURATION   = VL_DURATION.
*
*        WA_SAIDA_AUX-PARAD = VL_DURATION / 60.                                      "Tempo de Parada em Segundos
*
*        APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
*        CLEAR: WA_SAIDA_AUX.
*      ENDLOOP.
*
*      LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
*        COLLECT WA_SAIDA_AUX INTO IT_SAIDA.
*      ENDLOOP.
*
*      SORT IT_SAIDA BY BUKRS ASCENDING
*                       SWERK ASCENDING
*                       TPLMA ASCENDING.
*
*      LOOP AT IT_SAIDA INTO WA_SAIDA.
*        WA_SAIDA-OPERA = ( ( P_DATES-HIGH - P_DATES-LOW ) + 1 ) * 24 * 60.         "Tempo de Operação em Minutos
*        WA_SAIDA-DISPO = WA_SAIDA-OPERA - WA_SAIDA-PARAD.                          "Tempo Disponível em Segundos
*        WA_SAIDA-%DISP = ( WA_SAIDA-DISPO / WA_SAIDA-OPERA ) * 100.                "% de Disponibilidade
*        MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX.
*      ENDLOOP.
*    ENDIF.
*  ELSE.
*    LOOP AT IT_VIQMEL INTO WA_VIQMEL.
*      WA_SAIDA_AUX-BUKRS = WA_VIQMEL-BUKRS.
*      WA_SAIDA_AUX-SWERK = WA_VIQMEL-SWERK.
*      WA_SAIDA_AUX-TPLNR = WA_VIQMEL-TPLNR.
*      WA_SAIDA_AUX-EQUNR = WA_VIQMEL-EQUNR.
*
*      SELECT SINGLE PLTXT
*        FROM IFLO
*        INTO WA_SAIDA_AUX-PLTXT                                                  "Descrição do Local
*        WHERE TPLNR EQ WA_VIQMEL-TPLNR
*          AND SWERK EQ WA_VIQMEL-SWERK.
*
*      SELECT SINGLE EQKTX
*        FROM V_EQUI
*        INTO WA_SAIDA_AUX-EQKTX                                                  "Descrição do Equipamento
*        WHERE EQUNR EQ WA_VIQMEL-EQUNR
*          AND SWERK EQ WA_VIQMEL-SWERK.
*
*      SELECT SINGLE EQART
*        FROM V_EQUI
*        INTO VL_EQART
*        WHERE EQUNR EQ WA_VIQMEL-EQUNR
*          AND SWERK EQ WA_VIQMEL-SWERK.
*
*      SELECT SINGLE EARTX
*        FROM T370K_T
*        INTO WA_SAIDA_AUX-EARTX                                                  "Descrição do Tipo de Objeto
*        WHERE EQART EQ VL_EQART
*          AND SPRAS EQ SY-LANGU.
*
*      CALL FUNCTION 'SWI_DURATION_DETERMINE'
*        EXPORTING
*          START_DATE = WA_VIQMEL-AUSVN
*          END_DATE   = WA_VIQMEL-AUSBS
*          START_TIME = WA_VIQMEL-AUZTV
*          END_TIME   = WA_VIQMEL-AUZTB
*        IMPORTING
*          DURATION   = VL_DURATION.
*
*      WA_SAIDA_AUX-PARAD = VL_DURATION / 60.                                      "Tempo de Parada em Segundos
*
*      APPEND WA_SAIDA_AUX TO IT_SAIDA_AUX.
*      CLEAR: WA_SAIDA_AUX, VL_EQART.
*    ENDLOOP.
*
*    LOOP AT IT_SAIDA_AUX INTO WA_SAIDA_AUX.
*      COLLECT WA_SAIDA_AUX INTO IT_SAIDA.
*    ENDLOOP.
*
*    SORT IT_SAIDA BY BUKRS ASCENDING
*                     SWERK ASCENDING
*                     TPLMA ASCENDING
*                     TPLNR ASCENDING.
*
*    LOOP AT IT_SAIDA INTO WA_SAIDA.
*      WA_SAIDA-OPERA = ( ( P_DATES-HIGH - P_DATES-LOW ) + 1 ) * 24 * 60.         "Tempo de Operação em Minutos
*      WA_SAIDA-DISPO = WA_SAIDA-OPERA - WA_SAIDA-PARAD.                          "Tempo Disponível em Segundos
*      WA_SAIDA-%DISP = ( WA_SAIDA-DISPO / WA_SAIDA-OPERA ) * 100.                "% de Disponibilidade
*      MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX.
*    ENDLOOP.
**    ENDIF.
  ENDIF.

ENDFORM.

INCLUDE ZPMR0018_0100.
