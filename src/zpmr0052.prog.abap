**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|  Tester:                                                                  |*
**|    + Cleudo Ferreira ( cleudo.ferreira@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|    +                                                                      |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Relatorio de: Indicadores de Manuntenção MTBF, MTTR, Disponibilidade      |*
**/===========================================================================\*

REPORT ZPMR0052.

TABLES: QMIH, QMEL, ITOB.

TYPES: BEGIN OF TY_SAIDA,
         MES_N      TYPE CHAR2,
         MES        TYPE CHAR10,       " Mes
         KTX        TYPE FCKTX,        " Mes Abreviado
         D_PARADA   TYPE DMBTR,        " Duração da parada Total
         N_PARADA   TYPE I,            " Numero de paradas
         T_OPERACAO TYPE DMBTR,        " Tempo em Operação
         MTTR       TYPE DMBTR,     " MTTR
         MTBF       TYPE DMBTR,     " MTBF
         DISPO      TYPE ZPPED013,     " Disponibilidade
         TX_FALHA   TYPE KURSF,        " Taxa de falha
         CONFI      TYPE ZPPED014,     " Confiabilidade
         MMTTR      TYPE ZPPED013,     " Meta MTTR
         MMTBF      TYPE ZPPED014,     " Meta MTBF
         INDIS      TYPE ZPMT0033-INDIS,
         CONF       TYPE ZPMT0033-CONF,
         EQKTX      TYPE EQKT-EQKTX,
         EARTX      TYPE T370K_T-EARTX.
TYPES:  END OF TY_SAIDA.

TYPES: BEGIN OF TY_VIQMEL,
         QMNUM TYPE QMNUM,
         QMART TYPE QMART,
         EQUNR TYPE EQUNR,
         BEQUI TYPE BEQUI,
         BTPLN TYPE BTPLN,
         MSAUS TYPE MSAUS,
         AUSVN TYPE AUSVN,
         AUSBS TYPE AUSBS,
         AUZTV TYPE AUZTV,
         AUZTB TYPE AUZTB,
         AUSZT TYPE AUSZT,
         QMKAT TYPE QMKAT,
         QMGRP TYPE QMGRP,
         QMCOD TYPE QMCOD,
         EQART TYPE EQART.
TYPES:  END OF TY_VIQMEL.

DATA: T_VIQMEL    TYPE TABLE OF TY_VIQMEL,
      T_SAIDA     TYPE TABLE OF TY_SAIDA,
      IT_ZPMT0033 TYPE TABLE OF ZPMT0033,
      IT_EQKT     TYPE TABLE OF EQKT,
      IT_T370K_T  TYPE TABLE OF T370K_T,
      W_VIQMEL    TYPE TY_VIQMEL,
      W_SAIDA     TYPE TY_SAIDA,
      T_MESES     TYPE TABLE OF T247.

DATA: INI_MES TYPE N LENGTH 2,
      FIM_MES TYPE N LENGTH 2.

DATA: D_PARADA     TYPE DMBTR,
      N_PARADA     TYPE I,
      HTML_GRAFICO TYPE STRING,
      VL_FLUT      TYPE DMBTR.

DATA: EXP TYPE P DECIMALS 14 VALUE '2.718281828'.

DATA: BF        TYPE ZPPED013,
      TR        TYPE ZPPED013,
      CATEGORIA TYPE STRING,
      GRAFICO1  TYPE STRING,
      GRAFICO2  TYPE STRING,
      GRAFICO3  TYPE STRING,
      GRAFICO4  TYPE STRING,
      TEXT      TYPE STRING,
      SUBTITLE  TYPE STRING.

DATA: CONTAINER         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CONTAINER_GRAFICO TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV               TYPE REF TO CL_GUI_ALV_GRID,
      LAYOUT            TYPE LVC_S_LAYO,
      T_FCAT            TYPE LVC_T_FCAT,
      _STABLE           TYPE LVC_S_STBL VALUE 'XX',
      T_EXCLUDE         TYPE UI_FUNCTIONS.



SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_IWERK FOR QMIH-IWERK OBLIGATORY NO INTERVALS NO-EXTENSION ,
                S_ERDAT FOR QMEL-ERDAT OBLIGATORY NO-EXTENSION,
                S_EQART FOR ITOB-EQART NO INTERVALS NO-EXTENSION MODIF ID F1,
                S_EQUNR FOR QMIH-EQUNR NO INTERVALS NO-EXTENSION MODIF ID F2.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: X_EQUI   RADIOBUTTON GROUP G1,
            X_PEQUIP RADIOBUTTON GROUP G1,
            X_EMERG  RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK  B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS: G_MTTR  RADIOBUTTON GROUP G2,
            G_MTBF  RADIOBUTTON GROUP G2,
            G_DISPO RADIOBUTTON GROUP G2,
            G_CONF  RADIOBUTTON GROUP G2.
SELECTION-SCREEN END OF BLOCK B3.


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF S_EQART[] IS NOT INITIAL.
      IF SCREEN-GROUP1 = 'F2'.
        SCREEN-INPUT     = 0.
        SCREEN-ACTIVE    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ELSEIF S_EQUNR[] IS NOT INITIAL.
      IF SCREEN-GROUP1 = 'F1'.
        SCREEN-INPUT     = 0.
        SCREEN-ACTIVE    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.


START-OF-SELECTION.

  PERFORM SELECIONA_DADOS.

  CALL SCREEN 0100.



FORM SELECIONA_DADOS.

  IF X_EQUI IS NOT INITIAL.
    "individual
    SELECT *
      FROM VIQMEL AS A
      INNER JOIN EQUI AS B ON A~EQUNR EQ B~EQUNR
      INTO CORRESPONDING FIELDS OF TABLE T_VIQMEL
      WHERE A~IWERK IN S_IWERK
        AND A~EQUNR IN S_EQUNR
        AND B~EQART IN S_EQART
        AND A~ERDAT IN S_ERDAT
        AND A~MSAUS EQ ABAP_TRUE.

  ELSEIF X_PEQUIP IS NOT INITIAL.

    " Global
    SELECT *
      FROM VIQMEL AS A
      INNER JOIN EQUI AS B ON A~BEQUI EQ B~EQUNR
      INTO CORRESPONDING FIELDS OF TABLE T_VIQMEL
      WHERE A~IWERK IN S_IWERK
      AND   A~BEQUI IN S_EQUNR
      AND   A~ERDAT IN S_ERDAT
      AND   B~EQART IN S_EQART.

  ELSEIF X_EMERG IS NOT INITIAL.

    "Emergencial
    SELECT *
      FROM VIQMEL AS A
      INNER JOIN EQUI AS B ON A~BEQUI EQ B~EQUNR
      INTO CORRESPONDING FIELDS OF TABLE T_VIQMEL
      WHERE A~IWERK IN S_IWERK
      AND   A~BEQUI IN S_EQUNR
      AND   A~ERDAT IN S_ERDAT
      AND   A~QMART IN ( 'Z1', 'Y1', 'P1' )
      AND   B~EQART IN S_EQART.

  ENDIF.

  SELECT *
    FROM ZPMT0033
    INTO TABLE IT_ZPMT0033
  WHERE IWERK IN S_IWERK
    AND EQART IN S_EQART.

  SELECT *
    FROM T247
    INTO TABLE T_MESES
    WHERE SPRAS EQ SY-LANGU.

  IF S_EQART[] IS NOT INITIAL.
    SELECT *
      FROM T370K_T
      INTO TABLE IT_T370K_T
    WHERE SPRAS EQ SY-LANGU
      AND EQART IN S_EQART.
  ENDIF.


  IF S_EQUNR[] IS NOT INITIAL.
    SELECT *
      FROM EQKT
      INTO TABLE IT_EQKT
     WHERE EQUNR IN S_EQUNR
      AND  SPRAS EQ SY-LANGU.
  ENDIF.



  LOOP AT T_VIQMEL INTO W_VIQMEL.

    INI_MES = W_VIQMEL-AUSVN+4(2).
    FIM_MES = W_VIQMEL-AUSBS+4(2).
    N_PARADA = 0.

    "    DO.
    IF INI_MES > FIM_MES.
      EXIT.
    ENDIF.

    W_SAIDA-MES_N = INI_MES.
    W_SAIDA-MES = T_MESES[ MNR = INI_MES ]-LTX.
    W_SAIDA-KTX = T_MESES[ MNR = INI_MES ]-KTX.

    IF LINE_EXISTS( T_SAIDA[ MES = W_SAIDA-MES ] ) .
      PERFORM CALC_PARADA CHANGING D_PARADA.
      ADD D_PARADA TO W_SAIDA-D_PARADA.

      READ TABLE T_SAIDA INTO DATA(WSAIDA) WITH KEY MES = W_SAIDA-MES.
      IF SY-SUBRC = 0.
        W_SAIDA-N_PARADA = WSAIDA-N_PARADA + 1.
      ELSE.
        ADD 1 TO N_PARADA.
        W_SAIDA-N_PARADA = N_PARADA.
      ENDIF.


    ELSE.
      PERFORM CALC_PARADA CHANGING W_SAIDA-D_PARADA.
      N_PARADA = 1.
      W_SAIDA-N_PARADA = N_PARADA.
    ENDIF.

    PERFORM CALC_OPERACAO CHANGING W_SAIDA-T_OPERACAO.

    TRY .
        W_SAIDA-MTTR =  W_SAIDA-D_PARADA / W_SAIDA-N_PARADA.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    TRY .
        W_SAIDA-MTBF = ( W_SAIDA-T_OPERACAO - W_SAIDA-D_PARADA ) / W_SAIDA-N_PARADA.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    TRY .
        W_SAIDA-DISPO = ( W_SAIDA-MTBF / ( W_SAIDA-MTBF +  W_SAIDA-MTTR ) ) * 100.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    TRY .
        W_SAIDA-TX_FALHA = 1 / W_SAIDA-MTBF.
      CATCH CX_SY_ZERODIVIDE.
    ENDTRY.

    READ TABLE IT_ZPMT0033 INTO DATA(WA_ZPMT0033) WITH KEY EQART = W_VIQMEL-EQART.
    IF SY-SUBRC = 0.
      W_SAIDA-MMTTR = WA_ZPMT0033-MTTR.
      W_SAIDA-MMTBF = WA_ZPMT0033-MTBF.
      W_SAIDA-INDIS = WA_ZPMT0033-INDIS.
      W_SAIDA-CONF  = WA_ZPMT0033-CONF.
    ENDIF.


    READ TABLE IT_EQKT INTO DATA(WA_EQKT) WITH KEY EQUNR =  W_VIQMEL-EQUNR.
    IF SY-SUBRC = 0.
      W_SAIDA-EQKTX = WA_EQKT-EQKTX.
    ENDIF.

    READ TABLE IT_T370K_T INTO DATA(WA_T370K_T)  WITH KEY EQART =  W_VIQMEL-EQART.
    IF SY-SUBRC = 0.
      W_SAIDA-EARTX = WA_T370K_T-EARTX.
    ENDIF.

    BF = WA_ZPMT0033-MTBF.
    TR = WA_ZPMT0033-MTTR.

    W_SAIDA-CONFI = ( EXP ** ( - W_SAIDA-TX_FALHA * 30 ) ) * 100 .

    IF NOT LINE_EXISTS( T_SAIDA[ MES = W_SAIDA-MES ] ) .
      APPEND W_SAIDA TO T_SAIDA.
      CLEAR W_SAIDA.
    ELSE.
      MODIFY T_SAIDA
        FROM W_SAIDA
          TRANSPORTING D_PARADA N_PARADA
                       T_OPERACAO MTTR MTBF DISPO DISPO
                       TX_FALHA CONFI MMTTR MMTBF

      WHERE MES = W_SAIDA-MES.
      CLEAR W_SAIDA.
    ENDIF.

    "      ADD 1 TO INI_MES.
    "    ENDDO.

    CLEAR W_VIQMEL.
  ENDLOOP.

  SORT T_SAIDA BY MES_N.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

  PERFORM FIELDCATALOG USING:

      01 'MES'        ''  ''  '' ' ' '' 'X' '' '' '' 'Mês',
      02 'D_PARADA'   ''  ''  '' ' ' '' 'X' '' '' '' 'Duração da Parada Total',
      03 'N_PARADA'   ''  ''  '' ' ' '' 'X' '' '' '' 'Numero de paradas',
      04 'T_OPERACAO' ''  ''  '' ' ' '' 'X' '' '' '' 'Tempo em Operação',
      05 'MTTR'       ''  ''  '' ' ' '' 'X' '' '' '' 'MTTR',
      06 'MTBF'       ''  ''  '' ' ' '' 'X' '' '' '' 'MTBF',
      07 'DISPO'      ''  ''  '' ' ' '' 'X' '' '' '' 'Disponibilidade',
      08 'TX_FALHA'   ''  ''  '' ' ' '' 'X' '' '' '' 'Taxa de falha',
      09 'CONFI'      ''  ''  '' ' ' '' 'X' '' '' '' 'Confiabilidade',
      10 'MMTTR'      ''  ''  '' ' ' '' 'X' '' '' '' 'Meta MTTR',
      11 'MMTBF'      ''  ''  '' ' ' '' 'X' '' '' '' 'Meta MTBF',
      12 'INDIS'      ''  ''  '' ' ' '' 'X' '' '' '' 'Meta Disp',
      13 'CONF '      ''  ''  '' ' ' '' 'X' '' '' '' 'Meta Conf.'.

  IF CONTAINER IS INITIAL.

    CREATE OBJECT CONTAINER
      EXPORTING
        CONTAINER_NAME = 'CC'.

    CREATE OBJECT ALV
      EXPORTING
        I_PARENT = CONTAINER.

    CALL METHOD ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = LAYOUT
        I_SAVE               = 'A'
        IT_TOOLBAR_EXCLUDING = T_EXCLUDE
      CHANGING
        IT_FIELDCATALOG      = T_FCAT
        IT_OUTTAB            = T_SAIDA.
  ELSE.

    CALL METHOD ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = _STABLE.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'GRAFICO'.
      CALL SCREEN 0101.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0101 OUTPUT.

  SET PF-STATUS 'PF0101'.
  SET TITLEBAR 'TI0101'.

  PERFORM GRAFICO.

  CREATE OBJECT CONTAINER_GRAFICO
    EXPORTING
      CONTAINER_NAME = 'CCGRAFICO'.

  CL_ABAP_BROWSER=>SHOW_HTML(
  EXPORTING
   HTML_STRING = HTML_GRAFICO
   TITLE       = 'Indicadores MTBF MTTR'
*   MODAL       = ABAP_FALSE
*   FORMAT      = CL_ABAP_BROWSER=>LANDSCAPE
*   SIZE        = CL_ABAP_BROWSER=>SMALL
   CONTAINER   = CONTAINER_GRAFICO ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0101 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM FIELDCATALOG USING VALUE(P_COLNUM)
                        VALUE(P_FIELDNAME)
                        VALUE(P_TABNAME)
                        VALUE(P_EMPHASIZE)
                        VALUE(P_EDIT)
                        VALUE(P_ICON)
                        VALUE(P_HOTSPOT)
                        VALUE(P_OPT)
                        VALUE(P_CHECKBOX)
                        VALUE(P_DOSUM)
                        VALUE(P_F4)
                        VALUE(P_HEADER).

  APPEND
  VALUE #(
            COL_POS    = P_COLNUM
            FIELDNAME  = P_FIELDNAME
            TABNAME    = P_TABNAME
            EMPHASIZE  = P_EMPHASIZE
            COLTEXT    = P_HEADER
            EDIT       = P_EDIT
            ICON       = P_ICON
            REF_TABLE  = P_TABNAME
            HOTSPOT    = P_HOTSPOT
            COL_OPT    = P_OPT
            CHECKBOX   = P_CHECKBOX
            DO_SUM     = P_DOSUM
            F4AVAILABL = P_F4
         ) TO T_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALC_PARADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_VIQMEL_AUSBS  text
*      -->P_W_VIQMEL_AUSVN  text
*      <--P_W_SAIDA_D_PARADA  text
*----------------------------------------------------------------------*
FORM CALC_PARADA  CHANGING P_PARADA.

  DATA: DAYS_DIFF TYPE I,
        TIME_DIFF TYPE I,
        FIRS_DAY  TYPE SY-DATUM,
        LAST_DAY  TYPE SY-DATUM,
        FISR_H    TYPE AUZTB,
        LAST_H    TYPE AUZTV.

  IF W_VIQMEL-AUSBS+4(2) EQ W_VIQMEL-AUSVN+4(2).
    FIRS_DAY = W_VIQMEL-AUSVN.
    FISR_H   = W_VIQMEL-AUZTV.

    LAST_DAY = W_VIQMEL-AUSBS.
    LAST_H   = W_VIQMEL-AUZTB.
  ELSE.

    FIRS_DAY = |{ W_VIQMEL-AUSVN(4) }{ INI_MES }01|.

    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = FIRS_DAY
      IMPORTING
        E_DATE = LAST_DAY.

    IF W_VIQMEL-AUSVN >= FIRS_DAY.
      FIRS_DAY = W_VIQMEL-AUSVN.
      FISR_H   = W_VIQMEL-AUZTV.
    ELSE.
      FISR_H = '000000'.
    ENDIF.

    IF W_VIQMEL-AUSBS <= LAST_DAY.
      LAST_DAY = W_VIQMEL-AUSBS.
      LAST_H   = W_VIQMEL-AUZTB.
    ELSE.
      LAST_H = '235960'.
    ENDIF.

  ENDIF.

  IF LAST_DAY GE FIRS_DAY.
    DAYS_DIFF = LAST_DAY - FIRS_DAY.
    TIME_DIFF = LAST_H - FISR_H.
    P_PARADA = ( ( DAYS_DIFF * 24 * 60 ) + TIME_DIFF / 60 ) / 60.
  ELSE.
    P_PARADA = 0.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALC_OPERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_W_SAIDA_T_OPERACAO  text
*----------------------------------------------------------------------*
FORM CALC_OPERACAO  CHANGING P_OPERACAO.

  DATA: DAYS_DIFF TYPE I,
        TIME_DIFF TYPE I,
        FIRS_DAY  TYPE SY-DATUM,
        LAST_DAY  TYPE SY-DATUM,
        FISR_H    TYPE AUZTB,
        LAST_H    TYPE AUZTV.

  FIRS_DAY = |{ W_VIQMEL-AUSVN(4) }{ INI_MES }01|.

  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      I_DATE = FIRS_DAY
    IMPORTING
      E_DATE = LAST_DAY.

  FISR_H = '000000'.
  LAST_H = '235960'.

  DAYS_DIFF = LAST_DAY - FIRS_DAY.
  TIME_DIFF = LAST_H - FISR_H.
  P_OPERACAO = ( ( DAYS_DIFF * 24 * 60 ) + TIME_DIFF / 60 ) / 60.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GRAFICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GRAFICO.

  CLEAR: CATEGORIA, HTML_GRAFICO, GRAFICO1, GRAFICO2, GRAFICO3, GRAFICO4, TEXT, SUBTITLE.

  DATA(LM) = 100.

  LOOP AT T_MESES INTO DATA(W_MESES).
    W_MESES-KTX+1 = |{ W_MESES-KTX+1 CASE = LOWER }|.
    IF CATEGORIA IS INITIAL.
      CATEGORIA = |"{ W_MESES-KTX }"|.
    ELSE.
      CATEGORIA = |{ CATEGORIA }, "{ W_MESES-KTX }"|.
    ENDIF.
  ENDLOOP.


  LOOP AT T_MESES INTO W_MESES.

    LOOP AT T_SAIDA INTO W_SAIDA WHERE MES EQ W_MESES-LTX.

      BF =  W_SAIDA-MMTTR.
      TR =  W_SAIDA-MMTBF.

      IF GRAFICO1 IS INITIAL.
        IF W_SAIDA-MTBF IS INITIAL.
          GRAFICO1 = |0|.
        ELSE.
          GRAFICO1 = |{ W_SAIDA-MTBF }|.
        ENDIF.
      ELSE.
        IF W_SAIDA-MTBF IS INITIAL.
          GRAFICO1 = |{ GRAFICO1 }, 0|.
        ELSE.
          GRAFICO1 = |{ GRAFICO1 }, { W_SAIDA-MTBF }|.
        ENDIF.
      ENDIF.

      IF GRAFICO2 IS INITIAL.
        IF W_SAIDA-MTTR IS INITIAL.
          GRAFICO2 = |0|.
        ELSE.
          GRAFICO2 = |{ W_SAIDA-MTTR }|.
        ENDIF.
      ELSE.
        IF W_SAIDA-MTTR IS INITIAL.
          GRAFICO2 = |{ GRAFICO2 }, 0|.
        ELSE.
          GRAFICO2 = |{ GRAFICO2 }, { W_SAIDA-MTTR }|.
        ENDIF.
      ENDIF.


      IF GRAFICO3 IS INITIAL.
        IF W_SAIDA-DISPO IS INITIAL.
          GRAFICO3 = |0|.
        ELSE.
          GRAFICO3 = |{ W_SAIDA-DISPO }|.
        ENDIF.
      ELSE.
        IF W_SAIDA-DISPO IS INITIAL.
          GRAFICO3 = |{ GRAFICO3 }, 0|.
        ELSE.
          GRAFICO3 = |{ GRAFICO3 }, { W_SAIDA-DISPO }|.
        ENDIF.
      ENDIF.

      IF GRAFICO4 IS INITIAL.
        IF W_SAIDA-CONFI IS INITIAL.
          GRAFICO4 = |0|.
        ELSE.
          GRAFICO4 = |{ W_SAIDA-CONFI }|.
        ENDIF.
      ELSE.
        IF W_SAIDA-DISPO IS INITIAL.
          GRAFICO4 = |{ GRAFICO4 }, 0|.
        ELSE.
          GRAFICO4 = |{ GRAFICO4 }, { W_SAIDA-CONFI }|.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF SY-SUBRC IS NOT INITIAL.
      IF GRAFICO1 IS INITIAL.
        GRAFICO1 = |0|.
      ELSE.
        GRAFICO1 = |{ GRAFICO1 }, 0|.
      ENDIF.
      IF GRAFICO2 IS INITIAL.
        GRAFICO2 = |0|.
      ELSE.
        GRAFICO2 = |{ GRAFICO2 }, 0|.
      ENDIF.
      IF GRAFICO3 IS INITIAL.
        GRAFICO3 = |0|.
      ELSE.
        GRAFICO3 = |{ GRAFICO3 }, 0|.
      ENDIF.
      IF GRAFICO4 IS INITIAL.
        GRAFICO4 = |0|.
      ELSE.
        GRAFICO4 = |{ GRAFICO4 }, 0|.
      ENDIF.
    ENDIF.
  ENDLOOP.


  IF X_EQUI IS NOT INITIAL.
    TEXT = '"MTBF - MOTOR DE COMBUSTÃO PRINCIPAL"'.
  ELSEIF  X_PEQUIP IS NOT INITIAL.
    TEXT = '"MTBF - GERAL (MOTOR DE COMBUSTÃO PRINCIPAL)"'.
  ENDIF.

  IF S_EQART IS NOT INITIAL.
    SUBTITLE =  | { '"' && W_SAIDA-EARTX && '"' } |.
  ELSEIF S_EQUNR IS NOT INITIAL.
    SUBTITLE =  | { '"' && W_SAIDA-EQKTX && '"' } |.
  ENDIF.

*   // Grafico 01 MTBF - MOTOR DE COMBUSTÃO PRINCIPAL
  IF G_MTBF IS NOT INITIAL.

    HTML_GRAFICO =
     '<!DOCTYPE HTML>' &&
     '<html>' &&
     '<head>' &&
     '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' &&
     '<meta name="viewport" content="width=device-width, initial-scale=1">' &&
     '<title>Highcharts Example</title>' &&
     '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"' &&
     'integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">' &&
     '<style type="text/css">html{overflow-x:hidden;}</style>' &&
     '</head>' &&

     '<body>' &&

     '<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>' &&
     '<script src="https://code.highcharts.com/highcharts.js"></script>' &&

     '<div class="row">' &&
     '<div class="col" id="graf1" style="height: 250px"></div>' &&
     '</div>' &&

     '<script type="text/javascript">' &&

     'var chart = Highcharts.chart("graf1", { ' &&
     'title: { text:  ' && TEXT && '  }, subtitle:  { text: ' && SUBTITLE && '  }, ' &&
     'xAxis: { categories: [ ' && CATEGORIA && ' ], ' &&
     'title: { text: "Meses" } }, yAxis: {  title: { text: "Tempo entre falhas (H)", }, }, legend: { ' &&
     'layout: "vertical", align: "right", verticalAlign: "top", x: 0, y: 20, floating: true }, credits: { enabled: false }, ' &&
     'series: [ { name: "MTBF", type: "column", data: [' && GRAFICO1 && ' ], ' &&
     'showInLegend: true, dataLabels: { enabled: true } }, { type: "line", color: "red", ' &&
     |name: { BF }, data: [ { BF }, { BF }, { BF }, { BF }, { BF }, { BF }, { BF }, { BF }, { BF }, { BF }, { BF }, { BF } ], |.

    HTML_GRAFICO = HTML_GRAFICO &&
        'showInLegend: true, marker: { lineWidth: 0, lineColor: Highcharts.getOptions().colors[1], enabled: false } } ] }); ' &&
    '</script>' &&
    '</body>' &&
    '</html>'.

*   // Grafico 02 MTTF - MOTOR DE COMBUSTÃO PRINCIPAL
  ELSEIF G_MTTR IS NOT INITIAL.


    HTML_GRAFICO =
   '<!DOCTYPE HTML>' &&
   '<html>' &&
   '<head>' &&
   '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' &&
   '<meta name="viewport" content="width=device-width, initial-scale=1">' &&
   '<title>Highcharts Example</title>' &&
   '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"' &&
   'integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">' &&
   '<style type="text/css">html{overflow-x:hidden;}</style>' &&
   '</head>' &&

   '<body>' &&

   '<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>' &&
   '<script src="https://code.highcharts.com/highcharts.js"></script>' &&

   '<div class="row">' &&
   '<div class="col" id="graf2" style="height: 250px"></div>' &&
   '</div>' &&

   '<script type="text/javascript">' &&

   'var chart = Highcharts.chart("graf2", { ' &&
   'title: { text: ' && TEXT && '  }, subtitle:  { text: ' && SUBTITLE && '  }, ' &&
   'xAxis: { categories: [ ' && CATEGORIA && ' ], ' &&
   'title: { text: "Meses" } }, yAxis: { min: 8, max: 1000, title: { text: "Tempo entre falhas (H)", }, }, legend: { ' &&
   'layout: "vertical", align: "right", verticalAlign: "top", x: 0, y: 20, floating: true }, credits: { enabled: false }, ' &&
   'series: [ { name: "MTTF", type: "column", data: [' && GRAFICO2 && ' ], ' &&
   'showInLegend: true, dataLabels: { enabled: true } }, { type: "line", color: "red", ' &&
   |name: { TR }, data: [ { TR }, { TR }, { TR }, { TR }, { TR }, { TR }, { TR }, { TR }, { TR }, { TR }, { TR }, { TR } ], |.

    HTML_GRAFICO = HTML_GRAFICO &&
        'showInLegend: true, marker: { lineWidth: 0, lineColor: Highcharts.getOptions().colors[1], enabled: false } } ] }); ' &&
    '</script>' &&
    '</body>' &&
    '</html>'.

*       // Grafico 03 % DISPONIBILIDADE
  ELSEIF G_DISPO IS NOT INITIAL.

    HTML_GRAFICO =
     '<!DOCTYPE HTML>' &&
     '<html>' &&
     '<head>' &&
     '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' &&
     '<meta name="viewport" content="width=device-width, initial-scale=1">' &&
     '<title>Highcharts Example</title>' &&
     '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"' &&
     'integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">' &&
     '<style type="text/css">html{overflow-x:hidden;}</style>' &&
     '</head>' &&

     '<body>' &&

     '<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>' &&
     '<script src="https://code.highcharts.com/highcharts.js"></script>' &&

     '<div class="row">' &&
     '<div class="col" id="graf3" style="height: 250px"></div>' &&
     '</div>' &&

     '<script type="text/javascript">' &&

     'var chart = Highcharts.chart("graf3", { ' &&
     'title: { text: "% Disponibilidade" }, subtitle:  { text: ' && SUBTITLE && '  }, ' &&
     'xAxis: { categories: [ ' && CATEGORIA && ' ], ' &&
     'title: { text: "Meses" } }, yAxis: { min: 0, max: 100,  title: { text: "Tempo entre falhas (H)", }, }, legend: { ' &&
     'layout: "vertical", align: "right", verticalAlign: "top", x: 0, y: 20, floating: true }, credits: { enabled: false }, ' &&
     'series: [ { name: "Disponibilidade", type: "column", data: [' && GRAFICO3 && ' ], ' &&
     'showInLegend: true, dataLabels: { enabled: true } }, { type: "line", color: "red", ' &&
     |name: { LM }, data: [ { LM }, { LM }, { LM }, { LM }, { LM }, { LM }, { LM }, { LM }, { LM }, { LM }, { LM }, { LM } ], |.

    HTML_GRAFICO = HTML_GRAFICO &&
      'showInLegend: true, marker: { lineWidth: 0, lineColor: Highcharts.getOptions().colors[1], enabled: false } } ] }); ' &&
    '</script>' &&
    '</body>' &&
    '</html>'.

*    // Grafico 04 % CONFIABILIDADE
  ELSEIF G_CONF IS NOT INITIAL.

    HTML_GRAFICO =

     '<!DOCTYPE HTML>' &&
     '<html>' &&
     '<head>' &&
     '<meta http-equiv="Content-Type" content="text/html; charset=utf-8">' &&
     '<meta name="viewport" content="width=device-width, initial-scale=1">' &&
     '<title>Highcharts Example</title>' &&
     '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"' &&
     'integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">' &&
     '<style type="text/css">html{overflow-x:hidden;}</style>' &&
     '</head>' &&

     '<body>' &&

     '<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>' &&
     '<script src="https://code.highcharts.com/highcharts.js"></script>' &&

     '<div class="row">' &&
     '<div class="col" id="graf4" style="height: 250px"></div>' &&
     '</div>' &&

     '<script type="text/javascript">' &&

     'var chart = Highcharts.chart("graf4", { ' &&
     'title: { text: "% Confiabilidade" }, subtitle:  { text: ' && SUBTITLE && '  }, ' &&
     'xAxis: { categories: [ ' && CATEGORIA && ' ], ' &&
     'title: { text: "Meses" } }, yAxis: { min: 0, max: 100,  title: { text: "Tempo entre falhas (H)", }, }, legend: { ' &&
     'layout: "vertical", align: "right", verticalAlign: "top", x: 0, y: 20, floating: true }, credits: { enabled: false }, ' &&
     'series: [ { name: "Confiabilidade", type: "column", data: [' && GRAFICO4 && ' ], showInLegend: true, dataLabels: { enabled: true } }, ' .

    HTML_GRAFICO = HTML_GRAFICO &&
    '{ showInLegend: true, marker: { lineWidth: 0, lineColor: Highcharts.getOptions().colors[1], enabled: false } } ] }); ' &&
    '</script>' &&
    '</body>' &&
    '</html>'.


  ENDIF.


ENDFORM.
