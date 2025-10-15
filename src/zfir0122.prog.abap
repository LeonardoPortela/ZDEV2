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
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Marcos Santos ( marcos.santos@amaggi.com.br )                        |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Atualização de Saldo - Fundo Fixo                                         |*
**/===========================================================================\*
REPORT ZFIR0122.

TABLES ZFIT0216.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_ANO FOR ZFIT0216-GJAHR NO INTERVALS NO-EXTENSION OBLIGATORY,
                  S_MES FOR ZFIT0216-MONAT NO INTERVALS NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.

DATA: LV_SALDO_ANT   TYPE ZFIT0216-SALDO_FIXO,
      LV_TOTAL_MOV   TYPE ZFIT0217-DMBTR,
      LV_TOTAL_MOV_D TYPE ZFIT0217-DMBTR,
      LV_TOTAL_MOV_C TYPE ZFIT0217-DMBTR,
      LV_SALDO_ATUAL TYPE ZFIT0217-DMBTR.


START-OF-SELECTION.
  PERFORM F_SELECIONA_DADOS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  IF S_MES-LOW EQ '01'.
    DATA(ANO) = S_ANO-LOW - 1.
    DATA(MES) = '12'.
  ELSE.
    ANO = S_ANO-LOW.
    MES = S_MES-LOW - 1.
  ENDIF.

  SELECT *
    FROM ZFIT0216
    INTO TABLE @DATA(LT_0216)
    WHERE GJAHR EQ @ANO
      AND MONAT EQ @MES.

  IF LT_0216 IS INITIAL.
    MESSAGE 'Nenhuma Informação encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  LOOP AT LT_0216 INTO DATA(LS_0216).

    LV_SALDO_ANT = 0.

    LV_SALDO_ANT =
    REDUCE #( INIT I  TYPE DMBTR
              FOR LS_CC_216 IN LT_0216
              WHERE ( WERKS EQ LS_0216-WERKS )
              NEXT I = I + LS_CC_216-SALDO_FIXO
            ).

    SELECT _217~*
      FROM ZFIT0217 AS _217
        INNER JOIN ZIB_CONTABIL_CHV AS CHV ON _217~OBJ_KEY = CHV~OBJ_KEY
      INTO TABLE @DATA(LT_0217)
      WHERE _217~BUKRS EQ @LS_0216-BUKRS
        AND _217~WERKS EQ @LS_0216-WERKS
        AND _217~GJAHR EQ @S_ANO-LOW
        AND _217~MONAT EQ @S_MES-LOW
        AND _217~OBJ_KEY NE ''.

    LV_TOTAL_MOV_D = 0.
    LV_TOTAL_MOV_D =
    REDUCE #( INIT I TYPE DMBTR
              FOR LS_0217 IN LT_0217
              WHERE ( D_C EQ 'D' )
              NEXT I = I + LS_0217-DMBTR
            ).

    LV_TOTAL_MOV_D *= -1.

    LV_TOTAL_MOV_C = 0.
    LV_TOTAL_MOV_C =
    REDUCE #( INIT I TYPE DMBTR
              FOR LS_0217 IN LT_0217
              WHERE ( D_C EQ 'C' )
              NEXT I = I + LS_0217-DMBTR
            ).

    LV_TOTAL_MOV_C = ABS( LV_TOTAL_MOV_C ).

    LV_TOTAL_MOV = 0.
    LV_TOTAL_MOV = LV_TOTAL_MOV_D + LV_TOTAL_MOV_C.

    LV_SALDO_ATUAL = 0.
    LV_SALDO_ATUAL = LV_SALDO_ANT + LV_TOTAL_MOV.

    SELECT COUNT(*)
      FROM ZFIT0216
      WHERE BUKRS EQ @LS_0216-BUKRS
        AND WERKS EQ @LS_0216-WERKS
        AND GJAHR EQ @S_ANO-LOW
        AND MONAT EQ @S_MES-LOW.

    IF SY-SUBRC IS INITIAL.

      UPDATE ZFIT0216 SET SALDO_FIXO = LV_SALDO_ATUAL
        WHERE BUKRS EQ LS_0216-BUKRS
          AND WERKS EQ LS_0216-WERKS
          AND GJAHR EQ S_ANO-LOW
          AND MONAT EQ S_MES-LOW.

    ELSE.

      INSERT INTO ZFIT0216
      VALUES @(
      VALUE #( BUKRS = LS_0216-BUKRS
               WERKS = LS_0216-WERKS
               GJAHR = S_ANO-LOW
               MONAT = S_MES-LOW
               SALDO_FIXO = LV_SALDO_ATUAL
             ) ).

    ENDIF.

  ENDLOOP.

  MESSAGE 'Saldo Atualizado com Sucesso!' TYPE 'S'.

ENDFORM.
