*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Victor Hugo Souza Nunes ( victor.hugo@grupomaggi.com.br )            |*
*|                                                                           |*
*|                                                                           |*
*/===========================================================================\*


REPORT  ZCARGA.

TABLES: ZSDT0051, ZSDT0094, ZSDT0066.

DATA: IT_ZSDT0051 TYPE TABLE OF ZSDT0051,
      WA_ZSDT0051 TYPE ZSDT0051,
      IT_ZSDT0053 TYPE TABLE OF ZSDT0053,
      WA_ZSDT0053 TYPE ZSDT0053,
      IT_ZSDT0055 TYPE TABLE OF ZSDT0055,
      WA_ZSDT0055 TYPE ZSDT0055.

DATA: IT_ZSDT0094 TYPE TABLE OF ZSDT0094,
      WA_ZSDT0094 TYPE ZSDT0094.


DATA: VAR_ID TYPE C LENGTH 10.

DATA: VAR_TIPO TYPE C LENGTH 3.


DATA: VAR_FIXACAO TYPE C.
**********************************************************************
* Variaveis
**********************************************************************
DATA: VAR_MSG   TYPE STRING,
      VAR_CONT  TYPE C,
      VAR_CONTADOR_OK  TYPE SY-TABIX,
      VAR_CONTADOR_ER  TYPE SY-TABIX,
      VAR_PORTO TYPE DMBTR.

**********************************************************************
* Classe Exception
**********************************************************************
DATA: CX_EXCEPTION TYPE REF TO ZCX_WEBSERVICE.

**********************************************************************
* Classes que serão utilizadas nesse processo.
**********************************************************************
DATA: OBJ_ZCL_WEBSERVICE_TAXA_CURVA TYPE REF TO ZCL_WEBSERVICE_TX_CURVA.

SELECTION-SCREEN:  BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_NR_OV  FOR ZSDT0051-NRO_SOL_OV,
                P_PROG   FOR ZSDT0094-PROGRAMA.

PARAMETERS: R_DELETA  AS CHECKBOX DEFAULT '',
            R_NEGATI  AS CHECKBOX,
            R_NEG_P   AS CHECKBOX,
            R_CHAVE   AS CHECKBOX,
            R_DL_CHV  AS CHECKBOX,
            R_RIS_SN  AS CHECKBOX.

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN:  BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_INST  FOR ZSDT0066-INSTRUCAO,
                P_POSNR FOR ZSDT0066-POSNR,
                P_VBELN FOR ZSDT0066-VBELN.

PARAMETERS: R_DEL_66 AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK B2.

IF ( R_DELETA EQ 'X' ).
  IF ( P_PROG IS INITIAL  ).
    MESSAGE 'Valor do programa precisa ser preenchido' TYPE 'W'.
  ELSE.
    DELETE FROM ZSDT0094 WHERE NRO_SOL_OV IN P_NR_OV
                           AND PROGRAMA IN P_PROG.

    IF ( SY-SUBRC EQ 0 ).
      VAR_CONT = SY-DBCNT.
      CONCATENATE VAR_CONT 'Registros Deletados' INTO VAR_MSG SEPARATED BY SPACE.
      MESSAGE VAR_MSG TYPE 'E' DISPLAY LIKE 'S'.
    ENDIF.

  ENDIF.

ELSEIF ( R_DEL_66 EQ 'X' ).

  DELETE FROM ZSDT0066 WHERE NRO_SOL_OV IN P_NR_OV
                         AND INSTRUCAO  IN P_INST
                         AND POSNR      IN P_POSNR
                         AND VBELN      IN P_VBELN.

  COMMIT WORK.

  VAR_CONT = SY-DBCNT.
  CONCATENATE VAR_CONT 'Registros deletados' INTO VAR_MSG SEPARATED BY SPACE.
  MESSAGE VAR_MSG TYPE 'E' DISPLAY LIKE 'W'.

ELSEIF ( R_NEGATI EQ 'X' ).


  VAR_TIPO = 'FRE'.

  SELECT * FROM ZSDT0094
    INTO TABLE IT_ZSDT0094
  WHERE TIPO EQ VAR_TIPO
    AND NRO_SOL_OV IN P_NR_OV.

  IF ( SY-SUBRC EQ 0 ).

    LOOP AT IT_ZSDT0094 INTO WA_ZSDT0094.

      CONDENSE WA_ZSDT0094-FRETE_PORTO NO-GAPS.
      CONDENSE WA_ZSDT0094-FRETE_CIF   NO-GAPS.

      VAR_PORTO = WA_ZSDT0094-FRETE_PORTO.

      IF ( VAR_PORTO > WA_ZSDT0094-FRETE_CIF ).

        VAR_CONTADOR_OK = VAR_CONTADOR_OK + 1.

        WA_ZSDT0094-CADENCIA_QTE  = WA_ZSDT0094-CADENCIA_QTE * -1.
        WA_ZSDT0094-TOTAL_PROPORC = WA_ZSDT0094-TOTAL_PROPORC * -1.

        UPDATE ZSDT0094 SET CADENCIA_QTE  = WA_ZSDT0094-CADENCIA_QTE
                            TOTAL_PROPORC = WA_ZSDT0094-TOTAL_PROPORC
                         WHERE DATA_REGISTRO = WA_ZSDT0094-DATA_REGISTRO
                           AND HORA_REGISTRO = WA_ZSDT0094-HORA_REGISTRO
                           AND PROGRAMA      = WA_ZSDT0094-PROGRAMA.

        COMMIT WORK.
        WAIT UP TO 2 SECONDS.
      ELSE.
        VAR_CONTADOR_ER = VAR_CONTADOR_ER + 1.

      ENDIF.

      CLEAR: WA_ZSDT0094, VAR_PORTO.
    ENDLOOP.

    WRITE: / 'Registros afetados:'     , VAR_CONTADOR_OK,
             'Registros não afetados: ', VAR_CONTADOR_ER.

    CLEAR: VAR_CONTADOR_OK,
           VAR_CONTADOR_ER.

  ENDIF.


ELSEIF ( R_CHAVE EQ 'X' ).

  SELECT * FROM ZSDT0055
    INTO TABLE IT_ZSDT0055
  WHERE ID EQ 0000000000
    AND NRO_SOL_OV IN P_NR_OV.

  IF ( SY-SUBRC EQ 0 ).

    LOOP AT IT_ZSDT0055 INTO WA_ZSDT0055.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          NR_RANGE_NR = '01'
          OBJECT      = 'ZSEQ_LOG'
        IMPORTING
          NUMBER      = WA_ZSDT0055-ID.

      INSERT INTO ZSDT0055 VALUES WA_ZSDT0055.
      COMMIT WORK.
      CLEAR: WA_ZSDT0055.
    ENDLOOP.

  ENDIF.


ELSEIF ( R_DL_CHV EQ 'X' ).

  SELECT * FROM ZSDT0055
    INTO TABLE IT_ZSDT0055
  WHERE ID EQ 0000000000
    AND NRO_SOL_OV IN P_NR_OV.

  IF ( SY-SUBRC EQ 0 ) AND ( P_NR_OV IS INITIAL ).
    DELETE FROM ZSDT0055 WHERE ID EQ 0000000000.
    COMMIT WORK.

  ELSE.
    DELETE FROM ZSDT0055 WHERE ID EQ 0000000000
                           AND NRO_SOL_OV IN P_NR_OV.
    COMMIT WORK.
  ENDIF.

ELSEIF ( R_RIS_SN EQ 'X' ) .

  SELECT * FROM ZSDT0051
    INTO TABLE IT_ZSDT0051
  WHERE NRO_SOL_OV IN P_NR_OV
    AND RISCO_SACADO EQ SPACE.

  IF ( SY-SUBRC EQ  0 ).

    SELECT * FROM ZSDT0055
      INTO TABLE IT_ZSDT0055
      FOR ALL ENTRIES IN IT_ZSDT0051
    WHERE NRO_SOL_OV EQ IT_ZSDT0051-NRO_SOL_OV.


    LOOP AT IT_ZSDT0051 INTO WA_ZSDT0051.

      READ TABLE IT_ZSDT0055 INTO WA_ZSDT0055 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV.

      IF ( WA_ZSDT0055-VALDT_HEDGE IS INITIAL ).

        UPDATE ZSDT0051
          SET RISCO_SACADO = 'N'
        WHERE NRO_SOL_OV EQ WA_ZSDT0051-NRO_SOL_OV.
        COMMIT WORK.

        WAIT UP TO 2 SECONDS.

      ELSE.
        UPDATE ZSDT0051
          SET RISCO_SACADO = 'S'
        WHERE NRO_SOL_OV EQ WA_ZSDT0051-NRO_SOL_OV.
        COMMIT WORK.
        WAIT UP TO 2 SECONDS.
      ENDIF.

      CLEAR: WA_ZSDT0051, WA_ZSDT0055.
    ENDLOOP.

  ENDIF.

ELSE.

  SELECT * FROM ZSDT0051
    INTO TABLE IT_ZSDT0051
  WHERE STATUS     EQ 'L'
    AND NRO_SOL_OV IN P_NR_OV.

  CHECK NOT IT_ZSDT0051[] IS INITIAL.

  SELECT * FROM ZSDT0053
    INTO TABLE IT_ZSDT0053
    FOR ALL ENTRIES IN IT_ZSDT0051
  WHERE NRO_SOL_OV EQ IT_ZSDT0051-NRO_SOL_OV
    AND FIXACAO NE SPACE.

  LOOP AT IT_ZSDT0051 INTO WA_ZSDT0051.

    "WebService para Capturar o Valor da Taxa e gravar no campo TAXA_CURVA
    FREE: OBJ_ZCL_WEBSERVICE_TAXA_CURVA.
    CREATE OBJECT OBJ_ZCL_WEBSERVICE_TAXA_CURVA.
    TRY.
        OBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO  = WA_ZSDT0051-NRO_SOL_OV
                                                 I_TIPO    = 'LIB').

      CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
        VAR_MSG = CX_EXCEPTION->GET_TEXT( ).
        MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH VAR_MSG.
    ENDTRY.


  ENDLOOP.

ENDIF.
