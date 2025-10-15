*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 29/05/2012                                              &*
*& Descrição: Conversor de arquivo Fcont                              &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK922306   29.05.2012                            &*
*&--------------------------------------------------------------------&*

REPORT  ZFCONT_2.
TABLES: BSIS.

**--------------------------------------------------------------------**
**                       Declaração - Tabelas Internas
**--------------------------------------------------------------------**
DATA: BEGIN OF T_IMP OCCURS 0,
        FIELD(1000),
      END OF T_IMP,

      BEGIN OF T_SPLIT OCCURS 0,
       CAMPO(100),
      END OF T_SPLIT,

      BEGIN OF T_CONTAS OCCURS 0,
       SAKNR TYPE BSIS-HKONT,
       PERIO TYPE C,
      END OF T_CONTAS,

      BEGIN OF T_FAGLFLEXT OCCURS 0,
       RACCT TYPE FAGLFLEXT-RACCT,
       DRCRK TYPE FAGLFLEXT-DRCRK,
       HSLVT TYPE FAGLFLEXT-HSLVT,
       HSL01 TYPE FAGLFLEXT-HSL01,
       HSL02 TYPE FAGLFLEXT-HSL02,
       HSL03 TYPE FAGLFLEXT-HSL03,
       HSL04 TYPE FAGLFLEXT-HSL04,
       HSL05 TYPE FAGLFLEXT-HSL05,
       HSL06 TYPE FAGLFLEXT-HSL06,
       HSL07 TYPE FAGLFLEXT-HSL07,
       HSL08 TYPE FAGLFLEXT-HSL08,
       HSL09 TYPE FAGLFLEXT-HSL09,
       HSL10 TYPE FAGLFLEXT-HSL10,
       HSL11 TYPE FAGLFLEXT-HSL11,
       HSL12 TYPE FAGLFLEXT-HSL12,
       HSL13 TYPE FAGLFLEXT-HSL13,
       HSL14 TYPE FAGLFLEXT-HSL14,
       HSL15 TYPE FAGLFLEXT-HSL15,
       HSL16 TYPE FAGLFLEXT-HSL16,
      END OF T_FAGLFLEXT,

      T_OUT      LIKE TABLE OF T_IMP WITH HEADER LINE,
      T_OUT_AUX  LIKE TABLE OF T_IMP WITH HEADER LINE,

      T_IMP_I200 LIKE TABLE OF T_IMP WITH HEADER LINE.
**--------------------------------------------------------------------**
**                       Declaração - Variaveis globais
**--------------------------------------------------------------------**
DATA: X_FILENAME           TYPE STRING,
      WL_TABIX             TYPE SY-TABIX,
      X_MSGERRO            TYPE SY-LISEL,
      WG_PERIODO(3)        TYPE C,
      WG_FILE              TYPE RLGRAP-FILENAME,
      OK-CODE              TYPE SY-UCOMM VALUE 'LOCAL'.

**--------------------------------------------------------------------**
**                       Tela de Seleção
**--------------------------------------------------------------------**
SELECTION-SCREEN: BEGIN OF BLOCK A1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR BSIS-BUKRS,
                S_HKONT FOR BSIS-HKONT,
                S_COMP FOR BSIS-HKONT,
                S_GJAHR FOR BSIS-GJAHR.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-012.
PARAMETERS: P_ANUAL  RADIOBUTTON GROUP A1,
            P_TRIM   RADIOBUTTON GROUP A1.
*            p_2trim  radiobutton group a1,
*            p_3trim  radiobutton group a1.

SELECTION-SCREEN : END OF BLOCK B1.

*--> Arquivo
SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-011.
SELECTION-SCREEN: BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-013.
PARAMETERS: P_LOCAL  RADIOBUTTON GROUP A2 DEFAULT 'X' USER-COMMAND ASD,
            P_UNIX   RADIOBUTTON GROUP A2.

SELECTION-SCREEN : END OF BLOCK B3.

SELECTION-SCREEN: BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-014.

PARAMETERS: P_UTIL    RADIOBUTTON GROUP A4 DEFAULT 'X', "user-command ASD,
            P_BUSCA   RADIOBUTTON GROUP A4.
SELECT-OPTIONS: S_250 FOR BSIS-HKONT.

SELECTION-SCREEN : END OF BLOCK B4.

PARAMETERS: P_F_IN(250)  TYPE C OBLIGATORY DEFAULT 'C:\',
            P_F_AUX(250)  TYPE C NO-DISPLAY,
            P_F_OUT(250)  TYPE C OBLIGATORY DEFAULT 'C:\'.
SELECTION-SCREEN : END OF BLOCK B2.
*<--
SELECTION-SCREEN : END OF BLOCK A1.


AT SELECTION-SCREEN.
  PERFORM F_MODIFICA_TELA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_F_IN.
  PERFORM F_ABRE_ARQUIVO USING 'in'.

*at selection-screen on value-request for p_f_aux.
*  perform f_abre_arquivo using 'aux'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_F_OUT.
  PERFORM F_ABRE_ARQUIVO USING 'out'.

START-OF-SELECTION.
  CASE 'X'.
    WHEN P_LOCAL.
      PERFORM LER_TXT_LOCAL TABLES T_IMP
                            USING P_F_IN.

      IF P_F_AUX IS NOT INITIAL.
        PERFORM LER_TXT_LOCAL TABLES T_IMP_I200
                              USING P_F_AUX.
      ENDIF.
      IF T_IMP[] IS NOT INITIAL.
        IF P_BUSCA IS NOT INITIAL.
          PERFORM SELECIONA_DADOS_I200.
        ENDIF.
        PERFORM CONVERTE_ARQUIVO.
        PERFORM EXPORTA_ARQUIVO.
      ENDIF.
    WHEN P_UNIX.
      PERFORM LER_TXT_UNIX TABLES T_IMP
                           USING P_F_IN.
      IF P_F_AUX IS NOT INITIAL.
        PERFORM LER_TXT_UNIX TABLES T_IMP_I200
                             USING P_F_AUX.
      ENDIF.
      IF T_IMP[] IS NOT INITIAL.
        IF P_BUSCA IS NOT INITIAL.
          PERFORM SELECIONA_DADOS_I200.
        ENDIF.
        PERFORM CONVERTE_ARQUIVO.
        PERFORM EXPORTA_ARQUIVO_UNIX.
      ENDIF.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  F_ABRE_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ABRE_ARQUIVO USING P_FILE.
*  IF P_LOCAL EQ C_X.
*Funcao para abri "Disco Local"
  IF P_LOCAL IS NOT INITIAL.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
         EXPORTING
              FIELD_NAME    = 'C:\'
* FC - UPGRADE ECC 6.0 - LUP xxx - Início
*            MASK          = '.txt '
* FC - UPGRADE ECC6.0 - LUP xxx - Fim
         CHANGING
              FILE_NAME     = WG_FILE
         EXCEPTIONS
              MASK_TOO_LONG = 1
              OTHERS        = 2.
    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E897(SD) WITH 'Nome do arquivo he muito longo.'.
      WHEN 2.
        MESSAGE E897(SD) WITH 'Ocorreu um erro.'.
    ENDCASE.
    IF P_FILE EQ 'in'.
      P_F_IN = WG_FILE.
    ELSEIF P_FILE EQ 'aux'.
      P_F_AUX = WG_FILE.
    ELSEIF P_FILE EQ 'out'.
      P_F_OUT = WG_FILE.
    ENDIF.

  ENDIF.
ENDFORM.                    " F_ABRE_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  LER_TXT_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LER_TXT_LOCAL TABLES TL_IMP
                   USING P_FILE.
*  x_filename = p_f_in.
  X_FILENAME = P_FILE.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = X_FILENAME
      FILETYPE                = 'ASC'
      READ_BY_LINE            = 'X'
    TABLES
      DATA_TAB                = TL_IMP[]
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_READ_ERROR         = 2
      NO_BATCH                = 3
      GUI_REFUSE_FILETRANSFER = 4
      INVALID_TYPE            = 5
      NO_AUTHORITY            = 6
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8
      HEADER_NOT_ALLOWED      = 9
      SEPARATOR_NOT_ALLOWED   = 10
      HEADER_TOO_LONG         = 11
      UNKNOWN_DP_ERROR        = 12
      ACCESS_DENIED           = 13
      DP_OUT_OF_MEMORY        = 14
      DISK_FULL               = 15
      DP_TIMEOUT              = 16
      OTHERS                  = 17.


  IF SY-SUBRC NE 0.
*    MESSAGE I897(SD) WITH 'Erro na importação do arquivo.'.
        MESSAGE I897(SD) WITH sy-subrc.

    LEAVE LIST-PROCESSING.
    SET SCREEN 0.
  ELSE.
  ENDIF.





ENDFORM.                    "LER_TXT_LOCAL
*&---------------------------------------------------------------------*
*&      Form  CONVERTE_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONVERTE_ARQUIVO .
  DATA: BEGIN OF TL_LANCAMENTOS OCCURS 0,
        FIELD(1000),
        FLAG,
        END OF TL_LANCAMENTOS,

        BEGIN OF TL_SPERIO OCCURS 0,
         SAKNR TYPE SKA1-SAKNR,
         DPERIO TYPE FAGLFLEXT-HSL01,
         CPERIO TYPE FAGLFLEXT-HSL01,
         SFINAL TYPE FAGLFLEXT-HSL01,
         TIPO   TYPE C,
         PERIO  TYPE C,
        END OF TL_SPERIO.

  DATA: WL_VALOR_A TYPE FAGLFLEXT-HSL01,
        WL_VALOR_1 TYPE FAGLFLEXT-HSL01,
        WL_VALOR_2 TYPE FAGLFLEXT-HSL01,
        WL_VALOR_3 TYPE FAGLFLEXT-HSL01,
        WL_VALOR_4 TYPE FAGLFLEXT-HSL01,
        WL_VALOR_I TYPE FAGLFLEXT-HSL01,
        WL_VALOR_C TYPE FAGLFLEXT-HSL01,
        WL_VALOR_D TYPE FAGLFLEXT-HSL01,
        WL_VALOR_F TYPE FAGLFLEXT-HSL01,
        WL_VALOR_AUX(20),
        WL_LINHAS TYPE SY-TABIX,
        WL_LINHAS_AUX(6) TYPE C,
        WL_CONT TYPE SY-TABIX,
        WL_200_L TYPE SY-TABIX,
        WL_150_L TYPE SY-TABIX,
        WL_155_FLAG TYPE C,
        WL_DATA_AUX TYPE SY-DATUM,
        WL_DATA_AUX2 TYPE SY-DATUM,
        TL_OUT_AUX  LIKE TABLE OF T_OUT WITH HEADER LINE,
        TL_IMP_AUX  LIKE TABLE OF T_IMP WITH HEADER LINE,
        TL_SKA1     TYPE TABLE OF SKA1 WITH HEADER LINE,
        IT_ZSPED002 TYPE TABLE OF ZSPED002,
        TL_SKA1_24000 TYPE TABLE OF SKA1 WITH HEADER LINE,
        TL_FAGLFLEXT_155 LIKE TABLE OF FAGLFLEXT WITH HEADER LINE,
        TL_FAGLFLEXT_155_24000 LIKE TABLE OF FAGLFLEXT WITH HEADER LINE,
        TL_SPLIT_AUX LIKE TABLE OF T_SPLIT WITH HEADER LINE,
        TL_SPERIO_E LIKE TABLE OF TL_SPERIO WITH HEADER LINE,
        WL_FLAG,
        WL_TABIX_244000 TYPE SY-TABIX,
        WA_ZSPED002 TYPE ZSPED002,
        WL_PERIO.
  DATA:VAR_CAMPO  TYPE STRING,
       VAR_CAMPO1 TYPE STRING,
       VAR_CAMPO2 TYPE STRING,
       VAR_CAMPO_AUX TYPE ZSPED002-CONTA_REF_ECF.

*        TL_LANCAMENTO LIKE TABLE OF T_OUT WITH HEADER LINE.

*******  TL_IMP_AUX[] = T_IMP[].
*******  DELETE TL_IMP_AUX WHERE FIELD+1(4) NE 'I155'.
  LOOP AT T_IMP.
    IF T_IMP-FIELD+1(4) EQ 'I155'.
      APPEND T_IMP TO TL_IMP_AUX.
    ENDIF.
  ENDLOOP.
****> Pega contas para selecionar em tabela FAGLFLEXT
*  LOOP AT TL_IMP_AUX.
*    SPLIT TL_IMP_AUX AT '|' INTO TABLE T_SPLIT.
*    DELETE T_SPLIT INDEX 1.
*    CLEAR: T_SPLIT, T_CONTAS.
*    READ TABLE T_SPLIT INDEX 2.
*    IF T_SPLIT-CAMPO IS NOT  INITIAL.
*      MOVE: T_SPLIT-CAMPO TO T_CONTAS-HKONT.
*      APPEND T_CONTAS.
*      CLEAR T_CONTAS.
*    ENDIF.
*  ENDLOOP.

  SELECT * "#EC CI_DB_OPERATION_OK[2431747]
      FROM SKA1 "#EC CI_DB_OPERATION_OK[2389136]
      INTO TABLE TL_SKA1
       WHERE KTOPL EQ '0050'
         AND KTOKS EQ 'YB01' " 'YB02', 'YB03', 'YB04').
          OR   KTOKS EQ 'YB02'
          OR   KTOKS EQ 'YB03'
          OR   KTOKS EQ 'YB04' .

*  select racct drcrk hslvt
*         hsl01 hsl02 hsl03 hsl04 hsl05
*         hsl06 hsl07 hsl08 hsl09 hsl10
*         hsl11 hsl12 hsl13 hsl14 hsl15 hsl16
  SELECT *
      FROM FAGLFLEXT
      INTO TABLE TL_FAGLFLEXT_155
      FOR ALL ENTRIES IN TL_SKA1
       WHERE RYEAR IN S_GJAHR
*       AND RTCUR EQ 'BRL'
         AND RLDNR EQ '0L'
         AND RACCT EQ TL_SKA1-SAKNR
         AND RBUKRS IN S_BUKRS.


  SELECT * "#EC CI_DB_OPERATION_OK[2431747]
    FROM SKA1 "#EC CI_DB_OPERATION_OK[2389136]
    INTO TABLE TL_SKA1_24000
     WHERE KTOPL EQ '0050'
       AND  KTOKS EQ 'YB05'
         OR   KTOKS EQ 'YB06'
         OR   KTOKS EQ 'YB07'
         OR   KTOKS EQ 'YB08' .

*  select racct drcrk hslvt
*       hsl01 hsl02 hsl03 hsl04 hsl05
*       hsl06 hsl07 hsl08 hsl09 hsl10
*       hsl11 hsl12 hsl13 hsl14 hsl15 hsl16
  SELECT *
    FROM FAGLFLEXT
    INTO TABLE TL_FAGLFLEXT_155_24000
    FOR ALL ENTRIES IN TL_SKA1_24000
     WHERE RYEAR IN S_GJAHR
*     AND RTCUR EQ 'BRL'
       AND RLDNR EQ '0L'
       AND RACCT EQ TL_SKA1_24000-SAKNR
       AND RBUKRS IN S_BUKRS.


  SELECT *
     FROM ZSPED002
     INTO TABLE IT_ZSPED002
     WHERE BUKRS IN S_BUKRS.


*****************************

  TL_IMP_AUX[] = T_IMP[].
  DELETE TL_IMP_AUX WHERE FIELD+1(4) NE 'I200'
                      AND FIELD+1(4) NE 'I250'.


  REFRESH: T_OUT, T_CONTAS, T_FAGLFLEXT, TL_SPLIT_AUX, TL_SPERIO,
           TL_SPERIO_E.
  CLEAR: WL_FLAG.
  LOOP AT T_IMP.
    WL_TABIX = SY-TABIX.
    SPLIT T_IMP AT '|' INTO TABLE T_SPLIT.
    DELETE T_SPLIT INDEX 1.
    IF T_SPLIT[] IS NOT INITIAL.

      READ TABLE T_SPLIT INDEX 1.
      IF SY-SUBRC IS INITIAL.
**> Posição 0000
        IF T_SPLIT-CAMPO EQ '0000'.
***************************
****> Posição 2 -  LALU
          CLEAR T_SPLIT.
          MOVE 'LALU' TO T_SPLIT-CAMPO.
          MODIFY T_SPLIT INDEX 2.

****> Posição 12 -  0
          CLEAR T_SPLIT.
          MOVE '0|' TO T_SPLIT-CAMPO.
          INSERT T_SPLIT INDEX 12.
          CLEAR T_IMP.
          LOOP AT T_SPLIT.
*******************************************
            IF SY-TABIX LE 12.
              IF  SY-TABIX  EQ 1 .
                CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
              ELSE.
                CONCATENATE T_IMP-FIELD T_SPLIT-CAMPO INTO T_IMP-FIELD SEPARATED BY '|'.
              ENDIF.

            ENDIF.
          ENDLOOP.
          APPEND T_IMP TO T_OUT.
******************************I050
        ELSEIF  T_SPLIT-CAMPO EQ 'I050'.
          IF 'I050' IS NOT INITIAL.
            LOOP AT T_SPLIT.
              IF SY-TABIX EQ 6.
                VAR_CAMPO = T_SPLIT-CAMPO.
              ENDIF.
            ENDLOOP.
          ENDIF.
          APPEND T_IMP TO T_OUT.

        ELSEIF  T_SPLIT-CAMPO EQ 'I051'.
          READ TABLE IT_ZSPED002 INTO WA_ZSPED002 WITH KEY SAKNR = VAR_CAMPO.
          VAR_CAMPO_AUX = WA_ZSPED002-CONTA_REF_ECF.
          CONCATENATE VAR_CAMPO_AUX '|' INTO VAR_CAMPO_AUX.
          IF 'I051' IS NOT INITIAL.
            LOOP AT T_SPLIT.
              IF SY-TABIX EQ 2.
                T_SPLIT-CAMPO = '10'.
              ENDIF.
              IF SY-TABIX EQ 4.
                MOVE VAR_CAMPO_AUX TO T_SPLIT-CAMPO.
                MODIFY T_SPLIT INDEX 4.
              ENDIF.
              IF  SY-TABIX  EQ 1 .
                CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
              ELSE.
                CONCATENATE T_IMP-FIELD T_SPLIT-CAMPO INTO T_IMP-FIELD SEPARATED BY '|'.
              ENDIF.

            ENDLOOP.
            APPEND T_IMP TO T_OUT.
          ENDIF.
*************************
        ELSEIF T_SPLIT-CAMPO EQ 'I150'.
          CASE 'X'.
            WHEN P_ANUAL.
              READ TABLE T_OUT TRANSPORTING NO FIELDS
                WITH KEY FIELD+1(4) = 'I150'.
              IF SY-SUBRC IS NOT INITIAL.
                SPLIT T_IMP AT '|' INTO TABLE T_SPLIT.
                DELETE T_SPLIT INDEX 1.
****> inicio do ano posição I150
                CONCATENATE '0101' S_GJAHR-LOW INTO T_SPLIT-CAMPO.
                MODIFY T_SPLIT INDEX 2.
****> fim do ano posição I150
                CONCATENATE '3112' S_GJAHR-LOW INTO T_SPLIT-CAMPO.
                MODIFY T_SPLIT INDEX 3.

                READ TABLE T_SPLIT
                               WITH KEY CAMPO = '#'.
                IF SY-SUBRC IS INITIAL.
                  DELETE T_SPLIT INDEX SY-TABIX.
                ENDIF.
                LOOP AT T_SPLIT.
                  IF SY-TABIX EQ 1.
                    CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                  ELSE.
                    CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD.
                  ENDIF.
                  AT LAST.
                    CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                  ENDAT.
                ENDLOOP.
                APPEND T_IMP TO T_OUT.
                WL_150_L = SY-TABIX.
                CLEAR: WL_155_FLAG.
              ENDIF.
            WHEN P_TRIM.
              READ TABLE T_SPLIT INDEX 2.
              IF T_SPLIT-CAMPO+2(2) EQ '01'
              OR T_SPLIT-CAMPO+2(2) EQ '04'
              OR T_SPLIT-CAMPO+2(2) EQ '07'
              OR T_SPLIT-CAMPO+2(2) EQ '10'.
                SPLIT T_IMP AT '|' INTO TABLE T_SPLIT.
                DELETE T_SPLIT INDEX 1.
****> inicio do ano posição I150
                CONCATENATE '01' T_SPLIT-CAMPO+2(2) S_GJAHR-LOW INTO T_SPLIT-CAMPO.
                MODIFY T_SPLIT INDEX 2.


****> fim do ano posição I150
                IF T_SPLIT-CAMPO+2(2) EQ '01'.
                  CONCATENATE S_GJAHR-LOW '0301' INTO WL_DATA_AUX.
                ELSEIF T_SPLIT-CAMPO+2(2) EQ '04'.
                  CONCATENATE S_GJAHR-LOW '0601' INTO WL_DATA_AUX.
                ELSEIF T_SPLIT-CAMPO+2(2) EQ '07'.
                  CONCATENATE S_GJAHR-LOW '0901' INTO WL_DATA_AUX.
                ELSEIF T_SPLIT-CAMPO+2(2) EQ '10'.
                  CONCATENATE S_GJAHR-LOW '1201' INTO WL_DATA_AUX.
                ENDIF.
                CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
                  EXPORTING
                    DAY_IN            = WL_DATA_AUX
                  IMPORTING
                    LAST_DAY_OF_MONTH = WL_DATA_AUX2
                  EXCEPTIONS
                    DAY_IN_NOT_VALID  = 1
                    OTHERS            = 2.

                CONCATENATE WL_DATA_AUX2+6(2) WL_DATA_AUX2+4(2) WL_DATA_AUX2(4) INTO T_SPLIT-CAMPO.
                MODIFY T_SPLIT INDEX 3.

                LOOP AT T_SPLIT.
                  IF SY-TABIX EQ 1.
                    CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                  ELSE.
                    CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD.
                  ENDIF.
                  AT LAST.
                    CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                  ENDAT.
                ENDLOOP.
                APPEND T_IMP TO T_OUT.
                WL_150_L = SY-TABIX.
                CLEAR: WL_155_FLAG.
              ENDIF.

          ENDCASE.
        ELSEIF T_SPLIT-CAMPO EQ 'I155'.
          IF WL_150_L IS NOT INITIAL
          AND WL_155_FLAG IS INITIAL.
*              IF SY-SUBRC IS INITIAL.
            LOOP AT TL_SKA1.
              IF S_COMP[] IS NOT INITIAL.
                IF TL_SKA1-SAKNR IN S_COMP.
                  CONTINUE.
                ENDIF.
              ENDIF.
              CLEAR: WL_VALOR_I, WL_VALOR_D, WL_VALOR_C, WL_VALOR_F.
              LOOP AT TL_FAGLFLEXT_155
                WHERE RACCT EQ TL_SKA1-SAKNR.

                CLEAR : TL_SPERIO.

                CASE 'X'.
                  WHEN P_ANUAL.
                    ADD TL_FAGLFLEXT_155-HSLVT TO WL_VALOR_I.
                    IF TL_FAGLFLEXT_155-DRCRK EQ 'S'.
                      ADD TL_FAGLFLEXT_155-HSL01 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL02 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL03 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL04 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL05 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL06 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL07 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL08 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL09 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL10 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL11 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL12 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL13 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL14 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL15 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155-HSL16 TO WL_VALOR_D.
                    ELSEIF TL_FAGLFLEXT_155-DRCRK EQ 'H'.
                      ADD TL_FAGLFLEXT_155-HSL01 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL02 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL03 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL04 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL05 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL06 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL07 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL08 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL09 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL10 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL11 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL12 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL13 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL14 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL15 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155-HSL16 TO WL_VALOR_C.
                    ENDIF.
                  WHEN P_TRIM.
                    READ TABLE T_OUT INDEX WL_150_L.
                    SPLIT T_OUT AT '|' INTO TABLE TL_SPLIT_AUX.
                    DELETE TL_SPLIT_AUX INDEX 1.
                    READ TABLE TL_SPLIT_AUX INDEX 2.

******> Primeiro Trimeste.
                    IF TL_SPLIT_AUX-CAMPO+2(2) EQ '01'
                    OR TL_SPLIT_AUX-CAMPO+2(2) EQ '02'
                    OR TL_SPLIT_AUX-CAMPO+2(2) EQ '03'.
*******> Pega valor inicial da conta
                      ADD TL_FAGLFLEXT_155-HSLVT TO WL_VALOR_I.
                      IF TL_FAGLFLEXT_155-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155-HSL01 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL02 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL03 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155-HSL01 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL02 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL03 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO-PERIO = '1'.
******> Segundo Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '04'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '05'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '06'.
                      READ TABLE TL_SPERIO
                        WITH KEY SAKNR = TL_FAGLFLEXT_155-RACCT
                                 PERIO = '1'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.
                      IF TL_FAGLFLEXT_155-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155-HSL04 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL05 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL06 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155-HSL04 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL05 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL06 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO-PERIO = '2'.
******> Terceiro Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '07'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '08'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '09'.
                      READ TABLE TL_SPERIO
                         WITH KEY SAKNR = TL_FAGLFLEXT_155-RACCT
                                  PERIO = '2'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.

                      IF TL_FAGLFLEXT_155-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155-HSL07 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL08 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL09 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155-HSL07 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL08 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL09 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO-PERIO = '3'.
******> Quarto Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '10'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '11'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '12'.
                      READ TABLE TL_SPERIO
                        WITH KEY SAKNR = TL_FAGLFLEXT_155-RACCT
                                 PERIO = '3'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.

                      IF TL_FAGLFLEXT_155-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155-HSL10 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL11 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL12 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL13 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL14 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155-HSL15 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155-HSL10 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL11 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL12 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL13 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL14 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155-HSL15 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO-PERIO = '4'.
                    ENDIF.
                ENDCASE.
              ENDLOOP.
              IF SY-SUBRC IS NOT INITIAL.
                CONTINUE.
              ENDIF.

****> Posição I155 -> tipo de valor inicial> bloco 5
              IF WL_VALOR_I GE 0.
                MOVE 'D' TO T_SPLIT-CAMPO.
              ELSE.
                MOVE 'C' TO T_SPLIT-CAMPO.
              ENDIF.
              MODIFY T_SPLIT INDEX 5.

****> Posição I155 -> Valor de Debito> bloco 6
              IF WL_VALOR_D LT 0.
                MULTIPLY WL_VALOR_D BY -1.
              ENDIF.
              MOVE WL_VALOR_D TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 6.

****> Posição I155 -> Valor de Credito> bloco 7
              IF WL_VALOR_C LT 0.
                MULTIPLY WL_VALOR_C BY -1.
              ENDIF.
              MOVE WL_VALOR_C TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 7.
****> Posição I155 -> Valor final calculo
              IF TL_SKA1-SAKNR NE '0000244000'.
                WL_VALOR_F = WL_VALOR_I + WL_VALOR_D - WL_VALOR_C.
              ELSE.
                WL_VALOR_F = WL_VALOR_I.
              ENDIF.
****> Posição I155 -> Valor inicial da conta> bloco 4
              IF WL_VALOR_I LT 0.
                MULTIPLY WL_VALOR_I BY -1.
              ENDIF.
              MOVE WL_VALOR_I TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 4.

****> Posição I155 -> tipo de valor final> bloco 9
              IF WL_VALOR_F GE 0.
                MOVE 'D' TO T_SPLIT-CAMPO.
              ELSE.
                MOVE 'C' TO T_SPLIT-CAMPO.
              ENDIF.
              MODIFY T_SPLIT INDEX 9.

****> Posição I155 -> Valor final da conta> Bloco 8
              IF WL_VALOR_F LT 0.
                MULTIPLY WL_VALOR_F BY -1.
              ENDIF.
              MOVE WL_VALOR_F TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 8.

              MOVE TL_SKA1-SAKNR TO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 2.

              READ TABLE T_SPLIT
                WITH KEY CAMPO = '#'.
              IF SY-SUBRC IS INITIAL.
                DELETE T_SPLIT INDEX SY-TABIX.
              ENDIF.
              LOOP AT T_SPLIT.
                CONDENSE T_SPLIT-CAMPO NO-GAPS.
                IF SY-TABIX EQ 1.
                  CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ELSE.
                  CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ENDIF.
                AT LAST.
                  CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                ENDAT.
              ENDLOOP.
              MOVE: WL_VALOR_F    TO TL_SPERIO-SFINAL,
                    WL_VALOR_C    TO TL_SPERIO-CPERIO,
                    WL_VALOR_D    TO TL_SPERIO-DPERIO,
                    TL_SKA1-SAKNR TO TL_SPERIO-SAKNR.
              READ TABLE T_SPLIT INDEX 9.
              MOVE T_SPLIT-CAMPO TO TL_SPERIO-TIPO.

              IF TL_SKA1-SAKNR NE '0000244000'.

                IF WL_VALOR_F GT 0
                OR WL_VALOR_C GT 0
                OR WL_VALOR_D GT 0
                OR WL_VALOR_I GT 0.
                  APPEND T_IMP TO T_OUT.
                ENDIF.
                APPEND TL_SPERIO.

              ELSE.
                COLLECT TL_SPERIO.
              ENDIF.
*              APPEND TL_SPERIO.
              CLEAR: TL_SPERIO.

****> Flag q indica se teve algum movimento para o periodo.
              WL_155_FLAG = 'X'.
            ENDLOOP.

******************************************************************
            LOOP AT TL_SKA1_24000.
              CLEAR: WL_VALOR_I, WL_VALOR_D, WL_VALOR_C, WL_VALOR_F.
****> conta de encerramento 244000
              LOOP AT TL_FAGLFLEXT_155_24000
                WHERE RACCT EQ TL_SKA1_24000-SAKNR.
                CLEAR : TL_SPERIO_E.

                CASE 'X'.
                  WHEN P_ANUAL.
                    ADD TL_FAGLFLEXT_155_24000-HSLVT TO WL_VALOR_I.
                    IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                      ADD TL_FAGLFLEXT_155_24000-HSL01 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL02 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL03 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL04 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL05 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL06 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL07 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL08 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL09 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL10 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL11 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL12 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL13 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL14 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL15 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL16 TO WL_VALOR_D.
                    ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                      ADD TL_FAGLFLEXT_155_24000-HSL01 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL02 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL03 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL04 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL05 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL06 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL07 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL08 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL09 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL10 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL11 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL12 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL13 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL14 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL15 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL16 TO WL_VALOR_C.
                    ENDIF.
                  WHEN P_TRIM.
                    READ TABLE T_OUT INDEX WL_150_L.
                    SPLIT T_OUT AT '|' INTO TABLE TL_SPLIT_AUX.
                    DELETE TL_SPLIT_AUX INDEX 1.
                    READ TABLE TL_SPLIT_AUX INDEX 2.

******> Primeiro Trimeste.
                    IF TL_SPLIT_AUX-CAMPO+2(2) EQ '01'
                    OR TL_SPLIT_AUX-CAMPO+2(2) EQ '02'
                    OR TL_SPLIT_AUX-CAMPO+2(2) EQ '03'.
*******> Pega valor inicial da conta
*                      add tl_faglflext_155_24000-hs  lvt to wl_valor_i.
                      IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155_24000-HSL01 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL02 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL03 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155_24000-HSL01 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL02 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL03 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO_E-PERIO = '1'.
******> Segundo Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '04'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '05'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '06'.
                      READ TABLE TL_SPERIO_E
                        WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                                 PERIO = '1'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO_E-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO_E-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.
                      IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155_24000-HSL04 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL05 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL06 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155_24000-HSL04 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL05 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL06 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO_E-PERIO = '2'.
******> Terceiro Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '07'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '08'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '09'.
                      READ TABLE TL_SPERIO_E
                         WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                                  PERIO = '2'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO_E-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO_E-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.

                      IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155_24000-HSL07 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL08 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL09 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155_24000-HSL07 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL08 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL09 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO_E-PERIO = '3'.
******> Quarto Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '10'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '11'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '12'.
                      READ TABLE TL_SPERIO_E
                        WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                                 PERIO = '3'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO_E-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO_E-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.

                      IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155_24000-HSL10 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL11 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL12 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL13 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL14 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL15 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155_24000-HSL10 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL11 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL12 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL13 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL14 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL15 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO_E-PERIO = '4'.
                    ENDIF.
                ENDCASE.
              ENDLOOP.
              IF SY-SUBRC IS NOT INITIAL.
                CONTINUE.
              ENDIF.

****> Posição I155 -> tipo de valor inicial> bloco 5
              IF WL_VALOR_I GE 0.
                MOVE 'D' TO T_SPLIT-CAMPO.
              ELSE.
                MOVE 'C' TO T_SPLIT-CAMPO.
              ENDIF.
              MODIFY T_SPLIT INDEX 5.

****> Posição I155 -> Valor de Debito> bloco 6
              IF WL_VALOR_D LT 0.
                MULTIPLY WL_VALOR_D BY -1.
              ENDIF.
              MOVE WL_VALOR_D TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 6.

****> Posição I155 -> Valor de Credito> bloco 7
              IF WL_VALOR_C LT 0.
                MULTIPLY WL_VALOR_C BY -1.
              ENDIF.
              MOVE WL_VALOR_C TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 7.
****> Posição I155 -> Valor final calculo
              WL_VALOR_F = WL_VALOR_I + WL_VALOR_D - WL_VALOR_C.

****> Posição I155 -> Valor inicial da conta> bloco 4
              IF WL_VALOR_I LT 0.
                MULTIPLY WL_VALOR_I BY -1.
              ENDIF.
              MOVE WL_VALOR_I TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 4.

****> Posição I155 -> tipo de valor final> bloco 9
              IF WL_VALOR_F GE 0.
                MOVE 'D' TO T_SPLIT-CAMPO.
              ELSE.
                MOVE 'C' TO T_SPLIT-CAMPO.
              ENDIF.
              MODIFY T_SPLIT INDEX 9.

****> Posição I155 -> Valor final da conta> Bloco 8
              IF WL_VALOR_F LT 0.
                MULTIPLY WL_VALOR_F BY -1.
              ENDIF.
              MOVE WL_VALOR_F TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 8.

              MOVE TL_SKA1_24000-SAKNR TO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 2.


              MOVE: WL_VALOR_F    TO TL_SPERIO_E-SFINAL,
                    WL_VALOR_C    TO TL_SPERIO_E-CPERIO,
                    WL_VALOR_D    TO TL_SPERIO_E-DPERIO,
                    TL_SKA1_24000-SAKNR TO TL_SPERIO_E-SAKNR.
              READ TABLE T_SPLIT INDEX 9.
              MOVE T_SPLIT-CAMPO TO TL_SPERIO_E-TIPO.

              APPEND TL_SPERIO_E.
              CLEAR: TL_SPERIO_E.
            ENDLOOP.
******************************************************************

****> conta de encerramento 244000
            IF P_TRIM IS NOT INITIAL.
              LOOP AT TL_FAGLFLEXT_155_24000.
                CLEAR : TL_SPERIO.

                CASE 'X'.
                  WHEN P_ANUAL.
                    ADD TL_FAGLFLEXT_155_24000-HSLVT TO WL_VALOR_I.
                    IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                      ADD TL_FAGLFLEXT_155_24000-HSL01 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL02 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL03 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL04 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL05 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL06 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL07 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL08 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL09 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL10 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL11 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL12 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL13 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL14 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL15 TO WL_VALOR_D.
                      ADD TL_FAGLFLEXT_155_24000-HSL16 TO WL_VALOR_D.
                    ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                      ADD TL_FAGLFLEXT_155_24000-HSL01 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL02 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL03 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL04 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL05 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL06 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL07 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL08 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL09 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL10 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL11 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL12 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL13 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL14 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL15 TO WL_VALOR_C.
                      ADD TL_FAGLFLEXT_155_24000-HSL16 TO WL_VALOR_C.
                    ENDIF.
                  WHEN P_TRIM.
                    READ TABLE T_OUT INDEX WL_150_L.
                    SPLIT T_OUT AT '|' INTO TABLE TL_SPLIT_AUX.
                    DELETE TL_SPLIT_AUX INDEX 1.
                    READ TABLE TL_SPLIT_AUX INDEX 2.

******> Primeiro Trimeste.
                    IF TL_SPLIT_AUX-CAMPO+2(2) EQ '01'
                    OR TL_SPLIT_AUX-CAMPO+2(2) EQ '02'
                    OR TL_SPLIT_AUX-CAMPO+2(2) EQ '03'.
*******> Pega valor inicial da conta
                      ADD TL_FAGLFLEXT_155_24000-HSLVT TO WL_VALOR_I.
                      IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155_24000-HSL01 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL02 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL03 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155_24000-HSL01 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL02 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL03 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO_E-PERIO = '1'.
******> Segundo Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '04'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '05'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '06'.
                      READ TABLE TL_SPERIO
                        WITH KEY SAKNR = '0000244000'
                                 PERIO = '1'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.
                      IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155_24000-HSL04 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL05 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL06 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155_24000-HSL04 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL05 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL06 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO_E-PERIO = '2'.
******> Terceiro Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '07'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '08'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '09'.
                      READ TABLE TL_SPERIO
                         WITH KEY SAKNR = '0000244000'
                                  PERIO = '2'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.

                      IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155_24000-HSL07 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL08 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL09 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155_24000-HSL07 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL08 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL09 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO_E-PERIO = '3'.
******> Quarto Trimeste.
                    ELSEIF TL_SPLIT_AUX-CAMPO+2(2) EQ '10'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '11'
                       OR  TL_SPLIT_AUX-CAMPO+2(2) EQ '12'.
                      READ TABLE TL_SPERIO
                        WITH KEY SAKNR = '0000244000'
                                 PERIO = '3'.
                      IF SY-SUBRC IS INITIAL.
                        MOVE: TL_SPERIO-SFINAL TO WL_VALOR_I.
                        IF TL_SPERIO-TIPO EQ 'C'.
                          MULTIPLY WL_VALOR_I BY -1.
                        ENDIF.
                      ENDIF.

                      IF TL_FAGLFLEXT_155_24000-DRCRK EQ 'S'.
                        ADD TL_FAGLFLEXT_155_24000-HSL10 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL11 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL12 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL13 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL14 TO WL_VALOR_D.
                        ADD TL_FAGLFLEXT_155_24000-HSL15 TO WL_VALOR_D.
                      ELSEIF TL_FAGLFLEXT_155_24000-DRCRK EQ 'H'.
                        ADD TL_FAGLFLEXT_155_24000-HSL10 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL11 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL12 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL13 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL14 TO WL_VALOR_C.
                        ADD TL_FAGLFLEXT_155_24000-HSL15 TO WL_VALOR_C.
                      ENDIF.
                      TL_SPERIO_E-PERIO = '4'.
                    ENDIF.
                ENDCASE.
              ENDLOOP.

*****> Encerramento
              CLEAR: WL_TABIX_244000 , WL_PERIO.
              IF TL_SPERIO_E-PERIO NE '1'.
                WL_PERIO = TL_SPERIO_E-PERIO - 1.
              ELSE.
                WL_PERIO = TL_SPERIO_E-PERIO.
              ENDIF.

              READ TABLE TL_SPERIO
                WITH KEY SAKNR = '0000244000'
                         PERIO = WL_PERIO. "TL_SPERIO_E-PERIO.
              MOVE TL_SPERIO-SFINAL TO WL_VALOR_I.

************************
*              IF TL_SPERIO_E-PERIO NE '1'.
*                ADD 1 TO TL_SPERIO_E-PERIO.
*              ENDIF.
              READ TABLE TL_SPERIO
                WITH KEY SAKNR = '0000244000'
                         PERIO = TL_SPERIO_E-PERIO.
              WL_TABIX_244000 = SY-TABIX.
              ADD TL_SPERIO-DPERIO TO WL_VALOR_D.
              SUBTRACT TL_SPERIO-CPERIO FROM WL_VALOR_C.
************************

              IF TL_SPERIO-TIPO EQ 'C'.
                MULTIPLY WL_VALOR_I BY -1.
              ENDIF.
****> Posição I155 -> tipo de valor inicial> bloco 5
              IF WL_VALOR_I GE 0.
                MOVE 'D' TO T_SPLIT-CAMPO.
              ELSE.
                MOVE 'C' TO T_SPLIT-CAMPO.
              ENDIF.
              MODIFY T_SPLIT INDEX 5.


****> Posição I155 -> Valor de Debito> bloco 6
              IF WL_VALOR_D LT 0.
                MULTIPLY WL_VALOR_D BY -1.
              ENDIF.
              MOVE WL_VALOR_D TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 6.

****> Posição I155 -> Valor de Credito> bloco 7
              IF WL_VALOR_C LT 0.
                MULTIPLY WL_VALOR_C BY -1.
              ENDIF.
              MOVE WL_VALOR_C TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 7.
****> Posição I155 -> Valor final calculo
              WL_VALOR_F = WL_VALOR_I + WL_VALOR_D - WL_VALOR_C.
              CLEAR: TL_SPERIO.
****> Posição I155 -> Valor inicial da conta> bloco 4
              IF WL_VALOR_I LT 0.
                MULTIPLY WL_VALOR_I BY -1.
              ENDIF.
              MOVE WL_VALOR_I TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 4.

****> Posição I155 -> tipo de valor final> bloco 9
              IF WL_VALOR_F GE 0.
                MOVE 'D' TO T_SPLIT-CAMPO.
                MOVE 'D' TO TL_SPERIO-TIPO.
              ELSE.
                MOVE 'C' TO T_SPLIT-CAMPO.
                MOVE 'C' TO TL_SPERIO-TIPO.
              ENDIF.
              MODIFY T_SPLIT INDEX 9.

****> Posição I155 -> Valor final da conta> Bloco 8
              IF WL_VALOR_F LT 0.
                MULTIPLY WL_VALOR_F BY -1.
              ENDIF.
              MOVE WL_VALOR_F TO T_SPLIT-CAMPO.
              TRANSLATE T_SPLIT-CAMPO USING '.,'.
              MODIFY T_SPLIT INDEX 8.


              MOVE '0000244000' TO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 2.

              READ TABLE T_SPLIT
                WITH KEY CAMPO = '#'.
              IF SY-SUBRC IS INITIAL.
                DELETE T_SPLIT INDEX SY-TABIX.
              ENDIF.

              LOOP AT T_SPLIT.
                CONDENSE T_SPLIT-CAMPO NO-GAPS.
                IF SY-TABIX EQ 1.
                  CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ELSE.
                  CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD .
                ENDIF.
                AT LAST.
                  CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                ENDAT.
              ENDLOOP.

              MOVE: WL_VALOR_F    TO TL_SPERIO-SFINAL,
                     TL_SPERIO_E-PERIO TO TL_SPERIO-PERIO,
*                    WL_VALOR_C    TO TL_SPERIO-CPERIO,
*                    WL_VALOR_D    TO TL_SPERIO-DPERIO,
                    '0000244000'  TO TL_SPERIO-SAKNR.
*              ADD 1 TO TL_SPERIO-PERIO.
              READ TABLE T_SPLIT INDEX 9.
              MOVE T_SPLIT-CAMPO TO TL_SPERIO-TIPO.

              IF WL_VALOR_F GT 0
                OR WL_VALOR_C GT 0
                OR WL_VALOR_D GT 0
                OR WL_VALOR_I GT 0.
                APPEND T_IMP TO T_OUT.
              ENDIF.
*              APPEND TL_SPERIO.
              IF WL_TABIX_244000 IS NOT INITIAL.
                MODIFY TL_SPERIO INDEX WL_TABIX_244000.
              ENDIF.
              CLEAR: TL_SPERIO.
              IF TL_FAGLFLEXT_155_24000[] IS NOT INITIAL.
****> Flag q indica se teve algum movimento para o periodo.
                WL_155_FLAG = 'X'.
              ENDIF.
              ADD 1 TO WL_TABIX.
              READ TABLE T_IMP INDEX WL_TABIX.
              IF T_IMP-FIELD+1(4) NE 'I150'
              AND T_IMP-FIELD+1(4) NE 'I155'.
****> Caso nao haja nenhum movimento para o periodo o perido é eliminado do arquivo..
                IF WL_155_FLAG IS INITIAL.
                  DELETE T_OUT INDEX WL_150_L.
                  CLEAR : WL_150_L.
                ENDIF.
              ENDIF.
              SUBTRACT 1 FROM WL_TABIX.
            ENDIF.
          ENDIF.
        ELSEIF T_SPLIT-CAMPO EQ 'I200'.
          IF T_IMP_I200[] IS INITIAL.
****> Posição 5 - POR UM 'X'
            CLEAR T_SPLIT.
            READ TABLE T_SPLIT INDEX 5.
            IF T_SPLIT-CAMPO NE 'E'.
              MOVE 'X|' TO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 5.
              LOOP AT T_SPLIT.
                IF SY-TABIX EQ 1.
                  CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ELSE.
                  CONCATENATE T_IMP-FIELD T_SPLIT-CAMPO INTO T_IMP-FIELD SEPARATED BY '|'.
                ENDIF.
              ENDLOOP.
              APPEND T_IMP TO T_OUT.
              CLEAR T_OUT.
              WL_200_L = SY-TABIX.
              REFRESH TL_LANCAMENTOS.
            ELSE.
*            APPEND T_IMP TO T_OUT.
*            WL_200_L = SY-TABIX.
              CLEAR WL_200_L.
              REFRESH TL_LANCAMENTOS.
            ENDIF.
          ELSEIF WL_FLAG IS INITIAL.
            WL_FLAG = 'X'.
            LOOP AT T_IMP_I200 INTO T_IMP.
              WL_TABIX = SY-TABIX.
              APPEND T_IMP TO T_OUT.
*              split t_imp at '|' into table t_split.
*              delete t_split index 1.
*              if t_split[] is not initial.
*                read table t_split index 1.
*                if t_split-campo eq 'I200'.
*****> posição 5 - por um 'X'
*                  clear t_split.
*                  read table t_split index 5.
*                  if t_split-campo ne 'E'.
*                    move 'X|' to t_split-campo.
*                    modify t_split index 5.
*                    loop at t_split.
*                      if sy-tabix eq 1.
*                        concatenate '|'  t_split-campo into t_imp-field.
*                      else.
*                        concatenate t_imp-field t_split-campo into t_imp-field separated by '|'.
*                      endif.
*                    endloop.
*                    append t_imp to t_out.
*                    clear t_out.
*                    wl_200_l = sy-tabix.
*                    refresh tl_lancamentos.
*                  else.
**            APPEND T_IMP TO T_OUT.
**            WL_200_L = SY-TABIX.
*                    clear wl_200_l.
*                    refresh tl_lancamentos.
*                  endif.
*
*                elseif t_split-campo eq 'I250'.
*                  if wl_200_l is not initial.
**            READ TABLE T_OUT INDEX WL_200_L.
**            SPLIT T_OUT AT '|' INTO TABLE TL_SPLIT_AUX.
**            DELETE TL_SPLIT_AUX INDEX 1.
**            READ TABLE TL_SPLIT_AUX INDEX 5.
**            IF TL_SPLIT_AUX-CAMPO NE 'E'.
*                    read table t_split index 2.
*
*                    move t_imp-field to tl_lancamentos-field.
**            read table tl_ska1
**              with key saknr = t_split-campo.
*                    if t_split-campo in s_hkont.
*
*                      move 'X' to tl_lancamentos-flag.
*                    else.
*
*                      move space to tl_lancamentos-flag.
*                    endif.
*                    append tl_lancamentos.
*                    add 1 to wl_tabix.
*                    clear: tl_imp_aux.
*                    read table t_imp_i200 into tl_imp_aux index wl_tabix.
*                    if tl_imp_aux-field+1(4) ne 'I250'.
*                      read table tl_lancamentos
*                        with key flag = 'X'.
*                      if sy-subrc is initial.
*                        loop at tl_lancamentos.
*                          move tl_lancamentos-field to t_out-field.
*
*                          append t_out.
*                          clear: t_out.
*                        endloop.
**
**                READ TABLE TL_LANCAMENTOS
**                WITH KEY FLAG = SPACE.
**                IF SY-SUBRC IS INITIAL.
*                        read table t_out into t_imp index wl_200_l.
*                        split t_imp at '|' into table t_split.
*                        delete t_split index 1.
*
*                        t_split-campo = 'EF|'.
*                        modify t_split index 5.
*                        clear: t_out.
*                        loop at t_split.
*                          if sy-tabix eq 1.
*                            concatenate '|'  t_split-campo into t_imp-field.
*                          else.
*                            concatenate t_imp-field t_split-campo into t_imp-field separated by '|'.
*                          endif.
*                        endloop.
*                        append t_imp to t_out.
*                        clear: t_out.
*
*                        loop at tl_lancamentos.
*                          if tl_lancamentos-flag is initial.
*                            read table tl_ska1
*                              with key saknr = tl_lancamentos+6(10).
*                            if sy-subrc is initial.
*                              tl_lancamentos-field+6(10) = '0000244000'.
*                            else.
*
*                            endif.
*                          endif.
*                          move tl_lancamentos-field to t_out-field.
*
*                          append t_out.
*                          clear: t_out.
*                        endloop.
**                ENDIF.
*                      else.
*                        delete t_out index wl_200_l.
*                        clear wl_200_l.
*                      endif.
*                    endif.
*                    subtract 1 from wl_tabix.
**              DELETE T_OUT INDEX WL_200_L.
**              CLEAR WL_200_L.
**              LOOP AT TL_LANCAMENTO.
**                DELETE T_OUT WHERE FIELD EQ TL_LANCAMENTO-FIELD.
**              ENDLOOP.
**            ELSE.
**              MOVE T_IMP TO T_OUT.
**              APPEND T_OUT.
**              APPEND T_OUT TO TL_LANCAMENTO.
**              CLEAR: T_OUT.
**            ENDIF.
**            ELSE.
**              APPEND T_IMP TO T_OUT.
**            ENDIF.
*                  endif.
*                endif.
*              endif.
            ENDLOOP.
          ENDIF.
        ELSEIF T_SPLIT-CAMPO EQ 'I250'.
          IF T_IMP_I200[] IS INITIAL.
            IF WL_200_L IS NOT INITIAL.
*            READ TABLE T_OUT INDEX WL_200_L.
*            SPLIT T_OUT AT '|' INTO TABLE TL_SPLIT_AUX.
*            DELETE TL_SPLIT_AUX INDEX 1.
*            READ TABLE TL_SPLIT_AUX INDEX 5.
*            IF TL_SPLIT_AUX-CAMPO NE 'E'.
              READ TABLE T_SPLIT INDEX 2.

              MOVE T_IMP-FIELD TO TL_LANCAMENTOS-FIELD.
*            read table tl_ska1
*              with key saknr = t_split-campo.
              IF T_SPLIT-CAMPO IN S_HKONT.

                MOVE 'X' TO TL_LANCAMENTOS-FLAG.
              ELSE.

                MOVE SPACE TO TL_LANCAMENTOS-FLAG.
              ENDIF.
              APPEND TL_LANCAMENTOS.
              ADD 1 TO WL_TABIX.
              READ TABLE T_IMP INTO TL_IMP_AUX INDEX WL_TABIX.
              IF TL_IMP_AUX-FIELD+1(4) NE 'I250'.
                READ TABLE TL_LANCAMENTOS
                  WITH KEY FLAG = 'X'.
                IF SY-SUBRC IS INITIAL.
                  LOOP AT TL_LANCAMENTOS.
                    MOVE TL_LANCAMENTOS-FIELD TO T_OUT-FIELD.

                    APPEND T_OUT.
                    CLEAR: T_OUT.
                  ENDLOOP.
*
*                READ TABLE TL_LANCAMENTOS
*                WITH KEY FLAG = SPACE.
*                IF SY-SUBRC IS INITIAL.
                  READ TABLE T_OUT INTO T_IMP INDEX WL_200_L.
                  SPLIT T_IMP AT '|' INTO TABLE T_SPLIT.
                  DELETE T_SPLIT INDEX 1.

                  T_SPLIT-CAMPO = 'EF|'.
                  MODIFY T_SPLIT INDEX 5.
                  CLEAR: T_OUT.
                  LOOP AT T_SPLIT.
                    IF SY-TABIX EQ 1.
                      CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                    ELSE.
                      CONCATENATE T_IMP-FIELD T_SPLIT-CAMPO INTO T_IMP-FIELD SEPARATED BY '|'.
                    ENDIF.
                  ENDLOOP.
                  APPEND T_IMP TO T_OUT.
                  CLEAR: T_OUT.

                  LOOP AT TL_LANCAMENTOS.
                    IF TL_LANCAMENTOS-FLAG IS INITIAL.
                      READ TABLE TL_SKA1
                        WITH KEY SAKNR = TL_LANCAMENTOS+6(10).
                      IF SY-SUBRC IS INITIAL.
                        TL_LANCAMENTOS-FIELD+6(10) = '0000244000'.
                      ELSE.

                      ENDIF.
                    ENDIF.
                    MOVE TL_LANCAMENTOS-FIELD TO T_OUT-FIELD.

                    APPEND T_OUT.
                    CLEAR: T_OUT.
                  ENDLOOP.
*                ENDIF.
                ELSE.
                  DELETE T_OUT INDEX WL_200_L.
                  CLEAR WL_200_L.
                ENDIF.
              ENDIF.
              SUBTRACT 1 FROM WL_TABIX.
*              DELETE T_OUT INDEX WL_200_L.
*              CLEAR WL_200_L.
*              LOOP AT TL_LANCAMENTO.
*                DELETE T_OUT WHERE FIELD EQ TL_LANCAMENTO-FIELD.
*              ENDLOOP.
*            ELSE.
*              MOVE T_IMP TO T_OUT.
*              APPEND T_OUT.
*              APPEND T_OUT TO TL_LANCAMENTO.
*              CLEAR: T_OUT.
*            ENDIF.
*            ELSE.
*              APPEND T_IMP TO T_OUT.
*            ENDIF.
            ENDIF.
          ENDIF.
        ELSEIF T_SPLIT-CAMPO EQ 'I350'.
          CASE 'X'.
            WHEN P_ANUAL.
              CONCATENATE '3112' S_GJAHR-LOW INTO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 2.
              LOOP AT T_SPLIT.
                CONDENSE T_SPLIT-CAMPO NO-GAPS.
                IF SY-TABIX EQ 1.
                  CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ELSE.
                  CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ENDIF.
                AT LAST.
                  CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                ENDAT.
              ENDLOOP.
              APPEND T_IMP TO T_OUT.
              CLEAR: T_IMP.
            WHEN P_TRIM.
****** Primeiro Trimeste
              CONCATENATE S_GJAHR-LOW '03' '01' INTO WL_DATA_AUX.
              CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
                EXPORTING
                  DAY_IN            = WL_DATA_AUX
                IMPORTING
                  LAST_DAY_OF_MONTH = WL_DATA_AUX2
                EXCEPTIONS
                  DAY_IN_NOT_VALID  = 1
                  OTHERS            = 2.

              CONCATENATE WL_DATA_AUX2+6(2) WL_DATA_AUX2+4(2) WL_DATA_AUX2(4) INTO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 2.
              LOOP AT T_SPLIT.
                CONDENSE T_SPLIT-CAMPO NO-GAPS.
                IF SY-TABIX EQ 1.
                  CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ELSE.
                  CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ENDIF.
                AT LAST.
                  CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                ENDAT.
              ENDLOOP.
              APPEND T_IMP TO T_OUT.

*********> Posição I355
              CLEAR: WL_VALOR_I.
              LOOP AT TL_FAGLFLEXT_155_24000.
                READ TABLE TL_SPERIO_E
                  WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                           PERIO = '1'.
                IF SY-SUBRC IS INITIAL.
                  READ TABLE T_CONTAS
                    WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                             PERIO = '1'.
                  IF SY-SUBRC IS NOT INITIAL.
                    WL_VALOR_I = TL_SPERIO_E-DPERIO - TL_SPERIO_E-CPERIO.
                    MOVE: WL_VALOR_I TO WL_VALOR_AUX.
                    TRANSLATE WL_VALOR_AUX USING '.,'.
                    CONDENSE WL_VALOR_AUX NO-GAPS.
                    IF WL_VALOR_I GE 0.
                      CONCATENATE '|I355|' TL_SPERIO_E-SAKNR '||' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
                    ELSE.
                      MULTIPLY WL_VALOR_I BY -1.
                      MOVE: WL_VALOR_I TO WL_VALOR_AUX.
                      TRANSLATE WL_VALOR_AUX USING '.,'.
                      CONDENSE WL_VALOR_AUX NO-GAPS.
                      CONCATENATE '|I355|' TL_SPERIO_E-SAKNR '||' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
                    ENDIF.
                    IF WL_VALOR_I IS NOT INITIAL.
                      APPEND T_OUT.
                    ENDIF.
                    MOVE : TL_SPERIO_E-SAKNR TO T_CONTAS-SAKNR,
                           '1'               TO T_CONTAS-PERIO.

                    APPEND T_CONTAS.
                  ENDIF.
                ENDIF.
                CLEAR: T_OUT, WL_VALOR_I, WL_VALOR_AUX, TL_SPERIO_E, T_CONTAS.
              ENDLOOP.

****** Segundo Trimeste
              CONCATENATE S_GJAHR-LOW '06' '01' INTO WL_DATA_AUX.
              CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
                EXPORTING
                  DAY_IN            = WL_DATA_AUX
                IMPORTING
                  LAST_DAY_OF_MONTH = WL_DATA_AUX2
                EXCEPTIONS
                  DAY_IN_NOT_VALID  = 1
                  OTHERS            = 2.

              CONCATENATE WL_DATA_AUX2+6(2) WL_DATA_AUX2+4(2) WL_DATA_AUX2(4) INTO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 2.
              LOOP AT T_SPLIT.
                CONDENSE T_SPLIT-CAMPO NO-GAPS.
                IF SY-TABIX EQ 1.
                  CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ELSE.
                  CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ENDIF.
                AT LAST.
                  CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                ENDAT.
              ENDLOOP.
              APPEND T_IMP TO T_OUT.

*********> Posição I355
              CLEAR: WL_VALOR_I.
              LOOP AT TL_FAGLFLEXT_155_24000.
                READ TABLE TL_SPERIO_E
                  WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                           PERIO = '2'.
                IF SY-SUBRC IS INITIAL.
                  READ TABLE T_CONTAS
                    WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                             PERIO = '2'.
                  IF SY-SUBRC IS NOT INITIAL.
                    WL_VALOR_I = TL_SPERIO_E-DPERIO - TL_SPERIO_E-CPERIO.
                    MOVE: WL_VALOR_I TO WL_VALOR_AUX.
                    TRANSLATE WL_VALOR_AUX USING '.,'.
                    CONDENSE WL_VALOR_AUX NO-GAPS.
                    IF WL_VALOR_I GE 0.
                      CONCATENATE '|I355|' TL_SPERIO_E-SAKNR '||' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
                    ELSE.

                      MULTIPLY WL_VALOR_I BY -1.
                      MOVE: WL_VALOR_I TO WL_VALOR_AUX.
                      TRANSLATE WL_VALOR_AUX USING '.,'.
                      CONDENSE WL_VALOR_AUX NO-GAPS.
                      CONCATENATE '|I355|' TL_SPERIO_E-SAKNR '||' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
                    ENDIF.
                    IF WL_VALOR_I IS NOT INITIAL.
                      APPEND T_OUT.
                    ENDIF.
                    MOVE : TL_SPERIO_E-SAKNR TO T_CONTAS-SAKNR,
                           '2'               TO T_CONTAS-PERIO.
                    APPEND T_CONTAS.
                  ENDIF.
                ENDIF.
                CLEAR: T_OUT, WL_VALOR_I, WL_VALOR_AUX, TL_SPERIO_E, T_CONTAS.
              ENDLOOP.

****** Terceiro Trimeste
              CONCATENATE S_GJAHR-LOW '09' '01' INTO WL_DATA_AUX.
              CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
                EXPORTING
                  DAY_IN            = WL_DATA_AUX
                IMPORTING
                  LAST_DAY_OF_MONTH = WL_DATA_AUX2
                EXCEPTIONS
                  DAY_IN_NOT_VALID  = 1
                  OTHERS            = 2.

              CONCATENATE WL_DATA_AUX2+6(2) WL_DATA_AUX2+4(2) WL_DATA_AUX2(4) INTO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 2.
              LOOP AT T_SPLIT.
                CONDENSE T_SPLIT-CAMPO NO-GAPS.
                IF SY-TABIX EQ 1.
                  CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ELSE.
                  CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ENDIF.
                AT LAST.
                  CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                ENDAT.
              ENDLOOP.
              APPEND T_IMP TO T_OUT.
*********> Posição I355
              CLEAR: WL_VALOR_I.
              LOOP AT TL_FAGLFLEXT_155_24000.
                READ TABLE TL_SPERIO_E
                  WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                           PERIO = '3'.
                IF SY-SUBRC IS INITIAL.
                  READ TABLE T_CONTAS
                    WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                             PERIO = '3'.
                  IF SY-SUBRC IS NOT INITIAL.
                    WL_VALOR_I = TL_SPERIO_E-DPERIO - TL_SPERIO_E-CPERIO.
                    MOVE: WL_VALOR_I TO WL_VALOR_AUX.
                    TRANSLATE WL_VALOR_AUX USING '.,'.
                    CONDENSE WL_VALOR_AUX NO-GAPS.
                    IF WL_VALOR_I GE 0.
                      CONCATENATE '|I355|' TL_SPERIO_E-SAKNR '||' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
                    ELSE.
                      MULTIPLY WL_VALOR_I BY -1.
                      MOVE: WL_VALOR_I TO WL_VALOR_AUX.
                      TRANSLATE WL_VALOR_AUX USING '.,'.
                      CONDENSE WL_VALOR_AUX NO-GAPS.
                      CONCATENATE '|I355|' TL_SPERIO_E-SAKNR '||' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
                    ENDIF.
                    IF WL_VALOR_I IS NOT INITIAL.
                      APPEND T_OUT.
                    ENDIF.
                    MOVE : TL_SPERIO_E-SAKNR TO T_CONTAS-SAKNR,
                           '3'               TO T_CONTAS-PERIO.
                    APPEND T_CONTAS.
                  ENDIF.
                ENDIF.
                CLEAR: T_OUT, WL_VALOR_I, WL_VALOR_AUX, TL_SPERIO_E, T_CONTAS.
              ENDLOOP.

****** Quarto Trimeste
              CONCATENATE S_GJAHR-LOW '12' '01' INTO WL_DATA_AUX.
              CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
                EXPORTING
                  DAY_IN            = WL_DATA_AUX
                IMPORTING
                  LAST_DAY_OF_MONTH = WL_DATA_AUX2
                EXCEPTIONS
                  DAY_IN_NOT_VALID  = 1
                  OTHERS            = 2.

              CONCATENATE WL_DATA_AUX2+6(2) WL_DATA_AUX2+4(2) WL_DATA_AUX2(4) INTO T_SPLIT-CAMPO.
              MODIFY T_SPLIT INDEX 2.
              LOOP AT T_SPLIT.
                CONDENSE T_SPLIT-CAMPO NO-GAPS.
                IF SY-TABIX EQ 1.
                  CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ELSE.
                  CONCATENATE T_IMP-FIELD '|' T_SPLIT-CAMPO INTO T_IMP-FIELD.
                ENDIF.
                AT LAST.
                  CONCATENATE T_IMP-FIELD '|' INTO T_IMP-FIELD.
                ENDAT.
              ENDLOOP.
              APPEND T_IMP TO T_OUT.
*********> Posição I355
              CLEAR: WL_VALOR_I.
              LOOP AT TL_FAGLFLEXT_155_24000.
                READ TABLE TL_SPERIO_E
                  WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                           PERIO = '4'.
                IF SY-SUBRC IS INITIAL.
                  READ TABLE T_CONTAS
                    WITH KEY SAKNR = TL_FAGLFLEXT_155_24000-RACCT
                             PERIO = '4'.
                  IF SY-SUBRC IS NOT INITIAL.
                    WL_VALOR_I = TL_SPERIO_E-DPERIO - TL_SPERIO_E-CPERIO.
                    MOVE: WL_VALOR_I TO WL_VALOR_AUX.
                    TRANSLATE WL_VALOR_AUX USING '.,'.
                    CONDENSE WL_VALOR_AUX NO-GAPS.
                    IF WL_VALOR_I GE 0.
                      CONCATENATE '|I355|' TL_SPERIO_E-SAKNR '||' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
                    ELSE.
                      MULTIPLY WL_VALOR_I BY -1.
                      MOVE: WL_VALOR_I TO WL_VALOR_AUX.
                      TRANSLATE WL_VALOR_AUX USING '.,'.
                      CONDENSE WL_VALOR_AUX NO-GAPS.
                      CONCATENATE '|I355|' TL_SPERIO_E-SAKNR '||' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
                    ENDIF.
                    IF WL_VALOR_I IS NOT INITIAL.
                      APPEND T_OUT.
                    ENDIF.
                    MOVE : TL_SPERIO_E-SAKNR TO T_CONTAS-SAKNR,
                           '4'               TO T_CONTAS-PERIO.
                    APPEND T_CONTAS.
                  ENDIF.
                ENDIF.
                CLEAR: T_OUT, WL_VALOR_I, WL_VALOR_AUX, TL_SPERIO_E, T_CONTAS.
              ENDLOOP.

          ENDCASE.

        ELSEIF T_SPLIT-CAMPO EQ 'I355'.
          IF P_ANUAL IS NOT INITIAL.
            APPEND T_IMP TO T_OUT.
****> Pega contas para selecionar em tabela FAGLFLEXT
            CLEAR: T_SPLIT, T_CONTAS.
            READ TABLE T_SPLIT INDEX 2.
            IF T_SPLIT-CAMPO IS NOT  INITIAL.
              MOVE: T_SPLIT-CAMPO TO T_CONTAS-SAKNR.
              APPEND T_CONTAS.
              CLEAR T_CONTAS.
            ENDIF.
          ENDIF.
*******************************************************************
        ELSEIF T_SPLIT-CAMPO EQ 'J930'.
          IF 'J930' IS NOT INITIAL.
            LOOP AT T_SPLIT.
              IF SY-TABIX LE 6.
                  IF  SY-TABIX  EQ 1 .
                    CONCATENATE '|'  T_SPLIT-CAMPO INTO T_IMP-FIELD.
                  ELSE.
                    CONCATENATE T_IMP-FIELD T_SPLIT-CAMPO  INTO T_IMP-FIELD SEPARATED BY '|'.
                  ENDIF.
                ENDIF.

            ENDLOOP.
             CONCATENATE  T_IMP-FIELD '|'  INTO T_IMP-FIELD.
          ENDIF.

          APPEND T_IMP TO T_OUT.
******************************************************************************
        ELSEIF T_SPLIT-CAMPO EQ 'J990'.
          APPEND T_IMP TO T_OUT.
****> Posição M001
          CLEAR: T_OUT.
          MOVE: '|M001|0|' TO T_OUT-FIELD.
          APPEND T_OUT.
****> Posição M020
          CASE 'X'.
            WHEN P_ANUAL.
******> Anual
              CLEAR: T_OUT.
*              move: '|M020|10|0||||0|A00|1|||' to t_out-field.
              MOVE: '|M020|10|0||||0|A|1|||' TO T_OUT-FIELD.
              APPEND T_OUT.
            WHEN P_TRIM.
******> 1º Trimestre
              CLEAR: T_OUT.
*              move: '|M020|10|0||||0|T01|1|||' to t_out-field.
              MOVE: '|M020|10|0||||0|T|1|||' TO T_OUT-FIELD.
              APPEND T_OUT.
*******> 2º Trimestre
*              clear: t_out.
*              move: '|M020|10|0||||0|T02|1|||' to t_out-field.
*              append t_out.
*******> 3º Trimestre
*              clear: t_out.
*              move: '|M020|10|0||||0|T03|1|||' to t_out-field.
*              append t_out.
*******> 4º Trimestre
*              clear: t_out.
*              move: '|M020|10|0||||0|T04|1|||' to t_out-field.
*              append t_out.
          ENDCASE.

****> Posição M030
          IF T_FAGLFLEXT[] IS INITIAL.
            IF T_CONTAS[] IS NOT INITIAL.
              SELECT RACCT DRCRK HSLVT
                     HSL01 HSL02 HSL03 HSL04 HSL05
                     HSL06 HSL07 HSL08 HSL09 HSL10
                     HSL11 HSL12 HSL13 HSL14 HSL15 HSL16
                FROM FAGLFLEXT
                INTO TABLE T_FAGLFLEXT
                 FOR ALL ENTRIES IN T_CONTAS
                 WHERE RYEAR IN S_GJAHR
                   AND RTCUR EQ 'BRL'
                   AND RLDNR EQ '0L'
                   AND RACCT EQ T_CONTAS-SAKNR
                   AND RBUKRS IN S_BUKRS.

            ENDIF.
          ENDIF.
          CASE 'X'.
            WHEN P_ANUAL.
              CLEAR: WL_VALOR_A.
              LOOP AT T_FAGLFLEXT.
                ADD T_FAGLFLEXT-HSL01 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL02 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL03 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL04 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL05 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL06 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL07 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL08 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL09 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL10 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL11 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL12 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL13 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL14 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL15 TO WL_VALOR_A.
                ADD T_FAGLFLEXT-HSL16 TO WL_VALOR_A.

              ENDLOOP.
******> Anual
              CLEAR: T_OUT, WL_VALOR_AUX, WL_VALOR_A.
              MOVE WL_VALOR_A TO WL_VALOR_AUX.
              TRANSLATE WL_VALOR_AUX USING ', '.
              TRANSLATE WL_VALOR_AUX USING '.,'.
              CONDENSE WL_VALOR_AUX NO-GAPS.
              IF WL_VALOR_A GE 0.
                CONCATENATE '|M030|A00|' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
              ELSE.
                MULTIPLY WL_VALOR_A BY -1.
                MOVE WL_VALOR_A TO WL_VALOR_AUX.
                TRANSLATE WL_VALOR_AUX USING ', '.
                TRANSLATE WL_VALOR_AUX USING '.,'.
                CONDENSE WL_VALOR_AUX NO-GAPS.
                CONCATENATE '|M030|A00|' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
              ENDIF.
              APPEND T_OUT.
            WHEN P_TRIM.
******> 1º Trimestre
              CLEAR: WL_VALOR_1.
              LOOP AT T_FAGLFLEXT.
                ADD T_FAGLFLEXT-HSL01 TO WL_VALOR_1.
                ADD T_FAGLFLEXT-HSL02 TO WL_VALOR_1.
                ADD T_FAGLFLEXT-HSL03 TO WL_VALOR_1.

              ENDLOOP.
              CLEAR: T_OUT, WL_VALOR_AUX, WL_VALOR_1.
              MOVE WL_VALOR_1 TO WL_VALOR_AUX.
              TRANSLATE WL_VALOR_AUX USING ', '.
              TRANSLATE WL_VALOR_AUX USING '.,'.
              CONDENSE WL_VALOR_AUX NO-GAPS.

              IF WL_VALOR_1 GE 0.
                CONCATENATE '|M030|T01|' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
              ELSE.
                MULTIPLY WL_VALOR_1 BY -1.
                MOVE WL_VALOR_A TO WL_VALOR_AUX.
                TRANSLATE WL_VALOR_AUX USING ', '.
                TRANSLATE WL_VALOR_AUX USING '.,'.
                CONDENSE WL_VALOR_AUX NO-GAPS.
                CONCATENATE '|M030|T01|' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
              ENDIF.
              APPEND T_OUT.
******> 2º Trimestre
              CLEAR: WL_VALOR_2.
              LOOP AT T_FAGLFLEXT.
                ADD T_FAGLFLEXT-HSL04 TO WL_VALOR_2.
                ADD T_FAGLFLEXT-HSL05 TO WL_VALOR_2.
                ADD T_FAGLFLEXT-HSL06 TO WL_VALOR_2.

              ENDLOOP.
              CLEAR: T_OUT, WL_VALOR_AUX, WL_VALOR_2.
              MOVE WL_VALOR_2 TO WL_VALOR_AUX.
              TRANSLATE WL_VALOR_AUX USING ', '.
              TRANSLATE WL_VALOR_AUX USING '.,'.
              CONDENSE WL_VALOR_AUX NO-GAPS.

              IF WL_VALOR_2 GE 0.
                CONCATENATE '|M030|T02|' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
              ELSE.
                MULTIPLY WL_VALOR_2 BY -1.
                MOVE WL_VALOR_A TO WL_VALOR_AUX.
                TRANSLATE WL_VALOR_AUX USING ', '.
                TRANSLATE WL_VALOR_AUX USING '.,'.
                CONDENSE WL_VALOR_AUX NO-GAPS.
                CONCATENATE '|M030|T02|' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
              ENDIF.
              APPEND T_OUT.
******> 3º Trimestre
              CLEAR: WL_VALOR_3.
              LOOP AT T_FAGLFLEXT.
                ADD T_FAGLFLEXT-HSL07 TO WL_VALOR_3.
                ADD T_FAGLFLEXT-HSL08 TO WL_VALOR_3.
                ADD T_FAGLFLEXT-HSL09 TO WL_VALOR_3.

              ENDLOOP.
              CLEAR: T_OUT, WL_VALOR_AUX, WL_VALOR_3.
              MOVE WL_VALOR_3 TO WL_VALOR_AUX.
              TRANSLATE WL_VALOR_AUX USING ', '.
              TRANSLATE WL_VALOR_AUX USING '.,'.
              CONDENSE WL_VALOR_AUX NO-GAPS.
              IF WL_VALOR_3 GE 0.
                CONCATENATE '|M030|T03|' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
              ELSE.
                MULTIPLY WL_VALOR_3 BY -1.
                MOVE WL_VALOR_A TO WL_VALOR_AUX.
                TRANSLATE WL_VALOR_AUX USING ', '.
                TRANSLATE WL_VALOR_AUX USING '.,'.
                CONDENSE WL_VALOR_AUX NO-GAPS.
                CONCATENATE '|M030|T03|' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
              ENDIF.
              APPEND T_OUT.
******> 4º Trimestre
              CLEAR: WL_VALOR_4.
              LOOP AT T_FAGLFLEXT.
                ADD T_FAGLFLEXT-HSL10 TO WL_VALOR_4.
                ADD T_FAGLFLEXT-HSL11 TO WL_VALOR_4.
                ADD T_FAGLFLEXT-HSL12 TO WL_VALOR_4.
                ADD T_FAGLFLEXT-HSL13 TO WL_VALOR_4.
                ADD T_FAGLFLEXT-HSL14 TO WL_VALOR_4.
                ADD T_FAGLFLEXT-HSL15 TO WL_VALOR_4.

              ENDLOOP.
              CLEAR: T_OUT, WL_VALOR_AUX, WL_VALOR_4.
              MOVE WL_VALOR_4 TO WL_VALOR_AUX.
              TRANSLATE WL_VALOR_AUX USING ', '.
              TRANSLATE WL_VALOR_AUX USING '.,'.
              CONDENSE WL_VALOR_AUX NO-GAPS.

              IF WL_VALOR_4 GE 0.
                CONCATENATE '|M030|T04|' WL_VALOR_AUX '|D|' INTO T_OUT-FIELD.
              ELSE.
                MULTIPLY WL_VALOR_4 BY -1.
                MOVE WL_VALOR_A TO WL_VALOR_AUX.
                TRANSLATE WL_VALOR_AUX USING ', '.
                TRANSLATE WL_VALOR_AUX USING '.,'.
                CONDENSE WL_VALOR_AUX NO-GAPS.
                CONCATENATE '|M030|T04|' WL_VALOR_AUX '|C|' INTO T_OUT-FIELD.
              ENDIF.
              APPEND T_OUT.
          ENDCASE.

****> Posição M990
          CLEAR: T_OUT.
          MOVE: '|M990|0|' TO T_OUT-FIELD.
          APPEND T_OUT.

****> Posição 9001
*          CLEAR: T_OUT.
*          MOVE: '|9001|0|' TO T_OUT-FIELD.
*          APPEND T_OUT.

****> Posição 9990
*          CLEAR: T_OUT.
*          MOVE: '|9990|0|' TO T_OUT-FIELD.
*          APPEND T_OUT.
****> Posição 9900
        ELSEIF T_SPLIT-CAMPO EQ '9900'
            OR T_SPLIT-CAMPO EQ '9001'
            OR T_SPLIT-CAMPO EQ '9990'
            OR T_SPLIT-CAMPO EQ '9999'.
*****> Não faz nada
        ELSE.
          APPEND T_IMP TO T_OUT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

***> Posição M990
  REFRESH: TL_OUT_AUX, T_SPLIT.
  CLEAR: WL_LINHAS.
  TL_OUT_AUX[] = T_OUT[].

  DELETE TL_OUT_AUX WHERE FIELD+1(1) NE 'M'.
  DELETE TL_OUT_AUX WHERE FIELD+1(4) EQ 'M990'.
  DESCRIBE TABLE TL_OUT_AUX LINES WL_LINHAS.
  READ TABLE T_OUT
   WITH KEY FIELD+1(4) = 'M990'.
  IF SY-SUBRC IS INITIAL.
    WL_TABIX = SY-TABIX.
    SPLIT T_OUT AT '|' INTO TABLE T_SPLIT.
    DELETE T_SPLIT INDEX 1.
    IF T_SPLIT[] IS NOT INITIAL.
      READ TABLE T_SPLIT INDEX 1.
      WL_LINHAS_AUX = WL_LINHAS + 1.
      CONDENSE WL_LINHAS_AUX NO-GAPS.
      CONCATENATE '|' T_SPLIT-CAMPO '|' WL_LINHAS_AUX '|' INTO T_OUT-FIELD.

      MODIFY T_OUT INDEX WL_TABIX.
    ENDIF.
  ENDIF.
*
****> Posição 9900
*  CLEAR: WL_CONT, WL_CONT.
*  DATA: VAR_FIELD TYPE C LENGTH 4.
*
*  T_OUT_AUX[] = T_OUT[].
*
*
*  LOOP AT T_OUT.
*    CLEAR: VAR_FIELD.
*    READ TABLE T_OUT TRANSPORTING NO FIELDS
*      WITH KEY FIELD+1(4)  = '9900'
*               FIELD+6(4)  = T_OUT-FIELD+1(4).
*    VAR_FIELD = T_OUT-FIELD+1(4).
*    IF SY-SUBRC IS NOT INITIAL.
*
*      "IF T_OUT-FIELD+1(4) NE '9900' AND T_OUT-FIELD+1(4) NE '9990'.
*      IF T_OUT-FIELD+1(4) NE '9900' OR T_OUT-FIELD+1(4) NE '9990'.
*
*        ADD 1 TO WL_CONT.
*        TL_OUT_AUX[] = T_OUT[].
*        DELETE TL_OUT_AUX WHERE FIELD+1(4) NE T_OUT-FIELD+1(4).
*        DESCRIBE TABLE TL_OUT_AUX LINES WL_LINHAS.
*        WL_LINHAS_AUX = WL_LINHAS.
*        CONDENSE WL_LINHAS_AUX NO-GAPS.
*        CONCATENATE '|9900|' T_OUT-FIELD+1(4) '|' WL_LINHAS_AUX '|' INTO T_OUT-FIELD.
*        READ TABLE T_OUT TRANSPORTING NO FIELDS
*          WITH KEY FIELD+1(4) = '9001'.
*        IF SY-SUBRC IS INITIAL.
*
*          WL_TABIX = SY-TABIX + WL_CONT.
*          INSERT T_OUT INDEX WL_TABIX.
*          IF ( SY-SUBRC EQ 0 ).
*            DELETE T_OUT WHERE FIELD+1(4) EQ VAR_FIELD.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
*  T_OUT[] = T_OUT_AUX[].
*
*****> Posição 9900 -> 9900
*  WL_LINHAS_AUX = WL_CONT + 3.
*  CONDENSE WL_LINHAS_AUX NO-GAPS.
*  CONCATENATE '|9900|9900|' WL_LINHAS_AUX '|' INTO T_OUT-FIELD.
*  READ TABLE T_OUT TRANSPORTING NO FIELDS
*    WITH KEY FIELD+1(4) = '9001'.
*  IF SY-SUBRC IS INITIAL.
*    ADD 1 TO WL_CONT.
*    WL_TABIX = SY-TABIX + WL_CONT.
*    INSERT T_OUT INDEX WL_TABIX.
*  ENDIF.
*
*****> Posição 9900 -> 9990
*  MOVE: '|9900|9990|1|' TO T_OUT-FIELD.
*  READ TABLE T_OUT TRANSPORTING NO FIELDS
*    WITH KEY FIELD+1(4) = '9001'.
*  IF SY-SUBRC IS INITIAL.
*    ADD 1 TO WL_CONT.
*    WL_TABIX = SY-TABIX + WL_CONT.
*    INSERT T_OUT INDEX WL_TABIX.
*  ENDIF.
*
*****> Posição 9900 -> 9999
*  MOVE: '|9900|9999|1|' TO T_OUT-FIELD.
*  READ TABLE T_OUT TRANSPORTING NO FIELDS
*    WITH KEY FIELD+1(4) = '9001'.
*  IF SY-SUBRC IS INITIAL.
*    ADD 1 TO WL_CONT.
*    WL_TABIX = SY-TABIX + WL_CONT.
*    INSERT T_OUT INDEX WL_TABIX.
*  ENDIF.
*
****> Posição 9990
*  REFRESH: TL_OUT_AUX, T_SPLIT.
*  CLEAR: WL_LINHAS.
*  TL_OUT_AUX[] = T_OUT[].
*
*  DELETE TL_OUT_AUX WHERE FIELD+1(1) NE '9'.
*  DESCRIBE TABLE TL_OUT_AUX LINES WL_LINHAS.
*  READ TABLE T_OUT
*   WITH KEY FIELD+1(4) = '9990'.
*  IF SY-SUBRC IS INITIAL.
*    WL_TABIX = SY-TABIX.
*    SPLIT T_OUT AT '|' INTO TABLE T_SPLIT.
*    DELETE T_SPLIT INDEX 1.
*    IF T_SPLIT[] IS NOT INITIAL.
*      READ TABLE T_SPLIT INDEX 1.
*      WL_LINHAS_AUX = WL_LINHAS + 1.
*      CONDENSE WL_LINHAS_AUX NO-GAPS.
*      CONCATENATE '|' T_SPLIT-CAMPO '|' WL_LINHAS_AUX '|' INTO T_OUT-FIELD.
*
*      MODIFY T_OUT INDEX WL_TABIX.
*    ENDIF.
*  ENDIF.

***> Posição 9999
*  DESCRIBE TABLE T_OUT LINES WL_LINHAS.
*  WL_LINHAS_AUX = WL_LINHAS + 1.
*  CONDENSE WL_LINHAS_AUX NO-GAPS.
*  CONCATENATE '|9999|' WL_LINHAS_AUX '|' INTO T_OUT-FIELD.
*  APPEND T_OUT.
ENDFORM.                    " CONVERTE_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  EXPORTA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXPORTA_ARQUIVO .

  IF T_OUT[] IS NOT INITIAL.
    X_FILENAME = P_F_OUT.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME = X_FILENAME
      TABLES
        DATA_TAB = T_OUT
      EXCEPTIONS
        OTHERS   = 22.
    IF SY-SUBRC <> 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " EXPORTA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  LER_TXT_UNIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LER_TXT_UNIX TABLES TL_IMP
                  USING P_FILE.

  DATA: WL_IN_I200.
*  x_filename = p_f_in.
  X_FILENAME = P_FILE.

  PERFORM ABRIR_ARQUIVO_UNIX USING X_FILENAME
                                   'IN'.

  DO.
    READ DATASET X_FILENAME INTO TL_IMP.
    IF SY-SUBRC = 0.
      IF P_BUSCA IS NOT INITIAL.
        IF TL_IMP+1(4) EQ 'I200'.
          IF WL_IN_I200 IS NOT INITIAL.
            CLEAR TL_IMP.
            CONTINUE.
          ELSE.
            WL_IN_I200 = 'X'.
          ENDIF.
        ENDIF.

        IF TL_IMP+1(4) EQ 'I250'.
*        OR TL_IMP+1(4) EQ 'I200'.
          CLEAR TL_IMP.
          CONTINUE.
        ENDIF.
      ENDIF.
      APPEND TL_IMP.
      CLEAR: TL_IMP.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  PERFORM FECHAR_ARQUIVO_UNIX.
ENDFORM.                    " LER_TXT_UNIX
*-----------------------------------------------------------------------
* Form  FECHAR_ARQUIVO_UNIX
*-----------------------------------------------------------------------
FORM FECHAR_ARQUIVO_UNIX.

  CLOSE DATASET X_FILENAME.

  IF SY-SUBRC NE 0.
    WRITE: / 'Problemas no close dataset coletor', SY-SUBRC.
  ELSE.
*    message e836(sd) with 'Arquivo lido no UNIX'.
  ENDIF.

ENDFORM.                    "FECHAR_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_MODIFICA_TELA.
*  LOOP AT SCREEN.
*    IF SCREEN-NAME EQ 'P_BUTXT'
*    OR SCREEN-NAME EQ 'P_NAME'
*    OR SCREEN-NAME EQ 'P_NAME2'
*    OR SCREEN-NAME EQ 'P_VIVO'.
*
*if p
*      SCREEN-INPUT = 0.
**      IF SCREEN-NAME EQ 'P_VIVO'.
**        SCREEN-INVISIBLE = 1.
**      ENDIF.
*      MODIFY SCREEN.
*
*    ENDIF.
*
*  ENDLOOP.
*  if sy-ucomm eq 'ASD'.
*  case 'X'.
*    when p_unix.
*      if ok-code ne 'UNIX'.
*        move '/usr/interfaces/fcont_in/' to p_f_in.
*        move '/usr/interfaces/fcont_out/' to p_f_out.
*        ok-code = 'UNIX'.
*      endif.
*    when p_local.
*      if ok-code ne 'LOCAL'.
*        move 'C:\' to p_f_in.
*        move 'C:\' to p_f_out.
*        ok-code = 'LOCAL'.
*      endif.
*  endcase.
*  endif.
ENDFORM.                    " F_MODIFICA_TELA
*-----------------------------------------------------------------------
* Form  ABRIR_ARQUIVO_UNIX
*-----------------------------------------------------------------------
FORM ABRIR_ARQUIVO_UNIX USING P_FILE
                              WL_TYPE .
*data: wl_file_local(255) type c.
*  concatenate:  p_file  '.txt' into x_filename in character mode .    "smart: 11/01/10 E101
  MOVE P_FILE TO X_FILENAME.
*wl_file_local = '/usr/interfaces/dre/TESTE4.TXT'.
  TRANSLATE:    X_FILENAME     TO   LOWER CASE.

*condense wl_file_local no-gaps.
  IF WL_TYPE EQ 'IN'.
    OPEN DATASET  X_FILENAME FOR INPUT IN TEXT MODE MESSAGE X_MSGERRO   "smart: 11/01/10 E111
      ENCODING NON-UNICODE WITH WINDOWS LINEFEED.                                                "smart: 11/01/10 E111
  ELSE.
    OPEN DATASET  X_FILENAME FOR OUTPUT IN TEXT MODE MESSAGE X_MSGERRO   "smart: 11/01/10 E111
      ENCODING NON-UNICODE WITH WINDOWS LINEFEED.
  ENDIF.
*open dataset  wl_file_local for input in text mode "message x_msgerro
*                              encoding default
*                              with smart linefeed.
  IF SY-SUBRC NE 0.
    MESSAGE E836(SD) WITH P_FILE X_MSGERRO SY-SUBRC.
  ENDIF.

ENDFORM.                    "ABRIR_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*&      Form  EXPORTA_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXPORTA_ARQUIVO_UNIX .
  DATA: WL_CONT TYPE SY-TABIX.
  X_FILENAME = P_F_OUT.
  PERFORM ABRIR_ARQUIVO_UNIX USING X_FILENAME
                                   'OUT'.
  LOOP AT T_OUT.
    CLEAR: WL_CONT.
    WL_CONT =  STRLEN( T_OUT-FIELD ) - 1.
*if t_out-field .
    do.
      if t_out-field+wl_cont(1) eq '|'.
        add 1 to wl_cont.
        translate t_out-field+wl_cont using '# '.
        condense t_out-field+wl_cont no-gaps.
        exit.
      else.
        subtract 1 from wl_cont.
      endif.
    enddo.
    TRANSFER T_OUT-FIELD TO X_FILENAME.
*    endif.
  ENDLOOP.

  CLOSE DATASET X_FILENAME.

  MESSAGE S836(SD) WITH 'O arquivo foi gravado com'
                         'sucesso no servidor!'.

ENDFORM.                    " EXPORTA_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_I200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS_I200 .
  DATA: BEGIN OF TL_DOC_TOTAL OCCURS 0,
         DOCNR TYPE  FAGLFLEXA-DOCNR,
         TOTAL TYPE  FAGLFLEXA-HSL,
        END OF TL_DOC_TOTAL.

  DATA: BEGIN OF TL_LANCAMENTOS OCCURS 0,
          FIELD(1000),
*        flag,
          END OF TL_LANCAMENTOS.

  DATA: TL_FAGLFLEXA TYPE TABLE OF FAGLFLEXA WITH HEADER LINE,
        TL_FAGLFLEXA_AUX TYPE TABLE OF FAGLFLEXA WITH HEADER LINE,
        TL_BSIS TYPE TABLE OF BSIS WITH HEADER LINE,
        TL_BSAS TYPE TABLE OF BSAS WITH HEADER LINE,
        TL_SKA1 TYPE TABLE OF SKA1 WITH HEADER LINE,
        TL_BKPF TYPE TABLE OF BKPF WITH HEADER LINE,
        WL_TOTAL TYPE FAGLFLEXA-HSL,
        WL_VALOR_AUX(20).
  REFRESH: TL_FAGLFLEXA, TL_FAGLFLEXA_AUX, TL_BSIS, TL_BSAS, TL_SKA1, TL_BKPF.

  SELECT *
    FROM FAGLFLEXA
    INTO TABLE TL_FAGLFLEXA
     WHERE RYEAR IN S_GJAHR
       AND RLDNR EQ '0L'
       AND RBUKRS IN S_BUKRS
       AND RACCT IN S_HKONT
       AND POPER NE '16'.

  IF SY-SUBRC IS INITIAL.
    SELECT *
  FROM FAGLFLEXA
  INTO TABLE TL_FAGLFLEXA_AUX
    FOR ALL ENTRIES IN TL_FAGLFLEXA
   WHERE RYEAR IN S_GJAHR
     AND RLDNR EQ '0L'
     AND RBUKRS IN S_BUKRS
     AND DOCNR EQ TL_FAGLFLEXA-DOCNR
*     and racct not in s_250
     AND POPER NE '16'.

    IF S_250[] IS NOT INITIAL.
      DELETE TL_FAGLFLEXA_AUX WHERE RACCT IN S_250.
    ENDIF.

    REFRESH TL_FAGLFLEXA.
    TL_FAGLFLEXA[] = TL_FAGLFLEXA_AUX[].
    REFRESH: TL_FAGLFLEXA_AUX.

    IF TL_FAGLFLEXA[] IS NOT INITIAL.

      SELECT * "#EC CI_DB_OPERATION_OK[2389136]
        FROM SKA1 "#EC CI_DB_OPERATION_OK[2431747]
        INTO TABLE TL_SKA1
         FOR ALL ENTRIES IN TL_FAGLFLEXA
           WHERE KTOPL EQ '0050'
             AND SAKNR EQ TL_FAGLFLEXA-RACCT
             AND KTOKS IN ('YB01','YB02','YB03','YB04').

      SELECT *
        FROM BSIS
        INTO TABLE TL_BSIS
        FOR ALL ENTRIES IN TL_FAGLFLEXA
         WHERE GJAHR EQ TL_FAGLFLEXA-RYEAR
           AND BUKRS EQ TL_FAGLFLEXA-RBUKRS
           AND BELNR EQ TL_FAGLFLEXA-DOCNR
           AND BUZEI EQ TL_FAGLFLEXA-BUZEI.

      SELECT *
      FROM BSAS
      INTO TABLE TL_BSAS
      FOR ALL ENTRIES IN TL_FAGLFLEXA
       WHERE GJAHR EQ TL_FAGLFLEXA-RYEAR
         AND BUKRS EQ TL_FAGLFLEXA-RBUKRS
         AND BELNR EQ TL_FAGLFLEXA-DOCNR
         AND BUZEI EQ TL_FAGLFLEXA-BUZEI.

      SELECT *
        FROM BKPF
        INTO TABLE TL_BKPF
         FOR ALL ENTRIES IN TL_FAGLFLEXA
          WHERE BUKRS EQ TL_FAGLFLEXA-RBUKRS
            AND GJAHR EQ TL_FAGLFLEXA-RYEAR
            AND BELNR EQ TL_FAGLFLEXA-DOCNR.
    ENDIF.
    TL_FAGLFLEXA_AUX[] = TL_FAGLFLEXA[].
    DELETE TL_FAGLFLEXA_AUX WHERE DRCRK NE 'S'.
    DELETE TL_FAGLFLEXA_AUX WHERE  HSL IS INITIAL.

    LOOP AT TL_FAGLFLEXA_AUX.
      MOVE: TL_FAGLFLEXA_AUX-DOCNR TO TL_DOC_TOTAL-DOCNR,
            TL_FAGLFLEXA_AUX-HSL   TO TL_DOC_TOTAL-TOTAL.

      COLLECT TL_DOC_TOTAL.
      CLEAR: TL_DOC_TOTAL.
    ENDLOOP.
    SORT: TL_DOC_TOTAL BY DOCNR,
          TL_BSIS      BY  BUKRS GJAHR BELNR,
          TL_BSAS      BY  BUKRS GJAHR BELNR,
          TL_SKA1      BY  SAKNR,
          TL_BKPF      BY  BUKRS GJAHR BELNR.

    SORT TL_FAGLFLEXA BY DOCNR.
    REFRESH: TL_LANCAMENTOS.
    LOOP AT TL_FAGLFLEXA.
      ON CHANGE OF TL_FAGLFLEXA-DOCNR.
        IF TL_LANCAMENTOS[] IS NOT INITIAL.
*          append lines of tl_lancamentos to t_imp_i200.
          LOOP AT TL_LANCAMENTOS.
            IF TL_LANCAMENTOS-FIELD+1(4) EQ 'I250'.
              READ TABLE TL_SKA1
                WITH KEY SAKNR = TL_LANCAMENTOS-FIELD+6(10)
                         BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                MOVE: '0000244000' TO TL_LANCAMENTOS-FIELD+6(10).
              ENDIF.
              APPEND TL_LANCAMENTOS TO T_IMP_I200.
            ELSEIF TL_LANCAMENTOS-FIELD+1(4) EQ 'I200'.
              APPEND TL_LANCAMENTOS TO T_IMP_I200.

            ENDIF.
            CLEAR: TL_LANCAMENTOS, TL_SKA1.
          ENDLOOP.
          REFRESH: TL_LANCAMENTOS.
        ENDIF.

        CLEAR: WL_TOTAL, TL_DOC_TOTAL.
        READ TABLE TL_DOC_TOTAL
          WITH KEY DOCNR = TL_FAGLFLEXA-DOCNR
                   BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WL_TOTAL = TL_DOC_TOTAL-TOTAL.
        ENDIF.
*      loop at tl_faglflexa_aux
*        where docnr eq tl_faglflexa-docnr.
*        add tl_faglflexa_aux-hsl to wl_total.
*      endloop.
        IF WL_TOTAL IS NOT INITIAL.
          MOVE WL_TOTAL TO WL_VALOR_AUX.
          TRANSLATE WL_VALOR_AUX USING ', '.
          TRANSLATE WL_VALOR_AUX USING '.,'.
          CONDENSE WL_VALOR_AUX NO-GAPS.
          CONCATENATE '|I200|' TL_FAGLFLEXA-DOCNR '|' TL_FAGLFLEXA-BUDAT+6(2) TL_FAGLFLEXA-BUDAT+4(2) TL_FAGLFLEXA-BUDAT(4) '|' WL_VALOR_AUX '|X|'
          INTO T_IMP_I200-FIELD.
          CONCATENATE '|I200|' TL_FAGLFLEXA-DOCNR '|' TL_FAGLFLEXA-BUDAT+6(2) TL_FAGLFLEXA-BUDAT+4(2) TL_FAGLFLEXA-BUDAT(4) '|' WL_VALOR_AUX '|EF|'
          INTO TL_LANCAMENTOS-FIELD.

          APPEND T_IMP_I200.
          APPEND TL_LANCAMENTOS.
        ENDIF.
        CLEAR: T_IMP_I200, TL_LANCAMENTOS.
      ENDON.

      IF WL_TOTAL IS NOT INITIAL.
        IF TL_FAGLFLEXA-HSL LT 0.
          MULTIPLY TL_FAGLFLEXA-HSL BY -1.
        ENDIF.
        READ TABLE TL_BSIS
          WITH KEY BUKRS = TL_FAGLFLEXA-RBUKRS
                   GJAHR = TL_FAGLFLEXA-RYEAR
                   BELNR = TL_FAGLFLEXA-DOCNR
                   BINARY SEARCH.
        IF SY-SUBRC IS NOT INITIAL.
          READ TABLE TL_BSAS INTO TL_BSIS
          WITH KEY BUKRS = TL_FAGLFLEXA-RBUKRS
                   GJAHR = TL_FAGLFLEXA-RYEAR
                   BELNR = TL_FAGLFLEXA-DOCNR
                   BINARY SEARCH.
        ENDIF.

        READ TABLE TL_BKPF
          WITH KEY BUKRS = TL_FAGLFLEXA-RBUKRS
                   GJAHR = TL_FAGLFLEXA-RYEAR
                   BELNR = TL_FAGLFLEXA-DOCNR
                   BINARY SEARCH.

        IF TL_FAGLFLEXA-HSL IS NOT INITIAL.
          MOVE TL_FAGLFLEXA-HSL TO WL_VALOR_AUX.
          TRANSLATE WL_VALOR_AUX USING ', '.
          TRANSLATE WL_VALOR_AUX USING '.,'.
          CONDENSE WL_VALOR_AUX NO-GAPS.
          IF TL_FAGLFLEXA-DRCRK EQ 'H'.
            CONCATENATE '|I250|' TL_FAGLFLEXA-RACCT '||' WL_VALOR_AUX '|C|' TL_BKPF-XBLNR '||' TL_BSIS-SGTXT '||' INTO T_IMP_I200-FIELD.
          ELSE.
            CONCATENATE '|I250|' TL_FAGLFLEXA-RACCT '||' WL_VALOR_AUX '|D|' TL_BKPF-XBLNR '||' TL_BSIS-SGTXT '||' INTO T_IMP_I200-FIELD.
          ENDIF.
          APPEND T_IMP_I200.
          APPEND T_IMP_I200 TO TL_LANCAMENTOS.
        ENDIF.
      ENDIF.
      CLEAR: T_IMP_I200, TL_BKPF.

      AT LAST.
        IF TL_LANCAMENTOS[] IS NOT INITIAL.
*          append lines of tl_lancamentos to t_imp_i200.
          LOOP AT TL_LANCAMENTOS.
            IF TL_LANCAMENTOS-FIELD+1(4) EQ 'I250'.
              READ TABLE TL_SKA1
                WITH KEY SAKNR = TL_LANCAMENTOS-FIELD+6(10)
                         BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                MOVE: '0000244000' TO TL_LANCAMENTOS-FIELD+6(10).
              ENDIF.
              APPEND TL_LANCAMENTOS TO T_IMP_I200.
            ELSEIF TL_LANCAMENTOS-FIELD+1(4) EQ 'I200'.
              APPEND TL_LANCAMENTOS TO T_IMP_I200.

            ENDIF.
            CLEAR: TL_LANCAMENTOS, TL_SKA1.
          ENDLOOP.
          REFRESH: TL_LANCAMENTOS.
        ENDIF.
      ENDAT.
    ENDLOOP.
  ENDIF.

*  LOOP AT t_imp_i200
*    WHERE
ENDFORM.                    " SELECIONA_DADOS_I200
