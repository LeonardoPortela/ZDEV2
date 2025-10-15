************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 16.07.2014                                          *
* Objetivo    ...: Movimento Diário - Embarque                         *
* Transação   ...: ZLES0104                                            *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 16.07.2014   Camila Brand          Criação              DEVK938799   *
************************************************************************
REPORT  ZLESR0085.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
TABLES: VBFA, LIPS,VBAK,VBPA,LFA1,KNA1,T001W,MARA,MAKT,T023,ZSDT0001.

*&--------------------------------------------------------------------&*
*&  ESTRUTURA
*&--------------------------------------------------------------------&*
*DATA: wa_return LIKE zmme_return_ordem_servico,
*      it_return LIKE STANDARD TABLE OF wa_return.

*&---------------------------------------------------------------------*
*& Tipos
*&---------------------------------------------------------------------*
TYPES:
   BEGIN OF TY_ZLEST0095,
     ERDAT            TYPE VBFA-ERDAT,
     VBELN            TYPE LIPS-VBELN,
     MATNR            TYPE LIPS-MATNR,
     MATKL            TYPE LIPS-MATKL,
     WERKS            TYPE LIPS-WERKS,
     LFIMG            TYPE LIPS-LFIMG,
     VBELV            TYPE VBFA-VBELV ,
     AUART            TYPE VBAK-AUART,
     COD_CLIENTE      TYPE VBAK-KUNNR,
     COD_TRANSBORDO   TYPE VBPA-KUNNR,
     COD_PORTO        TYPE VBPA-LIFNR,
     NR_ROMANEIO      TYPE ZSDT0001-NR_ROMANEIO,
     TP_TRANSGENIA    TYPE ZSDT0001-TP_TRANSGENIA,
   END   OF TY_ZLEST0095,

BEGIN OF TY_ZLEST0096,
      ERDAT            TYPE ZLEST0095-ERDAT,
      WERKS            TYPE ZLEST0095-WERKS,
      COD_TRANSBORDO   TYPE ZLEST0095-COD_TRANSBORDO,
      "cod_destino       TYPE zlest0095-cod_destino,
      NM_TRANSBORDO    TYPE KNA1-NAME1  ,
      NM_DESTINO       TYPE KNA1-NAME1 ,
      NM_FILAIL        TYPE T001W-NAME1,
      WGBEZ            TYPE T023T-WGBEZ,
      TP_TRANSGENIA	   TYPE ZLEST0095-TP_TRANSGENIA,
      LFIMG            TYPE ZLEST0095-LFIMG ,
      QUANT_TRANSP     TYPE ZLEST0095-LFIMG ,
END   OF TY_ZLEST0096,

BEGIN OF TY_ZSDT0001,
      TP_TRANSGENIA TYPE ZSDT0001-TP_TRANSGENIA,
      NR_ROMANEIO   TYPE ZSDT0001-NR_ROMANEIO,
      VBELN         TYPE LIPS-VBELN,
END   OF TY_ZSDT0001,

BEGIN OF TY_LIPS,
      VBELN	 TYPE LIPS-VBELN,
      MATNR  TYPE LIPS-MATNR,
      MATKL  TYPE LIPS-MATKL,
      WERKS  TYPE LIPS-WERKS,
      LFIMG  TYPE LIPS-LFIMG,
END   OF TY_LIPS.



*&---------------------------------------------------------------------*
*& Tabelas Internas
*&---------------------------------------------------------------------*

DATA : IT_VBFA          TYPE TABLE OF VBFA,
       IT_LIPS          TYPE TABLE OF TY_LIPS,
       IT_VBAK          TYPE TABLE OF VBAK,
       IT_VBPA_LR       TYPE TABLE OF VBPA,
       IT_VBPA_Z1       TYPE TABLE OF VBPA,
       IT_VBPA          TYPE TABLE OF VBPA,
       IT_LFA1          TYPE TABLE OF LFA1,
       IT_KNA1          TYPE TABLE OF KNA1,
       IT_KNA1_DESTINO  TYPE TABLE OF KNA1,
       IT_T001W         TYPE TABLE OF T001W,
       IT_MARA          TYPE TABLE OF MARA,
       IT_MAKT          TYPE TABLE OF MAKT,
       IT_T023T         TYPE TABLE OF T023T,
       IT_ZSDT0001      TYPE TABLE OF TY_ZSDT0001,
       IT_ZLEST0095     TYPE TABLE OF ZLEST0095,
       IT_ZLEST0095_DEL TYPE TABLE OF ZLEST0095,
       IT_ZLEST0095_AUX TYPE TABLE OF ZLEST0095,
       IT_ZLEST0096     TYPE TABLE OF ZLEST0096,
       IT_ZLEST0096_DEL TYPE TABLE OF ZLEST0096.

*&---------------------------------------------------------------------*
*& WORKAREAS
*&---------------------------------------------------------------------*

DATA : WA_VBFA           TYPE  VBFA,
       WA_LIPS           TYPE  TY_LIPS,
       WA_VBAK           TYPE  VBAK,
       WA_VBPA           TYPE  VBPA,
       WA_VBPA_LR        TYPE  VBPA,
       WA_VBPA_Z1        TYPE  VBPA,
       WA_KNA1_DESTINO   TYPE  KNA1,
       WA_LFA1           TYPE  LFA1,
       WA_KNA1           TYPE  KNA1,
       WA_T001W          TYPE  T001W,
       WA_MARA           TYPE  MARA,
       WA_MAKT           TYPE  MAKT,
       WA_T023T          TYPE  T023T,
       WA_ZSDT0001       TYPE  TY_ZSDT0001,
       WA_ZLEST0095      TYPE  ZLEST0095 ,
       WA_ZLEST0095_AUX  TYPE  ZLEST0095 ,
       WA_ZLEST0095_DEL  TYPE  ZLEST0095,
       WA_ZLEST0096      TYPE  ZLEST0096 ,
       WA_ZLEST0096_DEL  TYPE  ZLEST0096 ,
       WA_ZLEST0096_AUX  TYPE  ZLEST0096 .

*&---------------------------------------------------------------------*
*& Ranges
*&---------------------------------------------------------------------*
RANGES: RG_ERDAT FOR VBFA-ERDAT,
        RG_VBELN FOR VBFA-VBELV.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_VKORG FOR VBAK-VKORG  OBLIGATORY ,
                P_WERKS FOR LIPS-WERKS   ,
                P_DATE  FOR VBFA-ERDAT   .

SELECTION-SCREEN END   OF BLOCK B1.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: VG_JOB      TYPE I.

  IF SY-BATCH = 'X'.
    SELECT SINGLE COUNT( * ) INTO VG_JOB
      FROM TBTCO
     WHERE JOBNAME EQ 'ZLESR0085'
       AND STATUS EQ 'R'.
  ELSE.
    VG_JOB = 1.
  ENDIF.

  IF ( VG_JOB EQ 1 ).
    PERFORM SELECIONAR_DADOS.
    PERFORM ORGANIZACAO_DADOS.
  ENDIF.
  " PERFORM ENVIAR_DADOS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .

  DATA: V_TP_TRANSGENIA TYPE ZSDT0001-TP_TRANSGENIA,
        V_NR_ROMANEIO   TYPE ZSDT0001-NR_ROMANEIO,
        V_DOC_REM       TYPE ZSDT0001-DOC_REM.

  IF SY-BATCH = 'X'.
    REFRESH: P_DATE, P_VKORG.

    P_DATE-LOW    = ( SY-DATUM ) - 30.
    P_DATE-HIGH   = SY-DATUM.
    P_DATE-SIGN   = 'I'.
    P_DATE-OPTION = 'BT'.
    APPEND P_DATE.

    P_VKORG-LOW    = '0001'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.

    P_VKORG-LOW    = '0015'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.

    P_VKORG-LOW    = '0018'.
    P_VKORG-SIGN   = 'I'.
    P_VKORG-OPTION = 'EQ'.
    APPEND P_VKORG.

  ENDIF.

  SELECT *
    INTO TABLE IT_VBFA
    FROM VBFA
    CLIENT SPECIFIED
  WHERE VBTYP_N = 'J'
    AND VBTYP_V = 'C'
    AND ERDAT IN  P_DATE
    AND MANDT EQ SY-MANDT.

  IF IT_VBFA IS NOT INITIAL.

    SELECT *
      FROM VBAK
      INTO TABLE IT_VBAK
      FOR ALL ENTRIES IN IT_VBFA
      WHERE VBELN EQ IT_VBFA-VBELV
      AND VKORG  IN P_VKORG
      AND AUART NOT IN ('ZEXI','ZEXP','ZPER')  .

    REFRESH: RG_VBELN.
    RG_VBELN-SIGN = 'I'.
    RG_VBELN-OPTION = 'EQ'.

    LOOP AT  IT_VBAK INTO WA_VBAK.
      MOVE WA_VBAK-VBELN TO RG_VBELN-LOW.
      APPEND RG_VBELN.
      CLEAR WA_VBAK.
      CLEAR: RG_ERDAT.
    ENDLOOP.
    DELETE IT_VBFA WHERE VBELV NOT IN RG_VBELN.

    SELECT VBELN MATNR MATKL WERKS LFIMG
      FROM LIPS
      INTO TABLE IT_LIPS
      FOR ALL ENTRIES IN IT_VBFA
      WHERE VBELN EQ IT_VBFA-VBELN
      AND  WERKS IN P_WERKS .

    SELECT *  "KUNNR
      FROM VBPA
      INTO TABLE IT_VBPA_LR
    FOR ALL ENTRIES IN IT_VBFA
      WHERE VBELN EQ IT_VBFA-VBELV
      AND PARVW EQ 'LR' .

    SELECT *  "VBPA-LIFNR
       FROM VBPA
       INTO TABLE IT_VBPA_Z1
    FOR ALL ENTRIES IN IT_VBFA
        WHERE VBELN EQ IT_VBFA-VBELV
        AND PARVW EQ'Z1'.

    SELECT *
     FROM T001W
    INTO TABLE IT_T001W
   FOR ALL ENTRIES IN IT_LIPS
     WHERE WERKS EQ IT_LIPS-WERKS.

    SELECT *
      FROM MAKT
      INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_LIPS
  WHERE MATNR EQ IT_LIPS-MATNR
  AND SPRAS EQ 'PT'.

    SELECT *
     FROM MARA
    INTO TABLE IT_MARA
   FOR ALL ENTRIES IN IT_LIPS
     WHERE MATNR EQ IT_LIPS-MATNR.

    SELECT *
     FROM T023T
    INTO TABLE IT_T023T
   FOR ALL ENTRIES IN IT_MARA
     WHERE MATKL   EQ IT_MARA-MATKL
      AND SPRAS    EQ 'PT'.

    LOOP AT IT_LIPS INTO WA_LIPS.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_LIPS-VBELN
        IMPORTING
          OUTPUT = V_DOC_REM.

      SELECT TP_TRANSGENIA NR_ROMANEIO
        INTO (V_TP_TRANSGENIA, V_NR_ROMANEIO)
        FROM ZSDT0001
        CLIENT SPECIFIED
        WHERE DOC_REM EQ V_DOC_REM
        AND TP_MOVIMENTO  = 'S'
        AND BUKRS IN P_VKORG
        AND MANDT EQ SY-MANDT.
        "AND nr_romaneio NE 0.

        IF ( ( V_TP_TRANSGENIA EQ ' ' ) OR ( V_TP_TRANSGENIA EQ 'CO' ) OR ( V_TP_TRANSGENIA EQ 'C' ) ).
          V_TP_TRANSGENIA = 'CO'.
        ELSE.
          V_TP_TRANSGENIA = 'TR'.
        ENDIF.

        WA_ZSDT0001-TP_TRANSGENIA = V_TP_TRANSGENIA .
        WA_ZSDT0001-NR_ROMANEIO   = V_NR_ROMANEIO.
        WA_ZSDT0001-VBELN         = WA_LIPS-VBELN.

        APPEND WA_ZSDT0001 TO IT_ZSDT0001.

      ENDSELECT.

    ENDLOOP.

    SELECT *  "XDESTINO
       FROM LFA1
       INTO TABLE IT_LFA1
    FOR ALL ENTRIES IN IT_VBPA_Z1
        WHERE LIFNR	 EQ IT_VBPA_Z1-LIFNR.

    SELECT *  "TRANSBORDO
        FROM KNA1
        INTO TABLE IT_KNA1
     FOR ALL ENTRIES IN IT_VBPA_LR
         WHERE KUNNR EQ IT_VBPA_LR-KUNNR.

    SELECT *  "Destino
        FROM KNA1
        INTO TABLE IT_KNA1_DESTINO
     FOR ALL ENTRIES IN IT_VBAK
         WHERE KUNNR EQ IT_VBAK-KUNNR.

  ELSE.
    MESSAGE I000(Z01) WITH 'Não foram encontrados dados!'.
    STOP.
  ENDIF.
  "  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZACAO_DADOS .

  DATA:  XQUANT         TYPE LIPS-LFIMG,
         XTOTAL_CAM     TYPE N LENGTH 4,
         X_DESTINO      TYPE LFA1-NAME1,
         X_TRANSBORDO   TYPE KNA1-NAME1,
         QTD            TYPE SY-TABIX,
         V_NOME_CLIENTE TYPE LFA1-NAME1.

  RANGES: RG_VBELN FOR ZLEST0095-VBELN.

  " Deleto o processamento anteorior
  "DELETE FROM ZLEST0095 WHERE ERDAT IN  P_DATE.
  "COMMIT WORK.

  LOOP AT IT_LIPS INTO WA_LIPS.

    READ TABLE IT_VBFA INTO WA_VBFA WITH KEY VBELN = WA_LIPS-VBELN.


    WA_ZLEST0095-ERDAT  = WA_VBFA-ERDAT.
    WA_ZLEST0095-VBELN  = WA_LIPS-VBELN.
    WA_ZLEST0095-MATNR  = WA_LIPS-MATNR.
    WA_ZLEST0095-MATKL  = WA_LIPS-MATKL.
    WA_ZLEST0095-WERKS  = WA_LIPS-WERKS.
    WA_ZLEST0095-LFIMG  = WA_LIPS-LFIMG.
    WA_ZLEST0095-VBELV  = WA_VBFA-VBELV.

    READ TABLE IT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_ZLEST0095-VBELV.

    WA_ZLEST0095-AUART = WA_VBAK-AUART.
    WA_ZLEST0095-COD_CLIENTE = WA_VBAK-KUNNR.

    IF ( WA_VBAK-AUART = 'ZRDC') OR ( WA_VBAK-AUART = 'ZRFL').

      READ TABLE IT_VBPA_LR INTO WA_VBPA_LR  WITH KEY VBELN = WA_VBFA-VBELV. " VBPA-KUNNR
      READ TABLE IT_VBPA_Z1 INTO WA_VBPA_Z1  WITH KEY VBELN = WA_VBFA-VBELV. " VBPA-LIFNR


      WA_ZLEST0095-COD_TRANSBORDO  = WA_VBPA_LR-KUNNR.
      WA_ZLEST0095-COD_PORTO       = WA_VBPA_Z1-LIFNR.


*      READ TABLE  IT_LFA1  INTO WA_LFA1 WITH KEY LIFNR = WA_VBPA_LR-LIFNR.
*      X_DESTINO = WA_LFA1-NAME1.
*      READ TABLE  IT_KNA1  INTO WA_KNA1 WITH KEY KUNNR = WA_VBPA_Z1-KUNNR.
*      X_TRANSBORDO = WA_KNA1-NAME1.


    ELSE.

      WA_ZLEST0095-COD_TRANSBORDO  = ''.
      WA_ZLEST0095-COD_PORTO       = ''.

    ENDIF.

    READ TABLE IT_ZSDT0001 INTO WA_ZSDT0001 WITH KEY VBELN =  WA_LIPS-VBELN.


    WA_ZLEST0095-NR_ROMANEIO   =  WA_ZSDT0001-NR_ROMANEIO.
    WA_ZLEST0095-TP_TRANSGENIA =  WA_ZSDT0001-TP_TRANSGENIA.
    WA_ZLEST0095-DT_ATUAL      =  SY-DATUM.
    WA_ZLEST0095-HR_ATUAL      =  SY-UZEIT.


    APPEND WA_ZLEST0095 TO IT_ZLEST0095.


    "INSERT INTO ZLEST0095 VALUES WA_ZLEST0095.

    CLEAR: WA_ZSDT0001,
           WA_VBPA_LR ,
           WA_VBPA_Z1 ,
           WA_VBAK,
           WA_VBFA.

  ENDLOOP.

  REFRESH: RG_VBELN.

  RG_VBELN-SIGN = 'I'.
  RG_VBELN-OPTION = 'EQ'.

  CLEAR: WA_ZLEST0095.
  LOOP AT IT_ZLEST0095 INTO WA_ZLEST0095.
    MOVE WA_ZLEST0095-VBELN TO RG_VBELN-LOW.
    APPEND RG_VBELN.
    CLEAR WA_ZLEST0095.
  ENDLOOP.



  " Nova alteração para tratar exclusão da ZLEST0095 - CSB 09.07.2015
  CLEAR: IT_ZLEST0095_DEL.
  SELECT *
    FROM ZLEST0095
    CLIENT SPECIFIED
    INTO TABLE IT_ZLEST0095_DEL
 "FOR ALL ENTRIES  IN IT_ZLEST0095
     WHERE ERDAT  IN  P_DATE
     AND   MANDT EQ SY-MANDT.


  DELETE IT_ZLEST0095_DEL WHERE VBELN IN RG_VBELN.


  LOOP AT IT_ZLEST0095_DEL INTO  WA_ZLEST0095_DEL.

    DELETE FROM  ZLEST0095 WHERE ERDAT IN  P_DATE AND VBELN EQ WA_ZLEST0095_DEL-VBELN .
    CLEAR: WA_ZLEST0095_DEL.
    " Agora pego o que tenho na IT_ZLEST0095 e faco update na ZLEST0095
  ENDLOOP.

  MODIFY ZLEST0095 FROM TABLE IT_ZLEST0095.

  " Agora pego o que tenho na IT_ZLEST0095 e faco update
  LOOP AT IT_ZLEST0095 INTO WA_ZLEST0095_AUX.
    MODIFY  ZLEST0095  FROM WA_ZLEST0095_AUX.
    CLEAR WA_ZLEST0095_AUX.
  ENDLOOP.
  " Fim Nova alteração.



  " Fim Nova alteração.

  IT_ZLEST0095_AUX[] = IT_ZLEST0095.

  SORT IT_ZLEST0095_AUX[] BY ERDAT WERKS MATKL COD_CLIENTE COD_TRANSBORDO COD_PORTO TP_TRANSGENIA.
  DELETE ADJACENT DUPLICATES FROM IT_ZLEST0095_AUX COMPARING ERDAT WERKS MATKL COD_CLIENTE COD_TRANSBORDO COD_PORTO TP_TRANSGENIA.

  " Não deleta mais as informações - CBRAND
*  " Deleto o processamento anteorior
*  DELETE FROM ZLEST0096 WHERE ERDAT IN  P_DATE.
*  COMMIT WORK.

  LOOP AT IT_ZLEST0095_AUX INTO WA_ZLEST0095_AUX.

    CLEAR: XQUANT , XTOTAL_CAM, WA_ZLEST0095.

    SORT : IT_ZLEST0095 BY  ERDAT WERKS MATKL  COD_CLIENTE COD_TRANSBORDO  COD_PORTO TP_TRANSGENIA.

    LOOP AT IT_ZLEST0095 INTO WA_ZLEST0095 WHERE ERDAT =  WA_ZLEST0095_AUX-ERDAT  AND WERKS = WA_ZLEST0095_AUX-WERKS AND MATKL =  WA_ZLEST0095_AUX-MATKL
      AND COD_CLIENTE = WA_ZLEST0095_AUX-COD_CLIENTE AND COD_TRANSBORDO =  WA_ZLEST0095_AUX-COD_TRANSBORDO
       AND COD_PORTO = WA_ZLEST0095_AUX-COD_PORTO AND  TP_TRANSGENIA = WA_ZLEST0095_AUX-TP_TRANSGENIA.

      XQUANT = XQUANT + WA_ZLEST0095-LFIMG.
      XTOTAL_CAM = XTOTAL_CAM + 1.

    ENDLOOP.

    READ TABLE  IT_LFA1  INTO WA_LFA1 WITH KEY LIFNR = WA_ZLEST0095_AUX-COD_PORTO.

    READ TABLE  IT_KNA1  INTO WA_KNA1 WITH KEY KUNNR = WA_ZLEST0095_AUX-COD_TRANSBORDO.


    WA_ZLEST0096-ERDAT           = WA_ZLEST0095_AUX-ERDAT.
    WA_ZLEST0096-WERKS           = WA_ZLEST0095_AUX-WERKS.
    WA_ZLEST0096-COD_TRANSBORDO  = WA_ZLEST0095_AUX-COD_TRANSBORDO.

    IF WA_ZLEST0095_AUX-COD_PORTO IS NOT INITIAL.
      WA_ZLEST0096-COD_DESTINO     = WA_ZLEST0095_AUX-COD_PORTO.
      WA_ZLEST0096-NM_DESTINO      = WA_LFA1-NAME1.
    ELSE.
      WA_ZLEST0096-COD_DESTINO     = WA_ZLEST0095_AUX-COD_CLIENTE.

      SELECT NAME1
         INTO V_NOME_CLIENTE
         FROM KNA1
         WHERE KUNNR EQ WA_ZLEST0095_AUX-COD_CLIENTE.

      ENDSELECT.

      WA_ZLEST0096-NM_DESTINO      = V_NOME_CLIENTE.

    ENDIF.

    WA_ZLEST0096-NM_TRANSBORDO  =  WA_KNA1-NAME1.

    "WA_ZLEST0096-NM_DESTINO      = WA_LFA1-NAME1.

    READ TABLE IT_T001W INTO WA_T001W WITH KEY WERKS = WA_ZLEST0095_AUX-WERKS.
    WA_ZLEST0096-NM_FILIAL       = WA_T001W-NAME1.

    READ TABLE IT_MARA  INTO WA_MARA  WITH KEY MATNR = WA_ZLEST0095_AUX-MATNR.
    READ TABLE IT_T023T INTO WA_T023T WITH KEY MATKL = WA_MARA-MATKL.

    WA_ZLEST0096-MATNR           = WA_ZLEST0095_AUX-MATNR.
    WA_ZLEST0096-WGBEZ           = WA_T023T-WGBEZ.
    WA_ZLEST0096-TP_TRANSGENIA   = WA_ZLEST0095_AUX-TP_TRANSGENIA.
    WA_ZLEST0096-LFIMG           = XQUANT.
    WA_ZLEST0096-QUANT_TRANSP    = XTOTAL_CAM.
    WA_ZLEST0096-DT_ATUAL        = SY-DATUM.
    WA_ZLEST0096-ATUAL           = 'X'.

    APPEND WA_ZLEST0096 TO IT_ZLEST0096.
    " CSB 10.07.2015
    "INSERT INTO ZLEST0096 VALUES WA_ZLEST0096 .


    CLEAR: WA_LFA1,
           WA_KNA1,
           WA_T001W,
           WA_MARA ,
           WA_T023T,
           WA_ZLEST0096,
           WA_ZLEST0095_AUX.


  ENDLOOP.

  " Nova alteração para tratar exclusão da ZLEST0095 - CSB 09.07.2015
  CLEAR IT_ZLEST0096_DEL.
  SELECT *
    FROM ZLEST0096
    CLIENT SPECIFIED
    INTO TABLE IT_ZLEST0096_DEL
 FOR ALL ENTRIES  IN IT_ZLEST0096
     WHERE ERDAT            IN  P_DATE
      AND  MATNR            NE  IT_ZLEST0096-MATNR
      AND  COD_TRANSBORDO   NE  IT_ZLEST0096-COD_TRANSBORDO
      AND  COD_DESTINO      NE  IT_ZLEST0096-COD_DESTINO
      AND  TP_TRANSGENIA    NE  IT_ZLEST0096-TP_TRANSGENIA.



  LOOP AT IT_ZLEST0096_DEL INTO  WA_ZLEST0096_DEL.

    DELETE FROM  ZLEST0096 WHERE ERDAT IN  P_DATE AND  WERKS  EQ WA_ZLEST0096_DEL-WERKS AND MATNR   EQ  WA_ZLEST0096_DEL-MATNR
        AND COD_TRANSBORDO EQ WA_ZLEST0096_DEL-COD_TRANSBORDO      AND COD_DESTINO    EQ WA_ZLEST0096_DEL-COD_DESTINO
        AND TP_TRANSGENIA  EQ WA_ZLEST0096_DEL-TP_TRANSGENIA.
    CLEAR WA_ZLEST0096_DEL.
  ENDLOOP.

  " Agora pego o que tenho na IT_ZLEST0096 e faco update na ZLEST0096
  LOOP AT IT_ZLEST0096 INTO WA_ZLEST0096_AUX.
    MODIFY  ZLEST0096  FROM WA_ZLEST0096_AUX.
    CLEAR WA_ZLEST0096_AUX.
  ENDLOOP.
  " Fim Nova alteração.


  " Limpa os estonos que ainda não foram realizados.
  DELETE FROM ZLEST0095 WHERE ERDAT IN  P_DATE AND NR_ROMANEIO = '000000000'.

  CLEAR: QTD.
  DESCRIBE TABLE IT_ZLEST0096 LINES QTD.

  IF SY-SUBRC EQ 0.
    MESSAGE S000(Z01) WITH QTD 'Registro(s) Processados com Sucesso. '.
  ENDIF.

ENDFORM.                    " ORGANIZACAO_DADOS
