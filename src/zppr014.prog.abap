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
**|    + Letecea Spiler ( leticia.haagsma@amaggi.com.br )                     |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Cleudo Ferreira ( cleudo.ferreira@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Relatorio de Acompanhamento SAP X Trace Cotton                            |*
**/===========================================================================\*

REPORT ZPPR014.

TABLES: MCHB, ZPPS_XIMFBF_LOG, ZTSAFRAFARDOS.

TYPES: BEGIN OF TY_HEADER, " Quantidade Trace X SAP = Diferença
         QTD_TRA TYPE I,
         QTD_SAP TYPE I,
         QTD_DIF TYPE I,
       END OF TY_HEADER,

       BEGIN OF TY_ENTRADA, " Quantidade Trace X SAP = Diferença
         ID     TYPE I,
         DESC   TYPE ZTSAFRAFARDOS-ALGODOEIRADESCRICAO,
         DT_INI TYPE SY-DATUM,
         DT_FIM TYPE SY-DATUM,
       END OF TY_ENTRADA,

       BEGIN OF TY_ROLOS, "Rolos Pendentes
         ROLO TYPE CHAR16,
       END OF TY_ROLOS,

       BEGIN OF TY_ROLOS_P, "Divergencia de Pesos TRACE X SAP
         ROLO    TYPE CHAR16,
         QTD_TRA TYPE P DECIMALS 0,
         QTD_SAP TYPE P DECIMALS 0,
         QTD_DIF TYPE P DECIMALS 0,
       END OF TY_ROLOS_P,

       BEGIN OF TY_TIPO_P, "Divergencia de Tipo TRACE X SAP
         ROLO   TYPE CHAR16,
         TP_TRA TYPE CHAR10,
         TP_SAP TYPE CHAR10,
       END OF TY_TIPO_P,

       BEGIN OF TY_0006, "Dados de Material, Lote
         CHARG            TYPE CHARG_D,
         MATNR            TYPE MATNR,
         CD_CLASSIFICACAO TYPE ZPPT0006-CD_CLASSIFICACAO,
         DATA             TYPE DATS,
         HORA             TYPE TIMS,
         ID_COTTON        TYPE ZPPT0006-ID_COTTON,
       END OF TY_0006,

       BEGIN OF TY_MCHB,
         WERKS TYPE WERKS_D,
         CHARG TYPE CHARG_D,
         CLABS TYPE LABST,
       END OF TY_MCHB,

       BEGIN OF TY_TELA,
         TELA1 TYPE N LENGTH 4,
         TELA2 TYPE N LENGTH 4,
         TELA3 TYPE N LENGTH 4,
       END OF TY_TELA,

       BEGIN OF TY_ZTSAFRAFARDOS,
         IDALGODOEIRA        TYPE ZTSAFRAFARDOS-IDALGODOEIRA,
         ALGODOEIRADESCRICAO TYPE ZTSAFRAFARDOS-ALGODOEIRADESCRICAO,
       END OF TY_ZTSAFRAFARDOS .


DATA: HEADER TYPE TY_HEADER.
DATA: ENTRADA TYPE TY_ENTRADA.
DATA: TELA TYPE TY_TELA.
DATA: T_ROLOS TYPE TABLE OF TY_ROLOS.
DATA: T_ROLOS_P TYPE TABLE OF TY_ROLOS_P.
DATA: T_TIPO_P TYPE TABLE OF TY_TIPO_P.
DATA: IT_RETURN TYPE TABLE OF DDSHRETVAL,
      ST_RETURN LIKE LINE OF  IT_RETURN.

TYPES: BEGIN OF TY_FARDO,
         IDFARDO                       TYPE I,
         IDFAZENDA                     TYPE I,
         FAZENDACODIGO                 TYPE CHAR1,
         FAZENDADESCRICAO              TYPE CHAR50,
         IDALGODOEIRAMAQUINA           TYPE I,
         ALGODOEIRAMAQUINACODIGO       TYPE CHAR1,
         ALGODOEIRAMAQUINADESCRICAO    TYPE CHAR100,
         ALGODOEIRAMAQUINAPREFIXO      TYPE I,
         IDALGODOEIRA                  TYPE I,
         ALGODOEIRADESCRICAO           TYPE CHAR100,
         ALGODOEIRAPREFIXO             TYPE CHAR50,
         IDSAFRA                       TYPE I,
         SAFRACODIGO                   TYPE CHAR10,
         SAFRADESCRICAO                TYPE CHAR100,
         CODIGOFARDO                   TYPE CHAR10,
         CODIGOSAI                     TYPE CHAR50,
         PESOBRUTO                     TYPE P DECIMALS 4,
         PESOLIQUIDO                   TYPE P DECIMALS 4,
         ALGODAOCLASSIFICACAOCODIGO    TYPE CHAR1,
         ALGODAOCLASSIFICACAODESCRICAO TYPE CHAR10,
         ALGODAOCLASSIFICACAOTPDESC    TYPE CHAR50,
         IDBLOCO                       TYPE I,
         IDFARDAO                      TYPE I,
         CODIGOFARDAO                  TYPE CHAR16,
         FARDAOPESO                    TYPE I,
         IDLOCALARMAZENAMENTO          TYPE I,
         LOCALARMAZENAMENTODESCRICAO   TYPE I,
         IDFARDOSITUACAO               TYPE I,
         FARDOSITUACAO                 TYPE CHAR100,
         DATAMOVIMENTO                 TYPE CHAR50,
         DATABENEFICIAMENTO            TYPE CHAR50,
         DATACLASSIFICACAO             TYPE CHAR50,
         GUID                          TYPE CHAR100,
         IDSESSAO                      TYPE I,
         TURNOINICIO                   TYPE CHAR50,
         TURNOFIM                      TYPE CHAR50,
         IDTURNOALGODOEIRA             TYPE I,
         TURNOTIPO                     TYPE CHAR50,
         ORDEMTURNO                    TYPE I,
         DESCARTADO                    TYPE CHAR10,
         IDALGODOEIRAMAQUINATURNO      TYPE I,
         ALGODOEIRAMAQUINATURNOGUID    TYPE CHAR100,
         FARDAOFAZENDA                 TYPE CHAR50,
         FARDAOFAZENDACODIGO           TYPE CHAR10,
         FARDOHVI                      TYPE CHAR1,
         POSICAOATUAL                  TYPE I,
         SAFRAPERIODO                  TYPE CHAR100,
         VARIEDADE                     TYPE CHAR100,
         VARIEDADEGRUPO                TYPE CHAR100,
         BORDADURA                     TYPE CHAR50,
         FAZENDASETOR                  TYPE CHAR100,
         TALHAO                        TYPE CHAR100,
         IDTALHAO                      TYPE I,
         TURNOCOLHEITA                 TYPE CHAR50,
         BLOCOCODIGO                   TYPE CHAR10,
         BLOCOSITUACAODESCRICAO        TYPE CHAR50,
         BLOCOFARDOCRIACAODATA         TYPE CHAR50,
         IDFAZENDAFARDAO               TYPE I,
         IDSAFRAPERIODO                TYPE I,
         IDEMPRESA                     TYPE I,
         HVIOS                         TYPE CHAR50,
         HVIUHML                       TYPE P DECIMALS 2,
         HVIUI                         TYPE P DECIMALS 2,
         HVISTR                        TYPE P DECIMALS 2,
         HVIELG                        TYPE P DECIMALS 2,
         HVIMIC                        TYPE P DECIMALS 2,
         HVIRD                         TYPE P DECIMALS 2,
         HVIB                          TYPE P DECIMALS 2,
         HVICG                         TYPE CHAR50,
         HVITCNT                       TYPE I,
         HVITAREA                      TYPE P DECIMALS 2,
         HVILEAF                       TYPE I,
         HVIMR                         TYPE P DECIMALS 2,
         HVISFI                        TYPE P DECIMALS 2,
         HVISCI                        TYPE I,
         HVICSP                        TYPE P DECIMALS 2,
         IDALGODAOCLASSIFICACAO        TYPE I,
         IDALGODAOCLASSIFICACAOTIPO    TYPE I,
         IDBLOCOSITUACAO               TYPE I,
       END OF TY_FARDO.

DATA: IT_0006                TYPE TABLE OF TY_0006,
      IT_0006_AUX            TYPE TABLE OF TY_0006,
      IT_MCHB                TYPE TABLE OF MCHB,
      IT_MCHB_AUX            TYPE TABLE OF TY_MCHB,
      WA_MCHB_AUX            TYPE TY_MCHB,
      IT_MCHB_SALDO          TYPE TABLE OF MCHB,
      IT_MCHB_ZERO           TYPE TABLE OF MCHB,
      IT_ZPPS_XIMFBF_LOG     TYPE TABLE OF ZPPS_XIMFBF_LOG,
      IT_0002                TYPE TABLE OF ZPPT0002,
      IT_0002_AUX            TYPE TABLE OF ZPPT0002,
      IT_ZPPS_XIMFBF_LOG_AUX TYPE TABLE OF ZPPS_XIMFBF_LOG,
      IT_ZPPS_XIMFBF_DEL     TYPE TABLE OF ZPPS_XIMFBF_LOG,
      WA_ZPPS_XIMFBF_LOG     TYPE ZPPS_XIMFBF_LOG,
      IT_FARDO               TYPE TABLE OF TY_FARDO,
      IT_FARDO_AUX           TYPE TABLE OF TY_FARDO,
      WA_FARDO               TYPE TY_FARDO.

DATA _URL  TYPE STRING.
*DATA _URLP TYPE STRING VALUE 'https://api.amaggi.com.br:6244/api-a/algodao/listar/fardo'.
DATA _IDAL TYPE STRING VALUE '?idAlgodoeira='.
DATA _DTMI TYPE STRING VALUE '&dataMovimentoInicio='.
DATA _DTMF TYPE STRING VALUE '&dataMovimentoFim='.
DATA _ID   TYPE WERKS_D.
DATA _DTI  TYPE CHAR10.
DATA _DTF  TYPE CHAR10.
DATA _TELA TYPE SY-DYNNR.
DATA _ERR TYPE CHAR1.

DATA OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE.
DATA: E_REASON     TYPE STRING,
      JSON_RETORNO TYPE STRING,
      E_HTTP       TYPE REF TO  IF_HTTP_CLIENT.

CREATE OBJECT OB_WEB_SERVICE.

DATA: G_GRID1              TYPE REF TO CL_GUI_ALV_GRID,
      G_GRID2              TYPE REF TO CL_GUI_ALV_GRID,
      G_GRID3              TYPE REF TO CL_GUI_ALV_GRID,
      G_CUSTOM_CONTAINER1  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CUSTOM_CONTAINER2  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_CUSTOM_CONTAINER3  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER,
      IT_FCAT              TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT1             TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT2             TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT3             TYPE TABLE OF LVC_S_FCAT,
      IT_ZTSAFRAFARDOS     TYPE TABLE OF TY_ZTSAFRAFARDOS,
      WL_FCAT              TYPE LVC_S_FCAT,
      _LAYOUT              TYPE LVC_S_LAYO,
      WA_STABLE            TYPE LVC_S_STBL VALUE 'XX'.

DATA: TG_SELECTEDROW TYPE LVC_T_ROW,
      WG_SELECTEDROW TYPE LVC_S_ROW.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
"ID_ALDO FOR ZPPS_XIMFBF_LOG-CD_SAFRA NO INTERVALS NO-EXTENSION,
                ID_ALDO FOR ZTSAFRAFARDOS-IDALGODOEIRA NO INTERVALS NO-EXTENSION,
                ID_DATA FOR MCHB-LAEDA.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_SIN AS CHECKBOX DEFAULT 'X',
            P_ANA AS CHECKBOX USER-COMMAND CHS.
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_01 AS CHECKBOX MODIF ID ANA,
            P_02 AS CHECKBOX MODIF ID ANA,
            P_03 AS CHECKBOX MODIF ID ANA.
SELECTION-SCREEN END OF BLOCK B3.
SELECTION-SCREEN END OF BLOCK B2.



AT SELECTION-SCREEN OUTPUT.

  IF P_ANA IS INITIAL.
    CLEAR: P_01, P_02, P_03.
  ENDIF.

  IF ID_ALDO IS NOT INITIAL AND ID_DATA IS NOT INITIAL.
    LOOP AT SCREEN.
      IF P_ANA IS INITIAL.
        IF SCREEN-GROUP1 EQ 'ANA'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        SCREEN-ACTIVE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    CLEAR: P_ANA, P_01, P_02, P_03.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'ANA'.
        SCREEN-ACTIVE = 0.
        MODIFY SCREEN.
      ELSE.
        SCREEN-ACTIVE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR ID_ALDO-LOW.
  PERFORM Z_MACHCODE.


START-OF-SELECTION.

  IF ID_ALDO IS INITIAL.
    MESSAGE 'Preencha o Campo "Id Algodoeira"!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF ID_DATA IS INITIAL.
    MESSAGE 'Preencha o Campo "Data de Movimento"!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  FREE: T_ROLOS, T_ROLOS_P.
  PERFORM INDICATOR USING 1 'Iniciando Busca Trace X SAP!'.

  PERFORM TELA.

  IF P_SIN IS NOT INITIAL OR P_ANA IS NOT INITIAL.
    PERFORM BUSCA_DADOS_TRACE.
    PERFORM Z_BUSCA_DADOS_SAP.
    PERFORM ORGANIZA_DADOS.
  ENDIF.

  CHECK _ERR IS INITIAL.

  IF IT_FARDO IS INITIAL.
    MESSAGE 'Nenhuma Informação Foi encontrada!' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN 0100.
  ENDIF.

FORM Z_BUSCA_DADOS_SAP.
  CHECK _ERR IS INITIAL.

  IF IT_FARDO_AUX IS NOT INITIAL.

*    PERFORM INDICATOR USING 55 'Buscando dados do SAP "MCHB"!'.
*    SELECT *
*        FROM MCHB
*          INTO TABLE IT_MCHB
*        FOR ALL ENTRIES IN IT_FARDO_AUX
*      WHERE CHARG EQ IT_FARDO_AUX-CODIGOFARDAO.

*    PERFORM INDICATOR USING 65 'Buscando dados do SAP "ZPPS_XIMFBF_LOG"!'.
*    SELECT *
*      FROM ZPPS_XIMFBF_LOG
*        INTO TABLE IT_ZPPS_XIMFBF_LOG
*      FOR ALL ENTRIES IN IT_FARDO_AUX
*    WHERE CHARG EQ IT_FARDO_AUX-CODIGOFARDAO AND MBLNR NE ' '.

    PERFORM INDICATOR USING 65 'Buscando dados do SAP "ZPPT0002"!'.
    SELECT *
      FROM ZPPT0002
        INTO TABLE IT_0002
      FOR ALL ENTRIES IN IT_FARDO_AUX
    WHERE  ID_COTTON EQ IT_FARDO_AUX-CODIGOFARDAO.

    PERFORM CHARG_BRANCO.

    PERFORM INDICATOR USING 65 'Buscando dados do SAP "ZPPS_XIMFBF_LOG"!'.
    SELECT *
      FROM ZPPS_XIMFBF_LOG
        INTO TABLE IT_ZPPS_XIMFBF_LOG
      FOR ALL ENTRIES IN IT_FARDO_AUX
    WHERE  ID_COTTON EQ IT_FARDO_AUX-CODIGOFARDAO.

    DELETE IT_ZPPS_XIMFBF_LOG WHERE MBLNR EQ ' '.

*    PERFORM INDICATOR USING 70 'Buscando dados do SAP "ZPPT0006"!'.
*    SELECT CHARG, MATNR, CD_CLASSIFICACAO, DATA, HORA, ID_COTTON
*       FROM ZPPT0006 AS A
*         INTO TABLE @IT_0006
*       FOR ALL ENTRIES IN @IT_FARDO_AUX
*     WHERE ID_COTTON EQ @IT_FARDO_AUX-CODIGOFARDAO.

  ENDIF.

  CHECK IT_0002 IS NOT INITIAL.

  PERFORM INDICATOR USING 55 'Buscando dados do SAP "MCHB"!'.

  SELECT *
      FROM MCHB
        INTO TABLE IT_MCHB
      FOR ALL ENTRIES IN IT_0002
    WHERE CHARG EQ IT_0002-CHARG.

  LOOP AT IT_MCHB INTO DATA(WA_MCHB).
    MOVE-CORRESPONDING WA_MCHB TO WA_MCHB_AUX.
    COLLECT WA_MCHB_AUX INTO IT_MCHB_AUX.
  ENDLOOP.


*  IF IT_ZPPS_XIMFBF_LOG IS NOT INITIAL.

*  SELECT *
*      FROM MCHB
*        INTO TABLE IT_MCHB
*      FOR ALL ENTRIES IN IT_ZPPS_XIMFBF_LOG
*    WHERE CHARG EQ IT_ZPPS_XIMFBF_LOG-CHARG.

*  ENDIF.

*  IF  IT_0006 IS NOT INITIAL.
*
*    SELECT *
*        FROM MCHB
*          APPENDING TABLE IT_MCHB
*        FOR ALL ENTRIES IN IT_0006
*      WHERE CHARG EQ IT_0006-CHARG.
*
*  ENDIF.
*
*  SORT IT_MCHB BY MATNR WERKS LGORT CHARG.
*  DELETE ADJACENT DUPLICATES FROM IT_MCHB COMPARING MATNR WERKS LGORT CHARG.

ENDFORM.



FORM BUSCA_DADOS_TRACE.
  DATA: VIDALGODOEIRA(4) TYPE N.

  PERFORM INDICATOR USING 10 'Buscando dados do TraceCotton!'.

  SELECT SINGLE URL, URL_TOKEN
    FROM ZCIOT_WEBSERVICE
    INTO @DATA(_URLP)
    WHERE TIPO EQ '6'
    AND SERVICO EQ '02'.

  IF SY-SUBRC IS NOT INITIAL.
    _ERR = ABAP_TRUE.
    MESSAGE 'Url não cadastrado na ZLES0096!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  _ID = ID_ALDO-LOW.
  _DTI = |{ ID_DATA-LOW(4) }-{ ID_DATA-LOW+4(2) }-{ ID_DATA-LOW+6(2) }|.
  _DTF = COND #( WHEN ID_DATA-HIGH IS INITIAL
                      THEN _DTI
                      ELSE |{ ID_DATA-HIGH(4) }-{ ID_DATA-HIGH+4(2) }-{ ID_DATA-HIGH+6(2) }|
                ).

  _URL = |{ _URLP-URL }{ _IDAL }{ _ID }{ _DTMI }{ _DTI }{ _DTMF }{ _DTF }|.


  VIDALGODOEIRA =  _ID.

  SELECT * FROM ZTSAFRAFARDOS
    INTO TABLE @DATA(T_ALGODOEIRA)
    WHERE IDALGODOEIRA EQ @VIDALGODOEIRA.

  SORT T_ALGODOEIRA BY IDALGODOEIRA.
  DELETE ADJACENT DUPLICATES FROM T_ALGODOEIRA COMPARING IDALGODOEIRA.

  READ TABLE T_ALGODOEIRA INTO DATA(W_ALGODOEIRA) WITH KEY IDALGODOEIRA = VIDALGODOEIRA.
  TRY.
      ENTRADA =
      VALUE #(
                ID     = _ID
                DESC   = W_ALGODOEIRA-ALGODOEIRADESCRICAO
                DT_INI = ID_DATA-LOW
                DT_FIM = COND #( WHEN ID_DATA-HIGH IS INITIAL THEN ID_DATA-LOW ELSE ID_DATA-HIGH )
             ).
    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
  ENDTRY.
  PERFORM INDICATOR USING 20 'Buscando dados do TraceCotton!'.

  CL_HTTP_CLIENT=>CREATE_BY_URL(
  EXPORTING URL   = |{ _URL }|
  IMPORTING CLIENT = E_HTTP
  EXCEPTIONS ARGUMENT_NOT_FOUND = 1
             PLUGIN_NOT_ACTIVE  = 2
             INTERNAL_ERROR     = 3 ).

  PERFORM INDICATOR USING 30 'Buscando dados do TraceCotton!'.

  CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
    EXPORTING
      NAME  = '~request_method'
      VALUE = 'GET'.

  CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
    EXPORTING
      NAME  = '~server_protocol'
      VALUE = 'HTTP/1.1'.

  CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
    EXPORTING
      NAME  = 'Content-Type'
      VALUE = 'application/json; charset=UTF-8'.

  CALL METHOD E_HTTP->REQUEST->SET_HEADER_FIELD
    EXPORTING
      NAME  = 'Accept'
      VALUE = 'application/json'.

  PERFORM INDICATOR USING 30 'Buscando dados do TraceCotton!'.

  CLEAR JSON_RETORNO.

  OB_WEB_SERVICE->ZIF_WEBSERVICE~ADD_TOKEN_OPUS_HTTP_CLIENTE(
    EXPORTING
      I_URL_DESTINO              = _URL
      I_URL_TOKEN                = CONV #( _URLP-URL_TOKEN )
    CHANGING
      I_HTTP                     = E_HTTP
    EXCEPTIONS
      HTTP_COMMUNICATION_FAILURE = 1
      HTTP_INVALID_STATE         = 2
      HTTP_PROCESSING_FAILED     = 3
      HTTP_INVALID_TIMEOUT       = 4
      OTHERS                     = 5
  ).

  OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR(
      EXPORTING
        I_HTTP                     = E_HTTP
      IMPORTING
        E_REASON                   = E_REASON
      RECEIVING
        E_RESULTADO                = JSON_RETORNO
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5   ).

  PERFORM INDICATOR USING 40 'Buscando dados do TraceCotton!'.

  /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = JSON_RETORNO CHANGING DATA = IT_FARDO ).

  PERFORM INDICATOR USING 50 'Buscando dados do TraceCotton!'.

  IT_FARDO_AUX = IT_FARDO.
  SORT IT_FARDO_AUX BY CODIGOFARDAO.
  DELETE ADJACENT DUPLICATES FROM IT_FARDO_AUX COMPARING CODIGOFARDAO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  PERFORM INDICATOR USING 90 'Exibindo Dados!'.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM ALV.

  DATA(_FUNCTION) =
        VALUE UI_FUNCTIONS(
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_COPY )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW )
                  ( CL_GUI_ALV_GRID=>MC_FC_LOC_CUT )
                  ( CL_GUI_ALV_GRID=>MC_FC_CHECK )
                  ( CL_GUI_ALV_GRID=>MC_FC_REFRESH )
                  ( CL_GUI_ALV_GRID=>MC_FC_PRINT )
                  ( CL_GUI_ALV_GRID=>MC_FC_SUBTOT )
                  ( CL_GUI_ALV_GRID=>MC_FC_SUM )
                  ( CL_GUI_ALV_GRID=>MC_FC_VIEWS )
                  ( CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL )
                  ( CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID )
                  ( CL_GUI_ALV_GRID=>MC_FC_GRAPH )

                  ( CL_GUI_ALV_GRID=>MC_FC_INFO )

                  ( CL_GUI_ALV_GRID=>MC_MB_EXPORT )
                  ( CL_GUI_ALV_GRID=>MC_MB_FILTER )
                  ( CL_GUI_ALV_GRID=>MC_MB_PASTE )
                  ( CL_GUI_ALV_GRID=>MC_MB_SUBTOT )
                  ( CL_GUI_ALV_GRID=>MC_MB_SUM )
                  ( CL_GUI_ALV_GRID=>MC_MB_VARIANT )
                  ( CL_GUI_ALV_GRID=>MC_MB_VIEW )

*                  ( CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL )
               ).

  PERFORM INDICATOR USING 100 'Exibindo Dados!'.

  IF P_01 IS NOT INITIAL.
    IF G_CUSTOM_CONTAINER1 IS INITIAL.

      CREATE OBJECT G_CUSTOM_CONTAINER1
        EXPORTING
          CONTAINER_NAME              = 'CC1'
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5
          OTHERS                      = 6.

      CREATE OBJECT G_GRID1
        EXPORTING
          I_PARENT          = G_CUSTOM_CONTAINER1
        EXCEPTIONS
          ERROR_CNTL_CREATE = 1
          ERROR_CNTL_INIT   = 2
          ERROR_CNTL_LINK   = 3
          ERROR_DP_CREATE   = 4
          OTHERS            = 5.

      _LAYOUT = VALUE #( GRID_TITLE = 'Rolos Pendentes' ).

      CALL METHOD G_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT                     = _LAYOUT
          I_SAVE                        = 'A'
          IT_TOOLBAR_EXCLUDING          = _FUNCTION
        CHANGING
          IT_OUTTAB                     = T_ROLOS
          IT_FIELDCATALOG               = IT_FCAT1
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
    ELSE.
      CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ENDIF.
  ENDIF.

  IF P_02 IS NOT INITIAL.
    IF G_CUSTOM_CONTAINER2 IS INITIAL.

      CREATE OBJECT G_CUSTOM_CONTAINER2
        EXPORTING
          CONTAINER_NAME              = 'CC2'
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5
          OTHERS                      = 6.

      CREATE OBJECT G_GRID2
        EXPORTING
          I_PARENT          = G_CUSTOM_CONTAINER2
        EXCEPTIONS
          ERROR_CNTL_CREATE = 1
          ERROR_CNTL_INIT   = 2
          ERROR_CNTL_LINK   = 3
          ERROR_DP_CREATE   = 4
          OTHERS            = 5.

      _LAYOUT = VALUE #( GRID_TITLE = 'Divergência Peso' ).

      CALL METHOD G_GRID2->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT                     = _LAYOUT
          I_SAVE                        = 'A'
          IT_TOOLBAR_EXCLUDING          = _FUNCTION
        CHANGING
          IT_OUTTAB                     = T_ROLOS_P
          IT_FIELDCATALOG               = IT_FCAT2
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
    ELSE.
      CALL METHOD G_GRID2->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ENDIF.
  ENDIF.

  IF P_03 IS NOT INITIAL.
    IF G_CUSTOM_CONTAINER3 IS INITIAL.

      CREATE OBJECT G_CUSTOM_CONTAINER3
        EXPORTING
          CONTAINER_NAME              = 'CC3'
        EXCEPTIONS
          CNTL_ERROR                  = 1
          CNTL_SYSTEM_ERROR           = 2
          CREATE_ERROR                = 3
          LIFETIME_ERROR              = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5
          OTHERS                      = 6.

      CREATE OBJECT G_GRID3
        EXPORTING
          I_PARENT          = G_CUSTOM_CONTAINER3
        EXCEPTIONS
          ERROR_CNTL_CREATE = 1
          ERROR_CNTL_INIT   = 2
          ERROR_CNTL_LINK   = 3
          ERROR_DP_CREATE   = 4
          OTHERS            = 5.

      _LAYOUT = VALUE #( GRID_TITLE = 'Divergência Tipo' ).

      CALL METHOD G_GRID3->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT                     = _LAYOUT
          I_SAVE                        = 'A'
          IT_TOOLBAR_EXCLUDING          = _FUNCTION
        CHANGING
          IT_OUTTAB                     = T_TIPO_P
          IT_FIELDCATALOG               = IT_FCAT3
        EXCEPTIONS
          INVALID_PARAMETER_COMBINATION = 1
          PROGRAM_ERROR                 = 2
          TOO_MANY_LINES                = 3
          OTHERS                        = 4.
    ELSE.
      CALL METHOD G_GRID3->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    ENDIF.
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
  ENDCASE.
ENDMODULE.


FORM ALV.

  FREE: IT_FCAT1, IT_FCAT2, IT_FCAT3.

  FREE IT_FCAT.
  PERFORM PREENCHE_CAT USING : 'ROLO' 'Rolo' '16' ''  '' ''  '' ''.

  APPEND LINES OF IT_FCAT TO IT_FCAT1.

  FREE IT_FCAT.
  PERFORM PREENCHE_CAT USING :
        'ROLO'    'Rolo'      '16' '' '' '' '' '',
        'QTD_TRA' 'Trace'     '6' '' '' '' '' '',
        'QTD_SAP' 'SAP'       '6' '' '' '' '' '',
        'QTD_DIF' 'Diferença' '10' '' '' '' '' ''.

  APPEND LINES OF IT_FCAT TO IT_FCAT2.

  FREE IT_FCAT.
  PERFORM PREENCHE_CAT USING :
        'ROLO'    'Fardinho' '16' '' '' '' '' '',
        'TP_TRA'  'Trace'    '10' '' '' '' '' '',
        'TP_SAP'  'SAP'      '10' '' '' '' '' ''.
  APPEND LINES OF IT_FCAT TO IT_FCAT3.

ENDFORM.


FORM PREENCHE_CAT USING  VALUE(P_CAMPO)
                         VALUE(P_DESC)
                         VALUE(P_TAM)
                         VALUE(P_ZERO)
                         VALUE(P_HOT)
                         VALUE(P_SUM)
                         VALUE(P_JUST)
                         VALUE(P_COR).

  APPEND VALUE #(
                  FIELDNAME = P_CAMPO
                  SCRTEXT_L = P_DESC
                  SCRTEXT_M = P_DESC
                  SCRTEXT_S = P_DESC
                  OUTPUTLEN = P_TAM
                  HOTSPOT   = P_HOT
                  NO_ZERO   = P_ZERO
                  DO_SUM    = P_SUM
                  JUST      = P_JUST
                  EMPHASIZE = P_COR
                ) TO IT_FCAT.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF P_SIN IS INITIAL.
      IF SCREEN-NAME CS 'HEADER'.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0741   text
*----------------------------------------------------------------------*
FORM INDICATOR USING VALUE(PORCENTAGEM) VALUE(TEXTO).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE = PORCENTAGEM
      TEXT       = TEXTO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HEADER.

  HEADER-QTD_TRA = REDUCE #( INIT X = 0 FOR LS1 IN IT_FARDO_AUX  NEXT X = X + 1 ).   "// Quantidade do Trace Cotton
  HEADER-QTD_SAP = REDUCE #( INIT Y = 0 FOR LS2 IN IT_MCHB_ZERO NEXT Y = Y + 1 ).    "// Quantidade da SAP
  HEADER-QTD_DIF = HEADER-QTD_TRA - HEADER-QTD_SAP.                                  "// Diferenla entre TRACE x SAP

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TELA.

  CLEAR TELA.

  IF P_01 IS NOT INITIAL.
    TELA-TELA1 = '0101'.
  ENDIF.

  IF P_02 IS NOT INITIAL.
    IF TELA-TELA1 IS INITIAL.
      TELA-TELA1 = '0102'.
    ELSE.
      TELA-TELA2 = '0102'.
    ENDIF.
  ENDIF.

  IF P_03 IS NOT INITIAL.
    IF TELA-TELA1 IS INITIAL.
      TELA-TELA1 = '0103'.
    ELSEIF TELA-TELA2 IS INITIAL.
      TELA-TELA2 = '0103'.
    ELSE.
      TELA-TELA3 = '0103'.
    ENDIF.
  ENDIF.


  IF TELA-TELA1 IS INITIAL.
    TELA-TELA1 = '0104'.
  ENDIF.

  IF TELA-TELA2 IS INITIAL.
    TELA-TELA2 = '0104'.
  ENDIF.

  IF TELA-TELA3 IS INITIAL.
    TELA-TELA3 = '0104'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZA_DADOS .

  CHECK _ERR IS INITIAL.

*  IT_MCHB_SALDO = VALUE #( FOR LS2 IN IT_MCHB WHERE ( CLABS NE 0 ) ( CORRESPONDING #( LS2 ) ) ).  "// Com saldo
*  IT_MCHB_ZERO  = VALUE #( FOR LS3 IN IT_MCHB WHERE ( CLABS EQ 0 ) ( CORRESPONDING #( LS3 ) ) ).  "// Sem Saldo

  IT_MCHB_SALDO = VALUE #( FOR LS2 IN IT_MCHB_AUX WHERE ( CLABS NE 0 ) ( CORRESPONDING #( LS2 ) ) ).  "// Com saldo
  IT_MCHB_ZERO  = VALUE #( FOR LS3 IN IT_MCHB_AUX WHERE ( CLABS EQ 0 ) ( CORRESPONDING #( LS3 ) ) ).  "// Sem Saldo


  SORT IT_MCHB_ZERO BY CHARG.
  DELETE ADJACENT DUPLICATES FROM IT_MCHB_ZERO COMPARING CHARG.

  IT_ZPPS_XIMFBF_DEL = VALUE #( FOR LS IN IT_ZPPS_XIMFBF_LOG WHERE ( ZST_ATLZ EQ 'E' ) ( CORRESPONDING #( LS ) ) ).

  LOOP AT IT_ZPPS_XIMFBF_DEL INTO DATA(WA_ZPPS_XIMFBF_DEL).
    DELETE IT_ZPPS_XIMFBF_LOG WHERE MBLNR EQ WA_ZPPS_XIMFBF_DEL-MBLNR.
  ENDLOOP.

* "// Fardos Pendentes "Contem Saldo"
  IF P_01 IS NOT INITIAL.

*    FREE IT_ZPPS_XIMFBF_LOG_AUX.

*    IT_ZPPS_XIMFBF_LOG_AUX = IT_ZPPS_XIMFBF_LOG.
*    SORT  IT_ZPPS_XIMFBF_LOG_AUX BY ID_COTTON.
*    DELETE ADJACENT DUPLICATES FROM IT_ZPPS_XIMFBF_LOG_AUX COMPARING ID_COTTON.
*
*    IT_0006_AUX = IT_0006.
*    DELETE IT_0006_AUX WHERE CHARG IS INITIAL.
*    SORT IT_0006_AUX BY ID_COTTON.
*    DELETE ADJACENT DUPLICATES FROM IT_0006_AUX COMPARING ID_COTTON.

    LOOP AT IT_FARDO_AUX INTO DATA(W_FARDO).
      "READ TABLE T_ALGODOEIRA INTO DATA(W_ALGODOEIRA) WITH KEY IDALGODOEIRA = VIDALGODOEIRA.
      IF W_FARDO-CODIGOFARDAO <> '/'.

*        READ TABLE IT_ZPPS_XIMFBF_LOG_AUX INTO DATA(W_LOG) WITH KEY  ID_COTTON = W_FARDO-CODIGOFARDAO.
*        IF SY-SUBRC IS INITIAL.
*          DATA(VL_CHARG) = W_LOG-CHARG.
*        ENDIF.
*
*        READ TABLE IT_0006_AUX INTO DATA(W_006) WITH KEY ID_COTTON = W_FARDO-CODIGOFARDAO.
*        IF SY-SUBRC IS INITIAL.
*          VL_CHARG = W_006-CHARG.
*        ENDIF.

        READ TABLE IT_0002 INTO DATA(W_002) WITH KEY ID_COTTON = W_FARDO-CODIGOFARDAO.
        IF SY-SUBRC IS INITIAL.
          DATA(VL_CHARG) = W_002-CHARG.
        ENDIF.

        IF NOT LINE_EXISTS( IT_MCHB_ZERO[ CHARG = VL_CHARG ] )." OR NOT LINE_EXISTS( IT_MCHB_ZERO[ CHARG = W_FARDO-CODIGOFARDAO ] ).
          APPEND VALUE #( ROLO = W_FARDO-CODIGOFARDAO ) TO T_ROLOS.  "// Não Exixte no SAP
        ENDIF.

      ENDIF.

    ENDLOOP.

    SORT T_ROLOS BY ROLO.
    DELETE ADJACENT DUPLICATES FROM T_ROLOS COMPARING ROLO.

  ENDIF.

  IF P_SIN IS NOT INITIAL.
    PERFORM HEADER.
  ENDIF.

  IF P_03 IS NOT INITIAL.

    FREE IT_0006_AUX.

    LOOP AT IT_0006 ASSIGNING FIELD-SYMBOL(<F_0006>).
      IF NOT LINE_EXISTS( IT_0006_AUX[ CHARG = <F_0006>-CHARG ] ).
        APPEND <F_0006> TO  IT_0006_AUX.
      ELSE.

        DATA(WA_0006) = IT_0006_AUX[ CHARG  = <F_0006>-CHARG ].
        IF WA_0006-DATA < <F_0006>-DATA.
          MODIFY IT_0006_AUX FROM <F_0006> TRANSPORTING CHARG MATNR CD_CLASSIFICACAO DATA HORA WHERE CHARG EQ <F_0006>-CHARG.
        ELSEIF WA_0006-DATA EQ <F_0006>-DATA.
          IF WA_0006-HORA < <F_0006>-HORA.
            MODIFY IT_0006_AUX FROM <F_0006> TRANSPORTING CHARG MATNR CD_CLASSIFICACAO DATA HORA WHERE CHARG EQ <F_0006>-CHARG.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF P_02 IS NOT INITIAL.
* "// Fardos com Divergencia de Pesos
    LOOP AT IT_FARDO_AUX INTO W_FARDO.
      FREE IT_ZPPS_XIMFBF_LOG_AUX.

      IT_ZPPS_XIMFBF_LOG_AUX = IT_ZPPS_XIMFBF_LOG.
      SORT  IT_ZPPS_XIMFBF_LOG_AUX BY ID_COTTON.
      DELETE ADJACENT DUPLICATES FROM IT_ZPPS_XIMFBF_LOG_AUX COMPARING ID_COTTON.
*     LOOP AT IT_ZPPS_XIMFBF_LOG INTO DATA(WA_XI) WHERE CHARG EQ W_FARDO-CODIGOFARDAO.
      LOOP AT IT_ZPPS_XIMFBF_LOG_AUX INTO DATA(WA_XI) WHERE ID_COTTON EQ W_FARDO-CODIGOFARDAO.
        IF WA_XI-QTEPROD NE W_FARDO-FARDAOPESO.
          APPEND VALUE #(
                         ROLO    = W_FARDO-CODIGOFARDAO
                         QTD_TRA = W_FARDO-FARDAOPESO
                         QTD_SAP = WA_XI-QTEPROD
                         QTD_DIF = W_FARDO-FARDAOPESO - WA_XI-QTEPROD
                        ) TO T_ROLOS_P.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDIF.

*  CHECK IT_0006_AUX IS NOT INITIAL.
* "// Fardos com Divergencia de Tipo
*  LOOP AT IT_FARDO INTO W_FARDO.
*    LOOP AT IT_0006_AUX INTO DATA(W_0006) WHERE CHARG EQ W_FARDO-CODIGOFARDO.
*      IF W_0006-CD_CLASSIFICACAO NE W_FARDO-ALGODAOCLASSIFICACAODESCRICAO.
*        APPEND VALUE #(
*                       ROLO   = W_FARDO-CODIGOFARDO
*                       TP_TRA = W_FARDO-ALGODAOCLASSIFICACAODESCRICAO
*                       TP_SAP = W_0006-CD_CLASSIFICACAO
*                      ) TO T_TIPO_P.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.

  CHECK IT_0002 IS NOT INITIAL.

  LOOP AT IT_FARDO INTO W_FARDO.
    LOOP AT IT_0002 INTO DATA(W_0002) WHERE ACHARG EQ W_FARDO-CODIGOFARDO.
      IF W_0002-CD_CLASSIFICACAO NE W_FARDO-ALGODAOCLASSIFICACAODESCRICAO.
        APPEND VALUE #(
                       ROLO   = W_FARDO-CODIGOFARDO
                       TP_TRA = W_FARDO-ALGODAOCLASSIFICACAODESCRICAO
                       TP_SAP = W_0002-CD_CLASSIFICACAO
                      ) TO T_TIPO_P.
      ENDIF.
    ENDLOOP.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_MACHCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_MACHCODE.

  SELECT IDALGODOEIRA
         ALGODOEIRADESCRICAO  FROM  ZTSAFRAFARDOS
    INTO TABLE IT_ZTSAFRAFARDOS.

  SORT  IT_ZTSAFRAFARDOS BY IDALGODOEIRA.
  DELETE ADJACENT DUPLICATES FROM IT_ZTSAFRAFARDOS.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'IDALGODOEIRA'
      WINDOW_TITLE    = 'ID Algodoeira'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = IT_ZTSAFRAFARDOS
      RETURN_TAB      = IT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  IF SY-SUBRC = 0.
    READ TABLE IT_RETURN INTO ST_RETURN  INDEX 1.
    ID_ALDO-LOW = ST_RETURN-FIELDVAL.
  ELSE.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHARG_BRANCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHARG_BRANCO .

  FREE IT_0002_AUX.

  IT_0002_AUX = IT_0002.

  DELETE IT_0002_AUX WHERE CHARG <> ' '.

  CHECK IT_0002_AUX IS NOT INITIAL.

  SELECT *
   FROM ZPPS_XIMFBF_LOG
   INTO TABLE @DATA(IT_ZPPS_XIMFBF_LOG)
   FOR ALL ENTRIES IN @IT_0002_AUX
   WHERE ID_COTTON = @IT_0002_AUX-ID_COTTON
   AND NOT EXISTS ( SELECT * FROM MSEG WHERE MSEG~SMBLN = ZPPS_XIMFBF_LOG~MBLNR  ).

  SELECT *
    FROM ZMMT0006 AS M1
    INTO TABLE @DATA(IT_ZMMT0006)
    FOR ALL ENTRIES IN @IT_0002_AUX
    WHERE ID_COTTON = @IT_0002_AUX-ID_COTTON
    AND  CH_REFERENCIA = ( SELECT MAX( M2~CH_REFERENCIA ) FROM ZMMT0006 AS M2 WHERE M2~ID_COTTON = M1~ID_COTTON ) .

  DELETE IT_ZMMT0006 WHERE TCODE  = 'MBST'.

  SORT IT_ZMMT0006 BY ID_COTTON.
  SORT IT_ZPPS_XIMFBF_LOG BY ID_COTTON.

  LOOP AT IT_0002_AUX ASSIGNING FIELD-SYMBOL(<FS0002>).
    IF <FS0002>-ID_COTTON IS NOT INITIAL.
      READ TABLE IT_ZPPS_XIMFBF_LOG INTO DATA(WA_ZPPS_XIMFBF_LOG) WITH KEY ID_COTTON = <FS0002>-ID_COTTON.
      IF SY-SUBRC IS INITIAL.
        <FS0002>-CHARG = WA_ZPPS_XIMFBF_LOG-CHARG.
      ELSE.
        READ TABLE IT_ZMMT0006 INTO DATA(WA_ZMMT0006)  WITH KEY ID_COTTON = <FS0002>-ID_COTTON.
        IF SY-SUBRC IS INITIAL.
          <FS0002>-CHARG = WA_ZMMT0006-BATCH_D.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_0002_AUX ASSIGNING <FS0002>.
    MODIFY IT_0002 FROM <FS0002>.
  ENDLOOP.

ENDFORM.
