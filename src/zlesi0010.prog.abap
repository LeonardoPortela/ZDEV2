*&---------------------------------------------------------------------*
*& Report  ZLESI0010                                                   *
*&---------------------------------------------------------------------*
* Autor      : Antonio Luiz Rodrigues da Silva                         *
* Data       : 15/05/2014                                              *
* Descrição  : Converte arquivo Excel em arquivo tipo Texto            *
* Transação  :                                                         *
*----------------------------------------------------------------------*

REPORT  ZLESI0010 MESSAGE-ID ZLES.

TYPES: BEGIN OF TY_MODAL,
      NOME_EMPRESA(70),   "– nome da empresa emissora do arquivo
      CNPJ_EMPRESA(14),   "– cnpj da empresa emissora do arquivo
      TP_OPERACAO(1),     "- tipo de operação “E” – entrada ou “S”- saída
      MODAL(2),           "- rodoviário , ferroviário ou aquaviário
      DT_ENVIO(10),       "- data de criação do arquivo
      HR_ENVIO(5),        "- hora de criação do arquivo
      PRODUTO(6),         "- descritivo breve do produto
      DT_CARGA(10),       "- data de carga, para os arquivos de saída “S”
      HR_CARGA(5),        "- data de carga, para os arquivos de saída “S”
      DT_DESCARGA(10),    "- data de descarga, para os arquivos de entrada  “E”
      HR_DESCARGA(5),     "- hora de descarga, para os arquivos de entrada “E”
      PLACA(11),          "- identificação do veiculo ( rodoviário, ferroviário ou auaviário)
      PESO(14),           "- peso de carga ou de descarga.
      PESO_NF(14),        "- peso das Notas fiscais
      NR_NF(11),          "- número da nota fiscal
      SERIE_NF(3),        "- série da nota fiscal
      DT_NF(10),           "- data de emissão da nota fiscal
      CPF_CNPJ_EMITENTE(14), "- cnpj do emitente da nota fiscal
      CNPJ_COEMITENTE(14),    "- cnpj do comprador da mercadoria
      DCL(10),            "- número do DCL no embarque ferroviário
      SERIE_DCL(3),       "- série do DCL no embarque ferroviário
      PESO_VAGAO(14),     "- peso do vagão L2 e L3
    END OF TY_MODAL,

    BEGIN OF Y_FILE,
       LINHA(400),
    END OF Y_FILE,

    TY_ARQUIVO(205)               TYPE C,

    BEGIN OF Y_REG10,
           TIPO(2),
           MOVTO(1),
           EMPRESA(70),
           CNPJ(14),
           DATAENV(10),
           HORAENV(8),
           OBSERV(100),
       END OF Y_REG10,

       BEGIN OF Y_REG20,
           TIPO(2),
           IDVAGAO(11),
           DCL(10),
           PESOVAGAO(14),
           DATADESC(10),
           HORADESC(5),
           SERIEDCL(3),
       END OF Y_REG20,

       BEGIN OF Y_REG20_L3,
           TIPO(2),
           IDVAGAO(10),
           DCL(11),
           PESOVAGAO(14),
           DATADESC(10),
           HORADESC(5),
           SERIEDCL(3),
       END OF Y_REG20_L3,

       BEGIN OF Y_REG30,
           TIPO(2),
           NF(11),
           PESONOTA(14),
           PESOCHGD(14),
           DATACHGD(10),
           CNPJ(14),
           COMPL(54),
           PROD(6),
       END OF Y_REG30.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*


DATA:  T_FILE         TYPE STANDARD TABLE OF Y_FILE
                           WITH HEADER LINE INITIAL SIZE 0,
      T_FILE_TRANSF   TYPE TABLE OF Y_FILE.

DATA: T_FILES_UNIX    TYPE TABLE OF EPSFILI,
      T_DIR_UNIX      TYPE TABLE OF EPSFILI,
      T_DIR_LOCAL     TYPE TABLE OF SDOKPATH,
      T_DIR_LOC_F     TYPE TABLE OF SDOKPATH,
      T_ARQUIVO       TYPE TABLE OF TY_ARQUIVO,
      T_ZLEST0007     TYPE TABLE OF ZLEST0007,
      T_ZLEST0008     TYPE TABLE OF ZLEST0008.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_MODAL      TYPE TY_MODAL,
      VMSG(50).


DATA: ST_FILES_DOC    TYPE SDOKPATH,
      ST_FILES_UNIX   TYPE EPSFILI,
      ST_MESS         TYPE ZLEST0008,
      ST_REG10        TYPE Y_REG10,
      ST_REG20        TYPE Y_REG20,
      ST_REG20_L3     TYPE Y_REG20_L3,
      ST_REG30        TYPE Y_REG30,
      W_ARQUIVO       TYPE TY_ARQUIVO,
      ST_ZLEST0007    TYPE ZLEST0007,
      ST_ZLEST0008    TYPE ZLEST0008,
      VL_CTRLFILE     TYPE C.
*----------------------------------------------------------------------*
* Variaveis                                                            *
*----------------------------------------------------------------------*
DATA: V_CAMINHO      TYPE EPSF-EPSDIRNAM,
      V_MENSAGEM     TYPE BAPI_MSG,
      V_MSG(255)     TYPE C,
      V_ERRO         TYPE C,
      V_PREFIX_ENT   TYPE ZLEST0007-PREFIX,
      V_PREFIX_LOG   TYPE ZLEST0007-PREFIX,
      V_PREFIX_PROC  TYPE ZLEST0007-PREFIX,
      V_VERSION      TYPE ZLEST0008-IDCTRL.
*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: C_X            TYPE C VALUE 'X',
           C_LOG(10)      TYPE C VALUE 'LOG',
           C_PROC(10)     TYPE C VALUE 'PROC',
           C_ENT(10)      TYPE C VALUE 'ENT',
           C_L1(2)        TYPE C VALUE 'L1',      " Confirmação de saída Ferroviário
           C_ASC(10)      TYPE C VALUE 'ASC',
           C_MASK_LOC(6)  TYPE C VALUE '*.xlsx',
           C_MASK_UNIX(6) TYPE C VALUE '*.*',
           C_U            TYPE C VALUE 'U',
           C_W            TYPE C VALUE 'W',
           C_L            TYPE C VALUE 'L',
           C_E            TYPE C VALUE 'E',
           C_S            TYPE C VALUE 'S'.


*----------------------------------------------------------------------*
* PROCESSAMENTO ARQUIVOS DIRETORIO
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME TITLE TEXT-002.
PARAMETERS: P_INPUT(60) TYPE C MODIF ID FIL,
            P_PROC(60)  TYPE C MODIF ID FIL,
            P_LOG(60)   TYPE C MODIF ID FIL,
            P_CHKSO(1)  TYPE C NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B.


*----------------------------------------------------------------------*
* Evento de tela                                                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'FIL'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF P_INPUT IS INITIAL AND P_PROC IS INITIAL AND P_LOG IS INITIAL
    OR T_ZLEST0007[] IS INITIAL.
    PERFORM VALIDA_TELA_SELECAO.
  ELSE.
    PERFORM BUSCA_FILE.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  VALIDA_TELA_SELECAO
*&---------------------------------------------------------------------*
FORM VALIDA_TELA_SELECAO .

  CLEAR V_ERRO.
  PERFORM SELECIONA_INTERFACE.

  IF V_ERRO IS INITIAL.
    PERFORM BUSCA_FILE.
  ELSE.
    CLEAR: P_INPUT,
           P_PROC,
           P_LOG,
           P_CHKSO.
  ENDIF.

ENDFORM.                    " VALIDA_TELA_SELECAO

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INTERFACE
*&---------------------------------------------------------------------*
FORM SELECIONA_INTERFACE .

  DATA: V_LIFNR TYPE LFA1-LIFNR.

  CLEAR: P_CHKSO,
         T_ZLEST0007[].

  SELECT SINGLE *
    FROM ZLEST0007
    INTO ST_ZLEST0007
   WHERE ID_INTERFACE = SY-REPID
     AND ID_CTG = C_ENT
     AND PREFIX = C_L1
     AND VLDE <= SY-DATUM
     AND VLATE >= SY-DATUM.

  IF SY-SUBRC IS INITIAL.
    APPEND ST_ZLEST0007 TO T_ZLEST0007.
  ELSE.
    MESSAGE W026 WITH C_ENT 'ZLES0009' INTO V_MENSAGEM.
    PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                         C_E
                                         '999'
                                         V_MENSAGEM.
    MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
    V_ERRO = C_X.
    EXIT.
  ENDIF.

  IF V_ERRO IS INITIAL.
    SELECT SINGLE *
     FROM ZLEST0007
     INTO  ST_ZLEST0007
    WHERE ID_INTERFACE = SY-REPID
      AND ID_CTG = C_LOG
      AND VLDE <= SY-DATUM
      AND VLATE >= SY-DATUM.

    IF SY-SUBRC IS INITIAL.
      APPEND ST_ZLEST0007 TO T_ZLEST0007.
    ELSE.
      MESSAGE W026 WITH C_LOG 'ZLES0009' INTO V_MENSAGEM.
      PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                           C_E
                                           '999'
                                           V_MENSAGEM.
      MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
      V_ERRO = C_X.
      EXIT.
    ENDIF.
  ENDIF.

  IF V_ERRO IS INITIAL.
    SELECT SINGLE *
      FROM ZLEST0007
      INTO  ST_ZLEST0007
     WHERE ID_INTERFACE = SY-REPID
       AND ID_CTG = C_PROC
       AND VLDE <= SY-DATUM
       AND VLATE >= SY-DATUM.

    IF SY-SUBRC IS INITIAL.
      APPEND ST_ZLEST0007 TO T_ZLEST0007.
    ELSE.
      MESSAGE W026 WITH C_PROC 'ZLES0009' INTO V_MENSAGEM.
      PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                           C_E
                                           '999'
                                           V_MENSAGEM.
      MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
      V_ERRO = C_X.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECIONA_INTERFACE

*----------------------------------------------------------------------*
* Start of Selection                                                   *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM LE_DIRETORIO.

*&---------------------------------------------------------------------*
*&      Form  LE_ARQUIVO
*&---------------------------------------------------------------------*
FORM LE_DIRETORIO .

  DATA: V_INDEX         TYPE SY-TABIX,
        V_MASK_UNIX     TYPE EPSFILNAM,
        V_MASK_LOCL(60) TYPE C,
        V_ERRO_LOG.

  CHECK: NOT P_INPUT IS INITIAL.



*  CONCATENATE V_PREFIX_ENT C_MASK_LOC INTO V_MASK_LOCL.
  CONCATENATE '' C_MASK_LOC INTO V_MASK_LOCL.

  CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
    EXPORTING
      DIRECTORY        = P_INPUT
      FILTER           = V_MASK_LOCL
*     IMPORTING
*       FILE_COUNT       =
*       DIR_COUNT        =
    TABLES
      FILE_TABLE       = T_DIR_LOC_F
      DIR_TABLE        = T_DIR_LOCAL
 EXCEPTIONS
   CNTL_ERROR       = 1
   OTHERS           = 2.

  IF SY-SUBRC <> 0 OR T_DIR_LOC_F[] IS INITIAL.

    MESSAGE W000
       WITH 'Diretório Local: ' P_INPUT
            ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
            V_MASK_LOCL
        INTO V_MENSAGEM.
    PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                         C_E
                                         '999'
                                         V_MENSAGEM.
    MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.

    LEAVE LIST-PROCESSING.

  ELSE.

*     Consiste arquivo apto para processamento
    LOOP AT T_DIR_LOC_F INTO ST_FILES_DOC.
      IF 'L1_L2_L3' CS ST_FILES_DOC-PATHNAME+0(2).
        PERFORM CARREGA_ARQ USING ST_FILES_DOC-PATHNAME.
        IF NOT V_ERRO IS INITIAL.
          V_ERRO_LOG = C_X.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.



* Verifica se houve erro em algum processamento...
  IF V_ERRO_LOG IS INITIAL.
    MESSAGE S000(ZLES) WITH 'Arquivos processado!'
                       DISPLAY LIKE C_S.
  ELSE.
    MESSAGE S000(ZLES)
       WITH 'Existem arquivos/registros que não foram'
            'processados,'
            'verificar transaçãö LOG ZLES0010'
       DISPLAY LIKE C_E.
  ENDIF.

ENDFORM.                    " LE_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  CARREGA_ARQ
*&---------------------------------------------------------------------*
FORM CARREGA_ARQ  USING    V_FILE.

  DATA: ST_FILE       TYPE Y_FILE,
        P_FILE        TYPE RLGRAP-FILENAME,
        VL_INDEX      TYPE SY-INDEX,
        VL_TABIX      TYPE SY-TABIX,
        VIDVAGAO(11).


  CLEAR:    ST_REG10,
            ST_REG20,
            ST_REG30.


  DATA: T_EXCEL   LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE ,
        T_EXCEL2  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE .


  CLEAR T_EXCEL.
  REFRESH: T_EXCEL, T_ARQUIVO.

  CONCATENATE P_INPUT V_FILE INTO P_FILE.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 22
      I_END_ROW               = 1000
    TABLES
      INTERN                  = T_EXCEL
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = 'Atualizando Dados'.

  T_EXCEL2[] = T_EXCEL[].
  SORT T_EXCEL2 BY ROW COL.
  CLEAR: T_EXCEL2, VL_TABIX.

  LOOP AT T_EXCEL.
    IF T_EXCEL-ROW = T_EXCEL2-ROW.
      CONTINUE.
    ENDIF.
    WA_MODAL-DCL = '0000000000'.
    WA_MODAL-SERIE_DCL = '000'.
    LOOP AT T_EXCEL2 WHERE ROW = T_EXCEL-ROW.
      CASE T_EXCEL2-COL.
        WHEN 1.
          WA_MODAL-NOME_EMPRESA = T_EXCEL2-VALUE.
        WHEN 2.
          WA_MODAL-CNPJ_EMPRESA  = T_EXCEL2-VALUE.
        WHEN 3.
          WA_MODAL-TP_OPERACAO  = T_EXCEL2-VALUE.
        WHEN 4.
          WA_MODAL-MODAL        = T_EXCEL2-VALUE.
        WHEN 5.
          WA_MODAL-DT_ENVIO     = T_EXCEL2-VALUE.
        WHEN 6.
          WA_MODAL-HR_ENVIO     = T_EXCEL2-VALUE.
        WHEN 7.
          WA_MODAL-PRODUTO      = T_EXCEL2-VALUE.
        WHEN 8.
          WA_MODAL-DT_CARGA     = T_EXCEL2-VALUE.
        WHEN 9.
          WA_MODAL-HR_CARGA     = T_EXCEL2-VALUE.
        WHEN 10.
          WA_MODAL-DT_DESCARGA  = T_EXCEL2-VALUE.
        WHEN 11.
          WA_MODAL-HR_DESCARGA  = T_EXCEL2-VALUE.
        WHEN 12.
          WA_MODAL-PLACA        = T_EXCEL2-VALUE.
        WHEN 13.
          WA_MODAL-PESO         = T_EXCEL2-VALUE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_MODAL-PESO
            IMPORTING
              OUTPUT = WA_MODAL-PESO.
        WHEN 14.
          WA_MODAL-PESO_NF      = T_EXCEL2-VALUE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_MODAL-PESO_NF
            IMPORTING
              OUTPUT = WA_MODAL-PESO_NF.
        WHEN 15.
          WA_MODAL-NR_NF        = T_EXCEL2-VALUE.
        WHEN 16.
          WA_MODAL-SERIE_NF     = T_EXCEL2-VALUE.
        WHEN 17.
          WA_MODAL-DT_NF        = T_EXCEL2-VALUE.
        WHEN 18.
          WA_MODAL-CPF_CNPJ_EMITENTE = T_EXCEL2-VALUE.
        WHEN 19.
          WA_MODAL-CNPJ_COEMITENTE   = T_EXCEL2-VALUE.
        WHEN 20.
          WA_MODAL-DCL         = T_EXCEL2-VALUE.
          IF V_FILE+0(2) = 'L2'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                INPUT  = WA_MODAL-DCL
              IMPORTING
                OUTPUT = WA_MODAL-DCL.
          ENDIF.
          IF WA_MODAL-DCL IS INITIAL OR WA_MODAL-DCL = ''.
            WA_MODAL-DCL = '0000000000'.
          ENDIF.
        WHEN 21.
          WA_MODAL-SERIE_DCL   = T_EXCEL2-VALUE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_MODAL-SERIE_DCL
            IMPORTING
              OUTPUT = WA_MODAL-SERIE_DCL.
        WHEN 22.
          WA_MODAL-PESO_VAGAO         = T_EXCEL2-VALUE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_MODAL-PESO_VAGAO
            IMPORTING
              OUTPUT = WA_MODAL-PESO_VAGAO.
      ENDCASE.
    ENDLOOP.
    CONCATENATE 'ARQ.' V_FILE ' Linha ' T_EXCEL-ROW  INTO VMSG SEPARATED BY SPACE.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        TEXT = VMSG.

    IF VL_TABIX = 0.
      ST_REG10-TIPO         = '10'.
      ST_REG10-MOVTO        = WA_MODAL-TP_OPERACAO.
      ST_REG10-EMPRESA      = WA_MODAL-NOME_EMPRESA.
      ST_REG10-CNPJ         = WA_MODAL-CNPJ_EMPRESA.
      ST_REG10-DATAENV      = WA_MODAL-DT_ENVIO.
      ST_REG10-HORAENV      = WA_MODAL-HR_ENVIO.
      W_ARQUIVO = ST_REG10.
      APPEND W_ARQUIVO TO T_ARQUIVO.
    ENDIF.
    ADD 1 TO VL_TABIX.

    IF VIDVAGAO NE WA_MODAL-PLACA AND WA_MODAL-PLACA IS NOT INITIAL.
      ST_REG20-TIPO         = '20'.
      ST_REG20-IDVAGAO      = WA_MODAL-PLACA.
      ST_REG20-DCL          = WA_MODAL-DCL.
      IF WA_MODAL-PESO_VAGAO NE '' AND WA_MODAL-PESO_VAGAO NE '00000000000000'.
        ST_REG20-PESOVAGAO    = WA_MODAL-PESO_VAGAO.
      ELSE.
        ST_REG20-PESOVAGAO    = WA_MODAL-PESO.
      ENDIF.
      IF WA_MODAL-DT_CARGA IS NOT INITIAL.
        ST_REG20-DATADESC     = WA_MODAL-DT_CARGA.
        ST_REG20-HORADESC     = WA_MODAL-HR_CARGA.
      ELSE.
        ST_REG20-DATADESC     = WA_MODAL-DT_DESCARGA.
        ST_REG20-HORADESC     = WA_MODAL-HR_DESCARGA.
      ENDIF.

      ST_REG20-SERIEDCL     = WA_MODAL-SERIE_DCL.
      MOVE-CORRESPONDING ST_REG20 TO ST_REG20_L3.
      IF V_FILE+0(2) = 'L3'.
        W_ARQUIVO = ST_REG20_L3.
      ELSE.
        W_ARQUIVO = ST_REG20.
      ENDIF.

      APPEND W_ARQUIVO TO T_ARQUIVO.
    ENDIF.
    VIDVAGAO = WA_MODAL-PLACA.

    ST_REG30-TIPO         = '30'.
    ST_REG30-NF           = WA_MODAL-NR_NF.
    ST_REG30-PESONOTA     = WA_MODAL-PESO_NF.
    ST_REG30-PESOCHGD     = WA_MODAL-PESO.
    ST_REG30-DATACHGD     = WA_MODAL-DT_NF.
    ST_REG30-CNPJ         = WA_MODAL-CPF_CNPJ_EMITENTE.
    ST_REG30-COMPL        = '000000000000000000000000000000000000000000000000000000'.
    ST_REG30-PROD         = WA_MODAL-PRODUTO.
    W_ARQUIVO = ST_REG30.
    APPEND W_ARQUIVO TO T_ARQUIVO.
  ENDLOOP.

  IF T_ARQUIVO[] IS NOT INITIAL.
    PERFORM ZF_GRAVA_ARQUIVO USING    P_FILE V_FILE.
  ENDIF.

ENDFORM.                    " CARREGA_ARQ


*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_GRAVA_ARQUIVO USING    V_FILE V_FILE2.
  DATA:
     WL_TABNAME(15),
     WL_INDEX(2),
     LV_NOME_ARQUIVO     TYPE STRING.

  FIELD-SYMBOLS <FS_TAB> TYPE ANY TABLE.

  WL_TABNAME = 't_arquivo[]'.
  ASSIGN (WL_TABNAME) TO <FS_TAB>.
  IF SY-SUBRC = 0.
    IF NOT <FS_TAB> IS INITIAL.
      MOVE V_FILE  TO LV_NOME_ARQUIVO.
      REPLACE 'xlsx' WITH 'txt'
            INTO LV_NOME_ARQUIVO.

      MOVE <FS_TAB>        TO T_ARQUIVO[].

      DATA: WL_FILENAME TYPE RLGRAP-FILENAME.
      MOVE: LV_NOME_ARQUIVO TO WL_FILENAME.

      CALL FUNCTION 'WS_DOWNLOAD'
        EXPORTING
          FILENAME                = WL_FILENAME
        TABLES
          DATA_TAB                = T_ARQUIVO
        EXCEPTIONS
          FILE_OPEN_ERROR         = 1
          FILE_WRITE_ERROR        = 2
          INVALID_FILESIZE        = 3
          INVALID_TYPE            = 4
          NO_BATCH                = 5
          UNKNOWN_ERROR           = 6
          INVALID_TABLE_WIDTH     = 7
          GUI_REFUSE_FILETRANSFER = 8
          CUSTOMER_ERROR          = 9
          NO_AUTHORITY            = 10
          OTHERS                  = 11.
      IF SY-SUBRC <> 0.
        MESSAGE ID '00' TYPE 'E' NUMBER '398' WITH
           'Erro ao criar o arquivo'
           'na pasta'
          V_FILE .
      ELSE.
        PERFORM TRANSFERE_FILE USING P_INPUT P_LOG P_PROC  V_FILE2 SPACE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ZF_GRAVA_ARQUIVO


*&---------------------------------------------------------------------*
*&      Form  ENVIA_MENSAGEM_PROCTO
*&---------------------------------------------------------------------*
FORM ENVIA_MENSAGEM_PROCTO  USING P_FILENAME
                                  P_MSGTYP
                                  P_MSGNR
                                  P_MSGV1.

  STATICS: LV_FILENAME  TYPE EPSFILNAM VALUE %_MAXCHAR,
           LV_VERSION   TYPE ZIDCTRL,
           LV_VCONT     TYPE NUMC10.

  IF LV_FILENAME <> P_FILENAME.

    CLEAR: ST_ZLEST0008,
           LV_VCONT.

    IF P_FILENAME = SY-REPID.

      SELECT MAX( IDCTRL ) MAX( CONT )
        INTO (ST_ZLEST0008-IDCTRL, LV_VCONT)
        FROM ZLEST0008
       WHERE FILENAME = P_FILENAME
        GROUP BY IDCTRL.
      ENDSELECT.

      IF SY-SUBRC IS INITIAL.
        IF LV_VCONT >= '9999999998'.
          LV_VERSION = ST_ZLEST0008-IDCTRL + 1.
          CLEAR LV_VCONT.
        ELSE.
          LV_VERSION = ST_ZLEST0008-IDCTRL.
        ENDIF.
      ELSE.
        LV_VERSION = ST_ZLEST0008-IDCTRL + 1.
      ENDIF.

    ELSE.
      READ TABLE T_ZLEST0008 INTO ST_ZLEST0008 WITH KEY FILENAME = P_FILENAME.
      LV_VERSION = ST_ZLEST0008-IDCTRL + 1.
    ENDIF.

    LV_FILENAME = P_FILENAME.
  ENDIF.


  ADD 1 TO LV_VCONT.
  ST_MESS-FILENAME = P_FILENAME.
  ST_MESS-IDCTRL   = LV_VERSION.
  ST_MESS-TCODE    = SY-TCODE.
  ST_MESS-CONT     = LV_VCONT.
  ST_MESS-DYNAME   = 'LES'.
  ST_MESS-MSGTYP   = P_MSGTYP.
  ST_MESS-MSGSPRA  = SY-LANGU.
  ST_MESS-MSGID    = 'FR'.
  ST_MESS-MSGNR    = P_MSGNR.
  ST_MESS-MSGV1    = P_MSGV1.
  ST_MESS-DATA     = SY-DATUM.
  ST_MESS-HORA     = SY-UZEIT.
  ST_MESS-USUARIO  = SY-UNAME.
  ST_MESS-LOTE     = SPACE.

  INSERT ZLEST0008 FROM ST_MESS.
  CLEAR ST_MESS.

ENDFORM.                    " ENVIA_MENSAGEM_PROCTO


*&---------------------------------------------------------------------*
*&      Form  BUSCA_FILE_LOCAL
*&---------------------------------------------------------------------*
FORM BUSCA_FILE .

  CHECK: P_CHKSO IS INITIAL.

  CLEAR: P_INPUT,
         P_LOG,
         P_PROC.

  PERFORM PREENCHE_CAMINHO USING: C_ENT  CHANGING P_INPUT,
                                  C_LOG  CHANGING P_LOG,
                                  C_PROC CHANGING P_PROC.

  IF P_INPUT IS INITIAL OR P_LOG  IS INITIAL OR P_PROC IS INITIAL.
    CLEAR: P_CHKSO.

    MESSAGE W004 INTO V_MENSAGEM.
    PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                         C_E
                                         '999'
                                         V_MENSAGEM.
    MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
  ENDIF.

ENDFORM.                    " BUSCA_FILE_LOCAL

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMINHO
*&---------------------------------------------------------------------*
FORM PREENCHE_CAMINHO  USING    V_CATEG
                       CHANGING V_PATH.

  READ TABLE T_ZLEST0007 INTO ST_ZLEST0007 WITH KEY ID_CTG = V_CATEG.

  IF NOT ST_ZLEST0007-PATHWIN IS INITIAL.
    V_PATH = ST_ZLEST0007-PATHWIN.
    P_CHKSO = C_W.
  ENDIF.

  CASE ST_ZLEST0007-ID_CTG.
    WHEN C_LOG.
      V_PREFIX_LOG = ST_ZLEST0007-PREFIX.
    WHEN C_PROC.
      V_PREFIX_PROC = ST_ZLEST0007-PREFIX.
    WHEN C_ENT.
      V_PREFIX_ENT = ST_ZLEST0007-PREFIX.
  ENDCASE.

  CLEAR ST_ZLEST0007.

ENDFORM.                    " PREENCHE_CAMINHO

*&---------------------------------------------------------------------*
*&      Form  TRANSFERE_FILE
*&---------------------------------------------------------------------*
FORM TRANSFERE_FILE  USING  V_ORIG
                            V_LOG
                            V_PROC
                            V_FILE
                            V_ERRO.

  DATA: V_DEST    TYPE STRING,
        V_SOUR    TYPE STRING,
        V_DEST1   TYPE STRING,
        V_SOUR1   TYPE STRING,
        V_RC_BOOL TYPE C,
        V_RC_NUM  TYPE I,
        LI_NREG1  TYPE I,
        LI_NREG2  TYPE I,
        L_PATH    TYPE STRING.

  CLEAR: V_DEST, VL_CTRLFILE.

* Identifica o destino da transferência
  IF V_ERRO = C_X.
    V_DEST = V_LOG.
    V_SOUR = V_ORIG.
  ELSE.
    V_DEST = V_PROC.
    V_SOUR = V_ORIG.
  ENDIF.

* Gera o Path completo
  CONCATENATE V_DEST   V_FILE INTO V_DEST1.
  CONCATENATE V_ORIG   V_FILE INTO V_SOUR1.
  CONDENSE V_DEST NO-GAPS.
  CONDENSE V_SOUR NO-GAPS.


*   Verifica a existência do diretório
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
    EXPORTING
      DIRECTORY = V_DEST
    RECEIVING
      RESULT    = V_RC_BOOL.

*     Cria o diretório para transferência
  IF V_RC_BOOL IS INITIAL.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE
      EXPORTING
        DIRECTORY = V_DEST
      CHANGING
        RC        = V_RC_NUM.

  ENDIF.

*   Transfere o arquivo para o diretório
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_COPY
    EXPORTING
      SOURCE      = V_SOUR1
      DESTINATION = V_DEST1
      OVERWRITE   = 'X'.

*   Elimina o arquivo de origem
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
    EXPORTING
      FILENAME = V_SOUR1
    CHANGING
      RC       = V_RC_NUM.

  VL_CTRLFILE = C_X.

ENDFORM.                    " TRANSFERE_FILE
