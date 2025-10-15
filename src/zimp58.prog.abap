************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 08.03.2013                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Geração de Arquivo de Tributos                      *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 08.03.2013    Antonio Luiz R Silva  Criação                          *
************************************************************************

REPORT  ZIMP58.

TABLES: ZIMP_TIPOS_IMPOS.
*-----------------------------------------------------------------------
* Tipos
*-----------------------------------------------------------------------
TYPES:

  BEGIN OF TY_TP_ARREC_1_E_2,
    IDENT_TRIBUTO               TYPE C,
    NOME_CLIENTE(40)            TYPE C,
    END_CLIENTE(40)             TYPE C,
    CEP(8)                      TYPE C,
    UF_CLIENTE(2)               TYPE C,
    CIDADE_CLIENTE(20)          TYPE C,
    BAIRRO_CLIENTE(20)          TYPE C,
    TIPO_INSCRICAO              TYPE C,
    NUM_INSCRICAO(15)           TYPE C,
    TEL_CLIENTE_PAGADOR(20)     TYPE C,
    DATA_PAGAMENTO_TRIBUTO(8)   TYPE N,
    AUTORIZA_PAGAMENTO          TYPE C,
    VALOR_PRINCIPAL(15)         TYPE N,
    VALOR_JUROS_ENCARGOS(15)    TYPE N,
    VALOR_MULTA(15)             TYPE N,
    RESERVA_1(15)               TYPE C,
    DATA_VENC_TRIBUTO(8)        TYPE N,
    CODIGO_RECEITA(4)           TYPE N,
    PERIODO_APURACAO(8)         TYPE N,
    PERCENTUAL(4)               TYPE N,
    REFERENCIA(17)              TYPE C,
    RECEITA_BRUTA_ACUMULADA(15) TYPE N,
    USO_EMPRESA(80)             TYPE C,
    RESERVA(49)                 TYPE C,
    AGENCIA(4)                  TYPE N,
    CONTA(7)                    TYPE N,
    MESTRE(8)                   TYPE N,
    RESERVA_2(23)               TYPE C,
  END OF TY_TP_ARREC_1_E_2,

  BEGIN OF TY_TP_ARREC_3,
    IDENT_TRIBUTO               TYPE C,
    NOME_CLIENTE(40)            TYPE C,
    END_CLIENTE(40)             TYPE C,
    CEP(8)                      TYPE C,
    UF_CLIENTE(2)               TYPE C,
    CIDADE_CLIENTE(20)          TYPE C,
    BAIRRO_CLIENTE(20)          TYPE C,
    TIPO_INSCRICAO              TYPE C,
    NUM_INSCRICAO(15)           TYPE C,
    TEL_CLIENTE_PAGADOR(20)     TYPE C,
    DATA_PAGAMENTO_TRIBUTO(8)   TYPE N,
    AUTORIZA_PAGAMENTO          TYPE C,
    VALOR_INSS(15)              TYPE N,
    VALOR_MONET_JUROS_MULTA(15) TYPE N,
    VALOR_OUTRAS_ENTIDADES(15)  TYPE N,
    VALOR_TOTAL(15)             TYPE N,
    CODIGO_RECEITA(4)           TYPE N,
    TIPO_IDENTIFICADOR(2)       TYPE C,
    IDENTIFICADOR(14)           TYPE C,
    ANO_COMPETENCIA(4)          TYPE C,
    MES_COMPETENCIA(2)          TYPE C,
    USO_EMPRESA(80)             TYPE C,
    NOME_RECOLHEDOR(40)         TYPE C,
    RESERVA_1(39)               TYPE C,
    AGENCIA(4)                  TYPE N,
    CONTA(7)                    TYPE N,
    MESTRE(8)                   TYPE N,
    RESERVA_2(23)               TYPE C,
  END OF TY_TP_ARREC_3,

  BEGIN OF TY_TP_ARREC_4_E_5,
    IDENT_TRIBUTO               TYPE C,
    NOME_CLIENTE(40)            TYPE C,
    END_CLIENTE(40)             TYPE C,
    CEP(8)                      TYPE C,
    UF_CLIENTE(2)               TYPE C,
    CIDADE_CLIENTE(20)          TYPE C,
    BAIRRO_CLIENTE(20)          TYPE C,
    TIPO_INSCRICAO              TYPE C,
    NUM_INSCRICAO(15)           TYPE C,
    TEL_CLIENTE_PAGADOR(20)     TYPE C,
    DATA_PAGAMENTO_TRIBUTO(8)   TYPE N,
    AUTORIZA_PAGAMENTO          TYPE C,
    VALOR_PRINCIPAL(15)         TYPE N,
    VALOR_JUROS_ENCARGOS(15)    TYPE N,
    VALOR_MULTA(15)             TYPE N,
    VALOR_ACRESCIMO_FIN(15)     TYPE N,
    VALOR_HONORARIOS_ADV(15)    TYPE N,
    RESERVA_1(17)               TYPE C,
    DATA_VENC_TRIBUTO(8)        TYPE N,
    CODIGO_RECEITA(4)           TYPE N,
    GRUPO_RECEITA               TYPE C,
    MES(2)                      TYPE N,
    ANO(4)                      TYPE N,
    INSCRICAO_ESTADUAL(12)      TYPE C,
    INSCRICAO_DIVIDA_ATIVA(13)  TYPE C,
    NUMERO_PARCELAMENTO(13)     TYPE N,
    RESERVA_2(2)                TYPE C,
    USO_EMPRESA(40)             TYPE C,
    OBSERVACOES(40)             TYPE C,
    CNAE(7)                     TYPE C,
    PLACA(7)                    TYPE C,
    AGENCIA(4)                  TYPE N,
    CONTA(7)                    TYPE N,
    MESTRE(8)                   TYPE N,
    RESERVA(23)                 TYPE C,
  END OF TY_TP_ARREC_4_E_5,

  BEGIN OF TY_TP_ARREC_6,
    IDENT_TRIBUTO               TYPE C,
    COD_BARRAS(48)              TYPE N,
    DATA_PAGAMENTO_TRIBUTO(8)   TYPE N,
    DATA_VENCIMENTO(8)          TYPE N,
    RESERVA(298)                TYPE C,
    AGENCIA(4)                  TYPE N,
    CONTA(7)                    TYPE N,
    MESTRE(8)                   TYPE N,
    AUTORIZA_PAGAMENTO(1)       TYPE C,
    USO_EMPRESA(80)             TYPE C,
  END OF TY_TP_ARREC_6,

  BEGIN OF TY_TP_ARREC_7,
    IDENT_TRIBUTO               TYPE C,
    COD_BARRAS(48)              TYPE N,
    DATA_PAGAMENTO_TRIBUTO(8)   TYPE N,
    DATA_VENCIMENTO(8)          TYPE N,
    VALOR_MULTA(15)             TYPE N,
    VALOR_JUROS(15)             TYPE N,
    RESERVA_1(45)               TYPE N,
    IDENTIFICADOR(20)           TYPE C,
    RESERVA_2(203)              TYPE C,
    AGENCIA(4)                  TYPE N,
    CONTA(7)                    TYPE N,
    MESTRE(8)                   TYPE N,
    AUTORIZA_PAGAMENTO(1)       TYPE C,
    USO_EMPRESA(80)             TYPE C,
  END OF TY_TP_ARREC_7,

  BEGIN OF TY_TP_ARREC_8_9_10_11,
    IDENT_TRIBUTO               TYPE C,
    COD_BARRAS(48)              TYPE N,
    DATA_PAGAMENTO_TRIBUTO(8)   TYPE N,
    RESERVA_1(15)               TYPE C,
    AUTORIZA_PAGAMENTO(1)       TYPE C,
    RESERVA_2(291)              TYPE C,
    AGENCIA(4)                  TYPE N,
    CONTA(7)                    TYPE N,
    MESTRE(8)                   TYPE N,
    USO_EMPRESA(80)             TYPE C,
  END OF TY_TP_ARREC_8_9_10_11 ,

  BEGIN OF TY_TP_ARREC_12,
    IDENT_TRIBUTO               TYPE C,
    RESERVA_1(146)              TYPE C,
    DATA_PAGAMENTO_TRIBUTO(8)   TYPE N,
    AUTORIZA_PAGAMENTO(1)       TYPE C,
    VALOR_PRINCIPAL(15)         TYPE N,
    VALOR_JUROS(15)             TYPE N,
    VALOR_MULTA(15)             TYPE N,
    VALOR_AT_MONET(15)          TYPE N,
    COD_BARRAS(48)              TYPE N,
    DATA_VENCIMENTO(8)          TYPE N,
    RESERVA_2(63)               TYPE C,
    AGENCIA(4)                  TYPE N,
    CONTA(7)                    TYPE N,
    MESTRE(8)                   TYPE N,
    USO_EMPRESA(40)             TYPE C,
    RESERVA_3(69)               TYPE C,
  END OF TY_TP_ARREC_12,

  BEGIN OF TY_TP_ARREC_13,
    IDENT_TRIBUTO               TYPE C,
    NOME_CLIENTE(40)            TYPE C,
    END_CLIENTE(40)             TYPE C,
    CEP(8)                      TYPE C,
    UF_CLIENTE(2)               TYPE C,
    CIDADE_CLIENTE(20)          TYPE C,
    TIPO_INSCRICAO              TYPE C,
    NUM_INSCRICAO(15)           TYPE C,
    TEL_CLIENTE_PAGADOR(20)     TYPE C,
    DATA_PAGAMENTO_TRIBUTO(8)   TYPE N,
    AUTORIZA_PAGAMENTO          TYPE C,
    VALOR_PRINCIPAL(15)         TYPE N,
    VALOR_JUROS(15)             TYPE N,
    VALOR_MULTA(15)             TYPE N,
    VALOR_AT_MONET(15)          TYPE N,
    COD_BARRAS(48)              TYPE N,
    DATA_VENCIMENTO_TRIBUTO(8)  TYPE N,
    CODIGO_RECEITA(6)           TYPE N,
    UF_FAVORECIDA(2)            TYPE C,
    NUM_DOC_ORIGEM(13)          TYPE C,
    RESERVA_1(1)                TYPE C,
    REFERENCIA(1)               TYPE C,
    MES(2)                      TYPE N,
    ANO(4)                      TYPE C,
    RESERVA_2                   TYPE C,
    INSCRICAO_ESTADUAL(12)      TYPE C,
    CONVENIO(40)                TYPE C,
    USO_EMPRESA(40)             TYPE C,
    PRODUTO(2)                  TYPE N,
    RESERVA_3(21)               TYPE C,
    CODIGO_SUB_RECEITA(2)       TYPE C,
    RESERVA_4(44)               TYPE C,
  END OF TY_TP_ARREC_13,

  BEGIN OF TY_ZIMP_LAYOUT_IMP,
    TP_IMPOSTO  TYPE ZIMP_LAYOUT_IMP-TP_IMPOSTO,
    CAMPOLAYOUT TYPE ZIMP_LAYOUT_IMP-CAMPOLAYOUT,
    CAMPOSABERT TYPE ZIMP_LAYOUT_IMP-CAMPOSABERT,
  END OF TY_ZIMP_LAYOUT_IMP,
  TY_ARQUIVO(463)               TYPE C,

   BEGIN OF TY_ZIB_CONTABIL_CHV,
      OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
      BELNR   TYPE ZIB_CONTABIL_CHV-BELNR,
      BUKRS   TYPE ZIB_CONTABIL_CHV-BUKRS,
      GJAHR   TYPE ZIB_CONTABIL_CHV-GJAHR,
    END OF TY_ZIB_CONTABIL_CHV.

*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------
DATA:

  W_ARQUIVO                     TYPE TY_ARQUIVO,
  P_UNIX_OLD                    TYPE CHAR1,
  W_TP_ARREC_1_E_2              TYPE TY_TP_ARREC_1_E_2,
  W_TP_ARREC_3                  TYPE TY_TP_ARREC_3,
  W_TP_ARREC_4_E_5              TYPE TY_TP_ARREC_4_E_5,
  W_TP_ARREC_6                  TYPE TY_TP_ARREC_6,
  W_TP_ARREC_7                  TYPE TY_TP_ARREC_7,
  W_TP_ARREC_8_9_10_11          TYPE TY_TP_ARREC_8_9_10_11,
  W_TP_ARREC_12                 TYPE TY_TP_ARREC_12,
  W_TP_ARREC_13                 TYPE TY_TP_ARREC_13,
  W_ZIMP_LAYOUT_IMP             TYPE TY_ZIMP_LAYOUT_IMP,
  W_ADRC                        TYPE ADRC,
  W_T012K                       TYPE T012K.


*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:

  T_ARQUIVO                       TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO1                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO2                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO3                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO4                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO5                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO6                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO7                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO8                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO9                      TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO10                     TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO11                     TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO12                     TYPE TABLE OF TY_ARQUIVO,
  T_ARQUIVO13                     TYPE TABLE OF TY_ARQUIVO,
  T_ZIMP_LAYOUT_IMP               TYPE TABLE OF TY_ZIMP_LAYOUT_IMP,

  T_ZIMP_CABECALHO              TYPE TABLE OF ZIMP_LANC_IMPOST,
  W_ZIMP_CABECALHO              TYPE          ZIMP_LANC_IMPOST,
  T_ZIMP_DETALHE                TYPE TABLE OF ZIMP_LANC_IMP_CT.


*-----------------------------------------------------------------------
* Símbolos de campo
*-----------------------------------------------------------------------
FIELD-SYMBOLS:

  <F_ZIMP_CABECALHO>            TYPE ZIMP_LANC_IMPOST,
  <F_ZIMP_DETALHE>              TYPE ZIMP_LANC_IMP_CT,
  <FS_CAMPO>                    TYPE ANY,
  <WA_DATA>                     TYPE ANY.

*-----------------------------------------------------------------------
* Variáveis
*-----------------------------------------------------------------------
DATA:

  V_BUTXT                       TYPE T001-BUTXT,
  XOBJ_KEY                      TYPE ZIB_CONTABIL-OBJ_KEY,
  V_REGS_PROC                   TYPE I,
  V_ARQS_PROC                   TYPE I,
  VLIFNR                        TYPE ZIMP_LANC_IMP_CT-LIFNR,
  VCGC(15).

* Início Alteração Ricardo Furst 07.07.2009
TYPES: BEGIN OF TY_LOG,
        BLDAT     TYPE BKPF-BLDAT,
        BLART     TYPE BKPF-BLART,
        BUKRS     TYPE EKKO-BUKRS,
        BUDAT     TYPE BKPF-BUDAT,
        WAERS     TYPE EKKO-WAERS,
        KONTO     TYPE RF05A-KONTO,
        HBKID     TYPE T012K-HBKID,
        GSBER     TYPE EKPO-WERKS,
        WRBTR(16) TYPE C,
        SEL01     TYPE RF05A-SEL01,
        AGKON     TYPE RF05A-AGKON,
        TIPO(1)   TYPE C,
        MSG(100)  TYPE C,
  END OF TY_LOG.

DATA: BEGIN OF T_BATCH OCCURS 0,
        BLDAT     TYPE BKPF-BLDAT,
        BLART     TYPE BKPF-BLART,
        BUKRS     TYPE EKKO-BUKRS,
        BUDAT     TYPE BKPF-BUDAT,
        WAERS     TYPE EKKO-WAERS,
        KONTO     TYPE RF05A-KONTO,
        HBKID     TYPE T012K-HBKID,
        GSBER     TYPE EKPO-WERKS,
        WRBTR(16) TYPE C,
        DMBTR(16) TYPE C,
        SEL01     TYPE RF05A-SEL01,
        AGKON     TYPE RF05A-AGKON,
        DATA(10)      TYPE C,
  BELNR TYPE BKPF-BELNR,
  GJAHR TYPE BKPF-GJAHR,
      END OF T_BATCH.

DATA: TI_LOG TYPE STANDARD TABLE OF TY_LOG,
      TI_BDC TYPE STANDARD TABLE OF BDCDATA,
      TI_MSG TYPE STANDARD TABLE OF BDCMSGCOLL.

DATA: WA_LOG LIKE LINE OF TI_LOG,
      WA_BDC LIKE LINE OF TI_BDC,
      WA_MSG LIKE LINE OF TI_MSG.

DATA: V_MSG           LIKE T100-TEXT,
      V_BUDAT         LIKE BKPF-BUDAT,
      V_MSGV1         LIKE BALM-MSGV1,
      V_MSGV2         LIKE BALM-MSGV2,
      V_MSGV3         LIKE BALM-MSGV3,
      V_MSGV4         LIKE BALM-MSGV4,
      V_MODE          TYPE C VALUE 'N',
      V_TIT           TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
      V_TIT_F         TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
      V_TOT_TIT       TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
      LV_NOME_ARQUIVO TYPE STRING.

DATA:  WA_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV,
       WA_BKPF             TYPE BKPF.
*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT-T01.

PARAMETERS:
  P_BNCEMP                      TYPE T012-HBKID DEFAULT 'BBD'.

SELECT-OPTIONS:
  S_NR_DOC                      FOR <F_ZIMP_DETALHE>-DOC_IMPOSTO,
  S_LIFNR                       FOR <F_ZIMP_DETALHE>-LIFNR.

PARAMETERS:
  P_EMP                         TYPE B120-BUKRS
                                OBLIGATORY.

SELECT-OPTIONS:
  S_DTLANC                      FOR WA_BKPF-BUDAT,

  S_DTVENC                      FOR <F_ZIMP_CABECALHO>-DT_VENC OBLIGATORY,
  S_TPARRC                      FOR ZIMP_TIPOS_IMPOS-TP_ARREC
                                NO INTERVALS OBLIGATORY.

SELECTION-SCREEN END OF BLOCK B01.

SELECTION-SCREEN BEGIN OF BLOCK B02 WITH FRAME TITLE TEXT-T02.
PARAMETERS:  P_PC   TYPE CHAR1 RADIOBUTTON GROUP RGR1 DEFAULT 'X' USER-COMMAND USR,
             P_UNIX TYPE CHAR1 RADIOBUTTON GROUP RGR1.

PARAMETERS: P_PATH(250) DEFAULT 'C:\' LOWER CASE.
SELECTION-SCREEN END OF BLOCK B02.

AT SELECTION-SCREEN.
  IF NOT S_DTVENC IS INITIAL.
    LOOP AT S_DTVENC.
      IF S_DTVENC-LOW < SY-DATUM.
        MESSAGE I398(00) WITH 'Data de vencimento dos titulos'
                              'menor que data atual'.
        STOP.
      ENDIF.
    ENDLOOP.

  ENDIF.

**
AT SELECTION-SCREEN OUTPUT.
  IF P_UNIX_OLD <> P_UNIX.
    P_UNIX_OLD = P_UNIX.
    IF P_PC = 'X'.
      P_PATH = 'C:\'.
    ELSE.
      P_PATH = '/sap/usr/'.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
  IF P_PC = 'X'.
    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
        DEF_FILENAME     = ' '
        DEF_PATH         = 'C:\'
        MASK             = '*.TXT'
        MODE             = 'S'
        TITLE            = 'Busca de Arquivo'
      IMPORTING
        FILENAME         = P_PATH
      EXCEPTIONS
        INV_WINSYS       = 1
        NO_BATCH         = 2
        SELECTION_CANCEL = 3
        SELECTION_ERROR  = 4
        OTHERS           = 5.
  ELSE.
    DATA: WL_PATH TYPE DXLPATH.
    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
    EXPORTING
      I_LOCATION_FLAG = ' '
*      i_server = lv_servername
      I_PATH = '//'
      FILEMASK = '*.*'
      FILEOPERATION = 'R'
    IMPORTING
*     O_LOCATION_FLAG =
*     O_SERVER =
      O_PATH = WL_PATH
*     ABEND_FLAG =
    EXCEPTIONS
      RFC_ERROR = 1
      OTHERS = 2.
    MOVE WL_PATH TO P_PATH.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_TPARRC-LOW.
  PERFORM MATCH_CODE_TPARRC.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM ZF_CARREGA_TABELAS.

  PERFORM ZF_MONTA_ARQUIVO.

  PERFORM ZF_INFORMACAO.

  PERFORM ZF_BATCH_INPUT.

*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_TABELAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_CARREGA_TABELAS .

  DATA VOBJ_KEY TYPE ZIB_CONTABIL_ERR-OBJ_KEY.

  SELECT  ZIMP_LANC_IMPOST~MANDT
          ZIMP_LANC_IMPOST~DOC_IMPOSTO
          ZIMP_LANC_IMPOST~BUKRS
          ZIMP_LANC_IMPOST~LOTE
          ZIMP_LANC_IMPOST~DT_VENC
          ZIMP_LANC_IMPOST~DT_APURACAO
          ZIMP_LANC_IMPOST~MES_APURACAO
          ZIMP_LANC_IMPOST~ANO_APURACAO
          ZIMP_LANC_IMPOST~OBSERVACAO
          ZIMP_LANC_IMPOST~COD_IMPOSTO
          ZIMP_LANC_IMPOST~REF_IMPOSTO
          ZIMP_LANC_IMPOST~TP_IMPOSTO
          ZIMP_LANC_IMPOST~COD_PGTO
          ZIMP_LANC_IMPOST~CONV_BANCO
          ZIMP_LANC_IMPOST~HBKID
          ZIMP_LANC_IMPOST~GSBER
          ZIMP_LANC_IMPOST~WAERS
          ZIMP_LANC_IMPOST~WAERS_F
          ZIMP_LANC_IMPOST~IDENTIFICADOR
          ZIMP_LANC_IMPOST~COD_BARRAS
          ZIMP_LANC_IMPOST~DATA_ATUAL
          ZIMP_LANC_IMPOST~HORA_ATUAL
          ZIMP_LANC_IMPOST~USUARIO
          ZIMP_LANC_IMPOST~LOEKZ
    INTO TABLE T_ZIMP_CABECALHO
    FROM ZIMP_LANC_IMPOST
    INNER JOIN ZIMP_CAD_LOTE
    ON ZIMP_CAD_LOTE~LOTE = ZIMP_LANC_IMPOST~LOTE
    AND ZIMP_CAD_LOTE~STATUS_LOTE = 'A'
    WHERE ZIMP_LANC_IMPOST~BUKRS       = P_EMP    AND
          ZIMP_LANC_IMPOST~DOC_IMPOSTO IN S_NR_DOC AND
          ZIMP_LANC_IMPOST~DT_VENC     IN S_DTVENC AND
          ZIMP_LANC_IMPOST~TP_IMPOSTO  IN S_TPARRC AND
          ZIMP_LANC_IMPOST~HBKID  = 'BBD' AND

          ZIMP_LANC_IMPOST~LOEKZ  NE 'X'.

  IF SY-SUBRC IS INITIAL.
    SORT T_ZIMP_CABECALHO BY DOC_IMPOSTO.

    SELECT *
      INTO TABLE T_ZIMP_DETALHE
      FROM ZIMP_LANC_IMP_CT
      FOR ALL ENTRIES IN T_ZIMP_CABECALHO
      WHERE DOC_IMPOSTO = T_ZIMP_CABECALHO-DOC_IMPOSTO
      AND   BUKRS       = T_ZIMP_CABECALHO-BUKRS.

    SELECT TP_IMPOSTO CAMPOLAYOUT CAMPOSABERT
      FROM ZIMP_LAYOUT_IMP
      INTO TABLE T_ZIMP_LAYOUT_IMP.

    IF NOT  S_LIFNR IS INITIAL.
      LOOP AT T_ZIMP_DETALHE ASSIGNING <F_ZIMP_DETALHE>.
        IF <F_ZIMP_DETALHE>-LIFNR IS NOT INITIAL.
          IF <F_ZIMP_DETALHE>-LIFNR NOT IN S_LIFNR.
            READ TABLE T_ZIMP_CABECALHO INTO W_ZIMP_CABECALHO WITH KEY DOC_IMPOSTO = <F_ZIMP_DETALHE>-DOC_IMPOSTO BINARY SEARCH.
            IF SY-SUBRC = 0.
              W_ZIMP_CABECALHO-LOEKZ = 'X'.
              MODIFY T_ZIMP_CABECALHO FROM W_ZIMP_CABECALHO INDEX  SY-TABIX TRANSPORTING LOEKZ.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT T_ZIMP_CABECALHO ASSIGNING <F_ZIMP_CABECALHO>.
      CONCATENATE 'ZP' <F_ZIMP_CABECALHO>-BUKRS <F_ZIMP_CABECALHO>-DOC_IMPOSTO '%' INTO VOBJ_KEY.
      SELECT SINGLE *
      FROM ZIB_CONTABIL_CHV
      INTO WA_ZIB_CONTABIL_CHV
      WHERE OBJ_KEY LIKE VOBJ_KEY.
      IF SY-SUBRC NE 0.
        CONCATENATE 'ZIMP' <F_ZIMP_CABECALHO>-DOC_IMPOSTO '%' INTO VOBJ_KEY.
        SELECT SINGLE *
        FROM ZIB_CONTABIL_CHV
        INTO WA_ZIB_CONTABIL_CHV
        WHERE OBJ_KEY LIKE VOBJ_KEY
        AND   BUKRS   =  <F_ZIMP_CABECALHO>-BUKRS .
      ENDIF.
      IF SY-SUBRC = 0.
        SELECT SINGLE *
          FROM BKPF
          INTO WA_BKPF
          WHERE BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS
          AND   BELNR = WA_ZIB_CONTABIL_CHV-BELNR
          AND   GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR.
        IF SY-SUBRC = 0.
          IF S_DTLANC IS NOT INITIAL AND NOT WA_BKPF-BUDAT IN S_DTLANC.
            MODIFY T_ZIMP_CABECALHO FROM <F_ZIMP_CABECALHO> INDEX  SY-TABIX TRANSPORTING LOEKZ.
          ENDIF.
        ENDIF.
      ELSE.
        <F_ZIMP_CABECALHO>-LOEKZ = 'X'.
        MODIFY T_ZIMP_CABECALHO FROM <F_ZIMP_CABECALHO> INDEX  SY-TABIX TRANSPORTING LOEKZ.
      ENDIF.
    ENDLOOP.
    DELETE T_ZIMP_CABECALHO WHERE LOEKZ = 'X'.


  ELSE.
*    MESSAGE i208(00) WITH 'Não existe dados para esta seleção'.
*    STOP.
  ENDIF.

ENDFORM.                    " ZF_CARREGA_TABELAS
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_MONTA_ARQUIVO .

  SORT T_ZIMP_DETALHE BY DOC_IMPOSTO COD_ABERTURA BUKRS.

  CLEAR: V_TOT_TIT.

  LOOP AT T_ZIMP_CABECALHO ASSIGNING <F_ZIMP_CABECALHO>.

    REFRESH T_ARQUIVO.

    CASE <F_ZIMP_CABECALHO>-TP_IMPOSTO.

      WHEN '01' OR '02'.
        PERFORM ZF_MONTA_TP_01_02.

      WHEN '03'.
        PERFORM ZF_MONTA_TP_03.
        APPEND W_ARQUIVO TO T_ARQUIVO3.

      WHEN '04' OR '05'.
        PERFORM ZF_MONTA_TP_04_05.
        IF <F_ZIMP_CABECALHO>-TP_IMPOSTO = '04'.
          APPEND W_ARQUIVO TO T_ARQUIVO4.
        ELSE.
          APPEND W_ARQUIVO TO T_ARQUIVO5.
        ENDIF.

      WHEN '06'.
        PERFORM ZF_MONTA_TP_06.
        APPEND W_ARQUIVO TO T_ARQUIVO6.

      WHEN '07'.
        PERFORM ZF_MONTA_TP_07.
        APPEND W_ARQUIVO TO T_ARQUIVO7.

      WHEN '08' OR '09' OR '10' OR '11'.
        PERFORM ZF_MONTA_TP_8_9_10_11.
        CASE <F_ZIMP_CABECALHO>-TP_IMPOSTO.
          WHEN '08'.
            APPEND W_ARQUIVO TO T_ARQUIVO8.
          WHEN '09'.
            APPEND W_ARQUIVO TO T_ARQUIVO9.
          WHEN '10'.
            APPEND W_ARQUIVO TO T_ARQUIVO10.
          WHEN '11'.
            APPEND W_ARQUIVO TO T_ARQUIVO11.
        ENDCASE.

      WHEN '12'.
        PERFORM ZF_MONTA_TP_12.
        APPEND W_ARQUIVO TO T_ARQUIVO12.

      WHEN '13'.
        PERFORM ZF_MONTA_TP_13.
        APPEND W_ARQUIVO TO T_ARQUIVO13.

    ENDCASE.

    APPEND W_ARQUIVO TO T_ARQUIVO.

    V_REGS_PROC = V_REGS_PROC + 1.

    PERFORM F_TBATCH.

    V_TOT_TIT = V_TOT_TIT + V_TIT.

  ENDLOOP.

  IF '08' IN S_TPARRC.
    PERFORM ZF_MONTA_TP_8.
  ENDIF.


  PERFORM ZF_GRAVA_ARQUIVO.

ENDFORM.                    " ZF_MONTA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_01_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_MONTA_TP_01_02 .

  ASSIGN W_TP_ARREC_1_E_2 TO <WA_DATA>.

  CLEAR W_TP_ARREC_1_E_2.

  W_TP_ARREC_1_E_2-IDENT_TRIBUTO = <F_ZIMP_CABECALHO>-TP_IMPOSTO.

  PERFORM ZF_NOME_CLIENTE USING  W_TP_ARREC_1_E_2-NUM_INSCRICAO.

  W_TP_ARREC_1_E_2-NOME_CLIENTE   = V_BUTXT.
  W_TP_ARREC_1_E_2-END_CLIENTE    = W_ADRC-STREET.
  W_TP_ARREC_1_E_2-CEP            = W_ADRC-POST_CODE1.
  W_TP_ARREC_1_E_2-UF_CLIENTE     = W_ADRC-REGION.
  W_TP_ARREC_1_E_2-CIDADE_CLIENTE = W_ADRC-CITY1.
  W_TP_ARREC_1_E_2-BAIRRO_CLIENTE = W_ADRC-CITY2.
  W_TP_ARREC_1_E_2-TIPO_INSCRICAO = '2'.
  W_TP_ARREC_1_E_2-TEL_CLIENTE_PAGADOR = W_ADRC-TEL_NUMBER.
  W_TP_ARREC_1_E_2-DATA_PAGAMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_1_E_2-AUTORIZA_PAGAMENTO = 'S'.

  PERFORM PREENCHE_VALOR.

  W_TP_ARREC_1_E_2-DATA_VENC_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_1_E_2-CODIGO_RECEITA = <F_ZIMP_CABECALHO>-COD_PGTO.
  W_TP_ARREC_1_E_2-PERIODO_APURACAO = <F_ZIMP_CABECALHO>-DT_APURACAO.
  W_TP_ARREC_1_E_2-REFERENCIA = VCGC.

  PERFORM ZF_BANCO_AGENCIA USING W_TP_ARREC_1_E_2-AGENCIA
                                 W_TP_ARREC_1_E_2-CONTA.

  W_TP_ARREC_1_E_2-RESERVA_2 = '                      .'.
  W_ARQUIVO = W_TP_ARREC_1_E_2.
  IF <F_ZIMP_CABECALHO>-TP_IMPOSTO = '01'.
    APPEND W_ARQUIVO TO T_ARQUIVO1.
  ELSE.
    APPEND W_ARQUIVO TO T_ARQUIVO2.
  ENDIF.
  CLEAR W_TP_ARREC_1_E_2.
ENDFORM.                    " ZF_MONTA_TP_01_02
*&---------------------------------------------------------------------*
*&      Form  ZF_NOME_CLIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_NOME_CLIENTE USING P_CGC TYPE CHAR15.

  DATA:
    LV_ADRNR TYPE T001-ADRNR,
    LV_CGC_NUMBER TYPE J_1BWFIELD-CGC_NUMBER.


  SELECT SINGLE ADRNR BUTXT
    INTO (LV_ADRNR, V_BUTXT)
    FROM T001
    WHERE BUKRS = P_EMP.

  SELECT SINGLE *
    INTO W_ADRC
    FROM ADRC
    WHERE ADDRNUMBER = LV_ADRNR   AND
          DATE_FROM  = '00010101' AND
          NATION     = SPACE.

* Início Alteração Ricardo Furst 14.07.2009
  IF SY-SUBRC = 0.

    REPLACE '-' WITH SPACE INTO W_ADRC-POST_CODE1.
    CONDENSE W_ADRC-POST_CODE1 NO-GAPS.
    CONCATENATE '0'
                W_ADRC-POST_CODE1
           INTO W_ADRC-POST_CODE1.

  ENDIF.
* Fim Alteração Ricardo Furst 14.07.2009

  CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
    EXPORTING
      BUKRS      = P_EMP
    IMPORTING
      CGC_NUMBER = LV_CGC_NUMBER.

  VCGC  = LV_CGC_NUMBER.
  P_CGC = LV_CGC_NUMBER.

ENDFORM.                    " ZF_NOME_CLIENTE
*&---------------------------------------------------------------------*
*&      Form  ZF_BANCO_AGENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_BANCO_AGENCIA  USING    P_AGENCIA TYPE ANY
                                P_CONTA   TYPE ANY.

  DATA: WL_AGENCIA TYPE BANKK,
        WL_CONTA   TYPE BANKN.
  SELECT BANKL UP TO 1 ROWS
    INTO WL_AGENCIA
    FROM T012
    WHERE BUKRS = P_EMP AND
          HBKID = P_BNCEMP.
  ENDSELECT.

  SELECT BANKN UP TO 1 ROWS
    INTO WL_CONTA
    FROM T012K
    WHERE BUKRS = P_EMP AND
          HBKID = P_BNCEMP.
  ENDSELECT.

  MOVE: WL_CONTA TO P_CONTA,
        WL_AGENCIA TO P_AGENCIA.

ENDFORM.                    " ZF_BANCO_AGENCIA


"---------------------------------------------------------------------*
"      Form  ZF_MONTA_TP_03
"---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_MONTA_TP_03 .

  ASSIGN W_TP_ARREC_3 TO <WA_DATA>.
  CLEAR W_TP_ARREC_3.

  W_TP_ARREC_3-IDENT_TRIBUTO = '7'.

  PERFORM ZF_NOME_CLIENTE USING  W_TP_ARREC_3-NUM_INSCRICAO.

  W_TP_ARREC_3-NOME_CLIENTE   = V_BUTXT.
  W_TP_ARREC_3-END_CLIENTE    = W_ADRC-STREET.
  W_TP_ARREC_3-CEP            = W_ADRC-POST_CODE1.
  W_TP_ARREC_3-UF_CLIENTE     = W_ADRC-REGION.
  W_TP_ARREC_3-CIDADE_CLIENTE = W_ADRC-CITY1.
  W_TP_ARREC_3-BAIRRO_CLIENTE = W_ADRC-CITY2.
  W_TP_ARREC_3-TIPO_INSCRICAO = '2'.
  W_TP_ARREC_3-TEL_CLIENTE_PAGADOR = W_ADRC-TEL_NUMBER.
  W_TP_ARREC_3-DATA_PAGAMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_3-AUTORIZA_PAGAMENTO = 'S'.

  PERFORM PREENCHE_VALOR.

*  W_TP_ARREC_3-VALOR_INSS = <F_ZIMP_DETALHE>-VLR_PRINCIPAL * 100.
*  W_TP_ARREC_3-VALOR_MONET_JUROS_MULTA = ( <F_ZIMP_DETALHE>-VLR_JUROS +
*                                           <F_ZIMP_DETALHE>-VLR_MULTA ) * 100.
*  W_TP_ARREC_3-VALOR_OUTRAS_ENTIDADES = <F_ZIMP_DETALHE>-VLR_OUTRAS_ENT * 100.
*  W_TP_ARREC_3-VALOR_TOTAL = ( <F_ZIMP_DETALHE>-VLR_PRINCIPAL +
*                               <F_ZIMP_DETALHE>-VLR_JUROS     +
*                               <F_ZIMP_DETALHE>-VLR_MULTA     +
*                               <F_ZIMP_DETALHE>-VLR_OUTRAS_ENT ) * 100.

  W_TP_ARREC_3-CODIGO_RECEITA = <F_ZIMP_CABECALHO>-COD_PGTO.
  W_TP_ARREC_3-TIPO_IDENTIFICADOR = '01'.
  "W_TP_ARREC_3-IDENTIFICADOR = VCGC.
  IF <F_ZIMP_CABECALHO>-IDENTIFICADOR IS INITIAL.
    W_TP_ARREC_3-IDENTIFICADOR = VCGC.
  ELSE.
    W_TP_ARREC_3-IDENTIFICADOR = <F_ZIMP_CABECALHO>-IDENTIFICADOR.
  ENDIF.
  W_TP_ARREC_3-ANO_COMPETENCIA = <F_ZIMP_CABECALHO>-ANO_APURACAO.
  W_TP_ARREC_3-MES_COMPETENCIA = <F_ZIMP_CABECALHO>-MES_APURACAO.

  IF <F_ZIMP_CABECALHO>-COD_PGTO = '2631' OR
     <F_ZIMP_CABECALHO>-COD_PGTO = '2658'.

    W_TP_ARREC_3-NOME_RECOLHEDOR = V_BUTXT.

  ENDIF.

  PERFORM ZF_BANCO_AGENCIA USING W_TP_ARREC_3-AGENCIA
                                 W_TP_ARREC_3-CONTA.


  W_TP_ARREC_3-RESERVA_2 = '                      .'.

  W_ARQUIVO = W_TP_ARREC_3.

ENDFORM.                    " ZF_MONTA_TP_03
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_04_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_MONTA_TP_04_05 .

  ASSIGN W_TP_ARREC_4_E_5 TO <WA_DATA>.
  CLEAR W_TP_ARREC_4_E_5.

  CASE <F_ZIMP_CABECALHO>-TP_IMPOSTO.

    WHEN '04'.
      W_TP_ARREC_4_E_5-IDENT_TRIBUTO = '5'.

    WHEN '05'.
      W_TP_ARREC_4_E_5-IDENT_TRIBUTO = '6'.

  ENDCASE.

  PERFORM ZF_NOME_CLIENTE USING  W_TP_ARREC_4_E_5-NUM_INSCRICAO.

  W_TP_ARREC_4_E_5-NOME_CLIENTE   = V_BUTXT.
  W_TP_ARREC_4_E_5-END_CLIENTE    = W_ADRC-STREET.
  W_TP_ARREC_4_E_5-CEP            = W_ADRC-POST_CODE1.
  W_TP_ARREC_4_E_5-UF_CLIENTE     = W_ADRC-REGION.
  W_TP_ARREC_4_E_5-CIDADE_CLIENTE = W_ADRC-CITY1.
  W_TP_ARREC_4_E_5-BAIRRO_CLIENTE = W_ADRC-CITY2.
  W_TP_ARREC_4_E_5-TIPO_INSCRICAO = '2'.
  W_TP_ARREC_4_E_5-TEL_CLIENTE_PAGADOR = W_ADRC-TEL_NUMBER.
  W_TP_ARREC_4_E_5-DATA_PAGAMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_4_E_5-AUTORIZA_PAGAMENTO = 'S'.

  PERFORM PREENCHE_VALOR.

*  W_TP_ARREC_4_E_5-VALOR_PRINCIPAL = <F_ZIMP_DETALHE>-VLR_PRINCIPAL * 100.
*  W_TP_ARREC_4_E_5-VALOR_JUROS_ENCARGOS = <F_ZIMP_DETALHE>-VLR_JUROS * 100.
*  W_TP_ARREC_4_E_5-VALOR_MULTA = <F_ZIMP_DETALHE>-VLR_MULTA * 100.

  W_TP_ARREC_4_E_5-DATA_VENC_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_4_E_5-CODIGO_RECEITA = <F_ZIMP_CABECALHO>-COD_PGTO.
  W_TP_ARREC_4_E_5-ANO = <F_ZIMP_CABECALHO>-ANO_APURACAO.
  W_TP_ARREC_4_E_5-MES = <F_ZIMP_CABECALHO>-MES_APURACAO.

  PERFORM ZF_BANCO_AGENCIA USING W_TP_ARREC_4_E_5-AGENCIA
                                 W_TP_ARREC_4_E_5-CONTA.

  W_TP_ARREC_4_E_5-RESERVA_2 = '                      .'.
  W_ARQUIVO = W_TP_ARREC_4_E_5.

ENDFORM.                    " ZF_MONTA_TP_04_05
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_MONTA_TP_06 .

  CLEAR W_TP_ARREC_6.

  W_TP_ARREC_6-IDENT_TRIBUTO = 'R'.

  W_TP_ARREC_6-COD_BARRAS = <F_ZIMP_CABECALHO>-COD_BARRAS.
  W_TP_ARREC_6-DATA_PAGAMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_6-DATA_VENCIMENTO = <F_ZIMP_CABECALHO>-DT_VENC.


  PERFORM ZF_BANCO_AGENCIA USING W_TP_ARREC_6-AGENCIA
                                 W_TP_ARREC_6-CONTA.

  W_TP_ARREC_6-AUTORIZA_PAGAMENTO = 'S'.

  W_TP_ARREC_6-USO_EMPRESA = '                                                                               .'.
  W_ARQUIVO = W_TP_ARREC_6.

ENDFORM.                    " ZF_MONTA_TP_06
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_07
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_MONTA_TP_07 .
  DATA:
    LV_CGC_NUMBER TYPE J_1BWFIELD-CGC_NUMBER.

  ASSIGN W_TP_ARREC_7 TO <WA_DATA>.
  CLEAR W_TP_ARREC_7.

  W_TP_ARREC_7-IDENT_TRIBUTO = 'F'.

  W_TP_ARREC_7-COD_BARRAS = <F_ZIMP_CABECALHO>-COD_BARRAS.
  W_TP_ARREC_7-DATA_PAGAMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_7-DATA_VENCIMENTO = <F_ZIMP_CABECALHO>-DT_VENC.

  PERFORM PREENCHE_VALOR.

*  W_TP_ARREC_7-VALOR_MULTA = <F_ZIMP_DETALHE>-VLR_MULTA.
*  W_TP_ARREC_7-VALOR_JUROS = <F_ZIMP_DETALHE>-VLR_JUROS.

  CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
    EXPORTING
      BUKRS      = P_EMP
    IMPORTING
      CGC_NUMBER = LV_CGC_NUMBER.

  VCGC  = LV_CGC_NUMBER.
  "w_tp_arrec_7-identificador = vcgc.
  IF <F_ZIMP_CABECALHO>-IDENTIFICADOR IS INITIAL.
    W_TP_ARREC_7-IDENTIFICADOR = VCGC.
  ELSE.
    W_TP_ARREC_7-IDENTIFICADOR = <F_ZIMP_CABECALHO>-IDENTIFICADOR.
  ENDIF.


  PERFORM ZF_BANCO_AGENCIA USING W_TP_ARREC_7-AGENCIA
                                 W_TP_ARREC_7-CONTA.

  W_TP_ARREC_7-AUTORIZA_PAGAMENTO = 'S'.
  W_TP_ARREC_7-USO_EMPRESA = '                                                                               .'.
  W_ARQUIVO = W_TP_ARREC_7.

ENDFORM.                    " ZF_MONTA_TP_07
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_8_9_10_11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_MONTA_TP_8_9_10_11 .

  ASSIGN W_TP_ARREC_8_9_10_11 TO <WA_DATA>.
  CLEAR W_TP_ARREC_8_9_10_11.

  DATA: W_VALOR(15) TYPE N.

  CASE <F_ZIMP_CABECALHO>-TP_IMPOSTO.

    WHEN '08'.
      W_TP_ARREC_8_9_10_11-IDENT_TRIBUTO = 'B'.

    WHEN '09'.
      W_TP_ARREC_8_9_10_11-IDENT_TRIBUTO = 'D'.
      PERFORM PREENCHE_VALOR.
      "W_VALOR = <F_ZIMP_DETALHE>-VLR_PRINCIPAL * 100.
      W_TP_ARREC_8_9_10_11-RESERVA_1 = W_VALOR.
    WHEN '10'.
      W_TP_ARREC_8_9_10_11-IDENT_TRIBUTO = 'I'.
    WHEN '11'.
      W_TP_ARREC_8_9_10_11-IDENT_TRIBUTO = 'O'.

  ENDCASE.


*  IF <F_ZIMP_CABECALHO>-TP_IMPOSTO EQ '08'.
*    SELECT SINGLE COD_BARRAS ZFBDT FROM ZIMP_CONTAS_CONS
*     INTO (W_TP_ARREC_8_9_10_11-COD_BARRAS, W_TP_ARREC_8_9_10_11-DATA_PAGAMENTO_TRIBUTO)
*     WHERE BUKRS EQ <F_ZIMP_CABECALHO>-BUKRS  AND
*           BELNR EQ <F_ZIMP_CABECALHO>-BELNR.
*  ELSE.
  W_TP_ARREC_8_9_10_11-COD_BARRAS = <F_ZIMP_CABECALHO>-COD_BARRAS.
  W_TP_ARREC_8_9_10_11-DATA_PAGAMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_8_9_10_11-AUTORIZA_PAGAMENTO = 'S'.
*  ENDIF.




  PERFORM ZF_BANCO_AGENCIA USING W_TP_ARREC_8_9_10_11-AGENCIA
                                 W_TP_ARREC_8_9_10_11-CONTA.

  W_TP_ARREC_8_9_10_11-USO_EMPRESA = '                                                                               .'.

  W_ARQUIVO = W_TP_ARREC_8_9_10_11.

ENDFORM.                    " ZF_MONTA_TP_8_9_10_11
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_12
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_MONTA_TP_12 .

  ASSIGN W_TP_ARREC_12 TO <WA_DATA>.
  CLEAR W_TP_ARREC_12.

  W_TP_ARREC_12-IDENT_TRIBUTO = 'H'.

  W_TP_ARREC_12-DATA_PAGAMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_12-AUTORIZA_PAGAMENTO = 'S'.

  PERFORM PREENCHE_VALOR.

*  W_TP_ARREC_12-VALOR_PRINCIPAL = <F_ZIMP_DETALHE>-VLR_PRINCIPAL * 100.
*  W_TP_ARREC_12-VALOR_JUROS = <F_ZIMP_DETALHE>-VLR_JUROS.
*  W_TP_ARREC_12-VALOR_MULTA = <F_ZIMP_DETALHE>-VLR_MULTA.
*  W_TP_ARREC_12-VALOR_AT_MONET = <F_ZIMP_DETALHE>-VLR_ATUAL_MONE.

  W_TP_ARREC_12-COD_BARRAS = <F_ZIMP_CABECALHO>-COD_BARRAS.
  W_TP_ARREC_12-DATA_VENCIMENTO = <F_ZIMP_CABECALHO>-DT_VENC.

  PERFORM ZF_BANCO_AGENCIA USING W_TP_ARREC_12-AGENCIA
                                 W_TP_ARREC_12-CONTA.

  W_TP_ARREC_12-RESERVA_3 = '                                                                    .'.
  W_ARQUIVO = W_TP_ARREC_12.

ENDFORM.                    " ZF_MONTA_TP_12
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_13
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_MONTA_TP_13 .

  ASSIGN W_TP_ARREC_13 TO <WA_DATA>.
  CLEAR W_TP_ARREC_13.

  W_TP_ARREC_13-IDENT_TRIBUTO = 'G'.

  PERFORM ZF_NOME_CLIENTE USING  W_TP_ARREC_13-NUM_INSCRICAO.

  W_TP_ARREC_13-NOME_CLIENTE   = V_BUTXT.
  W_TP_ARREC_13-END_CLIENTE    = W_ADRC-STREET.
  W_TP_ARREC_13-CEP            = W_ADRC-POST_CODE1.
  W_TP_ARREC_13-UF_CLIENTE     = W_ADRC-REGION.
  W_TP_ARREC_13-CIDADE_CLIENTE = W_ADRC-CITY1.
  W_TP_ARREC_13-TIPO_INSCRICAO = '2'.
  W_TP_ARREC_13-TEL_CLIENTE_PAGADOR = W_ADRC-TEL_NUMBER.
  W_TP_ARREC_13-DATA_PAGAMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_13-AUTORIZA_PAGAMENTO = 'S'.

  PERFORM PREENCHE_VALOR.
*  W_TP_ARREC_13-VALOR_PRINCIPAL = <F_ZIMP_DETALHE>-VLR_PRINCIPAL * 100.
*  W_TP_ARREC_13-VALOR_JUROS = <F_ZIMP_DETALHE>-VLR_JUROS * 100.
*  W_TP_ARREC_13-VALOR_MULTA = <F_ZIMP_DETALHE>-VLR_MULTA * 100.

  W_TP_ARREC_13-COD_BARRAS = <F_ZIMP_CABECALHO>-COD_BARRAS.
  W_TP_ARREC_13-DATA_VENCIMENTO_TRIBUTO = <F_ZIMP_CABECALHO>-DT_VENC.
  W_TP_ARREC_13-CODIGO_RECEITA = <F_ZIMP_CABECALHO>-COD_PGTO.
  W_TP_ARREC_13-REFERENCIA = <F_ZIMP_CABECALHO>-COD_PGTO.
  W_TP_ARREC_13-ANO = <F_ZIMP_CABECALHO>-ANO_APURACAO.
  W_TP_ARREC_13-MES = <F_ZIMP_CABECALHO>-MES_APURACAO.

  W_TP_ARREC_13-RESERVA_4 = '                                           .'.

  W_ARQUIVO = W_TP_ARREC_13.

ENDFORM.                    " ZF_MONTA_TP_13
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_GRAVA_ARQUIVO .

  DATA:
    WL_TABNAME(15),
    WL_INDEX(2).
  FIELD-SYMBOLS <FS_TAB> TYPE ANY TABLE.

  DO 13 TIMES.
    MOVE SY-INDEX TO WL_INDEX.
*  CASE SY-INDEX.
    CONCATENATE 't_arquivo' WL_INDEX '[]' INTO WL_TABNAME.
    ASSIGN (WL_TABNAME) TO <FS_TAB>.
    IF SY-SUBRC = 0.
      IF NOT <FS_TAB> IS INITIAL.
        MOVE P_PATH TO LV_NOME_ARQUIVO.
        MOVE <FS_TAB>        TO T_ARQUIVO[].
        V_ARQS_PROC = V_ARQS_PROC + 1.
        IF P_PC IS INITIAL.
          OPEN DATASET LV_NOME_ARQUIVO FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

          IF SY-SUBRC <> 0.
            MESSAGE 'Caminho ou nome de arquivo inválido. Impossível continuar!'
            TYPE 'A'.
          ENDIF.

          LOOP AT T_ARQUIVO INTO W_ARQUIVO.
            TRANSFER W_ARQUIVO TO LV_NOME_ARQUIVO.
          ENDLOOP.

          CLOSE DATASET LV_NOME_ARQUIVO.
        ELSE.
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
               <F_ZIMP_CABECALHO>-DOC_IMPOSTO
               'na pasta'
               P_PATH.
          ENDIF.


        ENDIF.


      ENDIF.
    ENDIF.
  ENDDO.

  PERFORM GERA_RELATORIO.

ENDFORM.                    " ZF_GRAVA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  ZF_INFORMACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_INFORMACAO .

  MESSAGE I398(00) WITH V_REGS_PROC
                        'registros foram processados em'
                        V_ARQS_PROC 'arquivo(s).'.

ENDFORM.                    " ZF_INFORMACAO
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTA_TP_8
*&---------------------------------------------------------------------*
FORM ZF_MONTA_TP_8 .

  DATA: BEGIN OF T_CONTAS OCCURS 0.
          INCLUDE STRUCTURE ZIMP_CONTAS_CONS.
  DATA: VENCI LIKE SY-DATUM.
  DATA: END OF T_CONTAS.

  SELECT * FROM ZIMP_CONTAS_CONS
    INTO TABLE T_CONTAS
    WHERE BUKRS        EQ P_EMP    AND
            BELNR      IN S_NR_DOC AND
            BUDAT      IN S_DTLANC AND
            LIFNR      IN S_LIFNR  AND
            DOC_COMP   EQ SPACE    AND
            COD_BARRAS NE SPACE.

  LOOP AT T_CONTAS.
    T_CONTAS-VENCI = T_CONTAS-ZFBDT + T_CONTAS-ZBD1T.
    MODIFY T_CONTAS.
  ENDLOOP.

  DELETE T_CONTAS WHERE NOT VENCI IN S_DTVENC.

  LOOP AT T_CONTAS.
    V_TOT_TIT = V_TOT_TIT + T_CONTAS-DMBTR.

    CLEAR W_TP_ARREC_8_9_10_11.

    W_TP_ARREC_8_9_10_11-IDENT_TRIBUTO = 'B'.

    W_TP_ARREC_8_9_10_11-COD_BARRAS = T_CONTAS-COD_BARRAS.
    W_TP_ARREC_8_9_10_11-DATA_PAGAMENTO_TRIBUTO = T_CONTAS-VENCI.

    W_TP_ARREC_8_9_10_11-AUTORIZA_PAGAMENTO = 'S'.





    PERFORM ZF_BANCO_AGENCIA USING W_TP_ARREC_8_9_10_11-AGENCIA
                                   W_TP_ARREC_8_9_10_11-CONTA.


    W_ARQUIVO = W_TP_ARREC_8_9_10_11.
    W_ARQUIVO+462 = '.'.

    APPEND W_ARQUIVO TO T_ARQUIVO8.
    APPEND W_ARQUIVO TO T_ARQUIVO.

    V_REGS_PROC = V_REGS_PROC + 1.


    T_CONTAS-ZFBDT = T_CONTAS-ZFBDT + T_CONTAS-ZBD1T.

    CONCATENATE T_CONTAS-ZFBDT+6(2)
                T_CONTAS-ZFBDT+4(2)
                T_CONTAS-ZFBDT(4)
           INTO T_BATCH-BUDAT.

    T_BATCH-BLART = 'ZP'.
    T_BATCH-BUKRS = T_CONTAS-BUKRS.
    CONCATENATE T_CONTAS-BUDAT+6(2)
                T_CONTAS-BUDAT+4(2)
                T_CONTAS-BUDAT(4)
           INTO T_BATCH-BLDAT.
    T_BATCH-WAERS = 'BRL'.

    SELECT *
      INTO W_T012K
      FROM T012K
      UP TO 1 ROWS
      WHERE BUKRS = T_CONTAS-BUKRS
        AND HBKID = P_BNCEMP.
    ENDSELECT.

    T_BATCH-KONTO = W_T012K-HKONT.
    CONCATENATE T_CONTAS-BUKRS+2(2)
                '01'
           INTO T_BATCH-GSBER.
*    t_batch-gsber = '0101'."t_contas-gsber.
    T_BATCH-WRBTR = T_CONTAS-DMBTR.
    REPLACE '.' WITH ',' INTO T_BATCH-WRBTR.
    T_BATCH-SEL01 = T_CONTAS-BELNR.
    T_BATCH-AGKON = T_CONTAS-LIFNR.

    MOVE: T_BATCH-BUDAT TO T_BATCH-DATA,
          T_CONTAS-BELNR TO T_BATCH-BELNR,
          T_CONTAS-GJAHR TO T_BATCH-GJAHR.

    APPEND T_BATCH.

  ENDLOOP.

ENDFORM.                    " ZF_MONTA_TP_8
*&---------------------------------------------------------------------*
*&      Form  MATCH_CODE_TPARRC
*&---------------------------------------------------------------------*
FORM MATCH_CODE_TPARRC .

  DATA: BEGIN OF T_VALUE OCCURS 0,
          LINE(30),
        END OF T_VALUE,

        BEGIN OF T_ZIMP_TIPOS_IMPOS OCCURS 0.
          INCLUDE STRUCTURE ZIMP_TIPOS_IMPOS.
  DATA: END OF T_ZIMP_TIPOS_IMPOS.

  DATA: T_HFIELDS LIKE HELP_VALUE OCCURS 0 WITH HEADER LINE.

  CLEAR T_HFIELDS.
  T_HFIELDS-TABNAME = 'ZIMP_TIPOS_IMPOS'.
  T_HFIELDS-FIELDNAME = 'TP_ARREC'.
  T_HFIELDS-SELECTFLAG = 'X'.
  APPEND T_HFIELDS.

  CLEAR T_HFIELDS.
  T_HFIELDS-TABNAME = 'ZIMP_TIPOS_IMPOS'.
  T_HFIELDS-FIELDNAME = 'ARRECADACAO'.
  APPEND T_HFIELDS.

  SELECT *
    FROM ZIMP_TIPOS_IMPOS
    INTO TABLE T_ZIMP_TIPOS_IMPOS.

  SORT T_ZIMP_TIPOS_IMPOS BY TP_ARREC.

  LOOP AT T_ZIMP_TIPOS_IMPOS.

    T_VALUE-LINE = T_ZIMP_TIPOS_IMPOS-TP_ARREC.
    APPEND T_VALUE.

    T_VALUE-LINE = T_ZIMP_TIPOS_IMPOS-ARRECADACAO.
    APPEND T_VALUE.

  ENDLOOP.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
    EXPORTING
      TITEL                     = TEXT-001
    IMPORTING
      SELECT_VALUE              = S_TPARRC-LOW
    TABLES
      FIELDS                    = T_HFIELDS
      VALUETAB                  = T_VALUE
    EXCEPTIONS
      FIELD_NOT_IN_DDIC         = 1
      MORE_THEN_ONE_SELECTFIELD = 2
      NO_SELECTFIELD            = 3
      OTHERS                    = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " MATCH_CODE_TPARRC
*&---------------------------------------------------------------------*
*&      Form  GERA_RELATORIO
*&---------------------------------------------------------------------*
FORM GERA_RELATORIO .

  WRITE:/ SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.


  READ TABLE S_DTVENC INDEX 1.
  WRITE:/1 SY-VLINE,
         2 'Empresa Pagadora:',
         20 P_EMP," P_bukrs,
         202 SY-VLINE.

  WRITE:/1 SY-VLINE,
         2 'Nome do Arquivo:',
         19 LV_NOME_ARQUIVO,
         202 SY-VLINE.

  WRITE:/1 SY-VLINE,
         2 'Data Vencimento:',
*         19 <F_ZIMP_CABECALHO>-DT_VENC,
         19 S_DTVENC-LOW,
         202 SY-VLINE.

  WRITE:/1 SY-VLINE,
         2 'Valor Total dos Títulos:',
         27 V_TOT_TIT,
         202 SY-VLINE.

  WRITE:/1 SY-ULINE.

ENDFORM.                    " GERA_RELATORIO


*&---------------------------------------------------------------------*
*&      Form  f_shdb
*&---------------------------------------------------------------------*
FORM F_SHDB  USING    P_PROGRAMA
                      P_TELA
                      P_ID
                      P_CAMPO
                      P_VALOR.

  CLEAR WA_BDC.

  WA_BDC-PROGRAM        = P_PROGRAMA.
  WA_BDC-DYNPRO         = P_TELA.
  WA_BDC-DYNBEGIN       = P_ID.
  WA_BDC-FNAM           = P_CAMPO.
  WA_BDC-FVAL           = P_VALOR.

  APPEND WA_BDC TO TI_BDC.

ENDFORM.                    " f_shdb
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_VALOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREENCHE_VALOR .
  DATA: STRING(50) TYPE C ,
       VCOD_ABERTURA  TYPE ZIMP_LANC_IMP_CT-COD_ABERTURA,
       VCALCULO       TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
       VTOTAL         TYPE ZIMP_LANC_IMP_CT-VALOR_IMP,
       XLOOP          TYPE I,
       XOPERACAO(1),
       VFIELD(50).

  CLEAR V_TIT.
  LOOP AT T_ZIMP_LAYOUT_IMP INTO W_ZIMP_LAYOUT_IMP WHERE TP_IMPOSTO = <F_ZIMP_CABECALHO>-TP_IMPOSTO.
    STRING = W_ZIMP_LAYOUT_IMP-CAMPOSABERT.
    VCALCULO = 0.
    VTOTAL  = 0.
    CLEAR: VCOD_ABERTURA, XOPERACAO.
    WHILE STRING NE SPACE.
      IF STRING(1) = '<'.
        CLEAR VCOD_ABERTURA.
      ELSEIF STRING(1) = '>'.
        ADD 1 TO XLOOP .
        VCALCULO = 0.
        LOOP AT T_ZIMP_DETALHE ASSIGNING <F_ZIMP_DETALHE>
              WHERE DOC_IMPOSTO  = <F_ZIMP_CABECALHO>-DOC_IMPOSTO
              AND   COD_ABERTURA = VCOD_ABERTURA
              AND   BUKRS        = <F_ZIMP_CABECALHO>-BUKRS.
          IF <F_ZIMP_DETALHE>-LIFNR IS INITIAL.
            VCALCULO = <F_ZIMP_DETALHE>-VALOR_IMP.
          ELSE.
            VLIFNR = <F_ZIMP_DETALHE>-LIFNR.
          ENDIF.
        ENDLOOP.
        IF XOPERACAO  IS INITIAL.
          XOPERACAO = '+'.
        ENDIF.
        IF XOPERACAO = '+'.
          ADD VCALCULO TO VTOTAL.
          CLEAR XOPERACAO.
        ELSEIF XOPERACAO = '-'.
          SUBTRACT VCALCULO FROM VTOTAL.
          CLEAR XOPERACAO.
        ELSEIF XOPERACAO = '*'.
          VTOTAL = VTOTAL * VCALCULO.
          CLEAR XOPERACAO.
        ELSEIF XOPERACAO = '/'.
          VTOTAL = VTOTAL / VCALCULO.
          CLEAR XOPERACAO.
        ENDIF.
      ELSEIF '+_-_*_/' CS STRING(1) AND NOT STRING(1) IS INITIAL.
        XOPERACAO = STRING(1).
      ELSEIF NOT STRING(1) IS INITIAL.
        CONCATENATE VCOD_ABERTURA STRING(1) INTO VCOD_ABERTURA.
      ENDIF.
      SHIFT STRING.
    ENDWHILE.
    IF VTOTAL GT 0.
      VFIELD = W_ZIMP_LAYOUT_IMP-CAMPOLAYOUT.
      CONDENSE VFIELD NO-GAPS.
      ASSIGN COMPONENT VFIELD  OF STRUCTURE <WA_DATA> TO <FS_CAMPO>.
      <FS_CAMPO> = VTOTAL * 100.
      V_TIT = V_TIT + VTOTAL.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " PREENCHE_VALOR
*&---------------------------------------------------------------------*
*&      Form  F_TBATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_TBATCH .
  CONCATENATE  <F_ZIMP_CABECALHO>-DT_APURACAO+6(2)
               <F_ZIMP_CABECALHO>-DT_APURACAO+4(2)
               <F_ZIMP_CABECALHO>-DT_APURACAO(4)
          INTO T_BATCH-BLDAT.
  T_BATCH-BLART = 'ZP'.
  T_BATCH-BUKRS = <F_ZIMP_CABECALHO>-BUKRS.
  CONCATENATE <F_ZIMP_CABECALHO>-DT_VENC+6(2)
              <F_ZIMP_CABECALHO>-DT_VENC+4(2)
              <F_ZIMP_CABECALHO>-DT_VENC(4)
         INTO T_BATCH-BUDAT.
  IF <F_ZIMP_CABECALHO>-WAERS NE 'BRL'.
    T_BATCH-WAERS = <F_ZIMP_CABECALHO>-WAERS.
  ELSE.
    T_BATCH-WAERS = 'BRL'.
  ENDIF.
  WRITE <F_ZIMP_CABECALHO>-DT_VENC  TO T_BATCH-DATA.

  SELECT *
    INTO W_T012K
    FROM T012K
    UP TO 1 ROWS
    WHERE BUKRS = <F_ZIMP_CABECALHO>-BUKRS
      AND HBKID = P_BNCEMP.
  ENDSELECT.

  CONCATENATE 'ZP' <F_ZIMP_CABECALHO>-BUKRS <F_ZIMP_CABECALHO>-DOC_IMPOSTO '%' INTO XOBJ_KEY.
  SELECT SINGLE *
  FROM ZIB_CONTABIL_CHV
  INTO WA_ZIB_CONTABIL_CHV
  WHERE OBJ_KEY LIKE XOBJ_KEY.
  IF SY-SUBRC NE 0.
    CONCATENATE 'ZIMP' <F_ZIMP_CABECALHO>-DOC_IMPOSTO '%' INTO XOBJ_KEY.
    SELECT SINGLE *
      FROM ZIB_CONTABIL_CHV
      INTO WA_ZIB_CONTABIL_CHV
      WHERE OBJ_KEY LIKE XOBJ_KEY
      AND   BUKRS   =  <F_ZIMP_CABECALHO>-BUKRS .
  ENDIF.
  CLEAR: V_TIT, V_TIT_F.
  LOOP AT T_ZIMP_DETALHE ASSIGNING <F_ZIMP_DETALHE>
             WHERE DOC_IMPOSTO = <F_ZIMP_CABECALHO>-DOC_IMPOSTO.
    IF <F_ZIMP_DETALHE>-LIFNR IS INITIAL.
      V_TIT  = V_TIT + <F_ZIMP_DETALHE>-VALOR_IMP.
      V_TIT_F = V_TIT_F + <F_ZIMP_DETALHE>-VALOR_FOR.
    ELSE.
      VLIFNR = <F_ZIMP_DETALHE>-LIFNR.

    ENDIF.
  ENDLOOP.
  T_BATCH-KONTO = W_T012K-HKONT.
  T_BATCH-GSBER = <F_ZIMP_CABECALHO>-GSBER.
  IF <F_ZIMP_CABECALHO>-WAERS NE 'BRL'.
    T_BATCH-WRBTR = V_TIT_F.
    T_BATCH-DMBTR = V_TIT.
  ELSE.
    CLEAR V_TIT_F.
    T_BATCH-WRBTR = V_TIT.
    T_BATCH-DMBTR = V_TIT_F.
  ENDIF.

  REPLACE '.' WITH ',' INTO T_BATCH-WRBTR.
  REPLACE '.' WITH ',' INTO T_BATCH-DMBTR.

  T_BATCH-SEL01 = WA_ZIB_CONTABIL_CHV-BELNR.
  T_BATCH-AGKON =  VLIFNR.
  APPEND T_BATCH.
ENDFORM.                    " F_TBATCH
*&---------------------------------------------------------------------*
*&      Form  ZF_BATCH_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZF_BATCH_INPUT .

*  PERFORM f_tbatch.

  LOOP AT T_BATCH.
* executa o mapeamento
    PERFORM F_MAPEAMENTO.
* executa a transação
    PERFORM F_TRANSACTION.
* trata msg
    PERFORM F_LOG.
  ENDLOOP.
ENDFORM.                    " ZF_BATCH_INPUT

*&---------------------------------------------------------------------*
*&      Form  F_MAPEAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_MAPEAMENTO .
*eduardo
*if v_budat is INITIAL.
  V_BUDAT = T_BATCH-BUDAT.
*  endif.

  CLEAR TI_BDC[].

  PERFORM F_SHDB USING:

'SAPMF05A'  '0103'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '/00'                 ,
   ''         ''    ''                                  'BKPF-BLDAT'  T_BATCH-DATA          ,
   ''         ''    ''                                  'BKPF-BLART'  'ZP'                  ,
   ''         ''    ''                                  'BKPF-BUKRS'  T_BATCH-BUKRS         ,
* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************
*   ''         ''    ''                                  'BKPF-BUDAT'  <f_zimp_cabecalho>-zfbdt, " t_batch-budat         ,
   ''         ''    ''                                  'BKPF-BUDAT'   T_BATCH-DATA         ,
* Inclusão - Inicio - KZORZAN - ROLLOUT - 30.07.2009 ******************
   ''         ''    ''                                  'BKPF-MONAT'  '7'                   ,
   ''         ''    ''                                  'BKPF-WAERS'  T_BATCH-WAERS         ,
   ''         ''    ''                                  'RF05A-KONTO' T_BATCH-KONTO         ,
   ''         ''    ''                                  'BSEG-GSBER'  T_BATCH-GSBER         ,
   ''         ''    ''                                  'BSEG-WRBTR'  T_BATCH-WRBTR         ,
   ''         ''    ''                                  'BSEG-DMBTR'  T_BATCH-DMBTR         ,
   ''         ''    ''                                  'RF05A-AGKON' T_BATCH-AGKON         ,
   ''         ''    ''                                  'RF05A-AGKOA' 'K'                   ,
   ''         ''    ''                                  'RF05A-XNOPS' 'X'                   ,
   ''         ''    ''                                  'RF05A-XPOS1(01)' ''                ,
   ''         ''    ''                                  'RF05A-XPOS1(03)' 'X'               ,

'SAPMF05A'  '0731'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=PA'                 ,
   ''         ''    ''                                  'RF05A-SEL01(01)' T_BATCH-SEL01    ,

'SAPDF05X'  '3100'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=PI'                 ,
   ''         ''    ''                                  'BDC_SUBSCR'  'SAPDF05X                                6102PAGE',
   ''         ''    ''                                  'RF05A-ABPOS' '1'                   ,

'SAPDF05X'  '3100'  'X'                                     ''         ''                   ,
   ''         ''    ''                                  'BDC_OKCODE'  '=BS'                 ,
   ''         ''    ''                                  'BDC_SUBSCR'   'SAPDF05X                                6102PAGE',
   ''         ''    ''                                  'RF05A-ABPOS'  '1'                  ,

'SAPMF05A'  '0700' 'X'                                      ''          ''                  ,
   ''         ''    ''                                  'BDC_OKCODE'  '=BU'                 .

ENDFORM.                    " f_mapeamento
*&---------------------------------------------------------------------*
*&      Form  f_transaction
*&---------------------------------------------------------------------*
FORM F_TRANSACTION .

  V_MODE = 'E'.

  CLEAR TI_MSG[].
  CALL TRANSACTION 'F-53'
        USING TI_BDC
        MODE V_MODE
        UPDATE 'S'
        MESSAGES INTO TI_MSG.

  IF SY-SUBRC = 0.
    UPDATE ZIMP_CONTAS_CONS SET DOC_COMP = 'X'
                          WHERE BUKRS = T_BATCH-BUKRS AND
                                BELNR = T_BATCH-BELNR AND
                                GJAHR = T_BATCH-GJAHR.
  ENDIF.

ENDFORM.                    " f_transaction
*&---------------------------------------------------------------------*
*&      Form  f_log
*&---------------------------------------------------------------------*
FORM F_LOG .

  CLEAR WA_MSG.

  WA_LOG-BLDAT = T_BATCH-BLDAT.
  WA_LOG-BLART = T_BATCH-BLART.
  WA_LOG-BUKRS = T_BATCH-BUKRS.
  WA_LOG-BUDAT = T_BATCH-BUDAT.
  WA_LOG-WAERS = T_BATCH-WAERS.
  WA_LOG-KONTO = T_BATCH-KONTO.
  WA_LOG-HBKID = T_BATCH-HBKID.
  WA_LOG-GSBER = T_BATCH-GSBER.
  WA_LOG-WRBTR = T_BATCH-WRBTR.
  WA_LOG-SEL01 = T_BATCH-SEL01.

  LOOP AT TI_MSG INTO WA_MSG.

    V_MSGV1 = WA_MSG-MSGV1.
    V_MSGV2 = WA_MSG-MSGV2.
    V_MSGV3 = WA_MSG-MSGV3.
    V_MSGV4 = WA_MSG-MSGV4.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        LANGUAGE               = WA_MSG-MSGSPRA
        MSG_ID                 = WA_MSG-MSGID
        MSG_NO                 = WA_MSG-MSGNR
        MSG_VAR1               = V_MSGV1
        MSG_VAR2               = V_MSGV2
        MSG_VAR3               = V_MSGV3
        MSG_VAR4               = V_MSGV4
      IMPORTING
        MSG_TEXT               = V_MSG
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.

    IF SY-SUBRC <> 0.
      CLEAR WA_LOG-TIPO.
      V_MSG = 'Não houve mensagem.'.
    ENDIF.

    WA_LOG-TIPO     = WA_MSG-MSGTYP.
    WA_LOG-MSG      = V_MSG.

    APPEND WA_LOG TO TI_LOG.

  ENDLOOP.

ENDFORM.                    " f_logc
