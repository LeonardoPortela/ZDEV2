*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: Maggi                                                   *
* Autor......: Alexandre Ferrari                                       *
* Data.......: 05.08.2010                                              *
* Descrição  : Entradas de VL31N, MIGO, MIRO                           *
* Projeto....: Maggi - Projeto Evoluir                                 *
* Cód Espec..: GAP_MM02 / GAP_MM03 / GAP_MM04                          *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :  Marcus Scauri                         Data: 13/08/2010 *
* Observações:  Ajustes                                                *
*----------------------------------------------------------------------*
REPORT  ZMMR019_FORCE_MIRO.


*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: C_S(1)      TYPE C                            VALUE 'S',
           C_W(1)      TYPE C                            VALUE 'W',
           C_0(1)      TYPE C                            VALUE '0',
           C_1(1)      TYPE C                            VALUE '1',
           C_E(1)      TYPE C                            VALUE 'E',
           C_V(1)      TYPE C                            VALUE 'V',
           C_X(1)      TYPE C                            VALUE 'X',
           C_I(1)      TYPE C                            VALUE 'I',
           C_VL31N(5)  TYPE C                            VALUE 'VL31N',
           C_VL32N(5)  TYPE C                            VALUE 'VL32N',
           C_0001(4)   TYPE C                            VALUE '0001',
           C_000001(6) TYPE C                            VALUE '000001',
           C_000002(6) TYPE C                            VALUE '000002',
           C_000003(6) TYPE C                            VALUE '000003',
           C_N(1)      TYPE C                            VALUE 'N',
           C_02(2)     TYPE C                            VALUE '02',
           C_03(2)     TYPE C                            VALUE '03',
           C_B(1)      TYPE C                            VALUE 'B',
           C_04(2)     TYPE C                            VALUE '04',
           C_08(2)     TYPE C                            VALUE '08',
           C_06(2)     TYPE C                            VALUE '06',
           C_09(2)     TYPE C                            VALUE '09',
           C_311(3)    TYPE C                            VALUE '311',
           C_01        TYPE BAPI2017_GM_CODE             VALUE '01',
           C_10        TYPE ZFIE_RET_DOCUMENT-INTERFACE  VALUE '10',
           C_11        TYPE ZFIE_RET_DOCUMENT-INTERFACE  VALUE '11',
           C_12        TYPE ZFIE_RET_DOCUMENT-INTERFACE  VALUE '12',
           C_05        TYPE ZFIE_RET_DOCUMENT-INTERFACE  VALUE '05',
           C_MM        TYPE ZFIE_RET_DOCUMENT-ID         VALUE 'MM', " id da mensagem de retorno para migo/miro
           C_8B        TYPE ZFIE_RET_DOCUMENT-ID         VALUE '8B', " id da mensagem de retorno para docnum
           C_899       TYPE ZFIE_RET_DOCUMENT-NUM        VALUE '899',
           C_BRL(3)    TYPE C                            VALUE 'BRL',
           C_USD(3)    TYPE C                            VALUE 'USD',
           C_BBD(3)    TYPE C                            VALUE 'BBD',
           C_122(3)    TYPE C                            VALUE '122',
           C_101(3)    TYPE C                            VALUE '101',
           C_0002(4)   TYPE C                            VALUE '0002',
           C_NF55(4)   TYPE C                            VALUE 'NF55',
           C_00010(5)  TYPE C                            VALUE '00010',
           C_ZG(2)     TYPE C                            VALUE 'ZG',
           C_ZW(2)     TYPE C                            VALUE 'ZW',
           C_LF(2)     TYPE C                            VALUE 'LF',
           C_LA(2)     TYPE C                            VALUE 'LA',
           C_21(2)     TYPE C                            VALUE '21',
           C_31(2)     TYPE C                            VALUE '31',
           C_40(2)     TYPE C                            VALUE '40',
           C_50(2)     TYPE C                            VALUE '50',
           C_SA(2)     TYPE C                            VALUE 'SA'.

*----------------------------------------------------------------------*
* Protótipos
*----------------------------------------------------------------------*

* Declaração de Tabela BDCDATA para Batch-Imput
TYPES: BEGIN OF TY_BDCDATA,
         PROGRAM  TYPE BDCDATA-PROGRAM,
         DYNPRO   TYPE BDCDATA-DYNPRO,
         DYNBEGIN TYPE BDCDATA-DYNBEGIN,
         FNAM     TYPE BDCDATA-FNAM,
         FVAL     TYPE BDCDATA-FVAL,
       END OF TY_BDCDATA.

* Tabela de Mensagem de erro p/ Batch-Input
TYPES: BEGIN OF TY_MSG,
         TCODE   TYPE BDCMSGCOLL-TCODE,
         DYNAME  TYPE BDCMSGCOLL-DYNAME,
         DYNUMB  TYPE BDCMSGCOLL-DYNUMB,
         MSGTYP  TYPE BDCMSGCOLL-MSGTYP,
         MSGSPRA TYPE BDCMSGCOLL-MSGSPRA,
         MSGID   TYPE BDCMSGCOLL-MSGID,
         MSGNR   TYPE BDCMSGCOLL-MSGNR,
         MSGV1   TYPE BDCMSGCOLL-MSGV1,
         MSGV2   TYPE BDCMSGCOLL-MSGV2,
         MSGV3   TYPE BDCMSGCOLL-MSGV3,
         MSGV4   TYPE BDCMSGCOLL-MSGV4,
         ENV     TYPE BDCMSGCOLL-ENV,
         FLDNAME TYPE BDCMSGCOLL-FLDNAME,
       END OF TY_MSG.

* Impostos
TYPES: BEGIN OF TY_ZMMT_EEIMP_ZGR.
         INCLUDE STRUCTURE ZMMT_EEIMP_ZGR.
TYPES: END OF TY_ZMMT_EEIMP_ZGR.

* Depósitos
TYPES: BEGIN OF TY_LGORT,
         EBELN TYPE EKPO-EBELN,
         EBELP TYPE EKPO-EBELP,
         LGORT TYPE LGORT_D,
         CHARG TYPE CHARG_D,
       END OF TY_LGORT.

* Fluxo
TYPES: BEGIN OF TY_VBFA,
         VBELV   TYPE VBFA-VBELV,
         VBELN   TYPE VBFA-VBELN,
         OBJ_KEY TYPE ZMMT_EEIMP_ZGR-OBJ_KEY,
       END OF TY_VBFA.

* Centros / Filiais
TYPES: BEGIN OF TY_T001W,
         WERKS      TYPE T001W-WERKS,
         BWKEY      TYPE T001W-BWKEY,
         J_1BBRANCH TYPE T001W-J_1BBRANCH,
       END OF TY_T001W.

* Nota Fiscal
TYPES: BEGIN OF TY_J_1BNFDOC,
         DOCNUM  TYPE J_1BNFDOC-DOCNUM,
         PARID   TYPE J_1BNFDOC-PARID,
         NFENUM  TYPE J_1BNFDOC-NFENUM,
         AUTHCOD TYPE J_1BNFDOC-AUTHCOD,
*         conting TYPE j_1bnfdoc-conting,
       END OF TY_J_1BNFDOC.

* Categoria Nota
TYPES: BEGIN OF TY_J_1BAA,
         NFTYPE TYPE J_1BAA-NFTYPE,
         NFE    TYPE J_1BAA-NFE,
         FORM   TYPE J_1BAA-FORM,
       END OF TY_J_1BAA.

* Confirmação Pedido
TYPES: BEGIN OF TY_EKES,
         EBELN TYPE EKES-EBELN,
         EBELP TYPE EKES-EBELP,
         ETENS TYPE EKES-ETENS,
       END OF TY_EKES.

TYPES: BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF TY_MAKT.

TYPES: BEGIN OF TY_TAXCODE,
         TAX_CODE TYPE MWSKZ,
         HKONT    TYPE BSEG-HKONT,
       END OF TY_TAXCODE.

*----------------------------------------------------------------------*
* Tabela Interna
*----------------------------------------------------------------------*
DATA: IT_ZMMT_EE_ZGR     TYPE TABLE OF ZMMT_EE_ZGR,
      IT_ZMMT_EE_ZGR_AUX TYPE TABLE OF ZMMT_EE_ZGR,
      IT_ITEM            TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
      IT_SERIALNUMBER    TYPE TABLE OF BAPI2017_GM_SERIALNUMBER,
      IT_RETURN          TYPE TABLE OF BAPIRET2,
      IT_RETURN2         TYPE TABLE OF BAPIRETURN,
      IT_OUTRETURN       TYPE TABLE OF ZFIE_RET_DOCUMENT,
      IT_ITEMDATA        TYPE TABLE OF BAPI_INCINV_CREATE_ITEM,
      IT_GLACCOUNTDATA   TYPE TABLE OF BAPI_INCINV_CREATE_GL_ACCOUNT,
      IT_BDCDATA         TYPE TABLE OF TY_BDCDATA,
      IT_MSG             TYPE TABLE OF TY_MSG,
      IT_WITHTAXDATA     TYPE TABLE OF BAPI_INCINV_CREATE_WITHTAX,
      IT_TAXDATA         TYPE TABLE OF BAPI_INCINV_CREATE_TAX,
      IT_ZMMT_EEIMP_ZGR  TYPE TABLE OF TY_ZMMT_EEIMP_ZGR,
      IT_ZMMT_EEIVA_ZGR  TYPE TABLE OF ZMMT_EEIVA_ZGR,
      IT_LGORT           TYPE TABLE OF TY_LGORT,
      IT_VBFA            TYPE TABLE OF TY_VBFA,
      IT_VBFA_AUX        TYPE TABLE OF TY_VBFA,
      IT_T001W           TYPE TABLE OF TY_T001W,
      IT_ZIB_CONTABIL    TYPE TABLE OF ZIB_CONTABIL,
      IT_J_1BNFDOC       TYPE TABLE OF TY_J_1BNFDOC,
      IT_J_1BNFDOC_AUX   TYPE TABLE OF TY_J_1BNFDOC,
      IT_J_1BAA          TYPE TABLE OF TY_J_1BAA,
      IT_DOC_PARTNER     TYPE TABLE OF J_1BNFNAD,
      IT_DOC_ITEM        TYPE TABLE OF J_1BNFLIN,
      IT_DOC_ITEM_TAX    TYPE TABLE OF J_1BNFSTX,
      IT_DOC_HEADER_MSG  TYPE TABLE OF J_1BNFFTX,
      IT_DOC_REFER_MSG   TYPE TABLE OF J_1BNFREF,
      IT_DOC_OT_PARTNER  TYPE TABLE OF J_1BNFCPD,
      IT_EKES            TYPE TABLE OF TY_EKES,
      IT_EKES_SRT        TYPE SORTED TABLE OF TY_EKES
        WITH NON-UNIQUE KEY EBELN,
      IT_XEKES           TYPE TABLE OF UEKES,
      IT_YEKES           TYPE TABLE OF UEKES,
      IT_OBJ_PARTNER     TYPE TABLE OF BAPI_J_1BNFNAD,
      IT_OBJ_ITEM        TYPE TABLE OF BAPI_J_1BNFLIN,
      IT_OBJ_ITEM_ADD    TYPE TABLE OF BAPI_J_1BNFLIN_ADD,
      IT_OBJ_ITEM_TAX    TYPE TABLE OF BAPI_J_1BNFSTX,
      IT_OBJ_OT_PARTNER  TYPE TABLE OF BAPI_J_1BNFCPD,
      IT_MAKT            TYPE TABLE OF TY_MAKT,
      IT_TAXCODE         TYPE TABLE OF TY_TAXCODE,
      IT_ZOB_MENSAGEM    TYPE TABLE OF ZOB_MENSAGEM,
      IT_SET             TYPE TABLE OF RGSB4,
      OBJ_ENTRADA        TYPE REF TO ZCL_CARGA_RECEBIMENTO,
      IT_MATERIAL_BLOQU  TYPE TABLE OF MATNR WITH HEADER LINE.

*----------------------------------------------------------------------*
* Estrutura
*----------------------------------------------------------------------*
DATA: WA_MOV_ESTQ             TYPE ZMMT_EE_ZGR,
      WA_ESTORNO              TYPE ZMMS003,
      WA_BDCDATA              TYPE TY_BDCDATA,
      WA_HEAD_RET             TYPE BAPI2017_GM_HEAD_RET,
      WA_RETURN               TYPE BAPIRET2,
      WA_RETURN2              TYPE BAPIRETURN,
      WA_MSG                  TYPE TY_MSG,
      WA_DOC_YEAR             TYPE BAPI2017_GM_HEAD_02-DOC_YEAR,
      WA_PSTNG_DATE           TYPE BAPI2017_GM_HEAD_02-PSTNG_DATE,
      WA_MAT_DOC              TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
      WA_HEADERDATA           TYPE BAPI_INCINV_CREATE_HEADER,
      WA_OUTRETURN            TYPE ZFIE_RET_DOCUMENT,
      WA_HEADER               TYPE BAPI2017_GM_HEAD_01,
      WA_ITEM                 TYPE BAPI2017_GM_ITEM_CREATE,
      WA_SERIALNUMBER         TYPE BAPI2017_GM_SERIALNUMBER,
      WA_ITEMDATA             TYPE BAPI_INCINV_CREATE_ITEM,
      WA_GLACCOUNTDATA        TYPE BAPI_INCINV_CREATE_GL_ACCOUNT,
      WA_WITHTAXDATA          TYPE BAPI_INCINV_CREATE_WITHTAX,
      WA_TAXDATA              TYPE BAPI_INCINV_CREATE_TAX,
      WA_ZMMT_EEIMP_ZGR       TYPE TY_ZMMT_EEIMP_ZGR,
      WA_ZMMT_EEIVA_ZGR       TYPE ZMMT_EEIVA_ZGR,
      WA_LGORT                TYPE TY_LGORT,
      WA_VBFA                 TYPE TY_VBFA,
      WA_VBFA_AUX             TYPE TY_VBFA,
      WA_T001W                TYPE TY_T001W,
      WA_ZIB_CONTABIL         TYPE ZIB_CONTABIL,
      WA_J_1BNFDOC            TYPE TY_J_1BNFDOC,
      WA_J_1BNFDOC_AUX        TYPE TY_J_1BNFDOC,
      WA_DOC_HEADER           TYPE J_1BNFDOC,
      WA_EKES                 TYPE TY_EKES,
      WA_XEKES                TYPE UEKES,
      WA_YEKES                TYPE UEKES,
      WA_OBJ_HEADER           TYPE BAPI_J_1BNFDOC,
      WA_OBJ_HEADER_ADD       TYPE BAPI_J_1BNFDOC_ADD,
      WA_OBJ_PARTNER          TYPE BAPI_J_1BNFNAD,
      WA_OBJ_ITEM             TYPE BAPI_J_1BNFLIN,
      WA_OBJ_ITEM_ADD         TYPE BAPI_J_1BNFLIN_ADD,
      WA_OBJ_ITEM_TAX         TYPE BAPI_J_1BNFSTX,
      WA_OBJ_OT_PARTNER       TYPE BAPI_J_1BNFCPD,
      WA_MATERIAL_TEXT_RECORD TYPE MAKT,
      WA_NFCHECK              TYPE BAPI_J_1BNFCHECK,
      WA_MAKT                 TYPE TY_MAKT,
      WA_SET                  TYPE RGSB4,
      WA_TAXCODE              TYPE TY_TAXCODE.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: VG_GMCODE                TYPE BAPI2017_GM_CODE,
      VG_OBJ_KEY               TYPE ZMMT_EE_ZGR-OBJ_KEY,
      VG_INDEX                 TYPE SY-TABIX,
      VG_RETURN                TYPE STRING,
      VG_RECEBIMENTO           TYPE VBELN_VL,
      VG_INVOICEDOCNUMBER_MIGO TYPE BAPI2017_GM_HEAD_RET,
      VG_INVOICEDOCNUMBER_MIRO TYPE BAPI_INCINV_FLD-INV_DOC_NO,
      VG_ANO_MIGO              TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
      VG_ANO_MIRO              TYPE BAPI_INCINV_FLD-FISC_YEAR,
      VG_ERRO                  TYPE C,
      VG_MESSAGE               TYPE SYMSGV,
      VG_LGORT                 TYPE LGORT_D,
      VG_INTERFACE(2)          TYPE C,
      VG_LIFNR                 TYPE EKKO-LIFNR,
      VG_HKONT(10)             TYPE N,
      VG_WITHT                 TYPE LFBW-WITHT,
      VG_DOCNUM                TYPE BAPI_J_1BNFDOC-DOCNUM,
      VG_VR_PERCEPCOES         TYPE ZMMT_EEIVA_ZGR-VLR_IMP,
      VG_TOTAL_ITEM            TYPE ZMMT_EEIVA_ZGR-VLR_IMP.


PARAMETERS POBJKEY TYPE ZMMT_EE_ZGR-OBJ_KEY NO-DISPLAY.

*PARAMETERS: POBJKEY TYPE AWKEY NO-DISPLAY.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  "DATA(USUARIO) = SY-UNAME.

  "SY-UNAME = 'R3JOB'.

  CREATE OBJECT OBJ_ENTRADA.

  PERFORM: Z_SELECIONA_DADOS," Seleção de Dados
           Z_EXECUTA_BAPIS,  "Executa rotinas de criação VL31N/MIGO/MIRO
           Z_ENVIA_LOG_LEGADO. "Envia retorno de msg para o legado
  "           z_atualiza_confirmacao_pedidos. "Atualiza todos os pedidos das entradas enviadas

  OBJ_ENTRADA->ZIF_CARGA~FREE( ).
  CLEAR OBJ_ENTRADA.

  "SY-UNAME = USUARIO.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona Dados
*----------------------------------------------------------------------*
FORM Z_SELECIONA_DADOS .

  DATA: LT_ZMMT_EE_ZGR TYPE TABLE OF ZMMT_EE_ZGR,
        VG_JOB         TYPE I.

  FREE: IT_ZMMT_EE_ZGR,
        IT_ZMMT_EEIMP_ZGR,
        IT_ZMMT_EEIVA_ZGR.

  SELECT SINGLE COUNT(*) INTO VG_JOB
    FROM TBTCO
   WHERE JOBNAME EQ 'ENT_ESTOQUE_GRAOS'
     AND STATUS EQ 'R'.

*  call function 'ENQUE_READ2'
*    exporting
*      gname = 'ZIB_CONTABIL'
*    tables
*      enq   = raw_enq.
*    vg_job = 1.
  IF POBJKEY IS NOT INITIAL.

    SELECT *
      FROM ZMMT_EE_ZGR
      INTO TABLE IT_ZMMT_EE_ZGR_AUX
     WHERE ZRG_ATLZ      EQ C_1
       AND ( TP_OPERACAO IN (C_01, C_02, C_03, C_04, C_08, C_09, C_10 ) OR TP_OPERACAO EQ '' )
       AND OBJ_KEY       EQ POBJKEY.

  ELSEIF ( VG_JOB EQ 1 ). "and ( raw_enq[] is initial ).

*  Busca os dados da tabela intermediária de criação
*  alterando para nao considerar mais o tipo de operação em função da interface da argentina que separa migo de miro.
    SELECT *
      FROM ZMMT_EE_ZGR
      INTO TABLE IT_ZMMT_EE_ZGR_AUX
     WHERE ZRG_ATLZ      EQ C_0
       AND ( TP_OPERACAO IN (C_01, C_02, C_03, C_04, C_08, C_09, C_10 ) OR TP_OPERACAO EQ '' ).

  ENDIF.

  LOOP AT IT_ZMMT_EE_ZGR_AUX INTO DATA(WA_ENTRADA).
    TRY .
        OBJ_ENTRADA->ZIF_CARGA~BLOQUEAR_ENTRADA( I_OBJ_KEY = WA_ENTRADA-OBJ_KEY ).
        APPEND WA_ENTRADA TO IT_ZMMT_EE_ZGR.
      CATCH ZCX_CARGA INTO DATA(EX_CARGA).    "
        EX_CARGA->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    ENDTRY.
  ENDLOOP.

  IF IT_ZMMT_EE_ZGR[] IS NOT INITIAL.
    "Seleciona Depósito
    SELECT A~EBELN
           A~EBELP
           A~LGORT
           B~CHARG
      FROM EKPO AS A
      INNER JOIN EKET AS B
      ON A~EBELN EQ B~EBELN AND
         A~EBELP EQ B~EBELP
      INTO TABLE IT_LGORT
      FOR ALL ENTRIES IN IT_ZMMT_EE_ZGR
      WHERE A~EBELN = IT_ZMMT_EE_ZGR-PO_NUMBER AND
            A~EBELP = IT_ZMMT_EE_ZGR-PO_ITEM.

    SORT IT_ZMMT_EE_ZGR BY OBJ_KEY.

    FREE: IT_ZMMT_EE_ZGR_AUX.
    IT_ZMMT_EE_ZGR_AUX[] = IT_ZMMT_EE_ZGR.
    SORT IT_ZMMT_EE_ZGR_AUX BY OBJ_KEY.
    DELETE ADJACENT DUPLICATES FROM IT_ZMMT_EE_ZGR_AUX
                    COMPARING OBJ_KEY.

*  Busca dados da tabela intermediária de impostos
*  Somente retornará valor se estiver previsto
    SELECT * FROM ZMMT_EEIMP_ZGR
       INTO TABLE IT_ZMMT_EEIMP_ZGR
         FOR ALL ENTRIES IN IT_ZMMT_EE_ZGR_AUX
          WHERE OBJ_KEY EQ IT_ZMMT_EE_ZGR_AUX-OBJ_KEY.


    IF SY-SUBRC IS INITIAL.
      SORT IT_ZMMT_EEIMP_ZGR BY OBJ_KEY.
    ENDIF. " FimSe da seleção zmmt_eeimp_zgr

*  Busca dados da tabela intermediária de PERCEPÇÕES
*  Somente retornará valor se estiver previsto
    SELECT * FROM ZMMT_EEIVA_ZGR
      INTO TABLE IT_ZMMT_EEIVA_ZGR
       FOR ALL ENTRIES IN IT_ZMMT_EE_ZGR_AUX
     WHERE OBJ_KEY EQ IT_ZMMT_EE_ZGR_AUX-OBJ_KEY.


    IF SY-SUBRC IS INITIAL.
      SORT IT_ZMMT_EEIVA_ZGR BY OBJ_KEY.
    ENDIF. " FimSe da seleção zmmt_eeimp_zgr


    LT_ZMMT_EE_ZGR = IT_ZMMT_EE_ZGR.

    SORT LT_ZMMT_EE_ZGR BY PLANT.

    DELETE ADJACENT DUPLICATES
      FROM LT_ZMMT_EE_ZGR COMPARING PLANT.

    SELECT WERKS BWKEY J_1BBRANCH
      FROM T001W
      INTO TABLE IT_T001W
      FOR ALL ENTRIES IN LT_ZMMT_EE_ZGR
      WHERE WERKS EQ LT_ZMMT_EE_ZGR-PLANT.

    IF SY-SUBRC EQ 0.
      SORT IT_T001W BY WERKS.
    ENDIF.

    LT_ZMMT_EE_ZGR = IT_ZMMT_EE_ZGR.

    SORT LT_ZMMT_EE_ZGR BY J_1BNFTYPE.

    DELETE ADJACENT DUPLICATES
      FROM LT_ZMMT_EE_ZGR COMPARING J_1BNFTYPE.

    SELECT NFTYPE NFE FORM
      FROM J_1BAA
      INTO TABLE IT_J_1BAA
      FOR ALL ENTRIES IN LT_ZMMT_EE_ZGR
      WHERE NFTYPE EQ LT_ZMMT_EE_ZGR-J_1BNFTYPE AND
            NFE    EQ C_X.

    IF SY-SUBRC EQ 0.
      SORT IT_J_1BAA BY NFTYPE.
    ENDIF.

    LT_ZMMT_EE_ZGR = IT_ZMMT_EE_ZGR.

    SORT LT_ZMMT_EE_ZGR BY PO_NUMBER.

    DELETE ADJACENT DUPLICATES
      FROM LT_ZMMT_EE_ZGR COMPARING PO_NUMBER.

    IF NOT LT_ZMMT_EE_ZGR IS INITIAL.
      SELECT EBELN EBELP ETENS
        FROM EKES
        INTO TABLE IT_EKES
        FOR ALL ENTRIES IN LT_ZMMT_EE_ZGR
        WHERE EBELN EQ LT_ZMMT_EE_ZGR-PO_NUMBER AND
              EBELP EQ C_00010.

      IF SY-SUBRC EQ 0.
        SORT IT_EKES BY EBELN ASCENDING
                        ETENS DESCENDING.

        DELETE ADJACENT DUPLICATES
          FROM IT_EKES COMPARING EBELN.

        IT_EKES_SRT = IT_EKES.
      ENDIF.
    ENDIF.

    SELECT MATNR MAKTX
       FROM MAKT
    INTO TABLE IT_MAKT
       FOR ALL ENTRIES IN LT_ZMMT_EE_ZGR
    WHERE MATNR EQ LT_ZMMT_EE_ZGR-MATERIAL
      AND SPRAS EQ SY-LANGU.

  ENDIF. " FimSe da seleção zmmt_ee_zgr

*-CS2020000574 - 10.12.2020 - inicio
* SET DADOS CONTAS
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      SETNR           = 'ZMMR019_CONTAS'
      TABLE           = 'BKPF'
      CLASS           = '0000'
      FIELDNAME       = 'XBLNR'
      NO_DESCRIPTIONS = ''
      NO_RW_INFO      = ''
    TABLES
      SET_VALUES      = IT_SET
    EXCEPTIONS
      SET_NOT_FOUND   = 1
      OTHERS          = 2.

  LOOP AT IT_SET INTO WA_SET.
    VG_HKONT            = WA_SET-TITLE.
    WA_TAXCODE-TAX_CODE = WA_SET-FROM.
    WA_TAXCODE-HKONT    = VG_HKONT.
    APPEND WA_TAXCODE  TO IT_TAXCODE.
  ENDLOOP.

  SORT IT_TAXCODE BY TAX_CODE.
*-CS2020000574 - 10.12.2020 - fim

ENDFORM.                    " Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTA_BAPIS
*&---------------------------------------------------------------------*
FORM Z_EXECUTA_BAPIS .


  TYPES: BEGIN OF TY_ZLEST0109,
           EBELN      TYPE ZLEST0109-EBELN,
           EBELP      TYPE ZLEST0109-EBELP,
           WERKS      TYPE ZLEST0109-WERKS,
           LGORT      TYPE ZLEST0109-LGORT,
           CHARG      TYPE ZLEST0109-CHARG,
           VBELN      TYPE ZLEST0109-VBELN,
           LIFNR      TYPE ZLEST0109-LIFNR,
           MATNR      TYPE ZLEST0109-MATNR,
           NFNUM      TYPE ZLEST0109-NFNUM,
           SERIE      TYPE ZLEST0109-SERIE,
           QTDE_AVISO TYPE ZLEST0109-QTDE_AVISO,
           UNIDADE    TYPE ZLEST0109-UNIDADE,
         END OF TY_ZLEST0109.

  DATA: DOC_GERADOS       TYPE ZMMT_EE_ZGR_DOCS,
        WA_ZLEST0109      TYPE TY_ZLEST0109,
        DOC_AVISO_GER     TYPE ZLEST0109,
        VG_VALIDA_IMP     TYPE SY-SUBRC,
        DOC_NUMBER_REF    TYPE J_1BDOCNUM,

        VL_CENTRO_AR      TYPE C LENGTH 1,
        VL_DOCNUM         TYPE J_1BNFDOC-DOCNUM,
        WA_SETLEAF        TYPE SETLEAF,
        VL_NRO_NF_AUX     TYPE STRING,
        VL_SERIE_NF_AUX   TYPE STRING,
        VL_NRO_NF         TYPE ZLEST0109-NFNUM,
        VL_SERIE_NF       TYPE ZLEST0109-SERIE,
        VL_INI_COPY       TYPE I,
        VL_FIM_COPY       TYPE I,
        VL_ERRO_EST_MIGO  TYPE C,
        VL_ERRO_EST_MIRO  TYPE C,
        VL_QTDE_AVISO_REC TYPE ZMMT_EE_ZGR-ENTRY_QNT,
        VL_QTDE_ENT_GR    TYPE ZMMT_EE_ZGR-ENTRY_QNT,
        VL_QTD_AVISO_AUX  TYPE STRING,
        VL_QTD_ENT_AUX    TYPE STRING.

  DATA: I_GNAME TYPE SEQG3-GNAME,
        I_GARG  TYPE SEQG3-GARG,
        IT_ENQ  TYPE TABLE OF SEQG3.

  LOOP AT IT_ZMMT_EE_ZGR INTO WA_MOV_ESTQ.
    PERFORM Z_ATUALIZA_TABELA USING WA_MOV_ESTQ.
  ENDLOOP.

  "Ajusta Armazem da Entrada
*  LOOP AT IT_ZMMT_EE_ZGR ASSIGNING FIELD-SYMBOL(<FS_ENTRADA>).
*
*    IF <FS_ENTRADA>-CH_REFERENCIA IS NOT INITIAL.
*
*      SELECT SINGLE * INTO @DATA(WA_ZSDT0001)
*        FROM ZSDT0001
*       WHERE CH_REFERENCIA EQ @<FS_ENTRADA>-CH_REFERENCIA.
*
*      IF SY-SUBRC IS INITIAL.
*
*        ZCL_DEPOSITO=>ZIF_DEPOSITO~GET_INSTANCE(
*            )->GET_DEPOSITO_MATERIAL_FILIAL(
*                EXPORTING
*                  I_MATNR      = WA_ZSDT0001-MATNR    " Nº do material
*                  I_TP_PRODUTO = CONV #( COND STRING( WHEN WA_ZSDT0001-TP_TRANSGENIA(1) EQ 'C' THEN ZIF_CARGA=>ST_TP_TRANSGENIASE_CO ELSE 'RR' ) )    " Tipo de Produto
*                  I_BUKRS      = WA_ZSDT0001-BUKRS    " Empresa
*                  I_BRANCH     = WA_ZSDT0001-BRANCH    " Local de negócios
*                IMPORTING
*                  E_LGORT          = <FS_ENTRADA>-LGORT    " Depósito
*            ).
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDLOOP.

  LOOP AT IT_ZMMT_EE_ZGR INTO WA_MOV_ESTQ.

    IF IT_MATERIAL_BLOQU[] IS NOT INITIAL.
      LOOP AT IT_MATERIAL_BLOQU.
        CALL FUNCTION 'ZDEQUEUE_MATERIAL'
          EXPORTING
            MATNR = IT_MATERIAL_BLOQU.
      ENDLOOP.
    ENDIF.

    IF WA_MOV_ESTQ-MATERIAL IS NOT INITIAL AND POBJKEY IS NOT INITIAL.

      DATA(CL_BLOQUEIO) = ABAP_TRUE.

      WHILE CL_BLOQUEIO EQ ABAP_TRUE.
        I_GNAME = 'ZMATNR'.
        CONCATENATE SY-MANDT WA_MOV_ESTQ-MATERIAL INTO I_GARG.

        CALL FUNCTION 'ENQUEUE_READ'
          EXPORTING
            GNAME                 = I_GNAME
            GARG                  = I_GARG
          TABLES
            ENQ                   = IT_ENQ
          EXCEPTIONS
            COMMUNICATION_FAILURE = 1
            SYSTEM_FAILURE        = 2
            OTHERS                = 3.

        IF IT_ENQ[] IS NOT INITIAL.
          CLEAR: IT_ENQ[].
          WAIT UP TO 1 SECONDS.
        ELSE.

          CALL FUNCTION 'ZENQUEUE_MATERIAL'
            EXPORTING
              MATNR          = WA_MOV_ESTQ-MATERIAL
              _WAIT          = ABAP_TRUE
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.

          IF SY-SUBRC IS INITIAL.
            CL_BLOQUEIO = ABAP_FALSE.
          ELSE.
            MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.
      ENDWHILE.

    ENDIF.

    CLEAR: DOC_NUMBER_REF.

    SELECT SINGLE *
      FROM SETLEAF
      INTO WA_SETLEAF
     WHERE SETNAME = 'MAGGI_EMPRESA_EXTERIOR'
       AND VALFROM = WA_MOV_ESTQ-COMP_CODE.

    IF SY-SUBRC IS INITIAL .
      VL_CENTRO_AR = 'S'.
    ELSE.
      VL_CENTRO_AR = 'N'.
    ENDIF.

    CLEAR DOC_GERADOS.

    VG_INDEX = SY-TABIX.
    VG_OBJ_KEY = WA_MOV_ESTQ-OBJ_KEY.

    SELECT SINGLE * INTO DOC_GERADOS
      FROM ZMMT_EE_ZGR_DOCS
     WHERE OBJ_KEY EQ WA_MOV_ESTQ-OBJ_KEY.


    "Verifica se já foi criado Aviso de Recebimento no SAP(ZLES0113) para a NF/Serie.
*    CLEAR: wa_zlest0109, doc_aviso_ger.
*
*    IF ( wa_mov_estq-ref_doc_no IS NOT INITIAL ) AND
*       ( wa_mov_estq-in_aviso_receb = 'S'      ) AND
*       ( wa_mov_estq-tp_operacao    = '01'     ).
*
*      CLEAR: wa_zlest0109, vl_nro_nf  , vl_serie_nf      , vl_nro_nf_aux , vl_serie_nf_aux,
*             vl_ini_copy , vl_fim_copy, vl_qtde_aviso_rec, vl_qtde_ent_gr.
*
*      FIND '-' IN wa_mov_estq-ref_doc_no MATCH OFFSET vl_ini_copy.
*
*      "Extrair Numero NF
*      vl_nro_nf_aux = wa_mov_estq-ref_doc_no(vl_ini_copy).
*      ADD 1 TO vl_ini_copy.
*
*      "Extrair Serie NF
*      vl_fim_copy = strlen( wa_mov_estq-ref_doc_no ) - vl_ini_copy.
*
*      vl_serie_nf_aux = wa_mov_estq-ref_doc_no+vl_ini_copy(vl_fim_copy).
*
*      vl_nro_nf   = vl_nro_nf_aux.
*      vl_serie_nf = vl_serie_nf_aux.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = vl_nro_nf
*        IMPORTING
*          output = vl_nro_nf.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = vl_serie_nf
*        IMPORTING
*          output = vl_serie_nf.
*
*      SELECT SINGLE
*             a~ebeln a~ebelp a~werks a~lgort a~charg a~vbeln a~lifnr
*             a~matnr a~nfnum a~serie a~qtde_aviso a~unidade
*        INTO wa_zlest0109
*        FROM zlest0109 AS a
*        INNER JOIN likp AS b ON a~vbeln = b~vbeln
*       WHERE ebeln = wa_mov_estq-po_number
*         AND ebelp = wa_mov_estq-po_item
*         AND nfnum = vl_nro_nf
*         AND serie = vl_serie_nf.
*
*      IF ( sy-subrc = 0 ) AND ( wa_zlest0109-vbeln IS NOT INITIAL ).
*        vl_qtde_aviso_rec = wa_zlest0109-qtde_aviso.
*        vl_qtde_ent_gr    = wa_mov_estq-entry_qnt.
*        IF vl_qtde_aviso_rec NE vl_qtde_ent_gr.
*
*          vl_qtd_aviso_aux = vl_qtde_aviso_rec.
*          vl_qtd_ent_aux   = vl_qtde_ent_gr.
*          CONCATENATE 'Aviso de Recebimento já gerado no SAP para a Chave de Referência:' wa_mov_estq-obj_key ', com quantidades distintas:'
*                      'NF-Serie:'        vl_nro_nf_aux '-' vl_serie_nf_aux '/'
*                      'Nro. Aviso Rec.:' wa_zlest0109-vbeln '/'
*                      'Qtde Aviso:'      vl_qtd_aviso_aux '/'
*                      'Qtde Entrada: '   vl_qtd_ent_aux '!' INTO vg_return SEPARATED BY space.
*          PERFORM z_prepara_mensagem2 USING vg_obj_key c_e vg_return wa_mov_estq-obj_key space c_11.
*          CONTINUE.
*        ENDIF.
*
*        MOVE-CORRESPONDING  wa_zlest0109 TO doc_aviso_ger.
*
*      ENDIF.
*
*    ENDIF. "Fim - Verifica se já foi criado Aviso de Recebimento no SAP(ZLES0113) para a NF/Serie.

    DATA(CK_COTINUE) = ABAP_FALSE.
    PERFORM VERIFICA_MIRO IN PROGRAM ZMMR019_01 IF FOUND USING WA_MOV_ESTQ VG_OBJ_KEY CHANGING CK_COTINUE VG_RETURN .
    IF CK_COTINUE EQ ABAP_TRUE.
      PERFORM Z_PREPARA_MENSAGEM2 USING VG_OBJ_KEY C_E VG_RETURN WA_MOV_ESTQ-OBJ_KEY SPACE C_11.
      CONTINUE.
    ENDIF.

* Validação de notas
    SELECT SINGLE DOCNUM
      FROM J_1BNFDOC
      INTO (VL_DOCNUM)
     WHERE DOCDAT EQ WA_MOV_ESTQ-DOC_DATE
       AND PSTDAT EQ WA_MOV_ESTQ-PSTNG_DATE
       AND BUKRS  EQ WA_MOV_ESTQ-COMP_CODE
       AND BRANCH EQ WA_MOV_ESTQ-PLANT
       AND PARID  EQ WA_MOV_ESTQ-LIFNR
       AND NFE    EQ 'X'
       AND NFENUM EQ WA_MOV_ESTQ-NFENUM
       AND CANCEL NE 'X'.

    IF ( SY-SUBRC EQ 0 ).
      CLEAR WA_OUTRETURN.
      WA_OUTRETURN-MESSAGE_V1 = WA_MOV_ESTQ-NFENUM.
      WA_OUTRETURN-MESSAGE_V2 = VL_DOCNUM.

      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          ID         = 'Z01'
          NUMBER     = '009'
          LANGUAGE   = SY-LANGU
          TEXTFORMAT = 'ASC'
          MESSAGE_V1 = WA_OUTRETURN-MESSAGE_V1
          MESSAGE_V2 = WA_OUTRETURN-MESSAGE_V2
        IMPORTING
          MESSAGE    = WA_OUTRETURN-MESSAGE.

      PERFORM Z_PREPARA_MENSAGEM2 USING VG_OBJ_KEY C_E WA_OUTRETURN-MESSAGE WA_OUTRETURN-MESSAGE_V1 WA_OUTRETURN-MESSAGE_V2 C_11.
      CONTINUE.
    ENDIF.

    PERFORM VERIFICA_FORNECEDOR_NFE USING WA_MOV_ESTQ VG_VALIDA_IMP.
    IF NOT VG_VALIDA_IMP IS INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM VERIFICA_FORNECEDOR USING WA_MOV_ESTQ VG_VALIDA_IMP.
    IF NOT VG_VALIDA_IMP IS INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM VERIFICA_IMPOSTOS_RETIDOS USING WA_MOV_ESTQ VG_VALIDA_IMP.
    IF NOT VG_VALIDA_IMP IS INITIAL.
      CONTINUE.
    ENDIF.

***********************************************************************
* Executa BAPIs
***********************************************************************

    CLEAR: VG_ERRO,
           VL_ERRO_EST_MIRO, VL_ERRO_EST_MIGO,
           VG_INVOICEDOCNUMBER_MIGO, VG_ANO_MIGO,
           VG_INVOICEDOCNUMBER_MIRO, VG_ANO_MIRO.

    "ALRS
    DOC_GERADOS-OBJ_KEY = WA_MOV_ESTQ-OBJ_KEY.
    DOC_GERADOS-BUKRS   = WA_MOV_ESTQ-COMP_CODE.
    DOC_GERADOS-BRANCH  = WA_MOV_ESTQ-PLANT.

    "PERFORM VERIFICAR_MIGO_CHAVE_REF

***Verifica se é necessário o estorno
    IF WA_MOV_ESTQ-IN_AVISO_RECEB = C_S.
      "Executa call transaction para VL31N
*      PERFORM z_batch_vl31n USING wa_mov_estq doc_gerados doc_aviso_ger.
*      IF vg_erro EQ c_x.
*        DELETE it_outreturn WHERE obj_key EQ wa_mov_estq-obj_key AND type EQ c_s.
*        DELETE it_outreturn WHERE obj_key EQ wa_mov_estq-obj_key AND type EQ c_w.
*        CONTINUE.
*      ENDIF.
    ENDIF.

    IF VG_ERRO IS INITIAL .
      "Executa BAPI para criação da MIGO

      IF ( WA_MOV_ESTQ-TP_OPERACAO EQ C_01 AND WA_MOV_ESTQ-MOVE_TYPE EQ C_101 AND WA_MOV_ESTQ-IN_AVISO_RECEB NE C_S AND VL_CENTRO_AR = 'N' ) OR  "Brasil tem que ter tp_operacao eq c_01 E MOVE_TYPE eq c_101
         ( WA_MOV_ESTQ-MOVE_TYPE EQ C_101 AND WA_MOV_ESTQ-IN_AVISO_RECEB NE C_S AND  VL_CENTRO_AR = 'S' AND  WA_MOV_ESTQ-CDV NE 'X').            "Argentina tem que ter apenas MOVE_TYPE eq c_101
        "PERFORM z_bapi_migo USING wa_mov_estq doc_gerados.
      ENDIF.
      IF VG_ERRO IS INITIAL AND WA_MOV_ESTQ-TP_OPERACAO NE C_08 AND WA_MOV_ESTQ-TP_OPERACAO NE C_10 AND
        ( ( WA_MOV_ESTQ-MOVE_TYPE IS INITIAL AND VL_CENTRO_AR = 'S')  OR ( VL_CENTRO_AR = 'N' ) OR ( VL_CENTRO_AR = 'S' AND ( WA_MOV_ESTQ-TEXT2 EQ 'FAT' OR WA_MOV_ESTQ-TEXT2 EQ 'FCA' ) ) ).

*        "Check Saldo Centro Fixo e à Fixar.
*        IF ( WA_MOV_ESTQ-TP_OPERACAO = C_02  ) AND
*           ( NOT ( WA_MOV_ESTQ-CDV = 'X' AND VL_CENTRO_AR = 'S' ) ).
*          PERFORM F_SALDO_CENTRO_FIXO_FIXAR USING WA_MOV_ESTQ.
*        ENDIF.

        "Executa BAPI para criação da MIRO
        IF VG_ERRO IS INITIAL.
          PERFORM Z_BAPI_MIRO USING WA_MOV_ESTQ DOC_GERADOS DOC_NUMBER_REF.
        ENDIF.

        IF WA_MOV_ESTQ-TP_OPERACAO = C_02 AND
          VG_ERRO IS INITIAL.
          CLEAR WA_ITEM.
          REFRESH IT_ITEM.

          IF NOT ( WA_MOV_ESTQ-CDV = 'X' AND VL_CENTRO_AR = 'S' ) AND ( WA_MOV_ESTQ-TP_OPERACAO NE C_09 ).
            "PERFORM z_bapi_migo USING wa_mov_estq doc_gerados.
          ENDIF.

        ENDIF.
      ENDIF.

      IF ( WA_MOV_ESTQ-TP_OPERACAO EQ C_08 OR WA_MOV_ESTQ-TP_OPERACAO EQ C_10  ) AND DOC_GERADOS-MM_MBLNR IS INITIAL.
        IF NOT ( WA_MOV_ESTQ-CDV = 'X' AND VL_CENTRO_AR = 'S' ).
          " PERFORM z_bapi_migo USING wa_mov_estq doc_gerados.
        ENDIF.
      ENDIF.

    ENDIF.

***Verifica o controle de erro
    IF ( VG_ERRO IS NOT INITIAL ) AND ( WA_MOV_ESTQ-IN_AVISO_RECEB = C_S ).

*      IF ( doc_gerados-av_vbeln IS NOT INITIAL ).
*        PERFORM z_estorno_picking USING doc_gerados-av_vbeln
*                               CHANGING vl_erro_est_migo.
*
*        IF ( doc_aviso_ger IS INITIAL ). "Caso Aviso de Recebimento não foi gerado pela ZLES0113
*          vg_recebimento = doc_gerados-av_vbeln.
*          PERFORM: z_estorna_recebimento USING doc_gerados.
*        ENDIF.
*
*      ENDIF.

    ELSEIF ( VG_ERRO IS NOT INITIAL ) AND ( WA_MOV_ESTQ-TP_OPERACAO NE C_03 ).
      PERFORM Z_ESTORNA_MIRO USING DOC_GERADOS
                          CHANGING VL_ERRO_EST_MIRO.

      "PERFORM z_estorna_migo USING doc_gerados
      "                        CHANGING vl_erro_est_migo.

    ENDIF.

    "IF vg_erro IS NOT INITIAL.
    "  PERFORM f_check_reg_docs USING doc_gerados.
    "ENDIF.

    IF VG_ERRO IS INITIAL.
      PERFORM: Z_BAPI_TRANS_COMMIT.

*-CS2020000574 - 10.12.2020 - inicio
      IF ( WA_MOV_ESTQ-MOVE_TYPE = 'ZY1'    OR
           WA_MOV_ESTQ-MOVE_TYPE = 'ZY2'    OR
           WA_MOV_ESTQ-MOVE_TYPE = 'ZY4' ) AND "remessa entrega futura grãos
           WA_MOV_ESTQ-COMP_CODE <> '0100' AND
           WA_MOV_ESTQ-COMP_CODE <> '0101'.
        PERFORM Z_GRAVA_CONTABIL USING WA_MOV_ESTQ
                                       ''.
      ENDIF.
*-CS2020000574 - 10.12.2020 - fim

      "Comentando para subir melhoria retorno doc. contabil - WPP - Ini


      IF WA_MOV_ESTQ-MOVE_TYPE   = '101' AND
         (  WA_MOV_ESTQ-J_1BNFTYPE  = 'TF' OR
            WA_MOV_ESTQ-J_1BNFTYPE  = 'T4' OR
            WA_MOV_ESTQ-J_1BNFTYPE  = 'T4' OR
            WA_MOV_ESTQ-J_1BNFTYPE  = 'TG' OR
            WA_MOV_ESTQ-J_1BNFTYPE  = 'T5' OR
            WA_MOV_ESTQ-J_1BNFTYPE  = 'TO'  ) AND
            WA_MOV_ESTQ-COMP_CODE <> '0100' OR WA_MOV_ESTQ-COMP_CODE <> '0101'.
        PERFORM Z_GRAVA_CONTABIL_MILHO USING WA_MOV_ESTQ
                                             ''.
      ENDIF.
      "Comentando para subir melhoria retorno doc. contabil - WPP - Fim




      READ TABLE IT_J_1BAA
        WITH KEY NFTYPE = WA_MOV_ESTQ-J_1BNFTYPE
        BINARY SEARCH
        TRANSPORTING NO FIELDS.

      IF SY-SUBRC EQ 0.
        WA_J_1BNFDOC_AUX-NFENUM = WA_MOV_ESTQ-NT_REMESSA.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = WA_J_1BNFDOC_AUX-NFENUM
          IMPORTING
            OUTPUT = WA_J_1BNFDOC_AUX-NFENUM.

        WA_J_1BNFDOC_AUX-PARID   = VG_LIFNR.
        WA_J_1BNFDOC_AUX-AUTHCOD = WA_MOV_ESTQ-AUTHCOD.

        APPEND WA_J_1BNFDOC_AUX
          TO IT_J_1BNFDOC_AUX.
      ENDIF.
    ENDIF.

    "IF wa_mov_estq-tp_operacao EQ c_04 AND vl_centro_ar EQ 'N' AND vg_erro IS INITIAL.
    "  PERFORM z_cria_nf.
    "ENDIF. "Enqueue

    "Gravar quando.:
    " 1 - Não possuir erro na geração dos documentos
    " 2 - Gerar parte dos documentos e não conseguir fazer o estorno dos mesmos.
    "     (Ex.: Gravar MIGO, não conseguir gerar MIRO na sequencia, e não conseguir
    "           estornar a MIGO devido Material estar Bloqueado ).
    IF ( DOC_GERADOS IS NOT INITIAL      ) AND
       ( VG_ERRO          IS INITIAL     OR   "Não houve nenhum erro no processo
         VL_ERRO_EST_MIRO IS NOT INITIAL OR   "Erro no estorno da MIRO criada
         VL_ERRO_EST_MIGO IS NOT INITIAL ).   "Erro no estorno da MIGO criada

      DOC_GERADOS-OBJ_KEY = WA_MOV_ESTQ-OBJ_KEY.
      DOC_GERADOS-BUKRS   = WA_MOV_ESTQ-COMP_CODE.
      DOC_GERADOS-BRANCH  = WA_MOV_ESTQ-PLANT.

      IF WA_MOV_ESTQ-TP_OPERACAO EQ C_08 OR WA_MOV_ESTQ-TP_OPERACAO EQ C_10 AND DOC_GERADOS-MM_MBLNR IS NOT INITIAL.
        DATA: LC_REFKEY TYPE J_1BREFKEY.
        LC_REFKEY = DOC_GERADOS-MM_MBLNR && DOC_GERADOS-MM_MJAHR.
        SELECT SINGLE * INTO @DATA(WA_J_1BNFLIN_MD)
          FROM J_1BNFLIN
         WHERE REFTYP EQ 'MD'
           AND REFKEY EQ @LC_REFKEY.
        IF SY-SUBRC IS INITIAL.
          DOC_GERADOS-DOCNUM = WA_J_1BNFLIN_MD-DOCNUM.
        ENDIF.
      ENDIF.

      MODIFY ZMMT_EE_ZGR_DOCS FROM DOC_GERADOS.

      "Gerar Movimento para Controle Certificação Socioambiental.
      "PERFORM z_check_mov_certificado USING wa_mov_estq doc_gerados.

    ENDIF.

  ENDLOOP.

  IF IT_MATERIAL_BLOQU[] IS NOT INITIAL.
    LOOP AT IT_MATERIAL_BLOQU.
      CALL FUNCTION 'ZDEQUEUE_MATERIAL'
        EXPORTING
          MATNR = IT_MATERIAL_BLOQU.
    ENDLOOP.
  ENDIF.

  IF NOT IT_VBFA_AUX IS INITIAL.
    SELECT VBELV VBELN
      FROM VBFA
      INTO TABLE IT_VBFA
      FOR ALL ENTRIES IN IT_VBFA_AUX
      WHERE VBELV EQ IT_VBFA_AUX-VBELV AND
            POSNV EQ '000010'.

    IF SY-SUBRC EQ 0.
      SORT IT_VBFA BY VBELV VBELN DESCENDING.

      VG_INTERFACE = C_11.

      LOOP AT IT_VBFA_AUX INTO WA_VBFA_AUX.
        READ TABLE IT_VBFA
          INTO WA_VBFA
          WITH KEY VBELV = WA_VBFA_AUX-VBELV
          BINARY SEARCH.

        IF SY-SUBRC EQ 0.
          MESSAGE S012(MIGO) WITH WA_VBFA-VBELN INTO VG_RETURN.

          PERFORM Z_PREPARA_MENSAGEM2 USING WA_VBFA_AUX-OBJ_KEY
                                            C_S
                                            VG_RETURN
                                            WA_VBFA-VBELN
                                            SPACE
                                            VG_INTERFACE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.


  IF NOT IT_J_1BNFDOC_AUX IS INITIAL.
    SORT IT_J_1BNFDOC_AUX BY NFENUM PARID.

    DELETE ADJACENT DUPLICATES
      FROM IT_J_1BNFDOC_AUX COMPARING NFENUM PARID.

    SELECT DOCNUM PARID NFENUM AUTHCOD
      FROM J_1BNFDOC
      INTO TABLE IT_J_1BNFDOC
      FOR ALL ENTRIES IN IT_J_1BNFDOC_AUX
      WHERE NFENUM EQ IT_J_1BNFDOC_AUX-NFENUM AND
            PARID  EQ IT_J_1BNFDOC_AUX-PARID  AND
            PARVW  EQ C_LF.

    IF SY-SUBRC EQ 0.
      LOOP AT IT_J_1BNFDOC INTO WA_J_1BNFDOC.
        CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
          EXPORTING
            DOC_NUMBER         = WA_J_1BNFDOC-DOCNUM
          IMPORTING
            DOC_HEADER         = WA_DOC_HEADER
          TABLES
            DOC_PARTNER        = IT_DOC_PARTNER
            DOC_ITEM           = IT_DOC_ITEM
            DOC_ITEM_TAX       = IT_DOC_ITEM_TAX
            DOC_HEADER_MSG     = IT_DOC_HEADER_MSG
            DOC_REFER_MSG      = IT_DOC_REFER_MSG
            DOC_OT_PARTNER     = IT_DOC_OT_PARTNER
          EXCEPTIONS
            DOCUMENT_NOT_FOUND = 1
            DOCUM_LOCK         = 2
            OTHERS             = 3.

        IF SY-SUBRC EQ 0.
          READ TABLE IT_J_1BNFDOC_AUX
            INTO WA_J_1BNFDOC_AUX
            WITH KEY NFENUM = WA_J_1BNFDOC-NFENUM
                     PARID  = WA_J_1BNFDOC-PARID
            BINARY SEARCH.

          IF SY-SUBRC EQ 0.
            WA_DOC_HEADER-AUTHCOD = WA_J_1BNFDOC_AUX-AUTHCOD.
*            wa_doc_header-conting = wa_j_1bnfdoc_aux-conting.

            CALL FUNCTION 'J_1B_NF_DOCUMENT_UPDATE'
              EXPORTING
                DOC_NUMBER            = WA_J_1BNFDOC-DOCNUM
                DOC_HEADER            = WA_DOC_HEADER
              TABLES
                DOC_PARTNER           = IT_DOC_PARTNER
                DOC_ITEM              = IT_DOC_ITEM
                DOC_ITEM_TAX          = IT_DOC_ITEM_TAX
                DOC_HEADER_MSG        = IT_DOC_HEADER_MSG
                DOC_REFER_MSG         = IT_DOC_REFER_MSG
                DOC_OT_PARTNER        = IT_DOC_OT_PARTNER
              EXCEPTIONS
                DOCUMENT_NOT_FOUND    = 1
                UPDATE_PROBLEM        = 2
                DOC_NUMBER_IS_INITIAL = 3
                OTHERS                = 4.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  LOOP AT IT_ZMMT_EE_ZGR INTO DATA(WA_ENTRADA).
    TRY .
        OBJ_ENTRADA->ZIF_CARGA~DESBLOQUEAR_ENTRADA( I_OBJ_KEY = WA_ENTRADA-OBJ_KEY ).
      CATCH ZCX_CARGA.
    ENDTRY.
  ENDLOOP.

ENDFORM.                    " Z_EXECUTA_BAPIS
*&---------------------------------------------------------------------*
*&      Form  Z_BATCH_VL31N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM Z_BATCH_VL31N USING WA_MOV_ESTQ    TYPE ZMMT_EE_ZGR
                         WA_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS
                         DOC_AVISO_GER  TYPE ZLEST0109.

  DATA: I_XBLNR	TYPE XBLNR_V1.

  DATA: GERA_APENAS_AVISO TYPE CHAR1.

  IF WA_DOC_GERADOS-AV_VBELN IS NOT INITIAL.
    EXIT.
  ENDIF.

  IF WA_MOV_ESTQ-TP_OPERACAO = '08' .
    GERA_APENAS_AVISO = 'X'.
  ELSE.
    GERA_APENAS_AVISO = ''.
  ENDIF.

  DATA(CK_CONTINUAR) = ABAP_TRUE.

*  WHILE CK_CONTINUAR EQ ABAP_TRUE.

  CALL FUNCTION 'Z_MM_CRIAR_AVISO'
    EXPORTING
      WA_MOV_ESTQ        = WA_MOV_ESTQ
      GERAR_APENAS_AVISO = GERA_APENAS_AVISO
      DOC_AVISO_GER      = DOC_AVISO_GER
    IMPORTING
      DOC_GERADOS        = WA_DOC_GERADOS
    TABLES
      IT_OUT             = IT_OUTRETURN
    EXCEPTIONS
      ERROR              = 1
      OTHERS             = 2.

*    IF WA_DOC_GERADOS-AV_VBELN IS INITIAL.
*      CLEAR: IT_RETURN[], IT_RETURN, WA_RETURN.
*      LOOP AT IT_OUTRETURN INTO DATA(WA_OUTRETURN).
*        WA_RETURN-TYPE        = WA_OUTRETURN-TYPE.
*        WA_RETURN-ID          = WA_OUTRETURN-ID.
*        WA_RETURN-NUMBER      = WA_OUTRETURN-NUM.
*        WA_RETURN-MESSAGE     = WA_OUTRETURN-MESSAGE.
*        WA_RETURN-MESSAGE_V1  = WA_OUTRETURN-MESSAGE_V1.
*        WA_RETURN-MESSAGE_V2  = WA_OUTRETURN-MESSAGE_V2.
*        WA_RETURN-MESSAGE_V3  = WA_OUTRETURN-MESSAGE_V3.
*        WA_RETURN-MESSAGE_V4  = WA_OUTRETURN-MESSAGE_V4.
*        APPEND WA_RETURN TO IT_RETURN.
*      ENDLOOP.
*      CLEAR: WA_RETURN.
*      DATA(_BLOQ) = ABAP_FALSE.
*      PERFORM F_CHECK_MSG_BLOQ TABLES IT_RETURN CHANGING _BLOQ WA_RETURN.
*      IF _BLOQ EQ ABAP_FALSE.
*        CK_CONTINUAR = ABAP_FALSE.
*      ELSE.
*        MESSAGE ID WA_RETURN-ID TYPE 'S'
*         NUMBER WA_RETURN-NUMBER
*           WITH WA_RETURN-MESSAGE_V1 WA_RETURN-MESSAGE_V2 WA_RETURN-MESSAGE_V3 WA_RETURN-MESSAGE_V4
*        DISPLAY LIKE 'E'.
*        WAIT UP TO 1 SECONDS.
*        CLEAR: IT_OUTRETURN[].
*      ENDIF.
*
*    ELSE.
*      CK_CONTINUAR = ABAP_FALSE.
*    ENDIF.
*  ENDWHILE.

  IF NOT SY-SUBRC IS INITIAL.

    VG_ERRO = C_X.

    CLEAR: IT_RETURN[], IT_RETURN, WA_RETURN.
    LOOP AT IT_OUTRETURN INTO DATA(WA_OUTRETURN).
      WA_RETURN-TYPE        = WA_OUTRETURN-TYPE.
      WA_RETURN-ID          = WA_OUTRETURN-ID.
      WA_RETURN-NUMBER      = WA_OUTRETURN-NUM.
      WA_RETURN-MESSAGE     = WA_OUTRETURN-MESSAGE.
      WA_RETURN-MESSAGE_V1  = WA_OUTRETURN-MESSAGE_V1.
      WA_RETURN-MESSAGE_V2  = WA_OUTRETURN-MESSAGE_V2.
      WA_RETURN-MESSAGE_V3  = WA_OUTRETURN-MESSAGE_V3.
      WA_RETURN-MESSAGE_V4  = WA_OUTRETURN-MESSAGE_V4.
      APPEND WA_RETURN TO IT_RETURN.
    ENDLOOP.

    LOOP AT IT_RETURN INTO DATA(WA_RETURN_INTERNO).
      MESSAGE ID WA_RETURN_INTERNO-ID TYPE 'S'
       NUMBER WA_RETURN_INTERNO-NUMBER
         WITH WA_RETURN_INTERNO-MESSAGE_V1 WA_RETURN_INTERNO-MESSAGE_V2 WA_RETURN_INTERNO-MESSAGE_V3 WA_RETURN_INTERNO-MESSAGE_V4
      DISPLAY LIKE 'E'.
    ENDLOOP.

  ELSE.
    WA_VBFA_AUX-VBELV   = WA_DOC_GERADOS-AV_VBELN.
    WA_VBFA_AUX-OBJ_KEY = WA_MOV_ESTQ-OBJ_KEY.

    IF NOT WA_DOC_GERADOS-AV_VBELN IS INITIAL.

      I_XBLNR = WA_MOV_ESTQ-NT_REMESSA.

      CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
        EXPORTING
          I_VBELN           = WA_DOC_GERADOS-AV_VBELN
          I_XBLNR           = I_XBLNR
        EXCEPTIONS
          DOCUMENT_BLOCKED  = 1
          UPDATE_NO_SUCCESS = 2
          OTHERS            = 3.

      IF SY-SUBRC IS NOT INITIAL.
        WAIT UP TO 2 SECONDS.
        CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
          EXPORTING
            I_VBELN           = WA_DOC_GERADOS-AV_VBELN
            I_XBLNR           = I_XBLNR
          EXCEPTIONS
            DOCUMENT_BLOCKED  = 1
            UPDATE_NO_SUCCESS = 2
            OTHERS            = 3.

        IF SY-SUBRC IS NOT INITIAL.
          WAIT UP TO 2 SECONDS.
          CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
            EXPORTING
              I_VBELN           = WA_DOC_GERADOS-AV_VBELN
              I_XBLNR           = I_XBLNR
            EXCEPTIONS
              DOCUMENT_BLOCKED  = 1
              UPDATE_NO_SUCCESS = 2
              OTHERS            = 3.
        ENDIF.
      ENDIF.

      SELECT SINGLE VBELN MJAHR INTO (WA_DOC_GERADOS-MM_MBLNR, WA_DOC_GERADOS-MM_MJAHR)
        FROM VBFA AS A
       WHERE VBTYP_N EQ 'R'
         AND VBTYP_V EQ '7'
         AND VBELV   EQ WA_DOC_GERADOS-AV_VBELN
         AND NOT EXISTS ( SELECT MBLNR FROM MSEG AS B WHERE B~SMBLN = A~VBELN ).

      "alrs
      MODIFY ZMMT_EE_ZGR_DOCS FROM WA_DOC_GERADOS.
      COMMIT WORK.

    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_VBFA_AUX-VBELV
      IMPORTING
        OUTPUT = WA_VBFA_AUX-VBELV.

    APPEND WA_VBFA_AUX TO IT_VBFA_AUX.

  ENDIF.

*  data: vl_mode       type c,
*        vl_data       type dats,
*        vl_data_em    type dats,
*        vl_lfimg(20)  type c,
*        vl_lines      type i,
*        vl_brgew(20)  type c,
*        it_xekes_aux type table of uekes.
*
*  concatenate wa_mov_estq-doc_date+6(2) wa_mov_estq-doc_date+4(2)
*              wa_mov_estq-doc_date(4) into vl_data.
*
*  concatenate wa_mov_estq-pstng_date+6(2) wa_mov_estq-pstng_date+4(2)
*              wa_mov_estq-pstng_date(4) into vl_data_em.
*
*  vl_lfimg   = wa_mov_estq-entry_qnt.
*  vl_brgew   = wa_mov_estq-peso_bruto.
*
*  shift vl_lfimg left deleting leading space.
*  translate vl_lfimg using '.,'.
*
*  shift vl_brgew left deleting leading space.
*  translate vl_brgew using '.,'.
*  condense vl_brgew.
*  clear it_bdcdata.
*
*  perform z_preenche_dbc using: 'X' 'SAPMV50A'       '4007',
*                                ' ' 'BDC_CURSOR'     'RV50A-LFDAT_LA',
*                                ' ' 'BDC_OKCODE'     '/00',
*                                ' ' 'LV50C-BSTNR'     wa_mov_estq-po_number,
*                                ' ' 'RV50A-LFDAT_LA' vl_data.
*
*  perform z_preenche_dbc using: 'X' 'SAPMV50A'   '1000',
*                                ' ' 'BDC_OKCODE' '=T\01'.
*
*  perform z_preenche_dbc using: 'X' 'SAPMV50A'           '1000',
*                                ' ' 'BDC_OKCODE'         '=T\03',
*                                ' ' 'BDC_SUBSCR'         'SAPMV50A',
*                                ' ' 'LIKP-BLDAT'         vl_data,
*                                ' ' 'BDC_SUBSCR'         'SAPMV50A',
*                                ' ' 'BDC_CURSOR'         'LIPS-VRKME(01)',
*                                ' ' 'RV50A-LFDAT_LA'     vl_data,
*                                ' ' 'RV50A-WADAT_IST_LA' vl_data_em,
*                                ' ' 'BDC_SUBSCR'         'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'         'SAPMV50A'.
*
*
*  perform z_preenche_dbc using: 'X' 'SAPMV50A'          '1000',
*                                ' ' 'BDC_OKCODE'        '/00',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_CURSOR'        'LIPS-GEWEI(01)',
*                                ' ' 'LIPSD-G_LFIMG(01)' vl_lfimg,
*                                ' ' 'LIPS-VRKME(01)'    wa_mov_estq-meins,
*                                ' ' 'LIPS-BRGEW(01)'    vl_brgew,
*                                ' ' 'LIPS-GEWEI(01)'    wa_mov_estq-meins,
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A'.
*
*  perform z_preenche_dbc using: 'X' 'SAPMV50A'          '1000',
*                                ' ' 'BDC_OKCODE'        '=SICH_T',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_CURSOR'        'LIPS-GEWEI(01)',
*                                ' ' 'LIPSD-G_LFIMG(01)' vl_lfimg,
*                                ' ' 'LIPS-VRKME(01)'    wa_mov_estq-meins,
*                                ' ' 'LIPS-BRGEW(01)'    vl_brgew,
*                                ' ' 'LIPS-GEWEI(01)'    wa_mov_estq-meins,
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A',
*                                ' ' 'BDC_SUBSCR'        'SAPMV50A'.
*
*
*  free: it_msg.
*
*  vl_mode = c_n.
*
****Chama a transação VL31N
*  call transaction c_vl31n
*     using it_bdcdata
*     mode   vl_mode
*     update c_s
*     messages into it_msg.
*
***Verifica se houve algum erro no processo da VL31N
*  read table it_msg into wa_msg with key msgtyp = c_e.
*  if sy-subrc = 0.
*    vg_erro = c_x.
*  else.
*    clear vg_erro.
*    "Verifica a msg de sucesso com o número criado
*    read table it_msg into wa_msg
*                  with key msgtyp = c_s
*                           msgnr  = c_311.
*    if sy-subrc is initial.
*      vg_interface = c_10.
*
*      call function 'MESSAGE_PREPARE'
*        exporting
*          msg_id                 = wa_msg-msgid
*          msg_no                 = wa_msg-msgnr
*          msg_var1               = wa_msg-msgv1(50)
*          msg_var2               = wa_msg-msgv2(50)
*          msg_var3               = wa_msg-msgv3(50)
*          msg_var4               = wa_msg-msgv4(50)
*        importing
*          msg_text               = vg_return
*        exceptions
*          function_not_completed = 1
*          message_not_found      = 2
*          others                 = 3.
*
*      perform z_prepara_mensagem2 using vg_obj_key
*                                        wa_msg-msgtyp
*                                        vg_return
*                                        wa_msg-msgv2
*                                        space
*                                        vg_interface.
*
*      clear: vg_recebimento.
*      move: wa_msg-msgv2 to vg_recebimento.
*
*      wa_doc_gerados-av_vbeln = vg_recebimento.
*
*      call function 'CONVERSION_EXIT_ALPHA_INPUT'
*        exporting
*          input  = wa_doc_gerados-av_vbeln
*        importing
*          output = wa_doc_gerados-av_vbeln.
*
***Entrada de mercadoria
*      refresh it_bdcdata.
*
*      clear it_msg.
*
*      perform z_preenche_dbc using:
*       'X'  'SAPMV50A'           '4104',
*       ' '  'BDC_OKCODE'         '/00',
*       ' '  'LIKP-VBELN'        vg_recebimento.
*
*      perform z_preenche_dbc using:
*        'X'  'SAPMV50A'            '1000',
*        ' '  'BDC_OKCODE'         '=WABU_T'.
*
*      vl_mode = c_n.
*
****Chama a transação VL32N
*      call transaction c_vl32n
*         using it_bdcdata
*         mode   vl_mode
*         update c_s
*         messages into it_msg.
*
*      read table it_msg into wa_msg
*                    with key msgtyp = c_s
*                             msgnr  = c_311.
*
*      if sy-subrc eq 0.
*        wa_vbfa_aux-vbelv   = wa_msg-msgv2.
*        wa_vbfa_aux-obj_key = wa_mov_estq-obj_key.
*
*        call function 'CONVERSION_EXIT_ALPHA_INPUT'
*          exporting
*            input  = wa_vbfa_aux-vbelv
*          importing
*            output = wa_vbfa_aux-vbelv.
*
*        append wa_vbfa_aux
*          to it_vbfa_aux.
*
*        clear it_msg.
*      endif.
*    endif.
*
*    clear: wa_ekes.
**     recuperando o ultimo registro para o pedido de compra
**    LOOP AT it_ekes_srt INTO wa_ekes WHERE ebeln = wa_mov_estq-po_number.
*
**    ENDLOOP.
*
*    read table it_ekes_srt
*      into wa_ekes
*      with key ebeln = wa_mov_estq-po_number
*      binary search.
*
**    IF NOT wa_ekes-etens IS INITIAL. "
*    if sy-subrc eq 0.
*      wa_ekes-etens = wa_ekes-etens + 1.
**      INSERT wa_ekes
**        INTO TABLE it_ekes_srt.
*
*      modify it_ekes_srt
*        from wa_ekes
*        index sy-tabix.
*    else.
**      wa_ekes-ebeln = wa_mov_estq-po_number.
**      wa_ekes-ebelp = c_00010.
**      wa_ekes-etens = c_0001.
**
**      INSERT wa_ekes
**        INTO TABLE it_ekes_srt.
*
**      clear: it_xekes_aux.
**      wa_xekes-etens = c_0001. "wa_ekes-etens.
**      wa_xekes-ebeln = wa_mov_estq-po_number.
**      wa_xekes-ebelp = c_00010.
**      wa_xekes-ebtyp = c_la.
**      wa_xekes-erdat = wa_mov_estq-pstng_date.
**      wa_xekes-lpein = 1.
**      wa_xekes-kzdis = c_x.
**      wa_xekes-kz    = c_i.
**      APPEND wa_xekes
**        TO it_xekes_aux.
**
**      CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
**        EXPORTING
**          i_ebeln = wa_mov_estq-po_number
**        TABLES
**          xekes   = it_xekes_aux
**          yekes   = it_yekes.
**
*    endif.
*  endif.
*
*  loop at it_msg into wa_msg.
*    call function 'MESSAGE_PREPARE'
*      exporting
*        msg_id                 = wa_msg-msgid
*        msg_no                 = wa_msg-msgnr
*        msg_var1               = wa_msg-msgv1(50)
*        msg_var2               = wa_msg-msgv2(50)
*        msg_var3               = wa_msg-msgv3(50)
*        msg_var4               = wa_msg-msgv4(50)
*      importing
*        msg_text               = vg_return
*      exceptions
*        function_not_completed = 1
*        message_not_found      = 2
*        others                 = 3.
*
*    if sy-subrc <> 0.
** Vazio
*    endif.
*    "Identificador de interface Aviso
*    vg_interface = c_10.
** Finalidade - Retornar erros do Batch-Input p/ tab. de erro
*    perform z_prepara_mensagem   using vg_obj_key
*                                       wa_msg-msgtyp
*                                       vg_return
*                                       vg_interface.
*
*  endloop.

ENDFORM.                    " Z_BATCH_VL31N
*&---------------------------------------------------------------------*
*&      Form  Z_BAPI_MIRO
*&---------------------------------------------------------------------*
*       Carregar BAPI MIGO
*----------------------------------------------------------------------*
FORM Z_BAPI_MIGO  USING WA_MOV_ESTQ TYPE ZMMT_EE_ZGR
                        WA_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS.



ENDFORM.                    " Z_BAPI_MIRO

*&---------------------------------------------------------------------*
*&      Form  Z_GRAVA-CONTABIL
*&---------------------------------------------------------------------*
FORM Z_GRAVA_CONTABIL USING WA_MOV_ESTQ TYPE ZMMT_EE_ZGR
                            P_ESTORNO.

  DATA: L_LIFNR   TYPE EKKO-LIFNR,
        L_TAX_AMT TYPE ZIB_CONTABIL-WRBTR.

  FREE: WA_ZIB_CONTABIL,
        L_TAX_AMT.

  SELECT WI_TAX_CODE, WI_TAX_AMT
    FROM ZMMT_EEIMP_ZGR
    INTO TABLE @DATA(T_EEIMP_ZGR)
   WHERE OBJ_KEY      = @WA_MOV_ESTQ-OBJ_KEY
     AND WI_TAX_CODE IN ('F0','F1').

  SELECT LIFNR
    INTO L_LIFNR
    FROM EKKO
      UP TO 1 ROWS
   WHERE EBELN = WA_MOV_ESTQ-PO_NUMBER.
  ENDSELECT.

  LOOP AT T_EEIMP_ZGR INTO DATA(W_EEIMP_ZGR).
    L_TAX_AMT = L_TAX_AMT + W_EEIMP_ZGR-WI_TAX_AMT.
  ENDLOOP.

  CONCATENATE 'ZEFG' WA_MAT_DOC VG_ANO_MIGO '00001'
         INTO WA_ZIB_CONTABIL-OBJ_KEY.

  WA_ZIB_CONTABIL-MANDT         = SY-MANDT.
  WA_ZIB_CONTABIL-SEQITEM       = C_000001.

  IF P_ESTORNO = C_X.
    WA_ZIB_CONTABIL-BSCHL       = C_50.
  ELSE.
    WA_ZIB_CONTABIL-BSCHL       = C_21.
  ENDIF.

  WA_ZIB_CONTABIL-HKONT         = L_LIFNR.
  WA_ZIB_CONTABIL-GSBER         = WA_MOV_ESTQ-PLANT.
  WA_ZIB_CONTABIL-BUKRS         = WA_MOV_ESTQ-COMP_CODE.
  WA_ZIB_CONTABIL-INTERFACE     = C_11.

  IF P_ESTORNO = C_X.
    CONCATENATE 'Estorno' WA_MOV_ESTQ-NT_REMESSA
           INTO  WA_ZIB_CONTABIL-BKTXT
           SEPARATED BY SPACE.
  ELSE.
    WA_ZIB_CONTABIL-BKTXT       = WA_MOV_ESTQ-NT_REMESSA.
  ENDIF.


  CONCATENATE WA_MOV_ESTQ-PSTNG_DATE+6(2)
              WA_MOV_ESTQ-PSTNG_DATE+4(2)
              WA_MOV_ESTQ-PSTNG_DATE(4)
    INTO WA_ZIB_CONTABIL-BUDAT SEPARATED BY '.'.

  CONCATENATE WA_MOV_ESTQ-DOC_DATE+6(2)
              WA_MOV_ESTQ-DOC_DATE+4(2)
              WA_MOV_ESTQ-DOC_DATE(4)
    INTO WA_ZIB_CONTABIL-BLDAT SEPARATED BY '.'.

  WA_ZIB_CONTABIL-GJAHR         = WA_MOV_ESTQ-PSTNG_DATE(4).
  WA_ZIB_CONTABIL-MONAT         = WA_MOV_ESTQ-PSTNG_DATE+4(2).
  WA_ZIB_CONTABIL-BLART         = C_SA.

  CONCATENATE WA_MAT_DOC VG_ANO_MIGO
         INTO WA_ZIB_CONTABIL-XBLNR.

  WA_ZIB_CONTABIL-WRBTR         = L_TAX_AMT.
  WA_ZIB_CONTABIL-WAERS         = C_BRL.
  WA_ZIB_CONTABIL-BUPLA         = WA_MOV_ESTQ-PLANT.
  WA_ZIB_CONTABIL-RG_ATUALIZADO = C_N.


  IF WA_MOV_ESTQ-TP_OPERACAO EQ  '08'.
**=================================Inicio USER STORY 79453* / Anderson Oenning
    WA_ZIB_CONTABIL-ZUONR         = WA_MOV_ESTQ-PO_NUMBER.
    WA_ZIB_CONTABIL-KIDNO         = WA_MAT_DOC.
    WA_ZIB_CONTABIL-XREF1         = WA_MOV_ESTQ-TEXT1+7(4).
    WA_ZIB_CONTABIL-XBLNR         = WA_MOV_ESTQ-NT_REMESSA.
    WA_ZIB_CONTABIL-SGTXT         = 'Impostos Remessa Entrega Futura (Fethab e Iagro)'.

    IF WA_MOV_ESTQ-MATERIAL IS NOT INITIAL.
      WA_MOV_ESTQ-MATERIAL = |{ WA_MOV_ESTQ-MATERIAL ALPHA = IN }|.
      SELECT SINGLE MAKTX FROM MAKT INTO ( WA_ZIB_CONTABIL-XREF3 )  WHERE MATNR EQ WA_MOV_ESTQ-MATERIAL AND SPRAS EQ SY-LANGU.
    ENDIF.


**=================================Fim USER STORY 79453* / Anderson Oenning
  ENDIF.

  MODIFY ZIB_CONTABIL  FROM WA_ZIB_CONTABIL.

  LOOP AT T_EEIMP_ZGR INTO W_EEIMP_ZGR.

    CLEAR WA_TAXCODE.
    READ TABLE IT_TAXCODE INTO WA_TAXCODE WITH KEY TAX_CODE = W_EEIMP_ZGR-WI_TAX_CODE
                                          BINARY SEARCH.

    CASE  W_EEIMP_ZGR-WI_TAX_CODE.
      WHEN 'F0'.
        WA_ZIB_CONTABIL-SEQITEM = C_000002.
        IF P_ESTORNO = C_X.
          WA_ZIB_CONTABIL-BSCHL = C_40.
        ELSE.
          WA_ZIB_CONTABIL-BSCHL = C_50.
        ENDIF.
        WA_ZIB_CONTABIL-HKONT   = WA_TAXCODE-HKONT.
        WA_ZIB_CONTABIL-WRBTR   = W_EEIMP_ZGR-WI_TAX_AMT.
        WA_ZIB_CONTABIL-BKTXT   = 'FACS'.
        MODIFY ZIB_CONTABIL  FROM WA_ZIB_CONTABIL.

      WHEN 'F1'.
        WA_ZIB_CONTABIL-SEQITEM = C_000003.
        IF P_ESTORNO = C_X.
          WA_ZIB_CONTABIL-BSCHL = C_40.
        ELSE.
          WA_ZIB_CONTABIL-BSCHL = C_50.
        ENDIF.
        WA_ZIB_CONTABIL-HKONT   =  WA_TAXCODE-HKONT.
        WA_ZIB_CONTABIL-WRBTR   = W_EEIMP_ZGR-WI_TAX_AMT.
        WA_ZIB_CONTABIL-BKTXT   = 'FETHAB'.
        MODIFY ZIB_CONTABIL  FROM WA_ZIB_CONTABIL.
    ENDCASE.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_BAPI_MIRO
*&---------------------------------------------------------------------*
*       Criar BAPI MIRO
*----------------------------------------------------------------------*
FORM Z_BAPI_MIRO USING WA_MOV_ESTQ    TYPE ZMMT_EE_ZGR
                       WA_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS
                       NUMBER_REF     TYPE J_1BDOCNUM.

  DATA: VL_MESSAGE_V1       TYPE SYMSGV,
        VL_MESSAGE_V2       TYPE SYMSGV,
        I_DOC               TYPE J_1BNFDOC,
        "p_notase            type znom_reme_notase,
        I_ACTTAB            TYPE J_1BNFE_ACTIVE,
        WA_HEADERDATA_LOCAL TYPE BAPI_INCINV_CREATE_HEADER,
        WA_EKKO             TYPE EKKO,
        WA_J_1BAA           TYPE J_1BAA,
        VG_INCO1            TYPE INCO1,
        VL_BELNR            TYPE RBKP-BELNR,
        VL_TAX_AMOUNT       TYPE BAPI_INCINV_CREATE_TAX-TAX_AMOUNT,
        VL_LINEID           TYPE SETLINE,
        VL_PERCENTUAL_TX    TYPE SETLINET-DESCRIPT,
        VL_ZERAR_BASE       TYPE C,
        VL_WI_TAX_BASE      TYPE ZMMT_EEIMP_ZGR-WI_TAX_BASE,
        VL_BSART            TYPE EKKO-BSART.

  DATA: WA_SETLEAF TYPE SETLEAF.

  IF WA_DOC_GERADOS-FT_BELNR IS NOT INITIAL.
    EXIT.
  ENDIF.


***Verifica tipo de documento
* Tipo de Documento

  CLEAR: IT_ITEM,
         IT_WITHTAXDATA,
         IT_TAXDATA,
         VG_ERRO,
         VL_WI_TAX_BASE,
         VL_ZERAR_BASE.

  "Busca fornecedor para buscar o tax code
  SELECT SINGLE *
    INTO WA_EKKO
    FROM EKKO
   WHERE EBELN = WA_MOV_ESTQ-PO_NUMBER.

  VG_LIFNR = WA_EKKO-LIFNR.
  VG_INCO1 = WA_EKKO-INCO1.

  "Percepções
  VG_VR_PERCEPCOES  = 0.
  LOOP AT IT_ZMMT_EEIVA_ZGR INTO WA_ZMMT_EEIVA_ZGR WHERE OBJ_KEY   EQ WA_MOV_ESTQ-OBJ_KEY.
    CLEAR WA_TAXDATA.
    WA_TAXDATA-TAX_CODE        = WA_ZMMT_EEIVA_ZGR-MWSKZ.
    WA_TAXDATA-TAX_AMOUNT      = WA_ZMMT_EEIVA_ZGR-VLR_IMP.
    WA_TAXDATA-TAX_BASE_AMOUNT = WA_ZMMT_EEIVA_ZGR-VLR_BASE.
    APPEND WA_TAXDATA TO IT_TAXDATA.

    VG_VR_PERCEPCOES = VG_VR_PERCEPCOES + WA_ZMMT_EEIVA_ZGR-VLR_IMP.
  ENDLOOP.
  "Fim Percepções

  CLEAR VG_TOTAL_ITEM.
  PERFORM: Z_CABEC_ITEM. " Cabeçalho e itens p/ BAPI MIRO

  CLEAR: WA_ZMMT_EEIMP_ZGR,
         WA_WITHTAXDATA,
         WA_TAXDATA.

  "Impostos
  LOOP AT IT_ZMMT_EEIMP_ZGR INTO WA_ZMMT_EEIMP_ZGR WHERE OBJ_KEY   EQ WA_MOV_ESTQ-OBJ_KEY.

    IF NOT VG_LIFNR IS INITIAL.
      "Busca Tax Code
      SELECT SINGLE WITHT
        FROM LFBW
        INTO VG_WITHT
        WHERE LIFNR     = VG_LIFNR
          AND BUKRS     = WA_MOV_ESTQ-COMP_CODE
          AND WT_WITHCD = WA_ZMMT_EEIMP_ZGR-WI_TAX_CODE.

*      IF SY-SUBRC IS NOT INITIAL.
*        VG_ERRO = 'X'.
**        Cria mensagem de retorno em caso de sucesso
*        CLEAR WA_RETURN.
*
*        WA_RETURN-TYPE    = C_E.
*        WA_RETURN-ID      = C_MM.
*        WA_RETURN-NUMBER  = C_899.
*        WA_RETURN-MESSAGE = 'La inconsistencia de los impuestos entre los datos maestros de SAP y SIGAM'.
*        APPEND WA_RETURN TO IT_RETURN.
*
*        CLEAR WA_RETURN.
*
*      ENDIF.

    ENDIF.

    WA_WITHTAXDATA-SPLIT_KEY   = C_000001.
    IF NOT ( WA_ZMMT_EEIMP_ZGR-WT_WITHCD IS INITIAL ).
      WA_WITHTAXDATA-WI_TAX_TYPE = WA_ZMMT_EEIMP_ZGR-WT_WITHCD.
    ELSE.
      WA_WITHTAXDATA-WI_TAX_TYPE = VG_WITHT.
    ENDIF.
    WA_WITHTAXDATA-WI_TAX_CODE = WA_ZMMT_EEIMP_ZGR-WI_TAX_CODE. " Código do IVA
    WA_WITHTAXDATA-WI_TAX_BASE = WA_ZMMT_EEIMP_ZGR-WI_TAX_BASE. " Valor base do imposto
    WA_WITHTAXDATA-WI_TAX_AMT  = WA_ZMMT_EEIMP_ZGR-WI_TAX_AMT.  " Valor do imposto
    WA_WITHTAXDATA-WI_TAX_WITHHELD_AMT = WA_ZMMT_EEIMP_ZGR-WI_TAX_AMT.
    APPEND WA_WITHTAXDATA TO IT_WITHTAXDATA.
    CLEAR VG_WITHT.
  ENDLOOP.
  DELETE IT_WITHTAXDATA WHERE WI_TAX_BASE EQ 0.
  "Fim Impostos

  "Ajuste Base Doc. Contabil - Pgto Direto
  IF ( WA_MOV_ESTQ-COMP_CODE = '0100' ). "Argentina
    IF IT_ZMMT_EEIMP_ZGR[] IS NOT INITIAL.

      LOOP AT IT_ZMMT_EEIMP_ZGR INTO WA_ZMMT_EEIMP_ZGR WHERE OBJ_KEY  EQ WA_MOV_ESTQ-OBJ_KEY
                                                         AND WI_TAX_BASE > 0.
        VL_WI_TAX_BASE = WA_ZMMT_EEIMP_ZGR-WI_TAX_BASE.
        EXIT.
      ENDLOOP.

      IF VL_WI_TAX_BASE = 0.
        VL_ZERAR_BASE = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SINGLE BSART
    FROM EKKO
    INTO VL_BSART
   WHERE EBELN = WA_MOV_ESTQ-PO_NUMBER.

  "se o tipo de de pedido = ZGF pega na tabela de percepções
  IF VL_BSART NE 'ZGF'.
    "Paraguay
    IF WA_MOV_ESTQ-COMP_CODE = '0101' .
      REFRESH IT_WITHTAXDATA[].
      SELECT SINGLE VALFROM
        FROM SETLEAF
        INTO WA_TAXDATA-TAX_CODE
       WHERE SETNAME = 'MAGGI_101_IVA'.

    ELSE.
      WA_TAXDATA-TAX_CODE        = WA_MOV_ESTQ-TAX_CODE.
    ENDIF.

    IF IT_ITEMDATA[] IS NOT INITIAL.
      READ TABLE IT_ITEMDATA ASSIGNING FIELD-SYMBOL(<FS_ITEM>) INDEX 1.

      "Ajustar Vr Imposto para Crédito de Pis/Cofins sobre Aquisição
      PERFORM ADD_PIS_COFINS IN PROGRAM ZMMR019_07 IF FOUND
        USING WA_MOV_ESTQ CHANGING WA_MOV_ESTQ-VR_IMPOSTOS <FS_ITEM>-ITEM_AMOUNT.
    ENDIF.

    WA_TAXDATA-TAX_AMOUNT = WA_MOV_ESTQ-VR_IMPOSTOS.

    CLEAR WA_TAXDATA-TAX_BASE_AMOUNT.
    SELECT SINGLE *
      FROM SETLEAF
      INTO WA_SETLEAF
     WHERE SETNAME = 'MAGGI_EMPRESA_EXTERIOR'
       AND VALFROM = WA_MOV_ESTQ-COMP_CODE.

    IF SY-SUBRC IS INITIAL.
      WA_TAXDATA-TAX_BASE_AMOUNT = WA_MOV_ESTQ-VL_CMV.
    ELSEIF WA_MOV_ESTQ-VR_IMPOSTOS NE 0.
      WA_TAXDATA-TAX_BASE_AMOUNT = ABS( VG_TOTAL_ITEM ).
    ENDIF.

    APPEND WA_TAXDATA TO IT_TAXDATA.
  ENDIF.
* Incluida uma tabela para que sejam enviados estes registros -- Argentina
  CLEAR VL_LINEID.

  CLEAR VL_LINEID.
  "Adicionada tabela que irá enviar as percepções não será preciso mais o trecho abaixo
*  IF WA_MOV_ESTQ-CD_AFIP IS NOT INITIAL.
*    SELECT SINGLE LINEID"H2
*      FROM SETLEAF
*      INTO VL_LINEID
*     WHERE SETNAME EQ 'MAGGI_0100_IVA'
*       AND VALFROM EQ WA_MOV_ESTQ-CD_AFIP.
*
*    IF SY-SUBRC IS INITIAL.
*      SELECT SINGLE DESCRIPT
*        FROM SETLINET
*        INTO VL_PERCENTUAL_TX
*       WHERE SETNAME EQ 'MAGGI_0100_IVA'
*         AND LINEID EQ VL_LINEID
*         AND LANGU  EQ 'PT'.
*
*      CLEAR : WA_TAXDATA.
*
*      READ TABLE IT_ITEMDATA INTO  WA_ITEMDATA INDEX 1.
*
*      WA_TAXDATA-TAX_CODE   = WA_MOV_ESTQ-CD_AFIP.
*      WA_TAXDATA-TAX_AMOUNT = ( WA_MOV_ESTQ-VL_CMV * VL_PERCENTUAL_TX / 100 ) .
*
*      WA_TAXDATA-TAX_BASE_AMOUNT = WA_MOV_ESTQ-VL_CMV.
*      APPEND WA_TAXDATA TO IT_TAXDATA.
*
*    ENDIF.
*
*  ENDIF.

  IF WA_SETLEAF-VALFROM  IS NOT INITIAL AND WA_MOV_ESTQ-TAX_CODE = 'C2'.
    VL_TAX_AMOUNT  = WA_TAXDATA-TAX_BASE_AMOUNT.

    CLEAR : WA_TAXDATA.
    READ TABLE IT_ITEMDATA INTO  WA_ITEMDATA INDEX 1.

    "READ TABLE IT_ITEMDATA INTO  WA_ITEMDATA INDEX 1.
    WA_TAXDATA-TAX_CODE = 'C9'.
    WA_TAXDATA-TAX_BASE_AMOUNT  = WA_ITEMDATA-ITEM_AMOUNT - VL_TAX_AMOUNT.

    APPEND WA_TAXDATA TO IT_TAXDATA.

    IF WA_MOV_ESTQ-CODE IS NOT INITIAL.
      WA_TAXDATA-TAX_CODE         = WA_MOV_ESTQ-CODE.
      WA_TAXDATA-TAX_BASE_AMOUNT  = WA_ITEMDATA-ITEM_AMOUNT - VL_TAX_AMOUNT.

      APPEND WA_TAXDATA TO IT_TAXDATA.
    ENDIF.

  ENDIF.

  CLEAR: WA_RETURN.
  FREE: IT_RETURN.

  MOVE-CORRESPONDING WA_HEADERDATA TO WA_HEADERDATA_LOCAL.

*  IF NOT wa_headerdata_local-j_1bnftype IS INITIAL.
*    SELECT SINGLE * INTO wa_j_1baa FROM j_1baa WHERE nftype EQ wa_headerdata_local-j_1bnftype.
*    IF ( sy-subrc IS INITIAL ) AND ( wa_j_1baa-nfe EQ 'X' ) AND ( wa_j_1baa-form IS NOT INITIAL ).
*      CLEAR: wa_headerdata_local-ref_doc_no.
*    ENDIF.
*  ENDIF.

  IF VG_ERRO IS INITIAL.

    SET PARAMETER ID 'ZWERKS' FIELD WA_MOV_ESTQ-PLANT.

    IF WA_MOV_ESTQ-PO_NUMBER IS NOT INITIAL.
      ZCL_PEDIDO_COMPRA=>VERIF_BLOQ_PEDIDO_WAIT( I_EBELN = WA_MOV_ESTQ-PO_NUMBER ).
    ENDIF.

    DATA(CK_CONTINUAR) = ABAP_TRUE.

    WHILE CK_CONTINUAR EQ ABAP_TRUE.

      "Verificar se é uma entrada de nota fiscal já gerada (GRC OutBound) """"""""""""""""
      SELECT SINGLE * INTO @DATA(WA_ZSDT0231)
        FROM ZSDT0231
       WHERE OBJ_KEY EQ @WA_MOV_ESTQ-OBJ_KEY(11).

      IF SY-SUBRC IS INITIAL.
        CLEAR: WA_HEADERDATA_LOCAL-J_1BNFTYPE.
      ELSE.
*-CS2021000183 - 31.03.2022 - JT - inicio
        SELECT SINGLE * INTO WA_ZSDT0231
          FROM ZSDT0231
         WHERE OBJ_KEY EQ WA_MOV_ESTQ-OBJKEY_NP.

        IF ( SY-SUBRC = 0 ) AND ( WA_MOV_ESTQ-OBJKEY_NP IS NOT INITIAL  ).
          CLEAR: WA_HEADERDATA_LOCAL-J_1BNFTYPE.
        ELSE.
*-CS2021000183 - 31.03.2022 - JT - fim
          CLEAR: WA_ZSDT0231.
        ENDIF.
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      PERFORM EXPORT_MZMMR019 IN PROGRAM ZMMR019_03 TABLES IT_ITEMDATA IF FOUND.

      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          HEADERDATA       = WA_HEADERDATA_LOCAL
        IMPORTING
          INVOICEDOCNUMBER = VG_INVOICEDOCNUMBER_MIRO
          FISCALYEAR       = VG_ANO_MIRO
        TABLES
          ITEMDATA         = IT_ITEMDATA
          GLACCOUNTDATA    = IT_GLACCOUNTDATA
          WITHTAXDATA      = IT_WITHTAXDATA
          TAXDATA          = IT_TAXDATA
          RETURN           = IT_RETURN.

      IF VG_INVOICEDOCNUMBER_MIRO IS INITIAL.

        DATA(_BLOQ) = ABAP_FALSE.
        PERFORM F_CHECK_MSG_BLOQ TABLES IT_RETURN
                               CHANGING _BLOQ WA_RETURN.
        IF _BLOQ EQ ABAP_FALSE.
          CK_CONTINUAR = ABAP_FALSE.
        ELSE.
          MESSAGE ID WA_RETURN-ID TYPE 'S'
           NUMBER WA_RETURN-NUMBER
             WITH WA_RETURN-MESSAGE_V1 WA_RETURN-MESSAGE_V2 WA_RETURN-MESSAGE_V3 WA_RETURN-MESSAGE_V4
          DISPLAY LIKE 'E'.

          WAIT UP TO 1 SECONDS.
          CLEAR: IT_RETURN[].
        ENDIF.
      ELSE.
        CK_CONTINUAR = ABAP_FALSE.
      ENDIF.

    ENDWHILE.

    EXPORT P1 = 0 TO MEMORY ID 'MZMMR019VLRITEM'.
    EXPORT P1 = 0 TO MEMORY ID 'MZMMR019VLRITEMPIS'.
    EXPORT P1 = 0 TO MEMORY ID 'MZMMR019VLRITEMCOFINS'.

    WAIT UP TO 2 SECONDS.

    SET PARAMETER ID 'ZWERKS' FIELD ''.
    CLEAR VG_ERRO.
    IF IT_RETURN[] IS NOT INITIAL.
      READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = C_E.
      IF SY-SUBRC = 0.
        VG_ERRO = C_X.
      ELSE.
        CLEAR VG_ERRO.
      ENDIF.

    ENDIF.
  ENDIF.
  IF ( NOT VG_INVOICEDOCNUMBER_MIRO IS INITIAL ) AND ( NOT VG_ANO_MIRO IS INITIAL ).

    IF WA_ZSDT0231 IS NOT INITIAL AND WA_ZSDT0231-DOCNUM IS NOT INITIAL.

      DATA(LC_REFKEY) = VG_INVOICEDOCNUMBER_MIRO && VG_ANO_MIRO.

      UPDATE J_1BNFDOC
         SET BELNR = VG_INVOICEDOCNUMBER_MIRO
             GJAHR = VG_ANO_MIRO
       WHERE DOCNUM EQ WA_ZSDT0231-DOCNUM.

      UPDATE J_1BNFLIN
         SET REFTYP = 'LI'
             REFKEY = LC_REFKEY
             REFITM = '000001'
       WHERE DOCNUM EQ WA_ZSDT0231-DOCNUM.

    ENDIF.

    PERFORM Z_BAPI_TRANS_COMMIT.

    PERFORM CHANGE_DOC_TROCA_TEXTO_ITEM IN PROGRAM ZMMR019_04 TABLES IT_ITEMDATA USING VG_INVOICEDOCNUMBER_MIRO VG_ANO_MIRO IF FOUND.
    "PERFORM Z_GERA_E_SOCIAL_E1250M USING VG_INVOICEDOCNUMBER_MIRO VG_ANO_MIRO .

    "PERFORM Z_BAPI_TRANS_COMMIT.

    SELECT SINGLE BELNR
      INTO VL_BELNR
      FROM RBKP
     WHERE BELNR = VG_INVOICEDOCNUMBER_MIRO
       AND GJAHR = VG_ANO_MIRO.

    IF SY-SUBRC IS INITIAL.

*  Cria mensagem de retorno em caso de sucesso
      CLEAR WA_RETURN.
*  Mensagem: O documento <nr_doc> para o exercício <ano>
*            foi criado com sucesso.

      WA_DOC_GERADOS-FT_BELNR = VG_INVOICEDOCNUMBER_MIRO.
      WA_DOC_GERADOS-FT_GJAHR = VG_ANO_MIRO.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_DOC_GERADOS-FT_BELNR
        IMPORTING
          OUTPUT = WA_DOC_GERADOS-FT_BELNR.

      "alrs
      MODIFY ZMMT_EE_ZGR_DOCS FROM  WA_DOC_GERADOS. " TRANSPORTING FT_BELNR FT_GJAHR.
      COMMIT WORK.

      CONCATENATE TEXT-001 VG_INVOICEDOCNUMBER_MIRO
                  TEXT-002 VG_ANO_MIRO
                  TEXT-003
                  INTO VG_MESSAGE
                  SEPARATED BY SPACE.

      CLEAR: VL_MESSAGE_V1,
             VL_MESSAGE_V2.

      VL_MESSAGE_V1 = VG_INVOICEDOCNUMBER_MIRO.
      VL_MESSAGE_V2 = VG_ANO_MIRO.

      WA_RETURN-TYPE    = C_S.
      WA_RETURN-ID      = C_MM.
      WA_RETURN-NUMBER  = C_899.
      WA_RETURN-MESSAGE = VG_MESSAGE.
      WA_RETURN-MESSAGE_V1 = VL_MESSAGE_V1.
      WA_RETURN-MESSAGE_V2 = VL_MESSAGE_V2.
      APPEND WA_RETURN TO IT_RETURN.
*  Cria mensagem de retorno em caso de sucesso
*  Retornando o docnum(fiscal) referente a MIRO criada quando existir..

      CLEAR WA_RETURN.

      SELECT SINGLE *
        INTO I_DOC
        FROM J_1BNFDOC
        WHERE BELNR = VG_INVOICEDOCNUMBER_MIRO
          AND GJAHR = VG_ANO_MIRO.

      IF SY-SUBRC EQ 0.

*        move-corresponding i_doc to p_notase.
*
*        call function 'ZPLANCOMP_REENVIO_NOTA'
*          exporting
*            p_docnum = i_doc-docnum
*            p_notase = p_notase.

        WA_DOC_GERADOS-DOCNUM = I_DOC-DOCNUM.

        IF WA_ZSDT0231 IS INITIAL.

          I_DOC-NTGEW  = WA_MOV_ESTQ-ENTRY_QNT.
          I_DOC-BRGEW  = WA_MOV_ESTQ-ENTRY_QNT.
          I_DOC-INCO1  = VG_INCO1.
          I_DOC-DOCREF = NUMBER_REF.
          MODIFY J_1BNFDOC FROM I_DOC.

          IF ( I_DOC-NFE EQ 'X' ) AND ( WA_MOV_ESTQ-AUTHCOD IS NOT INITIAL ).

            SELECT SINGLE * INTO I_ACTTAB FROM J_1BNFE_ACTIVE WHERE DOCNUM EQ I_DOC-DOCNUM.

            IF SY-SUBRC EQ 0.

              I_DOC-NFENUM     = WA_MOV_ESTQ-NFNUM.
              I_DOC-AUTHCOD    = WA_MOV_ESTQ-AUTHCOD.
              I_DOC-DOCSTAT    = WA_MOV_ESTQ-DOCSTAT.

              I_ACTTAB-NFNUM9  = WA_MOV_ESTQ-NFNUM.
              I_ACTTAB-AUTHCOD = WA_MOV_ESTQ-AUTHCOD.
              I_ACTTAB-DOCNUM9 = WA_MOV_ESTQ-NFENUM.
              I_ACTTAB-DOCSTA  = WA_MOV_ESTQ-DOCSTAT.
              I_ACTTAB-CDV     = WA_MOV_ESTQ-CDV.

              CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
                EXPORTING
                  I_ACTTAB  = I_ACTTAB
                  I_DOC     = I_DOC
                  I_UPDMODE = 'U'.
            ENDIF.

          ENDIF.

          COMMIT WORK.
        ENDIF.

*      Mensagem: O documento <nr_doc> para o exercício <ano>
*                foi criado com sucesso.
        CONCATENATE TEXT-001 I_DOC-DOCNUM
                    TEXT-002 VG_ANO_MIRO
                    TEXT-003
                    INTO VG_MESSAGE
                    SEPARATED BY SPACE.

        CLEAR: VL_MESSAGE_V1,
               VL_MESSAGE_V2.

        VL_MESSAGE_V1 = I_DOC-DOCNUM.
        VL_MESSAGE_V2 = VG_ANO_MIRO.

        WA_RETURN-TYPE    = C_S.
        WA_RETURN-ID      = C_MM.
        WA_RETURN-NUMBER  = C_899.
        WA_RETURN-MESSAGE = VG_MESSAGE.
        WA_RETURN-MESSAGE_V1 = VL_MESSAGE_V1.
        WA_RETURN-MESSAGE_V2 = VL_MESSAGE_V2.

        PERFORM Z_PREPARA_MENSAGEM2   USING VG_OBJ_KEY
                                         WA_RETURN-TYPE
                                         WA_RETURN-MESSAGE
                                         WA_RETURN-MESSAGE_V1
                                         WA_RETURN-MESSAGE_V2
                                         C_05.
      ENDIF.

      "Retorna Documento Contabil Sigam - US 107239 - WPP - Ini
      "PERFORM f_ret_doc_ctb_miro TABLES it_return
      "                            USING wa_doc_gerados.
      "Retorna Documento Contabil Sigam - US 107239 - WPP - Fim

    ENDIF.
  ENDIF.

  CLEAR IT_ITEMDATA.

  IF VG_ERRO IS INITIAL AND
     WA_MOV_ESTQ-TP_OPERACAO EQ C_02.
*    PERFORM z_bapi_trans_commit.
  ENDIF.

  IF VG_ERRO IS INITIAL AND
    WA_MOV_ESTQ-TP_OPERACAO EQ C_03.

*    PERFORM z_bapi_trans_commit.

*   Lançamento Contábil para Ajuste de CMV
    PERFORM Z_AJUSTE_CMV USING VG_INVOICEDOCNUMBER_MIRO.
  ENDIF.

  IF ( VG_ERRO IS INITIAL ) AND
     ( VL_ZERAR_BASE IS NOT INITIAL ) AND
     ( WA_MOV_ESTQ-COMP_CODE = '0100' ). "Argentina
    "Ajuste Base IRF Doc. Contábil.
    PERFORM Z_AJUSTE_BASE_IRF USING VG_INVOICEDOCNUMBER_MIRO VG_ANO_MIRO.
  ENDIF.

  "Identificador de interface MIRO
  VG_INTERFACE = C_12.
  LOOP AT IT_RETURN INTO WA_RETURN.
    PERFORM Z_PREPARA_MENSAGEM2   USING VG_OBJ_KEY
                                        WA_RETURN-TYPE
                                        WA_RETURN-MESSAGE
                                        WA_RETURN-MESSAGE_V1
                                        WA_RETURN-MESSAGE_V2
                                        VG_INTERFACE.
  ENDLOOP.
  " endif.
ENDFORM.                    " Z_BAPI_MIGO


*&---------------------------------------------------------------------*
*&      Form  Z_GERA_E_SOCIAL_E1250M
*&---------------------------------------------------------------------*
*       Evento E-1250 - E-Social.
*----------------------------------------------------------------------*
*      -->P_VG_INVOICEDOCNUMBER_MIRO  text
*      -->P_VG_ANO_MIRO  text
*----------------------------------------------------------------------*
*FORM Z_GERA_E_SOCIAL_E1250M  USING  P_VG_INVOICEDOCNUMBER_MIRO
*                                    P_VG_ANO_MIRO.
*
*  DATA: WA_ZHRST_EFD_E1250M TYPE ZHRST_EFD_E1250M,
*        WA_BKPF             TYPE BKPF,
*        WA_J_1BNFDOC        TYPE J_1BNFDOC,
*        VL_AWKEY            TYPE BKPF-AWKEY,
*        VL_NAME1            TYPE LFA1-NAME1,
*        VL_STCD1            TYPE LFA1-STCD1,
*        VL_STCD2            TYPE LFA1-STCD2,
*        VL_LINEID           TYPE SETLEAF-LINEID,
*        VL_PERCENTUAL       TYPE SETLINET-DESCRIPT.
*
*  CLEAR: WA_ZHRST_EFD_E1250M,
*         WA_BKPF,
*         VL_AWKEY.
*
*  CONCATENATE VG_INVOICEDOCNUMBER_MIRO
*              VG_ANO_MIRO
*         INTO VL_AWKEY.
*
*  SELECT SINGLE *
*    FROM BKPF
*    INTO WA_BKPF
*   WHERE AWKEY = VL_AWKEY
*     AND BLART = 'ZG'.
*
*  SELECT SINGLE *
*    FROM J_1BNFDOC
*    INTO WA_J_1BNFDOC
*   WHERE BELNR = VG_INVOICEDOCNUMBER_MIRO
*     AND GJAHR = VG_ANO_MIRO.
*
*  SELECT SINGLE NAME1 STCD1 STCD2
*    INTO (VL_NAME1, VL_STCD1, VL_STCD2)
*    FROM LFA1
*   WHERE LIFNR =  WA_J_1BNFDOC-PARID.
*
*  WA_ZHRST_EFD_E1250M-BUKRS     = WA_BKPF-BUKRS.
*  WA_ZHRST_EFD_E1250M-BRANCH    = WA_J_1BNFDOC-BRANCH.
*
*  IF VL_STCD2 IS INITIAL.
*    WA_ZHRST_EFD_E1250M-CPF     = VL_STCD1.
*  ELSE.
*    WA_ZHRST_EFD_E1250M-CPF     = VL_STCD2.
*  ENDIF.
*
*  WA_ZHRST_EFD_E1250M-BELNR     = WA_BKPF-BELNR.
*  WA_ZHRST_EFD_E1250M-GJAHR     = WA_BKPF-GJAHR.
*  WA_ZHRST_EFD_E1250M-CNAME     = VL_NAME1.
*  WA_ZHRST_EFD_E1250M-BUDAT     = WA_J_1BNFDOC-PSTDAT.
*  WA_ZHRST_EFD_E1250M-XBLNR     = WA_BKPF-XBLNR.
*  WA_ZHRST_EFD_E1250M-BLDAT     = WA_BKPF-BLDAT.
*  WA_ZHRST_EFD_E1250M-DMBTR     = WA_J_1BNFDOC-NFTOT.
*
*  PERFORM GET_PERCENTUAL USING  'MAGGI_EFD_E1250M' 'FU' CHANGING VL_PERCENTUAL.
*  WA_ZHRST_EFD_E1250M-VRCP      = WA_J_1BNFDOC-NFTOT * VL_PERCENTUAL / 100."Funrural
*
*  PERFORM GET_PERCENTUAL USING  'MAGGI_EFD_E1250M' 'RA' CHANGING VL_PERCENTUAL.
*  WA_ZHRST_EFD_E1250M-VRRAT     = WA_J_1BNFDOC-NFTOT * VL_PERCENTUAL / 100."Rat
*
*  PERFORM GET_PERCENTUAL USING  'MAGGI_EFD_E1250M' 'SE' CHANGING VL_PERCENTUAL.
*  WA_ZHRST_EFD_E1250M-VRSENAR   = WA_J_1BNFDOC-NFTOT * VL_PERCENTUAL / 100."Senar
*
*
*  WA_ZHRST_EFD_E1250M-ESTORNADO = SPACE.
*
*  MODIFY ZHRST_EFD_E1250M FROM WA_ZHRST_EFD_E1250M.
*
*ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNA_E_SOCIAL_E1250M
*&---------------------------------------------------------------------*
*       Evento E-1250 - E-Social.
*----------------------------------------------------------------------*
*      -->P_VG_INVOICEDOCNUMBER_MIRO  text
*      -->P_VG_ANO_MIRO  text
*----------------------------------------------------------------------*
*FORM Z_ESTORNA_E_SOCIAL_E1250M USING P_VG_INVOICEDOCNUMBER_MIRO
*                                     P_VG_ANO_MIRO.
*
*  DATA: WA_ZHRST_EFD_E1250M TYPE ZHRST_EFD_E1250M,
*        VL_BELNR            TYPE BKPF-BELNR,
*        VL_AWKEY            TYPE BKPF-AWKEY.
*
*  CLEAR: WA_ZHRST_EFD_E1250M,
*         VL_BELNR,
*         VL_AWKEY.
*
*  CONCATENATE P_VG_INVOICEDOCNUMBER_MIRO P_VG_ANO_MIRO INTO VL_AWKEY.
*
*  SELECT SINGLE BELNR
*    FROM BKPF
*    INTO VL_BELNR
*   WHERE AWKEY = VL_AWKEY
*     AND BLART = 'ZG'.
*
*  SELECT SINGLE *
*    FROM ZHRST_EFD_E1250M
*    INTO WA_ZHRST_EFD_E1250M
*   WHERE BELNR = VL_BELNR.
*
*  WA_ZHRST_EFD_E1250M-ESTORNADO = 'X'.
*
*  MODIFY ZHRST_EFD_E1250M FROM WA_ZHRST_EFD_E1250M.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_PERCENTUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_SET_NAME  text
*      -->P_VL_IMPOSTO  text
*----------------------------------------------------------------------*
FORM GET_PERCENTUAL  USING P_VL_SET_NAME TYPE SETLEAF-SETNAME
                           P_VL_IMPOSTO  TYPE SETLEAF-VALFROM
                  CHANGING P_VL_PERC     TYPE SETLINET-DESCRIPT .

  DATA : VL_LINEID TYPE SETLINET-LINEID .

  P_VL_PERC = 0.

  SELECT SINGLE LINEID"H1
    FROM SETLEAF
    INTO VL_LINEID
   WHERE SETNAME EQ P_VL_SET_NAME
     AND VALFROM EQ P_VL_IMPOSTO.

  IF SY-SUBRC IS INITIAL.

    SELECT SINGLE DESCRIPT
      FROM SETLINET
      INTO P_VL_PERC
     WHERE SETNAME EQ P_VL_SET_NAME
       AND LINEID  EQ VL_LINEID
       AND LANGU   EQ 'PT'.

  ENDIF.


ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  Z_CABEC_ITEM
*&---------------------------------------------------------------------*
*       Cabeçalho e Item de MIRO
*----------------------------------------------------------------------*
FORM Z_CABEC_ITEM .

  DATA: VL_GJAHR         TYPE EKBE-GJAHR,
        VL_BELNR2        TYPE EKBE-BELNR,
        VL_GJAHR2        TYPE EKBE-GJAHR,
        VL_WAERS         TYPE EKKO-WAERS,
        VL_PERCENTUAL_TX TYPE SETLINET-DESCRIPT,
        VL_PERCENTUAL    TYPE BPREI,
        VL_LINEID        TYPE SETLINE,
        VL_BSART         TYPE EKKO-BSART
        .

  DATA: WA_SETLEAF TYPE SETLEAF.
  CLEAR: WA_HEADERDATA,
         WA_ITEMDATA.
  IF IT_ITEM[] IS INITIAL.
***Dados Item
    "Busca dados do depósito
    READ TABLE IT_LGORT INTO WA_LGORT
    WITH KEY EBELN = WA_MOV_ESTQ-PO_NUMBER
             EBELP = WA_MOV_ESTQ-PO_ITEM.

    WA_ITEM-STGE_LOC     = WA_LGORT-LGORT.
    WA_ITEM-MOVE_TYPE    = WA_MOV_ESTQ-MOVE_TYPE.
    WA_ITEM-MVT_IND      = C_B.
    WA_ITEM-PO_ITEM      = WA_MOV_ESTQ-NU_ITEM.
    WA_ITEM-ENTRY_QNT    = WA_MOV_ESTQ-ENTRY_QNT.
    WA_ITEM-PO_NUMBER    = WA_MOV_ESTQ-PO_NUMBER.

    APPEND WA_ITEM TO IT_ITEM.
    CLEAR WA_ITEM.

    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_MOV_ESTQ-MATERIAL.
    IF WA_MOV_ESTQ-COMP_CODE = '0100'.

      "Tirar espaços a esquerda

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_MOV_ESTQ-TEXT1
        IMPORTING
          OUTPUT = WA_HEADERDATA-PAYMT_REF.

      CONDENSE WA_HEADERDATA-PAYMT_REF NO-GAPS.
    ELSE.
      CONCATENATE WA_LGORT-CHARG+0(4) '-' WA_MAKT-MAKTX INTO WA_HEADERDATA-PAYMT_REF SEPARATED BY SPACE.
    ENDIF.

  ENDIF.


  IF WA_MOV_ESTQ-TP_OPERACAO EQ C_03.
    WA_ITEMDATA-DE_CRE_IND   = C_X.

    READ TABLE IT_T001W
      INTO WA_T001W
      WITH KEY WERKS = WA_MOV_ESTQ-PLANT
      BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      WA_HEADERDATA-BUSINESS_PLACE = WA_T001W-J_1BBRANCH.
    ENDIF.
  ELSEIF WA_MOV_ESTQ-TP_OPERACAO EQ C_02 OR
         WA_MOV_ESTQ-TP_OPERACAO EQ C_04.

    CLEAR VL_GJAHR.

    IF WA_MOV_ESTQ-TP_OPERACAO EQ C_02.
      CLEAR WA_ITEMDATA-DE_CRE_IND.
    ELSE.
      WA_ITEMDATA-DE_CRE_IND = C_X.
    ENDIF.
  ENDIF.

  WA_HEADERDATA-BLINE_DATE = WA_MOV_ESTQ-PSTNG_DATE.
  IF WA_MOV_ESTQ-DT_VENCIMENTO IS NOT INITIAL.
    WA_HEADERDATA-BLINE_DATE = WA_MOV_ESTQ-DT_VENCIMENTO.
  ENDIF.

  IF WA_MOV_ESTQ-TP_OPERACAO EQ C_02 OR
     WA_MOV_ESTQ-TP_OPERACAO EQ C_04.
    CLEAR WA_HEADERDATA-INVOICE_IND.
  ELSE.
* Parâmetros de Entrada
    WA_HEADERDATA-INVOICE_IND    = C_X.
  ENDIF.

  WA_HEADERDATA-COMP_CODE      = WA_MOV_ESTQ-COMP_CODE.

  IF WA_MOV_ESTQ-BLART IS INITIAL .
    WA_HEADERDATA-DOC_TYPE       = C_ZG. "Fixo ZG
  ELSE.
    WA_HEADERDATA-DOC_TYPE       = WA_MOV_ESTQ-BLART.
  ENDIF.

  WA_HEADERDATA-DOC_DATE       = WA_MOV_ESTQ-DOC_DATE.
  WA_HEADERDATA-PSTNG_DATE     = WA_MOV_ESTQ-PSTNG_DATE.
  WA_HEADERDATA-GROSS_AMOUNT   = WA_MOV_ESTQ-VR_BRUTO.
  WA_HEADERDATA-DEL_COSTS_TAXC = WA_MOV_ESTQ-DEL_COSTS_TAXC.
  WA_HEADERDATA-PMNT_BLOCK     = WA_MOV_ESTQ-PMNT_BLOCK.
  WA_HEADERDATA-PYMT_METH      = WA_MOV_ESTQ-PYMT_METH.
  WA_HEADERDATA-HOUSEBANKID    = WA_MOV_ESTQ-SCBANK_IND.
  IF WA_HEADERDATA-COMP_CODE = '0101' .
    WA_HEADERDATA-BUS_AREA   = 'F101'.
  ELSEIF WA_MOV_ESTQ-BUS_AREA(1) = 'A' .
    WA_HEADERDATA-BUS_AREA   = 'T001'.
  ELSE.
    WA_HEADERDATA-BUS_AREA  = WA_MOV_ESTQ-BUS_AREA.
  ENDIF.

  SELECT SINGLE WAERS BSART
   FROM EKKO
   INTO (VL_WAERS, VL_BSART)
  WHERE EBELN = WA_MOV_ESTQ-PO_NUMBER.

  "Fatura da Argentina, a moeda da fatura deve ser sempre Peso(ARS)
  IF WA_MOV_ESTQ-COMP_CODE = '0100'.

    CLEAR WA_HEADERDATA-PARTNER_BK.

    IF WA_MOV_ESTQ-CFOP = '   /C' OR WA_MOV_ESTQ-CFOP = 'C'.
      "WA_HEADERDATA-DOC_TYPE = C_ZW.

      SELECT SINGLE BVTYP
        INTO WA_HEADERDATA-PARTNER_BK
        FROM LFBK
       WHERE LIFNR = WA_MOV_ESTQ-LIFNR
         AND BVTYP = 'C001'.  "Comissão vai cair por padrão nesse banco parceiro CS2023000319 - PSA

    ENDIF.

    VL_WAERS = 'ARS'.
  ENDIF.

  WA_HEADERDATA-CURRENCY       = VL_WAERS.

  IF WA_MOV_ESTQ-TP_OPERACAO EQ C_04.
    CLEAR WA_HEADERDATA-J_1BNFTYPE.
  ELSE.
    WA_HEADERDATA-J_1BNFTYPE     = WA_MOV_ESTQ-J_1BNFTYPE.
  ENDIF.

  SELECT SINGLE *
    FROM SETLEAF
    INTO WA_SETLEAF
   WHERE SETNAME = 'MAGGI_EMPRESA_EXTERIOR'
     AND VALFROM = WA_MOV_ESTQ-COMP_CODE.

  IF SY-SUBRC IS INITIAL.
    CLEAR: WA_HEADERDATA-J_1BNFTYPE, WA_HEADERDATA-DIFF_INV.
    IF WA_MOV_ESTQ-LIFNR IS NOT INITIAL AND WA_MOV_ESTQ-CFOP = 'C' .
      WA_HEADERDATA-DIFF_INV = WA_MOV_ESTQ-LIFNR.
    ENDIF.
  ENDIF.

  WA_HEADERDATA-ALLOC_NMBR     = WA_MOV_ESTQ-ALLOC_NMBR.
  WA_HEADERDATA-PMNTTRMS       = WA_MOV_ESTQ-PMNTTRMS.

  READ TABLE IT_J_1BAA WITH KEY NFTYPE = WA_MOV_ESTQ-J_1BNFTYPE INTO DATA(WA_J_1B11).

  IF NOT ( WA_J_1B11-FORM IS NOT INITIAL AND WA_MOV_ESTQ-ID_CARGA IS NOT INITIAL ).
    WA_HEADERDATA-REF_DOC_NO = WA_MOV_ESTQ-NT_REMESSA.
  ENDIF.

  CLEAR WA_HEADERDATA-HEADER_TXT.
  IF WA_MOV_ESTQ-TP_OPERACAO NE C_04.
    WA_HEADERDATA-HEADER_TXT = WA_MOV_ESTQ-HEADER_TXT.
    IF WA_MOV_ESTQ-COMP_CODE = '0100'.
      IF WA_MOV_ESTQ-CFOP = '   /C' OR WA_MOV_ESTQ-CFOP = 'C'.
        CONCATENATE WA_HEADERDATA-HEADER_TXT '-C' INTO WA_HEADERDATA-HEADER_TXT.
      ENDIF.
    ENDIF.
  ENDIF.

*  "se o tipo de de pedido = ZGF pega na tabela de percepções
*  if vl_bsart = 'ZGF'.
*
*    loop at IT_ZMMT_EEIVA_ZGR INTO wa_ZMMT_EEIVA_ZGR.
*
*      WA_ITEMDATA-INVOICE_DOC_ITEM = C_000001.
*      WA_ITEMDATA-PO_NUMBER        = WA_MOV_ESTQ-PO_NUMBER.
*      WA_ITEMDATA-PO_ITEM          = WA_MOV_ESTQ-PO_ITEM.
*      WA_ITEMDATA-QUANTITY         = WA_MOV_ESTQ-QUANTITY.
*      WA_ITEMDATA-PO_UNIT          = WA_MOV_ESTQ-MEINS.
*      WA_ITEMDATA-TAX_CODE         = wa_ZMMT_EEIVA_ZGR-MWSKZ.
*      WA_ITEMDATA-ITEM_AMOUNT      = wa_ZMMT_EEIVA_ZGR-VLR_IMP.
*
*      APPEND WA_ITEMDATA TO IT_ITEMDATA.
*
*      CLEAR: WA_ITEMDATA.
*
*    endloop.

  WA_ITEMDATA-INVOICE_DOC_ITEM = C_000001.
  WA_ITEMDATA-PO_NUMBER        = WA_MOV_ESTQ-PO_NUMBER.
  WA_ITEMDATA-PO_ITEM          = WA_MOV_ESTQ-PO_ITEM.
  WA_ITEMDATA-TAX_CODE         = WA_MOV_ESTQ-TAX_CODE.
  WA_ITEMDATA-QUANTITY         = WA_MOV_ESTQ-QUANTITY.
  WA_ITEMDATA-PO_UNIT          = WA_MOV_ESTQ-MEINS.

*  SELECT SINGLE BPRME
*    FROM EKPO
*    INTO @DATA(VBPRME)
*    WHERE EBELN = @WA_MOV_ESTQ-PO_NUMBER
*    AND   EBELP = @WA_MOV_ESTQ-PO_ITEM.
*
*  IF VBPRME NE WA_MOV_ESTQ-MEINS.
*    WA_ITEMDATA-PO_PR_UOM        = VBPRME.
*  ENDIF.

  CLEAR: VL_PERCENTUAL_TX,
         VL_LINEID,
         VL_WAERS.



  IF VL_BSART = 'ZGF'.
    WA_ITEMDATA-ITEM_AMOUNT      = WA_MOV_ESTQ-VR_BRUTO - VG_VR_PERCEPCOES.
    WA_MOV_ESTQ-VR_IMPOSTOS      = VG_VR_PERCEPCOES.
  ELSE.

    SELECT SINGLE LINEID
      FROM SETLEAF
      INTO VL_LINEID
     WHERE SETNAME EQ 'MAGGI_0100_IVA'
       AND VALFROM EQ WA_MOV_ESTQ-TAX_CODE.

    IF SY-SUBRC IS INITIAL.

      SELECT SINGLE DESCRIPT
      FROM SETLINET
      INTO VL_PERCENTUAL_TX
     WHERE SETNAME EQ 'MAGGI_0100_IVA'
       AND LINEID EQ VL_LINEID
       AND LANGU  EQ 'PT'.

      IF SY-SUBRC IS INITIAL.

        PERFORM CONVERTE_CHAR_DECIMAL USING VL_PERCENTUAL_TX CHANGING VL_PERCENTUAL.

        WA_ITEMDATA-ITEM_AMOUNT      = WA_MOV_ESTQ-VR_BRUTO - ( ( WA_MOV_ESTQ-VL_CMV * VL_PERCENTUAL ) / 100 ) - VG_VR_PERCEPCOES.
        WA_MOV_ESTQ-VR_IMPOSTOS      = ( WA_MOV_ESTQ-VL_CMV * VL_PERCENTUAL ) / 100.

        CLEAR VL_LINEID.
        IF WA_MOV_ESTQ-CD_AFIP IS NOT INITIAL.

          SELECT SINGLE LINEID"H2
            FROM SETLEAF
            INTO VL_LINEID
           WHERE SETNAME EQ 'MAGGI_0100_IVA'
             AND VALFROM EQ WA_MOV_ESTQ-CD_AFIP.

          IF SY-SUBRC IS INITIAL.
            SELECT SINGLE DESCRIPT
              FROM SETLINET
              INTO VL_PERCENTUAL_TX
             WHERE SETNAME EQ 'MAGGI_0100_IVA'
               AND LINEID EQ VL_LINEID
               AND LANGU  EQ 'PT'.
            IF SY-SUBRC IS INITIAL.
              CLEAR VL_PERCENTUAL.
              PERFORM CONVERTE_CHAR_DECIMAL USING VL_PERCENTUAL_TX CHANGING VL_PERCENTUAL.

              WA_ITEMDATA-ITEM_AMOUNT      =  WA_ITEMDATA-ITEM_AMOUNT - ( ( WA_MOV_ESTQ-VL_CMV * VL_PERCENTUAL ) / 100 )  - VG_VR_PERCEPCOES.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        WA_ITEMDATA-ITEM_AMOUNT      = WA_MOV_ESTQ-ITEM_AMOUNT - WA_MOV_ESTQ-VR_IMPOSTOS - VG_VR_PERCEPCOES .
      ENDIF.
    ELSE.
      WA_ITEMDATA-ITEM_AMOUNT      = WA_MOV_ESTQ-ITEM_AMOUNT - WA_MOV_ESTQ-VR_IMPOSTOS - VG_VR_PERCEPCOES .
    ENDIF.

    "WA_ITEMDATA-ITEM_AMOUNT = WA_MOV_ESTQ-ITEM_AMOUNT - WA_MOV_ESTQ-VR_IMPOSTOS - VG_VR_PERCEPCOES .
    IF WA_MOV_ESTQ-PMNTTRMS <> '' .
      SELECT SINGLE LINEID"H1
        FROM SETLEAF
        INTO VL_LINEID
       WHERE SETNAME EQ 'MAGGI_0100_IVA'
         AND VALFROM EQ WA_MOV_ESTQ-PMNTTRMS.

      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE DESCRIPT
          FROM SETLINET
          INTO VL_PERCENTUAL_TX
         WHERE SETNAME EQ 'MAGGI_0100_IVA'
           AND LINEID EQ VL_LINEID
           AND LANGU  EQ 'PT'.
        IF SY-SUBRC IS INITIAL.
          PERFORM CONVERTE_CHAR_DECIMAL USING VL_PERCENTUAL_TX
                                 CHANGING VL_PERCENTUAL.
          WA_ITEMDATA-ITEM_AMOUNT      = WA_ITEMDATA-ITEM_AMOUNT - ( ( WA_MOV_ESTQ-VL_CMV * VL_PERCENTUAL ) / 100 ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF."Fim ZGF

  PERFORM FATURA_ENTRADA_GRAOS IN PROGRAM ZMMR019_05
    USING VL_BSART WA_MOV_ESTQ WA_LGORT-CHARG CHANGING WA_ITEMDATA-ITEM_TEXT IF FOUND.

  " debito posterior argentina..
  IF WA_MOV_ESTQ-VL_CMV < 0 .
    WA_ITEMDATA-TAX_CODE         = 'C9'.
  ENDIF.

  "IF WA_MOV_ESTQ-TP_OPERACAO EQ C_02.
  "  WA_ITEMDATA-ITEM_AMOUNT = ABS( WA_ITEMDATA-ITEM_AMOUNT ) - WA_MOV_ESTQ-VR_IMPOSTOS.
  "ENDIF.

  ADD WA_MOV_ESTQ-ITEM_AMOUNT  TO VG_TOTAL_ITEM.
  APPEND WA_ITEMDATA TO IT_ITEMDATA.

  CLEAR: WA_ITEMDATA.




ENDFORM.                    " Z_CABEC_ITEM
*&---------------------------------------------------------------------*
*&      Form CONVERTE_CHAR_DECIMAL
*&---------------------------------------------------------------------*
*       Converte texto em numero
*----------------------------------------------------------------------*
FORM CONVERTE_CHAR_DECIMAL  USING  P_VALOR
                         CHANGING P_VALOR_DEC.

  DATA: LC_VALOR TYPE CHAR30.

  CLEAR P_VALOR_DEC.
  LC_VALOR = P_VALOR.
  REPLACE '.' WITH ',' INTO LC_VALOR.
*REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN LC_VALOR
*   *                          WITH ''.

  CALL FUNCTION 'OIU_ME_CHAR_TO_NUMBER'
    EXPORTING
      I_CHAR         = LC_VALOR
    IMPORTING
*     E_FLOAT        =
      E_PACKED       = P_VALOR_DEC
    EXCEPTIONS
      INVALID_NUMBER = 1
      OTHERS         = 2.

ENDFORM.                    " CONVERTE_CHAR_DECIMAL

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_DBC
*&---------------------------------------------------------------------*
*       Preenche tabela BDCData
*----------------------------------------------------------------------*
FORM Z_PREENCHE_DBC  USING  P_DYNBEGIN TYPE ANY
                            P_NAME     TYPE ANY
                            P_VALUE    TYPE ANY.


  IF P_DYNBEGIN = C_X.

    MOVE: P_NAME     TO WA_BDCDATA-PROGRAM,
          P_VALUE    TO WA_BDCDATA-DYNPRO,
          P_DYNBEGIN TO WA_BDCDATA-DYNBEGIN.

    APPEND WA_BDCDATA TO IT_BDCDATA.

  ELSE.

    MOVE: P_NAME     TO WA_BDCDATA-FNAM,
          P_VALUE    TO WA_BDCDATA-FVAL.

    APPEND WA_BDCDATA TO IT_BDCDATA.

  ENDIF.

  CLEAR: WA_BDCDATA.

ENDFORM.                    " Z_PREENCHE_DBC
*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*       Trata mensagens para serem enviadas para o legado
*----------------------------------------------------------------------*
FORM Z_PREPARA_MENSAGEM  USING    P_OBJ_KEY    TYPE ANY
                                  P_TYPE       TYPE ANY
                                  P_MESSAGE    TYPE ANY
                                  P_INTERFACE  TYPE ANY.

  DATA: WA_ZOB_MENSAGEM TYPE ZOB_MENSAGEM.

  WA_OUTRETURN-OBJ_KEY        = P_OBJ_KEY.
  WA_OUTRETURN-INTERFACE      = P_INTERFACE.
  WA_OUTRETURN-DT_ATUALIZACAO = SY-DATUM.
  WA_OUTRETURN-HR_ATUALIZACAO = SY-UZEIT.
  WA_OUTRETURN-TYPE           = P_TYPE.
  WA_OUTRETURN-ID             = C_MM.
  WA_OUTRETURN-NUM            = C_899.
  WA_OUTRETURN-MESSAGE        = P_MESSAGE.

  APPEND WA_OUTRETURN TO IT_OUTRETURN.

  MOVE-CORRESPONDING WA_OUTRETURN TO WA_ZOB_MENSAGEM.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZOB_MENSG'
    IMPORTING
      NUMBER                  = WA_ZOB_MENSAGEM-SEQ_REGISTRO
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.

  CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
    EXPORTING
      I_DATE   = SY-DATUM
      I_TIME   = SY-UZEIT
    IMPORTING
      E_TSTAMP = WA_ZOB_MENSAGEM-TIMESTAMP.

  APPEND WA_ZOB_MENSAGEM TO IT_ZOB_MENSAGEM.
  CLEAR WA_OUTRETURN.

ENDFORM.                    " Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNA_RECEBIMENTO
*&---------------------------------------------------------------------*
*       Estornar Recebimento
*----------------------------------------------------------------------*
FORM Z_ESTORNA_RECEBIMENTO USING WA_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS .
  DATA: VL_MODE TYPE C.

  PERFORM Z_PREENCHE_DBC USING:
    'X'  'SAPMV50A'           '4104',
 	 	' '  'BDC_OKCODE'	        '/00',
 	 	' '  'LIKP-VBELN'	        VG_RECEBIMENTO.

  PERFORM Z_PREENCHE_DBC USING:
    'X'  'SAPMV50A'	          '1000',
 	 	' '  'BDC_OKCODE'	        '/ELOES_T'.

* Carregar Transaçãlo
  FREE: IT_MSG.

  VL_MODE = C_N.

  CALL TRANSACTION C_VL32N
     USING IT_BDCDATA
     MODE   VL_MODE
     UPDATE C_S
     MESSAGES INTO IT_MSG.

  LOOP AT IT_MSG INTO WA_MSG.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        MSG_ID                 = WA_MSG-MSGID
        MSG_NO                 = WA_MSG-MSGNR
        MSG_VAR1               = WA_MSG-MSGV1(50)
        MSG_VAR2               = WA_MSG-MSGV2(50)
        MSG_VAR3               = WA_MSG-MSGV3(50)
        MSG_VAR4               = WA_MSG-MSGV4(50)
      IMPORTING
        MSG_TEXT               = VG_RETURN
      EXCEPTIONS
        FUNCTION_NOT_COMPLETED = 1
        MESSAGE_NOT_FOUND      = 2
        OTHERS                 = 3.

    IF SY-SUBRC <> 0.
* Vazio
    ENDIF.

    "Identificador de interface Aviso
    VG_INTERFACE = C_10.
* Finalidade - Retornar erros do Batch-Input p/ tab. de erro
    PERFORM Z_PREPARA_MENSAGEM   USING VG_OBJ_KEY
                                       WA_MSG-MSGTYP
                                       VG_RETURN
                                       VG_INTERFACE.

  ENDLOOP.

  REFRESH: IT_BDCDATA.

  IF WA_DOC_GERADOS-AV_VBELN IS NOT INITIAL.
    EXIT.
  ENDIF.

  "alrs
  CLEAR: WA_DOC_GERADOS-AV_VBELN.
  MODIFY ZMMT_EE_ZGR_DOCS FROM WA_DOC_GERADOS. "TRANSPORTING FT_BELNR FT_GJAHR.
  COMMIT WORK.

ENDFORM.                    " Z_ESTORNA_RECEBIMENTO
*&---------------------------------------------------------------------*
*&      Form  z_bapi_trans_commit
*&---------------------------------------------------------------------*
*       Executa o commit
*----------------------------------------------------------------------*
FORM Z_BAPI_TRANS_COMMIT .

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.

ENDFORM.                    " Z_BAPI_TRANS_COMMIT

FORM Z_BAPI_TRANS_ROLLBACK.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ENDFORM.                    " Z_BAPI_TRANS_COMMIT


*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_TABELA
*&---------------------------------------------------------------------*
*       Atualiza tabela
*----------------------------------------------------------------------*
FORM Z_ATUALIZA_TABELA  USING WA_MOV_ESTQ TYPE ZMMT_EE_ZGR.

  WA_MOV_ESTQ-ZRG_ATLZ = C_1.
  MODIFY ZMMT_EE_ZGR FROM WA_MOV_ESTQ.

ENDFORM.                    " Z_ATUALIZA_TABELA
*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_LOG_LEGADO
*&---------------------------------------------------------------------*
*       Envia log para o legado
*----------------------------------------------------------------------*
FORM Z_ENVIA_LOG_LEGADO .

  IF IT_ZOB_MENSAGEM[] IS NOT INITIAL.
    MODIFY ZOB_MENSAGEM FROM TABLE IT_ZOB_MENSAGEM.
  ENDIF.

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados
  IF NOT IT_OUTRETURN[] IS INITIAL.
    SORT IT_OUTRETURN BY OBJ_KEY INTERFACE.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = it_outreturn.

    DATA: LV_RFC TYPE RFCDEST.

    CONSTANTS: C_FM TYPE RS38L_FNAM VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        I_FM          = C_FM
      IMPORTING
        E_RFC         = LV_RFC
      EXCEPTIONS
        NO_RFC        = 1
        NO_RFC_CONFIG = 2
        OTHERS        = 3.

    IF SY-SUBRC EQ 0.
      CALL FUNCTION C_FM IN BACKGROUND TASK
        DESTINATION LV_RFC
        AS SEPARATE UNIT
        TABLES
          OUTRETURN = IT_OUTRETURN.
    ELSE.
      CALL FUNCTION C_FM IN BACKGROUND TASK
        TABLES
          OUTRETURN = IT_OUTRETURN.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
    COMMIT WORK.

  ENDIF.

  IF POBJKEY IS NOT INITIAL.
    READ TABLE IT_OUTRETURN WITH KEY TYPE = 'E' INTO DATA(WA_OUTRETURN).
    IF SY-SUBRC IS INITIAL.
      IF WA_OUTRETURN-MESSAGE IS NOT INITIAL.
        MESSAGE WA_OUTRETURN-MESSAGE TYPE 'E'.
      ELSE.
        MESSAGE ID WA_OUTRETURN-ID TYPE WA_OUTRETURN-TYPE NUMBER WA_OUTRETURN-NUM WITH WA_OUTRETURN-MESSAGE_V1 WA_OUTRETURN-MESSAGE_V2 WA_OUTRETURN-MESSAGE_V3 WA_OUTRETURN-MESSAGE_V4 .
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_ENVIA_LOG_LEGADO
*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNA_MIGO
*&---------------------------------------------------------------------*
*       Estornar MIGO
*----------------------------------------------------------------------*
FORM Z_ESTORNA_MIGO USING WA_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS
                    CHANGING P_ERRO.




ENDFORM.                    " Z_ESTORNA_MIGO

FORM Z_ESTORNA_MIRO USING WA_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS
                 CHANGING P_ERRO.

  DATA: VL_MSG_ESTORNO TYPE BAPIRET2-MESSAGE.

  DATA: LS_DOC_NEW    TYPE J_1BNFDOC,
        LS_ACTTAB_NEW TYPE J_1BNFE_ACTIVE.

  DATA: LWA_ZSDT0231 TYPE ZSDT0231.

  DATA: WA_INVOICEDOCNUMBER TYPE BAPI_INCINV_FLD,
        VL_PSTNG_DATE_EST   TYPE BAPI_INCINV_FLD-PSTNG_DATE,
        VG_NUMBER_MIRO_EST  TYPE BAPI_INCINV_FLD-INV_DOC_NO.

  CLEAR: WA_INVOICEDOCNUMBER, VL_PSTNG_DATE_EST, VG_NUMBER_MIRO_EST, LS_DOC_NEW, LS_ACTTAB_NEW,
         P_ERRO, LWA_ZSDT0231.

  CHECK ( WA_DOC_GERADOS-FT_BELNR IS NOT INITIAL ) AND
        ( WA_DOC_GERADOS-FT_GJAHR IS NOT INITIAL ).

  SELECT SINGLE * INTO LS_DOC_NEW
    FROM J_1BNFDOC
   WHERE BELNR = WA_DOC_GERADOS-FT_BELNR
     AND GJAHR = WA_DOC_GERADOS-FT_GJAHR.

  IF ( SY-SUBRC = 0 ) AND ( LS_DOC_NEW-NFE EQ 'X' ) AND ( WA_MOV_ESTQ-AUTHCOD IS NOT INITIAL ).

    SELECT SINGLE *
      FROM ZSDT0231 INTO LWA_ZSDT0231
     WHERE DOCNUM EQ LS_DOC_NEW-DOCNUM.

    IF SY-SUBRC NE 0.

      CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
        EXPORTING
          I_DOCNUM = LS_DOC_NEW-DOCNUM
        IMPORTING
          E_ACTTAB = LS_ACTTAB_NEW
        EXCEPTIONS
          NO_ENTRY = 1
          OTHERS   = 2.

      IF SY-SUBRC EQ 0.
        "Nota Fiscal
        MOVE '1'                   TO LS_DOC_NEW-DOCSTAT.
        "Active Nota Fiscal
        MOVE 'C'                   TO LS_ACTTAB_NEW-ACTION_REQU.
        MOVE '2'                   TO LS_ACTTAB_NEW-SCSSTA.
        MOVE 'X'                   TO LS_ACTTAB_NEW-CANCEL.
        MOVE 'B'                   TO LS_ACTTAB_NEW-MSSTAT.

        MODIFY J_1BNFDOC      FROM LS_DOC_NEW.
        MODIFY J_1BNFE_ACTIVE FROM LS_ACTTAB_NEW.
      ENDIF.

    ENDIF.

  ENDIF.

* Dados da Miro/Estorno
  WA_INVOICEDOCNUMBER-INV_DOC_NO = WA_DOC_GERADOS-FT_BELNR.
  WA_INVOICEDOCNUMBER-FISC_YEAR  = WA_DOC_GERADOS-FT_GJAHR.
  WA_INVOICEDOCNUMBER-REASON_REV = '01'.
  VL_PSTNG_DATE_EST              = WA_HEADER-PSTNG_DATE.

  CLEAR: WA_RETURN.
  FREE: IT_RETURN.

  "PERFORM Z_ESTORNA_E_SOCIAL_E1250M USING WA_INVOICEDOCNUMBER-INV_DOC_NO WA_INVOICEDOCNUMBER-FISC_YEAR.

*  IF WA_MOV_ESTQ-PO_NUMBER IS NOT INITIAL.
*    ZCL_PEDIDO_COMPRA=>VERIF_BLOQ_PEDIDO_WAIT( I_EBELN = WA_MOV_ESTQ-PO_NUMBER ).
*  ENDIF.

  DATA(CK_CONTINUAR) = ABAP_TRUE.

  WHILE CK_CONTINUAR EQ ABAP_TRUE.

*   BAPI Estorno da MIRO
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
      EXPORTING
        INVOICEDOCNUMBER          = WA_INVOICEDOCNUMBER-INV_DOC_NO
        FISCALYEAR                = WA_INVOICEDOCNUMBER-FISC_YEAR
        REASONREVERSAL            = WA_INVOICEDOCNUMBER-REASON_REV
        POSTINGDATE               = VL_PSTNG_DATE_EST
      IMPORTING
        INVOICEDOCNUMBER_REVERSAL = VG_NUMBER_MIRO_EST
      TABLES
        RETURN                    = IT_RETURN.

    READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = C_E.
    IF SY-SUBRC EQ 0.

      DATA(_BLOQ) = ABAP_FALSE.
      PERFORM F_CHECK_MSG_BLOQ TABLES IT_RETURN CHANGING _BLOQ WA_RETURN.
      IF _BLOQ EQ ABAP_FALSE.
        CK_CONTINUAR = ABAP_FALSE.
      ELSE.
        MESSAGE ID WA_RETURN-ID TYPE 'S' NUMBER WA_RETURN-NUMBER WITH WA_RETURN-MESSAGE_V1 WA_RETURN-MESSAGE_V2 WA_RETURN-MESSAGE_V3 WA_RETURN-MESSAGE_V4
        DISPLAY LIKE 'E'.

        WAIT UP TO 1 SECONDS.
        CLEAR: IT_RETURN[].
      ENDIF.

    ELSE.
      CK_CONTINUAR = ABAP_FALSE.
    ENDIF.

  ENDWHILE.

* BAPI TRANSACTION COMMIT
  PERFORM: Z_BAPI_TRANS_COMMIT.

  "Identificação da interface, MIRO
  VG_INTERFACE = C_12.
  LOOP AT IT_RETURN INTO WA_RETURN.
    CLEAR: VL_MSG_ESTORNO.
    CONCATENATE WA_RETURN-MESSAGE '(Estorno)'
           INTO VL_MSG_ESTORNO SEPARATED BY SPACE.

    PERFORM Z_PREPARA_MENSAGEM   USING VG_OBJ_KEY
                                       WA_RETURN-TYPE
                                       VL_MSG_ESTORNO
                                       VG_INTERFACE.
  ENDLOOP.

  READ TABLE IT_RETURN INTO WA_RETURN WITH KEY TYPE = C_E.
  IF SY-SUBRC EQ 0.
    P_ERRO = C_X.
  ELSE.

    CALL FUNCTION 'ZGRC_LIMPA_REF_MIRO_FISCAL'
      EXPORTING
        INVOICEDOCNUMBER = WA_DOC_GERADOS-FT_BELNR
        FISCALYEAR       = WA_DOC_GERADOS-FT_GJAHR.

    DELETE IT_OUTRETURN WHERE OBJ_KEY EQ WA_MOV_ESTQ-OBJ_KEY
                          AND TYPE    EQ C_S
                          AND MESSAGE_V1 = WA_DOC_GERADOS-FT_BELNR
                          AND MESSAGE_V2 = WA_DOC_GERADOS-FT_GJAHR.
    "alrs
    CLEAR: WA_DOC_GERADOS-FT_BELNR,
           WA_DOC_GERADOS-FT_GJAHR.
    MODIFY ZMMT_EE_ZGR_DOCS FROM WA_DOC_GERADOS. "TRANSPORTING FT_BELNR FT_GJAHR.
    COMMIT WORK.

    PERFORM TROCA_TEXTO_GRAOS_EST IN PROGRAM ZMMR019_06 USING WA_INVOICEDOCNUMBER VG_NUMBER_MIRO_EST IF FOUND.

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNA_MIGO
*&---------------------------------------------------------------------*
*       Estornar MIGO
*----------------------------------------------------------------------*
FORM Z_ESTORNO_PICKING USING P_VBELN TYPE VBELN
                    CHANGING P_ERRO.

  DATA: FP_BUDAT     TYPE SY-DATLO,
        FP_TCODE     TYPE SY-TCODE,
        FP_VBTYP     TYPE LIKP-VBTYP,
        IT_MESG      TYPE STANDARD TABLE OF MESG,
        WA_MESG      TYPE MESG,
        VL_AVISO_OUT TYPE C LENGTH 10,
        VL_AVISO_INP TYPE C LENGTH 10.

  FP_BUDAT = SY-DATLO.
  FP_TCODE = 'VL09'.
  FP_VBTYP = '7'.

  CLEAR: P_ERRO.

  CHECK P_VBELN IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = P_VBELN
    IMPORTING
      OUTPUT = VL_AVISO_INP.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = P_VBELN
    IMPORTING
      OUTPUT = VL_AVISO_OUT.

  CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
    EXPORTING
      I_VBELN                   = P_VBELN
      I_BUDAT                   = FP_BUDAT
      I_TCODE                   = FP_TCODE
      I_VBTYP                   = FP_VBTYP
    TABLES
      T_MESG                    = IT_MESG
    EXCEPTIONS
      ERROR_REVERSE_GOODS_ISSUE = 1
      OTHERS                    = 2.

  IF SY-SUBRC = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    DELETE IT_OUTRETURN WHERE OBJ_KEY    EQ WA_MOV_ESTQ-OBJ_KEY
                          AND TYPE       EQ C_S
                          AND ( MESSAGE_V1 EQ VL_AVISO_INP OR
                                MESSAGE_V1 EQ VL_AVISO_OUT  ).

  ELSE.
    P_ERRO = 'X'.
  ENDIF.


ENDFORM.                    " Z_ESTORNA_MIGO


*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM2
*&---------------------------------------------------------------------*
*       Trata mensagens para serem enviadas para o legado
*----------------------------------------------------------------------*
FORM Z_PREPARA_MENSAGEM2  USING   P_OBJ_KEY    TYPE ANY
                                  P_TYPE       TYPE ANY
                                  P_MESSAGE    TYPE ANY
                                  P_MESSAGE_V1 TYPE ANY
                                  P_MESSAGE_V2 TYPE ANY
                                  P_INTERFACE  TYPE ANY.

  DATA: WA_ZOB_MENSAGEM TYPE ZOB_MENSAGEM.

  WA_OUTRETURN-OBJ_KEY        = P_OBJ_KEY.
  WA_OUTRETURN-INTERFACE      = P_INTERFACE.
  WA_OUTRETURN-DT_ATUALIZACAO = SY-DATUM.
  WA_OUTRETURN-HR_ATUALIZACAO = SY-UZEIT.
  WA_OUTRETURN-TYPE           = P_TYPE.
  WA_OUTRETURN-ID             = C_MM.
  WA_OUTRETURN-NUM            = C_899.
  WA_OUTRETURN-MESSAGE        = P_MESSAGE.
  WA_OUTRETURN-MESSAGE_V1     = P_MESSAGE_V1.
  WA_OUTRETURN-MESSAGE_V2     = P_MESSAGE_V2.


  "Informações Adicionais - Ini
  CASE P_INTERFACE.
    WHEN C_12. "Miro
      PERFORM F_SET_INF_ADD_RET_MIRO CHANGING WA_OUTRETURN.
    WHEN C_11. "Migo
      PERFORM F_SET_INF_ADD_RET_MIGO CHANGING WA_OUTRETURN.
  ENDCASE.
  "Informações Adicionais - Fim



  APPEND WA_OUTRETURN TO IT_OUTRETURN.

  MOVE-CORRESPONDING WA_OUTRETURN TO WA_ZOB_MENSAGEM.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZOB_MENSG'
    IMPORTING
      NUMBER                  = WA_ZOB_MENSAGEM-SEQ_REGISTRO
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.

  CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
    EXPORTING
      I_DATE   = SY-DATUM
      I_TIME   = SY-UZEIT
    IMPORTING
      E_TSTAMP = WA_ZOB_MENSAGEM-TIMESTAMP.

  APPEND WA_ZOB_MENSAGEM TO IT_ZOB_MENSAGEM.
  CLEAR: WA_OUTRETURN.

ENDFORM.                    " Z_PREPARA_MENSAGEM2
*&---------------------------------------------------------------------*
*&      Form  Z_AJUSTE_CMV
*&---------------------------------------------------------------------*
*       Lançamento Contábil para Ajuste de CMV
*----------------------------------------------------------------------*
*      -->P_BELNR Doc Faturamento
*----------------------------------------------------------------------*
FORM Z_AJUSTE_CMV USING P_BELNR TYPE RE_BELNR.
  CLEAR WA_ZIB_CONTABIL.

  CONCATENATE WA_MOV_ESTQ-OBJ_KEY
              WA_MOV_ESTQ-PSTNG_DATE(4)
    INTO WA_ZIB_CONTABIL-OBJ_KEY.

  WA_ZIB_CONTABIL-SEQITEM       = C_000001.
  WA_ZIB_CONTABIL-BSCHL         = C_40.
  WA_ZIB_CONTABIL-GSBER         = WA_MOV_ESTQ-PLANT.
  WA_ZIB_CONTABIL-BUKRS         = WA_MOV_ESTQ-COMP_CODE.
  WA_ZIB_CONTABIL-INTERFACE     = C_03.
  WA_ZIB_CONTABIL-BKTXT         = P_BELNR.

  CONCATENATE WA_MOV_ESTQ-PSTNG_DATE+6(2)
              WA_MOV_ESTQ-PSTNG_DATE+4(2)
              WA_MOV_ESTQ-PSTNG_DATE(4)

    INTO WA_ZIB_CONTABIL-BLDAT SEPARATED BY '.'.

  WA_ZIB_CONTABIL-BUDAT         = WA_ZIB_CONTABIL-BLDAT.

  WA_ZIB_CONTABIL-GJAHR         = WA_MOV_ESTQ-PSTNG_DATE(4).
  WA_ZIB_CONTABIL-MONAT         = WA_MOV_ESTQ-PSTNG_DATE+4(2).
  WA_ZIB_CONTABIL-BLART         = C_SA.
  WA_ZIB_CONTABIL-HKONT         = WA_MOV_ESTQ-GL_DEBITO.
  WA_ZIB_CONTABIL-WRBTR         = ABS( WA_MOV_ESTQ-VL_CMV ).
  WA_ZIB_CONTABIL-WAERS         = C_BRL.
  WA_ZIB_CONTABIL-ZFBDT         = WA_ZIB_CONTABIL-BLDAT.
  WA_ZIB_CONTABIL-SGTXT         = TEXT-004.
  WA_ZIB_CONTABIL-BUPLA         = WA_T001W-J_1BBRANCH.
  WA_ZIB_CONTABIL-ZUONR         = WA_MOV_ESTQ-PO_NUMBER.
  WA_ZIB_CONTABIL-DMBE2         = WA_MOV_ESTQ-VL_CMV_LC.
  WA_ZIB_CONTABIL-WAERS_F       = C_USD.
  WA_ZIB_CONTABIL-RG_ATUALIZADO = C_N.
  WA_ZIB_CONTABIL-ZLSCH         = WA_MOV_ESTQ-PYMT_METH.

  APPEND WA_ZIB_CONTABIL
    TO IT_ZIB_CONTABIL.

  WA_ZIB_CONTABIL-BSCHL         = C_50.
  WA_ZIB_CONTABIL-SEQITEM       = WA_ZIB_CONTABIL-SEQITEM + 1.
  WA_ZIB_CONTABIL-HKONT         = WA_MOV_ESTQ-GL_CREDITO.

  APPEND WA_ZIB_CONTABIL
    TO IT_ZIB_CONTABIL.
ENDFORM.                    " Z_AJUSTE_CMV
*&---------------------------------------------------------------------*
*&      Form  Z_UPDATE_CONFIRMATION
*&---------------------------------------------------------------------*
*       Confirmações Pedido
*----------------------------------------------------------------------*
FORM Z_UPDATE_CONFIRMATION .
  DATA: "vl_data_em  TYPE dats,
    VL_TABIX     TYPE SY-TABIX,
    VL_EKES      TYPE I,
    IT_XEKES_AUX TYPE TABLE OF UEKES.

  CLEAR: IT_XEKES,
         IT_YEKES.

  "  CONCATENATE wa_mov_estq-pstng_date+6(2) wa_mov_estq-pstng_date+4(2)
  "              wa_mov_estq-pstng_date(4) INTO vl_data_em.

  CLEAR: WA_EKES.
*     recuperando o ultimo registro para o pedido de compra
*  LOOP AT it_ekes_srt INTO wa_ekes WHERE ebeln = wa_mov_estq-po_number.

*  ENDLOOP.
  READ TABLE IT_EKES_SRT
    INTO WA_EKES
    WITH KEY EBELN = WA_MOV_ESTQ-PO_NUMBER
    BINARY SEARCH.

*  IF NOT wa_ekes-etens IS INITIAL. "
  IF SY-SUBRC EQ 0.
    VL_TABIX = SY-TABIX.

    WA_EKES-ETENS = WA_EKES-ETENS + 1.
    WA_XEKES-ETENS = WA_EKES-ETENS.
    "    wa_xekes-etens = wa_ekes-etens.

*    INSERT wa_ekes
*      INTO TABLE it_ekes_srt.
    MODIFY IT_EKES_SRT
      FROM WA_EKES
      INDEX VL_TABIX.
  ELSE.
*    wa_ekes-ebeln = wa_mov_estq-po_number.
*    wa_ekes-ebelp = c_00010.
*    wa_ekes-etens = c_0001.
*
*    INSERT wa_ekes
*      INTO TABLE it_ekes_srt.

    SELECT SINGLE COUNT(*) INTO VL_EKES
      FROM EKES
      WHERE EBELN EQ WA_MOV_ESTQ-PO_NUMBER AND
            EBELP EQ C_00010.

    IF ( VL_EKES EQ 0 ).
      CLEAR: IT_XEKES_AUX.
      WA_XEKES-ETENS = C_0001. "wa_ekes-etens.
      WA_XEKES-EBELN = WA_MOV_ESTQ-PO_NUMBER.
      WA_XEKES-EBELP = C_00010.
      WA_XEKES-EBTYP = C_LA.
      WA_XEKES-ERDAT = WA_MOV_ESTQ-PSTNG_DATE.
      WA_XEKES-LPEIN = 1.
      WA_XEKES-KZDIS = C_X.
      WA_XEKES-KZ    = C_I.

      APPEND WA_XEKES
        TO IT_XEKES_AUX.

      CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
        EXPORTING
          I_EBELN = WA_MOV_ESTQ-PO_NUMBER
        TABLES
          XEKES   = IT_XEKES_AUX
          YEKES   = IT_YEKES.
    ENDIF.

    "    wa_xekes-etens = wa_ekes-etens.
  ENDIF.

*  wa_xekes-ebeln = wa_mov_estq-po_number.
*  wa_xekes-ebelp = c_00010.
*  wa_xekes-ebtyp = c_la.
*  wa_xekes-erdat = wa_mov_estq-pstng_date.
*  wa_xekes-lpein = 1.
*  wa_xekes-kzdis = c_x.
*  wa_xekes-kz    = c_i.
*
*  APPEND wa_xekes
*    TO it_xekes.
*
*  CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
*    EXPORTING
*      i_ebeln = wa_mov_estq-po_number
*    TABLES
*      xekes   = it_xekes
*      yekes   = it_yekes.

ENDFORM.                    " Z_UPDATE_CONFIRMATION
*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_NF
*&---------------------------------------------------------------------*
*       Escritura NF
*----------------------------------------------------------------------*
FORM Z_CRIA_NF.
  READ TABLE IT_T001W
    INTO WA_T001W
    WITH KEY WERKS = WA_MOV_ESTQ-PLANT
    BINARY SEARCH.

  WA_OBJ_HEADER-NFTYPE  = WA_MOV_ESTQ-J_1BNFTYPE.
  WA_OBJ_HEADER-DOCTYP  = 1.
  WA_OBJ_HEADER-DIRECT  = 2.
  WA_OBJ_HEADER-DOCSTAT = 1.
  WA_OBJ_HEADER-DOCDAT  = WA_MOV_ESTQ-DOC_DATE.
  WA_OBJ_HEADER-PSTDAT  = WA_MOV_ESTQ-PSTNG_DATE.
  WA_OBJ_HEADER-CREDAT  = WA_MOV_ESTQ-DOC_DATE.
  WA_OBJ_HEADER-MANUAL  = C_X.
  WA_OBJ_HEADER-WAERK   = C_BRL.
  WA_OBJ_HEADER-BUKRS   = WA_MOV_ESTQ-COMP_CODE.
  WA_OBJ_HEADER-BRANCH  = WA_T001W-J_1BBRANCH.
  WA_OBJ_HEADER-PARVW   = C_LF.
  WA_OBJ_HEADER-PARID   = WA_MOV_ESTQ-LIFNR.
  WA_OBJ_HEADER-NFE     = C_X.
  WA_OBJ_HEADER-FORM    = C_NF55.
  WA_OBJ_HEADER-MODEL   = 55.
  WA_OBJ_HEADER-NFENUM  = WA_MOV_ESTQ-NT_REMESSA.

  WA_OBJ_PARTNER-PARVW  = C_LF.
  WA_OBJ_PARTNER-PARID  = WA_MOV_ESTQ-LIFNR.
  WA_OBJ_PARTNER-PARTYP = C_V.

  APPEND WA_OBJ_PARTNER TO IT_OBJ_PARTNER.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      INPUT        = WA_MOV_ESTQ-MATERIAL
    IMPORTING
      OUTPUT       = WA_MOV_ESTQ-MATERIAL
    EXCEPTIONS
      LENGTH_ERROR = 1
      OTHERS       = 2.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'J_1B_MATERIAL_READ'
    EXPORTING
      MATNR                = WA_MOV_ESTQ-MATERIAL
      VAL_AREA             = WA_T001W-BWKEY
      VAL_TYPE             = SPACE
      LANGUAGE             = SY-LANGU
      I_WERKS              = WA_T001W-WERKS
    IMPORTING
      NBM                  = WA_OBJ_ITEM-NBM
      MATUSE               = WA_OBJ_ITEM-MATUSE
      MATORG               = WA_OBJ_ITEM-MATORG
      MATERIAL_TEXT_RECORD = WA_MATERIAL_TEXT_RECORD
      E_MATKL              = WA_OBJ_ITEM-MATKL
    EXCEPTIONS
      MATERIAL_NOT_FOUND   = 1
      VALUATION_NOT_FOUND  = 2
      OTHERS               = 3.

  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  WA_OBJ_ITEM-ITMNUM  = 10.

* > 05/07/2023 - Migração S4 - LM
*  wa_obj_item-matnr   = wa_mov_estq-material.

  IF STRLEN( WA_MOV_ESTQ-MATERIAL ) > 18.
    WA_OBJ_ITEM-MATNR_LONG = WA_MOV_ESTQ-MATERIAL.
  ELSE.
    WA_OBJ_ITEM-MATNR      = WA_MOV_ESTQ-MATERIAL.
  ENDIF.
* > 05/07/2023 - Migração S4 - LM

  WA_OBJ_ITEM-BWKEY   = WA_T001W-BWKEY.
  WA_OBJ_ITEM-MAKTX   = WA_MATERIAL_TEXT_RECORD-MAKTX.
  WA_OBJ_ITEM-MENGE   = WA_MOV_ESTQ-ENTRY_QNT.
  WA_OBJ_ITEM-MEINS   = WA_MOV_ESTQ-MEINS.
  WA_OBJ_ITEM-NETPR   = WA_MOV_ESTQ-GROSS_AMOUNT.
  WA_OBJ_ITEM-TAXLW1  = WA_MOV_ESTQ-TAXLW1.
  WA_OBJ_ITEM-TAXLW2  = WA_MOV_ESTQ-TAXLW2.
  WA_OBJ_ITEM-ITMTYP  = 1.
  WA_OBJ_ITEM-WERKS   = WA_T001W-WERKS.
  WA_OBJ_ITEM-CFOP_10 = WA_MOV_ESTQ-CFOP.
  WA_OBJ_ITEM-TAXLW4  = WA_MOV_ESTQ-TAXLW4.
  WA_OBJ_ITEM-TAXLW5  = WA_MOV_ESTQ-TAXLW5.

  APPEND WA_OBJ_ITEM
    TO IT_OBJ_ITEM.

  IF NOT WA_MOV_ESTQ-TAXTYP_ICMS IS INITIAL.
    WA_OBJ_ITEM_TAX-ITMNUM = 10.
    WA_OBJ_ITEM_TAX-TAXTYP = WA_MOV_ESTQ-TAXTYP_ICMS.
    WA_OBJ_ITEM_TAX-OTHBAS = WA_MOV_ESTQ-GROSS_AMOUNT.

    APPEND WA_OBJ_ITEM_TAX
      TO IT_OBJ_ITEM_TAX.
  ENDIF.

  IF NOT WA_MOV_ESTQ-TAXTYP_IPI IS INITIAL.
    WA_OBJ_ITEM_TAX-ITMNUM = 10.
    WA_OBJ_ITEM_TAX-TAXTYP = WA_MOV_ESTQ-TAXTYP_IPI.
    WA_OBJ_ITEM_TAX-OTHBAS = WA_MOV_ESTQ-GROSS_AMOUNT.

    APPEND WA_OBJ_ITEM_TAX
      TO IT_OBJ_ITEM_TAX.
  ENDIF.

  IF NOT WA_MOV_ESTQ-TAXTYP_PIS IS INITIAL.
    WA_OBJ_ITEM_TAX-ITMNUM = 10.
    WA_OBJ_ITEM_TAX-TAXTYP = WA_MOV_ESTQ-TAXTYP_PIS.
    WA_OBJ_ITEM_TAX-OTHBAS = WA_MOV_ESTQ-GROSS_AMOUNT.

    APPEND WA_OBJ_ITEM_TAX
      TO IT_OBJ_ITEM_TAX.
  ENDIF.

  IF NOT WA_MOV_ESTQ-TAXTYP_COFINS IS INITIAL.
    WA_OBJ_ITEM_TAX-ITMNUM = 10.
    WA_OBJ_ITEM_TAX-TAXTYP = WA_MOV_ESTQ-TAXTYP_COFINS.
    WA_OBJ_ITEM_TAX-OTHBAS = WA_MOV_ESTQ-GROSS_AMOUNT.

    APPEND WA_OBJ_ITEM_TAX
      TO IT_OBJ_ITEM_TAX.
  ENDIF.

  WA_NFCHECK-CHEKCON = C_X.

  CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      OBJ_HEADER     = WA_OBJ_HEADER
      NFCHECK        = WA_NFCHECK
    IMPORTING
      E_DOCNUM       = VG_DOCNUM
    TABLES
      OBJ_PARTNER    = IT_OBJ_PARTNER
      OBJ_ITEM       = IT_OBJ_ITEM
      OBJ_ITEM_TAX   = IT_OBJ_ITEM_TAX
      OBJ_OT_PARTNER = IT_OBJ_OT_PARTNER
      RETURN         = IT_RETURN.

  IF NOT VG_DOCNUM IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ENDIF.
ENDFORM.                    " Z_CRIA_NF
*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_CONFIRMACAO_PEDIDOS
*&---------------------------------------------------------------------*

FORM Z_ATUALIZA_CONFIRMACAO_PEDIDOS .

  DATA: VL_EBELN    TYPE TY_EKES-EBELN,
        VL_ETENS    TYPE TY_EKES-ETENS,
        WA_EKES_AUX TYPE TY_EKES.

  CLEAR: IT_XEKES,
         IT_YEKES.

  IF NOT IT_EKES_SRT IS INITIAL.
    SELECT EBELN EBELP ETENS
      FROM EKES
      INTO TABLE IT_EKES
      FOR ALL ENTRIES IN IT_EKES_SRT
      WHERE EBELN EQ IT_EKES_SRT-EBELN AND
            EBELP EQ C_00010.

    IF SY-SUBRC EQ 0.
      " Recuperando apenas a ultima confirmacao de cada pedido alterado pela interface.
      SORT IT_EKES BY EBELN ASCENDING
                      ETENS DESCENDING.
      DELETE ADJACENT DUPLICATES
        FROM IT_EKES COMPARING EBELN.
    ENDIF.
  ENDIF.
  CLEAR: VL_EBELN,
         VL_ETENS.

  LOOP AT IT_EKES_SRT INTO WA_EKES.
    IF VL_EBELN IS INITIAL OR VL_EBELN <> WA_EKES-EBELN.
      VL_EBELN = WA_EKES-EBELN.
      " Recuperando a ultima confirmacao para o pedido
      READ TABLE IT_EKES
          INTO WA_EKES_AUX
        WITH KEY EBELN = WA_EKES-EBELN
        BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        WA_EKES_AUX-ETENS = WA_EKES_AUX-ETENS + 1.
        VL_ETENS = WA_EKES_AUX-ETENS.
      ELSE.
        VL_ETENS = C_0001.
      ENDIF.
    ELSE.
      VL_ETENS = VL_ETENS + 1.
    ENDIF.
    WA_XEKES-ETENS = VL_ETENS.
    WA_XEKES-EBELN = WA_EKES-EBELN.
    WA_XEKES-EBELP = C_00010.
    WA_XEKES-EBTYP = C_LA.
    WA_XEKES-ERDAT = SY-DATUM."wa_mov_estq-pstng_date.
    WA_XEKES-LPEIN = 1.
    WA_XEKES-KZDIS = C_X.
    WA_XEKES-KZ    = C_I.

    APPEND WA_XEKES
      TO IT_XEKES.
  ENDLOOP.

  CALL FUNCTION 'ME_CONFIRMATION_UPDATE'
    EXPORTING
      I_EBELN = WA_MOV_ESTQ-PO_NUMBER
    TABLES
      XEKES   = IT_XEKES
      YEKES   = IT_YEKES.
  COMMIT WORK.

ENDFORM.                    " Z_ATUALIZA_CONFIRMACAO_PEDIDOS'

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_IMPOSTOS_RETIDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MOV_ESTQ  text
*----------------------------------------------------------------------*
FORM VERIFICA_IMPOSTOS_RETIDOS
  USING  P_WA_MOV_ESTQ TYPE ZMMT_EE_ZGR
         P_VALIDADO    TYPE SY-SUBRC.

  DATA: VG_FORNE           TYPE ELIFN,
        WA_LFA1            TYPE LFA1,
        VG_REGIO           TYPE REGIO,
        IT_LFBW            TYPE TABLE OF LFBW WITH HEADER LINE,
        WA_RETURN          TYPE BAPIRET2,
        IT_ZMMT_EE_ZGR_IMP TYPE TABLE OF ZMMT_EE_ZGR_IMP WITH HEADER LINE.

  P_VALIDADO = 0.

  IF ( P_WA_MOV_ESTQ-TP_OPERACAO NE C_08 ) AND ( NOT P_WA_MOV_ESTQ-PO_NUMBER IS INITIAL ) AND ( NOT P_WA_MOV_ESTQ-COMP_CODE IS INITIAL ).

    "Busca fornecedor para buscar o tax code
    SELECT SINGLE LIFNR
      INTO VG_FORNE
      FROM EKKO
     WHERE EBELN = P_WA_MOV_ESTQ-PO_NUMBER.

    CHECK SY-SUBRC IS INITIAL.

    "Busca Dados do Fornecedor
    SELECT SINGLE * INTO WA_LFA1
      FROM LFA1
     WHERE LIFNR EQ VG_FORNE.

    CHECK SY-SUBRC IS INITIAL.

    "Configurações Atuais
    SELECT * INTO TABLE IT_LFBW
      FROM LFBW
      WHERE LIFNR = VG_FORNE
        AND BUKRS = P_WA_MOV_ESTQ-COMP_CODE.

    "Configurações deve possuir - Impostos Nacionais
    SELECT * INTO TABLE IT_ZMMT_EE_ZGR_IMP
      FROM ZMMT_EE_ZGR_IMP
     WHERE P_FISICA EQ WA_LFA1-STKZN
       AND LAND1    EQ WA_LFA1-LAND1
       AND REGIO    EQ VG_REGIO.

    IF WA_LFA1-REGIO IS INITIAL.
      "Configurações deve possuir - Impostos Estaduais
      SELECT * APPENDING TABLE IT_ZMMT_EE_ZGR_IMP
        FROM ZMMT_EE_ZGR_IMP
       WHERE P_FISICA EQ WA_LFA1-STKZN
         AND LAND1    EQ WA_LFA1-LAND1
         AND REGIO    EQ WA_LFA1-REGIO.
    ENDIF.

    LOOP AT IT_ZMMT_EE_ZGR_IMP.
      READ TABLE IT_LFBW WITH KEY WITHT = IT_ZMMT_EE_ZGR_IMP-WITHT.
      IF NOT SY-SUBRC IS  INITIAL.
        "Imposto não parametrizado.
        P_VALIDADO = 1.
        CONCATENATE 'Imposto Retido' IT_ZMMT_EE_ZGR_IMP-WITHT 'não configurado em fornecedor' VG_FORNE INTO VG_RETURN SEPARATED BY SPACE.
        PERFORM Z_PREPARA_MENSAGEM2 USING VG_OBJ_KEY
                                          C_E
                                          VG_RETURN
                                          IT_ZMMT_EE_ZGR_IMP-WITHT
                                          VG_FORNE
                                          C_11.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " VERIFICA_IMPOSTOS_RETIDOS

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_FORNECEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VERIFICA_FORNECEDOR  USING P_WA_MOV_ESTQ TYPE ZMMT_EE_ZGR
                                P_VALIDADO    TYPE SY-SUBRC.


  DATA: VG_FORNE TYPE ELIFN.

  P_VALIDADO = 0.

  IF NOT P_WA_MOV_ESTQ-PO_NUMBER IS INITIAL.

    "Busca fornecedor para buscar o tax code
    SELECT SINGLE LIFNR
      INTO VG_FORNE
      FROM EKKO
     WHERE EBELN = P_WA_MOV_ESTQ-PO_NUMBER.

    CHECK ( SY-SUBRC IS INITIAL ) AND ( NOT VG_FORNE IS INITIAL ).

    CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
      EXPORTING
        P_KOART      = 'K'
        P_EMPRESA    = P_WA_MOV_ESTQ-COMP_CODE
        P_FORNECEDOR = VG_FORNE
      EXCEPTIONS
        ERROR        = 1
        BRANCH       = 2
        OTHERS       = 3.

    IF NOT SY-SUBRC IS INITIAL.
      P_VALIDADO = 1.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO VG_RETURN.
      PERFORM Z_PREPARA_MENSAGEM2 USING VG_OBJ_KEY
                                        C_E
                                        VG_RETURN
                                        VG_FORNE
                                        SPACE
                                        C_11.
    ENDIF.


  ENDIF.

ENDFORM.                    " VERIFICA_FORNECEDOR

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_REFERENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MOV_ESTQ  text
*----------------------------------------------------------------------*
FORM VERIFICA_REFERENCIA USING P_MOV_ESTQ TYPE ZMMT_EE_ZGR
                               DOC_NUMBER TYPE J_1BDOCNUM
                               VG_MSG     TYPE CLIKE.

  DATA: WA_J_1BAA  TYPE J_1BAA,
        VG_NUMERO  TYPE XBLNR,
        NF_NUMBER  LIKE J_1BNFDOC-NFNUM,
        SERIES     LIKE J_1BNFDOC-SERIES,
        SUBSERIES  LIKE J_1BNFDOC-SUBSER,
        NF_NUMBER9 TYPE J_1BNFDOC-NFENUM,
        PARTNER_ID LIKE  J_1BNFDOC-PARID.

  SELECT SINGLE * INTO WA_J_1BAA
    FROM J_1BAA
   WHERE NFTYPE EQ P_MOV_ESTQ-J_1BNFTYPE.

  SELECT SINGLE LIFNR INTO PARTNER_ID
    FROM EKKO
   WHERE EBELN = WA_MOV_ESTQ-PO_NUMBER.

  MOVE P_MOV_ESTQ-HEADER_TXT TO VG_NUMERO.

  CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
    EXPORTING
      REF_NUMBER   = VG_NUMERO
      I_NFEFLAG    = WA_J_1BAA-NFE
    IMPORTING
      NF_NUMBER    = NF_NUMBER
      SERIES       = SERIES
      NF_NUMBER9   = NF_NUMBER9
    EXCEPTIONS
      NUMBER_ERROR = 1
      OTHERS       = 2.

  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO VG_MSG.
  ENDIF.

  CALL FUNCTION 'J_1B_NF_DOCUMENT_SELECT_2'
    EXPORTING
      COMPANY                  = P_MOV_ESTQ-COMP_CODE
      BRANCH                   = P_MOV_ESTQ-PLANT
      NF_NUMBER                = NF_NUMBER
      SERIES                   = SERIES
      SUBSERIES                = SUBSERIES
      PARTNER_ID               = PARTNER_ID
      PARTNER_TYPE             = 'V'
      I_NFEFLAG                = WA_J_1BAA-NFE
      I_NFNUM9                 = NF_NUMBER9
    IMPORTING
      DOC_NUMBER               = DOC_NUMBER
    EXCEPTIONS
      DOCUMENT_NOT_FOUND       = 1
      DOC_WITH_SAME_YEAR_FOUND = 2
      DOC_WITH_DIFF_YEAR_FOUND = 3
      TOO_MANY_DOCUMENTS_FOUND = 4
      OTHERS                   = 5.

  IF NOT SY-SUBRC IS INITIAL.
    CASE WA_J_1BAA-NFE.
      WHEN 'X'.
        CONCATENATE 'Doc. Compl. Nr.:' NF_NUMBER9
                    'Série:' SERIES
                    'Empresa:' P_MOV_ESTQ-COMP_CODE
                    'Filial:'  P_MOV_ESTQ-PLANT
                    'Não Encontrado!'
               INTO VG_MSG SEPARATED BY SPACE.
      WHEN SPACE.
        CONCATENATE 'Doc. Compl. Nr.:' NF_NUMBER
                    'Série:' SERIES
                    'Empresa:' P_MOV_ESTQ-COMP_CODE
                    'Filial:'  P_MOV_ESTQ-PLANT
                    'Não Encontrado!'
               INTO VG_MSG SEPARATED BY SPACE.
    ENDCASE.
  ENDIF.

ENDFORM.                    " VERIFICA_REFERENCIA
*&---------------------------------------------------------------------*
*&      Form  Z_AJUSTE_BASE_CTB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_INVOICEDOCNUMBER_MIRO  text
*----------------------------------------------------------------------*
FORM Z_AJUSTE_BASE_IRF  USING P_BELNR TYPE RE_BELNR
                              P_GJAHR	TYPE GJAHR.


  DATA: DOC_CREATED  TYPE C,
        LV_SUMME     LIKE BSEG-WRBTR,
        UPDATE_QSSKZ TYPE BOOLE.

  DATA: I_BSEG TYPE BSEG OCCURS 1,
        I_BKPF TYPE BKPF OCCURS 1,
        I_BSEC TYPE BSEC OCCURS 1,       " int. table for bsec
        I_BSED TYPE BSED OCCURS 1,       " int. table for bsed
        I_BSET TYPE BSET OCCURS 1,       " int. table for bset
        I_BKDF TYPE BKDF OCCURS 1.       " int. table for bkdf

  DATA: WA_BSEG   LIKE BSEG,
        WA_BSIK   LIKE BSIK,               " work area open vendor items
        WA_BSID   LIKE BSID,               " work area open customer items
        WA_VBSEGK LIKE VBSEGK,             " work area parked vendor items
        WA_VBSEGD LIKE VBSEGD,             " work area parked customer items
        WAR_BSEG  LIKE BSEG,               " work area update bseg-qsskz
        WA_BKPF   LIKE BKPF.               " work area for bkpf

  DATA: H_BSEG            TYPE BSEG OCCURS 1 WITH HEADER LINE,
        H_BSET            TYPE BSET OCCURS 1 WITH HEADER LINE,
        ADDED_WITH_ITEM   LIKE WITH_ITEM OCCURS 1 WITH HEADER LINE,
        CHANGED_WITH_ITEM LIKE WITH_ITEM OCCURS 1 WITH HEADER LINE,
        A_X_WITH_ITEM     LIKE ACCIT_WT  OCCURS 1 WITH HEADER LINE,
        I_X_WITH_ITEM     LIKE WITH_ITEM OCCURS 1 WITH HEADER LINE.

  DATA: TG_BKPF  TYPE TABLE OF BKPF WITH HEADER LINE,
        TG_BSIK  TYPE TABLE OF BSIK WITH HEADER LINE,
        TG_LFB1  TYPE TABLE OF LFB1 WITH HEADER LINE,
        VL_AWKEY TYPE BKPF-AWKEY.

  CLEAR:   CHANGED_WITH_ITEM, A_X_WITH_ITEM, I_X_WITH_ITEM, VL_AWKEY.
  REFRESH: CHANGED_WITH_ITEM, A_X_WITH_ITEM, I_X_WITH_ITEM,
           TG_BKPF, TG_BSIK, TG_LFB1, I_BSEG.

  CHECK ( P_BELNR IS NOT INITIAL ) AND ( P_GJAHR IS NOT INITIAL ).

  CONCATENATE P_BELNR P_GJAHR INTO VL_AWKEY.

  SELECT *
    FROM BKPF INTO TABLE TG_BKPF
   WHERE AWKEY EQ VL_AWKEY
     AND BLART EQ 'ZG'.

  CHECK TG_BKPF[] IS NOT INITIAL.
  READ TABLE TG_BKPF INDEX 1.

  CHECK ( SY-SUBRC = 0 ) AND ( TG_BKPF IS NOT INITIAL ).

  SELECT *
    FROM BSIK INTO TABLE TG_BSIK
   WHERE BUKRS EQ TG_BKPF-BUKRS
     AND BELNR EQ TG_BKPF-BELNR
     AND GJAHR EQ TG_BKPF-GJAHR.

  CHECK TG_BSIK[] IS NOT INITIAL.

  SELECT *
    FROM LFB1 INTO TABLE TG_LFB1
    FOR ALL ENTRIES IN TG_BSIK
   WHERE BUKRS = TG_BSIK-BUKRS
     AND LIFNR = TG_BSIK-LIFNR.

  SORT TG_LFB1 BY LIFNR.
  DELETE ADJACENT DUPLICATES FROM TG_LFB1 COMPARING LIFNR.

  CHECK TG_LFB1[] IS NOT INITIAL.

  LOOP AT TG_LFB1.

    REFRESH: A_X_WITH_ITEM.

    "determine relevant w/tax types with posting time payment
    CALL FUNCTION 'FI_WT_DETERM_RELEVANT_TYPES'
      EXPORTING
        I_BUKRS          = TG_BKPF-BUKRS
        I_ACCT           = TG_LFB1-LIFNR
        I_KOART          = 'K'
        I_BUDAT          = SY-DATUM
        I_POSTINGTIME    = '2'
      TABLES
        T_WITH           = A_X_WITH_ITEM
      EXCEPTIONS
        NOTHING_SELECTED = 1
        OTHERS           = 2.

    CHECK ( SY-SUBRC = 0 ).

    LOOP AT TG_BSIK WHERE LIFNR = TG_LFB1-LIFNR.

      CLEAR: UPDATE_QSSKZ.
      REFRESH: I_X_WITH_ITEM.

      "Read wt info for open item
      CALL FUNCTION 'FI_WT_READ_WT_INFO'
        EXPORTING
          I_BUKRS     = TG_BSIK-BUKRS
          I_BELNR     = TG_BSIK-BELNR
          I_GJAHR     = TG_BSIK-GJAHR
          I_BUZEI     = TG_BSIK-BUZEI
        TABLES
          T_WITH_ITEM = I_X_WITH_ITEM
        EXCEPTIONS
          NOT_FOUND   = 1
          OTHERS      = 2.

      CHECK SY-SUBRC = 0 OR SY-SUBRC = 1.

      IF SY-SUBRC = 1.
        UPDATE_QSSKZ = 'X'.
      ENDIF.

      IF I_X_WITH_ITEM[] IS NOT INITIAL.

        LOOP AT I_X_WITH_ITEM.
          READ TABLE A_X_WITH_ITEM WITH KEY WITHT = I_X_WITH_ITEM-WITHT.
          IF SY-SUBRC = 0.
            IF A_X_WITH_ITEM-WT_WITHCD IS INITIAL.
              I_X_WITH_ITEM-WT_WITHCD = A_X_WITH_ITEM-WT_WITHCD.
            ENDIF.

            CLEAR: I_X_WITH_ITEM-WT_QSSHB,
                   I_X_WITH_ITEM-WT_QSSHH,
                   I_X_WITH_ITEM-WT_QSSH2,
                   I_X_WITH_ITEM-WT_QSSH3.

            MODIFY I_X_WITH_ITEM.

            MOVE-CORRESPONDING I_X_WITH_ITEM TO CHANGED_WITH_ITEM.
            APPEND CHANGED_WITH_ITEM.
          ENDIF.
        ENDLOOP.

      ELSE.

        "new WT types
        "loop over relev. types and compare with entries in table with_item
        "and make new entries and add it.

        LOOP AT A_X_WITH_ITEM.

          READ TABLE I_X_WITH_ITEM WITH KEY WITHT = A_X_WITH_ITEM-WITHT.
          IF SY-SUBRC <> 0. " item for relevant type is not existing
            CLEAR I_X_WITH_ITEM.
            I_X_WITH_ITEM-BUKRS     = TG_BSIK-BUKRS.
            I_X_WITH_ITEM-BELNR     = TG_BSIK-BELNR.
            I_X_WITH_ITEM-GJAHR     = TG_BSIK-GJAHR.
            I_X_WITH_ITEM-BUZEI     = TG_BSIK-BUZEI.
            I_X_WITH_ITEM-KOART     = 'K'.
            I_X_WITH_ITEM-WT_ACCO   = TG_BSIK-LIFNR.
            I_X_WITH_ITEM-WITHT     = A_X_WITH_ITEM-WITHT.
            I_X_WITH_ITEM-WT_WITHCD = A_X_WITH_ITEM-WT_WITHCD.

            CLEAR: I_X_WITH_ITEM-WT_QSSHB,
                   I_X_WITH_ITEM-WT_QSSHH,
                   I_X_WITH_ITEM-WT_QSSH2,
                   I_X_WITH_ITEM-WT_QSSH3.

            APPEND I_X_WITH_ITEM.
            MOVE-CORRESPONDING I_X_WITH_ITEM TO ADDED_WITH_ITEM.
            APPEND ADDED_WITH_ITEM.
            IF UPDATE_QSSKZ = 'X'.
              MOVE I_X_WITH_ITEM-BUKRS TO WA_BSEG-BUKRS.
              MOVE I_X_WITH_ITEM-BELNR TO WA_BSEG-BELNR.
              MOVE I_X_WITH_ITEM-GJAHR TO WA_BSEG-GJAHR.
              MOVE I_X_WITH_ITEM-BUZEI TO WA_BSEG-BUZEI.
              COLLECT WA_BSEG INTO I_BSEG.
              UPDATE_QSSKZ = ' '.
            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF. "else if i_x_with_item[] is not initial.

    ENDLOOP. "loop at tg_bsik where lifnr = tg_lfb1-lifnr.

  ENDLOOP. "loop at tg_lfb1.

  "---- update database
  IF ADDED_WITH_ITEM[] IS NOT INITIAL.

    INSERT WITH_ITEM FROM TABLE ADDED_WITH_ITEM.

*   update bseg-qsskz and index tables
    LOOP AT I_BSEG INTO WA_BSEG.
      SELECT SINGLE * FROM BKPF INTO WA_BKPF
          WHERE BUKRS = WA_BSEG-BUKRS AND
                BELNR = WA_BSEG-BELNR AND
                GJAHR = WA_BSEG-GJAHR.
      APPEND WA_BKPF TO I_BKPF.
      DATA ETL4395C6R4427 TYPE TABLE OF BSEG.
      DATA RLDNR_L4395C6R7232 TYPE RLDNR.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          E_RLDNR       = RLDNR_L4395C6R7232
        EXCEPTIONS
          NOT_FOUND     = 1
          MORE_THAN_ONE = 2.
      IF SY-SUBRC = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            I_RLDNR   = RLDNR_L4395C6R7232
            I_BUKRS   = WA_BSEG-BUKRS
            I_BELNR   = WA_BSEG-BELNR
            I_GJAHR   = WA_BSEG-GJAHR
            I_BUZEI   = WA_BSEG-BUZEI
          IMPORTING
            ET_BSEG   = ETL4395C6R4427
          EXCEPTIONS
            NOT_FOUND = 1.
      ENDIF.
      IF SY-SUBRC = 0 AND LINES( ETL4395C6R4427 ) = 1.
        WAR_BSEG = ETL4395C6R4427[ 1 ].
        SY-DBCNT = 1.
      ELSE.
        SY-SUBRC = 4.
        SY-DBCNT = 0.
      ENDIF.

      WAR_BSEG-QSSKZ = 'XX'.
      WA_BSEG = WAR_BSEG.
      MODIFY I_BSEG FROM WA_BSEG.
    ENDLOOP.

    CALL FUNCTION 'CHANGE_DOCUMENT'
      TABLES
        T_BKDF = I_BKDF
        T_BKPF = I_BKPF
        T_BSEC = I_BSEC
        T_BSED = I_BSED
        T_BSEG = I_BSEG
        T_BSET = I_BSET.

  ELSEIF CHANGED_WITH_ITEM[] IS NOT INITIAL.
    UPDATE WITH_ITEM FROM TABLE CHANGED_WITH_ITEM.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FI_WT_RECONSTRUCT_DOCUMENT
*&---------------------------------------------------------------------*
* Reconstruct document for recalculation of WT base
* add WT amounts posted at invoice time to the amount of the line items
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
FORM FI_WT_RECONSTRUCT_DOCUMENT TABLES O_BSEG STRUCTURE BSEG
                                       O_BSET STRUCTURE BSET
                                USING VALUE(I_BUKRS) LIKE T001-BUKRS
                                      VALUE(I_BELNR) LIKE BSEG-BELNR
                                      VALUE(I_GJAHR) LIKE BSEG-GJAHR.

  DATA: H_WITH  LIKE WITH_ITEMX OCCURS 1 WITH HEADER LINE,
        H_T059P LIKE T059P OCCURS 1 WITH HEADER LINE,
        H_T059Z LIKE T059Z OCCURS 1 WITH HEADER LINE.

  DATA: SUMME1 LIKE BSEG-WRBTR,
        SUMME2 LIKE BSEG-DMBTR,
        SUMME3 LIKE BSEG-DMBE2,
        SUMME4 LIKE BSEG-DMBE3.

  CLEAR: O_BSEG[], O_BSET[], H_WITH[].

  DATA RLDNR_L4445C2R5836 TYPE RLDNR.
  CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
    IMPORTING
      E_RLDNR       = RLDNR_L4445C2R5836
    EXCEPTIONS
      NOT_FOUND     = 1
      MORE_THAN_ONE = 2.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
      EXPORTING
        I_RLDNR   = RLDNR_L4445C2R5836
        I_BUKRS   = I_BUKRS
        I_BELNR   = I_BELNR
        I_GJAHR   = I_GJAHR
      IMPORTING
        ET_BSEG   = O_BSEG[]
      EXCEPTIONS
        NOT_FOUND = 1.
  ENDIF.
  IF SY-SUBRC <> 0 OR LINES( O_BSEG ) = 0.
    SY-SUBRC = 4.
    SY-DBCNT = 0.
  ELSE.
    SY-DBCNT = LINES( O_BSEG ).
  ENDIF.


  SELECT * FROM BSET INTO TABLE O_BSET
                                WHERE BUKRS = I_BUKRS
                                  AND BELNR = I_BELNR
                                  AND GJAHR = I_GJAHR.

  LOOP AT O_BSEG.

    CHECK O_BSEG-KOART = 'D' OR
          O_BSEG-KOART = 'K'.

    CLEAR: SUMME1,
           SUMME2,
           SUMME3,
           SUMME4.

    CLEAR H_WITH[].
    SELECT * FROM WITH_ITEM INTO TABLE H_WITH
                                 WHERE BUKRS = O_BSEG-BUKRS
                                   AND BELNR = O_BSEG-BELNR
                                   AND GJAHR = O_BSEG-GJAHR
                                   AND BUZEI = O_BSEG-BUZEI.

    LOOP AT H_WITH.
*--- read WT type info
      CALL FUNCTION 'FI_WT_READ_T059P'
        EXPORTING
          I_BUKRS   = I_BUKRS
          I_TYPE    = H_WITH-WITHT
        TABLES
          T_T059P   = H_T059P
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
*--- posting at invoice time?
      CHECK H_T059P-WT_POSTM = '1'.

*--- read WT code info
      CALL FUNCTION 'FI_WT_READ_T059Z'
        EXPORTING
          I_BUKRS     = I_BUKRS
          I_TYPE      = H_WITH-WITHT
          I_WT_WITHCD = H_WITH-WT_WITHCD
        TABLES
          T_T059Z     = H_T059Z
        EXCEPTIONS
          NOT_FOUND   = 1
          OTHERS      = 2.
*--- vendor/customer position reduced?
      CHECK H_T059Z-WT_POSIN = '1'.

      SUMME1 = SUMME1 + H_WITH-WT_QBSHB.
      SUMME2 = SUMME2 + H_WITH-WT_QBSHH.
      SUMME3 = SUMME3 + H_WITH-WT_QBSH2.
      SUMME4 = SUMME4 + H_WITH-WT_QBSH3.

    ENDLOOP.

    IF O_BSEG-SHKZG = 'H'.
      SUMME1 = - SUMME1.
      SUMME2 = - SUMME2.
      SUMME3 = - SUMME3.
      SUMME4 = - SUMME4.
    ENDIF.

    O_BSEG-WRBTR = O_BSEG-WRBTR + SUMME1.
    O_BSEG-DMBTR = O_BSEG-DMBTR + SUMME2.
    O_BSEG-DMBE2 = O_BSEG-DMBE2 + SUMME3.
    O_BSEG-DMBE3 = O_BSEG-DMBE3 + SUMME4.

    MODIFY O_BSEG.

  ENDLOOP.

ENDFORM.                               " FI_WT_RECONSTRUCT_DOCUMENT

FORM Z_CHECK_MOV_CERTIFICADO USING WA_MOV_ESTQ    TYPE ZMMT_EE_ZGR
                                   WA_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS.

  CALL FUNCTION 'ZSD_CHECK_MOV_CERTIFICADO'
    EXPORTING
      I_TP_MOV         = 'E'
      I_WA_MOV_ESTQ    = WA_MOV_ESTQ
      I_WA_DOC_GERADOS = WA_DOC_GERADOS.

ENDFORM.

FORM F_SALDO_CENTRO_FIXO_FIXAR USING P_MOV_ESTQ TYPE ZMMT_EE_ZGR.

  DATA: TG_0023       TYPE TABLE OF ZMM0023,
        WA_0023       TYPE ZMM0023,
        WA_0023_AUX   TYPE SY-SUBRC,
        SL_LIPS       TYPE LIPS,
        VL_CENTRO_A   TYPE WERKS_D,
        VL_CLABS_F    TYPE LABST,
        VL_CLABS_A    TYPE LABST,
        VL_TOTAL      TYPE LABST,
        VL_AUX        TYPE CHAR18,
        VL_MSN1       TYPE CHAR50,
        VL_MSN2       TYPE CHAR50,
        VL_MSN3       TYPE STRING,
        "WL_EKPO       TYPE EKPO,
        WA_DEPARA_CEN TYPE ZSDT_DEPARA_CEN,
        WA_ZMMT0017   TYPE ZMMT0017. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

  SELECT *
    FROM ZMM0023 INTO TABLE TG_0023.

*  SORT tg_0023 BY  werks ASCENDING matnr ASCENDING cwerks DESCENDING.
  SORT TG_0023 BY  WERKS ASCENDING MATNR ASCENDING MATKL ASCENDING CWERKS DESCENDING."PBALVES


  READ TABLE IT_LGORT INTO WA_LGORT
    WITH KEY EBELN = P_MOV_ESTQ-PO_NUMBER
             EBELP = P_MOV_ESTQ-PO_ITEM.

  CHECK ( SY-SUBRC EQ 0 ) AND
        ( WA_LGORT-LGORT IS NOT INITIAL ) AND
        ( WA_LGORT-CHARG IS NOT INITIAL ).

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF WA_DEPARA_CEN
    FROM EKPO AS A INNER JOIN ZSDT_DEPARA_CEN AS B ON A~WERKS = B~CENTROV_1
   WHERE A~EBELN = P_MOV_ESTQ-PO_NUMBER
     AND A~EBELP = P_MOV_ESTQ-PO_ITEM.

  CHECK SY-SUBRC = 0.

  CLEAR WA_0023.
  READ TABLE TG_0023 INTO WA_0023 WITH KEY WERKS = WA_DEPARA_CEN-CENTRO_REAL. "lê o primeiro
  "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria PSA

  IF SY-SUBRC NE 0.
    SELECT SINGLE MATKL INTO @DATA(_MATKL) FROM MARA WHERE MATNR = @P_MOV_ESTQ-MATERIAL.
    READ TABLE TG_0023 INTO WA_0023 WITH KEY WERKS = WA_DEPARA_CEN-CENTRO_REAL
                                             MATKL = _MATKL.
  ENDIF.
  "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
  CLEAR WA_0023_AUX.
  IF
    SY-SUBRC NE 0.
    WA_0023_AUX = SY-SUBRC.
  ENDIF.
  "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

  IF NOT SY-SUBRC = 0 OR WA_0023-STATUS NE 'A'.

*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO

*    SELECT SINGLE centro_a_fixar
*      FROM zmmt0017
*      INTO vl_centro_a
*    WHERE  matnr       EQ p_mov_estq-material
*      AND  centro_fixo EQ wa_depara_cen-centro_real
*      AND  lgort       EQ wa_lgort-lgort.


    ZCL_DEPARA_CENTRO_FIXO_AFIXAR=>ZIF_DEPARA_CENTRO_FIXO_AFIXAR~GET_DADOS_DEPARA(
       EXPORTING
         I_MATERIAL        = P_MOV_ESTQ-MATERIAL
         I_CENTRO_FIXO     = WA_DEPARA_CEN-CENTRO_REAL
         I_DEPOSITO        = WA_LGORT-LGORT
        " I_EUDR            =
       IMPORTING
         E_SINGLE_DEPARA            = WA_ZMMT0017
     ).

    IF WA_ZMMT0017 IS NOT INITIAL.
      VL_CENTRO_A = WA_ZMMT0017-CENTRO_A_FIXAR.

*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
      IF WA_0023_AUX IS NOT INITIAL.
        MESSAGE E897(SD) WITH  'Falta parâmetros na ZMM0029. '
                                  'Favor entrar em contato com '
                                   'a área de controladoria e estoque. '.
      ENDIF.
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

      SELECT SINGLE CLABS
          FROM MCHB
          INTO VL_CLABS_F
        WHERE  MATNR EQ P_MOV_ESTQ-MATERIAL
          AND  WERKS EQ WA_DEPARA_CEN-CENTRO_REAL
          AND  LGORT EQ WA_LGORT-LGORT
          AND  CHARG EQ WA_LGORT-CHARG.

      IF VL_CENTRO_A IS NOT INITIAL.
        SELECT SINGLE CLABS
          FROM MCHB
          INTO VL_CLABS_A
        WHERE  MATNR EQ P_MOV_ESTQ-MATERIAL
          AND  WERKS EQ VL_CENTRO_A
          AND  LGORT EQ WA_LGORT-LGORT
          AND  CHARG EQ WA_LGORT-CHARG.
      ENDIF.

      VL_TOTAL = VL_CLABS_A + VL_CLABS_F.

      IF WA_MOV_ESTQ-QUANTITY GT VL_TOTAL.
        VG_ERRO = C_X.
        VL_AUX = VL_TOTAL.
        CONDENSE VL_AUX NO-GAPS.
        CONCATENATE 'O total'
                    VL_AUX
                    'do centro e material'
               INTO VL_MSN1 SEPARATED BY SPACE.
        CONCATENATE WA_DEPARA_CEN-CENTRO_REAL
                    'e'
                    VL_CENTRO_A
               INTO VL_MSN2 SEPARATED BY SPACE.

        CONCATENATE VL_MSN1 VL_MSN2 'é menor que a quantidade do picking'
               INTO VL_MSN3 SEPARATED BY SPACE.

        "identificador de interface MIGO
        VG_INTERFACE = C_11.
        PERFORM Z_PREPARA_MENSAGEM2  USING VG_OBJ_KEY
                                           C_E
                                           VL_MSN3
                                           ''
                                           ''
                                           VG_INTERFACE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM F_CHECK_MSG_BLOQ TABLES P_RETURN STRUCTURE BAPIRET2
                    CHANGING P_BLOQ   TYPE C
                             _WL_RETURN TYPE BAPIRET2.

  DATA: LC_WAIT_INT_M3_897 TYPE I.

  P_BLOQ = ABAP_FALSE.

  READ TABLE P_RETURN INTO _WL_RETURN WITH KEY TYPE = 'E' ID = 'ME' NUMBER = '006'.
  IF SY-SUBRC EQ 0.
    P_BLOQ = ABAP_TRUE.
    EXIT.
  ENDIF.

  READ TABLE P_RETURN INTO _WL_RETURN WITH KEY TYPE = 'E' ID = 'M3' NUMBER = '024'.
  IF SY-SUBRC EQ 0.
    P_BLOQ = ABAP_TRUE.
    EXIT.
  ENDIF.

  READ TABLE P_RETURN INTO _WL_RETURN WITH KEY TYPE = 'E' ID = 'M3' NUMBER = '682'.
  IF SY-SUBRC EQ 0.
    P_BLOQ = ABAP_TRUE.
    EXIT.
  ENDIF.

  READ TABLE P_RETURN INTO _WL_RETURN WITH KEY TYPE = 'E' ID = 'M3' NUMBER = '897'.
  IF SY-SUBRC EQ 0.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_SET_INTERVAL_M3_897)
     WHERE SETNAME = 'ENT_EST_INTERVAL_M3_897'.

    IF ( SY-SUBRC EQ 0 ) AND ( _WL_SET_INTERVAL_M3_897-VALFROM > 0 ).
      LC_WAIT_INT_M3_897 = _WL_SET_INTERVAL_M3_897-VALFROM.
      WAIT UP TO LC_WAIT_INT_M3_897 SECONDS.
    ENDIF.

    SELECT SINGLE *
      FROM SETLEAF INTO @DATA(_WL_SET_EXIT_M3_897)
     WHERE SETNAME = 'ENT_EST_EXIT_M3_897'
       AND VALFROM = 'X'.

    IF SY-SUBRC NE 0.
      P_BLOQ = ABAP_TRUE.
      EXIT.
    ENDIF.
  ENDIF.

  LOOP AT P_RETURN INTO DATA(WA_RETURN_INTERNO).
    MESSAGE ID WA_RETURN_INTERNO-ID TYPE 'S'
     NUMBER WA_RETURN_INTERNO-NUMBER
       WITH WA_RETURN_INTERNO-MESSAGE_V1 WA_RETURN_INTERNO-MESSAGE_V2 WA_RETURN_INTERNO-MESSAGE_V3 WA_RETURN_INTERNO-MESSAGE_V4
    DISPLAY LIKE 'E'.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_FORNECEDOR_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MOV_ESTQ  text
*      -->P_VG_VALIDA_IMP  text
*----------------------------------------------------------------------*
FORM VERIFICA_FORNECEDOR_NFE  USING P_WA_MOV_ESTQ TYPE ZMMT_EE_ZGR
                                    P_VALIDADO    TYPE SY-SUBRC.

  DATA: LC_SERIES LIKE  J_1BNFDOC-SERIES.

  P_VALIDADO = 1.

  "Busca fornecedor para buscar o tax code
  SELECT SINGLE LIFNR
    INTO @DATA(VG_FORNE)
    FROM EKKO
   WHERE EBELN EQ @P_WA_MOV_ESTQ-PO_NUMBER.

  IF P_WA_MOV_ESTQ-J_1BNFTYPE IS NOT INITIAL AND VG_FORNE IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(WA_J_1BAA)
      FROM J_1BAA
     WHERE NFTYPE EQ @P_WA_MOV_ESTQ-J_1BNFTYPE.

    CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
      EXPORTING
        REF_NUMBER         = P_WA_MOV_ESTQ-NT_REMESSA
        I_NFEFLAG          = 'X'
      IMPORTING
        SERIES             = LC_SERIES
      EXCEPTIONS
        NUMBER_ERROR       = 1
        TOO_MANY_DASHES    = 2
        DOCNUM_TOO_LONG    = 3
        DOCNUM_NOT_NUMERIC = 4
        SERIES_NOT_NUMERIC = 5
        OTHERS             = 6.

    IF SY-SUBRC IS INITIAL.
      IF NOT ( LC_SERIES GE '890' AND LC_SERIES LE '999' ).

        IF ( WA_J_1BAA-NFE EQ ABAP_TRUE ) AND ( WA_J_1BAA-FORM IS INITIAL ).
          TRY .
              ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
                )->SET_PARCEIRO( I_PARCEIRO = VG_FORNE
                )->CK_EMISSOR_NF_E(
                ).

              IF WA_MOV_ESTQ-LIFNR IS NOT INITIAL.
                ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
                  )->SET_PARCEIRO( I_PARCEIRO = WA_MOV_ESTQ-LIFNR
                  )->CK_EMISSOR_NF_E(
                  ).
              ENDIF.

            CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).
              EX_PARCEIROS->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
              MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO VG_RETURN.
              PERFORM Z_PREPARA_MENSAGEM2 USING VG_OBJ_KEY C_E VG_RETURN VG_FORNE SPACE C_11.
              EXIT.
          ENDTRY.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

  P_VALIDADO = 0.

ENDFORM.

FORM F_CHECK_REG_DOCS  USING P_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS.

  CHECK ( P_DOC_GERADOS-OBJ_KEY IS NOT INITIAL ) .

  IF ( P_DOC_GERADOS-AV_VBELN       IS INITIAL ) AND "Não gerou Aviso Recebimento
     ( P_DOC_GERADOS-MM_MBLNR       IS INITIAL ) AND "Não gerou Migo
     ( P_DOC_GERADOS-FT_BELNR       IS INITIAL ) AND "Não gerou Miro
     ( P_DOC_GERADOS-DOCNUM         IS INITIAL ) AND "Não gerou Doc. Fiscal
     ( P_DOC_GERADOS-MM_MBLNR_SOBRA IS INITIAL ).    "Não gerou Doc.Material Sobra

    DELETE FROM ZMMT_EE_ZGR_DOCS WHERE OBJ_KEY = P_DOC_GERADOS-OBJ_KEY.
    COMMIT WORK.

  ENDIF.

ENDFORM.

FORM F_RET_DOC_CTB_MIRO  TABLES T_RETURN STRUCTURE BAPIRET2
                          USING P_DOC_GERADOS TYPE ZMMT_EE_ZGR_DOCS.

  CHECK P_DOC_GERADOS-FT_BELNR IS NOT INITIAL AND P_DOC_GERADOS-FT_GJAHR IS NOT INITIAL.

  CONCATENATE  P_DOC_GERADOS-FT_BELNR  P_DOC_GERADOS-FT_GJAHR INTO DATA(LWA_AWKEY_BKPF).

  SELECT SINGLE BELNR, BUKRS, GJAHR
    FROM BKPF INTO @DATA(LWA_BKPF_MIRO)
   WHERE BUKRS = @P_DOC_GERADOS-BUKRS
     AND GJAHR = @P_DOC_GERADOS-FT_GJAHR
     AND AWKEY = @LWA_AWKEY_BKPF
     AND BLART = 'ZG'.

  IF SY-SUBRC NE 0.

    SELECT SINGLE BELNR, BUKRS, GJAHR
      FROM BKPF INTO @LWA_BKPF_MIRO
     WHERE BUKRS = @P_DOC_GERADOS-BUKRS
       AND GJAHR = @P_DOC_GERADOS-FT_GJAHR
       AND BELNR = @P_DOC_GERADOS-FT_BELNR.

  ENDIF.

  CHECK SY-SUBRC EQ 0.

  CONCATENATE 'O Documento'  LWA_BKPF_MIRO-BELNR
              'foi gerado para a empresa' LWA_BKPF_MIRO-BUKRS
              'no exercicio de' LWA_BKPF_MIRO-GJAHR '!' INTO DATA(LWA_MSG) SEPARATED BY SPACE.

  WA_RETURN-TYPE       = C_S.
  WA_RETURN-ID         = 'Z01'.
  WA_RETURN-NUMBER     = 3.
  WA_RETURN-MESSAGE    = LWA_MSG.
  WA_RETURN-MESSAGE_V1 = LWA_BKPF_MIRO-BELNR.
  WA_RETURN-MESSAGE_V2 = LWA_BKPF_MIRO-BUKRS.
  WA_RETURN-MESSAGE_V3 = LWA_BKPF_MIRO-GJAHR.


  PERFORM Z_PREPARA_MENSAGEM3 USING VG_OBJ_KEY
                                    WA_RETURN
                                    '03'.

ENDFORM.

FORM F_SET_INF_ADD_RET_MIRO CHANGING P_RETURN TYPE ZFIE_RET_DOCUMENT.

  DATA: LWA_DADOS_ADD      TYPE ZSFI0001,
        LVA_JSON_DADOS_ADD TYPE C LENGTH 30000,
        LWA_BKPF_MIRO      TYPE BKPF.

  CHECK P_RETURN-INTERFACE EQ C_12. "Miro.

  CHECK P_RETURN-TYPE = 'S' AND P_RETURN-MESSAGE_V1 IS NOT INITIAL AND P_RETURN-MESSAGE_V2 IS NOT INITIAL.

  SELECT SINGLE *
    FROM RBKP INTO @DATA(_LWA_RBKP)
   WHERE BELNR EQ @P_RETURN-MESSAGE_V1(10)
     AND GJAHR EQ @P_RETURN-MESSAGE_V2(4).

  CHECK SY-SUBRC EQ 0.

  CONCATENATE  P_RETURN-MESSAGE_V1 P_RETURN-MESSAGE_V2 INTO DATA(LWA_AWKEY_BKPF).

  SELECT SINGLE BELNR BUKRS GJAHR BUDAT KURS2 CPUDT
    FROM BKPF INTO CORRESPONDING FIELDS OF LWA_BKPF_MIRO
   WHERE BUKRS = _LWA_RBKP-BUKRS
     AND GJAHR = _LWA_RBKP-GJAHR
     AND AWKEY = LWA_AWKEY_BKPF
     AND BLART = 'ZG'.

  IF SY-SUBRC NE 0.
    SELECT SINGLE BELNR BUKRS GJAHR BUDAT KURS2 CPUDT
      FROM BKPF INTO CORRESPONDING FIELDS OF LWA_BKPF_MIRO
     WHERE BUKRS = _LWA_RBKP-BUKRS
       AND GJAHR = _LWA_RBKP-GJAHR
       AND BELNR = _LWA_RBKP-BELNR.
  ENDIF.

  MOVE-CORRESPONDING LWA_BKPF_MIRO TO LWA_DADOS_ADD.

  LVA_JSON_DADOS_ADD  = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DADOS_ADD ).

  P_RETURN-INFO_ADICIONAL_1 = LVA_JSON_DADOS_ADD+0000(4000).
  P_RETURN-INFO_ADICIONAL_2 = LVA_JSON_DADOS_ADD+4000(4000).
  P_RETURN-INFO_ADICIONAL_3 = LVA_JSON_DADOS_ADD+8000(4000).

ENDFORM.

FORM F_SET_INF_ADD_RET_MIGO CHANGING P_RETURN TYPE ZFIE_RET_DOCUMENT.

  DATA: LWA_DADOS_ADD      TYPE ZSFI0001,
        LVA_JSON_DADOS_ADD TYPE C LENGTH 30000,
        LWA_BKPF_MIGO      TYPE BKPF.

  CLEAR: LWA_BKPF_MIGO.

  CHECK P_RETURN-INTERFACE EQ C_11. "Migo.

  CHECK P_RETURN-TYPE = 'S' AND P_RETURN-MESSAGE_V1 IS NOT INITIAL.

  SELECT SINGLE *
    FROM MKPF INTO @DATA(_LWA_MKPF)
   WHERE MBLNR EQ @P_RETURN-MESSAGE_V1(10).

  CHECK SY-SUBRC EQ 0.

  CONCATENATE  _LWA_MKPF-MBLNR _LWA_MKPF-MJAHR INTO DATA(LWA_AWKEY_BKPF).

  SELECT SINGLE BELNR BUKRS GJAHR BUDAT KURS2 CPUDT
    FROM BKPF INTO CORRESPONDING FIELDS OF LWA_BKPF_MIGO
   WHERE XBLNR = LWA_AWKEY_BKPF.

  IF SY-SUBRC NE 0.
    SELECT SINGLE BELNR BUKRS GJAHR BUDAT KURS2 CPUDT
      FROM BKPF INTO CORRESPONDING FIELDS OF LWA_BKPF_MIGO
     WHERE AWKEY = LWA_AWKEY_BKPF
       AND BLART <> 'ML'.
  ENDIF.

  CHECK LWA_BKPF_MIGO IS NOT INITIAL.

  MOVE-CORRESPONDING LWA_BKPF_MIGO TO LWA_DADOS_ADD.

  LVA_JSON_DADOS_ADD  = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DADOS_ADD ).

  P_RETURN-INFO_ADICIONAL_1 = LVA_JSON_DADOS_ADD+0000(4000).
  P_RETURN-INFO_ADICIONAL_2 = LVA_JSON_DADOS_ADD+4000(4000).
  P_RETURN-INFO_ADICIONAL_3 = LVA_JSON_DADOS_ADD+8000(4000).

ENDFORM.



FORM Z_PREPARA_MENSAGEM3  USING   P_OBJ_KEY    TYPE ANY
                                  P_RETURN     TYPE BAPIRET2
                                  P_INTERFACE  TYPE ANY.

  DATA: WA_ZOB_MENSAGEM TYPE ZOB_MENSAGEM.

  WA_OUTRETURN-OBJ_KEY        = P_OBJ_KEY.
  WA_OUTRETURN-INTERFACE      = P_INTERFACE.
  WA_OUTRETURN-DT_ATUALIZACAO = SY-DATUM.
  WA_OUTRETURN-HR_ATUALIZACAO = SY-UZEIT.
  WA_OUTRETURN-TYPE           = P_RETURN-TYPE.
  WA_OUTRETURN-ID             = P_RETURN-ID.
  WA_OUTRETURN-NUM            = P_RETURN-NUMBER.
  WA_OUTRETURN-MESSAGE        = P_RETURN-MESSAGE.
  WA_OUTRETURN-MESSAGE_V1     = P_RETURN-MESSAGE_V1.
  WA_OUTRETURN-MESSAGE_V2     = P_RETURN-MESSAGE_V2.
  WA_OUTRETURN-MESSAGE_V3     = P_RETURN-MESSAGE_V3.
  WA_OUTRETURN-MESSAGE_V4     = P_RETURN-MESSAGE_V4.


  APPEND WA_OUTRETURN TO IT_OUTRETURN.

  MOVE-CORRESPONDING WA_OUTRETURN TO WA_ZOB_MENSAGEM.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZOB_MENSG'
    IMPORTING
      NUMBER                  = WA_ZOB_MENSAGEM-SEQ_REGISTRO
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.

  CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
    EXPORTING
      I_DATE   = SY-DATUM
      I_TIME   = SY-UZEIT
    IMPORTING
      E_TSTAMP = WA_ZOB_MENSAGEM-TIMESTAMP.

  APPEND WA_ZOB_MENSAGEM TO IT_ZOB_MENSAGEM.
  CLEAR: WA_OUTRETURN.

ENDFORM.                    " Z_PREPARA_MENSAGEM2
*&---------------------------------------------------------------------*
*&      Form  Z_GRAVA_CONTABIL_MILHO
*&---------------------------------------------------------------------*
FORM Z_GRAVA_CONTABIL_MILHO  USING  WA_MOV_ESTQ TYPE ZMMT_EE_ZGR
                                    P_ESTORNO.

  TYPES: BEGIN OF TY_EEIMP,
           WI_TAX_CODE TYPE CHAR3,
           WT_WITHCD   TYPE CHAR4,
           WI_TAX_AMT  TYPE ZMMT_EEIMP_ZGR-WI_TAX_AMT,
         END OF TY_EEIMP.

  DATA: IT_EEIMP TYPE STANDARD TABLE OF TY_EEIMP,
        LS_EEIMP TYPE TY_EEIMP.


  DATA: L_LIFNR   TYPE EKKO-LIFNR,
        L_TAX_AMT TYPE ZIB_CONTABIL-WRBTR.

  FREE: WA_ZIB_CONTABIL,
        L_TAX_AMT.

  SELECT WI_TAX_CODE, WT_WITHCD, WI_TAX_AMT
    FROM ZMMT_EEIMP_ZGR
    INTO TABLE @DATA(T_EEIMP_ZGR)
   WHERE OBJ_KEY      = @WA_MOV_ESTQ-OBJ_KEY
     AND WI_TAX_CODE = 'F1'
     AND WT_WITHCD   = 'FH'.
  IF SY-SUBRC IS INITIAL.
    LOOP AT T_EEIMP_ZGR INTO DATA(LS_ZGE).
      MOVE-CORRESPONDING: LS_ZGE TO LS_EEIMP.
      APPEND LS_EEIMP TO IT_EEIMP.
      CLEAR LS_EEIMP.
    ENDLOOP.




    SELECT SINGLE MAKTX
      FROM MAKT
      INTO @DATA(LS_MAKT)
      WHERE MATNR EQ @WA_MOV_ESTQ-MATERIAL
        AND SPRAS EQ 'P'.

    IF IT_EEIMP[] IS NOT INITIAL.
      SELECT KONTS
        FROM T030
        INTO TABLE @DATA(LT_T030)
        FOR ALL ENTRIES IN @IT_EEIMP
        WHERE KTOPL  = '0050'   "plano de contas
          AND KTOSL  = 'WIT'
          AND BWMOD  = @IT_EEIMP-WT_WITHCD
          AND KOMOK  = @IT_EEIMP-WI_TAX_CODE.
    ENDIF.


    SELECT LIFNR
      INTO L_LIFNR
      FROM EKKO
        UP TO 1 ROWS
     WHERE EBELN = WA_MOV_ESTQ-PO_NUMBER.
    ENDSELECT.

    LOOP AT T_EEIMP_ZGR INTO DATA(W_EEIMP_ZGR).
      L_TAX_AMT = L_TAX_AMT + W_EEIMP_ZGR-WI_TAX_AMT.
    ENDLOOP.




    WA_ZIB_CONTABIL-MANDT         = SY-MANDT.
    WA_ZIB_CONTABIL-SEQITEM       = C_000001.


    IF P_ESTORNO = C_X.
      CONCATENATE 'EST' VG_INVOICEDOCNUMBER_MIRO VG_ANO_MIRO '001' INTO WA_ZIB_CONTABIL-OBJ_KEY.


*    wa_zib_contabil-bschl       = c_21.
      WA_ZIB_CONTABIL-BSCHL       = C_40. "
    ELSE.
*    wa_zib_contabil-bschl       = c_31.
      WA_ZIB_CONTABIL-BSCHL       = C_50. "
      CONCATENATE 'ZGR' VG_INVOICEDOCNUMBER_MIRO VG_ANO_MIRO '001' INTO WA_ZIB_CONTABIL-OBJ_KEY.
    ENDIF.

    WA_ZIB_CONTABIL-INTERFACE     = C_11.
    WA_ZIB_CONTABIL-BKTXT         = WA_MOV_ESTQ-NT_REMESSA.


    SELECT SINGLE VBUND INTO @DATA(LV_VBUND) FROM LFA1 WHERE LIFNR EQ @L_LIFNR. "RJF-Ini

    IF LV_VBUND IS NOT INITIAL.
      WA_ZIB_CONTABIL-HKONT         = '0000323511'.
      WA_ZIB_CONTABIL-VBUND         = LV_VBUND.
    ELSE.
      WA_ZIB_CONTABIL-HKONT         = '0000323510'.
      WA_ZIB_CONTABIL-VBUND         = ABAP_OFF.
    ENDIF.
    "RJF-Fim

*  wa_zib_contabil-hkont         = l_lifnr.
    WA_ZIB_CONTABIL-GSBER         = WA_MOV_ESTQ-PLANT.
    WA_ZIB_CONTABIL-BUKRS         = WA_MOV_ESTQ-COMP_CODE.
    WA_ZIB_CONTABIL-GJAHR         = WA_MOV_ESTQ-PSTNG_DATE(4).
    WA_ZIB_CONTABIL-MONAT         = WA_MOV_ESTQ-PSTNG_DATE+4(2).
    WA_ZIB_CONTABIL-BLART         = C_SA.

    CONCATENATE WA_MOV_ESTQ-PSTNG_DATE+6(2)
                WA_MOV_ESTQ-PSTNG_DATE+4(2)
                WA_MOV_ESTQ-PSTNG_DATE(4)
      INTO WA_ZIB_CONTABIL-BUDAT SEPARATED BY '.'.

    CONCATENATE WA_MOV_ESTQ-DOC_DATE+6(2)
                WA_MOV_ESTQ-DOC_DATE+4(2)
                WA_MOV_ESTQ-DOC_DATE(4)
      INTO WA_ZIB_CONTABIL-BLDAT SEPARATED BY '.'.

    WA_ZIB_CONTABIL-WRBTR              = W_EEIMP_ZGR-WI_TAX_AMT.
    WA_ZIB_CONTABIL-WAERS              = C_BRL.
    WA_ZIB_CONTABIL-BUPLA              = WA_MOV_ESTQ-PLANT.
    WA_ZIB_CONTABIL-RG_ATUALIZADO      = C_N.
    WA_ZIB_CONTABIL-ZUONR              = WA_MOV_ESTQ-PO_NUMBER.
















































    WA_ZIB_CONTABIL-KIDNO              = WA_MAT_DOC.
*  wa_zib_contabil-xref1              = wa_mov_estq-text1+7(4).
    WA_ZIB_CONTABIL-XREF1              = ABAP_OFF.
    WA_ZIB_CONTABIL-XREF3              = LS_MAKT.
    WA_ZIB_CONTABIL-XBLNR              = WA_MOV_ESTQ-NT_REMESSA.
    WA_ZIB_CONTABIL-SGTXT              = 'Fethab compra milho'.
    WA_ZIB_CONTABIL-PRCTR              = '9900'. "RJF
    WA_ZIB_CONTABIL-MATNR              = WA_MOV_ESTQ-MATERIAL."RJF


    MODIFY ZIB_CONTABIL  FROM WA_ZIB_CONTABIL.

    "---------------------- ITEM 0002 ----------------------"

    WA_ZIB_CONTABIL-MANDT         = SY-MANDT.
    WA_ZIB_CONTABIL-SEQITEM       = C_000002.

    IF P_ESTORNO = C_X.
      CONCATENATE 'EST' VG_INVOICEDOCNUMBER_MIRO VG_ANO_MIRO '001' INTO WA_ZIB_CONTABIL-OBJ_KEY.
      WA_ZIB_CONTABIL-BSCHL       = C_50.
    ELSE.
      CONCATENATE 'ZGR' VG_INVOICEDOCNUMBER_MIRO VG_ANO_MIRO '001' INTO WA_ZIB_CONTABIL-OBJ_KEY.
      WA_ZIB_CONTABIL-BSCHL       = C_40.
    ENDIF.


    WA_ZIB_CONTABIL-INTERFACE     = C_11.
    WA_ZIB_CONTABIL-BKTXT         = WA_MOV_ESTQ-NT_REMESSA.
    READ TABLE LT_T030 INTO DATA(LS_T030) INDEX 1.
    WA_ZIB_CONTABIL-HKONT         = LS_T030-KONTS.
    WA_ZIB_CONTABIL-GSBER         = WA_MOV_ESTQ-PLANT.
    WA_ZIB_CONTABIL-BUKRS         = WA_MOV_ESTQ-COMP_CODE.
    WA_ZIB_CONTABIL-GJAHR         = WA_MOV_ESTQ-PSTNG_DATE(4).
    WA_ZIB_CONTABIL-MONAT         = WA_MOV_ESTQ-PSTNG_DATE+4(2).
    WA_ZIB_CONTABIL-BLART         = C_SA.

    CONCATENATE WA_MOV_ESTQ-PSTNG_DATE+6(2)
                WA_MOV_ESTQ-PSTNG_DATE+4(2)
                WA_MOV_ESTQ-PSTNG_DATE(4)
      INTO WA_ZIB_CONTABIL-BUDAT SEPARATED BY '.'.

    CONCATENATE WA_MOV_ESTQ-DOC_DATE+6(2)
                WA_MOV_ESTQ-DOC_DATE+4(2)
                WA_MOV_ESTQ-DOC_DATE(4)
      INTO WA_ZIB_CONTABIL-BLDAT SEPARATED BY '.'.

    WA_ZIB_CONTABIL-WRBTR              = W_EEIMP_ZGR-WI_TAX_AMT.
    WA_ZIB_CONTABIL-WAERS              = C_BRL.
    WA_ZIB_CONTABIL-BUPLA              = WA_MOV_ESTQ-PLANT.
    WA_ZIB_CONTABIL-RG_ATUALIZADO      = C_N.
    WA_ZIB_CONTABIL-ZUONR              = WA_MOV_ESTQ-PO_NUMBER.
    WA_ZIB_CONTABIL-KIDNO              = WA_MAT_DOC.
    WA_ZIB_CONTABIL-XREF1              = WA_MOV_ESTQ-TEXT1+7(4).
    WA_ZIB_CONTABIL-XREF3              =  LS_MAKT.
    WA_ZIB_CONTABIL-XBLNR              = WA_MOV_ESTQ-NT_REMESSA.
    WA_ZIB_CONTABIL-SGTXT              = 'Fethab compra milho'.

    MODIFY ZIB_CONTABIL  FROM WA_ZIB_CONTABIL.
**  LOOP AT t_eeimp_zgr INTO w_eeimp_zgr.
**
**    CLEAR wa_taxcode.
**    READ TABLE it_taxcode INTO wa_taxcode WITH KEY tax_code = w_eeimp_zgr-wi_tax_code
**                                          BINARY SEARCH.
**
**    CASE  w_eeimp_zgr-wi_tax_code.
**      WHEN 'F0'.
**        wa_zib_contabil-seqitem = c_000002.
**        IF p_estorno = c_x.
**          wa_zib_contabil-bschl = c_40.
**        ELSE.
**          wa_zib_contabil-bschl = c_50.
**        ENDIF.
**        wa_zib_contabil-hkont   = wa_taxcode-hkont.
**        wa_zib_contabil-wrbtr   = w_eeimp_zgr-wi_tax_amt.
**        wa_zib_contabil-bktxt   = 'FACS'.
**        MODIFY zib_contabil  FROM wa_zib_contabil.
**
**      WHEN 'F1'.
**        wa_zib_contabil-seqitem = c_000003.
**        IF p_estorno = c_x.
**          wa_zib_contabil-bschl = c_40.
**        ELSE.
**          wa_zib_contabil-bschl = c_50.
**        ENDIF.
**        wa_zib_contabil-hkont   =  wa_taxcode-hkont.
**        wa_zib_contabil-wrbtr   = w_eeimp_zgr-wi_tax_amt.
**        wa_zib_contabil-bktxt   = 'FETHAB'.
**        MODIFY zib_contabil  FROM wa_zib_contabil.
**    ENDCASE.
**  ENDLOOP.

    COMMIT WORK.

  ENDIF.

ENDFORM.
