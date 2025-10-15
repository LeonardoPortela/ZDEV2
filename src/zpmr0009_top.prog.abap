*&---------------------------------------------------------------------*
*&  Include           ZPMR0009_TOP
*&---------------------------------------------------------------------*
PROGRAM  ZPMR0009.

* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_EQUIPAMENTO,
          EQUNR      TYPE EQUZ-EQUNR,                   " Nº Equipamento
          HEQUI      TYPE EQUZ-HEQUI,                   " Equipamento Superior
          HEQNR      TYPE EQUZ-HEQNR,                   " Item
          IWERK      TYPE EQUZ-IWERK,                   " Centro Planejamento
          GEWRK      TYPE EQUZ-GEWRK,                   " Centro de trabalho
          EQTYP      TYPE EQUI-EQTYP,                   " Ctg Equipamento
          EQART      TYPE EQUI-EQART,                   " Tipo do objeto técnico
          HERST      TYPE EQUI-HERST,                   " Fabricante
          TYPBZ      TYPE EQUI-TYPBZ,                   " Denominação do tipo
          OBJNR      TYPE EQUI-OBJNR,                   " Objeto
          DATBI      TYPE EQUZ-DATBI,                   " Última data de validade de montagem
          DATAB      TYPE EQUZ-DATAB,                   " Válido desde
          EQKTX      TYPE EQKT-EQKTX,                   " descrição de equipamento
          PLTXT      TYPE IFLOTX-PLTXT,                 " descrição de local
          TIMBI      TYPE EQUZ-TIMBI,                   " Última hora de montagem
          NUM_AXLE   TYPE FLEET-NUM_AXLE,               " Número de eixos
          READ_FLOC  TYPE BAPI_ITOB_EQ_ONLY-READ_FLOC,  " Local de instalação
          TECHID     TYPE BAPI_ITOB_EQ_ONLY-TECHID,     " Local de instalação
          AUTHGRP    TYPE BAPI_ITOB-AUTHGRP,            " Grupo de autorização
          CONSTTYPE  TYPE BAPI_ITOB-CONSTTYPE,          " Material
          INVENTORY  TYPE BAPI_ITOB-INVENTORY,          " Inventário
          DESCRIPT   TYPE BAPI_ITOB-DESCRIPT ,
          ABCINDIC   TYPE BAPI_ITOB-ABCINDIC ,
          ACQUISVAL  TYPE BAPI_ITOB-ACQUISVAL ,
          CURRENCY   TYPE BAPI_ITOB-CURRENCY ,
          OBJ_WEIGHT TYPE BAPI_ITOB-OBJ_WEIGHT ,
          UNIT_OF_WT TYPE BAPI_ITOB-UNIT_OF_WT ,
          CONSTYEAR  TYPE BAPI_ITOB-CONSTYEAR ,
          CONSTMONTH TYPE BAPI_ITOB-CONSTMONTH ,
          OBJ_SIZE   TYPE BAPI_ITOB-OBJ_SIZE ,
          MANFACTURE TYPE BAPI_ITOB-MANFACTURE ,
          MANSERNO   TYPE BAPI_ITOB-MANSERNO,
          EQUI_TEXT  TYPE BAPI_ITOB_STATUS-TEXT,
          USER_TEXT  TYPE BAPI_ITOB_STATUS-TEXT,
          UDATE      TYPE JCDS-UDATE,
          ALLOCATION TYPE API_KSSK-CLASS ,
          GWLDT      TYPE BGMKOBJ-GWLDT,
          GWLEN      TYPE BGMKOBJ-GWLEN,
          WAGET      TYPE BGMKOBJ-WAGET,
          GAERB      TYPE BGMKOBJ-GAERB,
        END OF TY_EQUIPAMENTO,

        BEGIN OF TY_SUBEQUIP,
          EQUNR TYPE EQUZ-EQUNR,
          OBJNR TYPE EQUI-OBJNR,
          DATBI TYPE EQUZ-DATBI,
          EQTYP TYPE EQUI-EQTYP,
          TIMBI TYPE EQUZ-TIMBI,
          ERDAT TYPE EQUI-ERDAT,
        END OF TY_SUBEQUIP,

        BEGIN OF TY_TL_ELIMINA,
          EQUNR            TYPE EQUZ-EQUNR,
          EQTYP            TYPE EQUI-EQTYP,
          TPLNR            TYPE IFLOT-TPLNR,
          QMNUM            TYPE QMEL-QMNUM,
          CODE_ELIMINADOR  TYPE QPCT-CODE,
          CODE_NOTIFICA    TYPE VIQMEL-QMNAM,
          CODE             TYPE QPCT-CODE,
          DESC_CAUSA       TYPE QPCT-KURZTEXT,
          DESC_ELIMINADOR  TYPE QPCT-KURZTEXT,
          DESC_MOTIVO      TYPE QPCT-KURZTEXT,
          CODE_MOTIVO      TYPE QPCT-CODE,
          CODE_MOTIVO_DESC TYPE QPCT-KURZTEXT,
          ARBPL            TYPE CRHD-ARBPL,
          DATA             TYPE SY-DATUM,
        END OF TY_TL_ELIMINA,

        BEGIN OF TY_IPML_SELECT,
          WARPL    LIKE VIMPLASTAT-WARPL,
          MPTYP    LIKE VIMPLASTAT-MPTYP,
          STRAT    LIKE VIMPLASTAT-STRAT,
          OBJNR    LIKE VIMPLASTAT-OBJNR,
        END OF TY_IPML_SELECT,

        BEGIN OF TY_QPCT,
          CODE     TYPE QPCT-CODE,
          KURZTEXT TYPE QPCT-KURZTEXT,
        END OF TY_QPCT,

        BEGIN OF TY_IFLOT,
          TPLNR TYPE IFLOT-TPLNR,
        END OF TY_IFLOT,

        BEGIN OF TY_CRHD,
          OBJID TYPE CRHD-OBJID,
          ARBPL TYPE CRHD-ARBPL,
        END OF TY_CRHD.

TYPES: BEGIN OF TY_ERROS.
*        INCLUDE TYPE bapiret2.
TYPES: MESSAGE TYPE BAPIRET2-MESSAGE,
       EQUNR   TYPE EQUI-EQUNR.
TYPES: END OF TY_ERROS.

* Objetos de tela
*----------------------------------------------------------------------*
DATA: " Tela de avisos
      OBJ_GRID      TYPE REF TO CL_GUI_ALV_GRID,
      OBJ_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GW_LAYOUT     TYPE LVC_S_LAYO,
      GT_FIELDCAT   TYPE LVC_T_FCAT,
      GW_FIELDCAT   TYPE LVC_S_FCAT,
      " Tela principal
      GW_TELA       TYPE TY_EQUIPAMENTO,
      GT_PONTOS     TYPE TABLE OF DIIMPT,
      GW_PONTOS     LIKE LINE OF GT_PONTOS,
      " Tela de eliminação
      GW_TL_ELIMINA TYPE TY_TL_ELIMINA.

* Váriaveis
*----------------------------------------------------------------------*
DATA: GV_TIPO       TYPE C,
      GV_EQUIP_INAT TYPE C,
      GW_MSG_AVISO  TYPE C LENGTH 250,
      GV_NUM_REG    TYPE C LENGTH 255,

      GT_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
      GT_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE,

      GT_BDCDATA    TYPE STANDARD TABLE OF BDCDATA,
      GW_BDCDATA    LIKE LINE OF GT_BDCDATA,

      GT_QPCT_CAUS  TYPE TABLE OF TY_QPCT,
      GT_QPCT_MOT   TYPE TABLE OF TY_QPCT,
      GT_QPCT_ELIM  TYPE TABLE OF TY_QPCT,
      GW_QPCT       TYPE TY_QPCT,

      GT_CRHD       TYPE TABLE OF TY_CRHD,

      GW_IFLOT      TYPE TY_IFLOT,
      GT_RETURN     TYPE TABLE OF BAPIRET2.

FIELD-SYMBOLS: <FS_OUTTAB>   TYPE ANY TABLE.

* Definições
*----------------------------------------------------------------------*
DEFINE F_PREENCHE_FIELDCAT.

  CLEAR GW_FIELDCAT.
  GW_FIELDCAT-COL_POS       = &1.
  GW_FIELDCAT-FIELDNAME     = &3.
  GW_FIELDCAT-TABNAME       = &2.
  GW_FIELDCAT-REF_TABLE     = &2.
  GW_FIELDCAT-REF_FIELD     = &3.
  GW_FIELDCAT-REPTEXT       = &4.
  GW_FIELDCAT-SCRTEXT_S     = &4.
  GW_FIELDCAT-SCRTEXT_M     = &4.
  GW_FIELDCAT-SCRTEXT_L     = &4.
  GW_FIELDCAT-DO_SUM        = &5.
  GW_FIELDCAT-KEY           = &6.
  GW_FIELDCAT-INTTYPE       = &7.
  APPEND GW_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.

DEFINE F_PREENCHE_SHDB.
  CLEAR GW_BDCDATA.
  GW_BDCDATA-PROGRAM   = &1.
  GW_BDCDATA-DYNPRO    = &2.
  GW_BDCDATA-DYNBEGIN  = &3.
  GW_BDCDATA-FNAM      = &4.
  GW_BDCDATA-FVAL      = &5.
  APPEND GW_BDCDATA TO GT_BDCDATA.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS LCL_EQUIPAMENTO DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EQUIPAMENTO DEFINITION.
  PUBLIC SECTION.
**  Variáveis públicas
    DATA: GT_STATUS_EQUIP TYPE TABLE OF BAPI_ITOB_STATUS,
          GT_STATUS_USER  TYPE TABLE OF BAPI_ITOB_STATUS,
          GT_API_KSSK     TYPE TABLE OF API_KSSK,
          GT_RETURN       TYPE TABLE OF BAPIRET2,
          GT_NOTIFICATION TYPE TABLE OF BAPI2080_1,
          GT_RESULT       TYPE TABLE OF BAPI_ALM_ORDER_LISTHEAD_RESULT,
          GT_DIIMPT       TYPE TABLE OF DIIMPT,
          GT_SUBEQUIP     TYPE TABLE OF TY_SUBEQUIP,
          GT_SELC_PLANO   TYPE TABLE OF TY_IPML_SELECT,
          GT_JCDS         TYPE TABLE OF JCDS,

          GW_STATUS_EQUIP TYPE BAPI_ITOB_STATUS,
          GW_RETURN_NOTA  TYPE BAPIRETURN,
          GW_RETURN       TYPE BAPIRET2,
          GW_ITOB         TYPE BAPI_ITOB,
          GW_ITOB_ESPEC   TYPE BAPI_ITOB_EQ_ONLY,
          GW_FLEET        TYPE BAPI_FLEET,
          GW_BGMKOBJ      TYPE BGMKOBJ,
          GW_IFLOTX       TYPE IFLOTX,
          GW_EQKT         TYPE EQKT,
          GW_STATUS_USER  TYPE BAPI_ITOB_STATUS,
          GW_EQUIPAMENTO  TYPE TY_EQUIPAMENTO,
          GW_JCDS         TYPE JCDS.

**  Métodos públicos
    METHODS CONSULTAR_EQUIPAMENTO    IMPORTING P_EQUNR           TYPE EQUI-EQUNR
                                               P_DATA            TYPE SY-DATUM DEFAULT SY-DATUM
                                               P_HORA            TYPE SY-UZEIT DEFAULT SY-UZEIT
                                     CHANGING  C_INATIVO         TYPE C.

    METHODS CONSULTAR_NOTAS_PEND     IMPORTING P_EQUNR           TYPE EQUI-EQUNR.

    METHODS CONSULTAR_ORDENS_PEND    IMPORTING P_EQUNR           TYPE EQUI-EQUNR.

    METHODS CONSULTAR_PONTOS_EQUIP   IMPORTING P_EQUNR           TYPE EQUI-EQUNR.

    METHODS DESATIVAR_PONTOS         IMPORTING P_POINT           TYPE DIIMPT-POINT
                                               P_EQUNR           TYPE EQUI-EQUNR .

    METHODS CONSULTAR_SUBEQUIPAMENTO IMPORTING P_EQUI_SUP        TYPE EQUI-EQUNR.

    METHODS MOVIMENTAR_EQUIPAMENTO   IMPORTING P_LOCAL           TYPE IFLOT-TPLNR
                                               P_EQUNR           TYPE EQUI-EQUNR.

    METHODS APONTAR_SULCO_PNEUS      IMPORTING P_EQUNR           TYPE EQUI-EQUNR.

    METHODS CONSULTAR_PLANOS         IMPORTING P_EQUNR           TYPE EQUI-EQUNR.

    METHODS DESATIVAR_PLANOS         IMPORTING P_OBJNR           TYPE VIMPLASTAT-OBJNR
                                               P_EQUNR           TYPE EQUI-EQUNR.

    METHODS ELIMINAR_EQUIPAMENTO     IMPORTING P_OBJNR           TYPE EQUI-OBJNR
                                               P_EQUNR           TYPE EQUI-EQUNR
                                               P_CODE            TYPE QPCT-CODE
                                               P_CODE_MOTIVO     TYPE QPCT-CODE
                                               P_CODE_ELIMINADOR TYPE QPCT-CODE
                                               P_CODE_NOTIFICA   TYPE VIQMEL-QMNAM
                                     EXPORTING E_VIQMEL          TYPE VIQMEL.

ENDCLASS.                    "lcl_equipamento DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_equipamento IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EQUIPAMENTO IMPLEMENTATION.
  METHOD CONSULTAR_EQUIPAMENTO.
    DATA: LV_EQUIPAMENTO TYPE EQUI-EQUNR,
          LV_OBJETO      TYPE KSSK-OBJEK.

    IF P_EQUNR IS INITIAL.
      MESSAGE TEXT-001 TYPE 'S'.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = P_EQUNR
        IMPORTING
          OUTPUT = LV_EQUIPAMENTO.

** BAPI  buscar dados gerais do equipamento
"*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      CALL FUNCTION 'BAPI_EQUI_GETDETAIL'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          EQUIPMENT         = LV_EQUIPAMENTO
        IMPORTING
          DATA_GENERAL_EXP  = GW_ITOB
          DATA_SPECIFIC_EXP = GW_ITOB_ESPEC
          DATA_FLEET_EXP    = GW_FLEET
          RETURN            = GW_RETURN.

      IF GW_RETURN IS INITIAL.
** BAPI buscar dados de status equipamento
"*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
        CALL FUNCTION 'BAPI_EQUI_GETSTATUS'"#EC CI_USAGE_OK[2438131]
          EXPORTING
            EQUIPMENT     = LV_EQUIPAMENTO
          IMPORTING
            RETURN        = GW_RETURN
          TABLES
            SYSTEM_STATUS = GT_STATUS_EQUIP
            USER_STATUS   = GT_STATUS_USER.

**  Descrição de local
        SELECT SINGLE *
          FROM IFLOTX
          INTO GW_IFLOTX
          WHERE TPLNR = GW_ITOB_ESPEC-READ_FLOC
           AND  SPRAS = SY-LANGU.

** Descrição de equipamento
        SELECT SINGLE *
          FROM EQKT
          INTO GW_EQKT
          WHERE EQUNR = GW_ITOB_ESPEC-READ_SUPEQ.

** Classe padrão
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = P_EQUNR
          IMPORTING
            OUTPUT = LV_OBJETO.

        CALL FUNCTION 'CLAP_DDB_GET_CLASSIFICATION'
          EXPORTING
            OBJECT                 = LV_OBJETO
            OBTAB                  = 'EQUI'
            SPRAS                  = SY-LANGU
            CLASSTYPE              = '002'
          TABLES
            ALLOCATIONS            = GT_API_KSSK
          EXCEPTIONS
            NO_ALLOCATION          = 1
            SET_AENNR              = 2
            CHANGE_NR_NOT_EXIST    = 3
            DATE_IN_PAST           = 4
            ERROR_CLASS            = 5
            ERROR_DATE_RESTRICTION = 6
            ERROR_STATUS           = 7.

** Dados de garantia
        SELECT SINGLE *
          FROM BGMKOBJ
          INTO GW_BGMKOBJ
          WHERE J_OBJNR = GW_ITOB-READ_OBJNR
           AND  GAART   = '2'.

        GW_EQUIPAMENTO-EQUNR      = P_EQUNR.
        GW_EQUIPAMENTO-HEQUI      = GW_ITOB_ESPEC-READ_SUPEQ.
        GW_EQUIPAMENTO-HEQNR      = GW_ITOB_ESPEC-INST_POS.
        GW_EQUIPAMENTO-IWERK      = GW_ITOB-MAINTPLANT.
        GW_EQUIPAMENTO-GEWRK      = GW_ITOB-PLANPLANT.
        GW_EQUIPAMENTO-EQTYP      = GW_ITOB_ESPEC-EQUICATGRY.
        GW_EQUIPAMENTO-EQART      = GW_ITOB-OBJECTTYPE.
        GW_EQUIPAMENTO-HERST      = GW_ITOB-MANFACTURE.
        GW_EQUIPAMENTO-TYPBZ      = GW_ITOB-MANMODEL.
        GW_EQUIPAMENTO-OBJNR      = GW_ITOB-READ_OBJNR.
        GW_EQUIPAMENTO-DATAB      = GW_ITOB-START_FROM.
        GW_EQUIPAMENTO-NUM_AXLE   = GW_FLEET-NUM_AXLE.
        GW_EQUIPAMENTO-READ_FLOC  = GW_ITOB_ESPEC-READ_FLOC.
        GW_EQUIPAMENTO-TECHID     = GW_ITOB_ESPEC-TECHID.
        GW_EQUIPAMENTO-AUTHGRP    = GW_ITOB-AUTHGRP.
        GW_EQUIPAMENTO-CONSTTYPE  = GW_ITOB-CONSTTYPE.
        GW_EQUIPAMENTO-INVENTORY  = GW_ITOB-INVENTORY.
        GW_EQUIPAMENTO-DESCRIPT   = GW_ITOB-DESCRIPT.
        GW_EQUIPAMENTO-ABCINDIC   = GW_ITOB-ABCINDIC.
        GW_EQUIPAMENTO-ACQUISVAL  = GW_ITOB-ACQUISVAL.
        GW_EQUIPAMENTO-CURRENCY   = GW_ITOB-CURRENCY.
        GW_EQUIPAMENTO-OBJ_WEIGHT = GW_ITOB-OBJ_WEIGHT.
        GW_EQUIPAMENTO-UNIT_OF_WT = GW_ITOB-UNIT_OF_WT.
        GW_EQUIPAMENTO-CONSTYEAR  = GW_ITOB-CONSTYEAR.
        GW_EQUIPAMENTO-CONSTMONTH = GW_ITOB-CONSTMONTH.
        GW_EQUIPAMENTO-OBJ_SIZE   = GW_ITOB-OBJ_SIZE.
        GW_EQUIPAMENTO-MANFACTURE = GW_ITOB-MANFACTURE.
        GW_EQUIPAMENTO-MANSERNO   = GW_ITOB-MANSERNO.

        CLEAR C_INATIVO.
        SELECT *
          FROM JCDS
          INTO TABLE GT_JCDS
          FOR ALL ENTRIES IN GT_STATUS_EQUIP
          WHERE OBJNR = GW_ITOB-READ_OBJNR
           AND  STAT  = GT_STATUS_EQUIP-STATUS.

        SORT GT_JCDS BY CHGNR DESCENDING.

** Checa se o equipamento foi eliminado
        READ TABLE GT_STATUS_EQUIP INTO GW_STATUS_EQUIP WITH KEY STATUS = 'I0076'.
        IF SY-SUBRC IS INITIAL.
          READ TABLE GT_JCDS INTO GW_JCDS WITH KEY STAT = 'I0076'.
          GW_EQUIPAMENTO-UDATE = GW_JCDS-UDATE.
        ELSE.
** Checa se equipamento está inativo para evitar que o mesmo seja eliminado
          READ TABLE GT_STATUS_EQUIP INTO GW_STATUS_EQUIP WITH KEY STATUS = 'I0320'.
          IF SY-SUBRC IS INITIAL.
            READ TABLE GT_JCDS INTO GW_JCDS WITH KEY STAT = 'I0320'.
            GW_EQUIPAMENTO-UDATE = GW_JCDS-UDATE.
            C_INATIVO = 'X'.
          ELSE.
            READ TABLE GT_STATUS_EQUIP INTO GW_STATUS_EQUIP INDEX 1.
          ENDIF.
        ENDIF.

        GW_EQUIPAMENTO-EQUI_TEXT  = GW_STATUS_EQUIP-TEXT.
        GW_EQUIPAMENTO-USER_TEXT  = GW_STATUS_USER-TEXT.
        GW_EQUIPAMENTO-PLTXT      = GW_IFLOTX-PLTXT.
        GW_EQUIPAMENTO-EQKTX      = GW_EQKT-EQKTX.
*        GW_EQUIPAMENTO-ALLOCATION = GW_API_KSSK-CLASS.
        GW_EQUIPAMENTO-GWLDT      = GW_BGMKOBJ-GWLDT.
        GW_EQUIPAMENTO-GWLEN      = GW_BGMKOBJ-GWLEN.
        GW_EQUIPAMENTO-WAGET      = GW_BGMKOBJ-WAGET.
        GW_EQUIPAMENTO-GAERB      = GW_BGMKOBJ-GAERB.
      ELSE.
        MESSAGE TEXT-002 TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "consultar_equipamento

  METHOD CONSULTAR_NOTAS_PEND.
    DATA: LV_EQUIPAMENTO TYPE EQUI-EQUNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_EQUNR
      IMPORTING
        OUTPUT = LV_EQUIPAMENTO.

    CALL FUNCTION 'BAPI_ALM_NOTIF_LIST_EQUI'
      EXPORTING
        EQUIPMENT         = LV_EQUIPAMENTO
        NOTIFICATION_DATE = '01011900'
      IMPORTING
        RETURN            = GW_RETURN_NOTA
      TABLES
        NOTIFICATION      = GT_NOTIFICATION
      EXCEPTIONS
        OTHERS            = 1.

    DELETE GT_NOTIFICATION WHERE S_STATUS CS 'MSEN'.

  ENDMETHOD.                    "CONSULTAR_NOTAS_PEND

  METHOD CONSULTAR_ORDENS_PEND.
    DATA: TL_RANGES TYPE TABLE OF BAPI_ALM_ORDER_LISTHEAD_RANGES,
          WL_RANGES LIKE LINE OF TL_RANGES.

    CLEAR WL_RANGES.
    WL_RANGES-FIELD_NAME = 'SHOW_DOCS_WITH_FROM_DATE'.
    WL_RANGES-LOW_VALUE  = '10000101'.
    APPEND WL_RANGES TO TL_RANGES.

    CLEAR WL_RANGES.
    WL_RANGES-FIELD_NAME = 'OPTIONS_FOR_EQUIPMENT'.
    WL_RANGES-LOW_VALUE  = P_EQUNR.
    APPEND WL_RANGES TO TL_RANGES.

    CLEAR WL_RANGES.
    WL_RANGES-FIELD_NAME = 'OPTIONS_FOR_STATUS_EXCLUSIVE'.
    WL_RANGES-SIGN       = 'I'.
    WL_RANGES-OPTION     = 'EQ'.
    WL_RANGES-LOW_VALUE  = 'I0045'.
    WL_RANGES-HIGH_VALUE = 'I0046'.
    APPEND WL_RANGES TO TL_RANGES.

"*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_ALM_ORDERHEAD_GET_LIST'"#EC CI_USAGE_OK[2438131]
      TABLES                                   "#EC CI_USAGE_OK[2669857]
        IT_RANGES = TL_RANGES
        ET_RESULT = GT_RESULT
        RETURN    = GT_RETURN
      EXCEPTIONS
        OTHERS    = 1.

  ENDMETHOD.                    "CONSULTAR_ORDENS_PEND

  METHOD CONSULTAR_PONTOS_EQUIP.
    DATA: LV_EQUIPAMENTO TYPE EQUI-EQUNR.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_EQUNR
      IMPORTING
        OUTPUT = LV_EQUIPAMENTO.

    FREE: GT_DIIMPT, GT_RETURN.

    CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
      EXPORTING
        I_EQUNR    = LV_EQUIPAMENTO
      TABLES
        ET_RETURN1 = GT_RETURN
        ET_DIIMPT  = GT_DIIMPT.

  ENDMETHOD.                    "consultar_pontos_equip

  METHOD DESATIVAR_PONTOS.
    DATA: TL_MSG  TYPE TABLE OF BDCMSGCOLL,
          WL_MSG  TYPE BDCMSGCOLL,

          TL_T100 TYPE TABLE OF T100,
          WL_T100 TYPE T100.

    DATA: LV_MODE   TYPE C,
          LV_UPDATE TYPE C,
          LV_MSG    TYPE BAPI_MSG.

    LV_MODE   = 'E'.
    LV_UPDATE = 'S'.

    FREE GT_BDCDATA.

    F_PREENCHE_SHDB:
      ''          ''      'T'   ''            'IK02'      ,
      'SAPLIMR0'  '1110'  'X'   ''            ''          ,
      ''          ''      ''    'BDC_CURSOR'  'IMPT-POINT',
      ''          ''      ''    'BDC_OKCODE'  '/00'       ,
      ''          ''      ''    'IMPT-POINT'  P_POINT     ,
      ''          ''      ''    'BDC_SUBSCR'  'SAPLIMR4                                7500MPOBJ',
      'SAPLIMR0'  '5110'  'X'   ''            '',
      ''          ''      ''    'BDC_CURSOR'  'IMPT-PSORT',
      ''          ''      ''    'BDC_OKCODE'  '=ACT2'     ,
      'SAPLIMR0'  '5110'  'X'   ''            ''          ,
      ''          ''      ''    'BDC_CURSOR'  'IMPT-PSORT',
      ''          ''      ''    'BDC_OKCODE'  '=BU'       .

    CALL TRANSACTION 'IK02' USING GT_BDCDATA
      MODE LV_MODE
      MESSAGES INTO TL_MSG
      UPDATE LV_UPDATE.

    SELECT *
      INTO TABLE TL_T100
      FROM T100
      FOR ALL ENTRIES IN TL_MSG
     WHERE ARBGB EQ TL_MSG-MSGID
       AND MSGNR EQ TL_MSG-MSGNR
       AND SPRSL EQ SY-LANGU.

    READ TABLE TL_T100 INTO WL_T100 WITH KEY SPRSL = SY-LANGU
                                             ARBGB = 'IR'
                                             MSGNR = '121'.
    IF SY-SUBRC IS INITIAL.
      LV_MSG = WL_T100-TEXT.
      REPLACE ALL OCCURRENCES OF '&' IN LV_MSG WITH P_POINT.
      CONCATENATE 'Equip.:' P_EQUNR '>>' LV_MSG INTO LV_MSG SEPARATED BY SPACE.

      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        EXPORTING
          I_TP_MSG   = 'S'
          I_MENSAGEM = LV_MSG
          I_TCODE    = SY-TCODE.

    ELSE.
      LOOP AT TL_T100 INTO WL_T100.
        READ TABLE TL_MSG INTO WL_MSG WITH KEY MSGID = WL_T100-ARBGB
                                               MSGNR = WL_T100-MSGNR.
        LV_MSG = WL_T100-TEXT.
        REPLACE '&' IN LV_MSG WITH WL_MSG-MSGV1.
        CONCATENATE 'Equip.:' P_EQUNR '>>' LV_MSG INTO LV_MSG SEPARATED BY SPACE.

        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          EXPORTING
            I_TP_MSG   = 'S'
            I_MENSAGEM = LV_MSG
            I_TCODE    = SY-TCODE.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "DESATIVAR_PONTOS

  METHOD CONSULTAR_SUBEQUIPAMENTO.
    DATA: TL_DEPENDENTES TYPE TABLE OF RIHEQUI,
          LV_EQUIPAMENTO TYPE EQUI-EQUNR,
          WL_DEPENDENTES TYPE RIHEQUI,
          WL_SUBEQUI     TYPE TY_SUBEQUIP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_EQUI_SUP
      IMPORTING
        OUTPUT = LV_EQUIPAMENTO.

**  Verifica se existe equipamento inferior
    CALL FUNCTION 'EQUI_HIERARCHY_READ'
      EXPORTING
        EQUIPMENT  = LV_EQUIPAMENTO
        LEVEL_DOWN = '01'
      TABLES
        HIER_TAB   = TL_DEPENDENTES.

**  Remove equipamento superior da lista
    DELETE TL_DEPENDENTES WHERE EQUNR = LV_EQUIPAMENTO.

    LOOP AT TL_DEPENDENTES INTO WL_DEPENDENTES.
      CLEAR WL_SUBEQUI.
      MOVE-CORRESPONDING WL_DEPENDENTES TO WL_SUBEQUI.
      APPEND WL_SUBEQUI TO GT_SUBEQUIP.
    ENDLOOP.

  ENDMETHOD.                    "buscar_subequipamento

  METHOD MOVIMENTAR_EQUIPAMENTO.
    DATA: WL_RETURN      TYPE BAPIRET2,
          WL_INST        TYPE BAPI_ITOB_EQ_INSTALL_EXT,
          LV_EQUIPAMENTO TYPE EQUI-EQUNR,
          LV_MSG         TYPE BAPI_MSG.

**  Desmontando equipamento
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_EQUNR
      IMPORTING
        OUTPUT = LV_EQUIPAMENTO.

"*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_EQUI_DISMANTLE'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        EQUIPMENT = LV_EQUIPAMENTO
      IMPORTING
        RETURN    = WL_RETURN.

    IF WL_RETURN IS INITIAL
    OR WL_RETURN-NUMBER = '007'.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      WL_INST-FUNCLOC   = P_LOCAL.
      WL_INST-INST_DATE = SY-DATUM.
      WL_INST-INST_TIME = SY-UZEIT.
      ADD 100 TO WL_INST-INST_TIME.

      WAIT UP TO 5 SECONDS.

      CLEAR WL_RETURN.

**  Montar equipamento em sucata
"*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      CALL FUNCTION 'BAPI_EQUI_INSTALL'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          EQUIPMENT    = LV_EQUIPAMENTO
          DATA_INSTALL = WL_INST
        IMPORTING
          RETURN       = WL_RETURN.

      IF WL_RETURN IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        CONCATENATE 'Equip.: ' P_EQUNR ' >> Montado em ' P_LOCAL INTO LV_MSG SEPARATED BY SPACE.

        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          EXPORTING
            I_TP_MSG   = 'S'
            I_MENSAGEM = LV_MSG
            I_TCODE    = SY-TCODE.

      ELSE.
        FREE GT_RETURN.
        APPEND WL_RETURN TO GT_RETURN.

        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          TABLES
            T_RETURN = GT_RETURN.

      ENDIF.
    ELSE.
      FREE GT_RETURN.
      APPEND WL_RETURN TO GT_RETURN.

      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        TABLES
          T_RETURN = GT_RETURN.

    ENDIF.

  ENDMETHOD.                    "movimentar_equipamento

  METHOD APONTAR_SULCO_PNEUS.
    DATA: TL_MSG TYPE TABLE OF BDCMSGCOLL,
          WL_MSG TYPE BDCMSGCOLL,

          TL_T100 TYPE TABLE OF T100,
          WL_T100 TYPE T100.

    DATA: LV_MODE   TYPE C,
          LV_UPDATE TYPE C,
          LV_MSG    TYPE BAPI_MSG.

    LV_MODE   = 'E'.
    LV_UPDATE = 'A'.

    FREE GT_BDCDATA.

    F_PREENCHE_SHDB:
*        ''                  ''      'T' 'ZPM0006'     'BS EA     F',
        'ZFTPM_MEDSULC'     '0100'  'X' ''            ''           ,
        ''                  ''      ''  'BDC_CURSOR'  'V_NRSAP'    ,
        ''                  ''      ''  'BDC_OKCODE'  '=ENTER'     ,
        ''                  ''      ''  'V_NRSAP'     P_EQUNR      ,
        'ZFTPM_MEDSULC'     '0100'  'X' ''            ''           ,
        ''                 	''      '' 	'BDC_CURSOR'  'GV_DATA'    ,
        ''                  ''      ''  'BDC_OKCODE'  '=BTN_APONT' ,
        ''                  ''      ''  'V_NRSAP'     P_EQUNR.

    CALL TRANSACTION 'ZPM0006' USING GT_BDCDATA
      MODE LV_MODE
      MESSAGES INTO TL_MSG
      UPDATE LV_UPDATE.

    READ TABLE TL_T100 INTO WL_T100 WITH KEY SPRSL = SY-LANGU
                                             ARBGB = 'IR'
                                             MSGNR = '126'.
    IF SY-SUBRC IS INITIAL.
      CONCATENATE 'Equip.:' P_EQUNR '>> Medição de sulcos executada.' INTO LV_MSG SEPARATED BY SPACE.

      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        EXPORTING
          I_TP_MSG   = 'S'
          I_MENSAGEM = LV_MSG
          I_TCODE    = SY-TCODE.

    ELSE.
      LOOP AT TL_MSG INTO WL_MSG.
        CONCATENATE 'Equip.:' P_EQUNR '>> Medição de sulcos: ' WL_MSG-MSGV1 INTO LV_MSG SEPARATED BY SPACE.

        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          EXPORTING
            I_TP_MSG   = WL_MSG-MSGTYP
            I_MENSAGEM = LV_MSG
            I_TCODE    = SY-TCODE.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.                    "apontar_sulco_pneus

  METHOD CONSULTAR_PLANOS.
    DATA: LV_EQUIPAMENTO TYPE EQUI-EQUNR.

    DATA: TL_EQUNR TYPE RANGE OF EQUI-EQUNR,
          WL_EQUNR LIKE LINE OF TL_EQUNR,
          TL_WARPL TYPE RANGE OF VIMPLA-WARPL,
          TL_MPTYP TYPE RANGE OF VIMPLA-MPTYP,
          TL_STRAT TYPE RANGE OF VIMPLA-STRAT,
          TL_TPLNR TYPE RANGE OF VIMPLA-TPLNR,
          TL_KDAUF TYPE RANGE OF VIMPLA-KDAUF,
          TL_KDPOS TYPE RANGE OF VIMPLA-KDPOS,
          TL_PROT  TYPE RANGE OF SPROT_U.

    FREE GT_SELC_PLANO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_EQUNR
      IMPORTING
        OUTPUT = LV_EQUIPAMENTO.

    WL_EQUNR-LOW    = LV_EQUIPAMENTO.
    WL_EQUNR-SIGN   = 'I'.
    WL_EQUNR-OPTION = 'EQ'.
    APPEND WL_EQUNR TO TL_EQUNR.

    CALL FUNCTION 'MAINTENANCE_PLAN_SELECTION'
      TABLES
        I_EQUNR = TL_EQUNR
        I_SELC  = GT_SELC_PLANO
        I_KDAUF = TL_KDAUF
        I_WARPL = TL_WARPL
        I_MPTYP = TL_MPTYP
        I_STRAT = TL_STRAT
        I_TPLNR = TL_TPLNR
        I_KDPOS = TL_KDPOS
        I_PROT  = TL_PROT.

  ENDMETHOD.                    "consultar_planos

  METHOD DESATIVAR_PLANOS.
    DATA: LV_MSG      TYPE C LENGTH 255,
          LV_MSG_BAPI TYPE BAPI_MSG.

    CALL FUNCTION 'STATUS_CHANGE_FOR_ACTIVITY'
      EXPORTING
        ACTIVITY_ALLOWED     = 'X'
*        CHECK_ONLY           = 'X'
        NO_CHECK             = ''
        OBJNR                = P_OBJNR
        VRGNG                = 'LVMS'
      EXCEPTIONS
        ACTIVITY_NOT_ALLOWED = 1
        OBJECT_NOT_FOUND     = 2
        STATUS_INCONSISTENT  = 3
        STATUS_NOT_ALLOWED   = 4
        WRONG_INPUT          = 5
        WARNING_OCCURED      = 6.

    LV_MSG = P_OBJNR+3(19).
    SHIFT LV_MSG LEFT DELETING LEADING '0'.

    IF SY-SUBRC IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      CONCATENATE 'Equip.:' P_EQUNR ' ->''Plano desativado:"' LV_MSG '"' INTO LV_MSG_BAPI SEPARATED BY SPACE.

      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        EXPORTING
          I_TP_MSG   = 'S'
          I_MENSAGEM = LV_MSG_BAPI
          I_TCODE    = SY-TCODE.

    ELSE.
      CASE SY-SUBRC.
        WHEN 1.
          CONCATENATE 'Equip.:' P_EQUNR ' -> ''Plano não liberado:"' LV_MSG '"' INTO LV_MSG_BAPI SEPARATED BY SPACE.
        WHEN 2.
          CONCATENATE 'Equip.:' P_EQUNR ' -> ''Plano não encontrado:"' LV_MSG '"' INTO LV_MSG_BAPI SEPARATED BY SPACE.
        WHEN 3.
          CONCATENATE 'Equip.:' P_EQUNR ' -> ''Plano:"' LV_MSG '" com status inconsistente' INTO LV_MSG_BAPI SEPARATED BY SPACE.
        WHEN 4.
          CONCATENATE 'Equip.:' P_EQUNR ' -> ''Plano:"' LV_MSG '" não liberado' INTO LV_MSG_BAPI SEPARATED BY SPACE.
        WHEN 5.
          CONCATENATE 'Equip.:' P_EQUNR ' -> ''Dados de plano:"' LV_MSG '" incorretos' INTO LV_MSG_BAPI SEPARATED BY SPACE.
        WHEN 6.
          CONCATENATE 'Equip.:' P_EQUNR ' -> ''Erro desativando plano:"' LV_MSG '"' INTO LV_MSG_BAPI SEPARATED BY SPACE.
        WHEN OTHERS.
          CONCATENATE 'Equip.:' P_EQUNR ' -> ''Erro não identificado desativando plano:"' LV_MSG '"' INTO LV_MSG_BAPI SEPARATED BY SPACE.
      ENDCASE.

      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        EXPORTING
          I_TP_MSG   = 'E'
          I_MENSAGEM = LV_MSG_BAPI
          I_TCODE    = SY-TCODE.

    ENDIF.

  ENDMETHOD.                    "DESATIVAR_PLANO

  METHOD ELIMINAR_EQUIPAMENTO.
    DATA: LV_DATA         TYPE SY-DATUM,
          LV_HORA         TYPE SY-UZEIT,
          LV_TEXT         TYPE C LENGTH 128,
          LV_MSG          TYPE BAPI_MSG,
          LV_EQUIPAMENTO  TYPE C LENGTH 128,

          WL_NOTIF_HEADER TYPE ALM_ME_NOTIF_HEADER,
          LV_NOTIF_TP     TYPE ALM_ME_NOTIF_HEADER-NOTIF_TYPE,

          TL_NOTIF_ITEM   TYPE TABLE OF ALM_ME_NOTIF_ITEM,
          WL_NOTIF_ITEM   TYPE ALM_ME_NOTIF_ITEM,

          TL_NOTIF_CAUSE  TYPE TABLE OF ALM_ME_NOTIF_CAUSE,
          WL_NOTIF_CAUSE  TYPE ALM_ME_NOTIF_CAUSE,

          WL_NOT_EXP      TYPE BAPI2080_NOTHDRE,

          TL_RETURN       TYPE TABLE OF BAPIRET2.

    LV_DATA = SY-DATUM.
    LV_HORA = SY-UZEIT.

** Criando nota de eliminação
    MOVE P_EQUNR TO WL_NOTIF_HEADER-EQUIPMENT.
    MOVE P_EQUNR TO LV_EQUIPAMENTO.

    WL_NOTIF_HEADER-REPORTEDBY = P_CODE_NOTIFICA.
    SHIFT LV_EQUIPAMENTO LEFT DELETING LEADING '0'.
    CONCATENATE 'Eliminação de equipamento ' LV_EQUIPAMENTO INTO LV_TEXT SEPARATED BY SPACE.
    WL_NOTIF_HEADER-SHORT_TEXT = LV_TEXT(40).
    WL_NOTIF_HEADER-CAT_TYPE   = 'D'.
    WL_NOTIF_HEADER-CODE_GROUP = 'FPN-0010'.
    WL_NOTIF_HEADER-CODING     = P_CODE_ELIMINADOR.

    LV_NOTIF_TP = 'Z4'.

    WL_NOTIF_ITEM-ITEM_KEY     = '0001'.
    WL_NOTIF_ITEM-ITEM_SORT_NO = '0001'.
    APPEND WL_NOTIF_ITEM TO TL_NOTIF_ITEM.

    WL_NOTIF_CAUSE-ITEM_KEY      = '0001'.
    WL_NOTIF_CAUSE-CAUSE_SORT_NO = '0001'.
    WL_NOTIF_CAUSE-CAUSE_CAT_TYP = '5'.
    WL_NOTIF_CAUSE-CAUSE_CODEGRP = 'FPN-0010'.
    WL_NOTIF_CAUSE-CAUSE_CODE    = P_CODE.
    WL_NOTIF_CAUSE-ITEM_SORT_NO  = '0001'.
    APPEND WL_NOTIF_CAUSE TO TL_NOTIF_CAUSE.

    WL_NOTIF_CAUSE-ITEM_KEY      = '0001'.
    WL_NOTIF_CAUSE-CAUSE_SORT_NO = '0002'.
    WL_NOTIF_CAUSE-CAUSE_CAT_TYP = '5'.
    WL_NOTIF_CAUSE-CAUSE_CODEGRP = 'FPN-0020'.
    WL_NOTIF_CAUSE-CAUSE_CODE    = P_CODE_MOTIVO.
    WL_NOTIF_CAUSE-ITEM_SORT_NO  = '0001'.
    APPEND WL_NOTIF_CAUSE TO TL_NOTIF_CAUSE.

"*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'ALM_ME_NOTIFICATION_CREATE'"#EC CI_USAGE_OK[2438006]
      EXPORTING
        NOTIFICATION_HEADER         = WL_NOTIF_HEADER
        NOTIF_TYPE                  = LV_NOTIF_TP
      IMPORTING
        NOTIFICATION_EXPORT         = WL_NOT_EXP
      TABLES
        NOTIFICATION_ITEM           = TL_NOTIF_ITEM
        NOTIFICATION_CAUSE          = TL_NOTIF_CAUSE
        RETURN                      = TL_RETURN
      EXCEPTIONS
        ERROR_IN_INPUT_DATA         = 1
        NOTIFICATION_ALREADY_EXISTS = 2
        TASK_NOT_REL_OR_COMPL       = 3
        USER_STATUS_NOT_CHANGED     = 4
        OTHERS                      = 5.


    IF TL_RETURN IS NOT INITIAL.
      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        TABLES
          T_RETURN = TL_RETURN.
    ENDIF.

    CALL FUNCTION 'IQS4_SAVE_NOTIFICATION'
      EXPORTING
        I_QMNUM            = WL_NOT_EXP-NOTIF_NO
        I_COMMIT           = ABAP_TRUE
        I_WAIT             = ABAP_TRUE
        I_REFRESH_COMPLETE = ABAP_TRUE
      IMPORTING
        E_VIQMEL           = E_VIQMEL
      TABLES
        RETURN             = TL_RETURN.

    IF TL_RETURN IS NOT INITIAL.
      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        TABLES
          T_RETURN = TL_RETURN.
    ENDIF.

** Eliminando o equipamento
    IF E_VIQMEL IS NOT INITIAL.
      CALL FUNCTION 'STATUS_CHANGE_FOR_ACTIVITY'
        EXPORTING
          ACTIVITY_ALLOWED     = 'X'
*        CHECK_ONLY           = 'X'
          NO_CHECK             = ''
          OBJNR                = P_OBJNR
          VRGNG                = 'LVMS'
*        DATE                 = lv_data
        EXCEPTIONS
          ACTIVITY_NOT_ALLOWED = 1
          OBJECT_NOT_FOUND     = 2
          STATUS_INCONSISTENT  = 3
          STATUS_NOT_ALLOWED   = 4
          WRONG_INPUT          = 5
          WARNING_OCCURED      = 6.

      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        CONCATENATE 'Equip.:' P_EQUNR ' -> Nota eliminação:"' E_VIQMEL-QMNUM '"' INTO LV_MSG SEPARATED BY SPACE.

        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          EXPORTING
            I_TP_MSG   = 'S'
            I_MENSAGEM = LV_MSG
            I_TCODE    = SY-TCODE.

      ELSE.

        CASE SY-SUBRC.
          WHEN 1.
            CONCATENATE 'Equip.:' P_EQUNR ' -> Equipamento não liberado.' INTO LV_MSG SEPARATED BY SPACE.
          WHEN 2.
            CONCATENATE 'Equip.:' P_EQUNR ' -> Equipamento não encontrado' INTO LV_MSG SEPARATED BY SPACE.
          WHEN 3.
            CONCATENATE 'Equip.:' P_EQUNR ' -> Equipamento com status inconsistente' INTO LV_MSG SEPARATED BY SPACE.
          WHEN 4.
            CONCATENATE 'Equip.:' P_EQUNR ' -> Status não liberado' INTO LV_MSG SEPARATED BY SPACE.
          WHEN 5.
            CONCATENATE 'Equip.:' P_EQUNR ' -> Dados de equipamento incorretos' INTO LV_MSG SEPARATED BY SPACE.
          WHEN 6.
            CONCATENATE 'Equip.:' P_EQUNR ' -> Erro desativando equipamento' INTO LV_MSG SEPARATED BY SPACE.
          WHEN OTHERS.
            CONCATENATE 'Equip.:' P_EQUNR ' -> Erro não identificado desativando equipamento' INTO LV_MSG SEPARATED BY SPACE.
        ENDCASE.

        CALL FUNCTION 'Z_GRAVA_LOG_PM'
          EXPORTING
            I_TP_MSG   = 'E'
            I_MENSAGEM = LV_MSG
            I_TCODE    = SY-TCODE.

      ENDIF.

    ELSE.
      CONCATENATE 'Equip.:' P_EQUNR ' -> Nota eliminação não gerada' INTO LV_MSG SEPARATED BY SPACE.

      CALL FUNCTION 'Z_GRAVA_LOG_PM'
        EXPORTING
          I_TP_MSG   = 'S'
          I_MENSAGEM = LV_MSG
          I_TCODE    = SY-TCODE.

    ENDIF.

  ENDMETHOD.                    "eliminar_equipamento

ENDCLASS.                    "lcl_equipamento IMPLEMENTATION


* Objetos
*----------------------------------------------------------------------*
DATA: OBJ_EQUIPAMENTO TYPE REF TO LCL_EQUIPAMENTO.
DATA: OBJ_SUB_EQUIPAMENTO TYPE REF TO LCL_EQUIPAMENTO.
