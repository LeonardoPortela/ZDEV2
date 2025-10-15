*&-----------------------------------------------------------------------------*
*& CLASS ZBAPIS DEFINITION                                                     *
*& AUTOR: ENIO JESUS                                                           *
*& 13.07.2015                                                                  *
*&-----------------------------------------------------------------------------*
PUBLIC SECTION.

  TYPES: BEGIN OF TY_SELC_PLAN,
            WARPL    TYPE VIMPLASTAT-WARPL,
            MPTYP    TYPE VIMPLASTAT-MPTYP,
            STRAT    TYPE VIMPLASTAT-STRAT,
            OBJNR    TYPE VIMPLASTAT-OBJNR,
          END OF TY_SELC_PLAN.
*&---------------------------------------------------------------------*
*& METHOD INICIA PROCESSO DAS BAPIS                                    *
*&---------------------------------------------------------------------*
  METHODS: Z_INICIAR_PROCESSO_EMPRESTIMO.
  METHODS: Z_INICIAR_PROCESSO_DEVOLUCAO.
*&---------------------------------------------------------------------*
*& METHOD NOTA DE EMPRÉSTIMO                                           *
*&---------------------------------------------------------------------*
  METHODS: Z_CRIAR_NOTA_EMPRESTIMO IMPORTING
                                       EQUIPMENT TYPE EQUNR
                                      SHORT_TEXT TYPE QMTXT
                                        PRIORITY TYPE PRIOK
                                      CODE_GROUP TYPE QMGRP
                                          CODING TYPE QMCOD
                                      NOTIF_TYPE TYPE CHAR2.

  METHODS: Z_CLOSE_NOTA_EMPRESTIMO IMPORTING
                                       REFDATE  TYPE SY-DATUM
                                       REFTIME  TYPE SYUZEIT
                                       NOTIF_NO TYPE QMNUM.
*&---------------------------------------------------------------------*
*& METHOD ORDENS DE MANUTENÇÃO                                         *
*&---------------------------------------------------------------------*
  METHODS: Z_CRIAR_ORDENS_MANUTENC IMPORTING
                                      ORDER_TYPE TYPE AUFART
                                      SHORT_TEXT TYPE AUFTEXT
                                       PLANPLANT TYPE IWERK
                                        BUS_AREA TYPE GSBER
                                       MN_WK_CTR TYPE GEWRK
                                           PLANT TYPE WERGW
                                      MAINTPLANT TYPE SWERK
                                    LOC_BUS_AREA TYPE GSBER
                                       PLANGROUP TYPE INGRP
                                       EQUIPMENT TYPE EQUNR
                                      COSTCENTER TYPE KOSTL
                                       PMACTTYPE TYPE CHAR3
                                        PRIORITY TYPE PRIOK
                                        ACTIVITY TYPE VORNR
                                     CONTROL_KEY TYPE STEUS
                                     DESCRIPTION TYPE LTXA1.

  METHODS: Z_CRIAR_ORDENS_MANUTENC_GET
                   RETURNING VALUE(NUMERO_ORDEM) TYPE CHAR12.

  METHODS: Z_ENCERRAR_TODAS_ORDENS IMPORTING
                                       EQUIPMENT TYPE EQUNR
                                      STANDORDER TYPE DAUFN
                                      SETTLORDER TYPE ILOM_ORDST.

*&---------------------------------------------------------------------*
*& METHOD MODIFICAR EQUIPAMENTO                                        *
*&---------------------------------------------------------------------*
  METHODS: Z_DESINSTAL_EQUIPAMENTO IMPORTING
                                       EQUIPMENT TYPE EQUNR.

  METHODS: Z_DETALHES_EQUIPAMENTO IMPORTING
                                       EQUIPMENT TYPE EQUNR
                                  EXPORTING
                                      COSTCENTER TYPE EKOSTL.

  METHODS: Z_STATUS_EQUIPAMENTO  IMPORTING
                                       EQUIPMENT TYPE EQUNR.

  METHODS: Z_MODIFICAR_EQUIPAMENTO IMPORTING
                                       EQUIPMENT TYPE EQUNR
                                       PLANPLANT TYPE IWERK
                                        WORK_CTR TYPE NUM8
                                      STANDORDER TYPE DAUFN
                                      SETTLORDER TYPE ILOM_ORDST.

  METHODS: Z_INSTALAR_EQUIPAMENTO IMPORTING
                                       EQUIPMENT TYPE EQUNR
                                         SWERK TYPE SWERK.
*&---------------------------------------------------------------------*
*& METHOD MODIFICAR PLANO DE MANUTENÇÃO                                *
*&---------------------------------------------------------------------*
  METHODS: Z_SELECIONAR_PLANOS IMPORTING
                                       EQUIPMENT TYPE EQUNR.

  METHODS: Z_SET_ID_EQUIPAMENT  IMPORTING
                                           SWERK TYPE SWERK
                                EXPORTING
                                    ID_EQUIPMENT TYPE NUM8.


  METHODS: Z_MODIFICAR_PLANOS IMPORTING
                                       EQUIPMENT TYPE EQUNR
                                           SWERK TYPE IWERK
                                           GSBER TYPE GSBER
                                           GEWRK TYPE NUM8
                                           LAUFN TYPE DAUFN.
*&---------------------------------------------------------------------*
*& ATRIBUTOS DA CLASSE                                                 *
*&---------------------------------------------------------------------*
PRIVATE SECTION .
  DATA: IT_METHODS              TYPE TABLE OF BAPI_ALM_ORDER_METHOD,
        IT_HEADER               TYPE TABLE OF BAPI_ALM_ORDER_HEADERS_I,
        IT_OPERATION            TYPE TABLE OF BAPI_ALM_ORDER_OPERATION,
        IT_RETURN               TYPE TABLE OF BAPIRET2,
        IT_SELC_PLAN            TYPE TABLE OF TY_SELC_PLAN,
        IT_NOTIFICATION         TYPE STANDARD TABLE OF BAPI2080_1.

  DATA: WA_METHODS              TYPE BAPI_ALM_ORDER_METHOD,
        WA_HEADER               TYPE BAPI_ALM_ORDER_HEADERS_I,
        WA_OPERATION            TYPE BAPI_ALM_ORDER_OPERATION,
        WA_DATA_GENERAL         TYPE BAPI_ITOB,
        WA_DATA_GENERALX        TYPE BAPI_ITOBX,
        WA_DATA_SPECIFIC        TYPE BAPI_ITOB_EQ_ONLY,
        WA_DATA_SPECIFICX       TYPE BAPI_ITOB_EQ_ONLYX,
        WA_RETURN_BAPI_EQMT     TYPE BAPIRETURN,
        WA_RETURN               TYPE BAPIRET2,
        WA_NOTIFHEADER_EXPORT   TYPE BAPI2080_NOTHDRE,
        WA_NOTIFHEADER          TYPE BAPI2080_NOTHDRI,
        WA_SELC_PLAN            TYPE TY_SELC_PLAN,
        WA_PLKO                 TYPE PLKO,
        WA_SYSSTAT              TYPE BAPI2080_NOTSTI.

  DATA: AT_NUMERO_ORDEM         TYPE CHAR12,
        AT_NUMERO_ORDEM_REMONTA TYPE CHAR12,
        AT_NUMERO_ORDEM_ABASTEC TYPE CHAR12,
        AT_ID_EQUIPMENT         TYPE NUM8.

  DATA: SHORT_TEXT              TYPE CHAR40.
