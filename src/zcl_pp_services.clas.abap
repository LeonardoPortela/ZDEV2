class ZCL_PP_SERVICES definition
  public
  create public .

public section.

  constants:
    BEGIN OF INTEGRACAO_OPUS_DESCRITIVO,
        NAO_PROCESSADO(14) VALUE 'Não processado',
        PROCESSANDO(14)    VALUE 'Processando',
        PROCESSADO(14)     VALUE 'Processado',
        EXCLUIDO(14)       VALUE 'Excluído',
      END OF INTEGRACAO_OPUS_DESCRITIVO .
  constants:
    BEGIN OF INTEGRACAO_OPUS,
        NAO_PROCESSADO VALUE ' ',
        PROCESSANDO    VALUE 'A',
        PROCESSADO     VALUE 'P',
      END OF INTEGRACAO_OPUS .
  constants:
    BEGIN OF INTEGRACAO_XI,
        PROCESSANDO VALUE 'A',
        BLOQUEADO   VALUE 'B',
      END OF INTEGRACAO_XI .
  constants:
    BEGIN OF PO_STATUS,
        EM_PRODUCAO(4)          VALUE 'PROD',
        CONCLUIDO(4)            VALUE 'CPRD',
        ESTORNADO(4)            VALUE 'ESTN',
        AGUARDANDO_SEQUENCIA(3) VALUE 'ENT',
        AUTORIZADO_ENTRADA(4)   VALUE 'AUTZ',
        PESAGEM_VAZIO(4)        VALUE 'PSVA',
      END OF PO_STATUS .

  methods CHANGE_PLANNINGS_XI
    importing
      !VBELN type VBEP-VBELN
      !POSNR type VBEP-POSNR
      !EDATU type VBEP-EDATU
      !BMENG type VBEP-BMENG
      !STATUS type C .
  methods CHANGE_PO_USER_STATUS
    importing
      !ORDER type BAPI_ORDER_KEY-ORDER_NUMBER
      !STATUS type BAPI_ORDER_FUNC_CNTRL-STATUS
    raising
      ZCX_PP_SERVICES .
  methods PROCESS_ORDER_IS_LOCKED
    importing
      !ORDER type AUFK-AUFNR
      !WAIT type ABAP_BOOL optional
    raising
      ZCX_PP_SERVICES .
  methods UPDATE_SOLICITATION_BOARDING
    importing
      !ITEM type ZSDT0082 .
  methods CHANGE_SALES_ORDER
    importing
      !ORDER type BAPIVBELN-VBELN
      !ITEM type VBEP-POSNR optional
      !OPERATION type CHAR20
      !COMMIT type C optional
      !QUANTITY type VBEP-WMENG optional
    changing
      !DATE type SY-DATUM optional
      !_PLANNING type ZPPT0008 optional
    exceptions
      ORDER_NOT_CHANGED .
  methods CONCLUDE_PROCESS_ORDER
    importing
      !QUANTITY type BAPI_PI_HDRLEVEL-YIELD
      !DATA type BAPI_PI_HDRLEVEL-POSTG_DATE
    raising
      ZCX_PP_SERVICES .
  methods CREATE_PROCESS_ORDER
    importing
      !QUANTIDADE type VBEP-BMENG
      !MATRICULA type CI_AUFK-MATRICULA
      !ORDEM_CARREGAMENTO type CI_AUFK-ORDEMCARREG
      !PLACA type CI_AUFK-PLACA
    exporting
      !ORDER_WEIGHT type MARA-BRGEW
      !ORDER_NUMBER type AUFNR
    raising
      ZCX_PP_SERVICES .
  methods STORNO_PROCESS_ORDER
    importing
      !ORDEM_CARREGAMENTO type AUFK-ORDEMCARREG
      !PSVA type CHAR1 optional
    raising
      ZCX_PP_SERVICES .
  methods CREATE_BATCH
    importing
      !CENTRO type MCHA-WERKS
      !ORDEM type AUFNR
      !DATA_VENCIMENTO type SY-DATUM
      !MATERIAL type AFPO-MATNR
    exporting
      !LOTE type MCHA-CHARG
    raising
      ZCX_PP_SERVICES .
  methods DELETE_ORDER_DIVISION
    importing
      !POSNR type VBEP-POSNR
      !ETENR type VBEP-ETENR .
  methods DELETE_PLANNING
    importing
      !VBELN type ZPPT0008-VBELN
      !POSNR type ZPPT0008-POSNR
      !EDATU type ZPPT0008-EDATU .
  methods DELETE_SHIPMENT_TRANSIT
    importing
      !ORDEM_CARREGAMENTO type CI_AUFK-ORDEMCARREG .
  methods GET_ADDITIONAL_DATA
    returning
      value(ADDITIONAL_DATA) type CI_AUFK .
  methods GET_CUSTOMER
    returning
      value(TABLE) type KNA1 .
  methods GET_MESSAGES
    returning
      value(TABLE) type BAPIRET2_T .
  methods GET_ORDER_BUSINESS
    returning
      value(TABLE) type VBKD .
  methods GET_ORDER_HEADER
    returning
      value(TABLE) type VBAK .
  methods GET_ORDER_ITEM
    returning
      value(TABLE) type VBAP .
  methods GET_PLANNING
    returning
      value(TABLE) type ZPPT0008 .
  methods GET_PLANNINGS
    returning
      value(TABLE) type ZPPCT001 .
  methods GET_SHIPMENTS
    returning
      value(TABLE) type ZPPCT002 .
  methods GET_TEXT_STATUS
    importing
      !STATUS type TJ30T-TXT04
    returning
      value(TABLE) type TJ30T .
  methods GET_SHIPMENTS_TRANSIT
    returning
      value(TABLE) type ZPPCT003 .
  methods GET_SHIPMENT_TRANSIT
    returning
      value(TABLE) type ZPPT0009 .
  methods GET_SIMULADOR_VENDA_HEADER
    returning
      value(TABLE) type ZSDT0040 .
  methods GET_SHIPMENT
    returning
      value(TABLE) type ZPPET001 .
  methods GET_COMPONENTS_MATERIAL
    importing
      !MATERIAL type CSAP_MBOM-MATNR
      !CENTRO type CSAP_MBOM-WERKS
      !ALTERNATIVA type CSAP_MBOM-STLAL
    returning
      value(TABLE) type T_STPO_API02 .
  methods GET_COMPONENT_DETAIL
    importing
      !MATERIAL type CSAP_MBOM-MATNR
      !CLASSE type KLAH-CLASS
      !INITIAL_CHARACT type RMCLM-BASISD optional
    exporting
      !DATA type RIHCLOBJDAT_TAB
    exceptions
      DATA_NOT_FOUND .
  methods GET_PROCESS_ORDER_HEADER
    returning
      value(TABLE) type AFKO .
  methods GET_OV_DIVISIONS
    returning
      value(TABLE) type VBEP_T .
  methods GET_PACKING_WEIGHT
    importing
      !QUANTIDADE type VBEP-WMENG
    returning
      value(VALUE) type MARA-BRGEW .
  methods GET_DELIVERED_QUANTITY
    importing
      !VBELN type VBFA-VBELV
      !POSNR type VBFA-POSNV
      !ERDAT type VBFA-ERDAT
    returning
      value(VALUE) type VBFA-RFMNG .
  methods HAS_MESSAGES
    returning
      value(VALUE) type ABAP_BOOL .
  methods GET_GREATER_PLANNING_DATE
    importing
      !VBELN type VBELN
      !POSNR type POSNR
    returning
      value(VALUE) type SY-DATUM .
  methods INSERT_ORDER_DIVISION
    importing
      !POSNR type POSNR
      !WMENG type WMENG
      !EDATU type EDATU optional
      !LIFSP type LIFSP optional
      !OBJKEY type CHAR70 optional .
  methods INSERT_ORDER_XI
    importing
      !ORDEM type VBAP-VBELN
      !ITEM type VBAP-POSNR
      !TIPO type VBAK-AUART
      !QUANTIDADE type VBEP-WMENG
      !DATA type VBEP-EDATU
      !ST_ATIVIDADE type VBAP-SPART
      !MATERIAL type VBAP-MATNR
      !DESCRICAO type VBAP-ARKTX
      !EMISSOR type VBAK-KUNNR
      !INCOTERMS type VBKD-INCO1
      !VLR_UNITARIO type VBAP-NETPR
      !SAFRA type ZSDT0040-SAFRA
      !EMPRESA type VBAK-VKORG
      !CENTRO type VBAP-WERKS
      !MOEDA type VBAP-WAERK .
  methods INSERT_PLANNING_XI
    importing
      !VBELN type VBEP-VBELN
      !POSNR type VBEP-POSNR
      !EDATU type VBEP-EDATU
      !BMENG type VBEP-BMENG
      !STATUS type C .
  methods INSERT_SHIPMENT_TRANSIT
    importing
      !ORDEM_VENDA type VBELN
      !ITEM type POSNR
      !ORDEM_CARREGAMENTO type CI_AUFK-ORDEMCARREG
      !DATA type DATUM
      !QUANTIDADE type WMENG
      !PLACA type AM_KFZKZ .
  methods MODIFY_PLANNING
    importing
      !ITEM type ZPPT0008 .
  methods MODIFY_PLANNINGS
    importing
      !ITEM type ZPPCT001 .
  methods PROCESS_ORDER_IS_AUTHORIZED
    importing
      !PROCESS_ORDER type AUFK-AUFNR
    exceptions
      PROCESS_ORDER_NOT_AUTHORIZED .
  methods REFRESH_SCHEDULE_LINES .
  methods RELEASE_PROCESS_ORDER
    importing
      !LOTE type MCHA-CHARG
      !ORDER type BAPI_ORDER_KEY-ORDER_NUMBER
    exceptions
      PROCESS_ORDER_NOT_RELEASED .
  methods SELECT_CUSTOMER
    importing
      !KUNNR type KNA1-KUNNR
    exporting
      !DATA type KNA1 .
  methods SELECT_MATERIAL
    importing
      !MATERIAL type MAKT-MATNR
      !CENTRO type AUFK-WERKS optional
    exporting
      !T_MARA type MARA
      !T_MARC type MARC
      !T_MAKT type MAKT .
  methods SELECT_ORDER_BUSINESS
    importing
      !VBELN type VBELN .
  methods SELECT_ORDER_HEADER
    importing
      !VBELN type VBELN .
  methods SELECT_ORDER_ITEM
    importing
      !VBELN type VBELN
      !POSNR type POSNR .
  methods SELECT_ORDER_PARTNERS
    importing
      !ORDEM type VBELN .
  methods SELECT_PLANNING
    importing
      !VBELN type VBELN
      !POSNR type POSNR
      !EDATU type EDATU
    raising
      ZCX_PP_SERVICES .
  methods SELECT_PLANNINGS
    importing
      !VBELN type VBELN
      !POSNR type POSNR .
  methods SELECT_PROCESS_ORDER_HEADER
    importing
      !ORDEM_CARREGAMENTO type AUFK-ORDEMCARREG .
  methods SELECT_SIMULADOR_VENDA_HEADER
    importing
      !ORDEM_VENDA type VBELN .
  methods SELECT_AUTHORIZED_ORDERS
    returning
      value(TABLE) type TT_AUFK .
  methods SELECT_SHIPMENTS
    importing
      !EDATU type EDATU optional
      !VBELN type VBELN optional
      !POSNR type POSNR optional
      !WERKS type AUFK-WERKS optional
      !ESTOR type ABAP_BOOL default ABAP_FALSE
    exporting
      !DATA type ZPPCT002
    exceptions
      DATA_NOT_FOUND .
  methods SELECT_RECIPES
    returning
      value(TABLE) type ZPPCT002 .
  methods GET_RECIPES
    returning
      value(TABLE) type ZPPCT004
    exceptions
      DATA_NOT_FOUND .
  methods IS_RESERVATIONS_AVAILABLE
    importing
      !RESERVATION_NO type AFKO-RSNUM
      !CHECK_IS_LOCKED type ABAP_BOOL optional
    raising
      ZCX_PP_SERVICES .
  methods SELECT_SHIPMENT
    importing
      !ORDEM_CARREGAMENTO type AUFK-ORDEMCARREG
      !DATE type AUFK-ERDAT optional
    exceptions
      SHIPMENT_NOT_FOUND
      ZCX_PP_SERVICES .
  methods SELECT_SHIPMENTS_TRANSIT
    importing
      !VBELN type VBELN
      !POSNR type POSNR
      !DATE type DATUM optional
    exporting
      !DATA type ZPPCT003
    exceptions
      DATA_NOT_FOUND .
  methods SELECT_SHIPMENT_TRANSIT
    importing
      !ORDEM_CARREGAMENTO type CI_AUFK-ORDEMCARREG
      !ORDEM_VENDA type VBELN optional
      !ITEM type POSNR optional
    raising
      ZCX_PP_SERVICES .
  methods SELECT_TEXT_STATUS
    importing
      !STATUS type TJ30T-TXT04 .
  methods SET_PLANNING_LOG
    importing
      !COMPONENT type ZPPT0010-COMPONENT
      !OPERATION type ZPPT0010-CHANGEID
      !OLD_ITEM type ANY
      !NEW_ITEM type ANY .
  methods SET_MESSAGE
    importing
      !MSG_TABLE type ANY optional
      !TYPE type SYMSGTY optional
      !ID type SYMSGID optional
      !NUMBER type SYMSGNO optional
      !V1 type ANY optional
      !V2 type ANY optional
      !V3 type ANY optional
      !V4 type ANY optional .
*    METHODS set_message_from_bapi
*      IMPORTING
*        message TYPE any.
*  class-methods CONVERT_TO_UTF8
*    changing
*      !JSON type STRING .
  methods SET_MESSAGES
    importing
      !MESSAGES type ANY TABLE .
  methods SET_BATCH_PROCESS_ORDER
    importing
      !AUFNR type AUFK-AUFNR
      !CHARG type MCHB-CHARG .
  methods UPDATE_ORDER_DIVISION
    importing
      !POSNR type POSNR
      !ETENR type ETENR
      !WMENG type WMENG optional
      !EDATU type WDATU optional
      !UNLOCK type ABAP_BOOL optional
      !OBJKEY type BAPISDKEY-REFOBJKEY optional .
  methods UPDATE_PROCESS_ORDER
    importing
      !ORDEM_PRODUCAO type BAPI_ORDER_KEY-ORDER_NUMBER
      !MATRICULA type CI_AUFK-MATRICULA
      !ORDEM_CARREGAMENTO type CI_AUFK-ORDEMCARREG
      !PLACA type CI_AUFK-PLACA
    exceptions
      PROCESS_ORDER_NOT_CHANGED .
  methods SELECT_BATCH
    importing
      !LOTE type MCHA-CHARG
      !MATERIAL type MARA-MATNR
      !CENTRO type AUFK-WERKS
      !DEPOSITO type MCHB-LGORT
    exceptions
      BATCH_NOT_FOUND .
  methods SELECT_OV_DIVISIONS
    importing
      !VBELN type VBEP-VBELN
      !POSNR type VBEP-POSNR
    exporting
      !ITEMS type VBEP_T .
  methods SELECT_MSKA
    importing
      !VBELN type VBELN
      !POSNR type POSNR
      !CHARG type CHARG_D
    raising
      ZCX_PP_SERVICES .
  methods UPDATE_SHIPMENT
    importing
      !ORDEM_CARREGAMENTO type CI_AUFK-ORDEMCARREG
      !ORDEM_VENDA type VBELN optional
      !ITEM type POSNR optional .
  methods GET_LAST_TIME
    importing
      value(VBELN) type VBELN
    returning
      value(HORA) type SY-UZEIT .
  methods CHECK_VL_NEGATIVO
    importing
      !LINES type OIJ_BAPISCHDL_T
    returning
      value(FLAG) type CHAR1 .
  class-methods GET_TEXT_SD
    importing
      !MATNR type MATNR
      !WERKS type WERKS_D
    exporting
      !TEXT type ZTY_TLINE .
  methods SELECT_ORDEM_CARREGAMENTO
    importing
      !ORDEM_CARREGAMENTO type ZPPED003
    raising
      ZCX_PP_SERVICES .
  PROTECTED SECTION.
    DATA MESSAGES TYPE TABLE OF BAPIRET2.
    DATA MESSAGE  TYPE BAPIRET2.

private section.

  data CUSTOMER type KNA1 .
  data ORDER_ITEM type VBAP .
  class-data ADDITIONAL_DATA type CI_AUFK .
  data ORDER_STATUS type TJ30T .
  data USERS_STATUS type TJ30 .
  data ORDER_DIVISION type VBEP .
  data OV_DIVISION_PLANNING type VBEP_T .
  data OV_DIVISION_SHIPMENT type VBEP .
  data OV_DIVISIONS type VBEP_T .
  data SCHED_LINES type OIJ_BAPISCHDL_T .
  data SCHED_LINESX type OIJ_BAPISCHDLX_T .
  data PROCESS_ORDER_HEADER type AFKO .
  data:
    SHIPMENTS_TRANSIT TYPE TABLE OF ZPPT0009 .
  data SHIPMENT_TRANSIT type ZPPT0009 .
  data HEADER_SIMULADOR_VENDA type ZSDT0040 .
  data PLANNING type ZPPT0008 .
  data ORDER_HEADER type VBAK .
  data BUSINESS_DATA type VBKD .
  data PLANNINGS type ZPPCT001 .
  data SHIPMENT type ZPPET001 .
  data SHIPMENTS type ZPPCT002 .
  data SHIPMENTS_IN_PRODUCTION type ZPPCT002 .
  data PARTNERS type TT_VBPA . "#EC CI_USAGE_OK[2228056]
  data:
    BDCDATA TYPE TABLE OF BDCDATA .
  data:
    MESSTAB TYPE TABLE OF BDCMSGCOLL .
  constants TP_EMBALAGEM type KLAH-CLASS value 'ZFERT_TIP_EMBALAGE' ##NO_TEXT.
  data MSKA type MSKA .

  methods GET_ORDER_DIVISION
    returning
      value(TABLE) type VBEP .
  methods GET_DIVISION_PLANNING
    returning
      value(TABLE) type VBEP_T .
  methods GET_DIVISION_SHIPMENT
    returning
      value(TABLE) type VBEP .
  methods GET_ORDER_PARTNERS
    returning
      value(TABLE) type TT_VBPA . "#EC CI_USAGE_OK[2228056]
  methods GET_SCHEDULE_LINES
    exporting
      !LINES type OIJ_BAPISCHDL_T
      !LINESX type OIJ_BAPISCHDLX_T .
  methods SELECT_ORDER_DIVISION
    importing
      !VBELN type VBEP-VBELN
      !POSNR type VBEP-POSNR
      !EDATU type VBEP-EDATU optional
      !LIFSP type VBEP-LIFSP optional
    exporting
      !ITEM type VBEP .
  methods SELECT_DIVISION_PLANNING
    importing
      !VBELN type VBEP-VBELN
      !POSNR type VBEP-POSNR
      !QUANTITY type VBEP-WMENG optional
      !OPERATION type CHAR20 optional
    exporting
      !ERRO type SCX_T100KEY
    exceptions
      ZCX_PP_SERVICES .
  methods SELECT_DIVISION_SHIPMENT
    importing
      !VBELN type VBEP-VBELN
      !POSNR type VBEP-POSNR
      !EDATU type VBEP-EDATU .
  methods SET_ADDITIONAL_DATA
    importing
      !MATRICULA type CI_AUFK-MATRICULA
      !ORDEM_CARREGAMENTO type CI_AUFK-ORDEMCARREG
      !PLACA type CI_AUFK-PLACA .
  methods SET_BDC_DYNPRO
    importing
      !PROGRAM type SY-REPID
      !DYNPRO type SY-DYNNR .
  methods SET_BDC_FIELD
    importing
      !FNAM type BDCDATA-FNAM
      !FVAL type BDCDATA-FVAL .
  methods GET_BDC_DATA
    returning
      value(TABLE) type BDCDATA_TAB .
  methods DELETE_BATCH
    importing
      !CENTRO type MCHA-WERKS
      !MATERIAL type AFPO-MATNR
      !LOTE type MCHA-CHARG .
  methods DATE_CONV_INT_TO_EXT
    importing
      !INPUT type SY-DATUM
    returning
      value(RETURN) type CHAR10 .
  methods BAPI_CREATE_BATCH
    importing
      !_NEW_LGORT type MCHB-LGORT
      !_RETURN type BAPIRET2_T
      !_YMCHAI type MCHA
    changing
      !_CHAR_OF_BATCH type CLBATCH_T
      !_NEW_BATCH type MCHA_TTY
      !_YMCHA type MCHA
    returning
      value(_SUBRC) type SY-SUBRC .
  methods COMMIT .
ENDCLASS.



CLASS ZCL_PP_SERVICES IMPLEMENTATION.


  METHOD BAPI_CREATE_BATCH.

    CALL FUNCTION 'VB_CREATE_BATCH'
      EXPORTING
        YMCHA                        = _YMCHA
        NEW_LGORT                    = _NEW_LGORT
        KZCLA                        = '2'
        CLASS                        = 'FERTILIZANTES'
        NO_CFC_CALLS                 = 'X'
        CHECK_EXTERNAL               = 'X'
        CHECK_CUSTOMER               = 'X'
      IMPORTING
        YMCHA                        = _YMCHA
      TABLES
        CHAR_OF_BATCH                = _CHAR_OF_BATCH
        NEW_BATCH                    = _NEW_BATCH
        RETURN                       = _RETURN
      EXCEPTIONS
        NO_MATERIAL                  = 1
        NO_BATCH                     = 2
        NO_PLANT                     = 3
        MATERIAL_NOT_FOUND           = 4
        PLANT_NOT_FOUND              = 5
        STOLOC_NOT_FOUND             = 6
        LOCK_ON_MATERIAL             = 7
        LOCK_ON_PLANT                = 8
        LOCK_ON_BATCH                = 9
        LOCK_SYSTEM_ERROR            = 10
        NO_AUTHORITY                 = 11
        BATCH_EXIST                  = 12
        STOLOC_EXIST                 = 13
        ILLEGAL_BATCH_NUMBER         = 14
        NO_BATCH_HANDLING            = 15
        NO_VALUATION_AREA            = 16
        VALUATION_TYPE_NOT_FOUND     = 17
        NO_VALUATION_FOUND           = 18
        ERROR_AUTOMATIC_BATCH_NUMBER = 19
        CANCELLED                    = 20
        WRONG_STATUS                 = 21
        INTERVAL_NOT_FOUND           = 22
        NUMBER_RANGE_NOT_EXTERN      = 23
        OBJECT_NOT_FOUND             = 24
        ERROR_CHECK_BATCH_NUMBER     = 25
        NO_EXTERNAL_NUMBER           = 26
        NO_CUSTOMER_NUMBER           = 27
        NO_CLASS                     = 28
        ERROR_IN_CLASSIFICATION      = 29
        INCONSISTENCY_IN_KEY         = 30
        REGION_OF_ORIGIN_NOT_FOUND   = 31
        COUNTRY_OF_ORIGIN_NOT_FOUND  = 32
        OTHERS                       = 33.

    MOVE SY-SUBRC TO _SUBRC.

  ENDMETHOD.


  METHOD change_plannings_xi.
*--> 25.08.2023 11:45:02 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_SD_OUTBOUND_PLANEJAMENTO_OV' IN BACKGROUND TASK
*      DESTINATION 'XI_PLANEJAMENTO_OV'
*      EXPORTING
*        vbeln  = vbeln
*        posnr  = posnr
*        edatu  = edatu
*        bmeng  = bmeng
*        status = status.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_PLANEJAMENTO_OV'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        EXPORTING
          vbeln  = vbeln
          posnr  = posnr
          edatu  = edatu
          bmeng  = bmeng
          status = status.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        EXPORTING
          vbeln  = vbeln
          posnr  = posnr
          edatu  = edatu
          bmeng  = bmeng
          status = status.
    ENDIF.

    COMMIT WORK.
* <-- 25.08.2023 11:45:02 - Migração S4 – ML – Fim
  ENDMETHOD.


  METHOD CHANGE_PO_USER_STATUS.
    DATA RETURN_DETAILS TYPE TABLE OF BAPI_ORDER_RETURN.
    DATA RETURN TYPE BAPIRET2.
    DATA SEQ TYPE I.

    DATA(_ORDER) =
      VALUE TB_BAPI_ORDER_KEY( ( ORDER_NUMBER = ORDER ) ).

    SEQ = 0.

    DO.

      CALL FUNCTION 'BAPI_PROCORD_SETUSERSTATUS'
        EXPORTING
          STATUS_PROFILE     = 'ZPI00001'
          STATUS             = STATUS
          WORK_PROCESS_GROUP = 'COWORK_BAPI'
          WORK_PROCESS_MAX   = 99
        IMPORTING
          RETURN             = RETURN
        TABLES
          ORDERS             = _ORDER
          DETAIL_RETURN      = RETURN_DETAILS.

      DELETE RETURN_DETAILS WHERE TYPE NE 'E'.

      IF SEQ > 4.
        EXIT.
      ENDIF.

      ADD 1 TO SEQ.

      IF ( RETURN_DETAILS IS INITIAL ).
        EXIT.
      ELSE.
        FREE RETURN_DETAILS.
      ENDIF.

      WAIT UP TO 3 SECONDS.

    ENDDO.

    IF ( RETURN_DETAILS IS NOT INITIAL ).
      ME->SET_MESSAGES( RETURN_DETAILS ).
      RAISE EXCEPTION TYPE ZCX_PP_SERVICES.
    ENDIF.
  ENDMETHOD.


  METHOD change_sales_order.
    DATA return         TYPE TABLE OF bapiret2.
    DATA difference_qty TYPE vbep-wmeng.
    DATA lw_return      TYPE bapiret2.

    DATA(_division_planning) = me->get_division_planning( ).

    DATA(_division_shipment) = me->get_division_shipment( ).
    DATA(_old_planning)      = _planning.
    DATA(_shipment)          = me->get_shipment( ).

    DATA(_quantity) = quantity.
    DATA(lv_realizado) = quantity.
    CLEAR lv_realizado.
    IF date IS INITIAL.
      date = me->get_greater_planning_date( vbeln = order posnr = item ).
    ENDIF.

    CASE operation.
      WHEN 'SAVE_PLAN'. "//Save planning
        DATA(_divisions) = me->get_ov_divisions( ).

        LOOP AT _divisions INTO DATA(_division) WHERE ( lifsp = 10 ).
          ADD _division-wmeng TO _quantity.

          IF _division-wmeng IS NOT INITIAL.
            me->delete_order_division( posnr = _division-posnr etenr = _division-etenr ).
          ENDIF.
        ENDLOOP.

        IF NOT _quantity IS INITIAL.
          CALL METHOD me->insert_order_division
            EXPORTING
              posnr = item
              wmeng = _quantity
              edatu = CONV sy-datum( date + 1 )
              lifsp = '10'.
        ENDIF.

      WHEN 'CREATE_PO'. "//Create new process order

        LOOP AT _division_planning ASSIGNING FIELD-SYMBOL(<fs_division_planning>).
          DATA(lv_index) = sy-tabix.

          IF _quantity <= 0.
            EXIT.
          ENDIF.

          IF _quantity <= <fs_division_planning>-wmeng.
            SUBTRACT _quantity FROM <fs_division_planning>-wmeng.
          ELSE.
            _quantity = <fs_division_planning>-wmeng.
            CLEAR <fs_division_planning>-wmeng.
          ENDIF.

          lv_realizado = lv_realizado + _quantity.

*          SUBTRACT quantity FROM _planning-wdisp.
          SUBTRACT _quantity FROM _planning-wdisp.
          _quantity = quantity - lv_realizado.

          IF _planning-wdisp IS INITIAL.
            _planning-iopus = integracao_opus-processado.
          ENDIF.

          IF NOT ( _division_shipment IS INITIAL ).
            CALL METHOD me->update_order_division
              EXPORTING
                posnr  = _division_shipment-posnr
                etenr  = _division_shipment-etenr
                wmeng  = _division_shipment-wmeng + quantity
                edatu  = _division_shipment-edatu
                unlock = abap_true.
          ELSEIF lv_index EQ 1.

            CALL METHOD me->insert_order_division
              EXPORTING
                posnr = _planning-posnr
                edatu = _planning-edatu
                wmeng = quantity
                lifsp = ' '.

          ENDIF.

          IF ( <fs_division_planning>-wmeng IS INITIAL ).
            CALL METHOD me->delete_order_division
              EXPORTING
                posnr = <fs_division_planning>-posnr
                etenr = <fs_division_planning>-etenr.
          ELSE.
*
*            IF quantity > <fs_division_planning>-wmeng.
*              <fs_division_planning>-wmeng = quantity.
*            ENDIF.

            CALL METHOD me->update_order_division
              EXPORTING
                posnr = <fs_division_planning>-posnr
                etenr = <fs_division_planning>-etenr
                wmeng = <fs_division_planning>-wmeng
                edatu = <fs_division_planning>-edatu.
          ENDIF.

          _planning-changedby = sy-uname.
          _planning-changeddt = sy-datum.

        ENDLOOP.

      WHEN 'CONCLU_PO'. "//Conclude the process order

        "//If the second weight is lower
        IF ( quantity < shipment-psmng ).

          CALL METHOD me->get_delivered_quantity
            EXPORTING
              vbeln = _planning-vbeln
              posnr = _planning-posnr
              erdat = _planning-edatu
            RECEIVING
              value = DATA(_delivered_qty).

          DATA(_difference_shipment) = _shipment-psmng - quantity.
          DATA(_difference_planning) = _planning-wmeng - ( _delivered_qty + quantity ).

          difference_qty = COND #( WHEN _difference_shipment <= _difference_planning
                                   THEN _difference_shipment
                                   ELSE _difference_planning
                                 ).

          CHECK ( difference_qty IS NOT INITIAL ).

          IF ( difference_qty < 0 ).
            CALL METHOD me->set_message
              EXPORTING
                id     = zcx_pp_services=>planning_quantity_unavailable-msgid
                number = zcx_pp_services=>planning_quantity_unavailable-msgno
                v1     = _planning-vbeln
                v2     = _planning-posnr
                v3     = _planning-edatu
                v4     = _planning-wmeng - _delivered_qty.

            RAISE order_not_changed.
          ENDIF.

          IF ( _division_planning IS NOT INITIAL ).
            LOOP AT _division_planning ASSIGNING <fs_division_planning> .

              "//Returns the difference for the balance
              me->update_order_division(
                posnr = <fs_division_planning>-posnr
                etenr = <fs_division_planning>-etenr
                wmeng = <fs_division_planning>-wmeng + _difference_shipment ).

            ENDLOOP.
          ELSE.
            me->insert_order_division(
              posnr = _planning-posnr
              edatu = CONV sy-datum( date + 1 )
              wmeng = difference_qty
              lifsp = '10' ).
          ENDIF.


          "//Remove the difference from shipment row
          me->update_order_division(
            posnr  = _division_shipment-posnr
            etenr  = _division_shipment-etenr
            wmeng  = _division_shipment-wmeng - _difference_shipment
            unlock = abap_true
          ).

          _planning-wdisp     = _planning-wdisp + _difference_shipment.
          _planning-iopus     = zcl_pp_services=>integracao_opus-processando.
          _planning-changedby = sy-uname.
          _planning-changeddt = sy-datum.

          "//If the second weight is bigger
        ELSEIF ( quantity > shipment-psmng ).
          difference_qty = quantity - _shipment-psmng.

          IF ( _division_planning IS NOT INITIAL ).
            CLEAR _quantity.

            LOOP AT _division_planning ASSIGNING <fs_division_planning>.
              IF _quantity > 0.
                EXIT.
              ENDIF.

              _quantity       = <fs_division_planning>-wmeng - difference_qty.
              _planning-wdisp = _planning-wdisp - difference_qty.

              IF _quantity > 0.
                CALL METHOD me->update_order_division
                  EXPORTING
                    posnr = <fs_division_planning>-posnr
                    etenr = <fs_division_planning>-etenr
                    wmeng = _quantity.
              ELSE.
                CALL METHOD me->delete_order_division
                  EXPORTING
                    posnr = <fs_division_planning>-posnr
                    etenr = <fs_division_planning>-etenr.
              ENDIF.

              CALL METHOD me->update_order_division
                EXPORTING
                  posnr  = _division_shipment-posnr
                  etenr  = _division_shipment-etenr
                  wmeng  = _division_shipment-wmeng + COND #( WHEN _quantity > 0 THEN difference_qty ELSE <fs_division_planning>-wmeng )
                  unlock = abap_true.

            ENDLOOP.

          ENDIF.
        ENDIF.

      WHEN 'STORNO_PO'. "//Storno process order

        READ TABLE _division_planning ASSIGNING <fs_division_planning> INDEX 1.
        IF sy-subrc IS INITIAL.

          IF ( <fs_division_planning> IS NOT INITIAL ).
            CALL METHOD me->update_order_division
              EXPORTING
                posnr = <fs_division_planning>-posnr
                etenr = <fs_division_planning>-etenr
                wmeng = <fs_division_planning>-wmeng + _quantity
                edatu = <fs_division_planning>-edatu.
          ELSE.
            CALL METHOD me->insert_order_division
              EXPORTING
                posnr = _planning-posnr
                edatu = CONV sy-datum( date + 1 )
                wmeng = _quantity
                lifsp = '10'.
          ENDIF.

          IF _quantity < _division_shipment-wmeng.
            CALL METHOD me->update_order_division
              EXPORTING
                posnr  = _division_shipment-posnr
                etenr  = _division_shipment-etenr
                wmeng  = _division_shipment-wmeng - _quantity
                edatu  = _division_shipment-edatu
                unlock = abap_true.
          ELSE.
            CALL METHOD me->delete_order_division
              EXPORTING
                posnr = _division_shipment-posnr
                etenr = _division_shipment-etenr.
          ENDIF.

          ADD _quantity TO _planning-wdisp.

          _planning-iopus     = zcl_pp_services=>integracao_opus-processando.
          _planning-changedby = sy-uname.
          _planning-changeddt = sy-datum.

        ENDIF.
    ENDCASE.

*    ME->SET_PLANNING_LOG(
*      EXPORTING
*        COMPONENT = CONV #( OPERATION )
*        OPERATION = 'U'
*        OLD_ITEM  = _OLD_PLANNING
*        NEW_ITEM  = _PLANNING ).

    CALL METHOD me->get_schedule_lines
      IMPORTING
        lines  = DATA(schedule_lines)
        linesx = DATA(schedule_linesx).

    CALL METHOD me->check_vl_negativo
      EXPORTING
        lines = schedule_lines
      RECEIVING
        flag  = DATA(_flag).

    IF _flag IS INITIAL.
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          salesdocument    = order
          order_header_inx = VALUE bapisdh1x( updateflag = zutil_crud-update )
        TABLES
          return           = return
          schedule_lines   = schedule_lines
          schedule_linesx  = schedule_linesx.
    ELSE.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = 'E'
          cl     = zcx_pp_services=>planning_quantity_negative-msgid
          number = zcx_pp_services=>planning_quantity_negative-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = lw_return.

      APPEND lw_return TO return.
      CLEAR lw_return.

    ENDIF.

    DELETE return WHERE type NE 'E'.
    me->set_messages( return ).

    me->refresh_schedule_lines( ).

    IF ( me->has_messages( ) = abap_true ).

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RAISE order_not_changed.

    ELSE.

      IF commit = abap_true.

        me->set_planning_log(
          EXPORTING
            component = CONV #( operation )
            operation = 'U'
            old_item  = _old_planning
            new_item  = _planning ).

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = abap_true
          IMPORTING
            return = lw_return.

        WAIT UP TO 5 SECONDS.

      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD CHECK_VL_NEGATIVO.

    LOOP AT LINES INTO DATA(_LINE).
      IF _LINE-REQ_QTY < 0.
        FLAG = ABAP_TRUE.
      ENDIF.
    ENDLOOP.

*    IF FLAG IS NOT INITIAL.
*      RAISE EXCEPTION TYPE ZCX_PP_SERVICES
*        EXPORTING
*          TEXTID = ZCX_PP_SERVICES=>PLANNING_QUANTITY_NEGATIVE.
*    ENDIF.

  ENDMETHOD.


  METHOD COMMIT.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = ABAP_TRUE.

  ENDMETHOD.


  METHOD conclude_process_order.
    DATA return_details TYPE TABLE OF bapi_coru_return.
    DATA: return  TYPE bapiret1,
          lv_erro TYPE scx_t100key.

    DATA(_planning)      = me->get_planning( ).
    DATA(_shipment)      = me->get_shipment( ).

    IF ( _shipment-txt04 = zcl_pp_services=>po_status-aguardando_sequencia  )
    OR ( _shipment-txt04 = zcl_pp_services=>po_status-autorizado_entrada    )
    OR ( _shipment-txt04 = zcl_pp_services=>po_status-estornado             ).
      RAISE EXCEPTION TYPE zcx_pp_services
        EXPORTING
          textid = VALUE #( msgid = zcx_pp_services=>process_order_not_released-msgid
                            msgno = zcx_pp_services=>process_order_not_released-msgno
                            attr1 = _shipment-aufnr ).
    ENDIF.

    IF ( _shipment-wemng IS INITIAL ).
      CALL METHOD me->select_division_planning
        EXPORTING
          vbeln     = _planning-vbeln
          posnr     = _planning-posnr
          quantity  = quantity
          operation = 'CONCLU_PO'
        IMPORTING
          erro      = lv_erro.
      IF lv_erro IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_pp_services
          EXPORTING
            textid = lv_erro.
      ENDIF.

      me->select_division_shipment(
        vbeln = _planning-vbeln
        posnr = _planning-posnr
        edatu = _planning-edatu
      ).

      "//Change sales order
      CALL METHOD me->change_sales_order(
        EXPORTING
          order             = _planning-vbeln
          item              = _planning-posnr
          quantity          = quantity
          operation         = 'CONCLU_PO'
        CHANGING
          _planning         = _planning
        EXCEPTIONS
          order_not_changed = 4 ).

      IF ( sy-subrc IS INITIAL ).
        DATA(_order_data) =
          VALUE bapi_pi_hdrlevel_t( ( orderid        = _shipment-aufnr
                                      fin_conf       = 'X'
                                      postg_date     = data
                                      conf_text      = 'Serviço OPUS - Confirmação'
                                      yield          = quantity
                                   ) ).

        CALL FUNCTION 'BAPI_PROCORDCONF_CREATE_HDR' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            post_wrong_entries = '0'
          IMPORTING
            return             = return
          TABLES
            athdrlevels        = _order_data
            detail_return      = return_details.

        DELETE return_details WHERE type NE 'E'.
        IF ( return_details IS INITIAL ).
          WAIT UP TO 2 SECONDS.
          me->modify_planning( _planning ). "//Commit all

          process_order_is_locked( order = _shipment-aufnr wait = abap_true ).

          TRY.
              me->change_po_user_status( order = _shipment-aufnr status = me->po_status-concluido ).
            CATCH zcx_pp_services.
          ENDTRY.

        ELSE.
          me->set_messages( return_details ).
        ENDIF.
      ENDIF.

    ELSE.
      IF _shipment-txt04 = zcl_pp_services=>po_status-em_producao.
        TRY.
            me->change_po_user_status( order = _shipment-aufnr status = me->po_status-concluido ).
          CATCH zcx_pp_services.
        ENDTRY.
      ELSE.
        RAISE EXCEPTION TYPE zcx_pp_services
          EXPORTING
            textid = VALUE #( msgid = zcx_pp_services=>order_already_concluded-msgid
                              msgno = zcx_pp_services=>order_already_concluded-msgno
                              attr1 = _shipment-aufnr ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method create_batch.

    data return    type table of bapiret2.
    data new_batch type table of mcha.
    data data_fabricacao type char10.
    data data_validade type char10.
    data id_mapa type atwrt.
    data: vl_class type klah-class value 'GARANTIA_FERT'.

    lote = |{ ordem alpha = out }|.

    me->select_material(
      exporting
        material = material
        centro   = centro "BUG SOLTO 165396
      importing
        t_mara   = data(_mara)
        t_marc   = data(_marc)
    ).

    me->select_batch(
      exporting
        lote            = lote
        material        = material
        centro          = centro
        deposito        = _marc-lgpro
      exceptions
        batch_not_found = 4
    ).

    check sy-subrc <> 0.

    data(_dias_vencimento) = conv i( _mara-mhdhb ).
    data(_data_validade)   = conv sy-datum( data_vencimento + _dias_vencimento ).

    data(_header) = value mcha( matnr = material
                                werks = centro
                                charg = lote
                                hsdat = data_vencimento
                              ).

    vl_class = |{ vl_class }_{ centro alpha = in }|.

    me->get_component_detail(
      exporting
        material        = material
        classe          = vl_class
        initial_charact = abap_true
      importing
        data            = data(_cd_mapa)
      exceptions
        data_not_found  = 4 ).

    id_mapa = |F04_{ centro }_CD_MAPA|.
    read table _cd_mapa into data(wl_cd_mapa) with key atnam = id_mapa.

    data(_characteristics) =
      value clbatch_t( ( atnam = 'FERTILIZANTES_MAPA'           atwtb = wl_cd_mapa-ausp1     )
                       ( atnam = 'FERTILIZANTES_DATAFABRICACAO' atwtb = me->date_conv_int_to_ext( input = data_vencimento ) )
                       ( atnam = 'FERTILIZANTES_DATAVALIDADE'   atwtb = me->date_conv_int_to_ext( input = _data_validade  ) )
                       ( atnam = 'FERTILIZANTES_MARCA'          atwtb = 'AMAGGI'        )
                     ).

    if me->bapi_create_batch(
                              exporting _ymchai        = _header
                                        _new_lgort     = _marc-lgpro
                                        _return        = return
                               changing _char_of_batch = _characteristics
                                        _ymcha         = _header
                                        _new_batch     = new_batch
                               ) is initial.

      delete return where type ne 'E'.
      me->set_messages( return ).
      me->commit( ).

      wait up to 2 seconds.

      me->select_batch(
        exporting
          lote            = lote
          material        = material
          centro          = centro
          deposito        = _marc-lgpro
        exceptions
          batch_not_found = 4
      ).

      if sy-subrc is not initial.

        if me->bapi_create_batch(
                                  exporting _ymchai        = _header
                                            _new_lgort     = _marc-lgpro
                                            _return        = return
                                   changing _char_of_batch = _characteristics
                                            _ymcha         = _header
                                            _new_batch     = new_batch
                                 ) is initial.

          delete return where type ne 'E'.
          me->set_messages( return ).
          me->commit( ).
        endif.

      endif.

    else.
      raise exception type zcx_pp_services
        exporting
          textid = value #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = sy-msgv1
                            attr2 = sy-msgv2
                            attr3 = sy-msgv3
                            attr4 = sy-msgv4
                          ).
    endif.
  endmethod.


  METHOD create_process_order.
    DATA: return  TYPE bapiret2,
          lv_erro TYPE scx_t100key.

    "//Get datas
    DATA(_planning)      = me->get_planning( ).
    DATA(_shipment)      = me->get_shipment( ).

    "//Select order item
    CALL METHOD me->select_order_item
      EXPORTING
        vbeln = _planning-vbeln
        posnr = _planning-posnr.

    "//Get packing weight by quantity
    order_weight = me->get_packing_weight( quantidade ).

    "//Create process order
    IF ( _shipment IS INITIAL ).
      IF quantidade > _planning-wdisp.
        RAISE EXCEPTION TYPE zcx_pp_services
          EXPORTING
            textid = zcx_pp_services=>planning_quantity_insufficient.
      ENDIF.

      CALL METHOD me->select_division_planning
        EXPORTING
          vbeln     = _planning-vbeln
          posnr     = _planning-posnr
          quantity  = quantidade
          operation = 'CREATE_PO'
        IMPORTING
          erro      = lv_erro.

      IF lv_erro IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_pp_services
          EXPORTING
            textid = lv_erro.
      ENDIF.

      me->select_division_shipment(
        vbeln = _planning-vbeln
        posnr = _planning-posnr
        edatu = _planning-edatu
      ).

      CALL METHOD me->change_sales_order
        EXPORTING
          order             = _planning-vbeln
          quantity          = quantidade
          operation         = 'CREATE_PO'
        CHANGING
          _planning         = _planning
        EXCEPTIONS
          order_not_changed = 4.

      IF ( sy-subrc IS INITIAL ).
        me->set_additional_data(
              matricula          = matricula
              ordem_carregamento = ordem_carregamento
              placa              = placa ).

        DATA(_orderdata) =
          VALUE bapi_pi_order_create(
                material         = me->get_order_item( )-matnr
                plant            = me->get_order_item( )-werks
                order_type       = 'PI01'
                basic_start_date = _planning-edatu
                recipe_counter   = '01'
                basic_start_time = sy-uzeit
                quantity         = quantidade
                quantity_uom     = me->get_order_item( )-meins
                sales_order      = _planning-vbeln
                sales_order_item = _planning-posnr ).

        CALL FUNCTION 'BAPI_PROCORD_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            orderdata    = _orderdata
          IMPORTING
            return       = return
            order_number = order_number.

        IF ( order_number IS INITIAL ).
          me->set_message( msg_table = return ).
        ELSE.
          me->modify_planning( _planning ).
          me->delete_shipment_transit( ordem_carregamento ).
        ENDIF.
      ENDIF.

    ELSE.
      MOVE _shipment-aufnr TO order_number.
    ENDIF.
  ENDMETHOD.


  METHOD DATE_CONV_INT_TO_EXT.
*// Convert Date de Vencimento YYYYMMDD para DD.MM.YYYY
    CALL 'DATE_CONV_INT_TO_EXT'
         ID 'DATINT' FIELD INPUT
         ID 'DATEXT' FIELD RETURN.

  ENDMETHOD.


  METHOD DELETE_BATCH.
    CALL METHOD ME->SELECT_MATERIAL
      EXPORTING
        MATERIAL = MATERIAL
      IMPORTING
        T_MARC   = DATA(_MARC).

    DATA(_DEL_FLAGS) =
        VALUE BATCHDELFLG( MCH1_DEL = ABAP_TRUE
                           MCHA_DEL = ABAP_TRUE
                           MCHB_DEL = ABAP_TRUE
                         ).

    CALL FUNCTION 'VB_BATCH_SET_DELETION_FLAG'
      EXPORTING
        MATNR           = MATERIAL
        CHARG           = LOTE
        WERKS           = CENTRO
        LGORT           = _MARC-LGPRO
*        IMPORTING
*       rc_mch1         =
*       rc_mcha         =
*       rc_mchb         =
      CHANGING
        BATCH_DEL_FLAGS = _DEL_FLAGS
      EXCEPTIONS
        NO_MATERIAL     = 1
        NO_BATCH        = 2
        NO_PLANT        = 3
        NO_STLOC        = 4
        OTHERS          = 5.

    CALL FUNCTION 'VB_UPDATE_BATCH_PREPARE'
      EXPORTING
        NO_CHANGE_DOCUMENT = SPACE.
  ENDMETHOD.


  METHOD DELETE_ORDER_DIVISION.
    DATA(NEW_SCHEDULE) =
        NEW BAPISCHDL( ITM_NUMBER = POSNR
                       SCHED_LINE = ETENR
                     ).

    DATA(NEW_SCHEDULE_X) =
        NEW BAPISCHDLX(
                       ITM_NUMBER = POSNR
                       SCHED_LINE = ETENR
                       UPDATEFLAG = ZUTIL_CRUD-DELETE
                      ).

    APPEND NEW_SCHEDULE->*   TO ME->SCHED_LINES.
    APPEND NEW_SCHEDULE_X->* TO ME->SCHED_LINESX.
  ENDMETHOD.


  METHOD DELETE_PLANNING.
    DELETE FROM ZPPT0008
     WHERE VBELN = VBELN
       AND POSNR = POSNR
       AND EDATU = EDATU.
  ENDMETHOD.


  METHOD DELETE_SHIPMENT_TRANSIT.

*    DELETE FROM ZPPT0009
*     WHERE ORDEM_CARREG = ORDEM_CARREGAMENTO.

    SELECT SINGLE *
      FROM ZPPT0009
      INTO @DATA(W_0009)
      WHERE ORDEM_CARREG EQ @ORDEM_CARREGAMENTO
        AND STATUS NE @ABAP_TRUE.

    IF SY-SUBRC IS INITIAL.
      UPDATE ZPPT0009
          SET STATUS = ABAP_TRUE
              DT_MODIFICACAO = SY-DATUM
              HR_MODIFICACAO = SY-UZEIT
           WHERE ORDEM_CARREG EQ W_0009-ORDEM_CARREG.
    ENDIF.

  ENDMETHOD.


  METHOD GET_ADDITIONAL_DATA.
    MOVE ME->ADDITIONAL_DATA TO ADDITIONAL_DATA.
  ENDMETHOD.


  METHOD GET_BDC_DATA.
    MOVE ME->BDCDATA TO TABLE.
  ENDMETHOD.


  METHOD GET_COMPONENTS_MATERIAL.
    DATA WARNING TYPE CAPIFLAG-FLWARNING.

    CALL FUNCTION 'CSAP_MAT_BOM_READ'
      EXPORTING
        MATERIAL    = MATERIAL
        PLANT       = CENTRO
        BOM_USAGE   = '1'
        ALTERNATIVE = ALTERNATIVA
*       VALID_FROM  =
*       VALID_TO    =
*       CHANGE_NO   =
*       REVISION_LEVEL       =
*       FL_DOC_LINKS         =
*       FL_DMU_TMX  =
      IMPORTING
        FL_WARNING  = WARNING
      TABLES
        T_STPO      = TABLE
*       T_STKO      =
*       T_DEP_DATA  =
*       T_DEP_DESCR =
*       T_DEP_ORDER =
*       T_DEP_SOURCE         =
*       T_DEP_DOC   =
*       T_DOC_LINK  =
*       T_DMU_TMX   =
*       T_LTX_LINE  =
*       T_STPU      =
*       T_SGT_BOMC  =
      EXCEPTIONS
        ERROR       = 1
        OTHERS      = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD get_component_detail.
    DATA lt_class      TYPE TABLE OF sclass.
    DATA lt_return     TYPE TABLE OF bapiret2.
* ---> S4 Migração - 21/06/2023 - FC - Inicio
    DATA objkey_mara   TYPE bapi1003_key-object.
    DATA objkey        TYPE ausp-objek.
* <--- S4 Migração - 21/06/2023 - FC - Fim

    DATA(_objkey_table) =
      VALUE tt_bapi1003_object_keys( ( key_field = 'MATNR'
                                       value_int = material
                                   ) ).

    CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
      EXPORTING
        objecttable    = 'MARA'
      IMPORTING
        objectkey_conc = objkey_mara
      TABLES
        objectkeytable = _objkey_table
        return         = lt_return.

* ---> S4 Migração - MG-5702 - 22/07/2023 - CA
    objkey = objkey_mara.
* <--- S4 Migração - MG-5702 - 22/07/2023 - CA

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
      EXPORTING
        class             = classe
        classtype         = '001'
        features          = 'X'
        language          = sy-langu
        object            = objkey
        objecttable       = 'MARA'
        key_date          = sy-datum
        initial_charact   = initial_charact
      TABLES
        t_class           = lt_class
        t_objectdata      = data
      EXCEPTIONS
        no_classification = 4
        no_classtypes     = 7.

    IF data IS INITIAL.
      RAISE data_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD GET_CUSTOMER.
    MOVE ME->CUSTOMER TO TABLE.
  ENDMETHOD.


  METHOD GET_DELIVERED_QUANTITY.
    ME->SELECT_SHIPMENTS(
      EXPORTING
        EDATU          = ERDAT
        VBELN          = VBELN
        POSNR          = POSNR
      IMPORTING
        DATA           = DATA(_SHIPMENTS)
      EXCEPTIONS
        DATA_NOT_FOUND = 4
    ).

    LOOP AT _SHIPMENTS INTO DATA(_SHIPMENT) WHERE TXT04 = ZCL_PP_SERVICES=>PO_STATUS-CONCLUIDO.
*      SHIFT _SHIPMENT-AUFNR LEFT DELETING LEADING '0'.
*
*      SELECT SINGLE MENGE,MBLNR
*        FROM MSEG
*        INTO @DATA(_MATERIAL_DOCUMENT)
*       WHERE MATNR = @_SHIPMENT-MATNR
*         AND WERKS = @_SHIPMENT-WERKS
*         AND CHARG = @_SHIPMENT-AUFNR
*         AND MJAHR = @SY-DATUM(4)
*         AND BWART = '601'.
*
*      IF SY-SUBRC IS INITIAL.
*        SELECT SINGLE MBLNR
*          FROM MSEG
*          INTO @DATA(_STORNO_DOCUMENT)
*         WHERE MATNR = @_SHIPMENT-MATNR
*           AND WERKS = @_SHIPMENT-WERKS
*           AND CHARG = @_SHIPMENT-AUFNR
*           AND MJAHR = @SY-DATUM(4)
*           AND SMBLN = @_MATERIAL_DOCUMENT-MBLNR
*           AND BWART = '602'.
*
*        IF NOT SY-SUBRC IS INITIAL.
*          VALUE = VALUE + _MATERIAL_DOCUMENT-MENGE.
*        ENDIF.
*      ENDIF.

      VALUE = VALUE + _SHIPMENT-WEMNG.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_DIVISION_PLANNING.
    MOVE ME->OV_DIVISION_PLANNING TO TABLE.
  ENDMETHOD.


  METHOD GET_DIVISION_SHIPMENT.
    MOVE ME->OV_DIVISION_SHIPMENT TO TABLE.
  ENDMETHOD.


  METHOD GET_GREATER_PLANNING_DATE.
    SELECT SINGLE
      MAX( EDATU )
      FROM ZPPT0008
      INTO VALUE
     WHERE VBELN = VBELN
       AND POSNR = POSNR.
  ENDMETHOD.


  METHOD GET_LAST_TIME.

    SELECT *
      FROM ZPPT0009
      INTO TABLE @DATA(T_0009)
      WHERE DT_MODIFICACAO EQ @SY-DATUM
      AND VBELN EQ @VBELN.

    SORT T_0009 BY DT_MODIFICACAO HR_MODIFICACAO DESCENDING.

    IF SY-SUBRC IS INITIAL.
      HORA = SY-UZEIT - T_0009[ 1 ]-HR_MODIFICACAO.
    ELSE.
      HORA = 9999.
    ENDIF.

  ENDMETHOD.


  METHOD GET_MESSAGES.
    MOVE ME->MESSAGES TO TABLE.
  ENDMETHOD.


  METHOD GET_ORDER_BUSINESS.
    MOVE ME->BUSINESS_DATA TO TABLE.
  ENDMETHOD.


  METHOD GET_ORDER_DIVISION.
    MOVE ME->ORDER_DIVISION TO TABLE.
  ENDMETHOD.


  METHOD GET_ORDER_HEADER.
    MOVE ME->ORDER_HEADER TO TABLE.
  ENDMETHOD.


  METHOD GET_ORDER_ITEM.
    MOVE ME->ORDER_ITEM TO TABLE.
  ENDMETHOD.


  METHOD GET_ORDER_PARTNERS.
    MOVE ME->PARTNERS TO TABLE.
  ENDMETHOD.


  METHOD GET_OV_DIVISIONS.
    MOVE ME->OV_DIVISIONS TO TABLE.
  ENDMETHOD.


  METHOD GET_PACKING_WEIGHT.
    DATA(_OV_ITEM) = ME->GET_ORDER_ITEM( ).

    DATA(_COMPONENTS) = ME->GET_COMPONENTS_MATERIAL(
                         MATERIAL    = _OV_ITEM-MATNR
                         CENTRO      = _OV_ITEM-WERKS
                         ALTERNATIVA = '1'
                       ).

    "//Get packaging weight
    LOOP AT _COMPONENTS INTO DATA(_COMPONENT) WHERE COMP_UNIT = 'UN'.
      CALL METHOD ME->GET_COMPONENT_DETAIL
        EXPORTING
          MATERIAL        = _COMPONENT-COMPONENT
          CLASSE          = TP_EMBALAGEM
          INITIAL_CHARACT = ABAP_TRUE
        EXCEPTIONS
          DATA_NOT_FOUND  = 4.

      IF ( SY-SUBRC = 0 ).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = _COMPONENT-COMPONENT
          IMPORTING
            OUTPUT = _COMPONENT-COMPONENT.

        ME->SELECT_MATERIAL(
          EXPORTING
           MATERIAL = _COMPONENT-COMPONENT
          IMPORTING
           T_MARA = DATA(_MARA)
        ).

        VALUE = _MARA-BRGEW * ( _COMPONENT-COMP_QTY * ( QUANTIDADE / 1000 ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_PLANNING.
    MOVE ME->PLANNING TO TABLE.
  ENDMETHOD.


  METHOD GET_PLANNINGS.
    MOVE ME->PLANNINGS TO TABLE.
  ENDMETHOD.


  METHOD GET_PROCESS_ORDER_HEADER.
    MOVE ME->PROCESS_ORDER_HEADER TO TABLE.
  ENDMETHOD.


  METHOD GET_RECIPES.
    DATA ORDER_RECIPE TYPE ZPPET004.
    DATA MATERIAL     TYPE MAKT.

    "//Get orders in production
    DATA(_SHIPMENTS) = ME->SELECT_RECIPES( ).

    LOOP AT _SHIPMENTS INTO DATA(_SHIPMENT).

      "//Get components (recipes) of the material
      CALL METHOD ME->GET_COMPONENTS_MATERIAL
        EXPORTING
          MATERIAL    = _SHIPMENT-MATNR
          CENTRO      = _SHIPMENT-WERKS
          ALTERNATIVA = _SHIPMENT-STLAL
        RECEIVING
          TABLE       = DATA(_COMPONENTS).

      "//Select material description
      CALL METHOD ME->SELECT_MATERIAL
        EXPORTING
          MATERIAL = _SHIPMENT-MATNR
        IMPORTING
          T_MAKT   = MATERIAL.

      SHIFT _SHIPMENT-AUFNR LEFT DELETING LEADING '0'.
      SHIFT _SHIPMENT-KDAUF LEFT DELETING LEADING '0'.
      SHIFT  MATERIAL-MATNR LEFT DELETING LEADING '0'.

      IF ( _SHIPMENT-TXT04 = ME->PO_STATUS-ESTORNADO ).
        ORDER_RECIPE-ESTORNADO = ABAP_TRUE.
      ENDIF.

      ORDER_RECIPE-ORDEM_PRODUCAO = _SHIPMENT-AUFNR.
      ORDER_RECIPE-COD_CLIENTE    = _SHIPMENT-KDAUF.
      ORDER_RECIPE-LOTE           = _SHIPMENT-AUFNR.
      ORDER_RECIPE-MAQUINA        = '1'.
      ORDER_RECIPE-PLACA          = _SHIPMENT-PLACA.
      ORDER_RECIPE-COD_FORMULA    = |{ MATERIAL-MATNR }-{ MATERIAL-MAKTX }|.
      ORDER_RECIPE-PESO_T_TOTAL   = _SHIPMENT-PSMNG.
      ORDER_RECIPE-BAT_PROG       = _SHIPMENT-PSMNG / 1000.
      ORDER_RECIPE-INF_01         = _SHIPMENT-ORDEMCARREG.

      CONDENSE ORDER_RECIPE-BAT_PROG.

      LOOP AT _COMPONENTS INTO DATA(_COMPONENT).
        ME->GET_COMPONENT_DETAIL(
          EXPORTING
            MATERIAL        = _COMPONENT-COMPONENT
            CLASSE          = 'ZFERT_ADITIVO'
          IMPORTING
            DATA            = DATA(_ADITIVOS)
          EXCEPTIONS
            DATA_NOT_FOUND  = 1
            OTHERS          = 2
        ).

        ME->GET_COMPONENT_DETAIL(
          EXPORTING
            MATERIAL        = _COMPONENT-COMPONENT
            CLASSE          = 'ZFERT_TIP_EMBALAGE'
          IMPORTING
            DATA            = DATA(_EMBALAGENS)
          EXCEPTIONS
            DATA_NOT_FOUND  = 1
            OTHERS          = 2
        ).

        CALL METHOD ME->SELECT_MATERIAL
          EXPORTING
            MATERIAL = _COMPONENT-COMPONENT
          IMPORTING
            T_MAKT   = MATERIAL.

        DATA(_COMPONENT_NAME) = |{ _COMPONENT-COMPONENT }-{ MATERIAL-MAKTX }|.

        IF NOT ( _ADITIVOS IS INITIAL ).
          TRY.
              DATA(_COMPORTA_SILO) = _ADITIVOS[ ATNAM = 'COMPORTA_SILO' ]-AUSP1.

              CASE _COMPORTA_SILO.
                WHEN 09. "//Micro-nutriente 01
                  ORDER_RECIPE-PRODUTO_09    = _COMPONENT_NAME.
                  ORDER_RECIPE-PP_PRODUTO_09 = _COMPONENT-COMP_QTY.
                WHEN 10. "//Micro-nutriente 02
                  ORDER_RECIPE-PRODUTO_10    = _COMPONENT_NAME.
                  ORDER_RECIPE-PP_PRODUTO_10 = _COMPONENT-COMP_QTY.
                WHEN 11. "//Anti-dusting
                  ORDER_RECIPE-PRODUTO_11    = _COMPONENT_NAME.
                  ORDER_RECIPE-PP_PRODUTO_11 = _COMPONENT-COMP_QTY.
                WHEN 12. "//Nbpt
                  ORDER_RECIPE-PRODUTO_12    = _COMPONENT_NAME.
                  ORDER_RECIPE-PP_PRODUTO_12 = _COMPONENT-COMP_QTY.
                WHEN 13. "//Megamicro
                  ORDER_RECIPE-PRODUTO_13    = _COMPONENT_NAME.
                  ORDER_RECIPE-PP_PRODUTO_13 = _COMPONENT-COMP_QTY.
              ENDCASE.

            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          ENDTRY.

        ELSEIF NOT ( _EMBALAGENS IS INITIAL ).
          TRY.
              ORDER_RECIPE-EMBALAGEM = _EMBALAGENS[ ATNAM = 'TIPO_DE_EMBALAGEM' ]-AUSP1.
            CATCH CX_SY_ITAB_LINE_NOT_FOUND.
          ENDTRY.

        ELSE.
          "//Macronutrientes
          IF ORDER_RECIPE-PRODUTO_01 IS INITIAL.
            ORDER_RECIPE-PRODUTO_01    = _COMPONENT_NAME.
            ORDER_RECIPE-PP_PRODUTO_01 = _COMPONENT-COMP_QTY.

          ELSEIF ORDER_RECIPE-PRODUTO_02 IS INITIAL.
            ORDER_RECIPE-PRODUTO_02    = _COMPONENT_NAME.
            ORDER_RECIPE-PP_PRODUTO_02 = _COMPONENT-COMP_QTY.

          ELSEIF ORDER_RECIPE-PRODUTO_03 IS INITIAL.
            ORDER_RECIPE-PRODUTO_03    = _COMPONENT_NAME.
            ORDER_RECIPE-PP_PRODUTO_03 = _COMPONENT-COMP_QTY.

          ELSEIF ORDER_RECIPE-PRODUTO_04 IS INITIAL.
            ORDER_RECIPE-PRODUTO_04    = _COMPONENT_NAME.
            ORDER_RECIPE-PP_PRODUTO_04 = _COMPONENT-COMP_QTY.

          ELSEIF ORDER_RECIPE-PRODUTO_05 IS INITIAL.
            ORDER_RECIPE-PRODUTO_05    = _COMPONENT_NAME.
            ORDER_RECIPE-PP_PRODUTO_05 = _COMPONENT-COMP_QTY.

          ELSEIF ORDER_RECIPE-PRODUTO_06 IS INITIAL.
            ORDER_RECIPE-PRODUTO_06    = _COMPONENT_NAME.
            ORDER_RECIPE-PP_PRODUTO_06 = _COMPONENT-COMP_QTY.

          ELSEIF ORDER_RECIPE-PRODUTO_07 IS INITIAL.
            ORDER_RECIPE-PRODUTO_07    = _COMPONENT_NAME.
            ORDER_RECIPE-PP_PRODUTO_07 = _COMPONENT-COMP_QTY.

          ELSEIF ORDER_RECIPE-PRODUTO_08 IS INITIAL.
            ORDER_RECIPE-PRODUTO_08    = _COMPONENT_NAME.
            ORDER_RECIPE-PP_PRODUTO_08 = _COMPONENT-COMP_QTY.
          ENDIF.
        ENDIF.
      ENDLOOP.

      APPEND ORDER_RECIPE TO TABLE.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_SCHEDULE_LINES.
    MOVE ME->SCHED_LINES  TO LINES.
    MOVE ME->SCHED_LINESX TO LINESX.
  ENDMETHOD.


  METHOD GET_SHIPMENT.
    MOVE ME->SHIPMENT TO TABLE.
  ENDMETHOD.


  METHOD GET_SHIPMENTS.
    MOVE ME->SHIPMENTS TO TABLE.
  ENDMETHOD.


  METHOD GET_SHIPMENTS_TRANSIT.
    MOVE ME->SHIPMENTS_TRANSIT TO TABLE.
  ENDMETHOD.


  METHOD GET_SHIPMENT_TRANSIT.
    MOVE ME->SHIPMENT_TRANSIT TO TABLE.
  ENDMETHOD.


  METHOD GET_SIMULADOR_VENDA_HEADER.
    MOVE ME->HEADER_SIMULADOR_VENDA TO TABLE.
  ENDMETHOD.


  METHOD get_text_sd.

    DATA: lv_matnr    TYPE mara-matnr,
          lv_matnr2   TYPE matnr18,
          lv_name_obj TYPE thead-tdname,
          lv_space    TYPE char22 VALUE '                      '.

    SELECT SINGLE sprsl
       FROM t002t
       INTO @DATA(vl_lang)
       WHERE sptxt EQ @werks
         AND spras EQ @sy-langu.

** RIM - SKM - IR121437 - 13.01.23 - Inicio
**    SELECT SINGLE sprsl
**       FROM t002t
**       INTO @DATA(vl_lang)
**       WHERE sptxt EQ @werks
**         AND spras EQ @sy-langu.
*data vl_lang like sy-langu.
** RIM - SKM - IR121437 - 13.01.23 - Fim

    SELECT SINGLE *
      from t001w INTO @DATA(lwa_t001w)
     WHERE werks eq  @werks.

    CHECK sy-subrc eq 0.

    SELECT SINGLE *
      FROM mvke
      INTO @DATA(lw_mvke)
     WHERE matnr  = @matnr
       AND vkorg  = @lwa_t001w-vkorg.

    CHECK sy-subrc eq 0.

    DATA name_obj TYPE thead-tdname.
    name_obj = lw_mvke-matnr && lw_mvke-vkorg && lw_mvke-vtweg.

    IF strlen( lw_mvke-matnr ) LE 18.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lw_mvke-matnr
        IMPORTING
          output = lv_matnr2.

      lv_matnr = lv_matnr2.
      CONCATENATE lv_matnr lw_mvke-vkorg INTO lv_name_obj SEPARATED BY lv_space.
      lv_name_obj = lv_name_obj && lw_mvke-vtweg.

    ENDIF.


    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = '0001'
        language                = COND #( WHEN vl_lang IS INITIAL THEN sy-langu ELSE vl_lang )
        name                    = name_obj
        object                  = 'MVKE'
      TABLES
        lines                   = text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc IS NOT INITIAL.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = '0001'
          language                = COND #( WHEN vl_lang IS INITIAL THEN sy-langu ELSE vl_lang )
          name                    = lv_name_obj
          object                  = 'MVKE'
        TABLES
          lines                   = text
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
    ENDIF.
  ENDMETHOD.


  METHOD GET_TEXT_STATUS.
    SELECT SINGLE *
      FROM TJ30T
      INTO TABLE
     WHERE STSMA = 'ZPI00001'
       AND TXT04 = STATUS.
  ENDMETHOD.


  METHOD HAS_MESSAGES.
    VALUE  = COND #( WHEN LINES( ME->MESSAGES ) > 0 THEN ABAP_TRUE ELSE ABAP_FALSE ).
  ENDMETHOD.


  METHOD INSERT_ORDER_DIVISION.
    DATA(NEW_SCHEDULE)  =
      VALUE BAPISCHDL(
             ITM_NUMBER = POSNR
             REQ_DATE   = EDATU
             REQ_QTY    = WMENG
             DATE_TYPE  = 1
             REQ_DLV_BL = LIFSP
             SCHED_TYPE = 'CP'
             REFOBJKEY  = OBJKEY ).

    DATA(NEW_SCHEDULE_X) =
      VALUE BAPISCHDLX(
             ITM_NUMBER = POSNR
             UPDATEFLAG = ZUTIL_CRUD-INSERT
             REQ_DATE   = ABAP_TRUE
             DATE_TYPE  = ABAP_TRUE
             REQ_QTY    = ABAP_TRUE
             REQ_DLV_BL = ABAP_TRUE
             SCHED_TYPE = ABAP_TRUE
             REFOBJKEY  = SWITCH #( OBJKEY WHEN SPACE THEN ABAP_FALSE ELSE ABAP_TRUE )
                      ).

    APPEND NEW_SCHEDULE   TO ME->SCHED_LINES.
    APPEND NEW_SCHEDULE_X TO ME->SCHED_LINESX.
  ENDMETHOD.


  METHOD insert_order_xi.
    CHECK quantidade >= 0.

    DATA: lt_partners TYPE TABLE OF vbpa,
          wa_envio    TYPE  zsde0001.
    MOVE me->get_order_partners( ) TO lt_partners.

*    CALL FUNCTION 'Z_SD_OUTBOUND_ORD_VENDA' IN BACKGROUND TASK
*      DESTINATION 'XI_ORDEM_VENDA'
*      EXPORTING
*        nu_ordem_venda = ordem
*        tp_ordem_venda = tipo
*        nu_item        = item
*        dt_ordem_venda = data
*        tp_frete       = incoterms
*        id_cliente     = emissor
*        qt_ordem_venda = CONV kwmeng( quantidade )
*        cd_material    = material
*        vr_unitario    = vlr_unitario
*        cd_safra       = CONV char10( safra )
*        cd_empresa     = empresa
*        cd_centro      = centro
*        cd_moeda       = moeda
*        desc_material  = descricao
*        st_atualizacao = 'A'
*        status         = 'A'
*        st_atividade   = st_atividade
*        dt_atualizacao = sy-datum
*        hr_atualizacao = sy-uzeit
*        rg_atualizado  = '0'
*        id_interface   = '14'
*      TABLES
*        it_vbpa        = lt_partners.

    wa_envio-nu_ordem_venda = ordem.
    wa_envio-tp_ordem_venda = tipo.
    wa_envio-nu_item        = item.
    wa_envio-dt_ordem_venda = data.
    wa_envio-tp_frete       = incoterms.
    wa_envio-id_cliente     = emissor.
    wa_envio-qt_ordem_venda = CONV kwmeng( quantidade ).
    wa_envio-cd_material    = material.
    wa_envio-vr_unitario    = vlr_unitario.
    wa_envio-cd_safra       = CONV char10( safra ).
    wa_envio-cd_empresa     = empresa.
    wa_envio-cd_centro      = centro.
    wa_envio-cd_moeda       = moeda.
    wa_envio-desc_material  = descricao.
    wa_envio-st_atualizacao = 'A'.
    wa_envio-status         = 'A'.
    wa_envio-st_atividade   = st_atividade.
    wa_envio-dt_atualizacao = sy-datum.
    wa_envio-hr_atualizacao = sy-uzeit.
    wa_envio-rg_atualizado  = '0'.
    wa_envio-id_interface   = '14'.
    wa_envio-vbpa           = lt_partners.

    CALL FUNCTION 'ZSDMF_ENVIA_ORDEM_XI'
      EXPORTING
        ordem = wa_envio.

  ENDMETHOD.


  METHOD insert_planning_xi.

*--> 25.08.2023 11:47:24 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_SD_OUTBOUND_PLANEJAMENTO_OV' IN BACKGROUND TASK
*    DESTINATION 'XI_PLANEJAMENTO_OV'
*    EXPORTING
*      VBELN  = VBELN
*      POSNR  = POSNR
*      EDATU  = EDATU
*      BMENG  = BMENG
*      STATUS = STATUS.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_PLANEJAMENTO_OV'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        EXPORTING
          vbeln  = vbeln
          posnr  = posnr
          edatu  = edatu
          bmeng  = bmeng
          status = status.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        EXPORTING
          vbeln  = vbeln
          posnr  = posnr
          edatu  = edatu
          bmeng  = bmeng
          status = status.
    ENDIF.

    COMMIT WORK.
*<-- 25.08.2023 11:47:24 - Migração S4 – ML – Fim

*    COMMIT WORK.
  ENDMETHOD.


  METHOD INSERT_SHIPMENT_TRANSIT.
    DATA(_ITEM) = VALUE ZPPT0009( VBELN        = ORDEM_VENDA
                                  POSNR        = ITEM
                                  ORDEM_CARREG = ORDEM_CARREGAMENTO
                                  QUANTIDADE   = QUANTIDADE
                                  DATA         = DATA
                                  PLACA        = PLACA
                                  DT_MODIFICACAO = SY-DATUM
                                  HR_MODIFICACAO = SY-UZEIT
                                  ).

    INSERT ZPPT0009 FROM _ITEM.
    COMMIT WORK.
  ENDMETHOD.


  METHOD IS_RESERVATIONS_AVAILABLE.
    DATA WMARD TYPE MARD.
    DATA OMARD TYPE MARD.

    SELECT *
      FROM RESB
      INTO TABLE @DATA(_RESERVATIONS)
     WHERE RSNUM = @RESERVATION_NO.

    LOOP AT _RESERVATIONS INTO DATA(_RESERVATION).

      IF ( CHECK_IS_LOCKED = ABAP_TRUE ).
        CALL FUNCTION 'ENQUEUE_EMMARCE'
          EXPORTING
            MODE_MARC      = 'E'
            MANDT          = SY-MANDT
            MATNR          = _RESERVATION-MATNR
            WERKS          = _RESERVATION-WERKS
            _SCOPE         = '2'
          EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.

        IF SY-SUBRC <> 0.
          RAISE EXCEPTION TYPE ZCX_PP_SERVICES
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_PP_SERVICES=>MATERIAL_LOCKED-MSGID
                                MSGNO = ZCX_PP_SERVICES=>MATERIAL_LOCKED-MSGNO
                                ATTR1 = _RESERVATION-MATNR
                                ATTR2 = SY-MSGV1 ).
        ENDIF.

        SELECT *
          FROM MCHB
          INTO TABLE @DATA(_LOTES)
         WHERE MATNR = @_RESERVATION-MATNR
           AND WERKS = @_RESERVATION-WERKS
           AND LGORT = @_RESERVATION-LGORT.

        LOOP AT _LOTES INTO DATA(_LOTE).
          CALL FUNCTION 'ENQUEUE_EMMCH1E'
            EXPORTING
              MODE_MCH1      = 'E'
              MANDT          = SY-MANDT
              MATNR          = _LOTE-MATNR
              CHARG          = _LOTE-CHARG
              _SCOPE         = '2'
            EXCEPTIONS
              FOREIGN_LOCK   = 1
              SYSTEM_FAILURE = 2
              OTHERS         = 3.

          IF SY-SUBRC <> 0.
            RAISE EXCEPTION TYPE ZCX_PP_SERVICES
              EXPORTING
                TEXTID = VALUE #( MSGID = ZCX_PP_SERVICES=>LOTE_IS_LOCKED-MSGID
                                  MSGNO = ZCX_PP_SERVICES=>LOTE_IS_LOCKED-MSGNO
                                  ATTR1 = _LOTE-CHARG
                                  ATTR2 = _LOTE-MATNR
                                  ATTR3 = SY-MSGV1 ).
          ENDIF.
        ENDLOOP.

        CLEAR _LOTES.
      ENDIF.

      CALL FUNCTION 'MARD_SINGLE_READ'
        EXPORTING
          MATNR      = _RESERVATION-MATNR
          WERKS      = _RESERVATION-WERKS
          LGORT      = _RESERVATION-LGORT
        IMPORTING
          WMARD      = WMARD
          O_MARD     = OMARD
        EXCEPTIONS
          WRONG_CALL = 1
          NOT_FOUND  = 2
          OTHERS     = 3.

      IF SY-SUBRC <> 0.
        RAISE EXCEPTION TYPE ZCX_PP_SERVICES
          EXPORTING
            TEXTID = VALUE #( MSGID = SY-MSGID
                              MSGNO = SY-MSGNO
                              ATTR1 = SY-MSGV1
                              ATTR2 = SY-MSGV2
                              ATTR3 = SY-MSGV3
                              ATTR4 = SY-MSGV4
                            ).
      ENDIF.

      IF ( WMARD-LABST < _RESERVATION-BDMNG ).
        SHIFT _RESERVATION-MATNR LEFT DELETING LEADING '0'.
        SHIFT _RESERVATION-AUFNR LEFT DELETING LEADING '0'.

        RAISE EXCEPTION TYPE ZCX_PP_SERVICES
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_PP_SERVICES=>MATERIAL_BALANCE_INSUFFICIENT-MSGID
                              MSGNO = ZCX_PP_SERVICES=>MATERIAL_BALANCE_INSUFFICIENT-MSGNO
                              ATTR1 = _RESERVATION-MATNR
                              ATTR2 = _RESERVATION-AUFNR ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD MODIFY_PLANNING.
    MODIFY ZPPT0008 FROM ITEM.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD MODIFY_PLANNINGS.
    MODIFY ZPPT0008 FROM TABLE ITEM.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD PROCESS_ORDER_IS_AUTHORIZED.
    SELECT SINGLE *
      FROM AUFK
    INNER JOIN JEST AS B ON AUFK~OBJNR = B~OBJNR
      INTO @DATA(T_PROCESS_ORDERS)
     WHERE AUFK~AUFNR = @PROCESS_ORDER
       AND AUFK~WERKS = '0175'
       AND B~STAT     = 'E0002'
       AND B~INACT    = ' '.

    CHECK NOT ( SY-SUBRC IS INITIAL ).
    RAISE PROCESS_ORDER_NOT_AUTHORIZED.
  ENDMETHOD.


  METHOD PROCESS_ORDER_IS_LOCKED.
    DATA FLAG_ENQUEUE_OK TYPE ABAP_BOOL.

    CHECK ORDER IS NOT INITIAL.

    IF ( WAIT IS INITIAL ).
      CALL FUNCTION 'ENQUEUE_ESORDER'
        EXPORTING
          AUFNR          = ORDER
          _SCOPE         = '2'
        EXCEPTIONS
          FOREIGN_LOCK   = 1
          SYSTEM_FAILURE = 2
          OTHERS         = 3.

      IF NOT SY-SUBRC IS INITIAL.
        RAISE EXCEPTION TYPE ZCX_PP_SERVICES
          EXPORTING
            TEXTID = VALUE #( MSGID = 'CO'
                              MSGNO = 726
                              ATTR1 = SY-MSGV1
                              ATTR2 = ORDER
                            ).
      ENDIF.

    ELSE.
      "//It should only be used to check if the order is released after some execution of a BAPI.
      "//Using ENQUEUE with mode 'V' the system wait until the object be released.
      DO.
        CALL FUNCTION 'ENQUEUE_ESORDER'
          EXPORTING
            MODE_AUFK      = 'V'
            MANDT          = SY-MANDT
            AUFNR          = ORDER
            _SCOPE         = '2'
            _WAIT          = ABAP_TRUE
          EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.

        IF SY-SUBRC IS INITIAL.
          WAIT UP TO 3 SECONDS.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD REFRESH_SCHEDULE_LINES.
    REFRESH ME->SCHED_LINES.
    REFRESH ME->SCHED_LINESX.
  ENDMETHOD.


  METHOD release_process_order.
    DATA return_details TYPE TABLE OF bapi_order_return.
    DATA return         TYPE bapiret2.
    DATA: ls_ordem     TYPE zppt0038,
          lv_warning   TYPE capiflag-flwarning,
          lt_stpo      TYPE TABLE OF stpo_api02,
          lt_stko      TYPE TABLE OF stko_api02,
          lv_cont      TYPE numc2,
          lv_cod_prod  TYPE string,
          lv_peso_prod TYPE string,
          lv_timestamp TYPE  timestamp,
          lv_iso       TYPE string,
          le_ordem     TYPE zppe_supervisorio,
          lv_string    TYPE string,
          lr_matnr     TYPE RANGE OF matnr18,
          lv_matnr     TYPE matnr18.

    FIELD-SYMBOLS: <fs_cod_prod>  TYPE string,
                   <fs_peso_prod> TYPE zppt0038-peso_produto_03.

    DATA(_order) =
      VALUE tb_bapi_order_key( ( order_number = order ) ).

    CALL FUNCTION 'BAPI_PROCORD_RELEASE'
      EXPORTING
        release_control    = '1'
        work_process_group = 'COWORK_BAPI'
        work_process_max   = 99
      IMPORTING
        return             = return
      TABLES
        orders             = _order
        detail_return      = return_details.

    DELETE return_details WHERE type NE 'E'.

    IF NOT return_details IS INITIAL.
      me->set_messages( return_details ).
      RAISE process_order_not_released.
    ENDIF.

    "//Set lote at process order, it must exist;
    me->set_batch_process_order( aufnr = order charg = lote ).

    "//Change user status;
    TRY.
        me->change_po_user_status( order = order status = me->po_status-em_producao ).
      CATCH zcx_pp_services.
    ENDTRY.

    SELECT SINGLE *
      FROM tvarvc
      INTO @DATA(ls_tvarvc)
      WHERE name = 'Z_AUT_FAB_FERTILIZANTES'
        AND type = 'P'
        AND numb = '0000'.
    IF sy-subrc IS INITIAL.

      IF ls_tvarvc-low = me->shipment-werks.

        CALL FUNCTION 'CSAP_MAT_BOM_READ'
          EXPORTING
            material   = me->shipment-matnr
            plant      = me->shipment-werks
            bom_usage  = '1'
          IMPORTING
            fl_warning = lv_warning
          TABLES
            t_stpo     = lt_stpo
            t_stko     = lt_stko
          EXCEPTIONS
            error      = 1
            OTHERS     = 2.
        IF sy-subrc IS INITIAL.

          IF lt_stpo IS NOT INITIAL.

            LOOP AT lt_stpo ASSIGNING FIELD-SYMBOL(<fs_stpo>).
              IF <fs_stpo>-comp_unit <> 'KG'.
                CONTINUE.
              ENDIF.

              APPEND INITIAL LINE TO lr_matnr ASSIGNING FIELD-SYMBOL(<fs_matnr>).

              <fs_matnr>-sign = 'I'.
              <fs_matnr>-option = 'EQ'.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs_stpo>-component
                IMPORTING
                  output = <fs_matnr>-low.

            ENDLOOP.

            SELECT *
              FROM makt
              INTO TABLE @DATA(lt_makt)
              WHERE matnr IN @lr_matnr
                AND spras = @sy-langu.
            IF sy-subrc IS INITIAL.
              SORT lt_makt BY matnr.
            ENDIF.
          ENDIF.

          GET TIME STAMP FIELD lv_timestamp.

          CALL METHOD cl_xlf_date_time=>create
            EXPORTING
              timestamp = lv_timestamp
            RECEIVING
              iso8601   = lv_iso.

          SELECT SINGLE vbeln,kunnr
            FROM vbak
            INTO @DATA(ls_vbak)
            WHERE vbeln = @me->shipment-kdauf.
          IF sy-subrc IS INITIAL.
            SELECT SINGLE kunnr,name1
              FROM kna1
              INTO @DATA(ls_kna1)
              WHERE kunnr = @ls_vbak-kunnr.
            IF sy-subrc IS INITIAL.
              ls_ordem-nome_cliente = ls_kna1-name1.
            ENDIF.
          ENDIF.

          ls_ordem-ordem       = me->shipment-aufnr.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = ls_ordem-ordem
            IMPORTING
              output = ls_ordem-ordem.

          ls_ordem-peso        = me->shipment-psmng.
          ls_ordem-e3timestamp = lv_iso.

          SELECT SINGLE maktx
            FROM makt
            INTO ls_ordem-formula
            WHERE matnr = me->shipment-matnr
              AND spras = sy-langu.

          ls_ordem-placa       = me->shipment-placa.
          ls_ordem-ativo       = '1'.
          ls_ordem-unidade     = me->shipment-werks.
          ls_ordem-sinc        = '1'.

          LOOP AT lt_stpo ASSIGNING <fs_stpo>.

            IF <fs_stpo>-comp_unit <> 'KG'.
              CONTINUE.
            ENDIF.

            lv_cont = lv_cont + 1.

            lv_cod_prod  = 'codigo_produto_' && lv_cont.
            lv_peso_prod = 'peso_produto_' && lv_cont.

            READ TABLE lt_stko ASSIGNING FIELD-SYMBOL(<fs_stko>) INDEX 1.

            ASSIGN COMPONENT lv_cod_prod OF STRUCTURE ls_ordem TO <fs_cod_prod>.
            IF <fs_cod_prod> IS ASSIGNED.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = <fs_stpo>-component
                IMPORTING
                  output = lv_matnr.

              READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<fs_makt>)
              WITH KEY matnr = lv_matnr
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                <fs_cod_prod> = <fs_makt>-maktx.
              ENDIF.

            ENDIF.

            ASSIGN COMPONENT lv_peso_prod OF STRUCTURE ls_ordem TO <fs_peso_prod>.
            IF <fs_peso_prod> IS ASSIGNED.
              IF <fs_stpo>-comp_qty IS INITIAL OR <fs_stpo>-comp_qty EQ 0.
                <fs_peso_prod> = 'null'.
              ELSE.
                <fs_peso_prod> = <fs_stpo>-comp_qty.
              ENDIF.

            ENDIF.

          ENDLOOP.

          MOVE-CORRESPONDING ls_ordem TO le_ordem.

          ls_ordem-status = 'NÃO SINCRONIZADO'.
          MODIFY zppt0038 FROM ls_ordem.

        ENDIF.

*          TRY .
*
*              zcl_int_ob_get_ordem_superv=>zif_integracao_outbound~get_instance( )->execute_request(  EXPORTING i_info_request = me->shipment-aufnr IMPORTING e_integracao = DATA(r_response) ).
*
*            CATCH zcx_integracao INTO DATA(zcx_integracao).
*
*            CATCH zcx_error INTO DATA(zcx_error).
*
*          ENDTRY.
*
*          IF r_response IS NOT INITIAL.
*
*            lv_string = r_response-ds_data_retorno.

*            IF lv_string CS 'error'.

        TRY .
            zcl_int_ob_cria_ordem_supervis=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = le_ordem ).
          CATCH zcx_integracao INTO DATA(zcx_integracao).

          CATCH zcx_error INTO DATA(zcx_error).

        ENDTRY.

*            ELSE.
*
*              TRY .
*                  zcl_int_ob_ativa_ordem_superv=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = me->shipment-aufnr ).
*                CATCH zcx_integracao INTO zcx_integracao.
*
*                CATCH zcx_error INTO zcx_error.
*
*              ENDTRY.
**
*            ENDIF.
*
*          ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD SELECT_AUTHORIZED_ORDERS.
    SELECT *
      FROM AUFK
INNER JOIN JEST AS B ON AUFK~OBJNR = B~OBJNR
      INTO CORRESPONDING FIELDS OF TABLE TABLE
     WHERE AUFK~WERKS = '0175'
       AND B~STAT     = 'E0001'
       AND B~INACT    = ' '.
  ENDMETHOD.


  METHOD SELECT_BATCH.
    SELECT SINGLE *
      FROM MCHB
      INTO @DATA(_BATCH)
     WHERE MATNR = @MATERIAL
       AND WERKS = @CENTRO
       AND LGORT = @DEPOSITO
       AND CHARG = @LOTE.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE BATCH_NOT_FOUND.
    ENDIF.
  ENDMETHOD.


  METHOD SELECT_CUSTOMER.
    SELECT SINGLE *
      FROM KNA1
      INTO ME->CUSTOMER
     WHERE KUNNR = KUNNR.

    MOVE ME->CUSTOMER TO DATA.
  ENDMETHOD.


  METHOD select_division_planning.

    DATA: lv_total_bloq  TYPE vbep-wmeng,
          difference_qty TYPE vbep-wmeng.

    DATA(_shipment)          = me->get_shipment( ).
    difference_qty = quantity - _shipment-psmng.

    SELECT *
      FROM vbep
      INTO TABLE me->ov_division_planning
     WHERE vbeln = vbeln
       AND posnr = posnr
       AND wmeng <> 0
       AND lifsp = '10'.

    IF sy-subrc IS INITIAL.
      LOOP AT me->ov_division_planning ASSIGNING FIELD-SYMBOL(<fs_div_plan>).
        lv_total_bloq = lv_total_bloq + <fs_div_plan>-wmeng.
      ENDLOOP.

      IF operation = 'CREATE_PO'.

        IF quantity > 0 AND quantity > lv_total_bloq.
          erro =  zcx_pp_services=>largest_quantity.
        ENDIF.

      ELSEIF operation = 'CONCLU_PO'.

        IF difference_qty > lv_total_bloq.
          erro = zcx_pp_services=>largest_quantity_prod.
        ENDIF.

      ENDIF.

    ELSEIF operation = 'CONCLU_PO' AND difference_qty > 0.
      erro = zcx_pp_services=>largest_quantity_prod.
    ENDIF.

  ENDMETHOD.


  METHOD select_division_shipment.
    SELECT SINGLE *
      FROM vbep
      INTO me->ov_division_shipment
     WHERE vbeln = vbeln
       AND posnr = posnr
       AND edatu = edatu
       AND wmeng <> 0
       AND lifsp = space.
  ENDMETHOD.


  method select_material.
    data: _material type makt-matnr,
          lv_matnr2 type matnr18.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = material
      importing
        output = _material.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = material
      importing
        output = lv_matnr2.

    select single *
      from mara
      into t_mara
     where matnr = _material.
    if sy-subrc is not initial.
      select single *
        from mara
        into t_mara
       where matnr = lv_matnr2.
    endif.

    if t_mara is not initial.

      select single *
        from marc
        into t_marc
       where matnr = t_mara-matnr
         and werks = centro. "BUG SOLTO 165396

      select single *
        from makt
        into t_makt
       where matnr = t_mara-matnr.

    endif.
  endmethod.


  METHOD SELECT_MSKA.

    SELECT SINGLE *
      FROM MSKA
      INTO ME->MSKA
     WHERE VBELN = VBELN
       AND POSNR = POSNR
       AND CHARG = CHARG.

    CHECK SY-SUBRC IS INITIAL.

    CHECK ME->GET_SHIPMENT( )-WEMNG NE ME->MSKA-KALAB.

    RAISE EXCEPTION TYPE ZCX_PP_SERVICES
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PP_SERVICES=>ORDER_ALREADY_MOVIMENT-MSGID
                          MSGNO = ZCX_PP_SERVICES=>ORDER_ALREADY_MOVIMENT-MSGNO
                          ATTR1 = CHARG
                         ).

  ENDMETHOD.


  METHOD SELECT_ORDER_BUSINESS.
    SELECT SINGLE *
      FROM VBKD
      INTO ME->BUSINESS_DATA
     WHERE VBELN = VBELN.
  ENDMETHOD.


  METHOD SELECT_ORDER_DIVISION.
    CLEAR ME->ORDER_DIVISION.

    IF EDATU IS INITIAL.
      SELECT SINGLE *
        FROM VBEP
        INTO ORDER_DIVISION
       WHERE VBELN = VBELN
         AND POSNR = POSNR
         AND WMENG <> 0
         AND LIFSP = LIFSP.
    ELSE.
      SELECT SINGLE *
        FROM VBEP
        INTO ORDER_DIVISION
       WHERE VBELN = VBELN
         AND POSNR = POSNR
         AND EDATU = EDATU
         AND WMENG <> 0
         AND LIFSP = LIFSP.
    ENDIF.

    MOVE ME->ORDER_DIVISION TO ITEM.
  ENDMETHOD.


  METHOD SELECT_ORDER_HEADER.
    SELECT SINGLE *
      FROM VBAK
      INTO ME->ORDER_HEADER
     WHERE VBELN = VBELN.
  ENDMETHOD.


  METHOD SELECT_ORDER_ITEM.
    SELECT SINGLE *
      FROM VBAP
      INTO ME->ORDER_ITEM
     WHERE VBELN = VBELN
       AND POSNR = POSNR.
  ENDMETHOD.


  METHOD SELECT_ORDER_PARTNERS.
    SELECT *
      FROM VBPA
      INTO TABLE ME->PARTNERS
     WHERE VBELN = ORDEM.
  ENDMETHOD.


  METHOD SELECT_OV_DIVISIONS.
    CLEAR ME->OV_DIVISIONS.

    SELECT *
      FROM VBEP
      INTO TABLE ME->OV_DIVISIONS
     WHERE VBELN = VBELN
       AND POSNR = POSNR.

    MOVE ME->OV_DIVISIONS TO ITEMS.
  ENDMETHOD.


  METHOD SELECT_PLANNING.
    DATA DATE_EXTERNAL TYPE STRING.
    CLEAR ME->PLANNING.

    SELECT SINGLE *
      FROM ZPPT0008
      INTO ME->PLANNING
     WHERE VBELN = VBELN
       AND POSNR = POSNR
       AND EDATU = EDATU.

    CHECK SY-SUBRC IS NOT INITIAL.
    TRY.
        CL_ABAP_DATFM=>CONV_DATE_INT_TO_EXT(
            EXPORTING IM_DATINT = EDATU
            IMPORTING EX_DATEXT = DATE_EXTERNAL ).
      CATCH CX_ABAP_DATFM_FORMAT_UNKNOWN.
    ENDTRY.

    RAISE EXCEPTION TYPE ZCX_PP_SERVICES
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PP_SERVICES=>PLANNING_NOT_FOUND-MSGID
                          MSGNO = ZCX_PP_SERVICES=>PLANNING_NOT_FOUND-MSGNO
                          ATTR1 = DATE_EXTERNAL
                          ATTR2 = VBELN
                          ATTR3 = POSNR
                         ).
  ENDMETHOD.


  METHOD SELECT_PLANNINGS.
    SELECT *
      FROM ZPPT0008
INTO TABLE ME->PLANNINGS
     WHERE VBELN = VBELN
       AND POSNR = POSNR.
  ENDMETHOD.


  METHOD SELECT_PROCESS_ORDER_HEADER.
    SELECT SINGLE *
      FROM AFKO
     INNER JOIN AUFK AS A ON A~AUFNR = AFKO~AUFNR
      INTO CORRESPONDING FIELDS OF ME->PROCESS_ORDER_HEADER
     WHERE A~ORDEMCARREG = ORDEM_CARREGAMENTO.
  ENDMETHOD.


  METHOD SELECT_RECIPES.
    SELECT *
      FROM AUFK AS A
INNER JOIN AFPO  AS B ON B~AUFNR = A~AUFNR
INNER JOIN JEST  AS D ON D~OBJNR = A~OBJNR
INNER JOIN TJ30T AS E ON E~ESTAT = D~STAT
INNER JOIN AFKO  AS F ON F~AUFNR = A~AUFNR
      INTO CORRESPONDING FIELDS OF TABLE TABLE
     WHERE A~WERKS = '0175'
       AND D~INACT = ' '
       AND E~TXT04 = ME->PO_STATUS-EM_PRODUCAO
       AND E~STSMA = 'ZPI00001'.

    SELECT *
      FROM AUFK AS A
INNER JOIN AFPO  AS B ON B~AUFNR = A~AUFNR
INNER JOIN JEST  AS D ON D~OBJNR = A~OBJNR
INNER JOIN TJ30T AS E ON E~ESTAT = D~STAT
INNER JOIN AFKO  AS F ON F~AUFNR = A~AUFNR
 APPENDING CORRESPONDING FIELDS OF TABLE TABLE
     WHERE A~WERKS = '0175'
       AND A~AEDAT = SY-DATUM
       AND D~INACT = ' '
       AND E~TXT04 = ME->PO_STATUS-ESTORNADO
       AND E~STSMA = 'ZPI00001'.
  ENDMETHOD.


  METHOD select_shipment.
    IF ( date IS INITIAL ).
      SELECT SINGLE *
        FROM aufk  AS a
  INNER JOIN afpo  AS b ON b~aufnr = a~aufnr
  INNER JOIN jest  AS d ON d~objnr = a~objnr
  INNER JOIN tj30t AS e ON e~estat = d~stat
  INNER JOIN afko  AS f ON f~aufnr = a~aufnr
      INTO CORRESPONDING FIELDS OF me->shipment
     WHERE a~ordemcarreg = ordem_carregamento
       AND a~erdat = ( SELECT MAX( erdat ) FROM aufk WHERE ordemcarreg = ordem_carregamento )
       AND d~inact = ' '
       AND e~stsma = 'ZPI00001'.
    ELSE.
      SELECT SINGLE *
        FROM aufk  AS a
  INNER JOIN afpo  AS b ON b~aufnr = a~aufnr
  INNER JOIN jest  AS d ON d~objnr = a~objnr
  INNER JOIN tj30t AS e ON e~estat = d~stat
  INNER JOIN afko  AS f ON f~aufnr = a~aufnr
      INTO CORRESPONDING FIELDS OF me->shipment
     WHERE a~ordemcarreg = ordem_carregamento
       AND a~erdat = date
       AND d~inact = ' '
       AND e~stsma = 'ZPI00001'.
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      RAISE shipment_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD SELECT_SHIPMENTS.
    CLEAR ME->SHIPMENTS.

    IF ( EDATU IS INITIAL ).
      "//Select all shipments by order/item
      SELECT *
        FROM AUFK AS A
  INNER JOIN AFPO  AS B ON B~AUFNR = A~AUFNR
  INNER JOIN JEST  AS D ON D~OBJNR = A~OBJNR
  INNER JOIN TJ30T AS E ON E~ESTAT = D~STAT
  INNER JOIN AFKO  AS F ON F~AUFNR = A~AUFNR
        INTO CORRESPONDING FIELDS OF TABLE ME->SHIPMENTS
       WHERE A~KDAUF = VBELN
         AND A~KDPOS = POSNR
         AND D~INACT = ' '
         AND E~STSMA = 'ZPI00001'.

    ELSEIF VBELN IS INITIAL.
      "//Select all shipments by order/item
      SELECT *
        FROM AUFK AS A
  INNER JOIN AFPO  AS B ON B~AUFNR = A~AUFNR
  INNER JOIN JEST  AS D ON D~OBJNR = A~OBJNR
  INNER JOIN TJ30T AS E ON E~ESTAT = D~STAT
  INNER JOIN AFKO  AS F ON F~AUFNR = A~AUFNR
        INTO CORRESPONDING FIELDS OF TABLE ME->SHIPMENTS
       WHERE A~ERDAT EQ EDATU
         AND A~WERKS EQ WERKS
         AND D~INACT EQ ' '
         AND E~STSMA EQ 'ZPI00001'.
    ELSE.
*      "//Select shipments by order/item/date
      SELECT *
        FROM AUFK AS A
  INNER JOIN AFPO  AS B ON B~AUFNR = A~AUFNR
  INNER JOIN JEST  AS D ON D~OBJNR = A~OBJNR
  INNER JOIN TJ30T AS E ON E~ESTAT = D~STAT
  INNER JOIN AFKO  AS F ON F~AUFNR = A~AUFNR
        INTO CORRESPONDING FIELDS OF TABLE ME->SHIPMENTS
       WHERE A~KDAUF = VBELN
         AND A~KDPOS = POSNR
         AND A~ERDAT = EDATU
         AND D~INACT = ' '
         AND E~STSMA = 'ZPI00001'.
    ENDIF.

    IF ESTOR = ABAP_FALSE. "//Estornados
      DELETE ME->SHIPMENTS WHERE TXT04 EQ 'ESTN'.
      DELETE ME->SHIPMENTS WHERE TXT04 EQ 'PSVA'.
    ENDIF.

    MOVE ME->SHIPMENTS TO DATA.

    IF ( DATA IS INITIAL ).
      RAISE DATA_NOT_FOUND.
    ENDIF.
  ENDMETHOD.


  METHOD SELECT_SHIPMENTS_TRANSIT.
    CLEAR ME->SHIPMENTS_TRANSIT.

    IF DATE IS INITIAL.
      SELECT *
        FROM ZPPT0009
        INTO TABLE ME->SHIPMENTS_TRANSIT
       WHERE VBELN = VBELN
         AND POSNR = POSNR
         AND STATUS NE ABAP_TRUE.
    ELSE.
      SELECT *
        FROM ZPPT0009
        INTO TABLE ME->SHIPMENTS_TRANSIT
       WHERE VBELN = VBELN
         AND POSNR = POSNR
         AND DATA  = DATE
         AND STATUS NE ABAP_TRUE.
    ENDIF.

    MOVE ME->SHIPMENTS_TRANSIT TO DATA.

    IF ( DATA IS INITIAL ).
      RAISE DATA_NOT_FOUND.
    ENDIF.
  ENDMETHOD.


  METHOD SELECT_SHIPMENT_TRANSIT.

    SELECT SINGLE *
      FROM ZPPT0009
      INTO ME->SHIPMENT_TRANSIT
     WHERE ORDEM_CARREG EQ ORDEM_CARREGAMENTO
       AND VBELN EQ ORDEM_VENDA
       AND POSNR EQ ITEM
       AND STATUS NE ABAP_TRUE.

    CHECK SY-SUBRC IS INITIAL.
    RAISE EXCEPTION TYPE ZCX_PP_SERVICES
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_PP_SERVICES=>OC_ALREARY_REGISTRED-MSGID
                          MSGNO = ZCX_PP_SERVICES=>OC_ALREARY_REGISTRED-MSGNO
                          ATTR1 = ME->SHIPMENT_TRANSIT-ORDEM_CARREG
                          ATTR2 = ME->SHIPMENT_TRANSIT-VBELN
                          ATTR3 = ME->SHIPMENT_TRANSIT-POSNR ).
  ENDMETHOD.


  METHOD SELECT_SIMULADOR_VENDA_HEADER.
    DATA DOC_SIMULACAO TYPE CHAR10.

    SELECT SINGLE DOC_SIMULACAO
       FROM ZSDT0041
       INTO DOC_SIMULACAO
      WHERE VBELN = ORDEM_VENDA.

    IF NOT ( SY-SUBRC IS INITIAL ).
      SELECT SINGLE DOC_SIMULACAO
        FROM ZSDT0090
        INTO DOC_SIMULACAO
       WHERE VBELN = ORDEM_VENDA.
    ENDIF.

    SELECT SINGLE *
      FROM ZSDT0040
      INTO ME->HEADER_SIMULADOR_VENDA
     WHERE DOC_SIMULACAO = DOC_SIMULACAO.
  ENDMETHOD.


  METHOD SELECT_TEXT_STATUS.
    SELECT SINGLE *
      FROM TJ30T
      INTO ME->ORDER_STATUS
     WHERE STSMA = 'ZPI00001'
       AND TXT04 = STATUS.
  ENDMETHOD.


  METHOD SET_ADDITIONAL_DATA.
    ME->ADDITIONAL_DATA = VALUE #( MATRICULA   = MATRICULA
                                   ORDEMCARREG = ORDEM_CARREGAMENTO
                                   PLACA       = PLACA ).
  ENDMETHOD.


  METHOD SET_BATCH_PROCESS_ORDER.
    DATA MESSAGES TYPE TABLE OF BDCMSGCOLL.
    DATA(_CTUMODE) = 'N'.

    ME->SET_BDC_DYNPRO( PROGRAM = 'SAPLCOKO'           DYNPRO = '5110'           ).
    ME->SET_BDC_FIELD(  FNAM    = 'BDC_CURSOR'         FVAL   = 'CAUFVD-AUFNR'   ).
    ME->SET_BDC_FIELD(  FNAM    = 'BDC_OKCODE'         FVAL   = '=KOZE'          ).
    ME->SET_BDC_FIELD(  FNAM    = 'CAUFVD-AUFNR'       FVAL   =  CONV #( AUFNR ) ).
    ME->SET_BDC_FIELD(  FNAM    = 'R62CLORD-FLG_COMPL' FVAL   = 'X'              ).

    ME->SET_BDC_DYNPRO( PROGRAM = 'SAPLCOKO'           DYNPRO = '5115'           ).
    ME->SET_BDC_FIELD(  FNAM    = 'BDC_OKCODE'         FVAL   = '=KOWE'          ).
    ME->SET_BDC_FIELD(  FNAM    = 'BDC_CURSOR'         FVAL   = 'CAUFVD-GAMNG'   ).

    ME->SET_BDC_DYNPRO( PROGRAM = 'SAPLCOKO'           DYNPRO = '5115'           ).
    ME->SET_BDC_FIELD(  FNAM    = 'BDC_OKCODE'         FVAL   = '=CHHZ'          ).
    ME->SET_BDC_FIELD(  FNAM    = 'BDC_CURSOR'         FVAL   = 'AFPOD-CHARG'    ).
    ME->SET_BDC_FIELD(  FNAM    = 'AFPOD-CHARG'        FVAL   = CONV #( CHARG )  ).

    ME->SET_BDC_DYNPRO( PROGRAM = 'SAPLCOKO'           DYNPRO = '5115'           ).
    ME->SET_BDC_FIELD(  FNAM    = 'BDC_OKCODE'         FVAL   = '=BU'            ).
    ME->SET_BDC_FIELD(  FNAM    = 'BDC_CURSOR'         FVAL   = 'AFPOD-CHARG'    ).

    DATA(_BDC_DATA) = ME->GET_BDC_DATA( ).

    CALL TRANSACTION 'COR2' USING _BDC_DATA MODE _CTUMODE UPDATE 'S'
       MESSAGES INTO MESSAGES.

    REFRESH ME->BDCDATA.
    REFRESH MESSAGES.
  ENDMETHOD.


  METHOD SET_BDC_DYNPRO.
    APPEND VALUE BDCDATA(
      PROGRAM  = PROGRAM
      DYNPRO   = DYNPRO
      DYNBEGIN = 'X' ) TO ME->BDCDATA.
  ENDMETHOD.


  METHOD SET_BDC_FIELD.
    APPEND VALUE BDCDATA(
      FNAM = FNAM
      FVAL = FVAL ) TO ME->BDCDATA.
  ENDMETHOD.


  METHOD SET_MESSAGE.
    IF MSG_TABLE IS INITIAL.
      APPEND VALUE BAPIRET2( TYPE       = TYPE
                             NUMBER     = NUMBER
                             ID         = ID
                             MESSAGE_V1 = CONV #( V1 )
                             MESSAGE_V2 = CONV #( V2 )
                             MESSAGE_V3 = CONV #( V3 )
                             MESSAGE_V4 = CONV #( V4 )
                           ) TO ME->MESSAGES.
    ELSE.
      MOVE-CORRESPONDING MSG_TABLE TO ME->MESSAGE.
      APPEND ME->MESSAGE TO ME->MESSAGES.
    ENDIF.
  ENDMETHOD.


  METHOD SET_MESSAGES.
    LOOP AT MESSAGES ASSIGNING FIELD-SYMBOL(<FS_MESSAGE>).
      MOVE-CORRESPONDING <FS_MESSAGE> TO ME->MESSAGE.
      APPEND ME->MESSAGE TO ME->MESSAGES.
    ENDLOOP.
  ENDMETHOD.


  METHOD SET_PLANNING_LOG.
    DATA(_ITEM_PLANNING) = CORRESPONDING ZPPT0008( NEW_ITEM ).
    DATA(_FIELDNAMES)    = ZCL_UTIL=>GET_STRUCTURE_DESCRIPTION( 'ZPPT0008' ).

    SELECT
      MAX( CHANGENR )
      INTO @DATA(_SEQUENCE)
      FROM ZPPT0010
     WHERE VBELN = @_ITEM_PLANNING-VBELN
       AND POSNR = @_ITEM_PLANNING-POSNR.

    LOOP AT _FIELDNAMES INTO DATA(_FIELDNAME).

      ASSIGN COMPONENT:
      _FIELDNAME-FIELDNAME OF STRUCTURE OLD_ITEM TO FIELD-SYMBOL(<OLD_VALUE>),
      _FIELDNAME-FIELDNAME OF STRUCTURE NEW_ITEM TO FIELD-SYMBOL(<NEW_VALUE>).

      CHECK <OLD_VALUE> IS ASSIGNED AND <NEW_VALUE> IS ASSIGNED.

      IF ( <OLD_VALUE> NE <NEW_VALUE> ).
        _SEQUENCE = _SEQUENCE + 1.

        IF _FIELDNAME-FIELDNAME = 'IOPUS'.
          DATA(_OLD_STATUS) = SWITCH #( <OLD_VALUE> WHEN ICON_LIGHT_OUT    THEN ZCL_PP_SERVICES=>INTEGRACAO_OPUS_DESCRITIVO-NAO_PROCESSADO
                                                    WHEN ICON_YELLOW_LIGHT THEN ZCL_PP_SERVICES=>INTEGRACAO_OPUS_DESCRITIVO-PROCESSANDO
                                                    WHEN ICON_GREEN_LIGHT  THEN ZCL_PP_SERVICES=>INTEGRACAO_OPUS_DESCRITIVO-PROCESSADO
                                                    WHEN ICON_DELETE       THEN ZCL_PP_SERVICES=>INTEGRACAO_OPUS_DESCRITIVO-EXCLUIDO ).

          DATA(_NEW_STATUS) = SWITCH #( <NEW_VALUE> WHEN ICON_LIGHT_OUT    THEN ZCL_PP_SERVICES=>INTEGRACAO_OPUS_DESCRITIVO-NAO_PROCESSADO
                                                    WHEN ICON_YELLOW_LIGHT THEN ZCL_PP_SERVICES=>INTEGRACAO_OPUS_DESCRITIVO-PROCESSANDO
                                                    WHEN ICON_GREEN_LIGHT  THEN ZCL_PP_SERVICES=>INTEGRACAO_OPUS_DESCRITIVO-PROCESSADO
                                                    WHEN ICON_DELETE       THEN ZCL_PP_SERVICES=>INTEGRACAO_OPUS_DESCRITIVO-EXCLUIDO ).

          ASSIGN _OLD_STATUS TO <OLD_VALUE>.
          ASSIGN _NEW_STATUS TO <NEW_VALUE>.
        ENDIF.


        DATA(_LOG) =
            VALUE ZPPT0010( VBELN     = _ITEM_PLANNING-VBELN
                            POSNR     = _ITEM_PLANNING-POSNR
                            EDATU     = _ITEM_PLANNING-EDATU
                            COMPONENT = COMPONENT
                            CHANGENR  = _SEQUENCE
                            CHANGEID  = OPERATION
                            OLD_VALUE = <OLD_VALUE>
                            NEW_VALUE = <NEW_VALUE>
                            FIELDNAME = _FIELDNAME-FIELDNAME
                            FIELDTEXT = _FIELDNAME-SCRTEXT_L
                            USERNAME  = SY-UNAME
                            DATA      = SY-DATUM
                            HORA      = SY-UZEIT
                          ).

        INSERT ZPPT0010 FROM _LOG.
      ENDIF.

      UNASSIGN: <OLD_VALUE>, <NEW_VALUE>.
    ENDLOOP.

    COMMIT WORK.
  ENDMETHOD.


  METHOD STORNO_PROCESS_ORDER.

    DATA PP_ORDER_TYPE      TYPE AFRU-ORIND VALUE '6'.
    DATA LOCKED             TYPE BAPI_CORU_PARAM-LOCKED.
    DATA LOTE               TYPE MCHB-CHARG.
    DATA RETURN             TYPE BAPIRET1.

    DATA: SCOPE_COMPL_TECH   TYPE CO_TECO_EXPLODE_NET  VALUE '1',
          WORK_PROCESS_GROUP TYPE RZLLI_APCL VALUE  'COWORK_BAPI',
          WORK_PROCESS_MAX   TYPE RZLLI_WPQT VALUE 99,
          I_ORDER_TYPE       TYPE AUFTYP VALUE 40,
          STATUS             TYPE P_TXT04,
          STATUS_PROFILE     TYPE CO_STSMA_HDR VALUE 'ZPI00001',
          RETURN2            TYPE BAPIRET2,
          RETURN_DETAILS     TYPE TABLE OF BAPI_ORDER_RETURN.

    DATA(_SHIPMENT) = ME->GET_SHIPMENT( ).

    IF _SHIPMENT-TXT04 = ZCL_PP_SERVICES=>PO_STATUS-ESTORNADO OR
       _SHIPMENT-TXT04 = ZCL_PP_SERVICES=>PO_STATUS-PESAGEM_VAZIO.
      RAISE EXCEPTION TYPE ZCX_PP_SERVICES
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_PP_SERVICES=>ORDER_ALREADY_STORNED-MSGID
                            MSGNO = ZCX_PP_SERVICES=>ORDER_ALREADY_STORNED-MSGNO
                            ATTR1 = _SHIPMENT-AUFNR ).
    ENDIF.

    STATUS  = COND #( WHEN PSVA IS NOT INITIAL
                      AND _SHIPMENT-TXT04 EQ ME->PO_STATUS-EM_PRODUCAO
                          THEN ME->PO_STATUS-PESAGEM_VAZIO
                          ELSE ME->PO_STATUS-ESTORNADO ).

    CALL METHOD ME->SELECT_MSKA
      EXPORTING
        CHARG = CONV #( |{ _SHIPMENT-AUFNR ALPHA = OUT }| )
        VBELN = _SHIPMENT-KDAUF
        POSNR = _SHIPMENT-KDPOS.

    IF ( _SHIPMENT IS NOT INITIAL ).
      TRY.
          CALL METHOD ME->SELECT_PLANNING
            EXPORTING
              VBELN = _SHIPMENT-KDAUF
              POSNR = _SHIPMENT-KDPOS
              EDATU = _SHIPMENT-ERDAT.

          DATA(_PLANNING) = ME->GET_PLANNING( ).

          "Comentando o codigo abaixo dando DUMP em QAS - Deve ser Verificado no dev
*          CALL METHOD ME->SELECT_DIVISION_PLANNING
*            EXPORTING
*              VBELN = _PLANNING-VBELN
*              POSNR = _PLANNING-POSNR.

          CALL METHOD ME->SELECT_DIVISION_SHIPMENT
            EXPORTING
              VBELN = _PLANNING-VBELN
              POSNR = _PLANNING-POSNR
              EDATU = _PLANNING-EDATU.
        CATCH ZCX_PP_SERVICES.
      ENDTRY.

      DATA: VL_WMENG TYPE WMENG.

      VL_WMENG =
      COND #(
      LET X = _SHIPMENT-WEMNG IN
          WHEN X IS NOT INITIAL
          THEN COND #( WHEN X < _SHIPMENT-PSMNG
                              THEN X
                              ELSE _SHIPMENT-PSMNG )
          ELSE _SHIPMENT-PSMNG ).

      TRY.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              INPUT  = _SHIPMENT-AUFNR
            IMPORTING
              OUTPUT = LOTE.

          CALL FUNCTION 'BAPI_PROCORDCONF_CANCEL'
            EXPORTING
              CONFIRMATION        = _SHIPMENT-RUECK
              CONFIRMATIONCOUNTER = _SHIPMENT-RMZHL
              POSTG_DATE          = SY-DATUM
              CONF_TEXT           = 'Serviço OPUS - Estorno'
            IMPORTING
              RETURN              = RETURN.

          IF ( RETURN-TYPE = 'E' ).
            ME->SET_MESSAGE( MSG_TABLE = RETURN ).
          ELSE.

            ME->CHANGE_SALES_ORDER(
              EXPORTING
                ORDER     = _PLANNING-VBELN
                ITEM      = _PLANNING-POSNR
*            QUANTITY  = COND #( LET X = _SHIPMENT-WEMNG IN WHEN X IS NOT INITIAL THEN X ELSE _SHIPMENT-PSMNG )
                QUANTITY  = VL_WMENG
                OPERATION = 'STORNO_PO'
              CHANGING
                _PLANNING = _PLANNING
              EXCEPTIONS
                ORDER_NOT_CHANGED = 4
            ).

            IF ( SY-SUBRC IS INITIAL ).

              "//Set deletion flag for Lote
              CALL METHOD ME->DELETE_BATCH
                EXPORTING
                  CENTRO   = _SHIPMENT-WERKS
                  MATERIAL = _SHIPMENT-MATNR
                  LOTE     = LOTE.

              "//Commit all
              _PLANNING-IOPUS = ZCL_PP_SERVICES=>INTEGRACAO_OPUS-PROCESSANDO.
              ME->MODIFY_PLANNING( _PLANNING ).

              "//Time to the cancel process be done.
              IF ( _SHIPMENT-WEMNG IS NOT INITIAL ).
                PROCESS_ORDER_IS_LOCKED( ORDER = _SHIPMENT-AUFNR WAIT = ABAP_TRUE ).
              ENDIF.

*             "//Set deletion flag PO.
              DATA(_ORDERS) = VALUE TB_BAPI_ORDER_KEY( ( ORDER_NUMBER = _SHIPMENT-AUFNR ) ).
              WAIT UP TO 1 SECONDS.

*             "//Change the user status.
              CALL FUNCTION 'BAPI_PROCORD_SETUSERSTATUS'
                EXPORTING
                  STATUS_PROFILE     = STATUS_PROFILE
                  STATUS             = STATUS
                  WORK_PROCESS_GROUP = WORK_PROCESS_GROUP
                  WORK_PROCESS_MAX   = WORK_PROCESS_MAX
                IMPORTING
                  RETURN             = RETURN2
                TABLES
                  ORDERS             = _ORDERS
                  DETAIL_RETURN      = RETURN_DETAILS.

              IF NOT LINE_EXISTS( RETURN_DETAILS[ TYPE = 'E' ] ).
                WAIT UP TO 1 SECONDS.

*             "//Complete orders technically.
                CALL FUNCTION 'COXT_BAPI_COMPLETE_TECH'
                  EXPORTING
                    I_SCOPE_COMPL_TECH   = SCOPE_COMPL_TECH
                    I_WORK_PROCESS_GROUP = WORK_PROCESS_GROUP
                    I_WORK_PROCESS_MAX   = WORK_PROCESS_MAX
                    I_ORDER_TYPE         = I_ORDER_TYPE
                  IMPORTING
                    ES_RETURN            = RETURN2
                  TABLES
                    IT_ORDERS            = _ORDERS
                    ET_DETAIL_RETURN     = RETURN_DETAILS.

                IF NOT LINE_EXISTS( RETURN_DETAILS[ TYPE = 'E' ] ).
                  WAIT UP TO 1 SECONDS.

*             "//Set deletion flag for orders
                  CALL FUNCTION 'COXT_BAPI_SET_DELETION_FLAG'
                    EXPORTING
                      I_WORK_PROCESS_GROUP = WORK_PROCESS_GROUP
                      I_WORK_PROCESS_MAX   = WORK_PROCESS_MAX
                      I_ORDER_TYPE         = I_ORDER_TYPE
                    IMPORTING
                      ES_RETURN            = RETURN2
                    TABLES
                      IT_ORDERS            = _ORDERS
                      ET_DETAIL_RETURN     = RETURN_DETAILS.

                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        CATCH ZCX_PP_SERVICES.
      ENDTRY.

    ELSE.
      ME->DELETE_SHIPMENT_TRANSIT( ORDEM_CARREGAMENTO ).
    ENDIF.
  ENDMETHOD.


  METHOD UPDATE_ORDER_DIVISION.
    DATA(NEW_SCHEDULE)  =
     VALUE BAPISCHDL(
             ITM_NUMBER = POSNR
             SCHED_LINE = ETENR
             REQ_QTY    = WMENG
             REQ_DATE   = EDATU
             REQ_DLV_BL = SWITCH #( UNLOCK WHEN ABAP_FALSE THEN 10 ELSE SPACE )
             REFOBJKEY  = OBJKEY
                     ).

    DATA(NEW_SCHEDULE_X) =
     VALUE BAPISCHDLX(
            ITM_NUMBER = POSNR
            SCHED_LINE = ETENR
            UPDATEFLAG = ZUTIL_CRUD-UPDATE
            REQ_DATE   = COND #( WHEN EDATU IS INITIAL THEN ABAP_FALSE ELSE ABAP_TRUE )
            REQ_QTY    = COND #( WHEN WMENG IS INITIAL THEN ABAP_FALSE ELSE ABAP_TRUE )
            REQ_DLV_BL = ABAP_TRUE
            REFOBJKEY  = SWITCH #( OBJKEY WHEN SPACE THEN ABAP_FALSE ELSE ABAP_TRUE )
                     ).

    APPEND NEW_SCHEDULE   TO ME->SCHED_LINES.
    APPEND NEW_SCHEDULE_X TO ME->SCHED_LINESX.
  ENDMETHOD.


  METHOD UPDATE_PROCESS_ORDER.
    DATA RETURN       TYPE BAPIRET2.
    DATA ORDER_DATA   TYPE BAPI_PI_ORDER_CHANGE.
    DATA ORDER_DATA_X TYPE BAPI_PI_ORDER_CHANGEX.

    ME->SET_ADDITIONAL_DATA(
      MATRICULA          = MATRICULA
      ORDEM_CARREGAMENTO = ORDEM_CARREGAMENTO
      PLACA              = PLACA
    ).

    CALL FUNCTION 'BAPI_PROCORD_CHANGE'
      EXPORTING
        NUMBER     = ORDEM_PRODUCAO
        ORDERDATA  = ORDER_DATA
        ORDERDATAX = ORDER_DATA_X
      IMPORTING
        RETURN     = RETURN
*       ORDER_TYPE =
*       ORDER_STATUS           =
*       MASTER_DATA_READ       =
      .

    IF RETURN-TYPE = 'E'.
      RAISE PROCESS_ORDER_NOT_CHANGED.
    ENDIF.
  ENDMETHOD.


  METHOD UPDATE_SHIPMENT.

    SELECT SINGLE *
      FROM ZPPT0009
      INTO @DATA(W_0009)
      WHERE ORDEM_CARREG EQ @ORDEM_CARREGAMENTO
        AND VBELN EQ @ORDEM_VENDA
        AND POSNR EQ @ITEM.

    CHECK SY-SUBRC IS INITIAL.

    W_0009-DT_MODIFICACAO = SY-DATUM.
    W_0009-HR_MODIFICACAO = SY-UZEIT.

    MODIFY ZPPT0009 FROM W_0009.
    COMMIT WORK.

  ENDMETHOD.


  METHOD UPDATE_SOLICITATION_BOARDING.
    MODIFY ZSDT0082 FROM ITEM.
    COMMIT WORK.
  ENDMETHOD.


  METHOD select_ordem_carregamento.

    SELECT COUNT(*)
      FROM zppt0009
     WHERE ordem_carreg = ordem_carregamento.

    CHECK sy-subrc IS NOT INITIAL.

    RAISE EXCEPTION TYPE zcx_pp_services
      EXPORTING
        textid = VALUE #( msgid = zcx_pp_services=>oc_invalid-msgid
                          msgno = zcx_pp_services=>oc_invalid-msgno
                          attr1 = ordem_carregamento
                         ).

  ENDMETHOD.
ENDCLASS.
