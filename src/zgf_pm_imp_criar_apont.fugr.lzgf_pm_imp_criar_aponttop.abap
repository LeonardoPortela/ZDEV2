FUNCTION-POOL ZGF_PM_IMP_CRIAR_APONT.       "MESSAGE-ID ..

* INCLUDE LZGF_PM_IMP_CRIAR_APONTD...        " Local class definition

DATA:  WA_INPUT         TYPE ZPMT0015.

DATA: IT_BDCDATA TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB  TYPE TABLE OF          BDCMSGCOLL,
      WA_BDCDATA LIKE LINE OF           IT_BDCDATA,
      WL_RETURN  TYPE BAPIRET2,
      ET_RETURN  TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
      LS_RETURN  TYPE BAPIRET2.

DATA:
  IT_OPERAT TYPE TABLE OF BAPI_ALM_ORDER_OPERATION_E,
  IT_OLIST  TYPE TABLE OF BAPI_ALM_ORDER_OBJECTLIST,
  IT_APONT  TYPE TABLE OF BAPI_ALM_TIMECONFIRMATION,
  IT_OPERA  TYPE TABLE OF ZPMT0015,
  GT_OPERA  TYPE TABLE OF ZPMT0015,
  CODES1    TYPE TABLE OF BAPI10011T,
  IT_RETURN TYPE  BAPIRET2,
  TG_RETURN TYPE TABLE OF BAPI_ALM_RETURN,
  T_RETURN  TYPE TABLE OF BAPIRET2,
  WA_RETURN TYPE BAPIRET2.

CLASS Z_CREATE_DADOS DEFINITION.

  PUBLIC SECTION.

    METHODS GET_BLOCO
      RETURNING VALUE(NR_BLOCO) TYPE NUMC15.

    METHODS CHECK_QTD_ITEM_BLOCO
      IMPORTING NR_BLOCO        TYPE NUMC15
      RETURNING VALUE(QTD_ITEM) TYPE INT4.

    METHODS CONVERT_DATE
      IMPORTING I_DATA        TYPE CHAR10
      RETURNING VALUE(E_DATA) TYPE SY-DATUM.

*   "//Criar ordem de manutenção.
    METHODS: Z_CRIAR_ORDENS
      IMPORTING W_ORDEM TYPE ZPMT0013
      EXPORTING ORDEM   TYPE AUFNR
                MSG     TYPE BAPI_MSG.

  PRIVATE SECTION.

    DATA T_RETURN TYPE BAPIRET2_T.
    DATA AT_ORDERID TYPE AUFNR VALUE 1.
    DATA AT_REFNUM  TYPE IFREFNUM VALUE 1.
    DATA AT_OPER_NO TYPE OBJIDEXT.

ENDCLASS.

CLASS Z_CREATE_DADOS IMPLEMENTATION.
* "// Pega o ultimo Bloco de envio
  METHOD GET_BLOCO.
    SELECT MAX( BLOCO ) FROM ZPMT0015 INTO NR_BLOCO.
    ADD 1 TO NR_BLOCO.
  ENDMETHOD.

* "// verifica se a quantidade de Bloco gravada corresponde ao enviado
  METHOD CHECK_QTD_ITEM_BLOCO.
    SELECT COUNT(*) FROM ZPMT0015 INTO QTD_ITEM WHERE BLOCO EQ NR_BLOCO.
  ENDMETHOD.
  METHOD CONVERT_DATE.

    DATA(X_DATA) = I_DATA.

    REPLACE ALL OCCURRENCES OF '/' IN X_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN X_DATA WITH ''.
*    REPLACE ALL OCCURRENCES OF ' ' IN X_DATA WITH ''.

    TRY.
        CL_ABAP_DATFM=>CONV_DATE_EXT_TO_INT( EXPORTING IM_DATEXT = X_DATA IMPORTING EX_DATINT = E_DATA ).
      CATCH CX_ABAP_DATFM_NO_DATE.
      CATCH CX_ABAP_DATFM_INVALID_DATE.
      CATCH CX_ABAP_DATFM_FORMAT_UNKNOWN.
      CATCH CX_ABAP_DATFM_AMBIGUOUS.
    ENDTRY.

  ENDMETHOD.

* "//Implementação criar ordem de manutenção.
  METHOD Z_CRIAR_ORDENS.

    AT_ORDERID = |{ AT_ORDERID ALPHA = IN }|.
    AT_ORDERID+0(1) = '%'.

    AT_REFNUM  = |{ AT_REFNUM  ALPHA = IN }|.

    AT_OPER_NO = AT_ORDERID.
    AT_OPER_NO+12(4) = '0010'.

    DATA(T_METHODS) =
    VALUE BAPI_ALM_ORDER_METHOD_T(
      ( REFNUMBER = AT_REFNUM OBJECTTYPE = 'HEADER'    METHOD = 'CREATE'  OBJECTKEY = AT_ORDERID )
      ( REFNUMBER = AT_REFNUM OBJECTTYPE = 'OPERATION' METHOD = 'CREATE'  OBJECTKEY = AT_OPER_NO )
      ( REFNUMBER = AT_REFNUM OBJECTTYPE = 'HEADER'    METHOD = 'RELEASE' OBJECTKEY = AT_ORDERID )
      ( REFNUMBER = ''        OBJECTTYPE = ''          METHOD = 'SAVE'    OBJECTKEY = AT_ORDERID )
                                 ).

    DATA(T_HEADER) =
    VALUE BAPI_ALM_ORDER_HEADER_T( (
        ORDERID      = AT_ORDERID
        ORDER_TYPE   = W_ORDEM-AUART
        FUNCT_LOC    = W_ORDEM-TPLNR
        SHORT_TEXT   = W_ORDEM-KTEXT
        PLANPLANT    = W_ORDEM-IWERK
        BUS_AREA     = W_ORDEM-IWERK
        MN_WK_CTR    = W_ORDEM-ARBPL
        PLANT        = W_ORDEM-IWERK
        MAINTPLANT   = W_ORDEM-IWERK
        LOC_BUS_AREA = W_ORDEM-IWERK
        PLANGROUP    = W_ORDEM-INGPR
        EQUIPMENT    = |{ W_ORDEM-EQUNR ALPHA = IN }|
        START_DATE   = SY-DATUM
        PRIORITY     = W_ORDEM-PRIOK
    ) ).

    DATA(T_OPERATION) =
    VALUE BAPI_ALM_ORDER_OPERATION_T( (
        ACTIVITY    = AT_OPER_NO+12(4)
        CONTROL_KEY = 'PM01'
        DESCRIPTION = W_ORDEM-KTEXT
    ) ).
"*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'"#EC CI_USAGE_OK[2438131]
      TABLES                               "#EC CI_USAGE_OK[2669857]
        IT_METHODS   = T_METHODS
        IT_HEADER    = T_HEADER
        IT_OPERATION = T_OPERATION
        RETURN       = T_RETURN.

    IF NOT LINE_EXISTS( T_RETURN[ TYPE = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = ABAP_TRUE.

      TRY .
          ORDEM = |{ T_RETURN[ NUMBER = '112' ]-MESSAGE_V2 ALPHA = OUT }|.
        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
      ENDTRY.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CLEAR MSG.
      LOOP AT T_RETURN INTO DATA(WA_RET).
        MSG = COND #( WHEN MSG IS INITIAL THEN WA_RET-MESSAGE ELSE |{ MSG }, { WA_RET-MESSAGE }| ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "Z_ORDER_MAINTAIN

ENDCLASS.


CLASS ZLC_APONTAMENTO DEFINITION.
  PUBLIC SECTION.

*Method processo de processamento dos dados de apontamento.
    METHODS: SET_APONTAR
      IMPORTING
        W_APONT TYPE ZPMT0015
      EXPORTING
        CONF    TYPE CO_RUECK
        MSG     TYPE BAPI_MSG.



ENDCLASS.


CLASS ZLC_APONTAMENTO IMPLEMENTATION.

  METHOD SET_APONTAR.

    IT_APONT = VALUE #( (


                                    CONF_NO         = W_APONT-AUERU
*                                    ORDERID         = W_APONT-AUFNR
                                    POSTG_DATE      = W_APONT-BUDAT
*                                    OPERATION       = W_APONT-VORNR
*                                    SUB_OPER        = LS-SUB_ACTIVITY
                                    WORK_CNTR       = W_APONT-ARBPL
                                    PERS_NO         = W_APONT-PERNR
                                    EXEC_START_DATE = W_APONT-ISDD
                                    EXEC_START_TIME = W_APONT-ISDZ
                                    EXEC_FIN_DATE   = W_APONT-IEDD
                                    EXEC_FIN_TIME   = W_APONT-IEDZ
                                    FIN_CONF        = W_APONT-AUERU
                                    ACT_WORK        = W_APONT-ISMNW
                                    UN_WORK         = W_APONT-ISMNE
                                    DEV_REASON      = W_APONT-GRUND
*                                  CLEAR_RES       = LS-CLEAR_RES
*                                    ACT_TYPE        = LS-ACTTYPE
                                  ) ).

    CALL FUNCTION 'BAPI_ALM_CONF_CREATE'
      IMPORTING
        RETURN        = WA_RETURN
      TABLES
        TIMETICKETS   = IT_APONT
        DETAIL_RETURN = TG_RETURN.


*    IT_RETURN = VALUE #( FOR LR IN TG_RETURN
*                                            (
*                                              TYPE        = LR-TYPE
*                                              ID          = LR-MESSAGE_ID
*                                              NUMBER      = LR-MESSAGE_NUMBER
*                                              MESSAGE     = LR-MESSAGE
*                                              LOG_NO      = LR-LOG_NUMBER
*                                              LOG_MSG_NO  = LR-LOG_MSG_NO
*                                              MESSAGE_V1  = LR-MESSAGE_V1
*                                              MESSAGE_V2  = LR-MESSAGE_V2
*                                              MESSAGE_V3  = LR-MESSAGE_V3
*                                              MESSAGE_V4  = LR-MESSAGE_V4
*                                              PARAMETER   = LR-PARAMETER
*                                              ROW         = LR-ROW
*                                              FIELD       = LR-FIELD
*                                              SYSTEM      = LR-SYSTEM
*                                            )
*                       ).
    READ TABLE TG_RETURN INTO DATA(WA_RET) INDEX 1.
    IF WA_RET-CONF_NO IS NOT INITIAL.
      IF NOT LINE_EXISTS( TG_RETURN[ TYPE = 'E' ] ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = ABAP_TRUE.

        CONF = WA_RET-CONF_NO.
      ENDIF.
    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CLEAR MSG.
      MSG = COND #( WHEN MSG IS INITIAL THEN WA_RET-MESSAGE ELSE |{ MSG }, { WA_RET-MESSAGE }| ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
