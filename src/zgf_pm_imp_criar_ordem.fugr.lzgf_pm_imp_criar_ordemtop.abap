FUNCTION-POOL ZGF_PM_IMP_CRIAR_ORDEM.       "MESSAGE-ID ..

*INCLUDE LZGF_PM_IMP_CRIAR_ORDEMD.        " Local class definition


*************************************************************************
* O trecho de Codigo Abaixo foi Enviado para uma Classe Global
************************************************************************
*DATA:  WA_INPUT         TYPE ZPMT0013.
*
*DATA: IT_BDCDATA TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
*      T_MESSTAB  TYPE TABLE OF          BDCMSGCOLL,
*      WA_BDCDATA LIKE LINE OF           IT_BDCDATA,
*      WL_RETURN  TYPE BAPIRET2,
*      ET_RETURN  TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
*      LS_RETURN  TYPE BAPIRET2.
*
*CLASS Z_CREATE_DADOS DEFINITION.
*
*  PUBLIC SECTION.
*
*    METHODS GET_BLOCO
*      RETURNING VALUE(NR_BLOCO) TYPE NUMC15.
*
*    METHODS SET_IDORD.
*    METHODS GET_IDORD RETURNING VALUE(E_IDOPR) TYPE ZPMED002.
*
*    METHODS SET_IDOPR.
*    METHODS GET_IDOPR RETURNING VALUE(E_IDOPR) TYPE ZPMED003.
*
*    METHODS SET_IDAPN.
*    METHODS GET_IDAPN RETURNING VALUE(E_IDAPN) TYPE ZPMED004.
*
*    METHODS SET_VORNR.
*    METHODS GET_VORNR RETURNING VALUE(E_VORNR) TYPE ARBPL.
*
*    METHODS CHECK_QTD_ITEM_BLOCO
*      IMPORTING NR_BLOCO        TYPE NUMC15
*      RETURNING VALUE(QTD_ITEM) TYPE INT4.
*
*    METHODS CONVERT_DATE
*      IMPORTING I_DATA        TYPE CHAR10
*      RETURNING VALUE(E_DATA) TYPE SY-DATUM.
*
*    METHODS CONVERT_TIME
*      IMPORTING I_TIME        TYPE CHAR10
*      RETURNING VALUE(E_TIME) TYPE FSEDZ.
*
*    METHODS CHECK_VORNR
*      IMPORTING I_ORDEM        TYPE AUFNR
*                I_VORNR        TYPE VORNR
*      RETURNING VALUE(E_SUBRC) TYPE SY-SUBRC.
*
*    METHODS CALL_REPORT.
*
**   "//Criar ordem de manutenção.
*    METHODS: Z_CRIAR_ORDENS
*      IMPORTING I_ORDEM       TYPE ZPMT0016
*                I_OPERACAO    TYPE ZPMT0017_T OPTIONAL
*                I_APONTAMENTO TYPE ZPMT0015_T OPTIONAL
*      EXPORTING E_ORDEM       TYPE AUFNR
*                E_MSG         TYPE BAPI_MSG.
*
*  PRIVATE SECTION.
*
*    DATA T_RETURN    TYPE BAPIRET2_T.
*    DATA AT_ORDERID  TYPE AUFNR VALUE 1.
*    DATA AT_REFNUM   TYPE IFREFNUM VALUE 1.
*    DATA AT_SEQOPR   TYPE IFREFNUM.
*    DATA AT_ACTIVITY TYPE VORNR.
*    DATA AT_OPER_NO  TYPE OBJIDEXT.
*    DATA AT_IDORD    TYPE ZPMED002.
*    DATA AT_IDOPR    TYPE ZPMED003.
*    DATA AT_IDAPN    TYPE ZPMED005.
*    DATA AT_VORNR    TYPE VORNR.
*
*
*ENDCLASS.
*
*
*CLASS Z_CREATE_DADOS IMPLEMENTATION.
** "// Pega o ultimo Bloco de envio
*  METHOD GET_BLOCO.
*    SELECT MAX( BLOCO ) FROM ZPMT0016 INTO NR_BLOCO.
*    ADD 1 TO NR_BLOCO.
*  ENDMETHOD.
*
*  METHOD SET_IDORD.
*    ADD 1 TO AT_IDORD.
*  ENDMETHOD.
*
*  METHOD GET_IDORD.
*    E_IDOPR = AT_IDORD.
*  ENDMETHOD.
*
*  METHOD SET_IDOPR.
*    ADD 1 TO AT_IDOPR.
*  ENDMETHOD.
*
*  METHOD GET_IDOPR.
*    E_IDOPR = AT_IDOPR.
*  ENDMETHOD.
*
*  METHOD SET_IDAPN.
*    ADD 1 TO AT_IDAPN.
*  ENDMETHOD.
*
*  METHOD GET_IDAPN.
*    E_IDAPN = AT_IDAPN.
*  ENDMETHOD.
*
*  METHOD SET_VORNR.
*    ADD 10 TO AT_VORNR.
*  ENDMETHOD.
*
*  METHOD GET_VORNR.
*    E_VORNR = AT_VORNR.
*  ENDMETHOD.
*
** "// verifica se a quantidade de Bloco gravada corresponde ao enviado
*  METHOD CHECK_QTD_ITEM_BLOCO.
*    SELECT COUNT(*) FROM ZPMT0016 INTO QTD_ITEM WHERE BLOCO EQ NR_BLOCO.
*  ENDMETHOD.
*  METHOD CONVERT_DATE.
*
*    DATA(X_DATA) = I_DATA.
*
*    REPLACE ALL OCCURRENCES OF '/' IN X_DATA WITH ''.
*    REPLACE ALL OCCURRENCES OF '.' IN X_DATA WITH ''.
*
*    TRY.
*        CL_ABAP_DATFM=>CONV_DATE_EXT_TO_INT( EXPORTING IM_DATEXT = X_DATA IMPORTING EX_DATINT = E_DATA ).
*      CATCH CX_ABAP_DATFM_NO_DATE.
*      CATCH CX_ABAP_DATFM_INVALID_DATE.
*      CATCH CX_ABAP_DATFM_FORMAT_UNKNOWN.
*      CATCH CX_ABAP_DATFM_AMBIGUOUS.
*    ENDTRY.
*
*  ENDMETHOD.
*
*  METHOD CONVERT_TIME.
*
*    DATA(X_TIME) = I_TIME.
*
*    REPLACE ALL OCCURRENCES OF ':' IN X_TIME WITH ''.
*    REPLACE ALL OCCURRENCES OF '.' IN X_TIME WITH ''.
*
*    E_TIME = X_TIME.
*
*  ENDMETHOD.
*
*  METHOD CHECK_VORNR.
*    SELECT COUNT(*)
*      FROM AFKO AS A
*      INNER JOIN AFVC AS B ON A~AUFPL EQ B~AUFPL
*      WHERE A~AUFNR EQ I_ORDEM
*        AND B~VORNR EQ I_VORNR.
*    E_SUBRC = SY-SUBRC.
*  ENDMETHOD.
*
*  METHOD CALL_REPORT.
*
*    DATA: NUMBER           TYPE TBTCJOB-JOBCOUNT,
*          NAME             TYPE TBTCJOB-JOBNAME VALUE 'JOB_ZPMR0045',
*          PRINT_PARAMETERS TYPE PRI_PARAMS.
*
*    CALL FUNCTION 'JOB_OPEN'
*      EXPORTING
*        JOBNAME          = NAME
*      IMPORTING
*        JOBCOUNT         = NUMBER
*      EXCEPTIONS
*        CANT_CREATE_JOB  = 1
*        INVALID_JOB_DATA = 2
*        JOBNAME_MISSING  = 3
*        OTHERS           = 4.
*
*    IF SY-SUBRC IS INITIAL.
*      SUBMIT ZPMR0045 TO SAP-SPOOL
*                       SPOOL PARAMETERS PRINT_PARAMETERS
*                       WITHOUT SPOOL DYNPRO
*                       VIA JOB NAME NUMBER NUMBER
*                       AND RETURN.
*      IF SY-SUBRC IS INITIAL.
*        CALL FUNCTION 'JOB_CLOSE'
*          EXPORTING
*            JOBCOUNT             = NUMBER
*            JOBNAME              = NAME
*            STRTIMMED            = ABAP_TRUE
*          EXCEPTIONS
*            CANT_START_IMMEDIATE = 1
*            INVALID_STARTDATE    = 2
*            JOBNAME_MISSING      = 3
*            JOB_CLOSE_FAILED     = 4
*            JOB_NOSTEPS          = 5
*            JOB_NOTEX            = 6
*            LOCK_FAILED          = 7
*            OTHERS               = 8.
*      ENDIF.
*    ENDIF.
*
*  ENDMETHOD.
*
** "//Implementação criar ordem de manutenção.
*  METHOD Z_CRIAR_ORDENS.
*
**  "//                                                                                      //"
**  "// Processamento da BAPI de Ordem                                                       //"
**  "// se o campo I_ORDEM-AUFNR estiver preenchido Vamos executar uma Atualização na Ordem  //"
**  "// se o campo I_ORDEM-AUFNR estiver vazio vamos executar uma Criação na Ordem           //"
**  "// as Operações Podem ser Criada com Varias VORNR ou Itens como preferir                //"
**  "// os Apontamentos so poderão ser executado quando a Ordem estiver Liberada             //"
**  "//                                                                                      //"
*
*    DATA T_OPERATION TYPE BAPI_ALM_ORDER_OPERATION_T.
*    DATA T_APONTAMENTO TYPE BAPI_ALM_TIMECONFIRMATION_T.
*    DATA T_ALM_RETURN TYPE TABLE OF BAPI_ALM_RETURN.
*    DATA _RETURN TYPE BAPIRET2.
*    DATA _METHOD TYPE SWO_METHOD.
*
*    AT_ORDERID = |{ AT_ORDERID ALPHA = IN }|.
*
*    IF I_ORDEM-AUFNR IS NOT INITIAL.
*      AT_ORDERID = I_ORDEM-AUFNR.
*      _METHOD = 'CHANGE'.
*    ELSE.
*      AT_ORDERID+0(1) = '%'.
*      _METHOD = 'CREATE'.
*    ENDIF.
*
*    AT_REFNUM  = |{ AT_REFNUM  ALPHA = IN }|.
*
*    AT_OPER_NO = AT_ORDERID.
*
*    DATA(T_METHODS) =
*    VALUE BAPI_ALM_ORDER_METHOD_T(
*                                    ( REFNUMBER = AT_REFNUM OBJECTTYPE = 'HEADER'    METHOD = _METHOD  OBJECTKEY = AT_ORDERID )
*                                 ).
*
*    DATA(T_HEADER) =
*    VALUE BAPI_ALM_ORDER_HEADER_T( (
*        ORDERID      = AT_ORDERID
*        ORDER_TYPE   = I_ORDEM-AUART
*        FUNCT_LOC    = I_ORDEM-TPLNR
*        SHORT_TEXT   = I_ORDEM-KTEXT
*        PLANPLANT    = I_ORDEM-IWERK
*        BUS_AREA     = I_ORDEM-IWERK
*        MN_WK_CTR    = I_ORDEM-ARBPL
*        PLANT        = I_ORDEM-IWERK
*        MAINTPLANT   = I_ORDEM-IWERK
*        LOC_BUS_AREA = I_ORDEM-IWERK
*        PLANGROUP    = I_ORDEM-INGPR
*        START_DATE   = I_ORDEM-DTINI
*        FINISH_DATE  = I_ORDEM-DTFIM
*        BASICSTART   = I_ORDEM-HRINI
*        BASIC_FIN    = I_ORDEM-HRFIM
*        PMACTTYPE    = I_ORDEM-ILART
*        EQUIPMENT    = |{ I_ORDEM-EQUNR ALPHA = IN }|
*        PRIORITY     = I_ORDEM-PRIOK
*    ) ).
*
*    LOOP AT I_OPERACAO INTO DATA(W_OPERACAO).
*
*      DATA(_METHOD_AUX) = _METHOD.
*
*      ADD 1 TO AT_SEQOPR.
*      AT_OPER_NO+12(4) = W_OPERACAO-VORNR.
*
*      IF AT_ORDERID IS NOT INITIAL.
*        IF ME->CHECK_VORNR( I_ORDEM = AT_ORDERID I_VORNR = W_OPERACAO-VORNR ) IS NOT INITIAL.
*          _METHOD_AUX = 'CREATE'.
*        ENDIF.
*      ENDIF.
*
*      APPEND VALUE #( REFNUMBER = AT_SEQOPR OBJECTTYPE = 'OPERATION' METHOD = _METHOD_AUX  OBJECTKEY = AT_OPER_NO ) TO T_METHODS.
*
*      APPEND VALUE #(
*          ACTIVITY    = W_OPERACAO-VORNR
*          WORK_CNTR   = W_OPERACAO-ARBPL
*          CONTROL_KEY = W_OPERACAO-STEUS
*          PLANT       = I_ORDEM-IWERK
*          DESCRIPTION = W_OPERACAO-DESCRIPTION
*      ) TO T_OPERATION.
*
*    ENDLOOP.
*
*    IF I_ORDEM-STATO EQ '1'.
*      APPEND VALUE #( REFNUMBER = AT_REFNUM OBJECTTYPE = 'HEADER'    METHOD = 'RELEASE' OBJECTKEY = AT_ORDERID ) TO T_METHODS.
*    ENDIF.
*
*    APPEND VALUE #( REFNUMBER = ''        OBJECTTYPE = ''          METHOD = 'SAVE'    OBJECTKEY = AT_ORDERID ) TO T_METHODS.
*
*    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
*      TABLES
*        IT_METHODS   = T_METHODS
*        IT_HEADER    = T_HEADER
*        IT_OPERATION = T_OPERATION
*        RETURN       = T_RETURN.
*
*    IF NOT LINE_EXISTS( T_RETURN[ TYPE = 'E' ] ).
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          WAIT = ABAP_TRUE.
*
*      TRY .
*          IF I_ORDEM-AUFNR IS INITIAL.
*            E_ORDEM = |{ T_RETURN[ NUMBER = '112' ]-MESSAGE_V2 ALPHA = OUT }|.
*          ELSE.
*            E_ORDEM = |{ T_RETURN[ NUMBER = '080' ]-MESSAGE_V1 ALPHA = OUT }|.
*          ENDIF.
*        CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*      ENDTRY.
*
*      IF I_ORDEM-STATO EQ '1'.
*        T_APONTAMENTO =
*        VALUE #(
*                  FOR LS  IN I_OPERACAO
*                  FOR LS1 IN I_APONTAMENTO WHERE ( IDORD EQ LS-IDORD
*                                               AND IDOPR EQ LS-IDOPR )
*                         (
*                           ORDERID         = |{ E_ORDEM ALPHA = IN }|
*                           OPERATION       = LS-VORNR
*                           POSTG_DATE      = LS1-BUDAT
*                           WORK_CNTR       = LS1-ARBPL
*                           PERS_NO         = LS1-PERNR
*                           EXEC_START_DATE = LS1-ISDD
*                           EXEC_START_TIME = LS1-ISDZ
*                           EXEC_FIN_DATE   = LS1-IEDD
*                           EXEC_FIN_TIME   = LS1-IEDZ
*                           FIN_CONF        = LS1-AUERU
*                           ACT_WORK        = LS1-ISMNW
*                           UN_WORK         = LS1-ISMNE
*                           DEV_REASON      = LS1-GRUND
*                         )
*              ).
*
*        IF T_APONTAMENTO IS NOT INITIAL.
*          CALL FUNCTION 'BAPI_ALM_CONF_CREATE'
*            IMPORTING
*              RETURN        = _RETURN
*            TABLES
*              TIMETICKETS   = T_APONTAMENTO
*              DETAIL_RETURN = T_ALM_RETURN.
*
*          IF _RETURN IS INITIAL.
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                WAIT = ABAP_TRUE.
*          ELSE.
*            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*    ELSE.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*      CLEAR E_MSG.
*      LOOP AT T_RETURN INTO DATA(WA_RET).
*        E_MSG = COND #( WHEN E_MSG IS INITIAL THEN WA_RET-MESSAGE ELSE |{ E_MSG }, { WA_RET-MESSAGE }| ).
*      ENDLOOP.
*    ENDIF.
*
*  ENDMETHOD.                    "Z_ORDER_MAINTAIN
*
*ENDCLASS.
