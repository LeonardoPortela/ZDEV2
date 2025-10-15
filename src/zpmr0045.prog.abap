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
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br )                  |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Report de Execução da ordem gerado apartir do Mobile                      |*
**/===========================================================================\*

REPORT ZPMR0045.

DATA:  WA_INPUT         TYPE ZPMT0013.

DATA: IT_BDCDATA TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      T_MESSTAB  TYPE TABLE OF          BDCMSGCOLL,
      WA_BDCDATA LIKE LINE OF           IT_BDCDATA,
      WL_RETURN  TYPE BAPIRET2,
      ET_RETURN  TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
      LOG_ERRO   TYPE ZPMT0018_T,
      LS_RETURN  TYPE BAPIRET2.

DATA T_ORDEM TYPE ZPMT0016_T.
DATA T_OPERACOES TYPE ZPMT0017_T.
DATA T_APONTAMENTOS TYPE ZPMT0015_T.
DATA T_ZPMT0017 TYPE ZPMT0017_T.
DATA T_ZPMT0015 TYPE ZPMT0015_T.

* "// Criando o Objeto da Classe
DATA(OBJ_CREATE) = NEW ZCL_PM_ORDEM( ).

START-OF-SELECTION.

* "// seleciona os dados da Ordem pegando somente os que foi enviado no momento pelo Mobile
  SELECT *
     FROM ZPMT0016
      INTO TABLE @DATA(T_ZPMT0016)
      WHERE STATP EQ @ABAP_FALSE.

* "// Processa as Ordem Enviadas
  LOOP AT T_ZPMT0016 ASSIGNING FIELD-SYMBOL(<_ORDEM>).

*   "// Seleciona as Operações com as Ordens Selecionadas Anteriormente
    SELECT *
         FROM ZPMT0017
          INTO TABLE T_ZPMT0017
          WHERE BLOCO EQ <_ORDEM>-BLOCO
          AND IDORD EQ <_ORDEM>-IDORD.

    IF T_ZPMT0017 IS NOT INITIAL.
*     "// Seleciona os Apontamentos com as Operações Selecionadas Anteriormente
      SELECT *
           FROM ZPMT0015
            INTO TABLE T_ZPMT0015
          FOR ALL ENTRIES IN T_ZPMT0017
            WHERE BLOCO EQ T_ZPMT0017-BLOCO
            AND IDORD EQ T_ZPMT0017-IDORD
            AND IDOPR EQ T_ZPMT0017-IDOPR.
    ENDIF.

*   "// Verifica se é Change ou Create
    DATA(_UPDATE) = COND #( WHEN <_ORDEM>-AUFNR IS NOT INITIAL THEN ABAP_TRUE ELSE ABAP_FALSE ).
*   "// Chama a BABI para Criar/Modificar as Ordens
    CALL METHOD OBJ_CREATE->CRIAR_ORDENS
      EXPORTING
        I_ORDEM       = <_ORDEM>      " Estrutura Ordem
        I_OPERACAO    = T_ZPMT0017    " Tabela de Operações
        I_APONTAMENTO = T_ZPMT0015    " Tabela de Apontamento
      IMPORTING
        E_ORDEM       = DATA(_ORDEM)  " Retorno da Ordem
        E_MSG         = DATA(_MSG).   " Retorno da mensagen de Erro/Sucesso

*   "// Ordem preenchida
    IF _ORDEM IS NOT INITIAL.
*     "// Preenche a ordem com ZEROS
      <_ORDEM>-AUFNR = |{ _ORDEM ALPHA = IN }|.
*     "// Autera o Status do Processamento para "P" de Processado
      <_ORDEM>-STATP = 'P'.  "// Processado
    ELSE.
*     "// Autera o Status do Processamento para "E" de Erro
      <_ORDEM>-STATP = 'E'.  "// Erro no Processamento
    ENDIF.

* "// Aplica na Tebela Fisica
    MODIFY ZPMT0016 FROM <_ORDEM>.
    COMMIT WORK.

  ENDLOOP.

************************************************************************************************
*  O trecho de codigo abaixo foi enviado para uma classe Global
************************************************************************************************

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
*    METHODS LOG_ERROS.
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
** "// Criando o Objeto da Classe
*DATA(OBJ_CREATE) = NEW Z_CREATE_DADOS( ).
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
*  METHOD LOG_ERROS.
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
*      LOG_ERRO = VALUE #(
*      FOR L_RET IN T_RETURN (
*                                BLOCO   = I_ORDEM-BLOCO
*                                IDORD   = I_ORDEM-IDORD
*                                IDOPR   = I_ORDEM-IDORD
*                                MESSAGE = L_RET-MESSAGE
*                            )
*             ).
*
*      MODIFY ZPMT0018 FROM TABLE LOG_ERRO.
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
