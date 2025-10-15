FUNCTION Z_LIBERA_PERMITS_PM.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_AUFNR) TYPE  AUFK-AUFNR
*"     REFERENCE(I_USER) TYPE  USNAM
*"     REFERENCE(I_ACAO) TYPE  SWR_DECIKEY
*"  EXPORTING
*"     REFERENCE(E_CODIGO)
*"     REFERENCE(E_MENSAGEM)
*"----------------------------------------------------------------------

  DATA: LV_AUFNR        TYPE AUFK-AUFNR,
        LV_OBJNR        TYPE AUFK-OBJNR,
        LV_ORDERID      TYPE ALM_ME_ORDER_HEADER-ORDERID,
        LV_ERRO         TYPE OAX,
        LV_NUM_PERMITS  TYPE NUMC2,
        GW_ORDER_HEADER TYPE ALM_ME_ORDER_HEADER,
        GW_USER_DATA    TYPE ALM_ME_USER_DATA,
        GW_USER_PROFILE TYPE ALM_ME_C010PRF.

  DATA: GT_GNSVB        LIKE GNSVB                       OCCURS 0 WITH HEADER LINE,
*        GT_ORDER_OPER   LIKE ALM_ME_ORDER_OPERATION      OCCURS 0 WITH HEADER LINE,
        GT_METHODS      TYPE BAPI_ALM_ORDER_METHOD       OCCURS 0 WITH HEADER LINE,
        GT_HEADER       TYPE BAPI_ALM_ORDER_HEADERS_I    OCCURS 0 WITH HEADER LINE,
        GT_HEADER_UP    TYPE BAPI_ALM_ORDER_HEADERS_UP   OCCURS 0 WITH HEADER LINE,
        GT_PARTNER      TYPE BAPI_ALM_ORDER_PARTN_MUL    OCCURS 0 WITH HEADER LINE,
        GT_PARTNER_UP   TYPE BAPI_ALM_ORDER_PARTN_MUL_UP OCCURS 0 WITH HEADER LINE,
        GT_OPERATION    TYPE BAPI_ALM_ORDER_OPERATION    OCCURS 0 WITH HEADER LINE,
        GT_OPERATION_UP TYPE BAPI_ALM_ORDER_OPERATION_UP OCCURS 0 WITH HEADER LINE,
        GT_NUMBERS      TYPE BAPI_ALM_NUMBERS            OCCURS 0 WITH HEADER LINE,
        GW_BPGE         TYPE BPGE,
        GT_RETURN       LIKE BAPIRET2                    OCCURS 0 WITH HEADER LINE,
        GW_RETURN       LIKE BAPIRET2,
        GT_BPAK         TYPE TABLE OF BPAK,
        GW_BPAK         TYPE BPAK,
        GT_IHGNS        TYPE TABLE OF IHGNS,
        GW_IHGNS        TYPE IHGNS,
        GT_IHSG         TYPE TABLE OF IHSG,
        GW_IHSG         TYPE IHSG,
        GT_ZPMR0002     TYPE TABLE OF ZPMR0002,
        GW_ZPMR0002     TYPE ZPMR0002.

** Pega informações detalhada da ordem
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = I_AUFNR
    IMPORTING
      OUTPUT = LV_ORDERID.

  CALL FUNCTION 'ALM_ME_ORDER_GETDETAIL'
    EXPORTING
      ORDERID       = LV_ORDERID
      RESOURCE      = 'X'
      USERDATA      = GW_USER_DATA
      ORDER_PROFILE = GW_USER_PROFILE
    IMPORTING
      ORDER_HEADER  = GW_ORDER_HEADER
    EXCEPTIONS
      READ_ERROR    = 1.

  IF SY-SUBRC IS INITIAL.
** Verificar se usuário tem permissão
    SELECT *
      FROM ZPMR0002
      INTO TABLE GT_ZPMR0002
      WHERE APROVADOR EQ I_USER.

    IF GT_ZPMR0002 IS INITIAL.
      E_CODIGO    = 'E000'.
      E_MENSAGEM  = 'Não possui permissão'.
    ELSE.
      SELECT *
        FROM IHSG
        INTO TABLE GT_IHSG
        WHERE OBJNR EQ GW_ORDER_HEADER-OBJECT_NO
         AND  LVORM EQ ''
        ORDER BY COUNTER.

      LOOP AT GT_ZPMR0002 INTO GW_ZPMR0002 WHERE CENTRO_DESP CS GW_ORDER_HEADER-PLANPLANT.
** Checa se encontra permit para o nível adequado do usuário
        READ TABLE GT_IHSG INTO GW_IHSG INDEX GW_ZPMR0002-NIVEL.
        IF SY-SUBRC IS INITIAL.
** Checa se há histórico de liberação da permit
          SELECT *
            INTO TABLE GT_IHGNS
            FROM IHGNS
            WHERE OBJNR   EQ GW_IHSG-OBJNR
             AND  COUNTER EQ GW_IHSG-COUNTER.

          IF GT_IHGNS[] IS NOT INITIAL.
** Libera permit quando já existe histórico
            SORT GT_IHGNS BY GENDATUM DESCENDING GENTIME DESCENDING COUNTER ASCENDING.
            READ TABLE GT_IHGNS INTO GW_IHGNS INDEX 1.

            IF I_ACAO EQ '0001'.
              IF GW_IHGNS-GENIAKT IS INITIAL.
                E_CODIGO   = 'E000'.
                E_MENSAGEM = 'Permissão já concedida'.
              ELSE.
                GT_GNSVB-OBJNR    = GW_ORDER_HEADER-OBJECT_NO.
                GT_GNSVB-COUNTER  = GW_IHGNS-COUNTER.
                GT_GNSVB-COUNTERS = GW_IHGNS-COUNTER.
                ADD 1 TO GT_GNSVB-COUNTERS.
                GT_GNSVB-GENPAUS  = 'X'.
                GT_GNSVB-GENNAME  = I_USER.
                GT_GNSVB-GENDATUM = SY-DATUM.
                GT_GNSVB-GENTIME  = SY-UZEIT.
                GT_GNSVB-GENVNAME = I_USER.
                GT_GNSVB-AKTIV    = '2'.

                CLEAR: GT_GNSVB-GENIAKT,
                       GT_GNSVB-GENINAME,
                       GT_GNSVB-GENIDATE,
                       GT_GNSVB-GENITIME.

                APPEND GT_GNSVB.

                CALL FUNCTION 'PM_SP_ISSUE_POST' IN UPDATE TASK
                  EXPORTING
                    I_BELGNSTAB = GT_GNSVB[].
*
                COMMIT WORK.
                WAIT UP TO 2 SECONDS.

                E_CODIGO   = 'S000'.
                E_MENSAGEM = 'Permissão concedida'.

              ENDIF.
            ELSEIF I_ACAO EQ '0002'.
              IF GW_IHGNS-GENIAKT IS INITIAL.
                E_CODIGO   = 'E000'.
                E_MENSAGEM = 'Permissão já anulada'.
              ELSE.
                GT_GNSVB-OBJNR    = GW_ORDER_HEADER-OBJECT_NO.
                GT_GNSVB-COUNTER  = GW_IHSG-COUNTER.
                GT_GNSVB-COUNTERS	= GW_IHGNS-COUNTERS.
                GT_GNSVB-GENPAUS  = 'X'.
                GT_GNSVB-GENNAME  = SY-UNAME.
                GT_GNSVB-GENDATUM	= SY-DATUM.
                GT_GNSVB-GENTIME  = SY-UZEIT.
                GT_GNSVB-GENVNAME = SY-UNAME.
                GT_GNSVB-AKTIV    = '1'.
                GT_GNSVB-GENIAKT  = 'X'.
                GT_GNSVB-GENINAME = SY-UNAME.
                GT_GNSVB-GENIDATE = SY-DATUM.
                GT_GNSVB-GENITIME = SY-UZEIT.

                APPEND GT_GNSVB.

                CALL FUNCTION 'PM_SP_ISSUE_POST' IN UPDATE TASK
                  EXPORTING
                    I_BELGNSTAB = GT_GNSVB[].

                COMMIT WORK.
                WAIT UP TO 2 SECONDS.

                E_CODIGO   = 'S000'.
                E_MENSAGEM = 'Permissão anulada'.

              ENDIF.
            ENDIF.
          ELSE.
            IF I_ACAO EQ '0001'.
** Libera a permit quando ainda não há histórico
              GT_GNSVB-OBJNR    = GW_ORDER_HEADER-OBJECT_NO.
              GT_GNSVB-COUNTER  = GW_IHSG-COUNTER.
              GT_GNSVB-COUNTERS = GW_IHSG-COUNTER.
              GT_GNSVB-GENPAUS  = 'X'.
              GT_GNSVB-GENNAME  = I_USER.
              GT_GNSVB-GENDATUM = SY-DATUM.
              GT_GNSVB-GENTIME  = SY-UZEIT.
              GT_GNSVB-GENVNAME = I_USER.
              GT_GNSVB-AKTIV    = '2'.

              CLEAR: GT_GNSVB-GENIAKT,
                     GT_GNSVB-GENINAME,
                     GT_GNSVB-GENIDATE,
                     GT_GNSVB-GENITIME.

              APPEND GT_GNSVB.

              CALL FUNCTION 'PM_SP_ISSUE_POST' IN UPDATE TASK
                EXPORTING
                  I_BELGNSTAB = GT_GNSVB[].

              COMMIT WORK.
              WAIT UP TO 2 SECONDS.

              E_CODIGO   = 'S000'.
              E_MENSAGEM = 'Permissão concedida'.

            ELSEIF I_ACAO EQ '0002'.
              GT_GNSVB-OBJNR    = GW_ORDER_HEADER-OBJECT_NO.
              GT_GNSVB-COUNTER  = GW_IHSG-COUNTER.
              GT_GNSVB-COUNTERS	= GW_IHSG-COUNTER.
              GT_GNSVB-GENPAUS  = 'X'.
              GT_GNSVB-GENNAME  = SY-UNAME.
              GT_GNSVB-GENDATUM	= SY-DATUM.
              GT_GNSVB-GENTIME  = SY-UZEIT.
              GT_GNSVB-GENVNAME = SY-UNAME.
              GT_GNSVB-AKTIV    = '1'.
              GT_GNSVB-GENIAKT  = 'X'.
              GT_GNSVB-GENINAME = SY-UNAME.
              GT_GNSVB-GENIDATE = SY-DATUM.
              GT_GNSVB-GENITIME = SY-UZEIT.

              APPEND GT_GNSVB.

              CALL FUNCTION 'PM_SP_ISSUE_POST' IN UPDATE TASK
                EXPORTING
                  I_BELGNSTAB = GT_GNSVB[].

              COMMIT WORK.
              WAIT UP TO 2 SECONDS.

              E_CODIGO   = 'S000'.
              E_MENSAGEM = 'Permissão anulada'.
            ENDIF.
          ENDIF.
** Checando permissão de ordem
          IF  GW_ORDER_HEADER-ESTIMATED_COSTS LE GW_ZPMR0002-VALOR_ATE
          OR  GW_ZPMR0002-VALOR_ATE EQ 0.

** Buscar dados de orçamento de ordem
            SELECT SINGLE *    "#EC CI_DB_OPERATION_OK[2226048]
              FROM BPGE        "#EC CI_DB_OPERATION_OK[2226072]
              INTO GW_BPGE
              WHERE OBJNR = GW_ORDER_HEADER-OBJECT_NO
               AND  VORGA = 'KBUD'.

            IF SY-SUBRC IS NOT INITIAL.
              CLEAR: GW_BPAK.
              GW_BPAK-E_OBJNR = GW_ORDER_HEADER-OBJECT_NO.
              GW_BPAK-BLDAT   = SY-DATUM.
              GW_BPAK-WERT    = GW_ORDER_HEADER-ESTIMATED_COSTS.
              GW_BPAK-SGTEXT  = 'Automático'.
              GW_BPAK-TWAER   = GW_ORDER_HEADER-CURRENCY.
              APPEND GW_BPAK TO GT_BPAK.

**    Setar valor de orçamento para o ano
              CLEAR: GW_BPAK.
              GW_BPAK-E_OBJNR = GW_ORDER_HEADER-OBJECT_NO.
              GW_BPAK-BLDAT   = SY-DATUM.
              GW_BPAK-S_GJAHR = SY-DATUM(4).
              GW_BPAK-E_GJAHR = SY-DATUM(4).
              GW_BPAK-WERT    = GW_ORDER_HEADER-ESTIMATED_COSTS.
              GW_BPAK-SGTEXT  = 'Automático'.
              GW_BPAK-TWAER   = GW_ORDER_HEADER-CURRENCY.
              APPEND GW_BPAK TO GT_BPAK.

**    Libera orçamento para ordem
              CALL FUNCTION 'KBPP_EXTERN_UPDATE_CO'
                EXPORTING
                  I_BUDGET_ACTIVITY = 'KBUD'
                  I_COMMIT_ALL      = 'X'
                IMPORTING
                  E_ERRORS_FOUND    = LV_ERRO
                TABLES
                  IT_BPAK           = GT_BPAK
                  IT_RETURN         = GT_RETURN
                EXCEPTIONS
                  NO_UPDATE         = 1.

              COMMIT WORK.    "alrs
              WAIT UP TO 2 SECONDS.

** Gravar logs da operação de liberação de orçamento
              CALL FUNCTION 'Z_GRAVA_LOG_PM'
                TABLES
                  T_RETURN = GT_RETURN.

            ENDIF.

            CALL FUNCTION 'Z_LISTA_ORDEM_PERMITS'
              EXPORTING
                I_USER  = SY-UNAME
                I_AUFNR = LV_ORDERID
              IMPORTING
                E_LINES = LV_NUM_PERMITS.

            IF LV_NUM_PERMITS IS INITIAL.
              GT_METHODS-REFNUMBER  = 1.
              GT_METHODS-OBJECTTYPE = 'HEADER'.
              GT_METHODS-METHOD     = 'RELEASE'.
              GT_METHODS-OBJECTKEY  = GW_ORDER_HEADER-ORDERID.
              APPEND GT_METHODS.

              GT_METHODS-OBJECTTYPE = ''.
              GT_METHODS-METHOD     = 'SAVE'.
              APPEND GT_METHODS.

              GT_HEADER-ORDERID =  GW_ORDER_HEADER-ORDERID.
              APPEND GT_HEADER.
* ---> S4 Migration - 22/06/2023 - MA
              CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN' "#EC CI_USAGE_OK[2669857]
                TABLES                                "#EC CI_USAGE_OK[2438131]
                  IT_METHODS      = GT_METHODS
                  IT_HEADER       = GT_HEADER
                  IT_HEADER_UP    = GT_HEADER_UP
                  IT_PARTNER      = GT_PARTNER
                  IT_PARTNER_UP   = GT_PARTNER_UP
                  IT_OPERATION    = GT_OPERATION
                  IT_OPERATION_UP = GT_OPERATION_UP
                  RETURN          = GT_RETURN
                  ET_NUMBERS      = GT_NUMBERS.
* <--- S4 Migration - 22/06/2023 - MA
** Gravar logs da operação de liberação da ordem
              CALL FUNCTION 'Z_GRAVA_LOG_PM'
                TABLES
                  T_RETURN = GT_RETURN.

              READ TABLE GT_RETURN INTO GT_RETURN WITH KEY TYPE   = 'S'
                                                           ID     = 'CO'
                                                           NUMBER = '085'.
              IF SY-SUBRC IS INITIAL.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    WAIT   = 'X'
                  IMPORTING
                    RETURN = GT_RETURN.

                WAIT UP TO 2 SECONDS.

                E_CODIGO   = 'S000'.
                E_MENSAGEM = 'Ordem liberada'.

              ELSE.
                E_CODIGO   = 'S000'.
                E_MENSAGEM = 'Permissão concedida'.

              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          E_CODIGO   = 'E000'.
          E_MENSAGEM = 'Não possui permissão'.
        ENDIF.
      ENDLOOP.

      IF SY-SUBRC IS NOT INITIAL.
        E_CODIGO   = 'E000'.
        E_MENSAGEM = 'Não possui permissão'.

      ENDIF.
    ENDIF.
  ELSE.
    E_CODIGO   = 'E000'.
    E_MENSAGEM = 'Ordem não encontrada'.

  ENDIF.

ENDFUNCTION.
