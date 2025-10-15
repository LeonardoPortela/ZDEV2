class ZCL_JOB definition
  public
  final
  create public .

public section.

  constants ST_NAME_JOB_ENTRADA_ESTOQUE type BTCJOB value 'ENT_ESTOQUE_GRAOS' ##NO_TEXT.
  constants ST_NAME_JOB_ESTORNO_ESTOQUE type BTCJOB value 'ESTORNO_ESTOQUE_GRAOS' ##NO_TEXT.
  constants ST_STATUS_FINALIZADO type BTCSTATUS value 'F' ##NO_TEXT.
  constants ST_STATUS_EXECUCAO type BTCSTATUS value 'R' ##NO_TEXT.
  constants ST_STATUS_ESPERA type BTCSTATUS value 'S' ##NO_TEXT.
  constants ST_STATUS_CANCELADO type BTCSTATUS value 'A' ##NO_TEXT.

  class-methods GET_DURACAO_ULTIMO_JOB
    importing
      !I_JOB_NAME type BTCJOB
    exporting
      !E_SECOND_WAIT type SAML2_DURATION_SECOND
      !E_SECOND_EXEC type SAML2_DURATION_SECOND
    raising
      ZCX_JOB .
  class-methods GET_TIMESTEMP_BANCO
    importing
      !I_JOB_NAME type BTCJOB
    exporting
      !E_TIMESTEMP type TIMESTAMP
    raising
      ZCX_JOB .
  class-methods GET_TIME_NEXT_JOB
    importing
      !I_JOB_NAME type BTCJOB
    exporting
      !E_JOBNAME type BTCJOB
      !E_JOBCOUNT type BTCJOBCNT
      !E_SECOND_NEXT type SAML2_DURATION_SECOND
    raising
      ZCX_JOB .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_JOB .
  class-methods GET_JOB_ESCALONADO
    importing
      !I_JOB_NAME type BTCJOB
    raising
      ZCX_JOB .
  class-methods GET_JOB_EXECUCAO
    importing
      !I_JOB_NAME type BTCJOB
    raising
      ZCX_JOB .
  class-methods GET_JOB_PROGRAMA_EXECUCAO
    importing
      !I_JOB_NAME type BTCJOB optional
      !I_JOB_NAME_CP type BTCJOB optional
      !I_PROGNAME type BTCPROG optional
      !I_SDLDATE type BTCSDLDATE optional
      !I_SDLDATE_INIT type BTCSDLDATE optional
      !I_STATUS type ZDE_BTCSTATUS_T optional
    exporting
      !E_QUANTIDADE type I
      !E_JOBS type /SDF/TBTCO_TT
    raising
      ZCX_JOB .
  methods GET_JOB_REGISTRO
    exporting
      !E_REGISTRO type TBTCO
    returning
      value(R_INSTANCIA) type ref to ZCL_JOB .
  methods GET_JOB_REGISTRO_INFO
    exporting
      !E_TEXTO_INFO type CHAR200
      !E_ATRASADO type CHAR01
    returning
      value(R_INSTANCIA) type ref to ZCL_JOB
    raising
      ZCX_JOB .
  methods SET_JOB_REGISTRO
    importing
      !I_REGISTRO type TBTCO
    returning
      value(R_INSTANCIA) type ref to ZCL_JOB .
  methods SET_KEY_JOB
    importing
      !I_JOBNAME type BTCJOB
      !I_JOBCOUNT type BTCJOBCNT
    returning
      value(R_INSTANCIA) type ref to ZCL_JOB
    raising
      ZCX_JOB .
  class-methods GET_INSTANCE
    returning
      value(R_INSTANCIA) type ref to ZCL_JOB .
  methods GET_WAIT_JOB_EXEC
    importing
      !I_TEXT_WAIT type STRING optional
    returning
      value(R_INSTANCIA) type ref to ZCL_JOB
    raising
      ZCX_JOB .
  class-methods GET_USER_JOB
    returning
      value(R_NAME) type UNAME
    raising
      ZCX_JOB .
  methods GET_LOG_JOB
    exporting
      !E_LOGS type BTC_T_JOB_LOG
    returning
      value(R_INSTANCE) type ref to ZCL_JOB
    raising
      ZCX_JOB .
  class-methods GET_CK_PROGRAM_EXECUCAO
    importing
      !I_NOME_PROGRAM type SYST_CPROG
    exporting
      !E_QTD type I
    raising
      ZCX_JOB .
  class-methods SET_JOB_OPEN
    importing
      !I_NAME type BTCJOB
    returning
      value(R_JOBCOUNT) type BTCJOBCNT
    raising
      ZCX_JOB .
  class-methods SET_JOB_CLOSE .
  class-methods SET_JOB_DELETE .
  class-methods WAIT_SCHEDULE_JOB
    importing
      !I_ID_FILA type ZDE_ID_FILA_JOB
    exporting
      !E_JOBCOUNT type BTCJOBCNT
      !E_JOBNAME type BTCJOB .
  class-methods INSERT_JOB_FILA_ESCALONAMENTO
    importing
      !I_NOME_JOB type BTCJOB
      !I_REPORT type PROGNAME
      !I_USER_JOB type USNAM
      !I_RSPARAMS_T type RSPARAMS_TT optional
      !I_WAIT_SCHEDULE type CHAR01 optional
      !I_WAIT_FINISH type CHAR01 optional
      !I_PROCESSAR_RETORNO type CHAR01 optional
      !I_DADOS_PROCESSAR type STRING optional
    exporting
      !E_JOBNAME type BTCJOB
      !E_JOBCOUNT type BTCJOBCNT
      value(E_ID_FILA) type ZDE_ID_FILA_JOB
    returning
      value(R_RETURN_PROCESSAMENTO) type STRING .
protected section.
private section.

  data AT_JOBNAME type BTCJOB .
  data AT_JOBCOUNT type BTCJOBCNT .
  data AT_REGISTRO type TBTCO .
  class-data AT_JOB type ref to ZCL_JOB .

  methods SET_ATUALIZA_REGISTRO .
ENDCLASS.



CLASS ZCL_JOB IMPLEMENTATION.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_JOB
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_JOB=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_JOB=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_JOB=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_JOB=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GET_CK_PROGRAM_EXECUCAO.

    SELECT * INTO TABLE @DATA(IT_TBTCO)
      FROM TBTCO AS J
     WHERE J~STATUS EQ 'R'.

    IF SY-SUBRC IS INITIAL.
      SELECT * INTO TABLE @DATA(IT_TBTCP)
        FROM TBTCP
         FOR ALL ENTRIES IN @IT_TBTCO
       WHERE PROGNAME EQ @I_NOME_PROGRAM
         AND JOBNAME  EQ @IT_TBTCO-JOBNAME
         AND JOBCOUNT EQ @IT_TBTCO-JOBCOUNT.
    ENDIF.

    DESCRIBE TABLE IT_TBTCP LINES E_QTD.

    IF E_QTD IS INITIAL.

      RAISE EXCEPTION TYPE ZCX_JOB
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_JOB=>ZCX_PROGRAM_NOT_EXECUTING-MSGID
                            MSGNO = ZCX_JOB=>ZCX_PROGRAM_NOT_EXECUTING-MSGNO
                            ATTR1 = CONV #( I_NOME_PROGRAM ) )
          MSGID  = ZCX_JOB=>ZCX_PROGRAM_NOT_EXECUTING-MSGID
          MSGNO  = ZCX_JOB=>ZCX_PROGRAM_NOT_EXECUTING-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_NOME_PROGRAM ).

    ENDIF.

  ENDMETHOD.


  METHOD GET_DURACAO_ULTIMO_JOB.

*    TRY.
*        EXEC SQL.
*          OPEN SQL_JOB_1 FOR
*             SELECT
*                EXTRACT(DAY    FROM  (TO_TIMESTAMP(STRTDATE||STRTTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(SDLSTRTDT||SDLSTRTTM,'YYYYMMDDHH24MISS'))  ) * 60 * 60 * 24 +
*                EXTRACT(HOUR   FROM  (TO_TIMESTAMP(STRTDATE||STRTTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(SDLSTRTDT||SDLSTRTTM,'YYYYMMDDHH24MISS'))  ) * 60 * 60 +
*                EXTRACT(MINUTE FROM  (TO_TIMESTAMP(STRTDATE||STRTTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(SDLSTRTDT||SDLSTRTTM,'YYYYMMDDHH24MISS'))  ) * 60 +
*                EXTRACT(SECOND FROM  (TO_TIMESTAMP(STRTDATE||STRTTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(SDLSTRTDT||SDLSTRTTM,'YYYYMMDDHH24MISS'))  ) AS SEGUNDOS_ATRASO,
*                EXTRACT(DAY    FROM  (TO_TIMESTAMP(ENDDATE||ENDTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(STRTDATE||STRTTIME,'YYYYMMDDHH24MISS'))  ) * 60 * 60 * 24 +
*                EXTRACT(HOUR   FROM  (TO_TIMESTAMP(ENDDATE||ENDTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(STRTDATE||STRTTIME,'YYYYMMDDHH24MISS'))  ) * 60 * 60 +
*                EXTRACT(MINUTE FROM  (TO_TIMESTAMP(ENDDATE||ENDTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(STRTDATE||STRTTIME,'YYYYMMDDHH24MISS'))  ) * 60 +
*                EXTRACT(SECOND FROM  (TO_TIMESTAMP(ENDDATE||ENDTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(STRTDATE||STRTTIME,'YYYYMMDDHH24MISS'))  )     AS SEGUNDOS_EXECUCAO
*           FROM SAPHANADB.TBTCO T
*          WHERE T.JOBNAME = :I_JOB_NAME
*            AND T.ENDDATE||T.ENDTIME  = ( SELECT MAX( T2.ENDDATE||T2.ENDTIME ) FROM SAPHANADB.TBTCO T2 WHERE T2.JOBNAME = :I_JOB_NAME AND T2.STATUS = 'F' )
*        ENDEXEC.
*      CATCH CX_SY_NATIVE_SQL_ERROR INTO DATA(EXC_REF).
*        ZCL_JOB=>GERA_ERRO_GERAL( I_TEXTO = EXC_REF->GET_TEXT( ) ).
*    ENDTRY.
*
*    DO.
*      EXEC SQL.
*        FETCH NEXT SQL_JOB_1 INTO
*          :E_SECOND_WAIT,
*          :E_SECOND_EXEC
*      ENDEXEC.
*      IF SY-SUBRC IS NOT INITIAL.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    EXEC SQL.
*      CLOSE SQL_JOB_1
*    ENDEXEC.

  ENDMETHOD.


  METHOD GET_INSTANCE.

    IF AT_JOB IS NOT BOUND.
      CREATE OBJECT AT_JOB.
    ENDIF.

    R_INSTANCIA = AT_JOB.

  ENDMETHOD.


  METHOD GET_JOB_ESCALONADO.

    "Verificar se o Job Está Escalonado
    SELECT SINGLE COUNT(*) INTO @DATA(VG_JOB)
      FROM TBTCO
     WHERE JOBNAME EQ @I_JOB_NAME
       AND STATUS  EQ 'S'.

    CHECK VG_JOB IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_JOB
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_JOB=>ZCX_JOB_NAO_ESCALONADO-MSGID
                          MSGNO = ZCX_JOB=>ZCX_JOB_NAO_ESCALONADO-MSGNO
                          ATTR1 = CONV #( I_JOB_NAME ) )
        MSGID  = ZCX_JOB=>ZCX_JOB_NAO_ESCALONADO-MSGID
        MSGNO  = ZCX_JOB=>ZCX_JOB_NAO_ESCALONADO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_JOB_NAME ).

  ENDMETHOD.


  METHOD GET_JOB_EXECUCAO.

    "Verificar se o Job Está em Execução
    SELECT SINGLE COUNT(*) INTO @DATA(VG_JOB)
      FROM TBTCO
     WHERE JOBNAME EQ @I_JOB_NAME
       AND STATUS  EQ 'R'.

    CHECK VG_JOB IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_JOB
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_JOB=>ZCX_JOB_NAO_EXECUTANDO-MSGID
                          MSGNO = ZCX_JOB=>ZCX_JOB_NAO_EXECUTANDO-MSGNO
                          ATTR1 = CONV #( I_JOB_NAME ) )
        MSGID  = ZCX_JOB=>ZCX_JOB_NAO_EXECUTANDO-MSGID
        MSGNO  = ZCX_JOB=>ZCX_JOB_NAO_EXECUTANDO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_JOB_NAME ).

  ENDMETHOD.


  METHOD GET_JOB_PROGRAMA_EXECUCAO.

    DATA: RG_NAME TYPE RANGE OF BTCJOB,
          RG_PROG TYPE RANGE OF BTCPROG,
          RG_DATA TYPE RANGE OF BTCSDLDATE,
          RG_STAT TYPE RANGE OF BTCSTATUS.

    CLEAR: E_JOBS[].

    E_QUANTIDADE = 0.

    IF I_JOB_NAME IS NOT INITIAL.
      RG_NAME = VALUE #( OPTION = 'EQ' SIGN = 'I' ( LOW = I_JOB_NAME HIGH = I_JOB_NAME ) ).
    ELSEIF I_JOB_NAME_CP IS NOT INITIAL.
      RG_NAME = VALUE #( OPTION = 'CP' SIGN = 'I' ( LOW = I_JOB_NAME_CP ) ).
    ENDIF.

    IF I_PROGNAME IS NOT INITIAL.
      RG_PROG = VALUE #( OPTION = 'EQ' SIGN = 'I' ( LOW = I_PROGNAME HIGH = I_PROGNAME ) ).
    ENDIF.

    IF I_SDLDATE IS NOT INITIAL.
      RG_DATA = VALUE #( OPTION = 'EQ' SIGN = 'I' ( LOW = I_SDLDATE HIGH = I_SDLDATE ) ).
    ELSEIF I_SDLDATE_INIT IS NOT INITIAL.
      RG_DATA = VALUE #( OPTION = 'GE' SIGN = 'I' ( LOW = I_SDLDATE_INIT ) ).
    ENDIF.

    LOOP AT I_STATUS INTO DATA(WA_STATUS).
      APPEND VALUE #( OPTION = 'EQ' SIGN = 'I' LOW = WA_STATUS HIGH = WA_STATUS ) TO RG_STAT.
    ENDLOOP.

    "Verificar se o Job Está em Execução
    SELECT T~JOBNAME, T~JOBCOUNT INTO TABLE @DATA(IT_TABLE)
      FROM TBTCO AS T
     INNER JOIN TBTCP AS P ON P~JOBNAME  EQ T~JOBNAME
                          AND P~JOBCOUNT EQ T~JOBCOUNT
     WHERE T~SDLDATE  IN @RG_DATA
       AND T~JOBNAME  IN @RG_NAME
       AND T~STATUS   IN @RG_STAT
       AND P~PROGNAME IN @RG_PROG.

    DESCRIBE TABLE IT_TABLE LINES E_QUANTIDADE .

    E_JOBS[] = CORRESPONDING #( IT_TABLE[] ).

  ENDMETHOD.


  METHOD GET_JOB_REGISTRO.

    IF ME->AT_REGISTRO IS INITIAL.
      SELECT SINGLE * INTO ME->AT_REGISTRO
        FROM TBTCO
       WHERE JOBNAME  = ME->AT_JOBNAME
         AND JOBCOUNT = ME->AT_JOBCOUNT.
    ENDIF.

    E_REGISTRO = ME->AT_REGISTRO.

  ENDMETHOD.


  METHOD GET_JOB_REGISTRO_INFO.

    DATA: LC_SDLSTRTDT  TYPE BTCSDATE, " DATS  8 0 Data de execução planejada de um job em background
          LC_SDLSTRTTM  TYPE BTCSTIME, " TIMS  6 0 Hora de execução planejada de um job em background
          LC_STRTDATE	  TYPE BTCXDATE, " DATS	8	0	Data de execução de um job
          LC_STRTTIME	  TYPE BTCXTIME, " TIMS	6	0	Hora de execução de um job
          E_SECOND_NEXT	TYPE SAML2_DURATION_SECOND,
          E_SECOND_EXEC	TYPE SAML2_DURATION_SECOND,
          LC_TEMPO      TYPE C LENGTH 11.

    DATA: vl_date_1 TYPE ALDATE,
          vl_time_1 TYPE ALTIME,
          vl_date_2 TYPE ALDATE,
          vl_time_2 TYPE ALTIME,
          vl_seconds_next TYPE INT4.


    E_ATRASADO = ABAP_FALSE.

    R_INSTANCIA = ME.

    ME->SET_ATUALIZA_REGISTRO( ).

    CASE ME->AT_REGISTRO-STATUS.
      WHEN ZCL_JOB=>ST_STATUS_ESPERA.

        "Aguardando Execução
        LC_SDLSTRTDT = ME->AT_REGISTRO-SDLSTRTDT.
        LC_SDLSTRTTM = ME->AT_REGISTRO-SDLSTRTTM.


        vl_date_1 = sy-datum.
        vl_time_1 = sy-uzeit.
        vl_date_2 = LC_SDLSTRTDT.
        vl_time_2 = LC_SDLSTRTTM.


        CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
          EXPORTING
            date_1        = vl_date_1
            time_1        = vl_time_1
            date_2        = vl_date_2
            time_2        = vl_time_2
          IMPORTING
            SECONDS       = vl_seconds_next.

        E_SECOND_NEXT = vl_seconds_next.



*        TRY.
*            EXEC SQL.
*              OPEN SQL_JOB_4 FOR
*                SELECT EXTRACT(DAY    FROM  (TO_TIMESTAMP( :LC_SDLSTRTDT || :LC_SDLSTRTTM,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 * 60 * 24 +
*                       EXTRACT(HOUR   FROM  (TO_TIMESTAMP( :LC_SDLSTRTDT || :LC_SDLSTRTTM,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 * 60 +
*                       EXTRACT(MINUTE FROM  (TO_TIMESTAMP( :LC_SDLSTRTDT || :LC_SDLSTRTTM,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 +
*                       EXTRACT(SECOND FROM  (TO_TIMESTAMP( :LC_SDLSTRTDT || :LC_SDLSTRTTM,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS')))
*                       AS SECOND_NEXT
*                  FROM DUAL
*            ENDEXEC.
*          CATCH CX_SY_NATIVE_SQL_ERROR INTO DATA(EXC_REF).
*            ZCL_JOB=>GERA_ERRO_GERAL( I_TEXTO = EXC_REF->GET_TEXT( ) ).
*        ENDTRY.
*
*        DO.
*          EXEC SQL.
*            FETCH NEXT SQL_JOB_4 INTO
*              :E_SECOND_NEXT
*          ENDEXEC.
*          IF SY-SUBRC IS NOT INITIAL.
*            EXIT.
*          ENDIF.
*        ENDDO.
*
*        EXEC SQL.
*          CLOSE SQL_JOB_4
*        ENDEXEC.

        IF E_SECOND_NEXT GT 0.
          WRITE E_SECOND_NEXT TO LC_TEMPO.
          CONDENSE LC_TEMPO NO-GAPS.
          CONCATENATE 'Aguardando Execução do Job: Falta(m)' LC_TEMPO 'segundo(s).......!' INTO E_TEXTO_INFO SEPARATED BY SPACE.
          E_ATRASADO = ABAP_FALSE.
        ELSE.
          E_SECOND_NEXT = ABS( E_SECOND_NEXT ).
          WRITE E_SECOND_NEXT TO LC_TEMPO.
          CONDENSE LC_TEMPO NO-GAPS.
          CONCATENATE 'Aguardando Execução do Job: Atraso de ' LC_TEMPO 'segundo(s).......!' INTO E_TEXTO_INFO SEPARATED BY SPACE.
          E_ATRASADO = ABAP_TRUE.
        ENDIF.

      WHEN ZCL_JOB=>ST_STATUS_EXECUCAO.

        LC_STRTDATE = ME->AT_REGISTRO-STRTDATE.
        LC_STRTTIME = ME->AT_REGISTRO-STRTTIME.

        vl_date_1 = sy-datum.
        vl_time_1 = sy-uzeit.
        vl_date_2 = LC_SDLSTRTDT.
        vl_time_2 = LC_SDLSTRTTM.


        CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
          EXPORTING
            date_1        = vl_date_1
            time_1        = vl_time_1
            date_2        = vl_date_2
            time_2        = vl_time_2
          IMPORTING
            SECONDS       = vl_seconds_next.

        E_SECOND_EXEC = vl_seconds_next.


*        TRY.
*            EXEC SQL.
*              OPEN SQL_JOB_4 FOR
*                SELECT EXTRACT(DAY    FROM  (TO_TIMESTAMP( :LC_STRTDATE || :LC_STRTTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 * 60 * 24 +
*                       EXTRACT(HOUR   FROM  (TO_TIMESTAMP( :LC_STRTDATE || :LC_STRTTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 * 60 +
*                       EXTRACT(MINUTE FROM  (TO_TIMESTAMP( :LC_STRTDATE || :LC_STRTTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 +
*                       EXTRACT(SECOND FROM  (TO_TIMESTAMP( :LC_STRTDATE || :LC_STRTTIME,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS')))
*                       AS SECOND_NEXT
*                  FROM DUAL
*            ENDEXEC.
*          CATCH CX_SY_NATIVE_SQL_ERROR INTO EXC_REF.
*            ZCL_JOB=>GERA_ERRO_GERAL( I_TEXTO = EXC_REF->GET_TEXT( ) ).
*        ENDTRY.
*
*        DO.
*          EXEC SQL.
*            FETCH NEXT SQL_JOB_4 INTO
*              :E_SECOND_EXEC
*          ENDEXEC.
*          IF SY-SUBRC IS NOT INITIAL.
*            EXIT.
*          ENDIF.
*        ENDDO.
*
*        EXEC SQL.
*          CLOSE SQL_JOB_4
*        ENDEXEC.

        E_SECOND_EXEC = ABS( E_SECOND_EXEC ).
        WRITE E_SECOND_EXEC TO LC_TEMPO.
        CONDENSE LC_TEMPO NO-GAPS.
        CONCATENATE 'Aguarde, Job está em Execução: Tempo' LC_TEMPO 'segundo(s).......!' INTO E_TEXTO_INFO SEPARATED BY SPACE.

      WHEN ZCL_JOB=>ST_STATUS_FINALIZADO.

        E_TEXTO_INFO = 'Job foi executado!'.

    ENDCASE.

  ENDMETHOD.


  METHOD GET_LOG_JOB.

    DATA: I_TEXTO TYPE STRING.

    CHECK AT_REGISTRO-JOBLOG IS NOT INITIAL.
    CHECK AT_REGISTRO-JOBCOUNT IS NOT INITIAL.
    CHECK AT_REGISTRO-JOBNAME IS NOT INITIAL.

    CALL FUNCTION 'BP_JOBLOG_READ'
      EXPORTING
        JOBCOUNT              = AT_REGISTRO-JOBCOUNT
        JOBLOG                = AT_REGISTRO-JOBLOG
        JOBNAME               = AT_REGISTRO-JOBNAME
      TABLES
        JOBLOGTBL             = E_LOGS
      EXCEPTIONS
        CANT_READ_JOBLOG      = 1
        JOBCOUNT_MISSING      = 2
        JOBLOG_DOES_NOT_EXIST = 3
        JOBLOG_IS_EMPTY       = 4
        JOBLOG_NAME_MISSING   = 5
        JOBNAME_MISSING       = 6
        JOB_DOES_NOT_EXIST    = 7
        OTHERS                = 8.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO I_TEXTO.
      ME->GERA_ERRO_GERAL( I_TEXTO = I_TEXTO ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_TIMESTEMP_BANCO.

    TRY.
        EXEC SQL.
          OPEN SQL_JOB_3 FOR
            SELECT to_char(current_date,'YYYYMMDD') || to_char(current_time,'HH24MISS') AS TIMESTEMP FROM DUMMY
        ENDEXEC.
      CATCH CX_SY_NATIVE_SQL_ERROR INTO DATA(EXC_REF).
        ZCL_JOB=>GERA_ERRO_GERAL( I_TEXTO = EXC_REF->GET_TEXT( ) ).
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT SQL_JOB_3 INTO
          :E_TIMESTEMP
      ENDEXEC.
      IF SY-SUBRC IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE SQL_JOB_3
    ENDEXEC.


  ENDMETHOD.


  METHOD GET_TIME_NEXT_JOB.

    DATA: vl_date_1 TYPE ALDATE,
          vl_time_1 TYPE ALTIME,
          vl_date_2 TYPE ALDATE,
          vl_time_2 TYPE ALTIME,
          vl_seconds_next TYPE INT4.


    "Conversao Hana - Amaggi - WPP...
    CLear: E_JOBNAME, E_JOBCOUNT, E_SECOND_NEXT.


    SELECT JOBNAME, JOBcount, SDLSTRTDT, SDLSTRTTM
      FROM TBTCO AS T INTO TABLE @DATA(LIT_JOBS)
     WHERE T~JOBNAME   = @I_JOB_NAME
       AND T~STATUS    = 'S'
       AND T~SDLSTRTDT = ( SELECT MAX( T2~SDLSTRTDT )
                             FROM TBTCO AS T2
                            WHERE T2~JOBNAME = @I_JOB_NAME
                             AND T2~STATUS = 'S' ).

    SORT LIT_JOBS BY SDLSTRTTM DESCENDING.

    LOOP AT LIT_JOBS INTO DATA(LWA_JOB).

      vl_date_1 = sy-datum.
      vl_time_1 = sy-uzeit.
      vl_date_2 = LWA_JOB-SDLSTRTDT.
      vl_time_2 = LWA_JOB-SDLSTRTTM.


      CALL FUNCTION 'SALP_SM_CALC_TIME_DIFFERENCE'
        EXPORTING
          date_1        = vl_date_1
          time_1        = vl_time_1
          date_2        = vl_date_2
          time_2        = vl_time_2
        IMPORTING
          SECONDS       = vl_seconds_next.


      E_JOBNAME     = LWA_JOB-jobname.
      E_JOBCOUNT    = LWA_JOB-jobcount.
      E_SECOND_NEXT = vl_seconds_next.

      RETURN.

    ENDLOOP.

    EXIT.

    TRY.
        EXEC SQL.
          OPEN SQL_JOB_2 FOR
            SELECT T.JOBNAME,
                   T.JOBCOUNT,
                   EXTRACT(DAY    FROM  (TO_TIMESTAMP(SDLSTRTDT||SDLSTRTTM,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 * 60 * 24 +
                   EXTRACT(HOUR   FROM  (TO_TIMESTAMP(SDLSTRTDT||SDLSTRTTM,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 * 60 +
                   EXTRACT(MINUTE FROM  (TO_TIMESTAMP(SDLSTRTDT||SDLSTRTTM,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS'))) * 60 +
                   EXTRACT(SECOND FROM  (TO_TIMESTAMP(SDLSTRTDT||SDLSTRTTM,'YYYYMMDDHH24MISS') - TO_TIMESTAMP(TO_CHAR(SYSDATE,'YYYYMMDD')||TO_CHAR(SYSDATE,'HH24MISS'),'YYYYMMDDHH24MISS')))
                   AS SECOND_NEXT
              FROM SAPHANADB.TBTCO T
             WHERE T.JOBNAME = :I_JOB_NAME
               AND T.STATUS  = 'S'
               AND T.SDLSTRTDT||T.SDLSTRTTM = ( SELECT MAX( T2.SDLSTRTDT||T2.SDLSTRTTM ) FROM SAPHANADB.TBTCO T2 WHERE T2.JOBNAME = :I_JOB_NAME AND T2.STATUS = 'S' )
        ENDEXEC.
      CATCH CX_SY_NATIVE_SQL_ERROR INTO DATA(EXC_REF).
        ZCL_JOB=>GERA_ERRO_GERAL( I_TEXTO = EXC_REF->GET_TEXT( ) ).
    ENDTRY.

    DO.
      EXEC SQL.
        FETCH NEXT SQL_JOB_2 INTO
          :E_JOBNAME,
          :E_JOBCOUNT,
          :E_SECOND_NEXT
      ENDEXEC.
      IF SY-SUBRC IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

    EXEC SQL.
      CLOSE SQL_JOB_2
    ENDEXEC.

  ENDMETHOD.


  METHOD GET_USER_JOB.

    SELECT SINGLE *
      FROM SETLEAF
      INTO @DATA(WA_SETLEAF)
     WHERE SETNAME = 'MAGGI_JOB_USER'.

    IF SY-SUBRC IS INITIAL.
      R_NAME = WA_SETLEAF-VALFROM.
    ELSE.
      RAISE EXCEPTION TYPE ZCX_JOB
        EXPORTING
          TEXTID = VALUE #( MSGNO  = ZCX_JOB=>ZCX_SEM_USER_DEFAULT-MSGNO
                            MSGID  = ZCX_JOB=>ZCX_SEM_USER_DEFAULT-MSGID )
          MSGTY  = 'E'
          MSGNO  = ZCX_JOB=>ZCX_SEM_USER_DEFAULT-MSGNO
          MSGID  = ZCX_JOB=>ZCX_SEM_USER_DEFAULT-MSGID.
    ENDIF.

  ENDMETHOD.


  METHOD GET_WAIT_JOB_EXEC.

    DATA: I_TEXTO   TYPE STRING,
          I_TEMPO   TYPE I,
          LC_TEXTO  TYPE STRING,
          LC_NUMERO TYPE CHAR30.

    R_INSTANCIA = ME.

    IF SY-BATCH NE ABAP_TRUE.
      IF I_TEXT_WAIT IS NOT INITIAL.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            PERCENTAGE = 0
            TEXT       = I_TEXT_WAIT.
      ELSE.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            PERCENTAGE = 0
            TEXT       = 'Aguardar Job Retornar de Processo Solicitado'.
      ENDIF.
    ENDIF.

    I_TEMPO = 0.

    WHILE ME->AT_REGISTRO-STATUS NE ZCL_JOB=>ST_STATUS_FINALIZADO.

      IF ME->AT_REGISTRO-STATUS EQ ZCL_JOB=>ST_STATUS_CANCELADO.

        ME->GET_LOG_JOB( IMPORTING E_LOGS = DATA(E_LOGS) ).

        READ TABLE E_LOGS WITH KEY MSGTYPE = 'E' INTO DATA(WA_LOGS).
        IF SY-SUBRC IS INITIAL.
          MESSAGE ID WA_LOGS-MSGID TYPE 'S' NUMBER WA_LOGS-MSGNO WITH WA_LOGS-MSGV1 WA_LOGS-MSGV2 WA_LOGS-MSGV3 WA_LOGS-MSGV4 INTO I_TEXTO.
          ME->GERA_ERRO_GERAL( I_TEXTO = I_TEXTO ).
        ENDIF.

        READ TABLE E_LOGS WITH KEY MSGTYPE = 'A' INTO WA_LOGS.
        IF SY-SUBRC IS INITIAL.
          MESSAGE ID WA_LOGS-MSGID TYPE 'S' NUMBER WA_LOGS-MSGNO WITH WA_LOGS-MSGV1 WA_LOGS-MSGV2 WA_LOGS-MSGV3 WA_LOGS-MSGV4 INTO I_TEXTO.
          ME->GERA_ERRO_GERAL( I_TEXTO = I_TEXTO ).
        ENDIF.

      ENDIF.

      WAIT UP TO 1 SECONDS.
      ADD 1 TO I_TEMPO.
      CALL FUNCTION 'TH_REDISPATCH'.

      WRITE I_TEMPO TO LC_NUMERO.
      CONDENSE LC_NUMERO NO-GAPS.

      IF SY-BATCH NE ABAP_TRUE.
        IF I_TEXT_WAIT IS NOT INITIAL.
          LC_TEXTO = I_TEXT_WAIT && ' Segundos: ' && LC_NUMERO.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              PERCENTAGE = 0
              TEXT       = LC_TEXTO.
        ELSE.
          LC_TEXTO = 'Aguardar Job Retornar de Processo Solicitado'.
          LC_TEXTO = LC_TEXTO && ' Segundos: ' && LC_NUMERO.

          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              PERCENTAGE = 0
              TEXT       = LC_TEXTO.
        ENDIF.
      ENDIF.

      ME->SET_ATUALIZA_REGISTRO( ).

    ENDWHILE.

  ENDMETHOD.


  METHOD SET_ATUALIZA_REGISTRO.

    SELECT SINGLE * INTO ME->AT_REGISTRO
      FROM TBTCO
     WHERE JOBNAME  = ME->AT_JOBNAME
       AND JOBCOUNT = ME->AT_JOBCOUNT.

  ENDMETHOD.


  method SET_JOB_CLOSE.

  endmethod.


  method SET_JOB_DELETE.

  endmethod.


  METHOD SET_JOB_OPEN.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
*       DELANFREP        = ' '
*       JOBGROUP         = ' '
        JOBNAME          = I_NAME
*       SDLSTRTDT        = NO_DATE
*       SDLSTRTTM        = NO_TIME
*       JOBCLASS         =
*       CHECK_JOBCLASS   =
      IMPORTING
        JOBCOUNT         = R_JOBCOUNT
*       INFO             =
* CHANGING
*       RET              =
      EXCEPTIONS
        CANT_CREATE_JOB  = 1
        INVALID_JOB_DATA = 2
        JOBNAME_MISSING  = 3
        OTHERS           = 4.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_JOB
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD SET_JOB_REGISTRO.

    R_INSTANCIA = ME.

    ME->AT_REGISTRO = I_REGISTRO.

  ENDMETHOD.


  METHOD SET_KEY_JOB.

    R_INSTANCIA = ME.

    ME->AT_JOBCOUNT = I_JOBCOUNT.
    ME->AT_JOBNAME  = I_JOBNAME.
    ME->SET_ATUALIZA_REGISTRO( ).

  ENDMETHOD.


  METHOD insert_job_fila_escalonamento.

    DATA: lwa_zjob0003     TYPE zjob0003,
          lit_zjob0004     TYPE TABLE OF zjob0004,
          lva_id_parametro TYPE zjob0004-id_parametro.

    CLEAR: r_return_processamento, e_id_fila, lit_zjob0004[], e_jobname, e_jobcount.

    CHECK i_nome_job IS NOT INITIAL AND
          i_report   IS NOT INITIAL.

    CLEAR: lwa_zjob0003.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZID_FL_JOB'
      IMPORTING
        number                  = lwa_zjob0003-id_fila_job
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF ( sy-subrc IS NOT INITIAL ) OR ( lwa_zjob0003-id_fila_job IS INITIAL ).
      MESSAGE 'Não foi possivel gerar um id para o Objeto de Numeração ZID_FL_JOB' TYPE 'E'.
      EXIT.
    ENDIF.

    lwa_zjob0003-jobname              = i_nome_job.
    lwa_zjob0003-progname             = i_report.
    lwa_zjob0003-dados_processar      = i_dados_processar.

    IF i_user_job IS NOT INITIAL.
      lwa_zjob0003-user_job = i_user_job.
    ELSE.
      lwa_zjob0003-user_job = zcl_job=>get_instance( )->get_user_job( ).
    ENDIF.

    lwa_zjob0003-user_create = sy-uname.
    lwa_zjob0003-date_create = sy-datum.
    lwa_zjob0003-time_create = sy-uzeit.


    MODIFY zjob0003 FROM lwa_zjob0003.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    CLEAR: lva_id_parametro.
    LOOP AT i_rsparams_t INTO DATA(lwa_param).

      ADD 1 TO lva_id_parametro.
      APPEND INITIAL LINE TO lit_zjob0004 ASSIGNING FIELD-SYMBOL(<fs_zjob0004>).

      <fs_zjob0004>-id_fila_job       =  lwa_zjob0003-id_fila_job.
      <fs_zjob0004>-id_parametro      =  lva_id_parametro.
      <fs_zjob0004>-selname           =  lwa_param-selname.
      <fs_zjob0004>-kind              =  lwa_param-kind.
      <fs_zjob0004>-sign              =  lwa_param-sign.
      <fs_zjob0004>-opcao             =  lwa_param-option.
      <fs_zjob0004>-low               =  lwa_param-low.
      <fs_zjob0004>-high              =  lwa_param-high.
    ENDLOOP.

    IF i_processar_retorno EQ abap_true.

      APPEND INITIAL LINE TO lit_zjob0004 ASSIGNING <fs_zjob0004>.

      ADD 1 TO lva_id_parametro.

      <fs_zjob0004>-id_fila_job       =  lwa_zjob0003-id_fila_job.
      <fs_zjob0004>-id_parametro      =  lva_id_parametro.
      <fs_zjob0004>-selname           =  'P_IDFILA'.
      <fs_zjob0004>-kind              =  'P'.
      <fs_zjob0004>-sign              =  'I'.
      <fs_zjob0004>-opcao             =  'EQ'.
      <fs_zjob0004>-low               =  lwa_zjob0003-id_fila_job.
    ENDIF.


    IF lit_zjob0004[] IS NOT INITIAL.
      MODIFY zjob0004 FROM TABLE lit_zjob0004.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        RETURN.
      ENDIF.
    ENDIF.

    e_id_fila = lwa_zjob0003-id_fila_job.

    COMMIT WORK.

    CHECK i_wait_schedule IS NOT INITIAL. "Aguardar Scheduler Job

    zcl_job=>wait_schedule_job( EXPORTING
                                    i_id_fila  =  e_id_fila
                                IMPORTING
                                    e_jobcount =  e_jobcount
                                    e_jobname  =  e_jobname ).

    CHECK e_jobcount IS NOT INITIAL AND e_jobname IS NOT INITIAL.


    CHECK i_wait_finish IS NOT INITIAL. "Aguardar Concluir Execução Job

    zcl_job=>get_instance( )->set_key_job( i_jobname  = e_jobname
                                           i_jobcount = e_jobcount
                           )->get_wait_job_exec( ).

    IF i_processar_retorno EQ abap_true.

      SELECT SINGLE *
        FROM zjob0003 INTO @DATA(lwa_zjob0003_current)
       WHERE id_fila_job EQ @lwa_zjob0003-id_fila_job.

      CHECK sy-subrc EQ 0.

      r_return_processamento = lwa_zjob0003_current-dados_processados.

    ENDIF.


  ENDMETHOD.


  METHOD wait_schedule_job.

    CLEAR: e_jobcount , e_jobname.

    CHECK i_id_fila IS NOT INITIAL.

    DO 300 TIMES.

      SELECT SINGLE *
        FROM zjob0003 INTO @DATA(lwa_zjob0003)
       WHERE id_fila_job EQ @i_id_fila.

      IF sy-subrc NE 0.
        RETURN.
      ENDIF.

      IF lwa_zjob0003-jobcount IS NOT INITIAL.
        e_jobcount  = lwa_zjob0003-jobcount.
        e_jobname   = lwa_zjob0003-jobname.
        RETURN.
      ENDIF.

      WAIT UP TO 2 SECONDS.
    ENDDO.


  ENDMETHOD.
ENDCLASS.
