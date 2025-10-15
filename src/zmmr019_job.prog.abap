*&---------------------------------------------------------------------*
*& Report  ZMMR019_JOB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR019_JOB MESSAGE-ID ZJOB.

  RANGES: R_BUKRS_EXT FOR T001-BUKRS.

  CONSTANTS: C_ZMMR019                 TYPE BTCPROG VALUE 'ZMMR019',
             C_ZMMR020                 TYPE BTCPROG VALUE 'ZMMR020',
             C_ENT_ESTOQUE_GRAOS       TYPE BTCJOB  VALUE 'ENT_ESTOQUE_GRAOS',
             C_ENT_ESTOQUE_GRAOS_0100  TYPE BTCJOB  VALUE 'ENT_ESTOQUE_GRAOS_0100'.

  TYPES: BEGIN OF TY_REG_PROCESSAR,
           OBJ_KEY   TYPE ZMMT_EE_ZGR-OBJ_KEY,
           PROGNAME  TYPE BTCPROG,
           EBELN     TYPE EKPO-EBELN,
           BUKRS     TYPE BUKRS,
           RG_PROC   TYPE C,
           INC_PROC  TYPE C,
           AREA_AVAL TYPE C LENGTH 15,
           ERDAT     TYPE ERDAT,
           ERZET     TYPE ERZET,
         END OF TY_REG_PROCESSAR.

  DATA: NUMBER           TYPE TBTCJOB-JOBCOUNT,
        NAME             TYPE TBTCJOB-JOBNAME,
        JOB_NAME_LIKE    TYPE TBTCJOB-JOBNAME,
        LC_BELNR_EKBE    TYPE EKBE-BELNR,
        PRINT_PARAMETERS TYPE PRI_PARAMS,
        LC_LIM_JOBS_EXEC TYPE I,
        LC_WAIT_INT_PROC TYPE I,
        LC_QTDE_TIMES    TYPE I,
        LC_ESCALONAR_AREA_AVAL TYPE C,
        LC_PROC_PENDENTES  TYPE C,
        LC_SHOW_MSG        TYPE C,
        LX_QUANTIDADE      TYPE C LENGTH 10.

  DATA: XV_JOBNM TYPE BTCJOB,
        XV_STEPC TYPE BTCSTEPCNT,
        VG_JOB   TYPE I.

  DATA: IT_REG_PROC      TYPE TABLE OF TY_REG_PROCESSAR WITH HEADER LINE,
        IT_REG_PROC_AREA TYPE TABLE OF TY_REG_PROCESSAR WITH HEADER LINE,
        IT_REG_PROC_AUX  TYPE TABLE OF TY_REG_PROCESSAR WITH HEADER LINE,
        IT_ENTRADA_EST   TYPE TABLE OF ZMMT_EE_ZGR      WITH HEADER LINE,
        IT_ESTORNO_EST   TYPE TABLE OF ZMMT_EEE_ZGR     WITH HEADER LINE,
        IT_EKBE          TYPE TABLE OF EKBE             WITH HEADER LINE,
        IT_EKBE_TMP      TYPE TABLE OF EKBE             WITH HEADER LINE,
        IT_EKPO          TYPE TABLE OF EKPO             WITH HEADER LINE,
        IT_EKKO          TYPE TABLE OF EKKO             WITH HEADER LINE.

  CLEAR: XV_JOBNM, XV_STEPC, VG_JOB, LC_WAIT_INT_PROC.

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      JOBNAME         = XV_JOBNM
      STEPCOUNT       = XV_STEPC
    EXCEPTIONS
      NO_RUNTIME_INFO = 1
      OTHERS          = 2.

  CHECK XV_JOBNM IS NOT INITIAL.

  CASE XV_JOBNM.
    WHEN C_ENT_ESTOQUE_GRAOS.

      SELECT SINGLE COUNT(*) INTO VG_JOB
        FROM TBTCO
       WHERE JOBNAME EQ C_ENT_ESTOQUE_GRAOS
         AND STATUS EQ 'R'.

    WHEN C_ENT_ESTOQUE_GRAOS_0100.

      SELECT SINGLE COUNT(*) INTO VG_JOB
        FROM TBTCO
       WHERE JOBNAME EQ C_ENT_ESTOQUE_GRAOS_0100
         AND STATUS  EQ 'R'.

    WHEN OTHERS.
      RETURN.
  ENDCASE.

  CHECK VG_JOB EQ 1.

  CLEAR: IT_REG_PROC[], IT_ENTRADA_EST[], IT_ESTORNO_EST[], IT_EKBE[], IT_EKKO[], R_BUKRS_EXT[].

  MESSAGE S010.


  R_BUKRS_EXT-SIGN   = 'I'.
  R_BUKRS_EXT-OPTION = 'EQ'.
  R_BUKRS_EXT-LOW    = '0100'.
  APPEND R_BUKRS_EXT.

  R_BUKRS_EXT-LOW    = '0101'.
  APPEND R_BUKRS_EXT.

*-----------------------------------------------------------------------------*
* Get registros do programa ZMMR019(Criação Registros) para processamento
*-----------------------------------------------------------------------------*

  SELECT *
    FROM ZMMT_EE_ZGR
    INTO TABLE IT_ENTRADA_EST
   WHERE ZRG_ATLZ      EQ '0'
     AND ( TP_OPERACAO IN ('01', '02', '03', '04', '08', '09', '10' ) OR TP_OPERACAO EQ SPACE )
    ORDER BY CH_REFERENCIA.

  LOOP AT IT_ENTRADA_EST.
    CLEAR: IT_REG_PROC.

    IT_REG_PROC-OBJ_KEY  = IT_ENTRADA_EST-OBJ_KEY.
    IT_REG_PROC-PROGNAME = C_ZMMR019.
    IT_REG_PROC-EBELN    = IT_ENTRADA_EST-PO_NUMBER.
    IT_REG_PROC-BUKRS    = IT_ENTRADA_EST-COMP_CODE.
    IT_REG_PROC-ERDAT    = IT_ENTRADA_EST-ZDT_ATLZ.
    IT_REG_PROC-ERZET    = IT_ENTRADA_EST-ZHR_ATLZ.
    APPEND IT_REG_PROC.
  ENDLOOP.

*-----------------------------------------------------------------------------*
* Get registros do programa ZMMR020(Estorno Registros) para processamento
*-----------------------------------------------------------------------------*

  SELECT *
    FROM ZMMT_EEE_ZGR
    INTO TABLE IT_ESTORNO_EST
   WHERE RG_ATUALIZADO EQ '0'.

  IF IT_ESTORNO_EST[] IS NOT INITIAL.
    SELECT *
      FROM EKBE APPENDING TABLE IT_EKBE
       FOR ALL ENTRIES IN IT_ESTORNO_EST
     WHERE BELNR EQ IT_ESTORNO_EST-MBLNR.

    SELECT *
      FROM EKBE APPENDING TABLE IT_EKBE
       FOR ALL ENTRIES IN IT_ESTORNO_EST
     WHERE BELNR EQ IT_ESTORNO_EST-RE_BELNR.

    DELETE IT_EKBE WHERE BELNR IS INITIAL.

    IF IT_EKBE[] IS NOT INITIAL.

      IT_EKBE_TMP[] = IT_EKBE[].

      SORT                            IT_EKBE_TMP BY        EBELN.
      DELETE ADJACENT DUPLICATES FROM IT_EKBE_TMP COMPARING EBELN.

      SELECT *
        FROM EKKO INTO TABLE IT_EKKO
         FOR ALL ENTRIES IN IT_EKBE_TMP
       WHERE EBELN EQ IT_EKBE_TMP-EBELN.
    ENDIF.
  ENDIF.

  LOOP AT IT_ESTORNO_EST.
    CLEAR: IT_REG_PROC, LC_BELNR_EKBE.

    IT_REG_PROC-OBJ_KEY  = IT_ESTORNO_EST-OBJ_KEY.
    IT_REG_PROC-PROGNAME = C_ZMMR020.

    IF IT_ESTORNO_EST-MBLNR IS NOT INITIAL.
      LC_BELNR_EKBE = IT_ESTORNO_EST-MBLNR.
    ELSEIF IT_ESTORNO_EST-RE_BELNR IS NOT INITIAL.
      LC_BELNR_EKBE = IT_ESTORNO_EST-RE_BELNR.
    ENDIF.

    IF LC_BELNR_EKBE IS NOT INITIAL.
      READ TABLE IT_EKBE WITH KEY BELNR = LC_BELNR_EKBE.
      IF SY-SUBRC EQ 0.
        IT_REG_PROC-EBELN = IT_EKBE-EBELN.

        READ TABLE IT_EKKO WITH KEY EBELN = IT_REG_PROC-EBELN.
        IF SY-SUBRC EQ 0.
          IT_REG_PROC-BUKRS = IT_EKKO-BUKRS.
        ENDIF.
      ENDIF.
    ENDIF.

    IT_REG_PROC-ERDAT    = IT_ESTORNO_EST-DT_ATUALIZACAO.
    IT_REG_PROC-ERZET    = IT_ESTORNO_EST-HR_ATUALIZACAO.
    APPEND IT_REG_PROC.
  ENDLOOP.

*-----------------------------------------------------------------------------*
*  Filtrar registros por Empresa para processamento
*-----------------------------------------------------------------------------*

  CASE XV_JOBNM.
    WHEN C_ENT_ESTOQUE_GRAOS.

      DELETE IT_REG_PROC WHERE BUKRS IN R_BUKRS_EXT.

    WHEN C_ENT_ESTOQUE_GRAOS_0100.

      DELETE IT_REG_PROC WHERE BUKRS NOT IN R_BUKRS_EXT.

    WHEN OTHERS.
      RETURN.
  ENDCASE.

  CHECK IT_REG_PROC[] IS NOT INITIAL.

*-----------------------------------------------------------------------------*
*  Determinar Area de avaliacao
*-----------------------------------------------------------------------------*
  CLEAR: IT_EKPO[].

  SELECT *
    FROM EKPO INTO TABLE IT_EKPO
     FOR ALL ENTRIES IN IT_REG_PROC
   WHERE EBELN EQ IT_REG_PROC-EBELN.

  SORT IT_EKPO BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM IT_EKPO COMPARING EBELN.

  LOOP AT IT_REG_PROC WHERE EBELN IS NOT INITIAL.
    READ TABLE IT_EKPO WITH KEY EBELN = IT_REG_PROC-EBELN.
    CHECK SY-SUBRC EQ 0.

    IT_REG_PROC-AREA_AVAL = |{ IT_EKPO-MATNR ALPHA = OUT }|.
    IT_REG_PROC-AREA_AVAL = IT_REG_PROC-AREA_AVAL && IT_EKPO-WERKS.

    MODIFY IT_REG_PROC.
  ENDLOOP.

  SORT IT_REG_PROC BY ERDAT ERZET ASCENDING.

*-----------------------------------------------------------------------------*
*  Fazer Balanceamento por Area de Avaliação
*-----------------------------------------------------------------------------*

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(WL_SET_BAL_AREA_AVAL)
   WHERE SETNAME = 'JOB_EST_GR_BAL_AREA_AVAL'
     AND VALFROM = 'X'.

  IF ( SY-SUBRC EQ 0 ).

    MESSAGE S012.

    IT_REG_PROC_AUX[]  = IT_REG_PROC[].
    IT_REG_PROC_AREA[] = IT_REG_PROC[].

    CLEAR: IT_REG_PROC[].

    SORT                            IT_REG_PROC_AREA BY        AREA_AVAL.
    DELETE ADJACENT DUPLICATES FROM IT_REG_PROC_AREA COMPARING AREA_AVAL.

    DATA(_POSSUI_REG_PENDENTES) = ABAP_TRUE.

    WHILE _POSSUI_REG_PENDENTES EQ ABAP_TRUE.
      "Incluir um registro de cada area de Avaliação
      LOOP AT IT_REG_PROC_AREA.
        LOOP AT IT_REG_PROC_AUX ASSIGNING FIELD-SYMBOL(<FS_REG_PROC_AUX>) WHERE AREA_AVAL = IT_REG_PROC_AREA-AREA_AVAL
                                                                            AND INC_PROC  = ABAP_FALSE.
          APPEND <FS_REG_PROC_AUX> TO IT_REG_PROC.
          <FS_REG_PROC_AUX>-INC_PROC = ABAP_TRUE.
          EXIT.
        ENDLOOP.
      ENDLOOP.

      READ TABLE IT_REG_PROC_AUX WITH KEY INC_PROC = ABAP_FALSE.
      IF SY-SUBRC EQ 0.
        _POSSUI_REG_PENDENTES = ABAP_TRUE.
      ELSE.
        _POSSUI_REG_PENDENTES = ABAP_FALSE.
      ENDIF.
    ENDWHILE.

  ENDIF.

  DATA E_QUANTIDADE TYPE I.

  DATA(LC_USER_JOB) = ZCL_JOB=>GET_USER_JOB( ).
  E_QUANTIDADE = 0.

  DATA: IT_STATUS TYPE ZDE_BTCSTATUS_T.

  APPEND 'S' TO IT_STATUS.
  APPEND 'R' TO IT_STATUS.
  APPEND 'P' TO IT_STATUS.

  DATA(_TOT_REG) = LINES( IT_REG_PROC[] ).

  LC_LIM_JOBS_EXEC = 10.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(WL_SET_JOB_EST_GR_LIM_EXEC)
   WHERE SETNAME = 'JOB_EST_GR_LIM_EXEC'.

  IF ( SY-SUBRC EQ 0 ).
    IF WL_SET_JOB_EST_GR_LIM_EXEC-VALFROM > 0.
      LC_LIM_JOBS_EXEC = WL_SET_JOB_EST_GR_LIM_EXEC-VALFROM.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM SETLEAF INTO @DATA(WL_SET_WAIT_INTERVAL)
   WHERE SETNAME = 'JOB_EST_GR_WAIT_INTERVAL'.

  IF ( SY-SUBRC EQ 0 ).
    IF WL_SET_WAIT_INTERVAL-VALFROM > 0.
      LC_WAIT_INT_PROC = WL_SET_WAIT_INTERVAL-VALFROM.
    ENDIF.
  ENDIF.

  CASE XV_JOBNM.
    WHEN C_ENT_ESTOQUE_GRAOS.

      SELECT SINGLE *
        FROM SETLEAF INTO @DATA(WL_SET_ESC_AREA_AVAL)
       WHERE SETNAME = 'JOB_EST_GR_ESC_AREA_AVAL'
         AND VALFROM = 'BR'.

    WHEN C_ENT_ESTOQUE_GRAOS_0100.

      SELECT SINGLE *
        FROM SETLEAF INTO WL_SET_ESC_AREA_AVAL
       WHERE SETNAME = 'JOB_EST_GR_ESC_AREA_AVAL'
         AND VALFROM = 'EX'.

    WHEN OTHERS.
      RETURN.
  ENDCASE.

  IF ( SY-SUBRC EQ 0 ).
    LC_ESCALONAR_AREA_AVAL = ABAP_TRUE.
  ELSE.
    LC_ESCALONAR_AREA_AVAL = ABAP_FALSE.
  ENDIF.

  "Processamento Pendentes..
  CASE XV_JOBNM.
    WHEN C_ENT_ESTOQUE_GRAOS.

      SELECT SINGLE *
        FROM SETLEAF INTO @DATA(WL_SET_ESC_CONFIG)
       WHERE SETNAME = 'JOB_EST_GR_CONFIG'
         AND VALFROM = 'BR_PROC_PEN'.

    WHEN C_ENT_ESTOQUE_GRAOS_0100.

      SELECT SINGLE *
        FROM SETLEAF INTO WL_SET_ESC_CONFIG
       WHERE SETNAME = 'JOB_EST_GR_CONFIG'
         AND VALFROM = 'EX_PROC_PEN'.

    WHEN OTHERS.
      RETURN.
  ENDCASE.

  IF ( SY-SUBRC EQ 0 ).
    LC_PROC_PENDENTES = ABAP_TRUE.
  ELSE.
    LC_PROC_PENDENTES = ABAP_FALSE.
  ENDIF.

  CHECK IT_REG_PROC[] IS NOT INITIAL.

  DATA(_CONTINUA_JOB) = ABAP_TRUE.

  WHILE _CONTINUA_JOB EQ ABAP_TRUE.

    MESSAGE S009.

    LOOP AT IT_REG_PROC WHERE RG_PROC EQ ABAP_FALSE.

      DATA(_TABIX) = SY-TABIX.

      MESSAGE S008 WITH _TABIX _TOT_REG.

      IF LC_WAIT_INT_PROC > 0.
        WAIT UP TO LC_WAIT_INT_PROC SECONDS.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.

      PERFORM F_CHECK_JOBS_ERRO_ESCAL.

      "Verifica Quantidade de Jobs Programados para o Programa ZMMR019
      IF E_QUANTIDADE GE LC_LIM_JOBS_EXEC OR E_QUANTIDADE EQ 0.

        PERFORM F_GET_QTDE_PROGRAM_EXEC USING ABAP_TRUE     "Exibir Mensagem no JOB
                                     CHANGING E_QUANTIDADE.

        LC_QTDE_TIMES = 0.

        WHILE E_QUANTIDADE GE LC_LIM_JOBS_EXEC.

          WAIT UP TO 1 SECONDS.

          LC_SHOW_MSG = ABAP_FALSE.
          IF LC_QTDE_TIMES EQ 10.
            LC_QTDE_TIMES = 0.
            LC_SHOW_MSG   = ABAP_TRUE.
          ENDIF.

          PERFORM F_GET_QTDE_PROGRAM_EXEC USING LC_SHOW_MSG    "Exibir Mensagem no JOB
                                       CHANGING E_QUANTIDADE.

          ADD 1 TO LC_QTDE_TIMES.
        ENDWHILE.

      ENDIF.

      "27/05/2019 - Unificação Jobs
      "DATA(LC_IDENTIFICA) = COND STRING( WHEN WA_ZMMT_EE_ZGR_AUX-PO_NUMBER IS NOT INITIAL THEN WA_ZMMT_EE_ZGR_AUX-PO_NUMBER ELSE WA_ZMMT_EE_ZGR_AUX-OBJ_KEY ).
      "CONCATENATE 'ENT_ESTOQUE_GRAOS' LC_IDENTIFICA INTO NAME SEPARATED BY '_'.


      DATA(LC_OPERACAO) = COND STRING( WHEN IT_REG_PROC-PROGNAME EQ C_ZMMR019 THEN 'C' ELSE 'E' ).

      IF LC_ESCALONAR_AREA_AVAL EQ ABAP_TRUE.
        IF IT_REG_PROC-AREA_AVAL IS NOT INITIAL.
          CONCATENATE 'EG'     IT_REG_PROC-AREA_AVAL IT_REG_PROC-OBJ_KEY LC_OPERACAO INTO NAME SEPARATED BY '_'.
          CONCATENATE 'EG' '_' IT_REG_PROC-AREA_AVAL '%' INTO JOB_NAME_LIKE.
        ELSE.
          CONCATENATE 'EG'     IT_REG_PROC-OBJ_KEY LC_OPERACAO INTO NAME SEPARATED BY '_'.
          CONCATENATE 'EG' '_' IT_REG_PROC-OBJ_KEY '%' INTO JOB_NAME_LIKE.
        ENDIF.
      ELSE.
        IF IT_REG_PROC-EBELN IS NOT INITIAL.
          CONCATENATE 'EG'     IT_REG_PROC-EBELN IT_REG_PROC-OBJ_KEY LC_OPERACAO INTO NAME SEPARATED BY '_'.
          CONCATENATE 'EG' '_' IT_REG_PROC-EBELN '%' INTO JOB_NAME_LIKE.
        ELSE.
          CONCATENATE 'EG'     IT_REG_PROC-OBJ_KEY LC_OPERACAO INTO NAME SEPARATED BY '_'.
          CONCATENATE 'EG' '_' IT_REG_PROC-OBJ_KEY '%' INTO JOB_NAME_LIKE.
        ENDIF.
      ENDIF.

      CONDENSE NAME          NO-GAPS.
      CONDENSE JOB_NAME_LIKE NO-GAPS.

      "Se Existe Um Job Escalonado ou Executando para o Pedido de Compra não gera outro job.
      SELECT SINGLE * INTO @DATA(WA_TBTCO)
        FROM TBTCO
       WHERE JOBNAME LIKE @JOB_NAME_LIKE
         AND STATUS  IN ('S','R','P').

      IF SY-SUBRC IS INITIAL.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          JOBNAME          = NAME
        IMPORTING
          JOBCOUNT         = NUMBER
        EXCEPTIONS
          CANT_CREATE_JOB  = 1
          INVALID_JOB_DATA = 2
          JOBNAME_MISSING  = 3
          OTHERS           = 4.

      IF SY-SUBRC IS INITIAL.

        CASE IT_REG_PROC-PROGNAME.
          WHEN C_ZMMR019.

            SUBMIT ZMMR019 TO SAP-SPOOL SPOOL PARAMETERS PRINT_PARAMETERS
            WITHOUT SPOOL DYNPRO VIA JOB NAME NUMBER NUMBER
            WITH POBJKEY EQ IT_REG_PROC-OBJ_KEY
            USER LC_USER_JOB
             AND RETURN.

            ADD 1 TO E_QUANTIDADE.
            MESSAGE S006 WITH NAME.

            UPDATE ZMMT_EE_ZGR SET ZRG_ATLZ = '1'
             WHERE OBJ_KEY EQ IT_REG_PROC-OBJ_KEY.

            COMMIT WORK AND WAIT.

          WHEN C_ZMMR020.

            SUBMIT ZMMR020 TO SAP-SPOOL SPOOL PARAMETERS PRINT_PARAMETERS
            WITHOUT SPOOL DYNPRO VIA JOB NAME NUMBER NUMBER
            WITH POBJKEY EQ IT_REG_PROC-OBJ_KEY
            USER LC_USER_JOB
              AND RETURN.

            ADD 1 TO E_QUANTIDADE.
            MESSAGE S006 WITH NAME.

            UPDATE ZMMT_EEE_ZGR SET RG_ATUALIZADO = '1'
             WHERE OBJ_KEY EQ IT_REG_PROC-OBJ_KEY.

            COMMIT WORK AND WAIT.

          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        IF SY-SUBRC IS INITIAL.

          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              JOBCOUNT             = NUMBER
              JOBNAME              = NAME
              STRTIMMED            = 'X'
            EXCEPTIONS
              CANT_START_IMMEDIATE = 1
              INVALID_STARTDATE    = 2
              JOBNAME_MISSING      = 3
              JOB_CLOSE_FAILED     = 4
              JOB_NOSTEPS          = 5
              JOB_NOTEX            = 6
              LOCK_FAILED          = 7
              OTHERS               = 8.

          IF SY-SUBRC IS NOT INITIAL.
            DATA(CK_ERRO) = ABAP_TRUE.

            MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.

            CALL FUNCTION 'BP_JOB_DELETE'
              EXPORTING
                JOBCOUNT                 = NUMBER
                JOBNAME                  = NAME
              EXCEPTIONS
                CANT_DELETE_EVENT_ENTRY  = 1
                CANT_DELETE_JOB          = 2
                CANT_DELETE_JOBLOG       = 3
                CANT_DELETE_STEPS        = 4
                CANT_DELETE_TIME_ENTRY   = 5
                CANT_DERELEASE_SUCCESSOR = 6
                CANT_ENQ_PREDECESSOR     = 7
                CANT_ENQ_SUCCESSOR       = 8
                CANT_ENQ_TBTCO_ENTRY     = 9
                CANT_UPDATE_PREDECESSOR  = 10
                CANT_UPDATE_SUCCESSOR    = 11
                COMMIT_FAILED            = 12
                JOBCOUNT_MISSING         = 13
                JOBNAME_MISSING          = 14
                JOB_DOES_NOT_EXIST       = 15
                JOB_IS_ALREADY_RUNNING   = 16
                NO_DELETE_AUTHORITY      = 17
                OTHERS                   = 18.
            IF SY-SUBRC IS NOT INITIAL.
              CK_ERRO = ABAP_FALSE.
            ENDIF.
          ELSE.
*            IF WA_ZMMT_EE_ZGR_AUX-IN_AVISO_RECEB EQ 'S'.
*              TRY .
*                  "Aguardar execução do job
*                  ZCL_JOB=>GET_INSTANCE(
*                   )->SET_KEY_JOB( I_JOBNAME = NAME I_JOBCOUNT = NUMBER
*                   )->GET_WAIT_JOB_EXEC(
*                   ).
*                CATCH ZCX_JOB.    "
*              ENDTRY.
*            ENDIF.
          ENDIF.
        ELSE.
          CK_ERRO = ABAP_TRUE.
          MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              JOBCOUNT                 = NUMBER
              JOBNAME                  = NAME
            EXCEPTIONS
              CANT_DELETE_EVENT_ENTRY  = 1
              CANT_DELETE_JOB          = 2
              CANT_DELETE_JOBLOG       = 3
              CANT_DELETE_STEPS        = 4
              CANT_DELETE_TIME_ENTRY   = 5
              CANT_DERELEASE_SUCCESSOR = 6
              CANT_ENQ_PREDECESSOR     = 7
              CANT_ENQ_SUCCESSOR       = 8
              CANT_ENQ_TBTCO_ENTRY     = 9
              CANT_UPDATE_PREDECESSOR  = 10
              CANT_UPDATE_SUCCESSOR    = 11
              COMMIT_FAILED            = 12
              JOBCOUNT_MISSING         = 13
              JOBNAME_MISSING          = 14
              JOB_DOES_NOT_EXIST       = 15
              JOB_IS_ALREADY_RUNNING   = 16
              NO_DELETE_AUTHORITY      = 17
              OTHERS                   = 18.
          IF SY-SUBRC IS NOT INITIAL.
            CK_ERRO = ABAP_FALSE.
          ENDIF.
        ENDIF.

      ENDIF.

      IT_REG_PROC-RG_PROC = ABAP_TRUE.
      MODIFY IT_REG_PROC.

    ENDLOOP. "LOOP AT IT_REG_PROC WHERE RG_PROC EQ ABAP_FALSE.

    _CONTINUA_JOB = ABAP_FALSE.

    IF LC_PROC_PENDENTES EQ ABAP_TRUE.
      LOOP AT IT_REG_PROC WHERE RG_PROC EQ ABAP_FALSE.
        _CONTINUA_JOB = ABAP_TRUE.
        EXIT.
      ENDLOOP.
    ENDIF.

    IF _CONTINUA_JOB = ABAP_TRUE.
      MESSAGE S011.
      WAIT UP TO 1 SECONDS.
    ENDIF.

  ENDWHILE. "WHILE CONTINUA_JOB EQ ABAP_TRUE

FORM F_GET_QTDE_PROGRAM_EXEC USING P_SHOW_MSG
                          CHANGING C_QUANTIDADE TYPE I.

  DATA: LC_QUANTIDADE_INT    TYPE I,
        LC_QUANTIDADE_STR_19 TYPE C LENGTH 10,
        LC_QUANTIDADE_STR_20 TYPE C LENGTH 10.

  CLEAR: C_QUANTIDADE.

*-----------------------------------------------------------------------------*
* Get Jobs do programa ZMMR019( Criação Registros ) em Execução
*-----------------------------------------------------------------------------*
  ZCL_JOB=>GET_JOB_PROGRAMA_EXECUCAO(
    EXPORTING
      I_PROGNAME   = C_ZMMR019   " Nome de um programa em uma etapa (p.ex. report)
      I_SDLDATE    = SY-DATUM    " Data de escalonamento de job ou etapa
      I_STATUS     = IT_STATUS   " Status de Jobs
    IMPORTING
      E_QUANTIDADE = LC_QUANTIDADE_INT ).

  ADD LC_QUANTIDADE_INT TO C_QUANTIDADE.

  WRITE LC_QUANTIDADE_INT TO LC_QUANTIDADE_STR_19.
  CONDENSE LC_QUANTIDADE_STR_19 NO-GAPS.

*-----------------------------------------------------------------------------*
* Get Jobs do programa ZMMR020( Estorno Registros ) em Execução
*-----------------------------------------------------------------------------*
  ZCL_JOB=>GET_JOB_PROGRAMA_EXECUCAO(
    EXPORTING
      I_PROGNAME   = C_ZMMR020   " Nome de um programa em uma etapa (p.ex. report)
      I_SDLDATE    = SY-DATUM    " Data de escalonamento de job ou etapa
      I_STATUS     = IT_STATUS   " Status de Jobs
    IMPORTING
      E_QUANTIDADE = LC_QUANTIDADE_INT ).

  ADD LC_QUANTIDADE_INT TO C_QUANTIDADE.

  WRITE LC_QUANTIDADE_INT TO LC_QUANTIDADE_STR_20.
  CONDENSE LC_QUANTIDADE_STR_20 NO-GAPS.

  IF P_SHOW_MSG EQ ABAP_TRUE.
    MESSAGE S007 WITH C_ZMMR019 LC_QUANTIDADE_STR_19 C_ZMMR020 LC_QUANTIDADE_STR_20.
  ENDIF.


ENDFORM.

FORM F_CHECK_JOBS_ERRO_ESCAL.

  DATA: LIT_JOBS         TYPE /SDF/TBTCO_TT,
        LIT_STATUS_SCAL  TYPE ZDE_BTCSTATUS_T,
        LIT_TBTCO_DEL    TYPE TABLE OF TBTCO.

  DATA: LVA_INIT_DATE    TYPE BTCSDLDATE.

  CLEAR: LIT_JOBS[], LIT_STATUS_SCAL[], LIT_TBTCO_DEL[].

  LVA_INIT_DATE = SY-DATUM - 1.

  APPEND 'P' TO LIT_STATUS_SCAL.

*-----------------------------------------------------------------------------*
* Get Jobs do programa ZMMR019 e ZMMR020( Criação Registros ) com status Escalonado
*-----------------------------------------------------------------------------*

  ZCL_JOB=>GET_JOB_PROGRAMA_EXECUCAO(
    EXPORTING
      I_JOB_NAME_CP   = 'EG_*'      " Nome de um programa em uma etapa (p.ex. report)
      I_SDLDATE_INIT  = LVA_INIT_DATE     " Data de escalonamento de job ou etapa
      I_STATUS        = LIT_STATUS_SCAL   " Status de Jobs
    IMPORTING
      E_JOBS          = LIT_JOBS ).

  CHECK LIT_JOBS[] IS NOT INITIAL.

  DATA(_QTDE_JOBS_ESC) = LINES( LIT_JOBS[] ).

  MESSAGE S013 WITH _QTDE_JOBS_ESC.

  WAIT UP TO 10 SECONDS.

  SELECT *
    FROM TBTCO INTO TABLE LIT_TBTCO_DEL
     FOR ALL ENTRIES IN LIT_JOBS
   WHERE JOBNAME  EQ LIT_JOBS-JOBNAME
     AND JOBCOUNT EQ LIT_JOBS-JOBCOUNT.

  LOOP AT LIT_TBTCO_DEL INTO DATA(LWA_TBTCO_DEL).

    MESSAGE S014 WITH LWA_TBTCO_DEL-JOBNAME.

    CALL FUNCTION 'BP_JOB_DELETE'
      EXPORTING
        JOBCOUNT                 = LWA_TBTCO_DEL-JOBCOUNT
        JOBNAME                  = LWA_TBTCO_DEL-JOBNAME
      EXCEPTIONS
        CANT_DELETE_EVENT_ENTRY  = 1
        CANT_DELETE_JOB          = 2
        CANT_DELETE_JOBLOG       = 3
        CANT_DELETE_STEPS        = 4
        CANT_DELETE_TIME_ENTRY   = 5
        CANT_DERELEASE_SUCCESSOR = 6
        CANT_ENQ_PREDECESSOR     = 7
        CANT_ENQ_SUCCESSOR       = 8
        CANT_ENQ_TBTCO_ENTRY     = 9
        CANT_UPDATE_PREDECESSOR  = 10
        CANT_UPDATE_SUCCESSOR    = 11
        COMMIT_FAILED            = 12
        JOBCOUNT_MISSING         = 13
        JOBNAME_MISSING          = 14
        JOB_DOES_NOT_EXIST       = 15
        JOB_IS_ALREADY_RUNNING   = 16
        NO_DELETE_AUTHORITY      = 17
        OTHERS                   = 18.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.
