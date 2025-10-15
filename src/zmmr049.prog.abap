*&---------------------------------------------------------------------*
*& Report  ZMMR049
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMMR049.

TYPES: BEGIN OF TY_ZTWF_MAT_LOG,
        MATNR TYPE ZTWF_MAT_LOG-MATNR,
        WERKS TYPE ZTWF_MAT_LOG-WERKS,
        DATA  TYPE ZTWF_MAT_LOG-DATA,
        VKORG TYPE ZTWF_MAT_LOG-VKORG,
        VTWEG TYPE ZTWF_MAT_LOG-VTWEG,
        NSEQU TYPE ZTWF_MAT_LOG-NSEQU,
        HORA  TYPE ZTWF_MAT_LOG-HORA,
        USNAM  TYPE ZTWF_MAT_LOG-USNAM,
        CONT  TYPE I,
        SEQ   TYPE I,
        STATUS(10),
      END OF TY_ZTWF_MAT_LOG,

      BEGIN OF TY_ZTWF_MAT_SEQ,
        MTART TYPE ZTWF_MAT_SEQ-MTART,
        WERKS TYPE ZTWF_MAT_SEQ-WERKS,
        NSEQU TYPE ZTWF_MAT_SEQ-NSEQU,
        VKORG TYPE ZTWF_MAT_SEQ-VKORG,
        VTWEG TYPE ZTWF_MAT_SEQ-VTWEG,
        AREA  TYPE ZTWF_MAT_SEQ-AREA,
      END OF TY_ZTWF_MAT_SEQ,

      BEGIN OF TY_MARA,
        MATNR     TYPE MARA-MATNR,
        MTART     TYPE MARA-MTART,
      END OF TY_MARA.

DATA: T_LOG      TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_LOG_ANT  TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_LOG_AUX  TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_LOG_AUX2 TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_SEQ      TYPE TABLE OF TY_ZTWF_MAT_SEQ,
      T_MARA      TYPE TABLE OF TY_MARA,
      T_ZTWF_MAT_AREA_US TYPE ZTWF_MAT_AREA_US   OCCURS 0 WITH HEADER LINE.

DATA: WA_LOG      TYPE TY_ZTWF_MAT_LOG,
      WA_LOG_AUX  TYPE TY_ZTWF_MAT_LOG,
      WA_LOG_AUX2 TYPE TY_ZTWF_MAT_LOG,
      WA_SEQ      TYPE TY_ZTWF_MAT_SEQ,
      WA_MARA     TYPE TY_MARA,
      TABIX       TYPE SY-TABIX,
      VMATNR      TYPE MARA-MATNR,
      W_DATA      TYPE SY-DATUM,
      W_CONT      TYPE I.

DATA: VG_JOB      TYPE I.

*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT SINGLE COUNT( * ) INTO VG_JOB
    FROM TBTCO
   WHERE JOBNAME EQ 'BLOQUEIO_WORKFLOW'
     AND STATUS EQ 'R'.


  IF ( VG_JOB EQ 1 ).
    PERFORM PROCESSA.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Processa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROCESSA.
  SELECT MATNR MTART
    FROM MARA
    INTO TABLE T_MARA
    WHERE MSTAE = '03' "bloqueio do suprimento
    AND   LAEDA = SY-DATUM. "alterados no dia apenas

  LOOP AT T_MARA INTO WA_MARA.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_MARA-MATNR
      IMPORTING
        OUTPUT = VMATNR.

    REFRESH: T_LOG, T_LOG_AUX,T_LOG_AUX2,T_SEQ,T_ZTWF_MAT_AREA_US.
    W_DATA = SY-DATUM - 30.
    SELECT MATNR WERKS DATA VKORG VTWEG NSEQU HORA USNAM
     FROM ZTWF_MAT_LOG
       INTO TABLE T_LOG
         WHERE MATNR EQ WA_MARA-MATNR
         AND   LOEKZ NE 'X'
         AND   DATA  GE W_DATA. "Apaga Workflow menos de 30 dias

    CHECK T_LOG[] IS NOT INITIAL.

    " Faz busca nos LOG unicos para identificar se é inicio ou fim de atendimento da sequencia
    T_LOG_AUX[]  = T_LOG[].
    SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA.
    DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU.
    LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
      W_CONT = 0.
      TABIX = SY-TABIX.
      LOOP AT T_LOG INTO WA_LOG WHERE WERKS = WA_LOG_AUX-WERKS
                                       AND   MATNR = WA_LOG_AUX-MATNR
                                       AND   VKORG = WA_LOG_AUX-VKORG
                                       AND   VTWEG = WA_LOG_AUX-VTWEG
                                       AND   NSEQU = WA_LOG_AUX-NSEQU.
        ADD 1 TO W_CONT.
      ENDLOOP.
      WA_LOG_AUX-CONT = W_CONT.
      MODIFY T_LOG_AUX FROM WA_LOG_AUX INDEX TABIX TRANSPORTING CONT.
    ENDLOOP.
    DELETE T_LOG_AUX WHERE CONT NE 1.

    IF T_LOG_AUX[] IS NOT INITIAL.
      W_DATA = SY-DATUM - 10. "até 10 dias considera mesmo atendimento
      SELECT MATNR WERKS DATA VKORG VTWEG NSEQU HORA
      FROM ZTWF_MAT_LOG
        INTO TABLE T_LOG_AUX2
        FOR ALL ENTRIES IN T_LOG_AUX
          WHERE MATNR EQ T_LOG_AUX-MATNR
            AND WERKS EQ T_LOG_AUX-WERKS
            AND VKORG EQ T_LOG_AUX-VKORG
            AND VTWEG EQ T_LOG_AUX-VTWEG
            AND DATA  GT W_DATA
            AND DATA  LT SY-DATUM.

      SORT: T_LOG_AUX2  BY WERKS MATNR VKORG VTWEG ASCENDING DATA DESCENDING  NSEQU DESCENDING.

      " Se ultimo LOG é da mesma sequencia, significa que é o ultimo atendimento o primeiro select
      LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
        READ TABLE T_LOG_AUX2 INTO WA_LOG_AUX2 WITH KEY WERKS = WA_LOG_AUX-WERKS
                                                        MATNR = WA_LOG_AUX-MATNR
                                                        VKORG = WA_LOG_AUX-VKORG
                                                        VTWEG = WA_LOG_AUX-VTWEG.
        IF SY-SUBRC = 0.
          IF WA_LOG_AUX2-NSEQU = WA_LOG_AUX-NSEQU.
            APPEND WA_LOG_AUX2 TO T_LOG.
          ENDIF.
        ENDIF.
      ENDLOOP.

      SELECT MTART WERKS NSEQU VKORG VTWEG AREA
        FROM ZTWF_MAT_SEQ
          INTO TABLE T_SEQ
          FOR ALL ENTRIES IN T_LOG
            WHERE MTART EQ WA_MARA-MTART
              AND WERKS EQ T_LOG-WERKS
              AND NSEQU EQ T_LOG-NSEQU
              AND VKORG EQ T_LOG-VKORG
              AND VTWEG EQ T_LOG-VTWEG.
    ENDIF.

    T_LOG_AUX[]  = T_LOG[].
    T_LOG_AUX2[] = T_LOG[].

    "Organiza dados
    DATA: VSEQ  TYPE I,
          VCONT TYPE I,
          DATA_INI  TYPE SY-DATUM,
          DATA_FIM  TYPE SY-DATUM,
          INICIO    TYPE ZTWF_MAT_LOG-HORA,
          FIM       TYPE ZTWF_MAT_LOG-HORA.

    SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA,
          T_LOG_AUX2 BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA,
          T_SEQ      BY MTART WERKS NSEQU VKORG VTWEG.

    DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU.

    LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
      VSEQ  = 1.
      VCONT = 0.
      LOOP AT T_LOG_AUX2 INTO WA_LOG_AUX2 WHERE WERKS = WA_LOG_AUX-WERKS
                                           AND   MATNR = WA_LOG_AUX-MATNR
                                           AND   VKORG = WA_LOG_AUX-VKORG
                                           AND   VTWEG = WA_LOG_AUX-VTWEG
                                           AND   NSEQU = WA_LOG_AUX-NSEQU.
        ADD 1 TO  VCONT.
        IF VCONT = 3.
          VCONT = 1.
          ADD 1 TO VSEQ.
        ENDIF.
        WA_LOG_AUX2-SEQ = VSEQ.
        MODIFY T_LOG_AUX2 FROM WA_LOG_AUX2 INDEX SY-TABIX TRANSPORTING SEQ.
      ENDLOOP.
    ENDLOOP.

    T_LOG_AUX[] = T_LOG_AUX2[].
    SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG  NSEQU SEQ DATA  HORA,
          T_LOG_AUX2 BY WERKS MATNR VKORG VTWEG  NSEQU SEQ DATA  HORA.

    DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU SEQ.

    LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
      TABIX = SY-TABIX.
      LOOP AT  T_SEQ INTO WA_SEQ WHERE MTART  = WA_MARA-MTART
                                  AND   WERKS = WA_LOG_AUX-WERKS
                                  AND   NSEQU = WA_LOG_AUX-NSEQU
                                  AND   VKORG = WA_LOG_AUX-VKORG
                                  AND   VTWEG = WA_LOG_AUX-VTWEG.
        " obtem Data inicio e Data Fim
        CLEAR: DATA_FIM, FIM.
        CLEAR: DATA_INI, INICIO.

        LOOP AT T_LOG_AUX2 INTO WA_LOG_AUX2 WHERE WERKS = WA_LOG_AUX-WERKS
                                            AND   MATNR = WA_LOG_AUX-MATNR
                                            AND   VKORG = WA_LOG_AUX-VKORG
                                            AND   VTWEG = WA_LOG_AUX-VTWEG
                                            AND   NSEQU = WA_LOG_AUX-NSEQU
                                            AND   SEQ   = WA_LOG_AUX-SEQ.
          IF DATA_INI IS INITIAL. "1
            DATA_INI    = WA_LOG_AUX2-DATA.
            INICIO      = WA_LOG_AUX2-HORA.
          ENDIF.
        ENDLOOP.
        IF WA_LOG_AUX2-DATA = DATA_INI AND
           WA_LOG_AUX2-HORA = INICIO.
          WA_LOG_AUX-STATUS   = 'Pendente'.
        ELSE.
          WA_LOG_AUX-STATUS   = 'Finalizado'.
        ENDIF.
        MODIFY T_LOG_AUX FROM WA_LOG_AUX INDEX TABIX TRANSPORTING STATUS.
      ENDLOOP.

    ENDLOOP.

    DELETE T_LOG_AUX WHERE STATUS   NE 'Pendente'.
    T_LOG_ANT[] = T_LOG_AUX[].
    SORT T_LOG_AUX BY WERKS VKORG VTWEG DATA.
    SORT T_LOG_ANT BY WERKS VKORG VTWEG DATA.
    DELETE ADJACENT DUPLICATES FROM  T_LOG_ANT  COMPARING WERKS VKORG VTWEG NSEQU DATA.

    SORT T_LOG_AUX BY WERKS VKORG VTWEG.
    DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS VKORG VTWEG.


    CHECK T_LOG_AUX[] IS NOT INITIAL.

    SELECT * INTO TABLE T_ZTWF_MAT_AREA_US FROM ZTWF_MAT_AREA_US
        FOR ALL ENTRIES IN T_LOG_AUX
        WHERE MTART = WA_MARA-MTART AND
              WERKS = T_LOG_AUX-WERKS AND
              VKORG = T_LOG_AUX-VKORG AND
              VTWEG = T_LOG_AUX-VTWEG.

    SORT T_ZTWF_MAT_AREA_US BY AREA.
    DELETE ADJACENT DUPLICATES FROM T_ZTWF_MAT_AREA_US COMPARING AREA.
    "
*deleting work items in inbox
    DATA :  T_HEAD TYPE STANDARD TABLE OF SWKWLHEAD WITH HEADER LINE,
            RETCODE TYPE SY-SUBRC ,
* ---> S4 Migration - 19/06/2023 - MA
*            XMAT(29),
            XMAT(60),
* <--- S4 Migration - 19/06/2023 - MA
            XWER(6),
            XELIM(1).

    W_DATA = SY-DATUM - 10.

    CLEAR XELIM.

    LOOP AT T_ZTWF_MAT_AREA_US.
      REFRESH T_HEAD .
      CALL FUNCTION 'SWK_LOCAL_INBOX_GET'
        EXPORTING
          USER_ID              = T_ZTWF_MAT_AREA_US-USNAM
          USER_LANGU           = SY-LANGU
          READ_OTYPE           = 'US'
          READ_RESUBMISSION    = ' '
          PASSIVE_SUBSTITUTION = ' '
        TABLES
          WI_LIST              = T_HEAD
        EXCEPTIONS
          USER_CANNOT_GET_WI   = 1
          OTHERS               = 2.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      DELETE T_HEAD WHERE WI_RH_TASK NE 'TS99900056'.
      DELETE T_HEAD WHERE WI_CD      LT W_DATA.

      CLEAR XELIM.
      "direto pelo MM02
      LOOP AT T_HEAD.
*        IF T_HEAD-WI_CD LE W_DATA.
*          CONTINUE.
*        ENDIF.
        CONCATENATE  'material' VMATNR '/' INTO XMAT  SEPARATED BY SPACE.
        CONCATENATE  '/' T_ZTWF_MAT_AREA_US-WERKS INTO XWER  SEPARATED BY SPACE.
        IF ( T_HEAD-WI_TEXT CS XMAT AND
           T_HEAD-WI_TEXT CS XWER ).
          CALL FUNCTION 'SAP_WAPI_WORKITEM_DELETE'
            EXPORTING
              WORKITEM_ID       = T_HEAD-WI_ID
              ACTUAL_AGENT      = SY-UNAME
              LANGUAGE          = SY-LANGU
              DO_COMMIT         = 'X'
              CHECK_FINAL_STATE = ' '
            IMPORTING
              RETURN_CODE       = RETCODE.
          XELIM = 'X'.
          WRITE: / T_HEAD-WI_TEXT.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF XELIM = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF XELIM = 'X'.
      "Eliminar do log
      LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
        UPDATE ZTWF_MAT_LOG SET LOEKZ = 'X'
        WHERE MATNR = WA_LOG_AUX-MATNR
        AND   WERKS = WA_LOG_AUX-WERKS
        AND   VKORG = WA_LOG_AUX-VKORG
        AND   VTWEG = WA_LOG_AUX-VTWEG
        AND   NSEQU = WA_LOG_AUX-NSEQU
        AND   DATA  = WA_LOG_AUX-DATA.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "Processa
