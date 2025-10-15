*&---------------------------------------------------------------------*
*& Report  Z_DRE_PROC_DIARIO
*&
*&---------------------------------------------------------------------*
*& Programa para job de DRE Nova
*& Este Programa deve ser programado para rodar com intervalos menores
*& que uma hora, pois o primeiro job do dia pega 1 hora a menos
*& Exemplo: Data 16/07/2015 00.00.00 será atrasado para pegar regsitro de 15/07/2015 24:00:01
*&---------------------------------------------------------------------*

REPORT Z_DRE_PROC_DIARIO.


SELECTION-SCREEN BEGIN OF BLOCK Z001 WITH FRAME.
SELECT-OPTIONS: P_DATUM FOR SY-DATUM NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK Z001.

START-OF-SELECTION.

  DATA: VG_JOB  TYPE I.

  SELECT SINGLE COUNT(*) INTO VG_JOB
    FROM TBTCO
   WHERE JOBNAME EQ 'Z_DRE_PROC_DIARIO'
     AND STATUS  EQ 'R'.

  IF ( VG_JOB EQ 1 ).

    IF P_DATUM-LOW IS INITIAL.
      P_DATUM-LOW = SY-DATUM.
    ENDIF.

    CALL FUNCTION 'Z_01_DRE_PROC_DIARIO'
      EXPORTING
        PDATA    = P_DATUM-LOW
      EXCEPTIONS
        ERRO_SQL = 1
        OTHERS   = 2.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.
