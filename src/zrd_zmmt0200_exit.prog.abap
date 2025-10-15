*&---------------------------------------------------------------------*
*& Report  ZRD_zmmt0200_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZRD_ZMMT0200_EXIT.

FORM F_EXIT_ZMMT0200_0001 CHANGING P_REGISTRO_MANTER TYPE ANY.

  DATA: WL_ZMMT0200 TYPE ZMMT0200.

  CLEAR: WL_ZMMT0200.

  WL_ZMMT0200-USER_REG  = SY-UNAME.
  WL_ZMMT0200-DATA_REG  = SY-DATUM.
  WL_ZMMT0200-HORA_REG  = SY-UZEIT.

  MOVE-CORRESPONDING WL_ZMMT0200 TO P_REGISTRO_MANTER.

ENDFORM.

FORM F_EXIT_ZMMT0200_0002 USING P_REGISTRO_MANTER TYPE ANY CHANGING P_ERROR TYPE CHAR01.

  DATA: WA_ZMMT0200 TYPE ZMMT0200.
  CLEAR:  WA_ZMMT0200.

  DATA: VAR_ANSWER TYPE C.

  DATA: LIT_ZMMT0200 TYPE TABLE OF ZMMT0200.

  MOVE-CORRESPONDING P_REGISTRO_MANTER TO WA_ZMMT0200.

  CLEAR: P_ERROR.

  SELECT SINGLE MATKL
    INTO @DATA(_MATKL)
    FROM T023
   WHERE MATKL = @WA_ZMMT0200-MATKL.

  IF SY-SUBRC <> 0.
    P_ERROR = ABAP_TRUE.
    MESSAGE 'Grupo Mercadoria Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE 1
    INTO @DATA(LV_DUMMY)
    FROM TSPA
   WHERE SPART = @WA_ZMMT0200-SPART.

  IF SY-SUBRC IS NOT INITIAL.
    P_ERROR = ABAP_TRUE.
    MESSAGE 'Setor de Atividade Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM F_EXIT_ZMMT0200_0003 CHANGING P_REGISTRO_MANTER TYPE ANY.

  DATA: WA_ZMMT0200 TYPE ZMMT0200.
  MOVE-CORRESPONDING P_REGISTRO_MANTER TO WA_ZMMT0200.

  WA_ZMMT0200-DATA_REG = SY-DATUM.
  WA_ZMMT0200-HORA_REG = SY-UZEIT.
  WA_ZMMT0200-USER_REG = SY-UNAME.

  MOVE-CORRESPONDING WA_ZMMT0200 TO P_REGISTRO_MANTER.

ENDFORM.

FORM F_EXIT_ZMMT0200_0008  CHANGING P_COL_POS
                                    P_REF_TABNAME
                                    P_REF_FIELDNAME
                                    P_TABNAME
                                    P_FIELD
                                    P_SCRTEXT_L
                                    P_OUTPUTLEN
                                    P_EDIT
                                    P_SUM
                                    P_EMPHASIZE
                                    P_JUST
                                    P_HOTSPOT
                                    P_F4
                                    P_CHECK.

  IF P_REF_TABNAME = 'ZMMT0200_OUT' AND
     P_FIELD       = 'MATKL'.
    P_SCRTEXT_L    = 'Grupo Mercadoria'.
    P_OUTPUTLEN    = 16.
  ENDIF.

  IF P_REF_TABNAME = 'ZMMT0200_OUT' AND
     P_FIELD       = 'SPART'.
    P_SCRTEXT_L    = 'Setor de atividade'.
    P_OUTPUTLEN    = 16.
  ENDIF.

  IF P_REF_TABNAME = 'ZMMT0200_OUT' AND
     P_FIELD       = 'USER_REG'.
    P_SCRTEXT_L    = 'Usuário'.
    P_OUTPUTLEN    = 20.
  ENDIF.

  IF P_REF_TABNAME = 'ZMMT0200_OUT' AND
     P_FIELD       = 'DATA_REG'.
    P_SCRTEXT_L    = 'Data'.
    P_OUTPUTLEN    = 12.
  ENDIF.

  IF P_REF_TABNAME = 'ZMMT0200_OUT' AND
     P_FIELD       = 'HORA_REG'.
    P_SCRTEXT_L    = 'Hora'.
    P_OUTPUTLEN    = 12.
  ENDIF.

ENDFORM.

FORM F_EXIT_ZMMT0200_0013  TABLES P_TABLES.

  CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
    EXPORTING
      CUSOBJ   = 'ZMMT0200'
      TABFIRST = 'X'.

ENDFORM.
