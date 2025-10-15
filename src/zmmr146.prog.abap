*&---------------------------------------------------------------------*
*& Report  ZMMR146
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR146.

DATA: T_SET TYPE STANDARD TABLE OF SETLEAF  WITH HEADER LINE.
DATA: T_LAY TYPE STANDARD TABLE OF SETLINET WITH HEADER LINE.

DATA  : IT_SELECTION TYPE TABLE OF RSPARAMS,
        WA_SELECTION LIKE LINE OF IT_SELECTION,
        ZLV_OFFSET   TYPE I,
        VMATNR       TYPE MARA-MATNR,
        VDESCRIPT    TYPE SETLINET-DESCRIPT.
"
FIELD-SYMBOLS: <LT_DATA>      TYPE ANY TABLE,
               <LT_DATA_LINE> TYPE ANY TABLE,
               <LS_DATA>      TYPE ANY,
               <LS_DATA_LINE> TYPE ANY.


DATA: LR_DATA            TYPE REF TO DATA,
      LR_DATA_LINE       TYPE REF TO DATA,
      LR_DATA_DESCR      TYPE REF TO CL_ABAP_DATADESCR,
      LR_DATA_LINE_DESCR TYPE REF TO CL_ABAP_DATADESCR.


START-OF-SELECTION.
  SELECT SINGLE COUNT(*) INTO @DATA(VG_JOB)
    FROM TBTCO
   WHERE JOBNAME EQ 'VIEW_FISCO_ITAC'
  AND STATUS EQ 'R'.

  IF ( VG_JOB EQ 1 ).
    PERFORM SELECIONA_DADOS.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .
  SELECT *
     FROM  SETLEAF
     INTO TABLE T_SET
     WHERE SETCLASS      = '0000'
  AND   SETNAME        = 'MAGGI_ESTOQUE_FISCO_ITAC'.

  SELECT *
    FROM SETLINET
    INTO TABLE T_LAY
    FOR ALL ENTRIES IN T_SET
    WHERE SETCLASS   = T_SET-SETCLASS
    AND SUBCLASS     = T_SET-SUBCLASS
    AND SETNAME      = T_SET-SETNAME
    AND LANGU        = 'P'
    AND LINEID       = T_SET-LINEID.

  LOOP AT T_LAY.
    READ TABLE T_SET WITH KEY SETCLASS     = T_LAY-SETCLASS
                              SUBCLASS     = T_LAY-SUBCLASS
                              SETNAME      = T_LAY-SETNAME
                              LINEID       = T_LAY-LINEID.
    SELECT WERKS
      FROM T001W
      INTO TABLE @DATA(IT_T001W)
      WHERE WERKS LIKE @T_SET-VALFROM.

    LOOP AT IT_T001W INTO DATA(WA_T001W).
      WA_SELECTION-SELNAME = 'S_WERKS'.
      WA_SELECTION-KIND    = 'S'.
      WA_SELECTION-SIGN    = 'I'.
      WA_SELECTION-OPTION  = 'EQ'.
      WA_SELECTION-LOW     = WA_T001W-WERKS.
      APPEND WA_SELECTION TO IT_SELECTION.
    ENDLOOP.
    CLEAR ZLV_OFFSET.
    VDESCRIPT = T_LAY-DESCRIPT+0(40).
    WHILE 1 = 1.
      FIND FIRST OCCURRENCE OF ',' IN VDESCRIPT MATCH OFFSET ZLV_OFFSET.
      VMATNR = VDESCRIPT+0(ZLV_OFFSET).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = VMATNR
        IMPORTING
          OUTPUT = VMATNR.

      WA_SELECTION-SELNAME = 'R_MATNR'.
      WA_SELECTION-KIND    = 'S'.
      WA_SELECTION-SIGN    = 'I'.
      WA_SELECTION-OPTION  = 'EQ'.
      WA_SELECTION-LOW     = VMATNR.
      APPEND WA_SELECTION TO IT_SELECTION.
      ADD 1 TO ZLV_OFFSET.
      VDESCRIPT+0(ZLV_OFFSET) = ''.
      CONDENSE VDESCRIPT NO-GAPS.
      IF VDESCRIPT IS INITIAL.
        EXIT.
      ENDIF.
    ENDWHILE.
    "
    CL_SALV_BS_RUNTIME_INFO=>SET(
     EXPORTING DISPLAY  = ABAP_FALSE
               METADATA = ABAP_FALSE
               DATA     = ABAP_TRUE ).

    SUBMIT ZCOR011 WITH SELECTION-TABLE IT_SELECTION
                   WITH P_POPER  = SY-DATUM+4(2)
                   WITH P_BDATJ  = SY-DATUM+0(4)
                   WITH R_FISCO = 'X'
                   AND RETURN.
*    TRY.
*        CL_SALV_BS_RUNTIME_INFO=>GET_DATA_REF(
*            IMPORTING R_DATA_DESCR      = LR_DATA_DESCR
*                      R_DATA_LINE_DESCR = LR_DATA_LINE_DESCR ).
*
*        CREATE DATA LR_DATA TYPE HANDLE LR_DATA_DESCR.
*        CREATE DATA LR_DATA_LINE TYPE HANDLE LR_DATA_LINE_DESCR.
*
*        ASSIGN LR_DATA->* TO <LT_DATA>.
*        ASSIGN LR_DATA_LINE->* TO <LT_DATA_LINE>.
*
*        CL_SALV_BS_RUNTIME_INFO=>GET_DATA(
*          IMPORTING T_DATA      = <LT_DATA>
*                    T_DATA_LINE = <LT_DATA_LINE> ).
*
*      CATCH CX_SALV_BS_SC_RUNTIME_INFO.
*        MESSAGE 'Não é possível recuperar os dados ALV' TYPE 'E'.
*    ENDTRY.
*
*    CL_SALV_BS_RUNTIME_INFO=>CLEAR_ALL( ).
*
*    ASSIGN LR_DATA->* TO <LS_DATA>.
*
*    ASSIGN LR_DATA_LINE->* TO <LS_DATA_LINE>.
*
*    LOOP AT <LT_DATA_LINE> ASSIGNING <LS_DATA_LINE>.
*
*    ENDLOOP.

  ENDLOOP.

ENDFORM.
