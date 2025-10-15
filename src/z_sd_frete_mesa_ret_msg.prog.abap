*&---------------------------------------------------------------------*
*& Report  Z_SD_FRETE_MESA_RET_MSG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_SD_FRETE_MESA_RET_MSG.
TYPES : BEGIN OF TY_ZSDT0067,
          BSTKD TYPE ZSDT0051-BSTKD,
          STATUS TYPE ZSDT0067-STATUS,
        END   OF TY_ZSDT0067.


DATA : T_ZSDT0051     TYPE TABLE OF ZSDT0051,
       T_ZSDT0066     TYPE TABLE OF ZSDT0066,
       T_ZSDT0067_AUX TYPE TABLE OF ZSDT0067,
       T_ZSDT0067     TYPE TABLE OF TY_ZSDT0067,
       T_ZSDT0068     TYPE TABLE OF ZSDT0068,

       T_OUTRETURN TYPE TABLE OF ZFIE_RET_DOCUMENT.

DATA : WA_ZSDT0051     TYPE ZSDT0051,
       WA_ZSDT0066     TYPE ZSDT0066,
       WA_ZSDT0067_AUX TYPE ZSDT0067,
       WA_ZSDT0067     TYPE TY_ZSDT0067,
       WA_ZSDT0068     TYPE ZSDT0068,


       WA_OUTRETURN TYPE ZFIE_RET_DOCUMENT
       .

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM: Z_SELECIONA_DADOS,
           Z_PROC_DADOS,
           Z_GRAVA_MENSAGEM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_SELECIONA_DADOS .

  DATA: VG_JOB TYPE I.

  SELECT SINGLE COUNT(*) INTO VG_JOB
    FROM TBTCO
   WHERE JOBNAME EQ 'CONTRATO_FRETE_SIGAM_RET'
     AND STATUS EQ 'R'.

  IF ( VG_JOB EQ 1 ).

    SELECT *
      INTO TABLE T_ZSDT0067_AUX
      FROM ZSDT0067
     WHERE RG_ATUALIZADO = '1'.

    IF SY-SUBRC IS INITIAL.

      LOOP AT T_ZSDT0067_AUX INTO WA_ZSDT0067_AUX.

        CLEAR WA_ZSDT0067.

        WA_ZSDT0067-BSTKD  = WA_ZSDT0067_AUX-CH_REFERENCIA.
        WA_ZSDT0067-STATUS = WA_ZSDT0067_AUX-STATUS.

        APPEND WA_ZSDT0067 TO T_ZSDT0067.

      ENDLOOP.

      SELECT *
        FROM ZSDT0051
        INTO TABLE T_ZSDT0051
         FOR ALL ENTRIES IN T_ZSDT0067
       WHERE BSTKD = T_ZSDT0067-BSTKD.

      SELECT *
        FROM ZSDT0066
        INTO TABLE T_ZSDT0066
         FOR ALL ENTRIES IN T_ZSDT0051
       WHERE NRO_SOL_OV = T_ZSDT0051-NRO_SOL_OV.

    ENDIF.

  ENDIF.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_PROC_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_PROC_DADOS .

  DATA : VL_MENSAGEM TYPE BAPI_MSG.

  SORT: T_ZSDT0067 BY BSTKD,
        T_ZSDT0066 BY NRO_SOL_OV,
        T_ZSDT0051 BY BSTKD.


  REFRESH : T_ZSDT0067_AUX.

  LOOP AT T_ZSDT0067 INTO WA_ZSDT0067.

    READ TABLE T_ZSDT0051 INTO WA_ZSDT0051 WITH KEY BSTKD = WA_ZSDT0067-BSTKD BINARY SEARCH.

    READ TABLE T_ZSDT0066 INTO WA_ZSDT0066 WITH KEY NRO_SOL_OV = WA_ZSDT0051-NRO_SOL_OV BINARY SEARCH.

    IF WA_ZSDT0066-VBELN IS NOT INITIAL.
      IF WA_ZSDT0067-STATUS = 'L'.
        VL_MENSAGEM = 'Documento Gerado'.
      ELSEIF WA_ZSDT0067-STATUS = 'B'.
        VL_MENSAGEM = 'Documento bloqueado'.
      ENDIF.

      PERFORM: MENSAGEM USING WA_ZSDT0067-BSTKD
                              '33'
                              ''
                              'S'
                              VL_MENSAGEM
                              WA_ZSDT0066-VBELN
                              ''
                              ''
                              ''
                              'OV'.

      UPDATE ZSDT0067 SET RG_ATUALIZADO = '2'
                          NR_OV = WA_ZSDT0066-VBELN
                          NRO_SOL_OV = WA_ZSDT0066-NRO_SOL_OV
       WHERE CH_REFERENCIA = WA_ZSDT0067-BSTKD.

      COMMIT WORK.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " Z_PROC_DADOS


*&---------------------------------------------------------------------*
*&      Form  MENSAGEM
*&---------------------------------------------------------------------*
FORM MENSAGEM  USING P_OBJ_KEY
                     P_INTERFACE
                     P_VBELN
                     P_TYPE
                     P_MESSAGE
                     P_MSG_V1
                     P_MSG_V2
                     P_MSG_V3
                     P_MSG_V4
                     P_ID.

  CLEAR: WA_OUTRETURN.

  WA_OUTRETURN-OBJ_KEY        = P_OBJ_KEY.
  WA_OUTRETURN-INTERFACE      = P_INTERFACE.
  WA_OUTRETURN-DT_ATUALIZACAO = SY-DATUM.
  WA_OUTRETURN-HR_ATUALIZACAO = SY-UZEIT.
  WA_OUTRETURN-TYPE           = P_TYPE.
  WA_OUTRETURN-ID             = P_ID.
  WA_OUTRETURN-NUM            = '899'.
  WA_OUTRETURN-MESSAGE        = P_MESSAGE.
  WA_OUTRETURN-MESSAGE_V1     = P_MSG_V1.

  APPEND WA_OUTRETURN TO T_OUTRETURN.

  CLEAR WA_OUTRETURN.

ENDFORM.                    " MENSAGEM

*&---------------------------------------------------------------------*
*&      Form  Z_GRAVA_MENSAGEM
*&---------------------------------------------------------------------*
FORM Z_GRAVA_MENSAGEM .

  IF NOT T_OUTRETURN[] IS INITIAL.

    SORT T_OUTRETURN BY OBJ_KEY INTERFACE.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        OUTRETURN = T_OUTRETURN[].

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = T_OUTRETURN[].
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = T_OUTRETURN[].
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
    COMMIT WORK.

  ENDIF.
ENDFORM.                    " GRAVA_MENSAGEM
