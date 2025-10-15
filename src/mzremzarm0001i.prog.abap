*----------------------------------------------------------------------*
***INCLUDE MZREMZARM0001I .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_APLICATIVO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_APLICATIVO INPUT.

  CASE OK_CODE.
    WHEN C_BACK OR C_EXIT OR C_CANCEL.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " EXIT_APLICATIVO  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_SET                                           *
*&---------------------------------------------------------------------*
*                       Verifica Set ZMM0019_LDC                       *
*----------------------------------------------------------------------*
FORM Z_VERIFICA_SET USING P_PLANT    TYPE WERKS_D
                 CHANGING P_TAX_CODE TYPE MWSKZ.

  DATA: TL_SETLEAF  TYPE TABLE OF SETLEAF,
        WL_SETLEAF  TYPE SETLEAF,
        WL_SETLINET TYPE SETLINET.

  SELECT *
    FROM SETLEAF
    INTO TABLE TL_SETLEAF
  WHERE  SETNAME EQ 'ZMM0019_LDC'.

  SORT TL_SETLEAF BY VALFROM ASCENDING.

  READ TABLE TL_SETLEAF INTO WL_SETLEAF
    WITH KEY VALFROM = P_PLANT
    BINARY SEARCH.

  CHECK SY-SUBRC IS INITIAL.

  SELECT SINGLE *
    FROM SETLINET
    INTO WL_SETLINET
  WHERE  SETCLASS EQ WL_SETLEAF-SETCLASS
    AND  SUBCLASS EQ WL_SETLEAF-SUBCLASS
    AND  SETNAME  EQ WL_SETLEAF-SETNAME
    AND  LANGU    EQ 'P'
    AND  LINEID   EQ WL_SETLEAF-LINEID.

  CHECK SY-SUBRC             IS INITIAL AND
    NOT WL_SETLINET-DESCRIPT IS INITIAL.

  TRANSLATE WL_SETLINET-DESCRIPT TO UPPER CASE.
  CONDENSE WL_SETLINET-DESCRIPT NO-GAPS.
  P_TAX_CODE = WL_SETLINET-DESCRIPT(02).

ENDFORM.                    " Z_VERIFICA_SET
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHAMA_ESTORNO .
  DATA: TL_RETURN   TYPE TABLE OF BAPIRET2,
        WA_HEAD_RET TYPE BAPI2017_GM_HEAD_RET,
        WA_MKPF     TYPE MKPF,
        SL_RETURN   TYPE BAPIRET2,
        VG_VERFICAR TYPE SY-SUBRC,
        ANSWER      TYPE C LENGTH 1,
        V_ERRO      TYPE C LENGTH 1.
  CLEAR V_ERRO.

  PERFORM VERIFICA_SELECAO_ROMANEIO USING VG_VERFICAR.
  IF VG_VERFICAR IS INITIAL.
    IF WA_ROMANEIO-DOC_MATERIAL IS INITIAL.
      MESSAGE W006 WITH WA_ROMANEIO-NR_ROMANEIO.
    ELSE.
      PERFORM F_LOCK_ROM USING 'B' WA_ROMANEIO-CH_REFERENCIA. "Bloqueia romaneio
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        EXIT.
      ENDIF.
      "
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          TITEL     = TEXT-001
          TEXTLINE1 = TEXT-004
          TEXTLINE2 = TEXT-003
        IMPORTING
          ANSWER    = ANSWER.

      IF ANSWER EQ 'J'.

        DATA(LVA_DOC_ESTORNADO) = ABAP_FALSE.

        SELECT SINGLE *
          FROM MKPF
          INTO WA_MKPF
          WHERE MBLNR = WA_ROMANEIO-DOC_MATERIAL
          AND   MJAHR = WA_ROMANEIO-ANO_MATERIAL.

        IF SY-SUBRC NE 0.
          LVA_DOC_ESTORNADO = ABAP_TRUE.
        ELSE.
          SELECT SINGLE *
            FROM MSEG INTO @DATA(LWA_MSEG_ESTORNO)
           WHERE SMBLN = @WA_ROMANEIO-DOC_MATERIAL
             AND SJAHR = @WA_ROMANEIO-ANO_MATERIAL.

          IF SY-SUBRC EQ 0.
            LVA_DOC_ESTORNADO = ABAP_TRUE.
          ENDIF.
        ENDIF.

        IF LVA_DOC_ESTORNADO = ABAP_FALSE.

          IF WA_MKPF-BUDAT+0(6) = SY-DATUM+0(6). "pega a data do documento
            WA_ROMANEIO-DT_CHEGADA = WA_MKPF-BUDAT.
          ELSE.
            WA_ROMANEIO-DT_CHEGADA = SY-DATUM.
          ENDIF.
          WA_HEAD_RET-MAT_DOC  = WA_ROMANEIO-DOC_MATERIAL.
          WA_HEAD_RET-DOC_YEAR = WA_ROMANEIO-ANO_MATERIAL.

          CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
            EXPORTING
              MATERIALDOCUMENT    = WA_ROMANEIO-DOC_MATERIAL
              MATDOCUMENTYEAR     = WA_ROMANEIO-ANO_MATERIAL
              GOODSMVT_PSTNG_DATE = WA_ROMANEIO-DT_CHEGADA
            IMPORTING
              GOODSMVT_HEADRET    = WA_HEAD_RET
            TABLES
              RETURN              = TL_RETURN.

          IF SY-SUBRC IS INITIAL.

            LOOP AT TL_RETURN INTO SL_RETURN.

              IF SL_RETURN-TYPE EQ C_E.
                MESSAGE I000(Z01) WITH 'Não foi possível Estornar o Romaneio:'
                                        SL_RETURN-MESSAGE(40)
                                        SL_RETURN-MESSAGE+40(40).

                V_ERRO = C_X.
              ENDIF.

              CLEAR SL_RETURN.
            ENDLOOP.
          ENDIF.

        ELSE.
          CLEAR: V_ERRO. "Continuar processo e limpar campo do documento de material, pois os mesmo não existe
        ENDIF.

        IF V_ERRO NE C_X .
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.

          WAIT UP TO 1 SECONDS.

          IF VG_RETORNO IS INITIAL.
            UPDATE ZSDT0001
               SET STATUS       = ''
                   DOC_MATERIAL = ''
                   ANO_MATERIAL = ''
             WHERE CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
          ELSE.
            UPDATE ZSDT0001
              SET STATUS       = ''
                  DOC_MATERIAL = ''
                  ANO_MATERIAL = ''
                  VBELN        = ''
            WHERE CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
          ENDIF.

          COMMIT WORK.

          PERFORM F_LOCK_ROM USING 'D' WA_ROMANEIO-CH_REFERENCIA. "Desbloqueia romaneio

*          CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
*            EXPORTING
*              CD_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA
*              TP_BLOQUEIO   = SPACE.

          MESSAGE I000(Z01) WITH 'Romaneio Estornado !'.
          PERFORM TELA_SELECAO_ROMANEIOS USING WA_PEDIDOS.
        ENDIF.


      ENDIF.
      PERFORM F_LOCK_ROM USING 'D' WA_ROMANEIO-CH_REFERENCIA. "Desbloqueia romaneio
    ENDIF.
  ELSE.
    MESSAGE W003.
  ENDIF.

ENDFORM.                    " CHAMA_ESTORNO
