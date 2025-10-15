*&--------------------------------------------------------------------------------*
*& Report  ZGL040
*&
*&--------------------------------------------------------------------------------*
*& Data Modif.: 09.05.2018
*& Objetivo...: Busca documentos criados na ZIB para preenchimento do campo BELNR
*&              na tabela ZGLT050 à partir do lote
*&--------------------------------------------------------------------------------*


REPORT ZGL040.


DATA: IT_ZGLT034  TYPE TABLE OF ZGLT034,
      IT_ZGLT050  TYPE TABLE OF ZGLT050,
      IT_ZIB_CHV  TYPE TABLE OF ZIB_CONTABIL_CHV WITH HEADER LINE,
      IT_ZIB_ERR  TYPE TABLE OF ZIB_CONTABIL_ERR WITH HEADER LINE,

      WA_ZGLT034  LIKE LINE OF IT_ZGLT034,
      WA_ZGLT050  LIKE LINE OF IT_ZGLT050,
      WA_ZIB_CHV  LIKE LINE OF IT_ZIB_CHV,
      WA_ZIB_ERR  LIKE LINE OF IT_ZIB_ERR.


DATA: VG_JOB   TYPE I,
      XV_JOBNM TYPE BTCJOB,
      XV_STEPC TYPE BTCSTEPCNT,
      V_OBJKEY TYPE CHAR20.
*#--------------------------------------------

START-OF-SELECTION.

CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
  IMPORTING
    JOBNAME                 = XV_JOBNM
    STEPCOUNT               = XV_STEPC
  EXCEPTIONS
    NO_RUNTIME_INFO         = 1
    OTHERS                  = 2.

CASE XV_JOBNM.

  WHEN 'BUSCA_DOC_ZGL047'.
    SELECT SINGLE COUNT(*) INTO VG_JOB
      FROM TBTCO
      WHERE JOBNAME EQ 'BUSCA_DOC_ZGL047'
      AND   STATUS  EQ 'R'.

ENDCASE.

IF ( VG_JOB EQ 1 ).

  IF XV_JOBNM EQ 'BUSCA_DOC_ZGL047'.

    SELECT * FROM ZGLT050
      INTO TABLE IT_ZGLT050
      WHERE VIG_DE      GE '01.01.1900' AND
            VIG_ATE     LE '31.12.9999' AND
            FINALIZADA  EQ '' AND
            BELNR       EQ ''.

            IF IT_ZGLT050[] IS NOT INITIAL.

              LOOP AT IT_ZGLT050 INTO WA_ZGLT050.

                SELECT SINGLE *
                  FROM ZGLT034 INTO WA_ZGLT034
                  WHERE BUKRS EQ WA_ZGLT050-BUKRS
                   AND  LOTE  EQ WA_ZGLT050-LOTE.

                  IF WA_ZGLT034 IS NOT INITIAL.

                    CONCATENATE 'ZGL17' WA_ZGLT050-DOC_LCTO WA_ZGLT034-DATA_ATUAL(4) INTO V_OBJKEY.

                    SELECT SINGLE *
                      FROM ZIB_CONTABIL_CHV
                      INTO WA_ZIB_CHV
                      WHERE OBJ_KEY EQ V_OBJKEY.

                      IF ( SY-SUBRC IS NOT INITIAL ).

                        SELECT SINGLE *
                          FROM ZIB_CONTABIL_ERR
                          INTO WA_ZIB_ERR
                          WHERE OBJ_KEY EQ V_OBJKEY.

                      ENDIF.

                  ENDIF.

                  IF WA_ZIB_CHV IS NOT INITIAL.

                    WA_ZGLT050-BELNR = WA_ZIB_CHV-BELNR.

                    UPDATE ZGLT050 SET BELNR = WA_ZGLT050-BELNR
                      WHERE SEQ_LCTO = WA_ZGLT050-SEQ_LCTO.

                  ENDIF.

                  CLEAR: WA_ZGLT034, WA_ZGLT050, WA_ZIB_CHV, WA_ZIB_ERR, V_OBJKEY.

              ENDLOOP.

            ENDIF.

  ENDIF.

ENDIF.
