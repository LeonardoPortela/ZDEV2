*&---------------------------------------------------------------------*
*& Report  ZMMR126_JOB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR126_JOB.

PARAMETERS PIDCARGA TYPE ZSDT0001CG-ID_CARGA.

START-OF-SELECTION.

  CHECK PIDCARGA IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(WA_ZSDT0001CG)
    FROM ZSDT0001CG
   WHERE ID_CARGA EQ @PIDCARGA.

  CASE WA_ZSDT0001CG-TP_CARGA.
    WHEN ZIF_CARGA=>ST_TP_CARGA_ENTRADA_FOB.
    WHEN ZIF_CARGA=>ST_TP_CARGA_SAIDA_OPUS.
    WHEN ZIF_CARGA=>ST_TP_CARGA_SAIDA_ENT_FOB.
      TRY .

          ZCL_CARGA_SAIDA=>ZIF_CARGA~GET_INSTANCE(
            )->SET_REGISTRO( EXPORTING I_ID_CARGA = WA_ZSDT0001CG-ID_CARGA
            )->SET_EMITIR_DOC_SAIDAS(
            ).
        CATCH ZCX_CARGA INTO DATA(EX_CARGA).

      ENDTRY.
  ENDCASE.
