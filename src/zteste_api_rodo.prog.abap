*&---------------------------------------------------------------------*
*& Report ZTESTE_API_RODO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zteste_api_rodo.

DATA: l_erro   TYPE char01,
      lc_api   TYPE REF TO zcl_token_vli_new,
      lc_token TYPE string.

CREATE OBJECT lc_api.

TRY .
    lc_token = lc_api->get_token( 'DESCARGA_RODOV_VLI' ).

  CATCH zcx_integracao INTO DATA(ex_integra).
    l_erro = abap_true.
  CATCH zcx_error INTO DATA(ex_error).
    l_erro = abap_true.
ENDTRY.

TRY .
    lc_token = lc_api->get_token( 'DESCARGA_RODOV_VLI' ).

  CATCH zcx_integracao INTO ex_integra.
    l_erro = abap_true.
  CATCH zcx_error INTO ex_error.
    l_erro = abap_true.
ENDTRY.

TRY .
    lc_token = lc_api->get_token( 'DESCARGA_RODOV_VLI' ).

  CATCH zcx_integracao INTO ex_integra.
    l_erro = abap_true.
  CATCH zcx_error INTO ex_error.
    l_erro = abap_true.
ENDTRY.

BREAK-POINT.
