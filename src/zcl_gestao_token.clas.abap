class ZCL_GESTAO_TOKEN definition
  public
  final
  create public .

public section.

  class-methods GET_TOKEN_VALIDO
    importing
      !I_ID_TOKEN type ZDE_ID_TOKEN
      !I_PROCESSO_TOKEN type ZDE_PROCESSO_TOKEN optional
    exporting
      !E_TOKEN type ZINTEGRACAO0001
    returning
      value(R_HEADER_AUTHORIZATION) type ZDE_HEADER_FIELD_T .
  class-methods UPDATE_TOKEN
    importing
      !I_TOKEN type ZINTEGRACAO0001 optional
      !I_PROCESSO_TOKEN type ZDE_PROCESSO_TOKEN optional
      !I_ID_TOKEN type ZDE_ID_TOKEN optional
      !I_ACCESS_TOKEN type STRING optional
      !I_TOKEN_TYPE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_GESTAO_TOKEN IMPLEMENTATION.


  METHOD get_token_valido.

    DATA: lva_table_timestamp   TYPE p,
          lva_current_timestamp TYPE p.

    CLEAR: r_header_authorization.

    SELECT SINGLE *
      FROM zintegracao0001 INTO @DATA(lwa_zintegracao0001)
     WHERE id_token       EQ @i_id_token
       AND processo_token EQ @i_processo_token. "LES - Consulta Documentos Viagem Carguero US 171450 - WPP

    CHECK sy-subrc EQ 0 AND
          lwa_zintegracao0001-access_token IS NOT INITIAL AND
          lwa_zintegracao0001-token_type   IS NOT INITIAL AND
          lwa_zintegracao0001-expires_in   IS NOT INITIAL.

    lva_table_timestamp   = lwa_zintegracao0001-time_stamp.

    GET TIME STAMP FIELD DATA(lva_timestamp_tmp).
    lva_current_timestamp = lva_timestamp_tmp.

    SELECT SINGLE *
     FROM zintegracao0003 INTO @DATA(lwa_zintegracao0003)
    WHERE id_token EQ @i_id_token.

    IF sy-subrc EQ 0 AND lwa_zintegracao0003-expires_in IS NOT INITIAL.
      lwa_zintegracao0001-expires_in =  lwa_zintegracao0003-expires_in.
    ENDIF.

    DATA(_dif_temp) = cl_abap_tstmp=>subtract( tstmp1 = lva_current_timestamp tstmp2 = lva_table_timestamp ).

    CHECK _dif_temp < lwa_zintegracao0001-expires_in.

    "Token ainda está válido.
    CONCATENATE lwa_zintegracao0001-token_type lwa_zintegracao0001-access_token INTO DATA(lva_header_token) SEPARATED BY space.

    e_token = lwa_zintegracao0001.

    r_header_authorization = VALUE #( ( name  = 'Authorization' value = lva_header_token ) ).


  ENDMETHOD.


  METHOD update_token.

    DATA: lwa_token TYPE zintegracao0001.

    IF i_token IS NOT INITIAL.
      lwa_token = i_token.
    ELSE.
      lwa_token-id_token       = i_id_token.
      lwa_token-access_token   = i_access_token.
      lwa_token-token_type     = i_token_type.
      lwa_token-processo_token = i_processo_token. "LES - Consulta Documentos Viagem Carguero US 171450 - WPP
    ENDIF.

    CHECK lwa_token-id_token IS NOT INITIAL AND lwa_token-access_token IS NOT INITIAL.

    IF lwa_token-time_stamp IS INITIAL.
      GET TIME STAMP FIELD DATA(lva_timestamp_current).
      lwa_token-time_stamp = lva_timestamp_current.
    ENDIF.

    IF lwa_token-token_type IS INITIAL.
      lwa_token-token_type = 'Bearer'.
    ENDIF.

    IF lwa_token-expires_in IS INITIAL.
      SELECT SINGLE *
        FROM zintegracao0003 INTO @DATA(lwa_zintegracao0003)
       WHERE id_token EQ @lwa_token-id_token.

      IF sy-subrc EQ 0 AND lwa_zintegracao0003-expires_in IS NOT INITIAL.
        lwa_token-expires_in = lwa_zintegracao0003-expires_in.
      ELSE.
        lwa_token-expires_in = '3300'.
      ENDIF.
    ENDIF.

    MODIFY zintegracao0001 FROM lwa_token.

  ENDMETHOD.
ENDCLASS.
