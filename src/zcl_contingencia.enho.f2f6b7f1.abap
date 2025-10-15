"Name: \TY:CL_NFE_CLOUD_ACTIVE_SERVER_COM\ME:SEND_REQUEST\SE:END\EI
ENHANCEMENT 0 ZCL_CONTINGENCIA.
*
*  SELECT SINGLE *
*    FROM zlest_contin
*    INTO @DATA(_zlest_contin)
*   WHERE branch = @is_company_definition-branch
*     AND model  = @is_company_definition-model.
*
*  IF sy-subrc = 0.
*    rs_active_server-server_date_time = COND #( WHEN rs_active_server-server_date_time IS INITIAL THEN _zlest_contin-server_date_time
*                                                                                                  ELSE rs_active_server-server_date_time ).
*    rs_active_server-server_code      = _zlest_contin-server_code.
*  ENDIF.
*
ENDENHANCEMENT.
