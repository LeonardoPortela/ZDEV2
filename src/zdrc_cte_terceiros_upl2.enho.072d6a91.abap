"Name: \TY:CL_EDOC_BR_CREATE_EDOCUMENT\ME:FILL_SEARCH_PARAMETERS\SE:BEGIN\EI
ENHANCEMENT 0 ZDRC_CTE_TERCEIROS_UPL2.
*
*-#132075-19.01.2024-JT-inicio
*-------------------------------------
* DRC aceitar o arquivo de CTE de terceiros no upload
*-------------------------------------
*    rs_search_parameters = mo_create_edocument->get_aceitar_arquivo_cte_terc(
*      iv_emit_cnpj = iv_emit_cnpj
*      iv_dest_cnpj = iv_dest_cnpj
*      iv_cfop      = iv_cfop
*      iv_toma      = iv_toma
*      iv_mod       = iv_mod
*      iv_accesskey = iv_accesskey ).
*
*    IF rs_search_parameters IS NOT INITIAL.
*      RETURN.
*    ENDIF.
*-#132075-19.01.2024-JT-fim
*
ENDENHANCEMENT.
