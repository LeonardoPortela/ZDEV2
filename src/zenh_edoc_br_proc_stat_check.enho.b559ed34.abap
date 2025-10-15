"Name: \TY:CL_EDOC_BR_PROC_STAT_CHECK\IN:IF_EDOC_BR_PROC\ME:EXECUTE\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_EDOC_BR_PROC_STAT_CHECK.

  FIELD-SYMBOLS: <ls_nfe> TYPE any.

  DATA: lo_parser         TYPE REF TO if_edoc_source_parser,
        lo_params         TYPE REF TO cl_edoc_br_create_entity_param,
        ls_nfeproc        TYPE edoc_br_nfe400,
        ls_cteproc        TYPE edoc_br_cte_if_root,
        lva_dest_cnpj_cpf TYPE j_1bcgc,
        md_xml            TYPE REF TO data.

  CLEAR: ls_nfeproc, ls_cteproc.

  "Converte NF-e
*  CREATE OBJECT lo_parser TYPE cl_edoc_xml_parser.
*  lo_parser->load_source( io_proc_params->get_xml( ) ).
*
*  md_xml   = lo_parser->parse_to_ddic( ls_nfeproc ).
*  ASSIGN md_xml->* TO <ls_nfe>.
*  ls_nfeproc        = <ls_nfe>.
*
*  "Converte CT-e
*  FREE: md_xml, lo_parser.
*  CREATE OBJECT lo_parser TYPE cl_edoc_xml_parser.
*  lo_parser->load_source( io_proc_params->get_xml( ) ).
*
*  md_xml   = lo_parser->parse_to_ddic( ls_cteproc ).
*  ASSIGN md_xml->* TO <ls_cte>.
*  ls_cteproc       = <ls_cte>.

  IMPORT lva_dest_cnpj_cpf TO lva_dest_cnpj_cpf FROM MEMORY ID 'CNPJ_CPF_DEST_TERCEIRO'.

  " 13.11.2023 - 127332 - RBL -->
  IF zcl_nfe_aux=>verificar_cnpj_terceiro( iv_cnpj = CONV #( lva_dest_cnpj_cpf ) ) = abap_true.
    rv_result = cl_edocument_br_in=>sc_edoc_result-status_authorized.
    RETURN.
  ENDIF.
  " 13.11.2023 - 127332 - RBL --<

ENDENHANCEMENT.
