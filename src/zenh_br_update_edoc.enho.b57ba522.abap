"Name: \TY:CL_EDOC_BR_UPDATE_EDOC\IN:IF_EDOC_BR_UPDATE_EDOC\ME:UPDATE_BUSINESS_MODEL\SE:END\EI
ENHANCEMENT 0 ZENH_BR_UPDATE_EDOC.


  DATA:
    lo_edoc_db          TYPE REF TO cl_edocument_br_in_db,
    rs_edocumenthistory TYPE edocumenthistory_tab,
    ls_edocumenthistory TYPE edocumenthistory,
    ls_edocument        TYPE edocument,
    ls_edofile          TYPE edocumentfile,
    lo_create_param     TYPE REF TO cl_edoc_br_create_entity_param,
    lo_nfe_entity       TYPE REF TO cl_edoc_br_nfe_entity,
    lo_nfeinbr_creator  TYPE REF TO if_edoc_br_create_entity.

  data: lva_dest_cnpj_cpf TYPE j_1bcgc.

  CREATE OBJECT lo_edoc_db.
  ls_edocument = lo_edoc_db->if_edocument_db~select_edocument( iv_edoc_guid = ls_edobrincoming-edoc_guid ).
  ls_edofile = lo_edoc_db->if_edocument_db~select_edocumentfile( iv_file_guid = ls_edocument-file_guid ).
  rs_edocumenthistory = lo_edoc_db->if_edocument_db~select_edocumenthistory_tab( iv_edoc_guid   = ls_edocument-edoc_guid ).

  CREATE OBJECT lo_nfeinbr_creator TYPE cl_edoc_br_nfe_entity_creator.

  CREATE OBJECT lo_create_param
    EXPORTING
      iv_xml = ls_edofile-file_raw.
  TRY.
      lo_nfe_entity ?= lo_nfeinbr_creator->create( lo_create_param ).

      IF lo_nfe_entity IS BOUND.

        IMPORT lva_dest_cnpj_cpf TO lva_dest_cnpj_cpf FROM MEMORY ID 'CNPJ_CPF_DEST_TERCEIRO'.

        "DATA(v_dest_cnpj) = lo_nfe_entity->retrieve_dest_cnpj( ).

        IF zcl_nfe_aux=>verificar_cnpj_terceiro( lva_dest_cnpj_cpf ) = abap_true.
* limpar a informa  o de empresa na tabela eDcoument
          CLEAR ls_edocument-bukrs .
          lo_edoc_db->if_edocument_db~modify_edocument( EXPORTING is_edocument   = ls_edocument
                                                                  iv_update_task = abap_false ).

* limpar a informa  o de empresa na tabela eDcoument BR Incoming
          CLEAR ls_edobrincoming-company_code.
          CLEAR ls_edobrincoming-plant.

          mo_incoming_db->modify_edobrincoming( is_edobrincoming = ls_edobrincoming ).

* limpar a informa  o de empresa na tabela eDcoument History
          LOOP AT  rs_edocumenthistory INTO ls_edocumenthistory.
            CLEAR ls_edocumenthistory-bukrs.
            lo_edoc_db->if_edocument_db~modify_edocumenthistory( EXPORTING is_edocumenthistory = ls_edocumenthistory
                                                                           iv_update_task = abap_false ).
          ENDLOOP.

        ENDIF. " BUKRS not found

      ENDIF. " lo_nfe_entity is bound

    CATCH cx_sy_ref_is_initial ##NO_HANDLER.
  ENDTRY.

ENDENHANCEMENT.
