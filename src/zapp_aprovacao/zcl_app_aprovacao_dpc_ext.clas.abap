class ZCL_APP_APROVACAO_DPC_EXT definition
  public
  inheriting from ZCL_APP_APROVACAO_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_APP_APROVACAO_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.

    DATA keysset_create_entity TYPE zcl_app_aprovacao_mpc=>ts_keys.
    DATA lv_entityset_name TYPE string.

    lv_entityset_name = io_tech_request_context->get_entity_set_name( ).

    CASE lv_entityset_name.
*-------------------------------------------------------------------------*
*             EntitySet -  KeysSet
*-------------------------------------------------------------------------*
      WHEN 'KeysSet'.





*     Call the entity set generated method
        keysset_create_entity(
             EXPORTING iv_entity_name     = iv_entity_name
                       iv_entity_set_name = iv_entity_set_name
                       iv_source_name     = iv_source_name
                       io_data_provider   = io_data_provider
                       it_key_tab         = it_key_tab
                       it_navigation_path = it_navigation_path
                       io_tech_request_context = io_tech_request_context
           	 IMPORTING er_entity          = keysset_create_entity
        ).
*     Send specific entity data to the caller interfaces
        copy_data_to_ref(
          EXPORTING
            is_data = keysset_create_entity
          CHANGING
            cr_data = er_entity
       ).

      WHEN OTHERS.
        super->/iwbep/if_mgw_appl_srv_runtime~create_entity(
           EXPORTING
             iv_entity_name = iv_entity_name
             iv_entity_set_name = iv_entity_set_name
             iv_source_name = iv_source_name
             io_data_provider   = io_data_provider
             it_key_tab = it_key_tab
             it_navigation_path = it_navigation_path
          IMPORTING
            er_entity = er_entity
      ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
