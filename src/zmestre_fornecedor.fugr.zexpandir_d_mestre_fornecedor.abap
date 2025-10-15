FUNCTION zexpandir_d_mestre_fornecedor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_BUKRS) TYPE  BUKRS
*"     REFERENCE(P_LIFNR) TYPE  LIFNR
*"----------------------------------------------------------------------

*CLASS lcl_data DEFINITION.
*  PUBLIC SECTION.
*    METHODS: constructor
*      IMPORTING
*        i_lifnr TYPE lifnr
*        i_bukrs TYPE bukrs,
*      create_vendor_data
*        EXPORTING
*          e_lifnr TYPE lifnr.
*
*  PRIVATE SECTION.
*    METHODS: prepare_data
*      RETURNING VALUE(re_flag) TYPE i.
**   Data Declarations
*    DATA: gs_vmds_extern   TYPE vmds_ei_main,
*          gs_succ_messages TYPE cvis_message,
*          gs_vmds_error    TYPE vmds_ei_main,
*          gs_err_messages  TYPE cvis_message,
*          gs_vmds_succ     TYPE vmds_ei_main,
*          gv_lifnr         TYPE lifnr,
*          gv_bukrs         TYPE bukrs.
*
*ENDCLASS.                    "lcl_data
*
*
*CLASS lcl_data IMPLEMENTATION.
*
*  METHOD constructor.
*    me->gv_bukrs  = i_bukrs .
*    me->gv_lifnr  = i_lifnr.
*  ENDMETHOD.
*
*
*  METHOD prepare_data.
*
*
**   Local Data Declaration
*    DATA: lt_contacts     TYPE vmds_ei_contacts_t,
*          ls_contacts     TYPE vmds_ei_contacts,
*          lt_vendors      TYPE vmds_ei_extern_t,
*          ls_vendors      TYPE vmds_ei_extern,
*          ls_address      TYPE cvis_ei_address1,
*          lt_company      TYPE vmds_ei_company_t,
*          ls_company      TYPE vmds_ei_company,
*          ls_company_data TYPE vmds_ei_vmd_company,
*          ls_purchas_data TYPE vmds_ei_vmd_purchasing,
*          lt_purchasing   TYPE vmds_ei_purchasing_t,
*          ls_purchasing   TYPE vmds_ei_purchasing,
*          lt_purch_func   TYPE vmds_ei_functions_t,
*          ls_purch_func   TYPE vmds_ei_functions,
*          ls_message      TYPE cvis_message,
*          lv_contactid    TYPE bapicontact_01-contact.
*
**   Clear the work area initially.
*    CLEAR gs_vmds_extern.
*
*    ls_vendors-header-object_instance-lifnr   = gv_lifnr.
*    ls_vendors-header-object_task = 'U'.      "Interface externa: código de modificação objeto
*
*
*
*
*
*
*
*  ENDMETHOD.
*ENDCLASS.
*
*
*
*
*
ENDFUNCTION.
