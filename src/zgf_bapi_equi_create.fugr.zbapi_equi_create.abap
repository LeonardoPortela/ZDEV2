FUNCTION ZBAPI_EQUI_CREATE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(EXTERNAL_NUMBER) LIKE  BAPI_ITOB_PARMS-EQUIPMENT
*"         OPTIONAL
*"     VALUE(DATA_GENERAL) LIKE  BAPI_ITOB STRUCTURE  BAPI_ITOB
*"     VALUE(DATA_SPECIFIC) LIKE  BAPI_ITOB_EQ_ONLY
*"  STRUCTURE  BAPI_ITOB_EQ_ONLY
*"     VALUE(DATA_FLEET) LIKE  BAPI_FLEET STRUCTURE  BAPI_FLEET
*"         OPTIONAL
*"     VALUE(VALID_DATE) LIKE  BAPI_ITOB_PARMS-INST_DATE
*"         DEFAULT SY-DATUM
*"     VALUE(DATA_INSTALL) LIKE  BAPI_ITOB_EQ_INSTALL
*"  STRUCTURE  BAPI_ITOB_EQ_INSTALL OPTIONAL
*"  EXPORTING
*"     VALUE(EQUIPMENT) LIKE  BAPI_ITOB_PARMS-EQUIPMENT
*"     VALUE(DATA_GENERAL_EXP) LIKE  BAPI_ITOB STRUCTURE  BAPI_ITOB
*"     VALUE(DATA_SPECIFIC_EXP) LIKE  BAPI_ITOB_EQ_ONLY
*"  STRUCTURE  BAPI_ITOB_EQ_ONLY
*"     VALUE(DATA_FLEET_EXP) LIKE  BAPI_FLEET STRUCTURE  BAPI_FLEET
*"     VALUE(RETURN) LIKE  BAPIRET2 STRUCTURE  BAPIRET2
*"  TABLES
*"      EXTENSIONIN STRUCTURE  BAPIPAREX OPTIONAL
*"      EXTENSIONOUT STRUCTURE  BAPIPAREX OPTIONAL
*"--------------------------------------------------------------------

  cl_eam_usage=>insert('BAPI_EQUI_CREATE').
  CLEAR return.

  DATA(lo_itob_eq_internal) = cl_itob_factory=>get_itob_bapi_eq_internal( ).
  IF lo_itob_eq_internal->check_disable_enhancements( ) IS INITIAL.
    "enhancement disabled during test automation, only.
*ENHANCEMENT-POINT bapi_equi_create_g6 SPOTS es_saplitob_bapi_eq.
  ENDIF.

* MFLE Mapping - Construction Type
  DATA(lo_other_functions) = cl_itob_factory=>get_itob_other_functions( ).
  lo_other_functions->material_number_conversion_in(
    EXPORTING
      iv_matnr18                     = data_general-consttype
      iv_guid                        = data_general-consttype_guid
      iv_version                     = data_general-consttype_version
      iv_matnr40                     = data_general-consttype_long
      iv_matnr_ext                   = data_general-consttype_external
    IMPORTING
      ev_matnr40                     = data_general-consttype_long
      ev_subrc                       = DATA(lv_subrc)
      es_return                      = return
  ).
  CHECK lv_subrc EQ 0.

* MFLE Mapping - Material Number
  lo_other_functions->material_number_conversion_in(
    EXPORTING
      iv_matnr18                     = data_specific-material
      iv_guid                        = data_specific-material_guid
      iv_version                     = data_specific-material_version
      iv_matnr40                     = data_specific-material_long
      iv_matnr_ext                   = data_specific-material_external
    IMPORTING
      ev_matnr40                     = data_specific-material_long
      ev_subrc                       = lv_subrc
      es_return                      = return
  ).
  CHECK lv_subrc EQ 0.

* MFLE Mapping - Configuration Material
  lo_other_functions->material_number_conversion_in(
    EXPORTING
      iv_matnr18                     = data_specific-configmat
      iv_guid                        = data_specific-configmat_guid
      iv_version                     = data_specific-configmat_version
      iv_matnr40                     = data_specific-configmat_long
      iv_matnr_ext                   = data_specific-configmat_external
    IMPORTING
      ev_matnr40                     = data_specific-configmat_long
      ev_subrc                       = lv_subrc
      es_return                      = return
  ).
  CHECK lv_subrc EQ 0.

* refresh buffers after rollback
  lo_itob_eq_internal->itob_clear_buffer(
    IMPORTING
      ev_subrc  = lv_subrc
      es_return = return
  ).
  CHECK lv_subrc EQ 0.

* check only one installation location is defined
  IF  NOT data_install-funcloc IS INITIAL
  AND NOT data_install-supequi IS INITIAL.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = 'E'
        cl     = 'IE'
        number = '048'
      IMPORTING
        return = return.
    lv_subrc = 9.
  ENDIF.

  CHECK lv_subrc EQ 0.

* map interface structures into internal ITOB structure
  DATA l_itob_rec  TYPE itob.
  DATA(lo_bapi_itob_serv) = cl_itob_factory=>get_itob_bapi_serv( ).
  lo_bapi_itob_serv->mapxi_bapi_itob_to_itob(
    EXPORTING
      is_bapi_itob  = data_general
    IMPORTING
      ev_subrc      = lv_subrc
      es_return     = return
    CHANGING
      cs_itob       = l_itob_rec
  ).
  CHECK lv_subrc EQ 0.

  lo_bapi_itob_serv->mapxi_bapi_itob_eq_only_to_ito(
    EXPORTING
      is_bapi_itob_eq_only = data_specific
    CHANGING
      cs_itob              = l_itob_rec
  ).


  IF NOT data_fleet IS INITIAL.
    DATA l_fleet_rec TYPE fleet.
*   map interface structure into internal FLEET structure
    lo_bapi_itob_serv->mapxi_bapi_fleet_to_fleet(
      EXPORTING
        is_bapi_fleet = data_fleet
      IMPORTING
        ev_subrc      = lv_subrc
        es_return     = return
      CHANGING
        cs_fleet      = l_fleet_rec
    ).
    CHECK lv_subrc EQ 0.

*   mapping of fields appearing bot in ITOB and FLEET
    l_fleet_rec-fleet_cat = l_itob_rec-eqart.
    l_fleet_rec-wgt_unit  = l_itob_rec-gewei.
  ENDIF.

* copy equipment number into ITOB structure
  CONDENSE external_number.                                 "n2894188
  l_itob_rec-equnr = external_number.

* call access level management
  DATA(lo_ibap) = cl_iwww_factory=>get_ibap( ).
  lo_ibap->object_set_access_level(
    EXPORTING
      id_objnr = space
      id_equnr = l_itob_rec-equnr
      id_level = gc_level_equi-bapi
  ).

*   equipment category data
  DATA(lo_ito2) = cl_itob_factory=>get_itob_ito2( ).
  lo_ito2->itob_check_category(
    EXPORTING
      iv_itobtype_imp      = itob_type-equi
      iv_category_imp      = l_itob_rec-eqtyp
      iv_dialog_mode       = abap_false
      iv_init_message_data = abap_false
      iv_read_text_tables  = abap_false
    IMPORTING
      "ev_subrc             = lv_subrc
      es_t370t_exp         = DATA(l_t370t_rec)
      "es_return            = return´
" return information not needed - ??????
" the privious code had not been interested about any result (subrc or return message)
*    EXCEPTIONS                                       " P6BK104089
*      OTHERS            = 0.                         " P6BK104089
  ).

* installation position to be set via structure DATA_INSTALL
  CLEAR l_itob_rec-posnr.
* clear output parameter
  CLEAR equipment.
  CLEAR data_general_exp.
  CLEAR data_specific_exp.
  CLEAR data_fleet_exp.

  IF lo_itob_eq_internal->check_disable_enhancements( ) IS INITIAL.

    "enhancement disabled during test automation, only.
*ENHANCEMENT-POINT ehp_bapi_equi_create_01 SPOTS es_saplitob_bapi_eq.
  ENDIF.


  IF cl_uid_cust=>iuid_used IS NOT INITIAL.

    DATA: ls_iuid_data     TYPE iuid_equi.
    DATA: ls_iuid_data_old TYPE iuid_equi.

    "check iuid data
    MOVE-CORRESPONDING l_itob_rec TO ls_iuid_data.
    CALL METHOD cl_iuid_equi=>check_iuid_fields
      EXPORTING
        is_iuid_data_old = ls_iuid_data_old
      CHANGING
        cs_iuid_data     = ls_iuid_data
      EXCEPTIONS
        check_failed     = 1
        check_warning    = 2
        OTHERS           = 3.

    "error handling
    IF sy-subrc NE 0.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = return.
      IF return-type EQ 'E' OR
         return-type EQ 'A' OR
         return-type EQ 'X'.
        EXIT.
      ENDIF.
    ELSE.
      l_itob_rec-iuid_type = ls_iuid_data-iuid_type.
      l_itob_rec-uii       = ls_iuid_data-uii.
    ENDIF.
  ENDIF.

* Linear Asset Management
  DATA(lo_eaml_bapi_util) = cl_eaml_factory=>get_eaml_bapi_util( ).
  IF  lo_eaml_bapi_util->switch_linear_asset_management( ) IS NOT INITIAL
  AND l_t370t_rec-lfe_ind IS NOT INITIAL.
    lo_eaml_bapi_util->check_data_bapi(
    EXPORTING
      iv_obart          = cl_eaml_util=>gc_obart-equi
      iv_key1           = l_itob_rec-equnr
      iv_akttyp         = cl_eaml_data_handler=>gc_aktyp_create
      is_bapi_structure = data_general
      iv_equnr          = l_itob_rec-equnr
      iv_category       = l_itob_rec-eqtyp
    IMPORTING
      et_return         = DATA(lt_return)
    ).
    CLEAR return.
    LOOP AT lt_return INTO return
     WHERE type CA 'AEX'.
      EXIT.
    ENDLOOP.
    CHECK return IS INITIAL.
  ENDIF.


* lock MATNR/SERNR combination before processing
  IF  data_specific-material_long IS NOT INITIAL
  AND data_specific-serialno      IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_GERNR_INPUT'
      EXPORTING
        input  = data_specific-serialno
      IMPORTING
        output = data_specific-serialno.

    DATA(lo_itob_ito3) = cl_itob_factory=>get_itob_ito3( ).
    lo_itob_ito3->itob_serialno_read_single(
      EXPORTING
          iv_matnr = data_specific-material_long
          iv_sernr = data_specific-serialno
      IMPORTING
        ev_subrc  = lv_subrc
    ).
    IF lv_subrc EQ 0.
      " object still exits => no creation needed
      MESSAGE e413(itob) WITH data_specific-serialno data_specific-material_long INTO return-message.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = return.
      EXIT.
    ENDIF.
    lo_itob_ito3->itob_serialno_lock_single(
      EXPORTING
        iv_matnr  = data_specific-material_long
        iv_sernr  = data_specific-serialno
     IMPORTING
        ev_subrc  = lv_subrc
        es_return = return
    ).
  ENDIF.
  CHECK lv_subrc EQ 0.

* call BAdI for mapping of EXTENSIONIN to ITOB
  DATA(lo_badi) = cl_itob_factory=>get_itob_bapi_badi( ).
  lo_badi->extensionin_equi_create(
     EXPORTING
       is_data_install = data_install
       it_extensionin  = extensionin[]
     CHANGING
       cs_object       = l_itob_rec
       cs_fleet        = l_fleet_rec
       cs_return       = return
  ).
  CHECK return-type CN 'EAX'.



* create equipment master via EQUIPMENT_SAVE
  l_itob_rec-datab = valid_date.
  DATA(lo_ie01) = cl_ieqm_factory=>get_ie01( ).
  lo_ie01->equipment_save(
    EXPORTING
      i_activity_type   = itob_activity-insert
      i_itob_type       = itob_type-equi
      i_install_rec     = data_install
      i_no_data_check   = itob_bool-false
      i_no_extnum_check = itob_bool-true
      i_sync_asset      = cl_eams_asset_sync_toggle=>get_instance( )->is_enabled( )
      i_write_cdocs     = l_t370t_rec-aebkz
      i_auth_tcode      = gc_transaction-equi_create
      i_filter_data     = itob_bool-true
      i_data_post       = itob_bool-true
      i_data_transfer   = itob_bool-false
      i_success_message = itob_bool-false
      i_commit_work     = itob_bool-false
    IMPORTING
      ev_subrc          = lv_subrc
      es_return         = return
    CHANGING
      c_object_rec      = l_itob_rec
      c_fleet_rec       = l_fleet_rec
  ).

  CHECK lv_subrc EQ 0.


* Linear Asset Management
  IF  lo_eaml_bapi_util->switch_linear_asset_management( ) IS NOT INITIAL
  AND l_t370t_rec-lfe_ind IS NOT INITIAL.
    lo_eaml_bapi_util->set_data_bapi(
    EXPORTING
      iv_obart          = cl_eaml_util=>gc_obart-equi
      iv_key1           = l_itob_rec-equnr
      iv_akttyp         = cl_eaml_data_handler=>gc_aktyp_create
      is_bapi_structure = data_general
      iv_equnr          = l_itob_rec-equnr
      iv_category       = l_itob_rec-eqtyp
    IMPORTING
      et_return         = lt_return
    CHANGING
      cs_bapi_struc_exp = data_general_exp
    ).
    CLEAR return.
    LOOP AT lt_return INTO return
     WHERE type CA 'AEX'.
      EXIT.
    ENDLOOP.
    CHECK return IS INITIAL.
  ENDIF.

* map internal ITOB structure into interface structures
  lo_bapi_itob_serv->map2e_itob_to_bapi_itob_eq_onl(
    EXPORTING
      is_itob              = l_itob_rec
    CHANGING
      cs_bapi_itob_eq_only = data_specific_exp
  ).


  lo_bapi_itob_serv->map2e_itob_to_bapi_itob(
    EXPORTING
      is_itob      = l_itob_rec
    CHANGING
      cs_bapi_itob = data_general_exp
  ).

  IF NOT data_fleet IS INITIAL.
    lo_bapi_itob_serv->map2e_fleet_to_bapi_fleet(
      EXPORTING
        is_fleet      = l_fleet_rec
      CHANGING
        cs_bapi_fleet = data_fleet_exp
    ).
  ENDIF.

* provide object instance number as export parameter
  equipment = l_itob_rec-equnr.

* call BAdI for mapping of ITOB to EXTENSIONOUT
  lo_badi->extensionout_equi(
    EXPORTING
      is_object       = l_itob_rec
      is_fleet        = l_fleet_rec
    CHANGING
      ct_extensionout = extensionout[]
      cs_return       = return
  ).
  CHECK return-type CN 'EAX'.

  lo_other_functions->material_number_conversion_out(
    EXPORTING
      iv_matnr40               = data_general_exp-consttype_long
    IMPORTING
      ev_matnr18               = data_general_exp-consttype
      ev_matnr40               = data_general_exp-consttype_long
      ev_version               = data_general_exp-consttype_version
      ev_guid                  = data_general_exp-consttype_guid
      ev_matnr_ext             = data_general_exp-consttype_external
      ev_subrc                 = lv_subrc
      es_return                = return
  ).
  CHECK lv_subrc EQ 0.

  lo_other_functions->material_number_conversion_out(
    EXPORTING
      iv_matnr40               = data_specific_exp-material_long
    IMPORTING
      ev_matnr18               = data_specific_exp-material
      ev_matnr40               = data_specific_exp-material_long
      ev_version               = data_specific_exp-material_version
      ev_guid                  = data_specific_exp-material_guid
      ev_matnr_ext             = data_specific_exp-material_external
      ev_subrc                 = lv_subrc
      es_return                = return
  ).
  CHECK lv_subrc EQ 0.

  lo_other_functions->material_number_conversion_out(
    EXPORTING
      iv_matnr40               = data_specific_exp-configmat_long
    IMPORTING
      ev_matnr18               = data_specific_exp-configmat
      ev_matnr40               = data_specific_exp-configmat_long
      ev_version               = data_specific_exp-configmat_version
      ev_guid                  = data_specific_exp-configmat_guid
      ev_matnr_ext             = data_specific_exp-configmat_external
      ev_subrc                 = lv_subrc
      es_return                = return
  ).
  CHECK lv_subrc EQ 0.

  IF lo_itob_eq_internal->check_disable_enhancements( ) IS INITIAL.

*ENHANCEMENT-POINT bapi_equi_create_g7 SPOTS es_saplitob_bapi_eq.

  ENDIF.

*   Close ITOB log if opened                                   "n2634546
  CALL FUNCTION 'ITOB_APPL_LOG_CLOSE'.                       "n2634546

ENDFUNCTION.
