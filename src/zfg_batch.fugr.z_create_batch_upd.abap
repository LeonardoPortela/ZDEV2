FUNCTION z_create_batch_upd.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  IMPORTING
*"     VALUE(AUFNR) TYPE  CAUFV-AUFNR
*"     VALUE(MATERIAL) TYPE  BAPIBATCHKEY-MATERIAL
*"     VALUE(BATCH) TYPE  BAPIBATCHKEY-BATCH
*"     VALUE(PLANT) TYPE  BAPIBATCHKEY-PLANT
*"----------------------------------------------------------------------
  DATA: lt_return TYPE bapiret2_t,
        lv_matnr  TYPE matnr.
  DATA lv_data_venc TYPE sy-datum.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = material
    IMPORTING
      output       = lv_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.


  SELECT SINGLE mhdhb
  FROM mara
  WHERE matnr = @lv_matnr
  INTO @DATA(lv_mhdhb).

  IF sy-subrc = 0.

    DATA lv_days TYPE t5a4a-dlydy.

    lv_days = lv_mhdhb.


    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = lv_days
        months    = 0
        years     = 0
      IMPORTING
        calc_date = lv_data_venc.

  ENDIF.

  DATA(_header) = VALUE mcha( matnr = lv_matnr
                              werks = plant
                              charg = batch
                              vfdat = lv_data_venc
                              ).

  CALL FUNCTION 'VB_CREATE_BATCH'
    EXPORTING
      ymcha                        = _header
      new_lgort                    = 'PR01'
    IMPORTING
      ymcha                        = _header
    TABLES
      return                       = lt_return
    EXCEPTIONS
      no_material                  = 1
      no_batch                     = 2
      no_plant                     = 3
      material_not_found           = 4
      plant_not_found              = 5
      stoloc_not_found             = 6
      lock_on_material             = 7
      lock_on_plant                = 8
      lock_on_batch                = 9
      lock_system_error            = 10
      no_authority                 = 11
      batch_exist                  = 12
      stoloc_exist                 = 13
      illegal_batch_number         = 14
      no_batch_handling            = 15
      no_valuation_area            = 16
      valuation_type_not_found     = 17
      no_valuation_found           = 18
      error_automatic_batch_number = 19
      cancelled                    = 20
      wrong_status                 = 21
      interval_not_found           = 22
      number_range_not_extern      = 23
      object_not_found             = 24
      error_check_batch_number     = 25
      no_external_number           = 26
      no_customer_number           = 27
      no_class                     = 28
      error_in_classification      = 29
      inconsistency_in_key         = 30
      region_of_origin_not_found   = 31
      country_of_origin_not_found  = 32
      OTHERS                       = 33.



ENDFUNCTION.
