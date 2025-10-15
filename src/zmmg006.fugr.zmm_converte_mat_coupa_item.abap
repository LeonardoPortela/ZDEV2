FUNCTION zmm_converte_mat_coupa_item.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ITEM) TYPE  ZMMS_INT_MAT_COUPA_ITEM
*"  EXPORTING
*"     REFERENCE(E_ITEM) TYPE  ZMMS_INT_MAT_COUPA_ITEM_STR
*"----------------------------------------------------------------------

  DATA lr_mmsta TYPE RANGE OF mmsta.
  DATA lv_asnum TYPE c LENGTH 20.

  CHECK i_item IS NOT INITIAL.

  APPEND 'IEQ01' TO lr_mmsta.
  APPEND 'IEQ02' TO lr_mmsta.
  APPEND 'IEQ03' TO lr_mmsta.
  APPEND 'IEQ04' TO lr_mmsta.
  APPEND 'IEQ05' TO lr_mmsta.


  MOVE-CORRESPONDING i_item TO e_item.

  WRITE i_item-matnr TO e_item-number LEFT-JUSTIFIED NO-ZERO.
  WRITE i_item-asnum TO lv_asnum LEFT-JUSTIFIED NO-ZERO.

  "e_item-description = i_item-maktx.

  e_item-description = i_item-maktx_l.

  e_item-name = i_item-wgbez60 && ` (` && i_item-matkl && ')'.

  e_item-werks = 'CS' && i_item-werks.

  IF i_item-mstae IN lr_mmsta.
    e_item-active = 'false'.
  ELSE.
    e_item-active = 'true'.
  ENDIF.

  IF i_item-mtart <> 'ZDIE'.
    WRITE i_item-matnr TO e_item-number LEFT-JUSTIFIED NO-ZERO.
    e_item-item_type = 'item'.
    e_item-item_name = e_item-number && ` - ` && i_item-maktx.
  ELSE.

    e_item-item_type = 'Service Quantity'.
    e_item-item_name = e_item-number && ` ` && i_item-maktx && ` - ` && lv_asnum && ` ` && i_item-asktx.
    e_item-cod_servico = e_item-number && ` - ` && lv_asnum.

    CLEAR e_item-number.

  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = i_item-meins
      language       = sy-langu
    IMPORTING
      output         = e_item-uom
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    e_item-uom = i_item-meins.
  ENDIF.

  e_item-ncm = i_item-steuc.
  e_item-tipo_material = i_item-mtart.

ENDFUNCTION.
