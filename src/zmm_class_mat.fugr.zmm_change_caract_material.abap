function zmm_change_caract_material.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_ZVALUE_ZEPI_CA) TYPE  ATWRT30
*"     REFERENCE(I_ZVALUE_ZEPI_PERI) TYPE  ATWRT30
*"     REFERENCE(I_ZVALUE_ZEPI_VALCA) TYPE  ATWRT30
*"  EXPORTING
*"     VALUE(E_ERRO) TYPE  CHAR01
*"     VALUE(E_MSG) TYPE  CHAR255
*"----------------------------------------------------------------------


  constants:
    gc_charact_ca    type atnam value 'ZEPI_CA',
    gc_charact_peri  type atnam value 'ZEPI_PERI',
    gc_charact_valca type atnam value 'ZEPI_VALCA',
    gc_mtart_zepi    type mtart value 'ZEPI',
    gc_mara          type char4 value 'MARA',
    gc_x             type char1 value 'X'.


  data: vg_matnr type char18.


*  *** Tipos
  types: begin of ty_object,
           objectkey   type bapi1003_key-object,
           objecttable type bapi1003_key-objecttable,
           classnum    type bapi1003_key-classnum,
           classtype   type bapi1003_key-classtype,
           status      type bapi1003_key-status,
         end of ty_object.

*** Tabelas internas
  data: lt_allocvalueschar type standard table of bapi1003_alloc_values_char, "BAPI1003_ALLOC_VALUES_CHAR.
        it_num             type standard table of bapi1003_alloc_values_num,
        it_curr            type standard table of bapi1003_alloc_values_curr,
        lt_return          type standard table of bapiret2.

*** Work-areas
  data: gs_return          type bapiret2,
        ls_object          type ty_object,
        gs_allocvalueschar type bapi1003_alloc_values_char.

  data: gs_headdata             type bapimathead.

  clear: e_erro, e_msg, ls_object, vg_matnr.
  free: lt_allocvalueschar.

  if i_matnr is not initial.
    vg_matnr = i_matnr.
    vg_matnr = |{ i_matnr alpha = in }|.
    i_matnr  = vg_matnr.
  endif.

  ls_object-objecttable = gc_mara.
  ls_object-objectkey   = i_matnr.
  ls_object-classnum    = 'MATEPI'.
  ls_object-classtype   = '023'.


  clear: lt_allocvalueschar[], gs_allocvalueschar.

  if i_zvalue_zepi_ca is not initial.
    gs_allocvalueschar-charact    = gc_charact_ca.
    gs_allocvalueschar-value_char = i_zvalue_zepi_ca.
    gs_allocvalueschar-instance   = 1.
    append gs_allocvalueschar to lt_allocvalueschar.
  endif.


  if i_zvalue_zepi_peri is not initial.
    gs_allocvalueschar-charact    = gc_charact_peri.
    gs_allocvalueschar-value_char = i_zvalue_zepi_peri.
    gs_allocvalueschar-instance   = 1.
    append gs_allocvalueschar to lt_allocvalueschar.
  endif.

  if i_zvalue_zepi_valca is not initial.
    gs_allocvalueschar-charact    = gc_charact_valca.
    gs_allocvalueschar-value_char = i_zvalue_zepi_valca.
    gs_allocvalueschar-instance   = 1.
    append gs_allocvalueschar to lt_allocvalueschar.
  endif.

*  Verifica existência.
  free: lt_return.
  call function 'BAPI_OBJCL_EXISTENCECHECK'
    exporting
      objectkey   = ls_object-objectkey
      objecttable = ls_object-objecttable
      classnum    = ls_object-classnum
      classtype   = ls_object-classtype
    tables
      return      = lt_return.

  read table lt_return transporting no fields
         with key type = 'S'
                  id = 'CL'
                  number = '731'.

  if sy-subrc eq 0.

    "Modificar caracteristica.
    free: lt_return.
    call function 'BAPI_OBJCL_CHANGE'
      exporting
        objectkey          = ls_object-objectkey
        objecttable        = ls_object-objecttable
        classnum           = ls_object-classnum
        classtype          = ls_object-classtype
      tables
        allocvaluesnumnew  = it_num
        allocvaluescharnew = lt_allocvalueschar
        allocvaluescurrnew = it_curr
        return             = lt_return.



    read table lt_return transporting no fields
         with key type = 'S'
                  id = 'CL'
                  number = '737'.
    if sy-subrc eq 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = gc_x.

      e_msg = |Caracteristica modificada com sucesso|.
    else.
      read table lt_return into data(ws_return) index 1.
      e_erro = abap_true.
      e_msg = |{ ws_return-message } { ws_return-message_v1 } { ws_return-message_v2 }|.
    endif.

  else.

    "Se não existir criar características
    free: lt_return.

    call function 'BAPI_OBJCL_CREATE'
      exporting
        objectkeynew    = ls_object-objectkey
        objecttablenew  = ls_object-objecttable
        classnumnew     = ls_object-classnum
        classtypenew    = ls_object-classtype
      tables
        allocvalueschar = lt_allocvalueschar
        return          = lt_return.

    read table lt_return transporting no fields
         with key type = 'S'
                  id = 'CL'
                  number = '735'.
    if sy-subrc eq 0.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = gc_x.

      e_msg = |Caracteristica criada com sucesso|.
    else.
      clear: ws_return.
      read table lt_return into ws_return index 1.
      e_erro = abap_true.
      e_msg = |{ ws_return-message } { ws_return-message_v1 } { ws_return-message_v2 }|.
    endif.
  endif.
endfunction.
