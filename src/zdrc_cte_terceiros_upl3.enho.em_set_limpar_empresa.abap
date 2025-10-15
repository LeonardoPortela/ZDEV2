METHOD set_limpar_empresa .

  DATA: ls_edobrcteincoming TYPE edobrcteincoming,
        lv_access_key       TYPE edoc_accesskey,
        ls_raiz_cnpj(8)     TYPE n,
        lo_edoc_db          TYPE REF TO cl_edocument_br_in_db,
        ls_edocument        TYPE edocument.

  lv_access_key       = iv_access_key.
  ls_edobrcteincoming = mo_incoming_db->select_edobrcteincoming_by_key( lv_access_key ).

  CHECK ls_edobrcteincoming-plant IS INITIAL.

  ls_raiz_cnpj = ls_edobrcteincoming-toma_cnpj(8).

  SELECT SINGLE *
    INTO @DATA(_t001z)
    FROM t001z
   WHERE party = 'J_1BCG'
     AND paval = @ls_raiz_cnpj.

  CHECK sy-subrc <> 0.

*-----------------------------------
* ler edocument
*-----------------------------------
  CREATE OBJECT lo_edoc_db.

  ls_edocument = lo_edoc_db->if_edocument_db~select_edocument( iv_edoc_guid = ls_edobrcteincoming-edoc_guid ).

  CLEAR ls_edocument-bukrs.

*-----------------------------------
* modificar empresa
*-----------------------------------
  TRY.
      lo_edoc_db->if_edocument_db~modify_edocument( EXPORTING is_edocument   = ls_edocument
                                                              iv_update_task = abap_false ).
    CATCH cx_edocument.
  ENDTRY.

ENDMETHOD.
