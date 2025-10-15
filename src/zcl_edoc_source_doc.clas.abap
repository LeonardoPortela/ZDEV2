class ZCL_EDOC_SOURCE_DOC definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EDOC_SOURCE_DOC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EDOC_SOURCE_DOC IMPLEMENTATION.


  METHOD if_edoc_source_doc~determine_document_type.

    DATA go_edocument_br_delete  TYPE REF TO if_edocument_br_delete.
    DATA lx_edocument_br_delete TYPE REF TO cx_edocument_br_delete.
    DATA ls_data TYPE edoc_src_data_file.
    DATA ls_key TYPE j_1b_nfe_access_key.
    DATA ls_access TYPE edoc_br_access_key.

    DATA: LRA_PROC_STATUS_REPLACE TYPE RANGE OF edocument-proc_status.

    CALL METHOD io_edoc_source->get_data
      IMPORTING
        es_data = ls_data.

    ls_key = ls_data-document_header-file_name.
    ls_access = ls_data-document_header-file_name.

    IF ls_key-model = '55' OR ls_data-document_header-file_type = 'NFE_XML'.

      SELECT SINGLE edoc_guid FROM edobrincoming
        INTO @DATA(lv_guid)
          WHERE accesskey = @ls_data-document_header-file_name.

      CHECK lv_guid IS NOT INITIAL.

      SELECT SINGLE proc_status FROM edocument
        INTO @DATA(lv_status)
          WHERE edoc_guid = @lv_guid. " (modelo = 55)

    ELSEIF ls_key-model = '57' OR ls_data-document_header-file_type = 'CTE_XML'.

      SELECT SINGLE edoc_guid FROM edobrcteincoming
        INTO lv_guid
          WHERE accesskey = ls_data-document_header-file_name.

      CHECK sy-dbcnt > 0.

      SELECT SINGLE proc_status FROM edocument
        INTO lv_status
          WHERE edoc_guid = lv_guid. " (modelo = 57)
    ENDIF.

    CHECK lv_status IS NOT INITIAL.

    SELECT *
      FROM TVARVC INTO TABLE @DATA(LIT_STATUS_REPLACE_EDOC)
     WHERE NAME EQ 'DRC_EDOC_STATUS_REPLACE_XML'.

    CHECK LIT_STATUS_REPLACE_EDOC[] IS NOT INITIAL.

    LOOP AT LIT_STATUS_REPLACE_EDOC INTO DATA(LWA_STATUS_REPLACE_EDOC).
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LWA_STATUS_REPLACE_EDOC-low ) TO  lra_proc_status_replace.
    ENDLOOP.

    CHECK lra_proc_status_replace IS NOT INITIAL.

    CHECK lv_status IN lra_proc_status_replace.

    CREATE OBJECT go_edocument_br_delete TYPE cl_edocument_br_delete.

    TRY.
        go_edocument_br_delete->execute( ls_access ).
        MESSAGE s209(edocument_br).

      CATCH cx_edocument_br_delete INTO lx_edocument_br_delete.
        lx_edocument_br_delete->raise_message( ).
    ENDTRY.

    "SUBMIT edoc_br_delete_edocument WITH p_key EQ ls_data-document_header-file_name AND RETURN.

  ENDMETHOD.


  method IF_EDOC_SOURCE_DOC~VALIDATE_SOURCE.
  endmethod.
ENDCLASS.
