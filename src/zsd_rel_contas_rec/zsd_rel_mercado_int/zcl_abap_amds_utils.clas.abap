CLASS zcl_abap_amds_utils DEFINITION
  PUBLIC

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS count_lines
      RETURNING VALUE(rt_count) TYPE zsdt_mi_count_vbeln.
    .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abap_amds_utils IMPLEMENTATION.

  METHOD count_lines
     BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT
     OPTIONS READ-ONLY USING mara.

    RETURN SELECT mandt AS r_client,
                    matnr AS r_vbeln,
                    1 AS r_count
             FROM mara;
*WHERE coluna1 = :p_param1
*                  "AND coluna2 > :p_param2;


  ENDMETHOD.

ENDCLASS.
