*&---------------------------------------------------------------------*
*& Report ZFIR099
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir099.

TABLES: lfb1.

TYPES: BEGIN of ty_bukrs,

bukrs TYPE lfb1-bukrs,

END OF ty_bukrs.

DATA r_bukrs TYPE RANGE OF lfb1-bukrs.

*https://blogs.sap.com/2021/11/20/working-with-select-options-and-ranges-tables-in-modern-abap/
*https://www.tutorialspoint.com/sap_abap/sap_abap_operators.htm

SELECT-OPTIONS: p_LIFNR FOR lfb1-lifnr,
                p_bukrs FOR lfb1-bukrs NO INTERVALS.

START-OF-SELECTION.

r_bukrs[] = p_bukrs[].

data empresa_range type range of bukrs.

SELECT 'I' AS sign,
       'EQ' AS option,
        bukrs AS low
into corresponding fields of table @empresa_range
FROM t001
WHERE bukrs not in @r_bukrs.


  SUBMIT rfitemap "fbl1n RFITEMAP
     WITH kd_lifnr-low IN p_LIFNR "lifnr Fornecedor
     WITH kd_bukrs-low IN empresa_range  "Bukrs Empresa
     WITH x_opsel EQ 'X' "Radio
     WITH pa_stida EQ sy-datum "
     WITH x_norm EQ 'X' "
   EXPORTING LIST TO MEMORY AND RETURN.

  DATA list_tab TYPE TABLE OF abaplist.

  SUBMIT report EXPORTING LIST TO MEMORY
                AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = list_tab
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc = 0.
    CALL FUNCTION 'WRITE_LIST'
      TABLES
        listobject = list_tab.
  ENDIF.
