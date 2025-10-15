class ZCL_LOC definition
  public
  final
  create public .

public section.

  methods Z_LIST_LOCAL
    importing
      !I_SWERK type SWERK
      !I_TPLNR type TPLNR
    exporting
      !GT_LOCAL type ZPME0024_T .
  methods Z_LIST_OBJECT
    importing
      !OBJNR type JEST-OBJNR
      !GT_LOCAL type ZPME0024_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_LOC IMPLEMENTATION.


  METHOD Z_LIST_LOCAL.

    SELECT *
    FROM IFLO
    INTO CORRESPONDING FIELDS OF TABLE GT_LOCAL
      WHERE SWERK EQ I_SWERK
        AND TPLNR EQ I_TPLNR.








  ENDMETHOD.


  METHOD Z_LIST_OBJECT.

    SELECT *
    FROM TJ02T AS A
    INNER JOIN JEST AS B ON B~STAT EQ A~ISTAT
      INTO TABLE @DATA(_JEST)
      FOR ALL ENTRIES IN @GT_LOCAL
        WHERE B~OBJNR EQ @GT_LOCAL-OBJNR.

  ENDMETHOD.
ENDCLASS.
