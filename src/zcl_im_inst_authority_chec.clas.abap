CLASS zcl_im_inst_authority_chec DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_inst_authority_check .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IM_INST_AUTHORITY_CHEC IMPLEMENTATION.


  METHOD if_ex_inst_authority_check~check_equi.

    IF sy-tcode = 'IE01' OR
       sy-tcode = 'IE02' OR
       sy-tcode = 'IE03'.

      FREE MEMORY ID 'ZIE'.
      EXPORT is_equi-eqtyp FROM is_equi-eqtyp TO MEMORY ID 'ZIE'.

    ENDIF.

  ENDMETHOD.


  METHOD if_ex_inst_authority_check~check_floc.
  ENDMETHOD.
ENDCLASS.
