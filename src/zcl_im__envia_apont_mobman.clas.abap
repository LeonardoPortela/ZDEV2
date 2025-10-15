CLASS zcl_im__envia_apont_mobman DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_workorder_confirm .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IM__ENVIA_APONT_MOBMAN IMPLEMENTATION.


  METHOD if_ex_workorder_confirm~at_cancel_check.
  ENDMETHOD.


  METHOD if_ex_workorder_confirm~at_save.
  ENDMETHOD.


  METHOD if_ex_workorder_confirm~before_update.
  ENDMETHOD.


  METHOD if_ex_workorder_confirm~individual_capacity.
  ENDMETHOD.


  METHOD if_ex_workorder_confirm~in_update.

    IF sy-tcode EQ 'IW41' OR
       sy-tcode EQ 'IW42' OR
       sy-tcode EQ 'IW44' OR
       sy-tcode EQ 'IW45'.

      CALL FUNCTION 'ZPM_DISPARA_API_ORDEM' IN BACKGROUND TASK AS SEPARATE UNIT
        EXPORTING
          confirmation = it_confirmation.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
