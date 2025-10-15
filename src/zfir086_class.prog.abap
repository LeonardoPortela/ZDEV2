*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_hotspot_click.

    READ TABLE t_alv INTO w_alv INDEX e_row_id-index.

    CASE e_column_id.

      WHEN 'DOCNUM'.
        SELECT docnum
          INTO @DATA(l_docnum)
          FROM j_1bnfdoc
            UP TO 1 ROWS
         WHERE docnum = @w_alv-docnum.
        ENDSELECT.

        IF sy-subrc = 0.
          SET PARAMETER ID 'JEF'   FIELD w_alv-docnum.
          CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
        ENDIF.

      WHEN 'PARC_ORIG'.
        SELECT lifnr
          INTO @DATA(l_parc_orig)
          FROM lfa1
            UP TO 1 ROWS
         WHERE lifnr = @w_alv-parc_orig.
        ENDSELECT.

        IF sy-subrc = 0.
          SET PARAMETER ID 'LIF'  FIELD w_alv-parc_orig.
          SET PARAMETER ID 'BUK'  FIELD w_alv-bukrs.
*---> 29/06/2023 - Migração S4 - JV
          "    CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN.

          DATA: lo_migbp TYPE REF TO /mignow/cl_migbp,
                lv_lifnr TYPE lifnr.

          lv_lifnr = CONV #( w_alv-parc_orig ).

          CREATE OBJECT lo_migbp
            EXPORTING
              im_test  = ABAP_false
              im_tcode = 'BP'.
          .
          CALL METHOD lo_migbp->mt_bp_display
            EXPORTING
*             IM_PARTNER       =
              im_lifnr         = lv_lifnr
              im_kunnr         = space
              im_vendor_view   = space
              im_customer_view = space.
*      IMPORTING
*        EM_ERRO          =
          .
*<--- 29/06/2023 - Migração S4 - JV
        ENDIF.

      WHEN 'PARC_CORR'.
        SELECT lifnr
          INTO @DATA(l_parc_corr)
          FROM lfa1
            UP TO 1 ROWS
         WHERE lifnr = @w_alv-parc_corr.
        ENDSELECT.

        IF sy-subrc = 0.
          SET PARAMETER ID 'LIF'  FIELD w_alv-parc_corr.
          SET PARAMETER ID 'BUK'  FIELD w_alv-bukrs.
*---> 29/06/2023 - Migração S4 - JV
          "    CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN.

          lv_lifnr = CONV #( w_alv-parc_corr ).

          CREATE OBJECT lo_migbp
            EXPORTING
              im_test  = ABAP_false
              im_tcode = 'BP'.
          .
          CALL METHOD lo_migbp->mt_bp_display
            EXPORTING
*             IM_PARTNER       =
              im_lifnr         = lv_lifnr
              im_kunnr         = space
              im_vendor_view   = space
              im_customer_view = space.
*      IMPORTING
*        EM_ERRO          =
          .
*<--- 29/06/2023 - Migração S4 - JV
        ENDIF.

      WHEN 'DOC_MATERIAL' OR
           'ANO_MATERIAL'.

        SELECT mblnr
          INTO @DATA(l_mblnr)
          FROM mkpf
            UP TO 1 ROWS
         WHERE mblnr = @w_alv-doc_material
           AND mjahr = @w_alv-ano_material.
        ENDSELECT.

        IF sy-subrc = 0.

* ---> S4 Migration - 19/07/2023 - DG
*          SET PARAMETER ID 'MBN'  FIELD w_alv-doc_material.
*          SET PARAMETER ID 'MJA'  FIELD w_alv-ano_material.
*          CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_action            = 'A04'
              i_refdoc            = 'R02'
              i_notree            = 'X'
              i_no_auth_check     = ' '
              i_deadend           = 'X'
              i_skip_first_screen = 'X'
              i_okcode            = 'OK_GO'
              i_mblnr             = w_alv-doc_material
              i_mjahr             = w_alv-ano_material.
              "I_ZEILE = I_FINAL-ZEILE.
* <--- S4 Migration - 19/07/2023 - DG
                                    endif.

      ENDCASE.

    ENDMETHOD.

    METHOD on_data_changed.
    ENDMETHOD.

    METHOD data_changed_finished.
    ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
