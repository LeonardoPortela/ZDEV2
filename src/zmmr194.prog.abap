*&---------------------------------------------------------------------*
*& Report ZMMR194
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr194.
TABLES lips.
DATA: materialdocument     TYPE bapi2017_gm_head_02-mat_doc,
      matdocumentyear      TYPE bapi2017_gm_head_02-doc_year,
      movement_header      TYPE bapi2017_gm_head_01,
      movement_items       TYPE bapi2017_gm_item_create_t,
*      goodsmvt_item_cwm    TYPE /cwm/bapi2017_gm_item_create,
      wa_movement_items    TYPE bapi2017_gm_item_create,
      return               TYPE TABLE OF bapiret2,
      lc_refkey            TYPE j_1brefkey,
      lc_gm_code           TYPE gm_code,
*      extensionin           TYPE TABLE OF bapiparex,
      wa_expheader         TYPE bapimepoheader, "BAPIMEPOHEADER
      wa_expheaderx        TYPE bapimepoheaderx, "BAPIMEPOHEADERX
      wa_exppoexpimpheader TYPE bapieikp,
      wa_purchaseorder     TYPE bapimepoheader-po_number,
      it_return            TYPE TABLE OF bapiret2.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_vbeln TYPE  lips-vbeln OBLIGATORY,
              p_lgort TYPE lips-lgort  OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
*  IF p_vbeln IS NOT INITIAL.
*    SELECT SINGLE m~*
*      INTO @DATA(wa_mkpf)
*      FROM  vbfa AS v
*      INNER JOIN mkpf AS m
*      ON    m~mblnr = v~vbeln
*      WHERE vbelv = @p_vbeln
*      AND   vbtyp_n = 'R'
*      AND   vbtyp_v = 'J'
*      AND   bwart   = '862'.
*    IF sy-subrc NE 0.
*      MESSAGE 'Remessa de saida não encontrada' TYPE 'I'.
*    ENDIF.
*
*    SELECT SINGLE *
*     FROM mseg
*     INTO @DATA(wa_mseg2)
*     WHERE mblnr = @wa_mkpf-mblnr
*     AND   mjahr = @wa_mkpf-mjahr
*     AND   lgort NE ' '
*      .
*    IF sy-subrc EQ 0.
*      p_lgort = wa_mseg2-lgort.
*      SET CURSOR FIELD 'P_LGORT' .
*    ENDIF.
*  ENDIF.



START-OF-SELECTION.

  IF p_lgort IS INITIAL.
    MESSAGE 'Informe o depósito destino' TYPE 'I'.
  ELSE.
    SELECT SINGLE m~*
      INTO @DATA(wa_mkpf)
      FROM  vbfa AS v
      INNER JOIN mkpf AS m
      ON    m~mblnr = v~vbeln
      WHERE vbelv = @p_vbeln
      AND   vbtyp_n = 'R'
      AND   vbtyp_v = 'J'
      AND   bwart   = '862'.
    IF sy-subrc NE 0.
      MESSAGE 'Remessa de saida não encontrada' TYPE 'I'.
    ENDIF.


    CLEAR: movement_header,wa_movement_items.
    REFRESH: movement_items.
    SELECT *
      INTO TABLE @DATA(it_lips)
      FROM lips
     WHERE vbeln = @p_vbeln.
    LOOP AT it_lips INTO DATA(wa_lips).

      movement_header = VALUE #( pstng_date      = sy-datum
                                 doc_date        = wa_mkpf-bldat
                                 header_txt      = wa_lips-vbeln
                                 ref_doc_no      = wa_mkpf-xblnr ).
      SELECT SINGLE *
        FROM mseg
        INTO @DATA(wa_mseg1)
        WHERE mblnr = @wa_mkpf-mblnr
        AND   mjahr = @wa_mkpf-mjahr
        AND   lgort = ' '.
      SELECT SINGLE *
      FROM mseg
      INTO @DATA(wa_mseg2)
      WHERE mblnr = @wa_mkpf-mblnr
      AND   mjahr = @wa_mkpf-mjahr
      AND   lgort NE ' '.
      wa_movement_items-mvt_ind               = 'B'.
      wa_movement_items-deliv_numb            = wa_lips-vbeln.
      wa_movement_items-deliv_item            = wa_lips-posnr.
      wa_movement_items-po_number             = wa_lips-vgbel.
      wa_movement_items-po_item               = wa_lips-vgpos.
      wa_movement_items-move_type             = '861'.
      wa_movement_items-entry_qnt             = wa_lips-lfimg.
      "
      wa_movement_items-material   = wa_lips-matnr.
      wa_movement_items-plant      = wa_mseg1-werks.
      wa_movement_items-stge_loc   = p_lgort.
      wa_movement_items-batch      = wa_lips-charg.
      wa_movement_items-entry_uom  = wa_lips-meins.
      APPEND wa_movement_items TO movement_items.
    ENDLOOP.

    lc_gm_code  = '01'.
    CLEAR: materialdocument, matdocumentyear.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = movement_header
        goodsmvt_code    = lc_gm_code  "01  MB01 - 02 MB31 - 03 MB1A - 04 MB1B - 05 MB1C - 06 MB11 - 07 MB04
      IMPORTING
        materialdocument = materialdocument
        matdocumentyear  = matdocumentyear
      TABLES
        goodsmvt_item    = movement_items
        return           = it_return.

    IF materialdocument IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      MESSAGE |Entrada gerada-> { materialdocument } | TYPE 'I'.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_structure_name = 'BAPIRET2'
          i_grid_title     = 'Mensagens'
        TABLES
          t_outtab         = it_return.

    ENDIF.
  ENDIF.


END-OF-SELECTION.
