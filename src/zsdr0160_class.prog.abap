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

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.

    DATA: l_tipo_integra TYPE ztipo_integra.

    CASE e_column_id.
      WHEN 'STATUS_TRACE'.

        CASE abap_true.
          WHEN p_formbl.
            READ TABLE t_alv_stat INTO w_alv_stat  INDEX e_row_id-index.

            CHECK sy-subrc = 0.

            CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
              EXPORTING
                i_id_carga     = w_alv_stat-id_carga
                i_matnr        = w_alv_stat-matnr
                i_werks        = w_alv_stat-werks
                i_lgort        = w_alv_stat-lgort
                i_acharg       = w_alv_stat-acharg
                i_safra        = w_alv_stat-safra
                i_tipo_integra = 'FB'.

          WHEN p_geralo.
            READ TABLE t_alv_lote INTO w_alv_lote  INDEX e_row_id-index.

            CHECK sy-subrc = 0.

            CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
              EXPORTING
                i_id_carga     = w_alv_lote-id_carga
                i_matnr        = w_alv_lote-matnr
                i_werks        = w_alv_lote-werks
                i_lgort        = w_alv_lote-lgort
                i_acharg       = w_alv_lote-acharg
                i_safra        = w_alv_lote-safra
                i_tipo_integra = 'GL'.

*-----comentado #129705-20.12.2023-JT-inicio
          WHEN p_estorn.
            READ TABLE t_alv_esto INTO w_alv_esto  INDEX e_row_id-index.

            CHECK sy-subrc = 0.

            l_tipo_integra = COND #( WHEN w_alv_esto-status_estorno = abap_off THEN 'FB'
                                     WHEN w_alv_esto-status_estorno = 'I'      THEN 'FB'
                                     WHEN w_alv_esto-status_estorno = 'D'      THEN 'ES' ).

            CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
              EXPORTING
                i_id_carga     = w_alv_esto-id_carga
                i_matnr        = w_alv_esto-matnr
                i_werks        = w_alv_esto-werks
                i_lgort        = w_alv_esto-lgort
                i_acharg       = w_alv_esto-acharg
                i_safra        = w_alv_esto-safra
                i_tipo_integra = l_tipo_integra.
*-----comentado #129705-20.12.2023-JT-fim
        ENDCASE.

      WHEN 'STATUS_NF'.
        READ TABLE t_alv_nota INTO w_alv_nota  INDEX e_row_id-index.

        CHECK sy-subrc = 0.

        CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
          EXPORTING
            i_id_carga     = w_alv_nota-id_carga
            i_matnr        = w_alv_nota-matnr
            i_werks        = w_alv_nota-werks
            i_lgort        = w_alv_nota-lgort
            i_acharg       = w_alv_nota-acharg
            i_safra        = w_alv_nota-safra
            i_tipo_integra = 'NF'.

      WHEN 'STATUS'.
        READ TABLE t_alv_reenv INTO w_alv_reenv  INDEX e_row_id-index.

        CHECK sy-subrc = 0.

        CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
          EXPORTING
            i_nro_sol_ov   = w_alv_reenv-nro_sol_ov
            i_posnr        = w_alv_reenv-posnr
            i_id_contrato  = w_alv_reenv-vbeln
            i_tipo_integra = 'OV'.

      WHEN 'MBLNR' OR 'MJAHR'.
        CASE abap_true.
          WHEN p_formbl.
            READ TABLE t_alv_stat INTO w_alv_stat  INDEX e_row_id-index.
*-----comentado #129705-20.12.2023-JT-inicio
          WHEN p_estorn.
            READ TABLE t_alv_esto INTO w_alv_esto  INDEX e_row_id-index.
*-----comentado #129705-20.12.2023-JT-fim
        ENDCASE.

        CHECK sy-subrc = 0.

        SET PARAMETER ID 'MBN' FIELD w_alv_stat-mblnr.
        SET PARAMETER ID 'MJA' FIELD w_alv_stat-mjahr.
        CALL TRANSACTION 'MB03'  AND SKIP FIRST SCREEN.

      WHEN 'MBLNR_ESTORNO' OR 'MJAHR_ESTORNO'.
        READ TABLE t_alv_esto INTO w_alv_esto  INDEX e_row_id-index.

        CHECK sy-subrc = 0.

        SET PARAMETER ID 'MBN' FIELD w_alv_esto-mblnr.
        SET PARAMETER ID 'MJA' FIELD w_alv_esto-mjahr.
        CALL TRANSACTION 'MB03'  AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM'.
        READ TABLE t_alv_nota INTO w_alv_nota  INDEX e_row_id-index.

        CHECK sy-subrc = 0.

        SET PARAMETER ID 'JEF'  FIELD w_alv_nota-docnum.
        CALL TRANSACTION 'J1B3N'  AND SKIP FIRST SCREEN.

      WHEN 'VBELN'.
        READ TABLE t_alv_reenv INTO w_alv_reenv  INDEX e_row_id-index.

        SELECT SINGLE vbeln
                 INTO @DATA(_vbeln)
                 FROM vbak
                WHERE vbeln = @w_alv_reenv-vbeln.

        IF sy-subrc = 0.
          SET PARAMETER ID 'AUN' FIELD w_alv_reenv-vbeln.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
